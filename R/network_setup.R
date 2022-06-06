#' Make MATSim network
#'
#' This bypasses the network link tables reader, and instead writes a network
#' directly to xml.
#'
#'
#'
make_matsim_network <- function(network, path){
  
  # Transform nodes tibble into a list of lists of lists ========
  # The XML2 library really only works with lists. But that's a possibility!
  nodes_nestdf <- network$nodes |> 
    sf::st_transform(4326) |> 
    dplyr::mutate(
      x = sf::st_coordinates(geometry)[, 1],
      y = sf::st_coordinates(geometry)[, 2]
    )  |> 
    st_set_geometry(NULL) |>
    mutate(name = row_number()) |>
    nest(data = !name)
  
  nodes_list <- lapply(nodes_nestdf$data, function(node){
    node = structure(list(), id = node$id, x = node$x, y = node$y)
  }) |>
    setNames(rep("node", nrow(nodes_nestdf)))
   
  # Do the same think with links  ==========
  links_nestdf <- network$links|> 
    st_set_geometry(NULL) |>
    mutate(name = row_number()) |>
    nest(data = !name)
  
  links_list <- lapply(links_nestdf$data, function(link){
    link = structure(
      list(),  
      id = link$link_id,  from = link$a, to = link$b,  
      length = link$length * 1609.34, # miles to meters
      freespeed = link$ffspeed *  0.44704,# mph to meters per second
      capacity = link$capacity,
      permlanes = link$lanes,
      type = link$type
    )
  }) |>
      setNames(rep("link", nrow(links_nestdf)))
  
  
  # Make an XML document ============
  # this is the list that contains all nodes, and all links, in appropriately
  # named sub lists
  net <- structure(
    list(
      nodes = nodes_list,
      links = links_list
    ), 
    name = "wfrc tdm network" 
  )
  
  # this converts the above net list into an xml document
  xml <- as_xml_document( list( network = net )    )
  
  # this sets up the document types for MATSim,
  root <- xml_new_root(xml_dtd(name = "network", system_id = "http://www.matsim.org/files/dtd/network_v1.dtd"))
  # and adds the network xml lists to it
  r <- xml_add_child(root, xml)
  
  # write out to file
  write_xml(r, path)
  
}

#' Make BEAM network
#' 
#' @param matim_network
#' 
#' 
make_beam_network <- function(beam_net_cleaner, matsim_net){
  
  dir <- dirname(matsim_net)
  system2(beam_net_cleaner)
  
  path <- file.path(dir, "highway_network_for_beam.xml")
  
  #copy to `for_beam` folder
  if(!dir.exists("for_beam/r5")) dir.create("for_beam/r5", recursive = T)
  file.copy(path, "for_beam/r5/targets_pipeline_network.xml")
  
  path
}


get_matsim_lib <- function(matsim_lib){
  
  zipfile <- str_c(matsim_lib, ".zip", sep = "")
  if(!file.exists(matsim_lib)){
    dir.create("lib")
    download.file("https://byu.box.com/shared/static/rvgbo8zvpqry9suovkpguhuq6wk1omcc.zip", 
                  destfile = zipfile)
    unzip(zipfile, exdir = "lib")
  } else {
    message("Matsim library already present")
  }
  return(matsim_lib)
}



#' Read dbf files from the WFRC / MAG TDM Network
#' 
#' @param node_file Path to a dbf node file exported from CUBE
#' @param link_file Path to a dbf link file exported from CUBE
#' 
#' @return A list with two objects:
#'   - `links` An object of class `sf` with the link shapes and line attributes
#'   - `nodes` An object of class `sf` with the node shapes
#' 
#' @importFrom foreign read.dbf 
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter transmute
#' 
#' 
#' 
read_wfrcmag <- function(node_file, link_file, crs = 32612){
  
  # Read links table and filter ===================
  links <- foreign::read.dbf(link_file, as.is = TRUE) |>
    tibble::as_tibble()
  
  my_links <- links |>
    # filter out centroid connectors and roads with no lanes (under construction)
    dplyr::filter(FT != 1) |>
    dplyr::filter(LANES > 0) |>
    # select the columns we want
    dplyr::transmute(
      link_id = row_number(), 
      a = A, b = B,
      aadt = DY_VOL,
      length = DISTANCE,
      # speed in meters per second
      ffspeed = FF_SPD,
      ftype = FT,
      # append osm highway types
      # motorway,
      # trunk,
      # primary,
      # secondary,
      # tertiary,
      # residential
      type = case_when(
        ftype >= 30 & ftype < 40 ~ "motorway", # all freeways
        ftype == 2 ~ "trunk",
        ftype == 3 ~ "primary",
        ftype == 4 ~ "secondary",
        ftype %in% c(5, 6, 7) ~ "tertiary",
        ftype > 10 & ftype < 20 ~ "trunk", # this includes bangerter highway and US-89 between towns
        ftype >= 40 ~ "motorway-link", #ramps
        TRUE ~ "residential"
      ), 
      lanes = LANES,
      capacity = CAP1HR1LN * LANES
    )
  
  # Read nodes table =====================
  nodes <- foreign::read.dbf(node_file) |>
    tibble::as_tibble()
  
  # turn into sf
  my_nodes <- nodes |>
    dplyr::select( id = N,  x = X, y = Y ) |>
    sf::st_as_sf(coords = c('x', 'y'), crs = crs)
  
  # get centroids as separate file
  taz_ids <- unique(nodes$TAZID)
  
  centroids <- my_nodes |>
    dplyr::filter(id %in% taz_ids)
  
  # Append node coords to links ====================
  link_geom <- bind_rows(
    my_links |>
      transmute(link_id, end = 1, id = a) |>
      left_join(my_nodes),
    my_links |>
      transmute(link_id, end = 2, id = b) |>
      left_join(my_nodes)
  )|>
    select(-id) |>
    st_as_sf(crs = crs) |>
    group_by(link_id) |>
    arrange(end, .by_group = TRUE) |>
    select(-end) |>
    summarise(do_union = FALSE) |>
    st_cast("MULTILINESTRING")
  
  
  mylinks <- my_links |>
    left_join(link_geom, by = "link_id") |>
    st_as_sf()
  
  
  # Return list of links and nodes ============
  list(
    links = mylinks,
    nodes = my_nodes,
    centroids = centroids
  )
}


#' Write link and node sets to CSV files
#' 
#' @param linknodeset A list with two SF objects named `links` and `nodes`
#' @param folder Where the files will be written.
#' 
#' @details 
#' Writes out three files in the target folder:
#'   - `network.geojson` A map for visualization
#'   - `links.csv` A CSV file of all the links with attributes
#'   - `nodes.csv` A CSV file of all the nodes with coordinates
#' 
#' 
write_linknodes <- function(linknodeset, folder){
  
  # check if folder exists
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  
  # write links as a geojson for mapping
  sf::st_write(linknodeset$links |> st_transform(4326), file.path(folder, "network.geojson"), 
               delete_dsn = TRUE)
  
  # write links as CSV file
  readr::write_csv(linknodeset$links |> sf::st_set_geometry(NULL), file.path(folder, "links.csv"))
  
  
  
  # write nodes file
  linknodeset$nodes %>% # Native R pipe does not work with dot (.) syntax
    st_transform(4326) %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, 1],
      y = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_set_geometry(NULL) %>%
    readr::write_csv(file.path(folder, "nodes.csv"))
  
  # if there is a centroid frame, write it out also
  if(!is.null(linknodeset$centroids)) {
    linknodeset$nodes %>%
      st_transform(4326) %>%
      dplyr::mutate(
        x = sf::st_coordinates(.)[, 1],
        y = sf::st_coordinates(.)[, 2]
      ) %>%
      sf::st_set_geometry(NULL) %>%
      readr::write_csv(file.path(folder, "centroids.csv"))
  }
  
  file.path(folder, "network.geojson")
}
