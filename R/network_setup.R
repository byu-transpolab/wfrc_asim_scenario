#' Make MATSim network
#'
#'
#'
make_matsim_network <- function(path, matsim_lib, write_net){
  
  system2("java", "-version")
  r <- system2("java", args = c(
    str_c('-cp ', matsim_lib, '', sep = '"'), 
    "org.matsim.project.LinkTablesReader",
    file.path(path, "nodes.csv"),
    file.path(path, "links.csv"),
    file.path(path, "highway_network.xml"),
    "EPSG:26912"))
  
  if(r != 0){
    stop("MATsim network converter failed to run")
  } else {
    message("Converted network to matsim")
    file.path(path, "highway_network.xml")
  }
  
}

#' Make BEAM network
#' 
#' @param matim_network
#' 
#' 
make_beam_network <- function(beam_net_cleaner, matsim_net){
  
  dir <- dirname(matsim_net)
  system2(beam_net_cleaner)
  
  file.path(dir, "highway_network_for_beam.xml")
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
  links <- foreign::read.dbf(link_file) %>%
    tibble::as_tibble()
  
  my_links <- links %>%
    # filter out centroid connectors and roads with no lanes (under construction)
    dplyr::filter(FT != 1) %>%
    dplyr::filter(LANES > 0) %>%
    # select the columns we want
    dplyr::transmute(
      link_id = LINKID, 
      a = A, b = B,
      aadt = DY_VOL,
      length = DISTANCE,
      # speed in meters per second
      speed = FF_SPD * 0.44704,
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
  nodes <- foreign::read.dbf(node_file) %>%
    tibble::as_tibble()
  
  # turn into sf
  my_nodes <- nodes %>%
    dplyr::select( id = N,  x = X, y = Y ) %>%
    sf::st_as_sf(coords = c('x', 'y'), crs = crs)
  
  # get centroids as separate file
  taz_ids <- unique(nodes$TAZID)
  
  centroids <- my_nodes %>%
    dplyr::filter(id %in% taz_ids)
  
  # Append node coords to links ====================
  link_geom <- bind_rows(
    my_links %>%
      transmute(link_id, end = 1, id = a) %>%
      left_join(my_nodes),
    my_links %>%
      transmute(link_id, end = 2, id = b) %>%
      left_join(my_nodes)
  )%>%
    select(-id) %>%
    st_as_sf(crs = crs) %>%
    group_by(link_id) %>%
    arrange(end, .by_group = TRUE) %>%
    select(-end) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")
  
  
  mylinks <- my_links %>%
    left_join(link_geom, by = "link_id") %>%
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
  sf::st_write(linknodeset$links %>% st_transform(4326), file.path(folder, "network.geojson"), 
               delete_dsn = TRUE)
  
  # write links as CSV file
  readr::write_csv(linknodeset$links %>% sf::st_set_geometry(NULL), file.path(folder, "links.csv"))
  
  
  
  # write nodes file
  linknodeset$nodes %>%
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
