#' Construct BEAM land use file
#' 
#' @param land_use
#' @param taz
build_beam_lu <- function(land_use){
  
  x <- land_use %>%
    transmute(
      tazid = zone_id,
      tothh = TOTHH,
      resacre = RESACRE,
      ciacre = CIACRE,
      totemp = TOTEMP,
      cbd = ifelse(area_type < 2, 1, 0)
    ) 
  
  path <- "reference_beam/utah-tpcm-loc.csv"
  write_csv(x, path)

  return(path)
}


#'  Build beam centroids file
#'  
#'  @param land_use
#'  
build_beam_centroids <- function(land_use, network){
  
  x <- land_use %>%
    transmute(
      taz = zone_id,
      wfrc_taz,
      area = TOTACRE * 4046.86 # acres to m2
    ) %>%
    left_join(network$centroids, by = c("wfrc_taz" = "id")) %>%
    st_as_sf() %>%
    transmute(
      taz, 
      `coord-x` = st_coordinates(.)[,1],
      `coord-y` = st_coordinates(.)[,2],
      area, 
    ) %>%
    st_set_geometry(NULL)
  
  
  path <- "reference_beam/parking_and_TAZ/utah-taz-centers.csv"
  write_csv(x, path)
  
  return(path)
  
}

get_gtfs <- function(){
  file <- "data_beam/r5/SLC.zip"
  if(!file.exists(file)){
    download.file("https://gtfsfeed.rideuta.com/gtfs.zip", 
                  destfile = file)
  }
  return(file)
}