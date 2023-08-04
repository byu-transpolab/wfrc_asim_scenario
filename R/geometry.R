#' Get a crosswalk between PUMA's and tracts
#' 
#' @param statefips Which state are we looking for?
#' @param pumas which pumas to keep?
get_puma_tr_cwalk <- function(st_fips, puma_list){
  puma_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt"
  
  # read US census crosswalk, filter, and re
  puma_tract <- read_csv(puma_url, col_types = list(PUMA5CE = col_integer())) %>%
    filter(STATEFP %in% st_fips) %>% 
    transmute(
      TRACT = str_c(STATEFP, COUNTYFP, TRACTCE),  
      PUMA = PUMA5CE
    ) %>%
    filter(PUMA %in% puma_list) %>%
    mutate(
      PUMA = formatC(PUMA, width = 5, flag = "0")
    )
  
  puma_tract
}


#' Get tracts geometry
#'
#' @param puma_tract
#'
#'
get_tracts <- function(st_fips, puma_tract, crs = 4326){
  tigris::tracts(st_fips, county = unique(substr(puma_tract$TRACT, 3, 5)), 
         class = "sf", progress_bar = FALSE, year = 2019) %>%
    st_transform(crs) %>%
    transmute(GEOID) %>%
    left_join(puma_tract, by = c("GEOID" = "TRACT")) %>%
    filter(!is.na(PUMA))
}


#' Create taz geographic layer
#'
#' @param taz_geo geojson for taz
#' @param ivt0 file with tazes to omit
#' 
#' 
get_taz <- function(taz_geo, tr, ivt0 = NULL){
  
  # WFRC has a lot of zones, including zones that are uninhabited and zones
  # that are external to the region.
  taz <- st_read(taz_geo)
  
  if ("EMPTY" %in% colnames(taz)){
    taz <- taz %>% 
      filter(!EMPTY)
  }
  if (!is.null(ivt0)){
    exclude <- read_lines(ivt0)
    taz <- taz %>% 
      filter(!TAZID %in% exclude)
  }
  
  clean_taz <-
    taz %>% 
    transmute(
      TAZ = TAZID,
      ACRES,
      DEVACRES,
      DISTRICT = DISTLRG,
      SD = DISTSML
    )
  
  
  # each taz needs to belong to at least one tract / PUMA
  tigris_taz <- clean_taz %>%
    # remove taz that do not map to a tract
    st_join(tr) %>% 
    filter(!is.na(PUMA)) %>%
    # remove duplicates of TAZ that map to many tracts
    group_by(TAZ) %>% slice(1) %>%  ungroup()
  
  # after removing these TAZ, we have some TAZs left
  final_taz <- tigris_taz %>% 
    arrange(TAZ) %>% 
    mutate(TAZ = as.character(TAZ))
  # ActivitySim can now deal with non-sequential zone numbers, so I
  # don't think we need this
  # %>%
  #   mutate(
  #     asim_taz = row_number(),
  #     .after = TAZ
  #   )
    
  final_taz
}


#' Get taz / puma/ tract crosswalk
#' 
#' @param taz
#' @param tr
#' 
get_crosswalk <- function(taz, tr){
  crosswalk <- taz %>%
    # determine which tract the taz is in
    select(TAZ) %>% st_centroid() %>%
    st_join(tr, st_within) %>% filter(!is.na(GEOID)) %>%
    # fill out remainder of columns
    rename(TRACT = GEOID) %>% mutate(REGION = "1")
  
  
  crosswalk
}