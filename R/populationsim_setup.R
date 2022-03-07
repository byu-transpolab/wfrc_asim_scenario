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
get_tracts <- function(st_fips, puma_tract){
  tracts(st_fips, county = unique(substr(puma_tract$TRACT, 3, 5)), 
         class = "sf", progress_bar = FALSE) %>%
    st_transform(4326) %>%
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
get_taz <- function(taz_geo, ivt0, tr){
  taz <- st_read(taz_geo) %>% 
    mutate(TAZ = as.character(TAZID)) %>%
    # remove external stations
    filter(CO_FIPS > 0) %>%
    select(TAZ) %>%
    
    # remove taz that do not map to a tract
    st_join(tr) %>% filter(!is.na(PUMA)) %>%
    
    # remove duplicates of TAZ that map to many tracts
    group_by(TAZ) %>% slice(1) %>% ungroup()
  
  # remove TAZs with IVT = 0
  IVT0 <- read_csv(ivt0)
  IVT0$TAZ <- as.character(IVT0$TAZ)
  taz <- taz %>% 
    filter(!TAZ %in% IVT0$TAZ)
  
  taz
}





