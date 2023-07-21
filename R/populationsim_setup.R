#' Get a crosswalk between PUMAs and tracts
#' 
#' @param st_fips Which state are we looking for?
#' @param puma_list which pumas to keep?
#' @param puma_url raw crosswalk from the census
get_puma_tr_cwalk <- function(st_fips, puma_list, puma_url){
 
  # read US census crosswalk, filter, and reformat
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
#' @param puma_tract filtered and reformatted puma tract crosswalk
get_tracts <- function(st_fips, puma_tract){
  tigris::tracts(st_fips, county = unique(substr(puma_tract$TRACT, 3, 5)), 
         class = "sf", progress_bar = FALSE, year = 2019) %>%
    st_transform(4326) %>%
    transmute(GEOID) %>%
    left_join(puma_tract, by = c("GEOID" = "TRACT")) %>%
    filter(!is.na(PUMA))
}
