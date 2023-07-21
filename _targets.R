library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse"))

r_files <- list.files("R", full.names = TRUE)

sapply(r_files, source)

data_targets <- tar_plan(
  
  # for populationsim
  puma_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt",
  st_fips = 49, # the state fips code for Utah
  puma_list = c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004),
  puma_tract = get_puma_tr_cwalk(st_fips, puma_list, puma_url),
  tr = get_tracts(st_fips, puma_tract),
)
