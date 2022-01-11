library(tidyverse)
library(sf)
library(wellknown)


sf <- read_sf(
  "Traffic_Analysis_Zones_(TAZ)_(Wasatch_Front)/TrafficAnalysisZones_WF.shp") %>% 
  select(TAZID, geometry)
land_use <- read_csv("data/land_use_taz.csv")

sf_tibble <- sf %>% as_tibble()

new_land_use <- left_join(land_use, sf_geom, by = c('ZONE' = 'TAZID'))

write_csv(new_land_use, "land_use.csv")
