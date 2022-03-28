if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, sf, wellknown)
###################################################

#Set shapefile path
shp_path <- "reference/TAC_areas/shapefiles/TAC_areas_ALL.shp"

#Set path to reference file (fleet size, shifts, etc.)
ref_path <- "reference/TAC_areas/zone_info.csv"

#Set path to scenarios file
scen_path <- "reference/TAC_areas/scenarios.csv"

#Set output dir
out_dir <- "reference_beam/ridehail_fleets"

#Set coordinate system to convert to (epsg #)
#(4326 = WGS84, 26912 = UTM12N)
toCoord <- 26912

###################################################

#read in shapefile and add wkt column
areasALL <- read_sf(shp_path) %>% 
  st_transform(toCoord) %>% 
  mutate(geofencePolygon = sf_convert(.))

#find centroids and add
centroids <- areasALL %>% 
  st_centroid()
areasALL %<>% 
  mutate(initialLocationX = map(centroids$geometry, 1) %>% unlist(),
         initialLocationY = map(centroids$geometry, 2) %>% unlist())

#read in reference file and join
ref <- read_csv(ref_path)
areasALL %<>%
  left_join(ref, by = c("name" = "Area"))

#create ids and fleet ids
ids <- sapply(areasALL$fleetSize, seq) %>% unlist()
areasALL %<>%
  rename(fleetId = name) %>% 
  uncount(fleetSize) %>% 
  mutate(id = ids)

#reorder columns
areasALL %<>%
  relocate(id, fleetId, rideHailManagerId, vehicleType,
           initialLocationX, initialLocationY, shifts,
           geofencePolygon)

#un-'st'-ify areasAll and remove st geometry
areasALL %<>%
  as_tibble() %>% 
  select(-geometry)

###################################################

#write full fleet
areasALL %>% 
  write_csv(paste0(out_dir, "/rhFleet_FULL__DO_NOT_USE.csv"))

####################################################

#read in scenarios and split zones
scenarios <- read_csv(scen_path) %>% 
  mutate(Zones = sapply(.$Zones, strsplit, split = ", "))

#write fleets
for(scen in scenarios$Name){
  zones <- filter(scenarios, Name == scen) %>%
    {.$Zones} %>%
    unlist()
  fleet <- areasALL %>% 
    filter(fleetId %in% zones)
  fleet %>% 
    write_csv(paste0(out_dir, "/rhFleet_", scen, ".csv"))
}
