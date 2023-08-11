library(tidyverse)
library(ggspatial)
library(sf)

taz <- st_read("data/WFRC_TAZ.geojson")
se <- read_csv("data/taz_se/taz_se_2019_all.csv")
synth <- read_csv("populationsim/output/2019/synthetic_households.csv")

synth_se <- synth %>% 
  count(TAZ, name = "tothh_synth") %>% 
  arrange(TAZ)

comp <- se %>% 
  select(zone_id, TOTHH) %>% 
  rename(tothh_se = TOTHH) %>% 
  full_join(synth_se, by = c("zone_id" = "TAZ")) %>% 
  replace_na(list("tothh_synth" = 0)) %>% 
  mutate(change = trunc(tothh_synth - tothh_se))

taz %>% 
  full_join(comp, by = c("TAZID" = "zone_id")) %>% 
  select(TAZID, tothh_se:change) %>% 
  # filter(change != 0) %>% 
  # mutate(change = na_if(change, 0)) %>% 
  ggplot() +
  geom_sf(aes(fill = change))


wfrcbe <- read_csv("data/taz_se/TAZ_SE_2019_WFRC.csv") %>% 
  filter(CO_NAME == "BOX ELDER") %>% 
  rename(zone_id = `;TAZID`) %>% 
  select(-(RETL:CO_NAME))
be <- read_csv("data/taz_se/TAZ_SE_2019_BOXELDER.csv") %>% 
  filter(DLRG_NAME == "Box Elder - WFRC") %>% 
  rename(zone_id = Index, CO_TAZID = `;CO_TAZID`) %>% 
  select(-(RETL:CITY_NAME))

combined <- full_join(wfrcbe, be, by = "zone_id", suffix = c("_wfrc", "_be"))

na_tazs <- combined %>% 
  filter(is.na(CO_TAZID_be)) %>% 
  {.$zone_id}


taz %>% 
  mutate(in_be_se_data = ifelse(TAZID %in% na_tazs, FALSE, TRUE)) %>% 
  ggplot() +
  geom_sf(aes(fill = in_be_se_data))

taz %>% 
  right_join(combined, by = c("TAZID" = "zone_id")) %>% 
  ggplot() +
  geom_sf(aes(fill = TOTHH_be - TOTHH_wfrc))
