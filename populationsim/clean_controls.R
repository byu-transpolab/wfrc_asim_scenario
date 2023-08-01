library(tidyverse)

########
wfrc_se <- "populationsim/reference/SE_WFRC_2019.csv"
be_se <- "populationsim/reference/SE_BOX ELDER_2019.csv"
out_file <- "populationsim/base2019/data/taz_controls.csv"

# landuse_se <- "populationsim/reference/SE_prison.csv"
########

be <-
  read_csv(be_se) %>%
  filter(DLRG_NAME == "Box Elder - WFRC") %>% 
  select(-`;CO_TAZID`) %>% 
  rename(zone_id = Index, HSENROLL = Enrol_High)

be_zones <- range(be$zone_id)[2]

wfrc <-
  read_csv(wfrc_se) %>%
  select(-CO_TAZID) %>% 
  rename(zone_id = `;TAZID`, HSENROLL = Enrol_High) %>% 
  filter(zone_id > be_zones)



se_data <-
  bind_rows(wfrc, be) %>% 
  arrange(zone_id)

se_data %>% 
  write_csv(out_file)
