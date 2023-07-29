library(tidyverse)

counties <- c("Box Elder",
              "Davis",
              "Salt Lake",
              "Utah",
              "Weber - WFRC")

# grand_tot <-
#   read_csv("populationsim/reference/ControlTotal_SE_AllCounties.csv") %>% 
#   filter(YEAR==2019, CO_NAME %in% counties) %>% 
#   select(-YEAR)
# 
# grand_tot %>% 
#   write_csv("populationsim/data/county_controls.csv")

wfrc <-
  read_csv("populationsim/reference/SE_WFRC_2019.csv") %>%
  rename(zone_id = `;TAZID`, HSENROLL = Enrol_High)
filter(CO_NAME != "BOX ELDER")

be <-
  read_csv("populationsim/reference/SE_BOX ELDER_2019.csv") %>%
  rename(zone_id = Index, HSENROLL = Enrol_High) %>%
  filter(DISTLRG == 1) %>%
  mutate(zone_id = `;CO_TAZID` - 3000)

se_data <- bind_rows(wfrc, be)

se_data %>% 
  write_csv("populationsim/data/taz_controls.csv")
