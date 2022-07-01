library(tidyverse)

last_run <- 10

calibration_dir <- "calibration/tour_mc"

rh_target <- 0.00138

shares <- list()
for(i in 0:last_run){
  shares[[i+1]] <- read_csv(paste0(
    calibration_dir, "/output/final_trips_RUN", i, ".csv")) %>% 
    mutate(upper_trip_mode = case_when(
      trip_mode %in% c("DRIVE_COM","DRIVE_EXP","DRIVE_LOC","DRIVE_LRF","DRIVE_HVY") ~ "drive_transit",
      trip_mode %in% c("WALK_COM","WALK_EXP","WALK_LOC","WALK_LRF","WALK_HVY") ~ "walk_transit",
      trip_mode %in% c("DRIVEALONEFREE","DRIVEALONEPAY") ~ "sov",
      trip_mode %in% c("SHARED2FREE","SHARED2PAY") ~ "sr2",
      trip_mode %in% c("SHARED3FREE","SHARED3PAY") ~ "sr3p",
      trip_mode == "BIKE" ~ "bike",
      trip_mode == "WALK" ~ "walk",
      trip_mode %in% c("TNC_SINGLE", "TNC_SHARED", "TAXI") ~ "rh"
    )) %>% 
  group_by(upper_trip_mode) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n / sum(n))
}

all_runs <- bind_rows(shares, .id = "iteration") %>% 
  filter(!upper_trip_mode %in% c("sov"))
all_runs$iteration <- as.integer(all_runs$iteration) %>%
  subtract(1)

ggplot(all_runs) +
  geom_line(aes(x = iteration, y = pct, color = upper_trip_mode)) +
  geom_hline(yintercept = rh_target, linetype = "dashed")