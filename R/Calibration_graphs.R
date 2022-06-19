library(tidyverse)

last_run <- 10

calibration_dir <- "calibration/tour_mc_no_RH"
asim_out_dir <- "output_activitysim/20pct_no_RH"

targets <- read_csv(paste0(calibration_dir, "/asimtourtargets.csv")) %>% 
  mutate(mode = str_replace(coefficient_name, "_ASC_.+", ""),
         auto_own = case_when(
           str_detect(coefficient_name, "auto_deficient") ~ "auto_deficient",
           str_detect(coefficient_name, "auto_sufficient") ~ "auto_sufficient",
           T ~ "no_auto"
         ),
         purpose = str_replace(coefficient_name, ".+ASC_.+_.+_", "")) %>% 
  filter(purpose != "all")

hh <- read_csv(paste0(asim_out_dir, "/final_households.csv"))

source("R/asim_tour_mc_calibration.R")

coeffs <- list()
for (i in 1:last_run) {
  coeffs[[i]] <- read_csv(paste0(
    paste0(calibration_dir, "/tour_mode_choice_coefficients_run", i, ".csv"))) %>%
    filter(coefficient_name %in% targets$coefficient_name) %>%
    select(-constrain)
}

mode_shares <- list()
for (i in 1:last_run){
  mode_shares[[i]] <- read_csv(paste0(
    paste0(calibration_dir, "/output/final_tours_run", i, ".csv"))) %>%
    left_join(hh, by = "household_id") %>%
    mutate(autoown = ifelse(auto_ownership == 0, "no_auto",
      ifelse(auto_ownership < num_workers, "auto_deficient", "auto_sufficient")
      )
    ) %>% 
    convert_to_basic_modes() %>% 
    get_basic_tour_shares() %>% 
    mutate(mode = str_replace(coefficient_name, "_ASC_.+", ""),
           auto_own = case_when(
             str_detect(coefficient_name, "auto_deficient") ~ "auto_deficient",
             str_detect(coefficient_name, "auto_sufficient") ~ "auto_sufficient",
             T ~ "no_auto"
           ),
           purpose = str_replace(coefficient_name, ".+ASC_.+_.+_", "")) %>%
    filter(mode %in% (targets$mode %>% unique()),
           purpose != "all")

}

calibration_shares <- bind_rows(mode_shares, .id = "iteration")
calibration_shares$iteration <- as.integer(calibration_shares$iteration)

calibration_shares %>% 
  ggplot() +
  geom_line(aes(x = iteration, y = model, color = mode)) +
  geom_hline(data = targets, aes(yintercept = target, color = mode), lty = "dashed") +
  facet_grid(auto_own ~ purpose)



trips_share <- read_csv(paste0(calibration_dir, "/output/final_trips_LATEST.csv")) %>%
  left_join(hh, by = "household_id") %>%
  mutate(autoown = ifelse(
    auto_ownership == 0,
    "no_auto",
    ifelse(auto_ownership < num_workers, "auto_deficient", "auto_sufficient"))
    ) %>%
  mutate(upper_trip_mode = case_when(
    trip_mode %in% c("DRIVE_COM","DRIVE_EXP","DRIVE_LOC","DRIVE_LRF","DRIVE_HVY") ~ "drive_transit",
    trip_mode %in% c("WALK_COM","WALK_EXP","WALK_LOC","WALK_LRF","WALK_HVY") ~ "walk_transit",
    trip_mode %in% c("DRIVEALONEFREE","DRIVEALONEPAY") ~ "sov",
    trip_mode %in% c("SHARED2FREE","SHARED2PAY") ~ "sr2",
    trip_mode %in% c("SHARED3FREE","SHARED3PAY") ~ "sr3p",
    trip_mode == "BIKE" ~ "bike",
    trip_mode == "WALK" ~ "walk",
    trip_mode == "TNC_SINGLE" ~ "tnc_single",
    trip_mode == "TNC_SHARED" ~ "tnc_shared",
    trip_mode == "TAXI" ~ "taxi"
  )) %>%
  group_by(primary_purpose,autoown,upper_trip_mode) %>%
  summarize(n = n()) %>%
  mutate(share = n/ sum(n)) %>%
  ungroup() %>%
  complete(primary_purpose,autoown,upper_trip_mode, fill = list(n = 0,share = 0)) %>%
  mutate(coefficient_name = paste(upper_trip_mode,"_ASC_",autoown,"_",primary_purpose,sep = ""),model = share) %>%
  select(coefficient_name,model) %>%
  filter(!grepl("sov",coefficient_name)) %>%
  filter(!grepl("drive_transit_ASC_no_auto",coefficient_name)) %>%
  rbind(data.frame("coefficient_name"=c("drive_transit_ASC_no_auto_all","sr2_ASC_no_auto_all"),"model"=c(0,0))) %>% 
  mutate(mode = str_replace(coefficient_name, "_ASC_.+", ""),
         auto_own = case_when(
           str_detect(coefficient_name, "auto_deficient") ~ "auto_deficient",
           str_detect(coefficient_name, "auto_sufficient") ~ "auto_sufficient",
           T ~ "no_auto"
         ),
         purpose = str_replace(coefficient_name, ".+ASC_.+_.+_", "")) %>%
  filter(purpose != "all") %>% 
  select(-coefficient_name) %>% 
  relocate(model, .after = purpose) %>% 
  rename("share" = "model")

