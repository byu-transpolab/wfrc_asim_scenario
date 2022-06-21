library(tidyverse)

last_run <- 1

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
    paste0(calibration_dir, "/tour_mc_coeffs_RUN", i, ".csv"))) %>%
    filter(coefficient_name %in% targets$coefficient_name) %>%
    select(-constrain)
}

mode_shares <- list()
for (i in 1:last_run){
  mode_shares[[i]] <- read_csv(paste0(
    paste0(calibration_dir, "/output/final_tours_RUN", i, ".csv"))) %>%
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



trips <- read_csv(paste0(calibration_dir, "/output/final_trips_LATEST.csv")) %>%
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
  ))

trips_auto_own <- trips %>% 
  group_by(primary_purpose,autoown,upper_trip_mode) %>%
  summarize(n = n()) %>%
  mutate(share = n/ sum(n)) %>%
  ungroup() %>%
  complete(primary_purpose,autoown,upper_trip_mode, fill = list(n = 0,share = 0)) %>% 
  mutate(coefficient_name = paste(upper_trip_mode,"_ASC_",autoown,"_",primary_purpose,sep = "")) %>%
  select(coefficient_name,share) %>%
  filter(!grepl("sov",coefficient_name)) %>%
  filter(!grepl("drive_transit_ASC_no_auto",coefficient_name)) %>%
  rbind(data.frame("coefficient_name"=c("drive_transit_ASC_no_auto_all","sr2_ASC_no_auto_all"),"share"=c(0,0))) %>% 
  mutate(mode = str_replace(coefficient_name, "_ASC_.+", ""),
         auto_own = case_when(
           str_detect(coefficient_name, "auto_deficient") ~ "auto_deficient",
           str_detect(coefficient_name, "auto_sufficient") ~ "auto_sufficient",
           T ~ "no_auto"
         ),
         purpose = str_replace(coefficient_name, ".+ASC_.+_.+_", "")) %>%
  filter(purpose != "all") %>% 
  relocate(share, .after = purpose)

trip_coeff_tar <- read_csv(paste0(calibration_dir, "/trip_targets/asimtriptargets.csv")) %>% 
  rename("asim_targets" = "target")
trip_auto_tar <- read_csv(paste0(calibration_dir, "/trip_targets/beamtriptargets.csv")) %>% 
  select(-tripTotals) %>% 
  rename("beam_targets" = "tripPercents")
trip_tot_tar <- read_csv(paste0(calibration_dir, "/trip_targets/beamtriptotalshares.csv")) %>% 
  rename("tot_targets" = "tripPercents")

auto_own_comparison <- trips_auto_own %>% 
  full_join(trip_auto_tar, by = c("mode", "auto_own" = "autoWorkRatio", "purpose" = "primary_purpose")) %>% 
  replace_na(list(beam_targets = 0)) %>% 
  mutate(error = share - beam_targets,
         error_pct = case_when(
           error == 0 ~ 0,
           beam_targets == 0 ~ share,
           T ~ error/beam_targets))

tours_old <- read_csv("calibration/tour_mc_no_RH/output/final_tours_RUN10.csv") %>% 
  group_by(tour_type) %>% 
  summarise(n_old = n()) %>% 
  mutate(pct_old = n_old / sum(n_old))
tours_new <- read_csv("output_activitysim/20pct_no_RH/final_tours.csv") %>% 
  group_by(tour_type) %>% 
  summarise(n_new = n()) %>% 
  mutate(pct = n_new / sum(n_new))

tours_old %>%
  left_join(tours_new, by = "tour_type") %>% 
  mutate(change = n_new - n_old,
         pct_change = change / n_old)

