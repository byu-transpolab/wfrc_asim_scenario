library(tidyverse)

last_run <- 7

targets <- read_csv("calibration/tour_mc/asimtourtargets.csv") %>% 
  mutate(mode = str_replace(coefficient_name, "_ASC_.+", ""),
         auto_own = case_when(
           str_detect(coefficient_name, "auto_deficient") ~ "auto_deficient",
           str_detect(coefficient_name, "auto_sufficient") ~ "auto_sufficient",
           T ~ "no_auto"
         ),
         purpose = str_replace(coefficient_name, ".+ASC_.+_.+_", "")) %>% 
  filter(purpose != "all")

hh <- read_csv("output_activitysim/20pct/final_households.csv")

source("R/asim_tour_mc_calibration.R")

coeffs <- list()
for (i in 1:last_run) {
  coeffs[[i]] <- read_csv(paste0(
    "calibration/tour_mc/tour_mode_choice_coefficients_run", i, ".csv")) %>%
    filter(coefficient_name %in% targets$coefficient_name) %>%
    select(-constrain)
}

mode_shares <- list()
for (i in 1:last_run){
  mode_shares[[i]] <- read_csv(paste0(
    "calibration/tour_mc/output/final_tours_run", i, ".csv")) %>%
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
