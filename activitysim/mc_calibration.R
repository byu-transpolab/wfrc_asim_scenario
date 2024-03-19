library(tidyverse)

odir <- "output"
cdir <- "configs_mc_calibration"

iters <- 5
tolerance <- 0.05

mc_targets <- read_csv("../data/mc_targets.csv")

make_asim_purpose <- function(df) {
  df %>%
    group_by(tour_id) %>%
    mutate(
      #If a tour doesn't start at work, assume it starts at home
      trip_purpose = replace(trip_purpose, 1, "home"),
      home_based = case_when(
        tour_purpose == "atwork" ~ FALSE,
        trip_purpose == "home" ~ TRUE,
        TRUE ~ FALSE
      ),
      purpose = case_when(
        !home_based ~ "nhb",
        tour_purpose == "work" ~ "hbw",
        TRUE ~ "hbo"
      ) %>%
        factor(c("hbw", "hbo", "nhb"))
    ) %>%
    pull(purpose)
}

asim_mode_translation <- function(mode) {
  case_match(
    mode,
    c("heavy_rail", "commuter_rail") ~ "crt",
    "sr3p" ~ "sr3",
    c("walk_light_rail", "drive_light_rail", "light_rail") ~ "lrt",
    c("taxi", "tnc_single", "tnc_shared", "rh") ~ "TNC",
    NA ~ NA,
    .default = mode
  )
}

asim_purpose_translation <- function(purpose) {
  case_match(
    purpose,
    "work" ~ "hbw",
    "atwork" ~ "nhb",
    NA ~ NA,
    .default = "hbo"
  )
}

for(i in 1:iters) {
  
  prev_iter <- list.files(cdir, recursive = FALSE) %>% 
    str_extract("^\\d+_trip_mode_choice_coefficients.csv") %>% 
    str_extract("\\d+") %>% 
    as.integer() %>% 
    max(na.rm = TRUE)
  
  iter <- prev_iter + 1
  
  prev_trips_file <- file.path(
    odir,
    paste("calibrate_mc", prev_iter, sep = "_"),
    "final_trips.csv")
  
  prev_trips_raw <- prev_trips_file %>% 
    read_csv()
  
  prev_trips <- prev_trips_raw %>% 
    mutate(
      mode = case_when(
        str_detect(trip_mode, "DRIVEALONE") ~ "drive_alone",
        str_detect(trip_mode, "SHARED2") ~ "sr2",
        str_detect(trip_mode, "SHARED3") ~ "sr3",
        str_detect(trip_mode, "TNC") ~ "TNC",
        str_detect(trip_mode, "TAXI") ~ "TNC",
        str_detect(trip_mode, "LOC") ~ "local_bus",
        str_detect(trip_mode, "EXP") ~ "express_bus",
        str_detect(trip_mode, "COM|HVY") ~ "crt",
        str_detect(trip_mode, "LRF") ~ "lrt",
        trip_mode %in% c("WALK", "BIKE") ~ str_to_lower(trip_mode)),
      purpose =  make_asim_purpose(
        select(
          .,
          tour_id,
          tour_purpose = primary_purpose,
          trip_purpose = purpose)),
      .keep = "none"
    ) %>% 
    count(purpose, mode, name = "asim_trips") %>% 
    mutate(
      asim_share = asim_trips/sum(asim_trips),
      .by = purpose
    )
  
  adjustments <- prev_trips %>% 
    full_join(mc_targets, join_by(purpose, mode)) %>% 
    mutate(wfrc_share = if_else(
      mode != "TNC",
      wfrc_share,
      case_match(
        purpose,
        "hbw" ~ 0.00015, #Made-up numbers
        "hbo" ~ 0.0038,  #roughly based on Chris's thesis
        "nhb" ~ 0.004    #and common sense
      ))) %>% 
    mutate(
      error = 1 - asim_share/wfrc_share,
      close_enough = abs(error) < tolerance,
      adjust = log(wfrc_share/asim_share)) %>% 
    select(purpose, mode, adjust, close_enough) %>% 
    filter(mode != "drive_alone")


  prev_tour_coeffs <- file.path(
    cdir,
    paste(prev_iter, "tour_mode_choice_coefficients.csv", sep = "_")) %>% 
    read_csv()
  
  new_tour_coeffs <- prev_tour_coeffs %>% 
    mutate(
      mode = case_when(
        !str_detect(coefficient_name, "ASC") ~ NA,
        TRUE ~ str_remove(coefficient_name, "_ASC.+") %>% 
          str_remove("^joint_") %>% 
          str_remove("_CBD")
      ),
      purpose = case_when(
        str_detect(coefficient_name, "joint") ~ NA,
        !str_detect(coefficient_name, "ASC") ~ NA,
        TRUE ~ str_remove(coefficient_name, ".+ASC_") %>% 
          str_remove(".*auto_") %>% 
          str_remove(".*sufficient_?") %>% 
          str_remove(".*deficient_?")) %>% 
        str_remove("_.+")
    ) %>% 
    mutate(
      mode = asim_mode_translation(mode),
      purpose = asim_purpose_translation(purpose)) %>% 
    left_join(adjustments, join_by(mode, purpose)) %>% 
    mutate(
      adjust = replace_na(adjust, 0), 
      across(c(close_enough, constrain), \(x) replace_na(x, FALSE))
    ) %>% 
    mutate(value2 = case_when(
      close_enough ~ value,
      constrain ~ value,
      TRUE ~ value + adjust)) %>% 
    select(coefficient_name, value, constrain)
  
  new_tour_coeffs_file <- paste(iter, "tour_mode_choice_coefficients.csv", sep = "_")
  
  write_csv(new_tour_coeffs, file.path(
    cdir, new_tour_coeffs_file))
  
  
  prev_trip_coeffs <- file.path(
    cdir,
    paste(prev_iter, "trip_mode_choice_coefficients.csv", sep = "_")) %>% 
    read_csv()
  
  # new_trip_coeffs <- 
  prev_trip_coeffs %>% 
    filter(!str_detect(coefficient_name, "#")) %>% 
    mutate(constrain = as.logical(constrain)) %>% 
    mutate(
      value = as.numeric(value),
      mode = case_when(
        !str_detect(coefficient_name, "ASC") ~ NA,
        TRUE ~ str_remove(coefficient_name, "^.+_ASC_") %>% 
          str_remove("_.+")
      ),
      purpose = case_when(
        !str_detect(coefficient_name, "ASC") ~ NA,
        # as.logical(constrain) ~ NA,
        # mode %in% c("rh") ~ NA,
        TRUE ~ str_remove(coefficient_name, ".+ASC_.+?_")) %>% 
        str_remove(".+?_") %>% 
        str_remove("_.+")
    ) %>% 
    mutate(
      mode = case_match(
        mode,
        "rh" ~ "tnc_single",
        "commuter" ~ "commuter_rail",
        "express" ~ "express_bus",
        "heavyrail" ~ "heavy_rail",
        "lightrail" ~ "light_rail",
        "tnc" ~ "tnc_single",
        .default = mode
      )
    ) %>% 
    mutate(
      mode = asim_mode_translation(mode),
      purpose = asim_purpose_translation(purpose)) %>% 
    left_join(adjustments, join_by(mode, purpose)) %>% 
    mutate(
      adjust = replace_na(adjust, 0), 
      across(c(close_enough, constrain), \(x) replace_na(x, FALSE))
    ) %>% 
    mutate(value2 = case_when(
      constrain ~ value,
      close_enough ~ value,
      TRUE ~ value + adjust)) %>% 
    select(coefficient_name, value, constrain)
  
  new_trip_coeffs_file <- paste(iter, "trip_mode_choice_coefficients.csv", sep = "_")
  
  write_csv(new_trip_coeffs, file.path(
    cdir, new_trip_coeffs_file))
  
  
  file.path(cdir, "tour_mode_choice.yaml") %>%
    read_lines() %>% 
    str_replace("COEFFICIENTS:.+", paste("COEFFICIENTS:", new_tour_coeffs_file)) %>% 
    write_lines(file.path(cdir, "tour_mode_choice.yaml"))
  
  file.path(cdir, "trip_mode_choice.yaml") %>%
    read_lines() %>% 
    str_replace("COEFFICIENTS:.+", paste("COEFFICIENTS:", new_trip_coeffs_file)) %>% 
    write_lines(file.path(cdir, "trip_mode_choice.yaml"))
  
  
  out_dir <- file.path(odir, paste0("calibrate_mc_", iter))  
  dir.create(out_dir)
  
  read_lines("calibrate_mc.sh") %>% 
    str_replace("-o output/.+", paste("-o", out_dir)) %>% 
    write_lines("calibrate_mc.sh")
  
  system2("./calibrate_mc.sh")
  
}
