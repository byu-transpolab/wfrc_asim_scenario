library(tidyverse)

odir <- "output"
cdir <- "configs_mc_calibration"

iter <- 0 # set later in script

mc_targets <- read_csv("../data/cube_mc_base_2019.csv")

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

while(iter <= 5) {
  
  prev_iter <- list.files(cdir, recursive = FALSE) %>% 
    str_extract("\\d+_trip_mode_choice_coefficients.csv") %>% 
    str_extract("\\d+") %>% 
    as.integer() %>% 
    max(na.rm = TRUE)
  
  iter <- prev_iter + 1
  
  prev_trips_file <- read_csv(file.path(
    odir,
    paste("calibrate_mc", prev_iter, sep = "_"),
    "final_trips.csv"))
  
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
    # pivot_longer(c(asim_trips, cube_trips), names_to = "model", values_to = "trips") %>% 
    # pivot_wider(names_from = mode, values_from = trips) %>% 
    # mutate(light_rail = walk_light_rail + drive_light_rail) %>% 
    # pivot_longer(-model, names_to = "mode", values_to = "trips") %>% 
    # mutate(model = str_remove(model, "_trips"), .keep = "unused") %>% 
    # group_by(model) %>% 
    # mutate(share = trips/sum(trips)) %>% 
    # select(-trips) %>% 
    # pivot_wider(names_from = model, values_from = share, names_prefix = "share_") %>% 
    mutate(adjust = log(wfrc_share/asim_share)) %>% 
    select(purpose, mode, adjust) %>% 
    filter(mode != "drive_alone") #%>% 
    # full_join(asim_mode_translation) %>% 
    # mutate(coef_mode = if_else(is.na(coef_mode), mode, coef_mode)) %>% 
    # select(mode = coef_mode, adjust)
  
  
  prev_tour_coeffs <- file.path(
    cdir,
    paste(prev_iter, "tour_mode_choice_coefficients.csv", sep = "_")) %>% 
    read_csv()
  
  new_tour_coeffs <- prev_tour_coeffs %>% 
    mutate(
      mode = case_when(
        !str_detect(coefficient_name, "ASC") ~ "",
        TRUE ~ str_remove(coefficient_name, "_ASC.+") %>% 
          str_remove("^joint_") %>% 
          str_remove("_CBD")
      )
    ) %>% 
    left_join(adjustments) %>% 
    mutate(adjust = replace_na(adjust, 0)) %>% 
    mutate(value = case_when(
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
  
  new_trip_coeffs <- prev_trip_coeffs %>% 
    filter(!str_detect(coefficient_name, "#")) %>% 
    mutate(
      value = as.numeric(value),
      mode = case_when(
        !str_detect(coefficient_name, "ASC") ~ "",
        TRUE ~ str_remove(coefficient_name, "^.+_ASC_") %>% 
          str_remove("_.+")
      )
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
    left_join(adjustments) %>% 
    mutate(adjust = replace_na(adjust, 0)) %>% 
    mutate(value = case_when(
      constrain ~ value,
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
