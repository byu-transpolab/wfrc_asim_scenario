library(tidyverse)

odir <- "output"
cdir <- "configs_mc_calibration"

iter <- 0 # set later in script

# get_cube_targets <- function(cube_omx, out_file) {
#   library(omxr)
#   
#   cube <- cube_omx %>% 
#     read_all_omx() 
#   cube %>%
#     mutate(
#       DRIVEALONEFREE = DA,
#       sr2 = SR2,
#       sr3p = SR3p,
#       local_bus = rowSums(across(c(contains("LCL"), contains("BRT")))),
#       express_bus = rowSums(across(contains("EXP"))),
#       commuter_rail = rowSums(across(contains("CRT"))),
#       walk_transit = wTRN,
#       drive_transit = dTRN,
#       walk_light_rail = wLRT,
#       drive_light_rail = dLRT,
#       other_transit = transit -
#         local_bus - express_bus - commuter_rail - walk_light_rail - drive_light_rail
#     ) %>% 
#     select(
#       DRIVEALONEFREE, sr2, sr3p,
#       bike, walk,
#       walk_transit, drive_transit, other_transit,
#       local_bus, express_bus,
#       commuter_rail, walk_light_rail, drive_light_rail) %>%
#     # pivot_longer(everything(), names_to = "mode", values_to = "cube_trips") %>% 
#     # arrange(mode)
#     # #mutate(other_transit = transit - sum(dBRT, dCOR, wBRT, wCOR, na.rm = TRUE)) %>%
#     # mutate(
#     #   other_transit = transit - dCOR - wCOR,
#     #   dLOC = dLCL + dBRT, wLOC = wLCL + wBRT) %>%
#     # select(
#     #   #origin, destination,
#     #   DA, SR2, SR3p,
#     #   dCRT, dEXP, dLOC, dLRT,
#     #   wCRT, wEXP, wLOC, wLRT,
#     #   other_transit, bike, walk) %>% 
#     colSums() %>% 
#     magrittr::divide_by(100) %>% 
#     enframe(name = "mode", value = "cube_trips") %>% 
#     write_csv(out_file)
# }

asim_mode_translation <- read_csv("../data/asim_mode_translation.csv")

mc_targets <- read_csv("../data/cube_mc_base_2019.csv")

while(iter <= 5) {
  
  prev_iter <- list.files(cdir, recursive = FALSE) %>% 
    str_extract("\\d+_trip_mode_choice_coefficients.csv") %>% 
    {.[!is.na(.)]} %>% 
    str_extract("\\d+") %>% 
    as.integer() %>% 
    max()
  
  iter <- prev_iter + 1
  
  prev_trips_file <- read_csv(file.path(
			odir,
			paste("calibrate_mc", prev_iter, sep = "_"),
			"final_trips.csv"))

    prev_trips <- prev_trips_file %>% 
    mutate(asim_mode = case_when(
      trip_mode == "SHARED2FREE" ~ "sr2",
      trip_mode == "SHARED3FREE" ~ "sr3p",
      trip_mode %in% c(
        "WALK", "BIKE", "TNC_SINGLE", "TNC_SHARED", "TAXI"
      ) ~ str_to_lower(trip_mode),
      TRUE ~ trip_mode
    )) %>% 
    count(asim_mode, name = "asim_trips") %>% 
      pivot_wider(names_from = asim_mode, values_from = asim_trips) %>% 
      mutate(
        walk_transit = rowSums(across(starts_with("WALK_"))),
        drive_transit = rowSums(across(starts_with("DRIVE_"))),
        local_bus = rowSums(across(ends_with("_LOC"))),
        express_bus = rowSums(across(ends_with("_EXP"))),
        # heavy_rail = rowSums(across(ends_with("_HVY"))),
        commuter_rail = rowSums(across(c(ends_with("_COM"), ends_with("_HVY")))),
        walk_light_rail = WALK_LRF,
        drive_light_rail = DRIVE_LRF,
        other_transit = rowSums(across(c(contains("taxi"), starts_with("tnc_")))),
        .keep = "unused"
      ) %>% 
      pivot_longer(everything(), names_to = "mode", values_to = "asim_trips")
  
  adjustments <- prev_trips %>% 
    full_join(mc_targets, join_by(mode)) %>% 
    pivot_longer(c(asim_trips, cube_trips), names_to = "model", values_to = "trips") %>% 
    pivot_wider(names_from = mode, values_from = trips) %>% 
    mutate(light_rail = walk_light_rail + drive_light_rail) %>% 
    pivot_longer(-model, names_to = "mode", values_to = "trips") %>% 
    mutate(model = str_remove(model, "_trips"), .keep = "unused") %>% 
    group_by(model) %>% 
    mutate(share = trips/sum(trips)) %>% 
    select(-trips) %>% 
    pivot_wider(names_from = model, values_from = share, names_prefix = "share_") %>% 
    mutate(adjust = log(share_cube/share_asim)) %>% 
    select(mode, adjust) %>% 
    filter(mode != "DRIVEALONEFREE") %>% 
    full_join(asim_mode_translation) %>% 
    mutate(coef_mode = if_else(is.na(coef_mode), mode, coef_mode)) %>% 
    select(mode = coef_mode, adjust)
  
  
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
    mutate(value = value + adjust) %>% 
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
    mutate(value = value + adjust) %>% 
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
