#' Calibrate the tour mode choice coefficient ASC values for ActivitySim
#' 
#' @param asim_households The final households file that is outputted by ActivitySim
#' This file is needed for the auto ownership and worker status of each household.
#' This file should remain constant after each run of the calibration.
#' @param asim_tour_targets The tour targets file was created using 2012 Utah 
#' Travel Survey data. It has rough estimates for tour/trip modal shares by tour
#' purpose, auto ownership, and tour/trip modes. This table was created in a different
#' script and matches the ActivitySim mode choice coefficient format.
#' This file should remain constant after each run of the calibration
#' @param asim_final_tours This final tours file that is outputted by ActivitySim.
#' This file is needed because it has all the information needed at the tour level.
#' It should change after each run of the calibration.
#' @param asim_tour_coeffs This is the tour mode choice coefficient csv value that 
#' houses all the ASC values for the tour mode choice. It should change after each
#' run of the calibration
#' 
#' @return A new 'asim_tour_coeffs' csv file with updated ASC values for the tour
#' mode choice for ActivitySim
#' 
#' @author Chris Day
#' 
#' @export
calibrate_asim_rh <- function(asim_out_dir, asim_config_dir, calib_dir){

#' Start up -------------------------------------------------------------------------------------#
#' Below are the libraries and files needed to run this script. Only two of the files need to be
#' changed after each iteration (see comments). 

# libraries needed
# library(tidyverse)

# Target RH share
target_rh_share <- 0.00138

# this file is the output of the ActivitySim trips of the run just completed
asim_final_trips <- read_csv(paste0(asim_out_dir, "/final_trips.csv"))

# this file is the tour mode choice coefficients used for the run just completed
asim_tour_coeffs <- read_csv(paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"))


#' Calibration -------------------------------------------------------------------------------------#
#' Run these lines of code to calibrate the ASC values used in the previous run of ActivitySim
#' A new csv file is generated that can be directly used as an input to the next run of ActivitySim


# convert the ActivitySim modes to more broad upper level mode choice options of ActivitySim
trip_upper_modes <- convert_to_basic_modes(asim_final_trips)


rh_shares <- trip_upper_modes %>% 
  group_by(upper_trip_mode) %>% 
  summarize(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  filter(str_detect(coefficient_name, "tnc"))

rh_change <- log(target_rh_share / sum(rh_shares$share))

new_tnc <- asim_tour_coeffs %>% 
  filter(str_detect(coefficient_name, "^tnc")) %>% 
  mutate(value = value + rh_change)

newasc <- bind_rows(asim_tour_coeffs %>%
                      filter(str_detect(coefficient_name, "^tnc", negate = T)),
                new_tnc)


# overwrite the config for asim
write_csv(newasc, paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"))
}


# Functions -------------------------------------------------------------------------------------#
#' this function takes in the lower, more specific tour mode choice options and converts them to the
#' more broad, upper level tour mode choice options of ActivitySim. A broader specifications makes it
#' easier to compare with target values
convert_to_basic_modes <- function(modes){
  modes %>%
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
}

#' copy calibration files each run to have a record
copy_calibration_files_tours <- function(asim_out_dir, asim_config_dir, calib_dir, i){
  if(!dir.exists(paste0(calib_dir, "/output")))
    dir.create(paste0(calib_dir, "/output"), recursive = TRUE)
  file.copy(paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"),
            paste0(calib_dir, "/tour_mc_coeffs_RUN", i, ".csv"), 
            overwrite = TRUE)
  file.copy(paste0(asim_out_dir, "/final_tours.csv"),
            paste0(calib_dir, "/output/final_tours_RUN", i, ".csv"), 
            overwrite = TRUE)
  file.copy(paste0(asim_out_dir, "/final_trips.csv"),
            paste0(calib_dir, "/output/final_trips_RUN", i, ".csv"),
            overwrite = TRUE)
}

#------------------------------------------------------------------------------------------------#
