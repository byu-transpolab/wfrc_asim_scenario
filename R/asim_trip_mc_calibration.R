#' Calibrate the trip mode choice coefficient ASC values for ActivitySim
#' 
#' @param asim_households The final households file that is outputted by ActivitySim
#' This file is needed for the auto ownership and worker status of each household.
#' This file should remain constant after each run of the calibration.
#' @param asim_trip_targets The trip targets file was created using 2012 Utah 
#' Travel Survey data. It has rough estimates for tour/trip modal shares by tour
#' purpose, tour mode, and trip modes. This table was created in a different
#' script and matches the ActivitySim mode choice coefficient format.
#' This file should remain constant after each run of the calibration
#' @param asim_final_tours This final tours file that is outputted by ActivitySim.
#' This file is needed because it has all the information needed at the tour level.
#' It should change after each run of the calibration.
#' @param asim_final_trips This final trips file that is outputted by ActivitySim.
#' This file is needed because it has all the information needed at the trip level.
#' It should change after each run of the calibration.
#' @param asim_trip_coeffs This is the trip mode choice coefficient csv value that 
#' houses all the ASC values for the tour mode choice. It should change after each
#' run of the calibration
#' 
#' @return A new 'asim_trip_coeffs' csv file with updated ASC values for the trip
#' mode choice for ActivitySim
#' 
#' @author Chris Day
#' 
#' @export

#' Start up -------------------------------------------------------------------------------------#
#' Below are the libraries and files needed to run this script. Only three of the files need to be
#' changed after each iteration (see comments). 

# libraries needed
library(tidyverse)

calibrate_asim_trips <- function(asim_out_dir, asim_config_dir, calib_dir){
# permanent files
asim_households <- read_csv(paste0(calib_dir, "/final_households.csv"))
asim_trip_targets <- read_csv(paste0(calib_dir, "/asimtriptargets.csv"))

# CHANGE THESE FILES AFTER EACH ACTIVITYSIM RUN
# this file is the output of the ActivitySim tours of the run just completed
asim_final_tours <- read_csv(paste0(asim_out_dir, "/final_tours.csv"))
# this file is the output of the ActivitySim trips of the run just completed
asim_final_trips <- read_csv(paste0(asim_out_dir, "/final_trips.csv"))
# this file is the trip mode choice coefficients used for the run just completed
asim_trip_coeffs <- read_csv(paste0(asim_config_dir, "/trip_mode_choice_coefficients.csv"))


#' Calibration -------------------------------------------------------------------------------------#
#' Run these lines of code to calibrate the ASC values used in the previous run of ActivitySim
#' A new csv file is generated that can be directly used as an input to the next run of ActivitySim

# join the household data and create the auto/worker ownership status column
asimtoursjoin <- left_join(asim_final_tours,asim_households,by = "household_id") %>%
  mutate(autoown = ifelse(auto_ownership == 0, "no_auto", ifelse(auto_ownership < 
                                                                   num_workers, "auto_deficient","auto_sufficient"))
  )

# convert the ActivitySim tour modes to more broad upper level mode choice options of ActivitySim
tour_upper_modes <- convert_to_basic_tour_modes(asimtoursjoin) %>% rename("upper_tour_mode" = newcol)

# join the tour mode data into the final trips data in order to get tour mode values at the trip level
asimtripsjoin <- left_join(asim_final_trips,tour_upper_modes,by="tour_id")

# convert the ActivitySim trip modes to more borad upper level mode choice options of ActivitySim
trips_upper_modes <- convert_to_basic_trip_modes(asimtripsjoin) %>% rename("upper_trip_mode" = newcol)

# summarize the trips taken and calculate the percentage of trips by primary purpose and tour mode
trip_shares <- get_basic_trip_shares(trips_upper_modes)

#' using the previous targets calculated from the household survey, and the trip shares just calculated,
#' calculate the new ASC value
tripascs <- determine_trip_ascs(asim_trip_targets,trip_shares)

#' write the new trip mode choice coefficient file to be used in the next run of ActivitySim
write_csv(tripascs, paste0(asim_config_dir, "/trip_mode_choice_coefficients.csv"))
}

# Functions -------------------------------------------------------------------------------------#
#' this function takes in the lower, more specific tour mode choice options and converts them to the
#' more broad, upper level tour mode choice options of ActivitySim. A broader specifications makes it
#' easier to compare with target values
convert_to_basic_tour_modes <- function(asimjoin){
  asimjoin %>%
    mutate(newcol = case_when(
      tour_mode %in% c("DRIVE_COM","DRIVE_EXP","DRIVE_LOC","DRIVE_LRF","DRIVE_HVY") ~ "drive_transit",
      tour_mode %in% c("WALK_COM","WALK_EXP","WALK_LOC","WALK_LRF","WALK_HVY") ~ "walk_transit",
      tour_mode %in% c("DRIVEALONEFREE","DRIVEALONEPAY") ~ "sov",
      tour_mode %in% c("SHARED2FREE","SHARED2PAY") ~ "sr2",
      tour_mode %in% c("SHARED3FREE","SHARED3PAY") ~ "sr3p",
      tour_mode == "BIKE" ~ "bike",
      tour_mode == "WALK" ~ "walk",
      tour_mode == "TNC_SINGLE" ~ "tnc_single",
      tour_mode == "TNC_SHARED" ~ "tnc_shared",
      tour_mode == "TAXI" ~ "taxi"
    ))
}
#' this function takes in the lower, more specific trip mode choice options and converts them to the
#' more broad, upper level trip mode choice options of ActivitySim. A broader specifications makes it
#' easier to compare with target values
convert_to_basic_trip_modes <- function(asimjoin){
  asimjoin %>%
    mutate(newcol = case_when(
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

#' this function calculates the percentage of trips taken categorized by primary purpose, tour mode, and
#' trip mode. 
get_basic_trip_shares <- function(trips_upper_modes){
  trips_upper_modes %>%
    # group and summarize by correct groupings
    group_by(primary_purpose.x,upper_tour_mode,upper_trip_mode) %>%
    summarize(n = n()) %>%
    mutate(share = n/ sum(n)) %>%
    ungroup() %>%
    # create matching variable name
    mutate(coefficient_name = paste("coef_",upper_tour_mode,"_ASC_",upper_trip_mode,"_",primary_purpose.x,sep = ""),model = share) %>%
    select(coefficient_name,model)
}

#' this function determines the new asc value by using the formula: NewASC = OldASC + ln(Target/Model)
determine_trip_ascs <- function(asim_trip_targets,trip_shares){
  # join together the target values with the model trip shares
  trip_tarshares <- left_join(asim_trip_targets,trip_shares, by = "coefficient_name") %>%
    # if the model share or target share is zero, estimate a new asc value based on the ratio model or target share
    mutate(model = ifelse(is.na(model),0,model),target = ifelse(is.na(target),0,target)) %>%
    mutate(model = ifelse(model == 0, ifelse(target == 0,0,target/4),model)) %>%
    mutate(target = ifelse(target == 0,ifelse(model == 0,0,model/4),target)) %>%
    # calculate the new asc value
    mutate(log = ifelse(target == 0, 0, log(target/model))) %>% 
    mutate(tm = "")
  
  # get all the drive transit and walk transit values, take the average depending on grouped purposes, take the mean of the target
  # and model values, and then calculate the log of the averages (instead of the average of the log as done in tour mc calibration)
  tripdt <- trip_tarshares %>%
    filter(grepl("drive",coefficient_name) | grepl("ASC_walk_transit",coefficient_name)) %>%
    mutate(tm = ifelse(grepl("drive",coefficient_name),"drive_transit","walk_transit")) %>%
    # group together certain purposes
    mutate(newcomb = case_when(
      grepl("_work",coefficient_name) ~ "work",
      grepl("school",coefficient_name) | grepl("univ",coefficient_name) ~ "univ_school",
      grepl("escort",coefficient_name) ~ "escort",
      TRUE ~ "shopping_eatout_othmaint_social_othdiscr_atwork"
    )) %>%
    group_by(tm,newcomb)%>%
    # take the mean of the target and model shares and take the log of that
    summarize(target = mean(target), model = mean(model)) %>%
    mutate(log = log(target/model)) %>%
    # duplicate the rough estimate for all lower level drive transit and walk transit modes
    mutate(lower_trip_mode = list(c("commuter","express","ferry","heavyrail","lightrail"))) %>%
    unnest(c(lower_trip_mode.groups=lower_trip_mode)) %>%
    mutate(coefficient_name = paste("coef_",tm,"_ASC_",lower_trip_mode,"_",newcomb,sep = "")) %>%
    select(coefficient_name,target,model,log)
  
  # join together the normal share table and the weird share table
  better_trip_tarshares <- rbind(trip_tarshares,tripdt)
  
  # left join tables, calculate the final asc value, and select needed columns
  newasc_trip <- left_join(asim_trip_coeffs,better_trip_tarshares,by = "coefficient_name") %>%
    #filter(grepl("ASC",coefficient_name),!grepl("joint",coefficient_name),!grepl("rh",coefficient_name),!grepl("ride",coefficient_name)) %>%
    mutate(value = ifelse(is.na(log),value,log+as.numeric(value))) %>%
    select(coefficient_name,value,constrain) %>%
    mutate(value = ifelse(is.na(value),"",value),
           constrain = ifelse(is.na(constrain),"",constrain))
  new_asc_trip
}

#' copy calibration files each run to have a record
copy_calibration_files <- function(asim_out_dir, asim_config_dir, calib_dir, i){
  if(!dir.exists(paste0(calib_dir, "/output")))
    dir.create(paste0(calib_dir, "/output"), recursive = TRUE)
  file.copy(paste0(asim_config_dir, "/trip_mode_choice_coefficients.csv"),
            paste0(calib_dir, "/trip_mc_coeffs_RUN", i, ".csv"), 
            overwrite = TRUE)
  file.copy(paste0(asim_out_dir, "/final_trips.csv"),
            paste0(calib_dir, "/output/final_trips_RUN", i, ".csv"), 
            overwrite = TRUE)
}

