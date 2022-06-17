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
calibrate_asim_tours <- function(asim_out_dir, asim_config_dir, calib_dir, i){

#' Start up -------------------------------------------------------------------------------------#
#' Below are the libraries and files needed to run this script. Only two of the files need to be
#' changed after each iteration (see comments). 

# libraries needed
# library(tidyverse)

# permanent files
asim_households <- read_csv(paste0(asim_out_dir, "/final_households.csv"))
asim_tour_targets <- read_csv(paste0(calib_dir, "/asimtourtargets.csv"))

# this file is the output of the ActivitySim tours of the run just completed
asim_final_tours <- read_csv(paste0(asim_out_dir, "/final_tours.csv"))

# this file is the tour mode choice coefficients used for the run just completed
asim_tour_coeffs <- read_csv(paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"))


#' Calibration -------------------------------------------------------------------------------------#
#' Run these lines of code to calibrate the ASC values used in the previous run of ActivitySim
#' A new csv file is generated that can be directly used as an input to the next run of ActivitySim

# join the household data and create the auto/worker ownership status column
asimtoursjoin <- left_join(asim_final_tours,asim_households,by = "household_id") %>%
  mutate(autoown = ifelse(auto_ownership == 0, "no_auto", ifelse(auto_ownership < 
         num_workers, "auto_deficient","auto_sufficient"))
  )

# convert the ActivitySim modes to more broad upper level mode choice options of ActivitySim
tour_upper_modes <- convert_to_basic_modes(asimtoursjoin)

# summarize the tours taken and calculate the percentage of tours by primary purpose and auto ownership
basic_tour_shares <- get_basic_tour_shares(tour_upper_modes)

#' using the previous targets calculated from the household survey, and the tour shares just calculated,
#' calculate the new ASC value
newasc <- determine_new_asc(asim_tour_targets,basic_tour_shares,asim_tour_coeffs)

# overwrite the config for asim
write_csv(newasc, paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"))
}


# Functions -------------------------------------------------------------------------------------#
#' this function takes in the lower, more specific tour mode choice options and converts them to the
#' more broad, upper level tour mode choice options of ActivitySim. A broader specifications makes it
#' easier to compare with target values
convert_to_basic_modes <- function(asimtoursjoin){
  asimtoursjoin %>%
    mutate(upper_tour_mode = case_when(
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

#' this function calculates the percentage of tours taken categorized by primary purpose, auto ownerhsip, and
#' tour mode. 
get_basic_tour_shares <- function(tour_upper_modes){
 tour_upper_modes %>%
    # group and summarize by correct groupings
    group_by(primary_purpose,autoown,upper_tour_mode) %>%
    summarize(n = n()) %>%
    mutate(share = n/ sum(n)) %>%
    ungroup() %>%
    # fill in any missing combinations with a percentage of 0
    complete(primary_purpose,autoown,upper_tour_mode, fill = list(n = 0,share = 0)) %>%
    # create matching variable name
    mutate(coefficient_name = paste(upper_tour_mode,"_ASC_",autoown,"_",primary_purpose,sep = ""),model = share) %>%
    select(coefficient_name,model) %>%
    # filter out sov and drive transit no auto tours
    filter(!grepl("sov",coefficient_name)) %>%
    filter(!grepl("drive_transit_ASC_no_auto",coefficient_name)) %>%
    # create new variables to match the original coefficient file
    rbind(data.frame("coefficient_name"=c("drive_transit_ASC_no_auto_all","sr2_ASC_no_auto_all"),"model"=c(0,0)))
} 

#' this function determines the new asc value by using the formula: NewASC = OldASC + ln(Target/Model)
determine_new_asc <- function(asim_tour_targets,basic_tour_shares,asim_tour_coeffs){
  # join together the old table with the new values
  tarshares <- left_join(asim_tour_targets,basic_tour_shares, by = "coefficient_name") %>%
    # if the model share or target share is zero, estimate a new asc value based on the ratio model or target share
    mutate(model = ifelse(model == 0, ifelse(target == 0,0,target/4),model)) %>%
    mutate(target = ifelse(target == 0,ifelse(model == 0,0,model/4),target)) %>%
    # calculate the new asc value
    mutate(log = ifelse(target == 0, 0, log(target/model)))
  
  # get all the sr2 no auto asc values, take the average, and then assign that to the sr2 no auto all asc value
  sr2noauto <- tarshares %>%
    filter(grepl("sr2_ASC_no_auto",coefficient_name)) %>%
    summarize(log = mean(log),target =mean(target),model = mean(model)) %>%
    mutate(coefficient_name = "sr2_ASC_no_auto_all")
  
  # filter out the sr2 no auto asc values and attach the new value that will be used for all cases
  tarshares2 <- tarshares %>%
    filter(!grepl("sr2_ASC_no_auto",coefficient_name)) %>%
    rbind(sr2noauto)
  
  # left join tables and select needed columns
  newasc <- left_join(asim_tour_coeffs,tarshares2,by = "coefficient_name") %>%
    mutate(value = ifelse(is.na(log),value,log+as.numeric(value))) %>%
    select(-target,-model,-log)
  
  newasc
}

#' copy calibration files each run to have a record
copy_calibration_files <- function(asim_out_dir, asim_config_dir, calib_dir, i){
  if(!dir.exists(paste0(calib_dir, "/output")))
    dir.create(paste0(calib_dir, "/output"), recursive = TRUE)
  file.copy(paste0(asim_config_dir, "/tour_mode_choice_coefficients.csv"),
            paste0(calib_dir, "/tour_mc_coeffs_RUN", i, ".csv"), 
            overwrite = TRUE)
  file.copy(paste0(asim_out_dir, "/final_tours.csv"),
            paste0(calib_dir, "/output/final_tours_RUN", i, ".csv"), 
            overwrite = TRUE)
  file.copy(paste0(asim_out_dir, "/final_trips.csv"),
            paste0(calib_dir, "/output/final_trips_LATEST.csv"),
            overwrite = TRUE)
}

#------------------------------------------------------------------------------------------------#