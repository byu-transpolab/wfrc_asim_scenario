calibrate_asim_rh_trips <- function(asim_out_dir, asim_config_dir, calib_dir){

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
	asim_trip_coeffs <- read_csv(paste0(asim_config_dir, "/trip_mode_choice_coefficients.csv"))
	asim_trip_coeffs$value <- as.numeric(asim_trip_coeffs$value)


	#' Calibration -------------------------------------------------------------------------------------#
	#' Run these lines of code to calibrate the ASC values used in the previous run of ActivitySim
	#' A new csv file is generated that can be directly used as an input to the next run of ActivitySim


	# convert the ActivitySim modes to more broad upper level mode choice options of ActivitySim
	trip_upper_modes <- convert_to_basic_modes(asim_final_trips)


	rh_shares <- trip_upper_modes %>%
		group_by(upper_trip_mode) %>%
		summarize(n = n()) %>%
		mutate(share = n / sum(n)) %>%
		filter(str_detect(upper_trip_mode, "tnc"))

	rh_change <- log(target_rh_share / sum(rh_shares$share))

	print(paste("RH coefficients will change by", rh_change))

	fixed_asc <- asim_trip_coeffs %>%
		filter(!str_detect(coefficient_name, "_ASC_tnc|_ASC_rh") | str_detect(coefficient_name, "coef_joint"))
	changed_asc <- asim_trip_coeffs %>%
		filter(str_detect(coefficient_name, "_ASC_tnc|_ASC_rh") & !str_detect(coefficient_name, "coef_joint")) %>%
		mutate(value = value + rh_change)

	newasc <- bind_rows(fixed_asc, changed_asc)

	# overwrite the config for asim
	write_csv(newasc, paste0(asim_config_dir, "/trip_mode_choice_coefficients.csv"), na="")
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
copy_calibration_files <- function(asim_out_dir, asim_config_dir, calib_dir, i){
	if(!dir.exists(paste0(calib_dir, "/output")))
		dir.create(paste0(calib_dir, "/output"), recursive = TRUE)
	file.copy(paste0(asim_out_dir, "/final_tours.csv"),
		paste0(calib_dir, "/output/final_tours_RUN", i, ".csv"),
		overwrite = TRUE)
	file.copy(paste0(asim_out_dir, "/final_trips.csv"),
		paste0(calib_dir, "/output/final_trips_RUN", i, ".csv"),
		overwrite = TRUE)
	file.copy(paste0(asim_config_dir, "trip_mode_choice_coefficients.csv"),
		paste0(calib_dir, "trip_mc_coeffs_RUN", i, ".csv"),
			overwrite = TRUE)
}

#------------------------------------------------------------------------------------------------#
