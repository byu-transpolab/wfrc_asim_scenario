library(tidyverse)
library(targets)

start_iter <- 0
end_iter <- 4
asim_out_dir <- "output_activitysim/20pct_no_RH"
asim_config_dir <- "configs_activitysim/20pct_no_RH"
calib_dir <- "calibration/tour_mc_no_RH"

source("R/asim_trip_mc_calibration.R")

for(i in start_iter:end_iter){
  tar_make()
  copy_calibration_files_trips(asim_out_dir, asim_config_dir, calib_dir, i)
  if(i != end_iter) calibrate_asim_trips(asim_out_dir, asim_config_dir, calib_dir)
}
