#!/usr/bin/env Rscript

library(tidyverse)
library(targets)

start_iter <- 1
end_iter <- 10
asim_out_dir <- "output_activitysim/20pct"
asim_config_dir <- "configs_activitysim/20pct"
calib_dir <- "calibration/tour_mc"

source("R/asim_rh_trip_calibration.R")

for(i in start_iter:end_iter){
  tar_make()
  copy_calibration_files(asim_out_dir, asim_config_dir, calib_dir, i)
  if(i != end_iter) calibrate_asim_rh_trips(asim_out_dir, asim_config_dir, calib_dir)
}
