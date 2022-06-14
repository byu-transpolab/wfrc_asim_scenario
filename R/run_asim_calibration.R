start_iter <- 1
end_iter <- 10
asim_out_dir <- "output_activitysim/20pct"
asim_config_dir <- "configs_activitysim/20pct"
calib_dir <- "calibration/tour_mc"

for(i in start_iter:end_iter){
  tar_make()
  calibrate_asim_tours(asim_out_dir, asim_config_dir, calib_dir, i)
}