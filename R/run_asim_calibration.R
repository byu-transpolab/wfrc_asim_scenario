start_iter <- 4
end_iter <- 10
asim_out_dir <- "output_activitysim/20pct"
asim_config_dir <- "configs_activitysim/20pct"
targets <- "calibration/tour_mc/asimtourtargets.csv"

for(i in start_iter:end_iter){
  tar_make()
  calibrate_asim_tours(asim_out_dir, asim_config_dir, targets, i)
}