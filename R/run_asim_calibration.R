start_iter <- 4
end_iter <- 10
asim_dir <- "output_activitysim/20pct"
targets <- "calibration/tour_mc/asimtourtargets.csv"

for(i in start_iter:end_iter){
  tar_make()
  calibrate_asim_tours(asim_dir, targets, i)
}