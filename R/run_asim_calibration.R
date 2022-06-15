start_iter <- 0
end_iter <- 10
asim_out_dir <- "output_activitysim/20pct"
asim_config_dir <- "configs_activitysim/20pct"
calib_dir <- "calibration/tour_mc"

source("R/asim_tour_mc_calibration.R")

for(i in start_iter:end_iter){
  if(i == 0){
    tar_make()
  } else{
    calibrate_asim_tours(asim_out_dir, asim_config_dir, calib_dir, i)
    tar_make()
  }
}
