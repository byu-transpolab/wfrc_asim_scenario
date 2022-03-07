library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/populationsim_setup.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus"))

# End this file with a list of target objects.
list(
  
  # Build populationsim targets ===========================
  # pumas and tracts in populationsim
  tar_target(st_fips, 49), # the state fips code for Utah
  tar_target(puma_list,  c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004)),
  tar_target(puma_tract, get_puma_tr_cwalk(st_fips, puma_list)),
  tar_target(tr, get_tracts(st_fips, puma_tract)),
  
  # Taz geometry
  tar_target(taz_geo, "inputs/taz.geojson", format = "file"),
  tar_target(ivt0,    "inputs/IVT0_tazs.csv", format = "file"),
  tar_target(taz, get_taz(taz_geo, ivt0, tr)),
  
  
  # Build land use dataset
  #tar_target(land_use, build_land_use()),
  #tar_target(land_use_file, write_land_use(land_use), format = "file"),
  
  
  tar_target(dummy, message("Done"))
  
)
