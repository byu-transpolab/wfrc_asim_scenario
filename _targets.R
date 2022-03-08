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
source("R/network_setup.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus"))

# End this file with a list of target objects.
list(
  
  # Build populationsim files ===========================
  # pumas and tracts in populationsim
  tar_target(st_fips, 49), # the state fips code for Utah
  tar_target(puma_list,  c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004)),
  tar_target(puma_tract, get_puma_tr_cwalk(st_fips, puma_list)),
  tar_target(tr, get_tracts(st_fips, puma_tract)),
  
  # Taz geometry
  tar_target(taz_geo, "inputs/taz.geojson", format = "file"),
  tar_target(ivt0,    "inputs/IVT0_tazs.csv", format = "file"),
  tar_target(taz, get_taz(taz_geo, ivt0, tr)),
  tar_target(crosswalk, get_crosswalk(taz, tr)),
  
  
  # Controls
  # The only TAZ-level control we get from WFRC is the number of households in each
  # TAZ. Therefore, this will be the only control in this file. The MAG data is only
  # available for 2020, so that's what we will be using even if 2017 or 2018 might
  # be more appropriate. We filter the list of controls to the TAZ that we are
  # getting in the crosswalk. We need to ensure that the TAZ code is a character
  # string and the household target is an integer. 
  tar_target(taz_control_file, "inputs/Household_Projections_(TAZ).csv", format = "file"),
  tar_target(taz_control, get_taz_control(taz_control_file, crosswalk)),
  # The attributes available from the ACS include the following:
  # - Household size, derived from Table `B08202: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD`
  # - Household workers, derived from the same table
  # - Age, derived from Table `B01001: SEX BY AGE`
  # - Income, derived from Table `B19001:HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)` 
  tar_target(mytracts, unique(crosswalk$TRACT)),
  tar_target(mycounties, unique(substr(crosswalk$TRACT, 3, 5))), 
  tar_target(acsvars, load_variables(2018, "acs5", cache = TRUE)),
  tar_target(sizes, get_sizework_controls(acsvars, mycounties)),
  tar_target(incs, get_income_controls(acsvars, mycounties)),
  tar_target(ages, get_age_controls(acsvars, mycounties)),
  tar_target(tract_controls, make_controls(mytracts, ages, incs, sizes)),
  tar_target(meta, get_meta(tract_controls)),
  
  
  #' Seed
  tar_target(hh_seed_file, "inputs/psam_h49.csv.zip", format = "file"),
  tar_target(pp_seed_file, "inputs/psam_p49.csv.zip", format = "file"),
  tar_target(seed, make_seed(hh_seed_file, pp_seed_file, crosswalk)),
  
  tar_target(write_popsim, write_files(meta, tract_controls, taz_control, seed, 
                                crosswalk, path = "data_popsim")),
  
  tar_target(popsim_success, run_populationsim(write_popsim, "data_popsim", "output_popsim")),
  
  
  # Build land use dataset =================================
  #tar_target(land_use, build_land_use()),
  #tar_target(land_use_file, write_land_use(land_use), format = "file"),
  
  
  
  # Build network ===========================================
  tar_target(link_file, "inputs/wfrc_links.dbf", format = "file"),
  tar_target(node_file, "inputs/wfrc_nodes.dbf", format = "file"),
  tar_target(network, read_wfrcmag(node_file, link_file, 32612)),
  tar_target(write_net, write_linknodes(network, "data/wfrc_network")),
  
  
  # Build ActivitySim Populaiton ===========================
  # This is basically just the populationsim outputs, but 
  
  tar_target(dummy, message("Done"))
  
)
