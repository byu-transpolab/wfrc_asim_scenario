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
source("R/activitysim_setup.R")
source("R/skims_setup.R")
source("R/beam_setup.R")


# debugging
#tar_option_set(debug = "land_use")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus"))


popsim_outputs <- "output_popsim"
activitysim_inputs <- "data_activitysim"


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
  # TAZ.  We actually get this from the travel model SE file (below)
  tar_target(taz_control, get_taz_control(se, crosswalk)),
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
  
  tar_target(write_popsim, write_popsim_files(meta, tract_controls, taz_control, seed, 
                                crosswalk, path = "data_popsim"), 
             format = "file"),
  tar_target(popsim_sh, "sh/runpopsim.sh", format = "file"),
  tar_target(popsim_success, run_populationsim(write_popsim, "data_popsim", popsim_outputs),
             format = "file"),
  
  
  # Build land use dataset =================================
  tar_target(se_boxelder, "inputs/SE_Box_Elder_2018.csv", format = "file"),
  tar_target(se_wfrc,     "inputs/SE_WF_2018.csv", format = "file"),
  tar_target(se, read_sedata(se_wfrc, se_boxelder)),
  tar_target(perdata, read_perdata(popsim_outputs, popsim_success)),
  tar_target(hhdata,  read_hhdata(popsim_outputs, popsim_success)),
  tar_target(urbanfile, "inputs/other/urbanization.csv", format = "file"),
  tar_target(urbanization, read_urbanization(urbanfile)),
  tar_target(buildfile, "inputs/other/buildings.csv", format = "file" ),
  tar_target(parcelsfile, "inputs/other/parcels.csv", format = "file" ),
  tar_target(buildings, make_buildings(buildfile, parcelsfile)),
  tar_target(topofile, "inputs/other/topography.csv", format = "file"),
  tar_target(schoolfile, "inputs/other/schools.csv", format = "file"),
  tar_target(schools, make_schools(schoolfile)),
  tar_target(topo, make_topo(topofile)), 
  tar_target(land_use, make_land_use(se, perdata, hhdata, urbanization, buildings, 
                                     topo, schools, taz)),
  tar_target(land_use_file, write_land_use(land_use, file.path(activitysim_inputs, 
                                                               "land_use.csv")), format = "file"),
  
  
  
  # Build network ===========================================
  tar_target(matsim_lib, get_matsim_lib("lib/payson_generator-0.0.1-SNAPSHOT.jar")),
  tar_target(link_file, "inputs/wfrc_links.dbf", format = "file"),
  tar_target(node_file, "inputs/wfrc_nodes.dbf", format = "file"),
  tar_target(network, read_wfrcmag(node_file, link_file, 32612)),
  tar_target(write_net, write_linknodes(network, "data/wfrc_network"), format = "file"),
  tar_target(matsim_net, make_matsim_network("data/wfrc_network", matsim_lib, write_net), format = "file"),
  tar_target(beam_net_cleaner, "sh/clean_matsim_types.sh", format = "file"),
  tar_target(beam_net, make_beam_network(beam_net_cleaner, matsim_net), format = "file"),
  
  
  # Build Skims ==============================================
  # The omx files with which we begin this process are converted from MTX files
  # output from the WFRC model. Those files are stored on BOX, and can be converted
  # with the script at `sh/convert_cube_omx.s`
  
  # OMX files that are small enough to stash on github are here already; the
  # two that are too large need to be downloaded from Box
  tar_target(ok_skims_file, get_ok_skims("inputs/skims/skm_auto_Ok.mtx.omx"), format = "file"),
  tar_target(pk_skims_file, get_pk_skims("inputs/skims/skm_auto_Pk.mtx.omx"), format = "file"),
  tar_target(skim_taz_map, write_taz_map(taz), format = "file"),
  tar_target(manifest, "inputs/skims/skim_manifest.csv", format = "file"),
  tar_target(skims_file, prepare_skims(ok_skims_file, pk_skims_file, manifest, skim_taz_map), 
             format = "file"),
  
  
  
  
  # Build ActivitySim Population ===========================
  tar_target(addressfile, "inputs/AddressCoordinates.csv", format = "file"),
  tar_target(asim_persons, make_asim_persons(popsim_outputs, popsim_success, taz)),
  tar_target(asim_hholds, make_asim_hholds(popsim_outputs, addressfile, taz, popsim_success)),
  tar_target(activitysim_population, move_population(asim_persons, asim_hholds, activitysim_inputs), 
             format = "file"),
  tar_target(run_asim, run_activitysim(activitysim_inputs, "configs", "output_activitysim", 
                                       activitysim_population, land_use_file)),
  
  
  
  # Build BEAM Inputs ========================
  tar_target(beam_lu, build_beam_lu(land_use), format = "file"),
  tar_target(beam_centroids, build_beam_centroids(land_use, network), format = "file"),
  tar_target(gtfs, get_gtfs(), format = "file")
  
  
)
