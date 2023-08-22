library(targets)
library(tarchetypes)

r_scripts <- list.files("R", full.names = TRUE)
sapply(r_scripts, source)

# debugging
#tar_option_set(debug = "land_use")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus"))


# Targets list
#
# This targets script runs setup steps for PopulationSim and ActivitySim,
# which are then run from a command line. Mostly this includes getting the
# population control and land use data to be in the correct format.
# Below is a list of scenarios as targets. Run individual setups with
# `tar_make(names = c(<target names>))`. Running `tar_make()` with no 
# arguments will run setup for all scenarios.
# In any case this script assumes we are in the Wasatch Front region of Utah.
#
# NOTE: Crucially, the ActivitySim setup steps won't work until after the
# relevant PopulationSim scenario is run. If you try to run an ActivitySim
# setup target otherwise, it will throw an error.

scenarios <- tar_plan(
  
  base2019_popsim = setup_popsim(
    se_taz = "data/taz_se/taz_se_2019_all.csv",
    popsim_data_dir = "populationsim/data/2019",
    meta, tract_controls, seed, crosswalk
  ),
  
  landuse_popsim = setup_popsim(
    se_taz = "data/taz_se/taz_se_new_landuse_all.csv",
    popsim_data_dir = "populationsim/data/new_landuse",
    meta, tract_controls, seed, crosswalk
  ),
  
  # To make all popsim scenarios
  popsim = list(base2019_popsim, landuse_popsim),
  
  
  base2019_asim = setup_asim(
    se_file = "data/taz_se/taz_se_2019_all.csv",
    asim_data_dir = "activitysim/data/base_2019",
    popsim_out_dir = "populationsim/output/2019",
    taz = taz,
    skims_file = "data/skims/_built/BY_2019.omx"
  ),
  
  landuse_asim = setup_asim(
    se_file = "data/taz_se/taz_se_new_landuse_all.csv",
    asim_data_dir = "activitysim/data/landuse",
    popsim_out_dir = "populationsim/output/new_landuse",
    taz = taz,
    skims_file = "data/skims/_built/BY_2019.omx"
  ),
  
  transit_asim = setup_asim(
    se_file = "data/taz_se/taz_se_2019_all.csv",
    asim_data_dir = "activitysim/data/transit",
    popsim_out_dir = "populationsim/output/2019",
    taz = taz,
    skims_file = "data/skims/_built/doubletrack.omx"
  ),
  
  wfh_asim = setup_asim(
    se_file = "data/taz_se/taz_se_2019_all.csv",
    asim_data_dir = "activitysim/data/wfh",
    popsim_out_dir = "populationsim/output/2019",
    taz = taz,
    skims_file = "data/skims/_built/BY_2019.omx"
  ),
  
  asim = list(base2019_asim, landuse_asim, transit_asim, wfh_asim)
  
)


populationsim_setup <- tar_plan(
  
	# pumas and tracts in populationsim
	st_fips = 49, # the state fips code for Utah
	puma_list = c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004),
	puma_tract = get_puma_tr_cwalk(st_fips, puma_list),
	tr = get_tracts(st_fips, puma_tract, crs = 26912), #crs should match `taz_geo`

	# Taz geometry
	### The following paraghaph is no longer true; no zones get removed ###
	#The TAZ geojson can have a field `EMPTY` with TRUE/FALSE values. If `EMPTY`
	#is `TRUE`, then the TAZ will be excluded. This is useful for TAZs that are
	#not connected to the network, such as Utah Lake.
	#A file listing TAZs to exclude may also be provided. If so, a TAZ will be
	#excluded either if it is listed or if it is marked `EMPTY` (or both).
	tar_target(taz_geo, "data/WFRC_TAZ.geojson", format = "file"), ###########
	#list of excluded TAZs (Utah Lake, etc.):
	# tar_target(ivt0, "reference/IVT0_tazs.csv", format = "file"), ##########
	taz = get_taz(taz_geo, tr),
	crosswalk = get_crosswalk(taz, tr),


	# Controls

	# The attributes available from the ACS include the following:
	# - Household size, derived from Table `B08202: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD`
	# - Household workers, derived from the same table
	# - Age, derived from Table `B01001: SEX BY AGE`
	# - Income, derived from Table `B19001:HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)`
	mytracts = unique(crosswalk$TRACT),
	mycounties = unique(substr(crosswalk$TRACT, 3, 5)),
	acsvars = load_variables(2019, "acs5", cache = TRUE),
	sizes = get_sizework_controls(acsvars, mycounties),
	incs = get_income_controls(acsvars, mycounties),
	ages = get_age_controls(acsvars, mycounties),
	tract_controls = make_controls(mytracts, ages, incs, sizes),
	meta = get_meta(tract_controls),


	# Seed
	tar_target(hh_seed_file, "data/psam_h49.csv.zip", format = "file"),
	tar_target(pp_seed_file, "data/psam_p49.csv.zip", format = "file"),
	seed = make_seed(hh_seed_file, pp_seed_file, crosswalk)

)

# build_skims <- tar_plan(
# 	# The omx files with which we begin this process are converted from MTX files
# 	# output from the WFRC model. Those files are stored on BOX, and can be converted
# 	# with the script at `sh/convert_cube_omx.s`
# 
# 	# OMX files that are small enough to stash on github are here already; the
# 	# two that are too large need to be downloaded from Box
# 	tar_target(ok_skims_file, get_ok_skims("inputs/skims/skm_auto_Ok.mtx.omx"), format = "file"),
# 	tar_target(pk_skims_file, get_pk_skims("inputs/skims/skm_auto_Pk.mtx.omx"), format = "file"),
# 	tar_target(skim_taz_map, write_taz_map(taz), format = "file"),
# 	tar_target(manifest, "inputs/skims/skim_manifest.csv", format = "file"),
# 
# 	#for staging (`tar_make(skims_setup)`)
# 	skims_setup = list(ok_skims_file, pk_skims_file, skim_taz_map, manifest, dirs),
# 
# 	tar_target(skims_file,
# 		prepare_skims(ok_skims_file, pk_skims_file, manifest,
# 			skim_taz_map, "data_activitysim", skims_setup,
# 			ok_skims_file, pk_skims_file, skim_taz_map, manifest),
# 		format = "file")
# )
 
# activitysim <- tar_plan(
# 
# 	tar_target(addressfile, "inputs/AddressCoordinates.csv", format = "file"),
# 	asim_persons = make_asim_persons("output_popsim", popsim_success, taz),
# 	asim_hholds = make_asim_hholds("output_popsim", addressfile, taz, popsim_success),
# 	tar_target(activitysim_population, move_population(asim_persons, asim_hholds, "data_activitysim"),
# 		format = "file"),
# 
# 	#for staging (`tar_make(asim_setup)`)
# 	asim_setup = list(activitysim_configs, activitysim_outputs,
# 		activitysim_population, land_use_file, gtfs,
# 		skims_file, config_tour_mc, config_trip_mc, tour_freq),
# 
# 	run_asim = run_activitysim("data_activitysim", activitysim_configs, activitysim_outputs, asim_setup,
# 	land_use_file, gtfs, skims_file, config_tour_mc, config_trip_mc, tour_freq)
# )

# Run all targets
tar_plan(
	populationsim_setup,
	# land_use_setup,
	# skims_setup,
	# activitysim_setup,
	scenarios
)
