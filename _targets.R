library(targets)
library(tarchetypes)

# r_scripts <- list.files("R", full.names = TRUE)
# sapply(r_scripts, source)

source("R/populationsim_setup.R")

# debugging
#tar_option_set(debug = "land_use")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus", "xml2"))


# Targets list
#
# This targets script runs setup steps for PopulationSim and ActivitySim,
# which are then run from a command line. Mostly this includes getting the
# population control data and travel skims to be in the correct format.
# Below is a list of scenarios as targets. Run individual setups with
# `tar_make(names = c(<target names>))`. Running `tar_make()` with no 
# arguments will run setup for all scenarios.
# In any case this script assumes we are in the Wasatch Front region of Utah.
#
# NOTE: Crucially, the ActivitySim setup steps won't work until after the
# relevant PopulationSim scenario is run. If you try to run an ActivitySim
# setup target otherwise, it will throw an error, but the other scenarios
# *should* still run.

scenarios <- tar_plan(
  
  base2019_popsim = setup_popsim(
    se_taz = "populationsim/taz_se_2019_all.csv",
    out_dir_data = "populationsim/data/2019",
    meta, tract_controls, seed, crosswalk
    ),
  
  landuse_popsim = setup_popsim(
    se_taz = "populationsim/taz_se_new_landuse_all.csv",
    out_dir_data = "populationsim/data/new_landuse",
    meta, tract_controls, seed, crosswalk
  ),
  
  popsim = c(base2019_popsim, landuse_popsim)
  
  
  #base2019_asim = setup_asim(skims_dir = "skims/dir")
  #landuse_asim
  #transit_asim
  #wfh_asim
  
  #asim = c(base2019_asim, landuse_asim, transit_asim, wfh_asim)
  
)


populationsim_setup <- tar_plan(
  
	# pumas and tracts in populationsim
	st_fips = 49, # the state fips code for Utah
	puma_list = c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004),
	puma_tract = get_puma_tr_cwalk(st_fips, puma_list),
	tr = get_tracts(st_fips, puma_tract, crs = 26912), #crs should match `taz_geo`

	# Taz geometry
	#The TAZ geojson can have a field `EMPTY` with TRUE/FALSE values. If `EMPTY`
	#is `TRUE`, then the TAZ will be excluded. This is useful for TAZs that are
	#not connected to the network, such as Utah Lake.
	#A file listing TAZs to exclude may also be provided. If so, a TAZ will be
	#excluded either if it is listed or if it is marked `EMPTY` (or both).
	tar_target(taz_geo, "reference/WFRC_TAZ.geojson", format = "file"), ###########
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
	tar_target(hh_seed_file, "reference/psam_h49.csv.zip", format = "file"),
	tar_target(pp_seed_file, "reference/psam_p49.csv.zip", format = "file"),
	seed = make_seed(hh_seed_file, pp_seed_file, crosswalk)

)

# build_land_use_dataset <- tar_plan(
# 	tar_target(se_boxelder, "inputs/SE_Box_Elder_2018.csv", format = "file"),
# 	tar_target(se_wfrc, "inputs/SE_WF_2018.csv", format = "file"),
# 	se = read_sedata(se_wfrc, se_boxelder),
# 
# 	tar_target(urbanfile, "inputs/other/urbanization.csv", format = "file"),
# 	tar_target(buildfile, "inputs/other/buildings.csv", format = "file"),
# 	tar_target(parcelsfile, "inputs/other/parcels.csv", format = "file"),
# 	tar_target(topofile, "inputs/other/topography.csv", format = "file"),
# 	tar_target(schoolfile, "inputs/other/schools.csv", format = "file"),
# 
# 	perdata = read_perdata("output_popsim", popsim_success),
# 	hhdata = read_hhdata("output_popsim", popsim_success),
# 	urbanization = read_urbanization(urbanfile),
# 	buildings = make_buildings(buildfile, parcelsfile),
# 	schools = make_schools(schoolfile),
# 	topo = make_topo(topofile),
# 	land_use = make_land_use(se, perdata, hhdata, urbanization, buildings,
# 		topo, schools, taz),
# 
# 	tar_target(land_use_file, write_land_use(land_use, file.path("data_activitysim",
# 				"land_use.csv")), format = "file")
# )
# 
# # build_network <- tar_plan(
# # 	tar_target(matsim_lib, get_matsim_lib("lib/payson_generator-0.0.1-SNAPSHOT.jar")),
# # 	tar_target(link_file, "inputs/wfrc_links.dbf", format = "file"),
# # 	tar_target(node_file, "inputs/wfrc_nodes.dbf", format = "file"),
# # 	tar_target(network, read_wfrcmag(node_file, link_file, 32612)),
# # 	tar_target(write_net, write_linknodes(network, "data/wfrc_network"), format = "file")
# # 	# I don't believe matsim_net is necessary since we're writing the pbf directly.
# # 	# Maybe we should do that here instead?
# # 	# TODO
# # 	#tar_target(matsim_net, make_matsim_network(network, "data/wfrc_network/highways_network.xml"), format = "file"),
# # )
# 
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
# 
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

# build_beam_inputs <- tar_plan(
# 	tar_target(beam_lu, build_beam_lu(land_use), format = "file"),
# 	tar_target(beam_centroids, build_beam_centroids(land_use, network), format = "file"),
# 	tar_target(gtfs, get_gtfs("reference", dirs), format = "file")
# )

# Run all targets
tar_plan(
	populationsim_setup,
	# land_use_setup,
	# skims_setup,
	# activitysim_setup,
	scenarios
)
