library(targets)
library(tarchetypes)

source("R/populationsim_setup.R")
source("R/network_setup.R")
source("R/activitysim_setup.R")
source("R/skims_setup.R")
source("R/beam_setup.R")
source("R/helpers.R")


# debugging
#tar_option_set(debug = "land_use")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "tigris", "tidycensus", "xml2"))

# Targets list

directories <- tar_plan(

	# Change depending on config
	activitysim_configs = "configs_activitysim/20pct",
	activitysim_outputs = "output_activitysim/20pct",

	# Make non-existent directories
	dirs = for(dir in c("data_popsim", "output_popsim", "data_activitysim")) if(!dir.exists(dir)) dir.create(dir, recursive = T),

	asim_conf = if(!dir.exists(activitysim_configs)) dir.create(activitysim_configs, recursive = T),
	asim_out = if(!dir.exists(activitysim_outputs)) dir.create(activitysim_outputs, recursive = T)
)

calibration_check <- tar_plan(
	tar_target(config_tour_mc, paste0(activitysim_configs, "/tour_mode_choice_coefficients.csv"),
	           format = "file"),
	tar_target(config_trip_mc, paste0(activitysim_configs, "/trip_mode_choice_coefficients.csv"),
	           format = "file"),
	tar_target(tour_freq, paste0(activitysim_configs, "/joint_tour_frequency_coeffs.csv"),
	           format = "file")
)

populationsim <- tar_plan(
	# pumas and tracts in populationsim
	st_fips = 49, # the state fips code for Utah
	puma_list = c(49001:49004, 11001:11002, 35001:35009, 57001:57002, 03001:03004),
	puma_tract = get_puma_tr_cwalk(st_fips, puma_list),
	tr = get_tracts(st_fips, puma_tract),

	# Taz geometry
	tar_target(taz_geo, "inputs/taz.geojson", format = "file"),
	tar_target(ivt0,    "inputs/IVT0_tazs.csv", format = "file"),
	taz = get_taz(taz_geo, ivt0, tr),
	crosswalk = get_crosswalk(taz, tr),


	# Controls
	# The only TAZ-level control we get from WFRC is the number of households in each
	# TAZ.  We actually get this from the travel model SE file (below)
	taz_control = get_taz_control(se, crosswalk),
	# The attributes available from the ACS include the following:
	# - Household size, derived from Table `B08202: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD`
	# - Household workers, derived from the same table
	# - Age, derived from Table `B01001: SEX BY AGE`
	# - Income, derived from Table `B19001:HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)`
	mytracts = unique(crosswalk$TRACT),
	mycounties = unique(substr(crosswalk$TRACT, 3, 5)),
	acsvars = load_variables(2018, "acs5", cache = TRUE),
	sizes = get_sizework_controls(acsvars, mycounties),
	incs = get_income_controls(acsvars, mycounties),
	ages = get_age_controls(acsvars, mycounties),
	tract_controls = make_controls(mytracts, ages, incs, sizes),
	meta = get_meta(tract_controls),


	# Seed
	tar_target(hh_seed_file, "inputs/psam_h49.csv.zip", format = "file"),
	tar_target(pp_seed_file, "inputs/psam_p49.csv.zip", format = "file"),
	seed = make_seed(hh_seed_file, pp_seed_file, crosswalk),

	write_popsim = write_files(meta, tract_controls, taz_control, seed,
			crosswalk, path = "data_popsim", dirs),
	tar_target(popsim_success, run_populationsim(write_popsim, "data_popsim", "output_popsim"),
		format = "file")
)

build_land_use_dataset <- tar_plan(
	tar_target(se_boxelder, "inputs/SE_Box_Elder_2018.csv", format = "file"),
	tar_target(se_wfrc, "inputs/SE_WF_2018.csv", format = "file"),
	se = read_sedata(se_wfrc, se_boxelder),

	tar_target(urbanfile, "inputs/other/urbanization.csv", format = "file"),
	tar_target(buildfile, "inputs/other/buildings.csv", format = "file"),
	tar_target(parcelsfile, "inputs/other/parcels.csv", format = "file"),
	tar_target(topofile, "inputs/other/topography.csv", format = "file"),
	tar_target(schoolfile, "inputs/other/schools.csv", format = "file"),

	perdata = read_perdata("output_popsim", popsim_success),
	hhdata = read_hhdata("output_popsim", popsim_success),
	urbanization = read_urbanization(urbanfile),
	buildings = make_buildings(buildfile, parcelsfile),
	schools = make_schools(schoolfile),
	topo = make_topo(topofile),
	land_use = make_land_use(se, perdata, hhdata, urbanization, buildings,
			topo, schools, taz),

	tar_target(land_use_file, write_land_use(land_use, file.path("data_activitysim",
				"land_use.csv")), format = "file"),
)

build_network <- tar_plan(
	tar_target(matsim_lib, get_matsim_lib("lib/payson_generator-0.0.1-SNAPSHOT.jar")),
	tar_target(link_file, "inputs/wfrc_links.dbf", format = "file"),
	tar_target(node_file, "inputs/wfrc_nodes.dbf", format = "file"),
	tar_target(network, read_wfrcmag(node_file, link_file, 32612)),
	tar_target(write_net, write_linknodes(network, "data/wfrc_network"), format = "file"),
	# I don't believe matsim_net is necessary since we're writing the pbf directly.
	# Maybe we should do that here instead?
	# TODO
	#tar_target(matsim_net, make_matsim_network(network, "data/wfrc_network/highways_network.xml"), format = "file"),
)

build_skims <- tar_plan(
	# The omx files with which we begin this process are converted from MTX files
	# output from the WFRC model. Those files are stored on BOX, and can be converted
	# with the script at `sh/convert_cube_omx.s`

	# OMX files that are small enough to stash on github are here already; the
	# two that are too large need to be downloaded from Box
	tar_target(ok_skims_file, get_ok_skims("inputs/skims/skm_auto_Ok.mtx.omx"), format = "file"),
	tar_target(pk_skims_file, get_pk_skims("inputs/skims/skm_auto_Pk.mtx.omx"), format = "file"),
	tar_target(skim_taz_map, write_taz_map(taz), format = "file"),
	tar_target(manifest, "inputs/skims/skim_manifest.csv", format = "file"),
	tar_target(skims_file, prepare_skims(ok_skims_file, pk_skims_file, manifest, skim_taz_map, "data_activitysim", dirs),
		format = "file"),
)

activitysim <- tar_plan(

	tar_target(addressfile, "inputs/AddressCoordinates.csv", format = "file"),
	asim_persons = make_asim_persons("output_popsim", popsim_success, taz),
	asim_hholds = make_asim_hholds("output_popsim", addressfile, taz, popsim_success),
	tar_target(activitysim_population, move_population(asim_persons, asim_hholds, "data_activitysim"),
		format = "file"),
	
	asim_setup = list(activitysim_configs, activitysim_outputs, land_use_file,
	                  activitysim_population, land_use_file, gtfs,
	                  skims_file, config_tour_mc, config_trip_mc, tour_freq),

	run_asim = run_activitysim("data_activitysim", asim_setup),
)

build_beam_inputs <- tar_plan(
	tar_target(beam_lu, build_beam_lu(land_use), format = "file"),
	tar_target(beam_centroids, build_beam_centroids(land_use, network), format = "file"),
	tar_target(gtfs, get_gtfs("reference", dirs), format = "file")
)

# Run all targets
tar_plan(
  directories,
  calibration_check,
  populationsim,
  build_land_use_dataset,
  build_network,
  build_skims,
  activitysim,
  build_beam_inputs
)
