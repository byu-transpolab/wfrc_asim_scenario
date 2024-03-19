library(targets)
library(tarchetypes)

tar_source("_compare_targets_R")

tar_option_set(packages = c("tidyverse", "sf", "ggspatial"), format = "qs")

tar_plan(
  
  tar_file(by_file, "data/skims/_built/BY_2019.omx"),
  tar_file(tr_file, "data/skims/_built/doubletrack.omx"),
  tar_file(taz_file, "data/WFRC_TAZ.geojson"),
  
  taz = sf::st_read(taz_file),
  
  trace = list(
    origin = c(3000,3333,1064),
    destination = c(3000,3333,1064)),
  
  # transit_skims_names = get_transit_skims_names(by_file),
  # Time of day and mode don't make much difference
  # so we only take PM D_C_W skims
  transit_skims_names = "DRV_COM_WLK_TOTIVT__PM",
  
  by_raw = omxr::read_all_omx(by_file, transit_skims_names),
  tr_raw = omxr::read_all_omx(tr_file, transit_skims_names),
  
  by = clean_omx(by_raw),
  tr = clean_omx(tr_raw),
  
  omx = combine_omx(by = by, tr = tr),
  
  trace_omx = dplyr::filter(
    omx, origin %in% trace$origin, destination %in% trace$destination
  ),
  
  omx_diff_hist = plot_omx_diff(omx),
  
  omx_origin_summary = dplyr::summarise(
    omx,
    increase = sum(increase, na.rm = TRUE),
    .by = c(origin, key, time, mode)
  ),
  
  omx_origin_taz = dplyr::right_join(
    taz, omx_origin_summary, join_by(TAZID == origin)
  )

)