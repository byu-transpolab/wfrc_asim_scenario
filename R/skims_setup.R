#' Prepare skims file
#' 
#' 
build_skims <- function(skims_dir, out_dir){
  
  # download_skims(pk_path, ok_path)
  
  skimsStatus <- system2(
    command = "python",
    args = c("activitysim/build_omx.py", skims_dir, out_dir)
  )
  
  if(skimsStatus != 0){
    stop("\n\nBuilding the skims failed. Check console and/or log(s) for details.\n")
  }
  
  return(file.path(out_dir, "skims.omx"))
}

#' Download big skims files from box
#' 
#' Can't do this right now since direct links are a premium feature on box.
#' For now, download them manually.
#' 
download_skims <- function(pk_path, ok_path){
  
  if(file.exists(pk_path)){
    message("Peak skim already downloaded")
  } else {
    tryCatch({
      download.file(
        # "https://byu.box.com/shared/static/j2jx524u6exlzcfjiyh6p0wgao1wunr8.omx",
        destfile = pk_path)
    }, error = function(e){
      file.remove(pk_path)
      stop("Could not download file")
    })
  }
  
  if(file.exists(ok_path)){
    message("Off-peak skim already downloaded")
  } else {
    tryCatch({
      download.file(
        # "https://byu.box.com/s/ixa1s6bpp0hdxi327q2ybiva3pv3tacy",
        destfile = ok_path)
    }, error = function(e){
      file.remove(ok_path)
      stop("Could not download file")
    })
  }
}


# write_taz_map <- function(taz) {
#   mapfile <- "inputs/skims/skim_taz_map.csv"
#   
#   m <- tibble(
#     wfrc_taz = 1:2881
#   ) %>%
#     left_join(
#       taz %>% st_set_geometry(NULL) %>% select(TAZ, asim_taz),
#       by = c("wfrc_taz" = "TAZ")) 
#   
#   write_csv(m, mapfile)
#   return(mapfile)
# }