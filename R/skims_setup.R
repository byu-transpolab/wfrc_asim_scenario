#' Prepare skims file
#' 
#' 
prepare_skims <- function(ok_skims_file, pk_skims_file, manifest, skim_taz_map){
  # TODO: I couldn't figure out a way to run the python scripts directly from targets.
  # We'll have to work on this later.
  
  system2("sh/build_skims.sh",
          args = c(
            "-m", dirname(manifest),
            "-t", skim_taz_map,
            "-o", "data_activitysim"
          ))
  
  return("data_activitysim/skims.omx")
}



#' Get off-peak skims omx file from box
#' 
#' 
get_ok_skims <- function(path){
  
  
  if(file.exists(path)){
    message("Off-peak skim already downloaded")
  } else {
    tryCatch({
      download.file("https://byu.box.com/shared/static/s4tkpcdtz367dbgiyu1q9jy5kxp44fcm.omx", destfile = path)
    }, error = function(e){
      file.remove(path)
      stop("Could not download file")
    }
    )
  }
  
  return(path)
  
}


#' Get peak skims omx file from box
#' 
#' 
get_pk_skims <- function(path){
  
  if(file.exists(path)){
    message("Peak skim already downloaded")
  } else {
    tryCatch({
      download.file("https://byu.box.com/shared/static/j2jx524u6exlzcfjiyh6p0wgao1wunr8.omx", destfile = path)
    }, error = function(e){
      file.remove(path)
      stop("Could not download file")
    }
    )
  }
  
  return(path)
  
  
}