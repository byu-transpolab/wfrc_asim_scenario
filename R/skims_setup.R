#' Prepare skims file
#' 
#' 
prepare_skims <- function(ok_skims_file, pk_skims_file, manifest, asim_data_dir){
  skimsStatus <- system2(
  command = "source",
  args = c("sh/build_skims.sh"),
  env = c(paste0("SKIMS_MANIFEST_DIR=", dirname(manifest)),
          paste0("ASIM_DTA_DIR=", asim_data_dir))
  )
  
  if(skimsStatus != 0){
    stop("\n\nBuilding the skims failed. Check console and/or log(s) for details.\n")
  }
  
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