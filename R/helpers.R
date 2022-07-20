## List of misc helper functions

#' Create directories from list
#' 
#' @param dir_list List of directories to create
#' 
#' @return The provided list of directories
#' 
#' @export
create_dirs <- function(dirs_list){
  
  for(dir in dirs_list){
    if(!dir.exists(dir)) dir.create(dir, recursive = T)
  }
  
  return(dirs_list)
}

#' Purge rows and columns containing ridehail modes
#' 
#' @param df Data to purge
#' @param check_col_index Index of column to check for ridehail strings
#' 
purge_rh <- function(df, check_col_index){
  df %>%
    filter(!str_detect(unlist(df[check_col_index]), "(?i)taxi|tnc")) %>%
    select(which(!str_detect(colnames(df), "(?i)taxi|tnc")))
}

create_old_files <- function(){
  for(i in c("tour_mode_choice.csv",
             "tour_mode_choice_coefficients.csv",
             "tour_mode_choice_coefficients_template.csv",
             "trip_mode_choice.csv",
             "trip_mode_choice_coefficients.csv",
             "trip_mode_choice_coefficients_template.csv")){
    
    read_csv(i) %>%
      write_csv(paste0(i, "_OLD"), na = "")
    
    read_csv(i) %>% 
      purge_rh(1) %>% 
      write_csv(i, na = "")
  }
}