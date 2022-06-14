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
