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

#' Gets the directory based on a pattern in the object name
#' 
#' This is necessary (at least until another way is found) due to how vector
#' items are named with `targets` dynamic branching
#' 
#' @param dirs_list Named vector of directories to search in
#' @param dirname_pattern The string pattern to search for
#' 
#' @return Name of directory
#' 
#' @export
dirget <- function(dirs_list, dirname_pattern){
    name <- dirs_list[str_which(names(dirs_list), dirname_pattern)]
    if(length(name) > 1){
      stop(str_glue("Multiple objects match dirname_pattern \"{dirname_pattern}\""))
    } else if(length(name) == 0){
      stop(str_glue("No objects match dirname_pattern \"{dirname_pattern}\""))
    } else {
      return(name)
    }
}
