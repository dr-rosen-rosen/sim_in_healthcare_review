#############################################
#############################################
#############################################
####### Main funcs for review doc handling
#############################################
#############################################
#############################################

get_references <- function(){
  file_names <- paste0(here(config$ref_file_dir), list.files(path = here(config$ref_file_dir)))
  return(read_bibliography(file_names))
}