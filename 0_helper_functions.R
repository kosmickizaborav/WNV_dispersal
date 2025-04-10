

# FUNCTION 1: generate_file_name ------------------------------------------

make_file_name <- function(
    study_id, individual_id, deployment_id, 
    sensor_id = NULL, 
    ending = NULL, 
    file_type = ".rds"
    ){
  
  if(is.null(sensor_id)){
    
    fname <- str_c(
      str_c(
        study_id,  "stu", 
        individual_id, "ind", 
        deployment_id, "dep",
        ending, 
        sep = "_"
      ), 
      file_type
    )
    
  } else{
    
    fname <- str_c(
      str_c(
        study_id,  "stu", 
        individual_id, "ind", 
        deployment_id, "dep",
        sensor_id, "sen",
        ending, 
        sep = "_"
      ), 
      file_type
    )
  }
  
    
 return(fname)
  
}

# FUNCTION 2: get_file_path -------------------------------------------

get_file_path <- function(file, folder, species, data_dir = here("Data")){
  
  file_paths <- file.path(
    data_dir, "Studies", str_replace(species, " ", "_"), folder, file
    )
  
  return(file_paths)
}


# check_file_exists <-  ---------------------------------------------------

check_file_exists <- function(file, folder, species, data_dir = here("Data")){
  
  file_path <- get_file_path(file, folder, species, data_dir = data_dir)
  
  return(file.exists(file_path))
}




