#' ---
#' title: "Cleaning deployments"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' Cleaning the deployments by eliminating the following locations:
#'   1 - empty geometry
#'   2 - weird years (i.e. year < 1950)
#'   3 - and coordinates (e.g. long > 180 and lat < 90)
#'   4 - duplicated time-stamps
#'   5 - only one location per deployment

# 0 - Defining parameters and packages ---------------------------------------

library(move2)
library(here)
library(tidyverse)
library(sf)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector()

# defining columns from deployment
col_deploy <- c("taxon_canonical_name", "study_id", "deployment_id", 
                "individual_id", "individual_local_identifier", 
                "individual_number_of_deployments", 
                "deployment_local_identifier", "tag_local_identifier", 
                "sex", "animal_life_stage",  "manipulation_type",
                "manipulation_comments"
)


# defining columns to keep for each study
col_study <- c(
  "individual_local_identifier", "deployment_id", "timestamp", 
  "geometry", "event_id", "sensor_type_id", "gps_hdop", "gps_dop", 
  "gps_vdop", "individual_id", 
  "study_id", "tag_id", "tag_local_identifier", "algorithm_marked_outlier", 
  "import_marked_outlier", "manually_marked_outlier"
)


# Clean deployments -------------------------------------------------------


target_sp |>
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    # create the directory with filtered data
    here(sp_dir, "2_cleaned") |>  dir.create()
    
    sfiles <- here(sp_dir, "1_deployments") |> 
      list.files(pattern = ".rds")
    
    sfiles |> 
      map(~{
        
        fname <- .x
        fout <- str_replace(fname, ".rds", "_cleaned.rds")
        
        print(paste(sp, fname, which(fname == sfiles), "|", length(sfiles)))
        
        
        mv <- here(sp_dir, "1_deployments", fname) |> 
          read_rds() |> 
          # in some cases there are multiple deployments per individual
          # group_by(individual_local_identifier, deployment_id) |> 
          # this should be out in the new script
          mutate(n_na = rowSums(is.na(pick(everything())))) |> 
          # ordering by gps_hdop, and or by the number of na's in rows, 
          # so that it's minimized
          arrange(across(any_of(c("timestamp", "gps_hdop", "n_na")))) |>
          # .by_group = T
          mutate(
            # mark duplicated timestamps
            duplicated_timestamp = duplicated(timestamp),
            n_locations = n()
          ) |>
          mutate(
            year = year(timestamp),
            lon = st_coordinates(geometry)[, 1], 
            lat = st_coordinates(geometry)[, 2], 
            track_problem = case_when(
              st_is_empty(geometry) ~ "empty geometry", 
              # problematic dates
              year < 1950 | year > year(Sys.Date()) ~ "weird date",
              # coordinates outside of the range even though proj is EPSG:4326
              abs(lon) > 180 | abs(lat) > 90 ~ "weird coordinates",
              # duplicated timestamp
              duplicated_timestamp == T ~ "duplicated timestamp", 
              n_locations == 1 ~ "only one point",
              # months outside of the mosquito season
              #!month %in% fmonths ~ "season out",
              # points outside of europe
              #!as.vector(st_intersects(geometry, eu_bb, sparse = FALSE)) ~ "area out",
              .default = NA
            )
          ) 
          # ungroup()
        
        mv_summary <- mv |> 
          as_tibble() |> 
          summarize(
            n = n(), 
            .by = c(
              "individual_id",
              "individual_local_identifier",
              "deployment_id", 
              "track_problem"
              )
          ) |> 
          mutate(file = fname) |> 
          mutate(across(contains("_id"), as.character))
        
        mv <- mv |> 
          filter(is.na(track_problem)) |> 
          select(any_of(c(col_deploy, col_study, "species", "account")))
        
        # if the data is empty, don't save the file
        if(nrow(mv) > 1){
          
          mv |>  write_rds(file = here(sp_dir, "2_cleaned", fout))
          
        } else{
          
          mv_summary <- mv_summary |> 
            mutate(comment = "track not saved!")
          
        }
          
        mv_summary
        
      }) |> 
      bind_rows() |> 
      mutate(species = sp)
    
  }
  ) |> 
  bind_rows() |>  
  mutate(script_ran_on = Sys.time()) |> 
  write_csv(here("Data", "Studies", str_c("2_deployments_clean_report.csv")))







