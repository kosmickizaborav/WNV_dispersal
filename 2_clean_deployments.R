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
#'       the time-stamps were first rounded to the lowest minute, so that we 
#'       select one location per minute with the lowest hdop
#'   5 - only one location per deployment

# 0 - Define parameters and packages ---------------------------------------

library(move2)
library(here)
library(tidyverse)
library(sf)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# defining columns that might be useful from deployments
columns_of_interest <- c(
  "timestamp", "geometry",
  "study_id", "deployment_id", "individual_id", "tag_id", 
  "individual_local_identifier", "deployment_local_identifier",
  "tag_local_identifier", "sensor_type_id", "individual_number_of_deployments", 
  "taxon_canonical_name", "sex", "animal_life_stage", 
  "manipulation_type", "manipulation_comments",
  "gps_hdop", "gps_cdop", "gps_vdop",
  "algorithm_marked_outlier", "import_marked_outlier",
  "manually_marked_outlier"
  )

# used to change sensor type id from number to character
sensors <- tibble(
  num = c(2299894820, 653), 
  char = c("sigfox", "gps")
)

# this is the study id from aiguamolls de l'emporda
E4WarningID <- 4043292285

# the date when the antenna was placed in the park
antenna_date <- as_datetime("13-11-2024", format = "%d-%m-%Y")

# checking manipulation comments because in some cases it seems to be more
# than just simple relocation
dep_df <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |> 
  distinct(manipulation_comments, manipulation_type)

# manipulation comments that seem more than just relocation
# 1. relocated and released for trials; for 2 trials was treated 
#    to shift light cycle
# 2. relocated and released for trials; for 1 trial was treated to shift 
#   light cycle
# 3. CS (clock-shift)
# 4. FL
# 5. XY (magnetically deprived)
# 6. AO (anosmic)
# 7. A: anosmia treatment prior to relocation and release
# 8. animal owned by farm and may have been restricted in movement or 
#    transported for sale (see Deployment Comments)
string_to_exclude <- "restricted|anosmi[ac]|shift|FL|magnetic"



# 1 - Create output directory ---------------------------------------------

# create directory for the cleaned deployments
target_sp |> 
  map(~{
    
    out_dir <- here("Data", "Studies", str_replace(.x, " ", "_"), "2_cleaned")
    
    if(!dir.exists(out_dir)){ out_dir |> dir.create() } 
    
  }) # close map target_sp


# 2 - Clean deployments -------------------------------------------------------


target_sp |>
  map(~{
    
    # species directory
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    # get all the study files
    sfiles <- here(sp_dir, "1_deployments") |> 
      list.files(pattern = ".rds")
    lfl <- length(sfiles)
    
    sfiles |> 
      map(~{
        
        fin <- .x
        fout <- str_replace(fin, ".rds", "_cleaned.rds")
        
        print(paste(sp, which(fin == sfiles), "|", lfl))
        
        mv <- here(sp_dir, "1_deployments", fin) |> 
          read_rds() |> 
          select(any_of(columns_of_interest)) |> 
          # change the value of sensor_type_id to the character value 
          # using variable sensors
          mutate(
            sensor_type = sensors$char[match(sensor_type_id, sensors$num)],
            # mark the E4Warning study
            study_site = if_else(study_id %in% E4WarningID, "E4Warning", "other"),
            # exclude the E4warning data that was collected by sigfox 
            # before we positioned the antenna in the park
            study_site_comment = case_when(
              study_site == "E4Warning" & timestamp > antenna_date &
                sensor_type_id == "sigfox" ~ "post-antenna",
              study_site == "E4Warning" & timestamp <= antenna_date & 
                sensor_type_id == "sigfox" ~ "pre-antenna",
              .default = NA
            )
          ) |>
          # fixing some of the manipulation types that seem not to be well
          # specified in the original file, the list of characters was taken by
          # looking at all the manipulation comments
          # in the deployment information
          mutate(
            manipulation_type = case_when(
              str_detect(manipulation_comments, string_to_exclude) ~ "other",
              str_detect(manipulation_comments, "[Dd]isplacement") ~ "relocated",
              is.na(manipulation_type) ~ "none",
              .default = manipulation_type
            )
          ) |>
          # rounding timestamp to minute to take only one position per minute
          mutate(
            n_na = rowSums(is.na(pick(everything()))),
            timestamp = floor_date(timestamp, unit = "min")
          ) |>
          # ordering by gps_hdop, and or by the number of NAs in rows,
          # so that it's minimized
          arrange(across(any_of(c("timestamp", "gps_hdop", "n_na")))) |>
          mutate(
            # mark duplicated timestamps
            duplicated_timestamp = duplicated(timestamp),
            n_locations = n(),
            year = year(timestamp),
            lon = st_coordinates(geometry)[, 1],
            lat = st_coordinates(geometry)[, 2]
          ) |> 
          select(-n_na)
        
        
        # officially there are no outliers, because when downloading it was 
        # specified not to include them, but left this just in case
        if(sum(str_detect("outlier", colnames(mv))) > 0){
          
          out_df <- mv |> 
            st_drop_geometry() |> 
            as_tibble() |> 
            # select only outlier columns
            select(contains("outlier")) |>
            mutate(row_id = str_c("row", 1:n())) |>
            pivot_longer(
              cols = contains("outlier"), 
              names_to = "outlier_type", 
              values_to = "outlier_value"
            ) |>
            # group_by row_id and check if there is any value with 
            # positive outlier
            summarize(
              outlier_sum = sum(outlier_value, na.rm = T),
              .by = row_id
            ) |> 
            # mark outliers
            mutate(outlier = ifelse(outlier_sum > 0, "outlier", NA)) |> 
            select(outlier, row_id)
          
          mv <- mv |> 
            mutate(row_id = str_c("row", 1:n())) |>
            left_join(out_df, by = "row_id") |>
            select(-row_id)
        
          } else { # close if sum(str_detect("outlier"))
            
            mv <- mv |> 
              mutate(outlier = NA)
            
          } # close else sum(str_detect("outlier"))
        
   
        mv <- mv |> 
          # marking problems in the track data
          # 1 - empty geometry (no coordinates),
          # 2 - weird date - year before 1950 or in the future,
          # 3 - weird coordinates - lon > 180 or lat > 90,
          # 4 - duplicated time-stamps - eliminated points that are sampled
          #     with the frequency higher than 1 minute
          # 5 - only one point - insufficient data
          # 6 - the animal was manipulated more than just relocation
          #     according to the comments
          # 7 - sigfox E4Warning data before putting the antenna in the park
          mutate(
            track_problem = case_when(
              st_is_empty(geometry) ~ "empty geometry",
              # problematic dates
              year < 1950 | year > year(Sys.Date()) ~ "weird date",
              # coordinates outside of the range even though
              # the projection is EPSG:4326 (checked)
              abs(lon) > 180 | abs(lat) > 90 ~ "weird coordinates",
              # duplicated timestamp
              duplicated_timestamp == T ~ "duplicated timestamp",
              n_locations == 1 ~ "only one location available",
              manipulation_type == "other" ~ "manipulated more than relocation",
              study_site_comment == "post-antenna" ~ "E4Warning sigfox pre-antenna",
              outlier == "outlier" ~ "outlier",
              .default = NA
            ) 
          ) |> 
          select(-year, -duplicated_timestamp, -n_locations, -outlier)
        
        
        mv_summary <- mv |>
          as_tibble() |>
          summarize(
            n = n(),
            .by = c(
              "study_id",
              "deployment_id",
              "individual_id",
              "individual_local_identifier",
              "track_problem"
              )
          ) |>
          mutate(file = fin) |>
          mutate(across(contains("_id"), as.character))

        mv <- mv |>
          filter(is.na(track_problem)) 

        # if the data is empty, don't save the file
        if(nrow(mv) > 2){

          mv |>  write_rds(file = here(sp_dir, "2_cleaned", fout))

        } else{

          mv_summary <- mv_summary |>
            mutate(comment = "track not saved!")

        }

        # in order to save it to report file
        mv_summary

      }) |> # close map files
      bind_rows() |> 
      mutate(species = sp)
    
  }) |> # close map species
  bind_rows() |> 
  mutate(script_ran_on = Sys.time()) |>
  write_csv(here("Data", "Studies", str_c("2_deployments_clean_report.csv")))


# previous code for outliers, way too slow
# rowwise() |> 
# mutate(
#   any_outlier = any(c_across(contains("outlier")), na.rm = T),
#   outlier = if_else(any_outlier, "outlier", NA)
# ) |> 
# ungroup() |>
# select(-any_outlier) |>  # Remove the helper column if not needed


