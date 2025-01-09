#' ---
#' title: "Exclude duplicated data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' Detecting duplicated tracks, and deciding which record to exclude. 
#' If the two entries have the same individual_local_identifier 
#' and/or tag_local_identifier, and they overlap in time 
#' we can save the one that either has longer duration, 
#' or higher number of recorded points
#' OUTPUT: "3_deployment_duplicates_check.rds"

# 0 - Defining parameters and packages ---------------------------------------

library(move2)
library(here)
library(tidyverse)
library(sf)
library(DescTools)

# getting the species of interest
# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()



# 1 - Summarize tracks ----------------------------------------------------

# summarizing tracks for when it started and ended, how many tracking days
# there are, and the time lag between locations

original_tracks <- target_sp |>
  map(~{
    
    # species directory
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    
    # get all the study files
    sfiles <- here(sp_dir, "2_cleaned") |> 
      list.files(pattern = ".rds")
    lfl <- length(sfiles)
    
    sfiles |> 
      map(~{
        
        fin <- .x
        
        print(paste(sp, which(fin == sfiles), "|", lfl))
        
        track <- here(sp_dir, "2_cleaned", fin) |> 
          read_rds()
        
        track |> 
          mutate(timelag = mt_time_lags(track, units = "min")) |> 
          # in the anne's script there was a check for individual_local_id, but 
          # individual local identifier should not be missing considering that
          # the data was downloaded using this local id
          mutate(
            across(contains("tag_"), as.character),
            tag_local_identifier = if_else(
              is.na(tag_local_identifier), #all(is.na(tag_local_identifier))
              tag_id,
              tag_local_identifier
              )
          ) |> 
          # getting the summary of each deployment:
          # start, end, number of tracking days, etc. 
          st_drop_geometry() |> 
          as_tibble() |> 
          summarise(
            across(contains(c("_id", "identifier")), unique),
            track_start = min(timestamp), 
            track_end = max(timestamp),
            track_period_days = as.numeric(
              difftime(track_end, track_start, units = "days")
            ),
            track_unique_days = length(unique(date(timestamp))), 
            median_timelag_mins = median(timelag, na.rm = T),
            mean_timelag_mins = mean(timelag, na.rm = T),
            min_timelag_mins = min(timelag, na.rm = T),
            n_locs = n()
          ) |> 
          mutate(across(contains(c("period", "timelag")), ~round(.x, 1))) |>
          mutate(file = fin) 
        
      }) |> 
      bind_rows() |> 
      mutate(species = sp)
          
  } 
  ) |> 
  bind_rows() 


# 2 - Define function -----------------------------------------------------

# find which track from the duplicates to exclude based on the temporal overlap
# and track duration or number of location per track 
# input: df with duplicated tracks
# output: vector of tracks to exclude (identified by row numbers)
 
find_tracks_to_exclude <- function(
    df, row_id = "row_id", start = "track_start", end = "track_end", 
    n_locs = "n_locs", duration = "track_unique_days",
    keep_criteria = "locations" # locations or duration
    ){
  
  if(nrow(df) == 1){
    
    print("only one track provided")
    
    excluded <- NULL
    
  } else { # close if(nrow(df) == 1)
    
    df <- df |> 
      rename(track_start = {{start}}, track_end = {{end}}) |> 
      mutate(n_na = rowSums(is.na(pick(everything()))))
    
    row_ids <- df |> 
      pull({{row_id}})
    
    overlapping <- expand_grid(id1 = row_ids, id2 = row_ids) |> 
      # removing comparisons to them selves (eg. ID2, ID2)
      filter(id1 != id2) |> 
      # removing the same comparisons, but different order
      # we collapse id1 and id2, and order them alphabetically, so that 
      # the same combo has always the same value
      # eg. for ID3-ID4 and ID4-ID3 => id combo would be ID3ID4
      rowwise() |> 
      mutate(id_combo = str_c(sort(c_across(c("id1", "id2"))), collapse = "")) |> 
      ungroup() |> 
      distinct(id_combo, .keep_all = T) |> 
      select(-id_combo) |> 
      # getting important info for each deployment
      mutate(
        id1_start = df$track_start[match(id1, df$row_id)], 
        id1_end =  df$track_end[match(id1, df$row_id)], 
        n_na1 = df$n_na[match(id1, df$row_id)], 
        id2_start = df$track_start[match(id2, df$row_id)], 
        id2_end =  df$track_end[match(id2, df$row_id)], 
        n_na2 = df$n_na[match(id2, df$row_id)]
      ) |> 
      # checking if the selected two (id1 and id2) tracks overlap in time
      rowwise() |> 
      mutate(
        overlap = c(id1_start, id1_end) %overlaps% c(id2_start, id2_end)
      ) |> 
      ungroup() |> 
      # keeping only the overlapping ones
      filter(overlap == T)
  
    if(nrow(overlapping) == 0) {
      
      print("there is no overlapping tracks")
      
      excluded = NULL
      
    } else { # if(nrow(overlapping) != 0)
      
      if(keep_criteria == "locations"){
        
        df <- df |>
          rename(n_locs = {{ n_locs }})
        
        # define which tracks to exclude
        excluded <- overlapping |>
          rowwise() |> 
          mutate(
            n_locs1 = df$n_locs[match(id1, df$row_id)],
            n_locs2 =  df$n_locs[match(id2, df$row_id)]
          ) |>
          mutate(
            selected = case_when(
              n_locs1 < n_locs2 ~ id1,
              n_locs2 < n_locs1 ~ id2,
              n_locs1 == n_locs2 & n_na1 > n_na2 ~ id1,
              n_locs1 == n_locs2 & n_na2 > n_na1 ~ id2,
              .default = id2
            )
          ) |> 
          ungroup() |>
          distinct(selected) |>
          pull()
        
        
        print("keeping track with the largest number of locations")
        
        } # close keep_criteria == "locations" 
      
      if(keep_criteria == "duration"){
        
        df <- df |>
          rename(duration = {{ duration }})
        
        # define which tracks to exclude
        excluded <- overlapping |>
          rowwise() |>
          mutate(
            duration1 = df$duration[match(id1, df$row_id)],
            duration2 =  df$duration[match(id2, df$row_id)]
          ) |>
          mutate(
            selected = case_when(
              duration1 < duration2 ~ id1,
              duration2 < duration1 ~ id2,
              duration1 == duration2 & n_na1 > n_na2 ~ id1,
              duration1 == duration2 & n_na2 > n_na1 ~ id2,
              .default = id2
            )
          ) |>
          ungroup() |> 
          distinct(selected) |>
          pull()
        
        print("keeping track with the longest duration")
        
      } # close keep_criteria == "duration"
      
    } # close else if(nrow(overlapping) == 0)
  
  } # close else if(nrow(df) == 1)
  
  # returns row_id which should be excluded
  return(excluded)  
    
}  


# 3 - Detect overlapping tracks ---------------------------------------------

# marking tracks to exclude using the function above 

original_tracks <- original_tracks |>  
  mutate(row_id = str_c("row_", 1:n())) |>
  group_split(species, individual_local_identifier) |> 
  map(~{
    
    ind_df <- .x
    
    rows_to_exclude <- find_tracks_to_exclude(
      ind_df, 
      keep_criteria = "duration"
    )
    
    ind_df |> 
      mutate(dup_ind_to_exclude = row_id %in% rows_to_exclude)
      
  }
  ) |> 
  bind_rows() |> 
  group_split(species, tag_local_identifier) |> 
  map(~{
    
    tag_df <- .x
    
    rows_to_exclude <- find_tracks_to_exclude(
      tag_df, 
      keep_criteria = "duration"
    )
    
    tag_df |> 
      mutate(dup_tag_to_exclude = row_id %in% rows_to_exclude)
    
  }) |> 
  bind_rows() |> 
  mutate(
    script_ran_on = Sys.time(), 
    excluded = if_else(
      dup_tag_to_exclude | dup_ind_to_exclude, "yes", "no", missing = "no"
    )
  ) 


original_tracks |> 
  write_rds(here("Data", "Studies", "3_deployment_duplicates_excluded.rds"))


# sanity check, 
# after running the script went through some of the selected tracks to 
# exclude to check if it makes sense
# duplicated_tracks <- original_tracks |>
#   group_split(species) |>
#   map(~{
# 
#     all_tracks <- .x
# 
#     duplicated_inds <- all_tracks |>
#       filter(duplicated(individual_local_identifier)) |>
#       pull(individual_local_identifier)
# 
#     duplicated_tags <- all_tracks |>
#       filter(duplicated(tag_local_identifier)) |>
#       pull(tag_local_identifier)
# 
#     all_tracks |>
#       mutate(
#         duplicated_ind = individual_local_identifier %in% duplicated_inds,
#         duplicated_tag = tag_local_identifier %in% duplicated_tags
#       )
#   }
#   ) |>
#   bind_rows() |>
#   filter(if_any(contains("duplicated"), ~.==T)) |>
#   select(
#     individual_local_identifier, tag_local_identifier,
#     track_start, track_end, track_unique_days, excluded
#   )

