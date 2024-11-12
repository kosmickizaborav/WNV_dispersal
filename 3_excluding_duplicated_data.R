#' ---
#' title: "Excluding duplicated data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' Detecting duplicated tracks, and deciding which record to exclude. 
#' If the two entries overlap in time we save the one that either has longer 
#' duration, or higher number of recorded points. 

# 0 - Defining parameters and packages ---------------------------------------

library(move2)
library(here)
library(tidyverse)
library(sf)
library(DescTools)

# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector()


# detecting duplicated individuals ----------------------------------------


original_tracks <- target_sp |>
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    sfiles <- here(sp_dir, "2_cleaned") |> 
      list.files(pattern = ".rds")
    
    sfiles |> 
      map(~{
        
        fname <- .x
        
        print(paste(sp, fname, which(fname == sfiles), "|", length(sfiles)))
        
        track <- here(sp_dir, "2_cleaned", fname) |> 
          read_rds()
        
        track |> 
          mutate(timelag = mt_time_lags(track, units = "min")) |> 
          # in the anne's script there was a check for individual_local_id, but 
          # individual local identifier should not be missing considering that
          # the data was downloaded using this local id
          mutate(
            across(contains("tag_"), as.character),
            tag_local_identifier = if_else(
              is.na(tag_local_identifier),
              tag_id,
              tag_local_identifier
              )
          ) |> 
          # getting the summary of each deployment:
          # start, end, number of tracking days, etc. 
          st_drop_geometry() |> 
          as_tibble() |> 
          select(-event_id) |> 
          summarise(
            across(contains(c("_id", "identifier")), unique),
            track_start = min(timestamp), 
            track_end = max(timestamp),
            track_period_days = round(
              as.numeric(difftime(track_end, track_start, units="days")), 2
            ),
            track_unique_days = length(unique(date(timestamp))), 
            median_timelag_mins = round(median(timelag, na.rm = T), 1),
            mean_timelag_mins = round(mean(timelag, na.rm = T), 1),
            min_timelag_mins = round(min(timelag, na.rm = T), 1),
            n_locs = n()
          ) |> 
          mutate(file = fname) 
        
      }) |> 
      bind_rows() |> 
      mutate(species = sp)
          
  } 
  ) |> 
  bind_rows() 


# finding duplicates ------------------------------------------------------

#--------------------------------------------------------------------------
# function to find tracks to exclude --------------------------------------
#--------------------------------------------------------------------------


# find which track from the duplicates to exclude based on the temporal overlap
# and track duration or number of location per track 
# input: df with duplicated tracks
# output: vector of tracks to exclude (identified by row numbers)
 
find_tracks_to_exclude <- function(
    df, row_id = "row_id", start = "track_start", end = "track_end", 
    n_locs = "n_locs", duration = "track_unique_days",
    keep_criteria = "locations" # locations or duration
    ){
  
  df <- df |> 
    rename(track_start = {{start}}, track_end = {{end}}) |> 
    mutate(n_na = rowSums(is.na(pick(everything()))))
  
  row_ids <- df |> 
    pull({{row_id}})
  
  overlapping <- expand_grid(id1 = row_ids, id2 = row_ids) |> 
    # removing comparisons to them selves (eg. ID2, ID2)
    filter(id1 != id2) |> 
    # removing te same comparisons, but different order 
    # (eg. ID3-ID4 and ID4-ID3) 
    rowwise() |> 
    mutate(fltr = str_c(sort(c_across(c("id1", "id2"))), collapse = "")) |> 
    distinct(fltr, .keep_all = TRUE) |>  
    select(-fltr) |> 
    mutate(
      id1_start = df$track_start[df$row_id == id1], 
      id1_end =  df$track_end[df$row_id == id1], 
      id2_start = df$track_start[df$row_id == id2], 
      id2_end =  df$track_end[df$row_id == id2], 
      n_na1 = df$n_na[df$row_id == id1], 
      n_na2 = df$n_na[df$row_id == id2]
    ) |> 
    mutate(
      overlap = c(id1_start, id1_end) %overlaps% c(id2_start, id2_end)
    ) |> 
    ungroup() |> 
    filter(overlap == T)
  
  if(nrow(overlapping) == 0) {

    print("there is no overlapping tracks")
    
    excluded = NULL

  } else {

    if( keep_criteria == "locations" ){

      df <- df |>
        rename(n_locs = {{ n_locs }})

      excluded <- overlapping |>
        rowwise() |> 
        # in theory unnecessary, but otherwise produced error with id: 
        # control 358
        mutate(
          n_locs1 = df$n_locs[df$row_id == id1],
          n_locs2 =  df$n_locs[df$row_id == id2]
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

    }

    if( keep_criteria == "duration" ){

      df <- df |>
        rename(duration = {{ duration }})

      excluded <- overlapping |>
        rowwise() |> 
        mutate(
          duration1 = df$duration[df$row_id == id1],
          duration2 =  df$duration[df$row_id == id2]
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

    }
    
  }
  
  return(excluded)  
    
  }  


#--------------------------------------------------------------------------
###########################################################################
#--------------------------------------------------------------------------

# marking tracks to exclude using the function above 

original_tracks <- original_tracks |>  
  mutate(
    row_id = str_c("row_", 1:n())
    #excluded = "no"
  ) |>
  group_split(species, individual_local_identifier) |> 
  map(~{
    
    ind_df <- .x
    
    if(nrow(ind_df) > 1) {
      
      rows_to_exclude <- find_tracks_to_exclude(
        ind_df, 
        keep_criteria = "duration"
        )
      
      ind_df <- ind_df |> 
        mutate(dup_ind_to_exclude = row_id %in% rows_to_exclude)
      
    }
    
    ind_df
  }
  ) |> 
  bind_rows() |> 
  group_split(species, tag_local_identifier) |> 
  map(~{
    
    tag_df <- .x
    
    if(nrow(tag_df) > 1) {
      
      rows_to_exclude <- find_tracks_to_exclude(
        tag_df, 
        keep_criteria = "duration"
        )
      
      tag_df <- tag_df |> 
        mutate(dup_tag_to_exclude = row_id %in% rows_to_exclude)
      
    }
    
    tag_df
  }
  ) |> 
  bind_rows() |> 
  mutate(
    script_ran_on = Sys.time(), 
    excluded = if_else(
      dup_tag_to_exclude | dup_ind_to_exclude, "yes", "no", missing = "no"
    )
  ) 


original_tracks |> 
  write_rds(here("Data", "Studies", "3_complete_deployment_list.rds"))

original_tracks |> 
  as_tibble() |> 
  write_csv(here("Data", "Studies", "3_complete_deployment_list.csv"))


# sanity check
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

