
# FUNCTION: find_tracks_to_exclude ---------------------------------------------

# find which track from the duplicates to exclude based on the temporal overlap
# and track duration or number of location per track 
# the idea for the function originates from a script provided by Anne Scharf
# input: df with duplicated tracks
# output: vector of tracks to exclude (identified by row numbers)

library(DescTools)

find_duplicated_tracks <- function(
    df, track_id, start, end, n_locs, duration,
    keep_criteria = "duration" # locations or duration
){
  
  if(nrow(df) == 1){ 
    
    cat("\n-----------only one track provided, none excluded!----------")
    return(NULL)
    
  } 
  
  df <- df |> 
    rename(
      track_start = {{start}}, track_end = {{end}}, track_id = {{track_id}}
    ) |> 
    mutate(n_na = rowSums(is.na(pick(everything()))))
  
  track_ids <- df |> pull(track_id)
  
  overlapping <- expand_grid(id1 = track_ids, id2 = track_ids) |> 
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
      id1_start = df$track_start[match(id1, df$track_id)], 
      id1_end = df$track_end[match(id1, df$track_id)], 
      n_na1 = df$n_na[match(id1, df$track_id)], 
      id2_start = df$track_start[match(id2, df$track_id)], 
      id2_end = df$track_end[match(id2, df$track_id)], 
      n_na2 = df$n_na[match(id2, df$track_id)]
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
    
    cat("\n--------------there is no overlapping tracks----------------")
    return(NULL)
    
  } 
  
  if(keep_criteria == "locations"){
    
    df <- df |>
      rename(n_locs = {{ n_locs }})
    
    # define which tracks to exclude
    excluded <- overlapping |>
      rowwise() |> 
      mutate(
        n_locs1 = df$n_locs[match(id1, df$track_id)],
        n_locs2 =  df$n_locs[match(id2, df$track_id)]
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
    
    
    cat("\n----keeping track with the largest number of locations!-----")
    
  } # close keep_criteria == "locations" 
  
  if(keep_criteria == "duration"){
    
    df <- df |>
      rename(duration = {{ duration }})
    
    # define which tracks to exclude
    excluded <- overlapping |>
      rowwise() |>
      mutate(
        duration1 = df$duration[match(id1, df$track_id)],
        duration2 =  df$duration[match(id2, df$track_id)]
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
    
    cat("\n----------keeping track with the longest duration!----------")
    
  }
  
  # returns track_id which should be excluded
  return(excluded)  
  
}  