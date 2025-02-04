#' ---
#' title: "6 1 calculate dcp distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' This script is used to generate the distances between the consecutive nights
#' as well as the maximum distances reached during the day.
#' INPUT:
#' - libraries needed
#' - sp and sp_dir: species and its folder 
#' - files and lfl: list of the files to be processed, and their number 
#' - limits for day and night: day_start, day_end
#' - cols_of_interest, day_lim: columns of interest and the day limit
#' 
#'  **SECTION 1 - calculate night steps**
#'  **SECTION 2 - calculate day steps**
#'  
#'  The output is saved in the 6_distances folder as night and day steps

# define output dataframe
dcp_df <- tibble()


# Loop through all deployments --------------------------------------------


for(fin in files){
  
  print(paste(sp, which(fin == files), "|", lfl))
  
  track <- here(sp_dir, "5_resampled", fin) |> 
    read_rds() |> 
    select(x_, y_, t_, all_of(c(cols_of_interest, day_lim))) |> 
    filter(!if_all(all_of(c("day_start", "day_end")), ~is.na(.))) 
  
  if(nrow(track) > 0){
    
    dcp_track <- track |>
      # adding day and night parameters
      mutate() |> 
      # in the northern latitudes sometimes the sun doesn't set so 
      # to filter out those locations if we are using the nautical dusk
      # filter(!is.na(day_start) & !is.na(day_end))
      mutate(
        date = as.Date(t_), 
        # before I defined day_cycle as day of the year, but decided to stick
        # with the numbers because in this way I don't mix feb 29 and march 1st
        # str_c(yday(date - 1), "_", year(date - 1)),
        # str_c(yday(date), "_", year(date))
        day_cycle = if_else(
          t_ < day_start,
          # in case the time is before day start for that date
          # asign the day_cycle to the previous day, so that night has continuity
          as.character(format(date-1, "%d%m%Y")), 
          as.character(format(date, "%d%m%Y"))     
        ),
        # alternative t_ > day_start & t_ < day_end, "day", "night" 
        # gives slightly different results
        # i defined night wihtout = sign so that we keep the "darkest"
        # period always, I had doubts with the variables night and nightEnd, 
        # but decided to keep this because of dawn and dusk, 
        # to make sure it's before dawn and after dusk
        day_period = if_else(t_ < day_start | t_ > day_end, "night", "day"),
        dcp = str_c(day_cycle, "_", day_period)
      ) |> 
      select(-date, -day_period, -day_cycle) |> 
      # group by dcp and calculate the distances between the points
      group_split(dcp) |> 
      map(~{
        
        # 1 - For each dcp calculate distances and median coordinates ---------
        
        dcp_one <- .x 
        
        # calculate median location of during the day_cycle_period
        dcp_summary <- dcp_one |> 
          summarize(
            n_locs = n(), 
            x_median = median(x_),
            y_median = median(y_),
            t_median = median(t_), 
            dcp = unique(dcp)
          )
        
        if(dcp_summary$n_locs > 1){
          
          # distance matrix between all the points available for dcp
          dist <- dcp_one |> 
            st_as_sf(coords = c("x_", "y_"), crs = get_crs(track)) |> 
            st_distance()
          dist[lower.tri(dist, diag = TRUE)] <- NA
          
          dcp_summary |> 
            mutate(
              dist_min = min(dist, na.rm = T),
              dist_max = max(dist, na.rm = T),
              dist_mean = mean(dist, na.rm = T),
              dist_median = median(dist, na.rm = T)
            ) |> 
            mutate(
              across(
                starts_with("dist"), ~ifelse(is.numeric(.x), .x, NA)
              )
            )
        }
        
      }) |>  # map period
      bind_rows() |> 
      mutate(
        track_file = fin, 
        dcp_available = T
      )
    
  } else{ # close if track 
    
    dcp_track <- tibble(
      track_file = fin, 
      n_locs = NA, 
      dcp_available = F
    )
  
  }
  
  
  dcp_df <- dcp_df |> 
    bind_rows(dcp_track)
  
  rm(track, dcp_track)
}


# Write output files -------------------------------------------------------------

# save the track
dcp_df |> 
  write_rds(here(sp_dir, "6_distances", dcp_file))

rm(dcp_df)
gc(verbose = F)

