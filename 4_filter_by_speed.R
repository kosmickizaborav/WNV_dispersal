#' ---
#' title: "filter tracks by speed using literature limits"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Flight speed limits from literature**
#' Bird speed data was downloaded from here, and converted from pdf to excel 
#' using an online converter
#' https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.0050197#s5
#' Bruderer_2001_extracted_from_paper extracted manually from paper: 
#' https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1474-919x.2001.tb04475.x
#'
#' **SECTION 2 - Speed distribution in unfiltered data**
#' - explore the speed distribution for each species
#' - plot graphs of speed and turning angle
#' - save the quantiles of speed distribution
#' 
#' **SECTION 3 - Filtering the speed**
#' using the literature limit for each species, filter the tracks, and save them 
#'     
#' **SECTION 4 - Speed distribution in filtered data**
#' code almost identical as in section 2 
#' I avoided using function as it takes longer to compute
#' - plot graphs of speed and turning angle
#' - save the quantiles of speed distribution


# 0 - packages and files --------------------------------------------------

library(tidyverse)
library(here)
library(move2)
library(sf)
library(readxl)
library(units)
library(ggpubr)
library(suncalc)
library(rnaturalearth)

tracks_list <- here("Data", "Studies", "3_deployment_duplicates_excluded.rds") |> 
  read_rds() |> 
  filter(excluded == "no") |> 
  select(file, species)

# getting species of interest
target_sp <- tracks_list |>
  distinct(species) |>
  pull() 
  # str_replace(" ", "_")

# 1 - Create output directories -------------------------------------------

# folder for filtered speed and graphs
target_sp |> 
  map(~{
    
    
    sp_dir <- here("Data", "Studies", str_replace(.x, " ", "_"))
    graphs_dir <- here(sp_dir, "Graphs")
    speed_dir <- here(sp_dir, "4_filtered_speed")
    
    if(!dir.exists(graphs_dir)){ dir.create(graphs_dir) }

    if(!dir.exists(speed_dir)){ dir.create(speed_dir) }
    
  })


# 2 -  Flight speed limits from literature --------------------------------

# data is loaded from literature: 
# for "Alerstam_2007_supplement_table_extracted.xlsx" I direclty copied the
# table provided in the supplementary (converting the pdf to excel using adobe), 
# for the bruderer I copied manually the values for the target species
# the highest value was selected as a limit and rounded up, so that 
# we just have approximate limit, as decimals are too precise

bird_speeds <- here("Alerstam_2007_supplement_table_extracted.xlsx")|> 
  read_xlsx(skip = 1) |> 
  select(2:4) |> 
  rename_with(~c("species", "speed", "sd"), everything()) |> 
  mutate(
    max_speed = if_else(is.na(sd), speed, speed + 2*sd), 
    species = str_squish(str_remove(species, "•"))
  ) |> 
  summarise(max_speed = max(max_speed), .by = species) |> 
  filter(species %in% target_sp)  |> 
  full_join(
    here("Bruderer_2001_extracted_from_paper.xlsx") |> 
      read_xlsx() |> 
      janitor::clean_names() |> 
      mutate(max_speed_bruderer = eq_va + 2*sd), 
    by = "species"
  ) |> 
  summarise(
    speed_lim = ceiling(max(c(max_speed_bruderer, max_speed), na.rm = T)), 
    .by = species
  ) 
  #mutate(speed_lim = set_units(speed_lim, m/s))


# 3 - Plot speed distribution before filtering ---------------------------------

# get the speeds and turning angles for every species

tracks_list |>
  select(file, species) |> 
  group_split(species) |> 
  map(~{
    
    # getting all file paths per species
    files <- .x |> pull(file)
    lfl <- length(files)
    
    # species directory
    sp <- unique(.x$species)
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))


    # 3 - 1 - Prepare data --------------------------------
    
    # getting data frame with all speeds from the species
    speeds_df <- files |>
      map(~{
        
        fin <- .x 
        
        print(paste(sp, which(fin == files), "|", lfl))

        track <- here(sp_dir, "2_cleaned", fin) |> 
          read_rds() 
        
        # calculate speed and turn
        track |> 
          mutate(
            speed = mt_speed(track, units="m/s"), 
            turn = mt_turnangle(track)
          ) |> 
          as_tibble() |>
          select(speed, turn) 
            
        }) |> 
      bind_rows() |> 
      drop_units()
    
   
    # 3 - 2 - Plot speeds and turns------------------------------
    
    # extracting the speed limit for the selected species (from the segment 1)
    speed_limit <- bird_speeds |> 
      filter(species == sp) |> 
      pull() 
    
    # checking which quantile is lower then the defined speed limit
    sp_limits_graph <- tibble(
      speed = unname(quantile(speeds_df$speed, seq(0.95, 1, 0.001), na.rm = T)), 
      name = str_c(seq(95, 100, 0.1), "% quantile")
      ) |> 
      filter(speed <= speed_limit) |> 
      filter(as.numeric(speed) <= as.numeric(speed_limit)) |> 
      arrange(speed) |> 
      slice_tail(n = 1) |> 
      bind_rows(tibble(speed = speed_limit, name = "applied speed limit")) |> 
      mutate(text = str_c(name, ": ", round(speed, 2), " m/s"))

    # plot the graphs of speeds and turning angles
    ps <- speeds_df |> 
      filter(!is.na(speed)) |> 
      ggplot() +
      geom_histogram(aes(speed), fill = "grey55", color = "black") +
      geom_vline(
        data = sp_limits_graph, 
        mapping = aes(xintercept = speed, color = text), 
        linewidth = 1.2
      ) +
      labs(x = "speed [m/s]", color = "") +
      theme_bw() +
      theme(legend.position = "bottom")
    
    pta <- speeds_df |> 
      filter(!is.na(turn)) |> 
      ggplot() +
      geom_histogram(aes(turn), fill = "grey55", color = "black") + # binwidth = 0.1) +
      labs(x = "turning angle [rad]") +
      theme_bw() 
    
    # save the graph
    ggarrange(ps, pta, common.legend = T) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp, " - unfiltered tracks"), face = "bold", size = 14
        )
      )
    
    ggsave(
      here(sp_dir, "Graphs", "4_speed_turn_before_filtering.png"), 
      width = 25, unit = "cm"
    )
    
    print(paste(sp, "DONE!"))
    
  }) 

gc(verbose = F)

# 4 - Filter maximum speeds -----------------------------------------------

# 4 - 1 - Variables to add ------------------------------------------------

# variables used in the section 4, to extract dawn and dusk times, and check 
# where in the world the track is located

# load world map for checking weather the track is in europe or elsewhere
# scale was set to "medium", becasue with large i get the following error
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented, : 
# Loop 0 is not valid: Edge 1028 has duplicate vertex with edge 1033
world <- ne_countries(scale = "medium", returnclass = "sf") |> 
  select(sovereignt, admin, continent) |> 
  rename(country = sovereignt, country_admin = admin)

# europe ranges taken from: 
# https://en.wikipedia.org/wiki/Extreme_points_of_Europe
# Latitude: 34°N to 81°N
# Longitude: 29°W to 69°E
eu_coords <- list(
  ymin = 34.0,
  ymax = 81.0,
  xmin = -29.0,
  xmax = 69.0
)

# define times of the sun positions that will be included in the data
sun_times <- c(
  "nightEnd", "night", "nauticalDawn", "nauticalDusk", "dawn", "dusk"
)

# eu_bb <- st_as_sfc(
#   st_bbox(
#     c(
#       xmin = eu_xmin, xmax = eu_xmax, ymin = eu_ymin, ymax = eu_ymax
#     ),
#     crs = st_crs(4326)
#     )
# )
# ORIGINALLY USED THIS, BUT GAVE UP, BECAUSE IT RETURNS WEIRD RESULTS
# e.g. for Turdus # POINT (29.70283 40.92096) returns FALSE
# track |>
#   mutate(europe = as.vector(st_intersects(geometry, eu_bb, sparse = FALSE)))
# TIME TRACKING
# start_time <- Sys.time()
# used for time tracking but gave up on that

tracks_list |>
  group_split(species) |>
  map(~{

    # getting files per species
    files <- .x |> pull(file)
    lfl <- length(files)

    # defining the specie sdir
    sp <- unique(.x$species)
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    # extracting the speed limit for the species
    speed_limit <- bird_speeds |> 
      filter(species == sp) |> 
      pull() |> 
      set_units("m/s")
    
    files |>
      map(~{
        
        # define input and output files
        fin <- .x
        fout <- fin |> str_replace("_cleaned.rds", "_speed_filtered.rds")

        print(paste(sp, which(fin == files), "|", lfl))


        # 4 - 2 - Filter speed ------------------------------------
        
        # load track and calculate speed
        track <- here(sp_dir, "2_cleaned", fin) |>
          read_rds()
        track <- track |> 
          mutate(speed = mt_speed(track, units = "m/s")) 
        
        # calculate the maximum speed from the tracking data and 
        # keep filtering out the points that produce max speed higher than
        # the speed limit from the literature (section 2)
        max_speed <- max(track$speed, na.rm = T)
        
        while(max_speed > speed_limit) {
          
          track <- track |> 
            filter(speed <= speed_limit)
    
          track <- track |> 
            mutate(speed = mt_speed(track, units="m/s")) 
          
          if(nrow(track) > 1){ 
            
            max_speed <- max(track$speed, na.rm = T) 
            
          } else {
            
            print("track data insufficient")
            break
          
          }
        } # close while loop for speed
        
        # save the track only if after filtering there is at least 3 points available
        if(nrow(track) >= 3){
          
          # adding the table with the times of dawn and dusk
          sun_time <- getSunlightTimes(
              data = tibble(
                date = date(track$timestamp), 
                lat = track$lat,
                lon = track$lon
              ), 
              keep = sun_times, # defined in the section 0
              tz = "UTC"
            ) |> 
            mutate(row_id = str_c("row_", 1:n()))
            
          track |>  
            mutate(row_id = str_c("row_", 1:n())) |>
            left_join(sun_time, by = c("lon", "lat", "row_id")) |>
            # getting the country and continent
            st_join(world) |> 
            mutate(
              within_eubb = (
                lon >= eu_coords$xmin & lon <= eu_coords$xmax & 
                  lat >= eu_coords$ymin & lat <= eu_coords$ymax
              )
            ) |> 
            select(
              -row_id, -lon, -lat, -speed, -date, 
              -contains(c("tag", "deployment_local")),
            ) |> 
            write_rds(
              here(sp_dir, "4_filtered_speed", fout)
            )
        } # close if(nrow(track) > 2)
         

      }) # close map for files 
    
    print(paste(sp, "DONE!"))

  })  # close map for species


# 5 - Speed distribution after filtering ----------------------------------

# get the speeds and turning angles for every species

target_sp |>
  map(~{
    
    # define species directory
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    # getting all the files
    file_paths <- here(sp_dir, "4_filtered_speed") |> 
      list.files(full.names = T) 
    lfl <- length(file_paths)
    

    speed_df <- file_paths |>
      map(~{
        
        print(paste(sp, which(.x == file_paths), "|", lfl))
        
        track <- .x |>
          read_rds() 
        
        track |> 
          mutate(
            speed = mt_speed(track, units="m/s"), 
            turn = mt_turnangle(track)
          ) |> 
          as_tibble() |>
          select(speed, turn) 
        
      }) |> # close file_paths
      bind_rows() 
    
    ps <- speed_df |> 
      filter(!is.na(speed)) |> 
      ggplot() +
      geom_histogram(
        aes(speed), fill = "grey55", color = "black"
      ) + #, binwidth = 10
      theme_bw()
    
    pta <- speed_df |> 
      filter(!is.na(turn)) |> 
      ggplot() +
      geom_histogram(
        aes(turn), fill = "grey55", color = "black"
      ) + # binwidth = 0.1) +
      theme_bw() 
    
    ggarrange(ps, pta) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp, " - tracks filtered by speed"), face = "bold", size = 14
        )
      )
    
    ggsave(here(sp_dir, "Graphs", "4_speed_turn_after_filtering.pdf"))
    
  }) # close map species
  


# BACKUP code for obtaining day cycles ---------------------------------------

# before we included information on the day cycle, but now I am not sure we
# will need it anymore, so here I just leave the older version of code used 
# to get the day cycles just in case we need it for the future

# adding the table with the times of dawn and dusk
# sun_time <- getSunlightTimes(
#   data = tibble(
#     date = date(track$timestamp), 
#     lat = st_coordinates(track)[, 2],
#     lon = st_coordinates(track)[, 1]
#   ), 
#   keep = sun_times, # defined in the section 0
#   tz = "UTC"
# ) |> 
#   mutate(
#     timestamp = track$timestamp,
#     year = year(date),
#     yearday = yday(date),
#     row_id = str_c("row_", 1:n())
#   ) |>
#   rename_with(~"day_start", all_of(sun_times[["start"]])) |>
#   rename_with(~"day_end", all_of(sun_times[["end"]])) |>
#   # grouping days by light cycle
#   mutate(
#     day_cycle = if_else(
#       timestamp < day_start, 
#       str_c(yday(date - 1), "_", year(date - 1)),
#       str_c(yearday, "_", year)
#     )
#   ) |> 
#   select(
#     row_id, day_cycle, day_start, day_end, 
#     any_of(sun_times), lon, lat
#   )


# BACKUP code for saving processing time ----------------------------------

# TIME TRACKING
# bind_rows() 
# mutate(
#   compute_start_time = start_time, 
#   computing_time = compute_end_time - compute_start_time
# ) 


