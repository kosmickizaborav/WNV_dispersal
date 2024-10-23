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


#source("0_helper_functions.R")


tracks_list <- here("Data", "Studies", "3_complete_deployment_list.rds") |> 
  read_rds() |> 
  filter(excluded == "no") |> 
  mutate(
    sp = str_replace(species, " ", "_"), 
    file_path = here("Data", "Studies", sp, "2_cleaned", file)
  )

# getting species of interest
target_sp <- tracks_list |> 
  distinct(species) |> 
  pull()


# folder for the graphs output
target_sp |> 
  str_replace(" ", "_") |> 
  map(~{
    
    if(!dir.exists(here("Data", "Studies", .x, "Graphs"))){
      
      dir.create(here("Data", "Studies", .x, "Graphs"))
      
    }
  })

day_lim <- c(end = "nauticalDusk", start = "nauticalDawn")


# 1 -  Flight speed limits from literature --------------------------------


# data is just loaded from literature 
# the highest value was selected as a limit

bird_speeds <- here("Alerstam_2007_supplement_table_extracted.xlsx")|> 
  read_xlsx(skip = 1) |> 
  select(2:4) |> 
  rename_with(~c("species", "speed", "sd"), everything()) |> 
  mutate(
    max_speed = if_else(is.na(sd), speed, speed + 2*sd), 
    species = str_squish(str_remove(species, "•"))
    # genus = str_split_i(species, " ", 1)
  ) |> 
  summarise(max_speed = max(max_speed), .by = species) |> 
  filter(species %in% target_sp) |>  
  full_join(
    here("Bruderer_2001_extracted_from_paper.xlsx") |> 
      read_xlsx() |> 
      janitor::clean_names() |> 
      mutate(max_speed_bruderer = eq_va + 2*sd), 
    by = "species"
  ) |> 
  summarise(
    speed_lim = max(c(max_speed_bruderer, max_speed), na.rm = T), 
    .by = species
  ) |> 
  mutate(speed_lim = set_units(speed_lim, m/s))


# 2 - Speed distribution in data ------------------------------------------

# get the speeds and turning angles for every species

start_time <- Sys.time()

speed_species <- tracks_list |>
  select(file_path, species) |> 
  group_split(species) |> 
  map(~{

    file_paths <- .x |> pull(file_path)
    
    lfp <- length(file_paths)
    
    sp_id <- unique(.x$species)


    file_paths |>
      map(~{

        print(paste(sp_id, which(.x == file_paths), "|", lfp))

        track <- .x |>
          read_rds() 
        
        track |> 
          mutate(
            speed = mt_speed(track, units="m/s"), 
            turn = mt_turnangle(track)
          ) |> 
          as_tibble() |>
          select(-event_id) |> 
          select(contains(c("_id", "_identifier")), speed, turn) 
            
            
          }) |> 
      bind_rows() |> 
      mutate(
        species = sp_id, 
        compute_end_time = Sys.time(), 
        compute_start_time = start_time
      )
    
    }
  ) 


# plotting speed and turning angles before filtering
speed_species |> 
  map(~{
    
    sp_id <- unique(.x$species)
    
    out_dir <- here("Data", "Studies", str_replace(sp_id, " ", "_"), "Graphs")
    
    ps <- ggplot(.x) +
      geom_histogram(aes(speed), fill = "grey55", color = "black")+ #, binwidth = 10
      theme_bw()
    
    pta <- ggplot(.x) +
      geom_histogram(aes(turn), fill = "grey55", color = "black") + # binwidth = 0.1) +
      theme_bw() 

    ggarrange(ps, pta) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp_id, " - unfiltered data"), face = "bold", size = 14
        )
      )
    
    ggsave(here(out_dir, "4_unfiltered_speed_turn.pdf"))
    
    }
  )

# getting the quantiles, to understand which part of the speed distribution 
# in the data is outside of the literature range
# the output is saved just for the future reference

quant_preclean <- speed_species |> 
  map(~{

    #sp_quant <- round(quantile(.x$speed, seq(0.5,1,0.01), na.rm = T), 2)
    sp_quant <- round(
      quantile(.x$speed, seq(0.95, 1, 0.001), na.rm = T),
      2
    )

    tibble(
      speed = sp_quant, #c(sp_quant, sp_quant_precise),
      quant = names(sp_quant), #c(names(sp_quant), names(sp_quant_precise)),
      species = unique(.x$species)
    )

  }) |>
  bind_rows() |>
  mutate(species = str_replace(species, "_", " ")) |>
  left_join(
    bird_speeds,
    by = "species"
  ) 

quant_preclean |> 
  write_csv(here("Data", "Studies", "4_unfiltered_speed_quantiles.csv"))

times <- speed_species |> 
  map(~{
    .x |> 
      distinct(species, compute_end_time, compute_start_time) |> 
      mutate(computing_time = compute_end_time - compute_start_time) 
  }) |> 
  bind_rows()

rm(speed_species)



# 3 - Filter speeds above the treshold ------------------------------------

start_time <- Sys.time()

species_clean <- tracks_list |>
  group_split(species) |>
  map(~{

    files <- .x |> pull(file)
    lfl <- length(files)

    sp <- unique(.x$sp)
    
    speed_limit <- bird_speeds |> 
      filter(species == unique(.x$species)) |> 
      pull()
    
    out_dir <- here("Data",  "Studies", sp, "4_filtered_speed")
    
    if(!dir.exists(out_dir)){ 
      
      dir.create(out_dir) 
      
    }
    
    files |>
      map(~{
        
        fname <- .x

        print(paste(sp, which(fname == files), "|", lfl))

        track <- here("Data", "Studies", sp, "2_cleaned", fname) |>
          read_rds()
        
        track <- track |> 
          mutate(speed = mt_speed(track, units = "m/s"))
        
        max_speed <- max(track$speed, na.rm = T)
        
        while(max_speed > speed_limit) {
          
          track <- track |> 
            # group_by(mt_track_id) |> 
            filter(speed <= speed_limit)
    
          track <- track |> 
            mutate(speed = mt_speed(track, units="m/s"))
          
          max_speed <- max(track$speed, na.rm = T)
          
        }
        
        if(nrow(track) > 2){
          
          # adding the table with the times of dawn and dusk
          sun_time <- getSunlightTimes(
              data = tibble(
                date = date(track$timestamp), 
                lat = st_coordinates(track)[, 2],
                lon = st_coordinates(track)[, 1]
              ), 
              keep = c(day_lim[["start"]], day_lim[["end"]]),
              tz = "UTC"
            ) |> 
            mutate(
              timestamp = track$timestamp,
              year = year(date),
              yearday = yday(date),
              row_id = str_c("row_", 1:n())
            ) |>
            rename_with(~"day_start", all_of(day_lim[["start"]])) |>
            rename_with(~"day_end", all_of(day_lim[["end"]])) |>
            # grouping days by light cycle
            mutate(
              day_cycle = str_c(
                if_else(timestamp < day_start, yearday - 1, yearday),
                "_",
                year
              )
            ) |> 
            select(row_id, day_cycle, day_start, day_end)
            
          
          track |>  
            mutate(row_id = str_c("row_", 1:n())) |> 
            left_join(sun_time, by = "row_id") |> 
            select(-row_id) |> 
            write_rds(
              here(
                out_dir, 
                str_replace(fname, "_cleaned.rds", "_speed_filtered.rds")
              )
            )
        }
         

      }) # close map for files
    
    tibble(
      species = sp, 
      compute_end_time = Sys.time()
    ) 

  }) |> # close map for species
  bind_rows() |> 
  mutate(
    compute_start_time = start_time, 
    computing_time = compute_end_time - compute_start_time
  ) 


# 4 - Speed distribution after filtering ----------------------------------


# get the speeds and turning angles for every species

start_time <- Sys.time()


target_sp |>
  map(~{
    
    sp <- .x
    
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    file_paths <- here(sp_dir, "4_filtered_speed") |> 
      list.files(full.names = T) 
    
    lfp <- length(file_paths)
    

    speed_df <- file_paths |>
      map(~{
        
        print(paste(sp, which(.x == file_paths), "|", lfp))
        
        track <- .x |>
          read_rds() 
        
        track |> 
          mutate(
            speed = mt_speed(track, units="m/s"), 
            turn = mt_turnangle(track)
          ) |> 
          as_tibble() |>
          select(-event_id) |> 
          select(contains(c("_id", "_identifier")), speed, turn) 
        
        
      }) |> # close file_paths
      bind_rows() 
    
    ps <- ggplot(speed_df) +
      geom_histogram(aes(speed), fill = "grey55", color = "black") + #, binwidth = 10
      theme_bw()
    
    pta <- ggplot(speed_df) +
      geom_histogram(aes(turn), fill = "grey55", color = "black") + # binwidth = 0.1) +
      theme_bw() 
    
    ggarrange(ps, pta) |> 
      annotate_figure(
        top = text_grob(
          str_c(sp, " - tracks filtered by speed"), face = "bold", size = 14
        )
      )
    
    ggsave(here(sp_dir, "Graphs", "4_filtered_speed_turn.pdf"))
    
    sp_quant <- round(
      quantile(speed_df$speed, seq(0.95, 1, 0.001), na.rm = T), 
      2
    )
    
    tibble(
      speed = sp_quant, #c(sp_quant, sp_quant_precise),
      quant = names(sp_quant), #c(names(sp_quant), names(sp_quant_precise)),
      species = sp, 
      compute_time_end = Sys.time()
    )

  }) |> 
  bind_rows() |> 
  left_join(
    bird_speeds,
    by = "species"
  ) |>  
  write_csv(here("Data", "Studies", "4_filtered_speed_quantiles.csv"))
  




# There were 20 warnings 
# Warning messages:
#   1: Removed 428 rows containing non-finite values (`stat_bin()`).
# 2: Removed 272857 rows containing non-finite values (`stat_bin()`).
# 3: Removed 23 rows containing non-finite values (`stat_bin()`).
# 4: Removed 108317 rows containing non-finite values (`stat_bin()`).
# 5: Removed 237 rows containing non-finite values (`stat_bin()`).
# 6: Removed 1083694 rows containing non-finite values (`stat_bin()`).
# 7: Removed 202 rows containing non-finite values (`stat_bin()`).
# 8: Removed 689 rows containing non-finite values (`stat_bin()`).
# 9: Removed 608 rows containing non-finite values (`stat_bin()`).
# 10: Removed 3211 rows containing non-finite values (`stat_bin()`).
# 11: Removed 427 rows containing non-finite values (`stat_bin()`).
# 12: Removed 272584 rows containing non-finite values (`stat_bin()`).
# 13: Removed 23 rows containing non-finite values (`stat_bin()`).
# 14: Removed 108317 rows containing non-finite values (`stat_bin()`).
# 15: Removed 236 rows containing non-finite values (`stat_bin()`).
# 16: Removed 1082314 rows containing non-finite values (`stat_bin()`).
# 17: Removed 197 rows containing non-finite values (`stat_bin()`).
# 18: Removed 676 rows containing non-finite values (`stat_bin()`).
# 19: Removed 574 rows containing non-finite values (`stat_bin()`).
# 20: Removed 3117 rows containing non-finite values (`stat_bin()`).



# Function for speeds, slows down -----------------------------------------

# # it is 2 minutes slower with the funciton 
# get_speeds <- function(file_paths, add_turns = F){
# 
#   if(add_turns == T){
# 
#     out_df <- file_paths |>
#       map(~{
# 
#         print(paste(.x, which(.x == file_paths), "|", length(file_paths)))
# 
#         .x |>
#           read_rds() |>
#           group_split(deployment_id) |>
#           map(~ {
# 
#             track <- .x
# 
#             track |>
#               mutate(
#                 speed = mt_speed(track),
#                 turns = mt_turnangle(track)
#               ) |>
#               as_tibble() |>
#               select(contains(c("_id", "_identifier")), speed, turns) |>
#               select(-event_id) |>
#               mutate(file_path = .x)
#           }
#           )
#       }
#       )
# 
#   } else {
# 
#     out_df <- file_paths |>
#       map(~{
# 
#         print(paste(.x, which(.x == file_paths), "|", length(file_paths)))
# 
#         .x |>
#           read_rds() |>
#           group_split(deployment_id) |>
#           map(~ {
# 
#             track <- .x
# 
#             track |>
#               mutate(speed = mt_speed(track)) |>
#               as_tibble() |>
#               select(contains(c("_id", "_identifier")), speed) |>
#               select(-event_id) |>
#               mutate(file_path = .x)
#           }
#           )
# 
# 
#       }) |>
#       bind_rows()
# 
#   }
# }
