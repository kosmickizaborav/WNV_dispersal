#' ---
#' title: "visualizing daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

# because some species were exhibiting extreme distances that repeat exactly, 
# explored the manipulation comments, and excluded the deployments that said
# [3] "animal owned by farm and may have been restricted in movement or transported for sale (see Deployment Comments)"
#[4] "displacement"                                                                                                   
#[5] "Displacement" 

# 0 - packages and files --------------------------------------------------

# to handle movement data
library(data.table)
library(tidyverse)
library(here)
library(move2)
library(sf)
library(amt)
library(ggpubr)
library(suncalc)

source("0_helper_functions.R")


sun_times <- c(start = "dawn", end = "dusk")


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() |> 
  str_replace(" ", "_")


# summary graph directory
ggraph_dir <- here("Data",  "Graphs")

dcols <- c("study_id", "individual_id", "deployment_id", "species", "file")

dep_df <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |> 
  select(any_of(dcols), sensor_type_ids, manipulation_comments) |> 
  mutate(
    sensor = case_when(
      str_detect(sensor_type_ids, "sigfox") ~ "sigfox", 
      str_detect(sensor_type_ids, "radio") ~ "radio", 
      str_detect(sensor_type_ids, "argos") ~ "argos", 
      str_detect(sensor_type_ids, "gps") ~ "gps"
    )
  ) |> 
  mutate(
    problematic_deployment = str_detect(
        manipulation_comments, "restricted in movement|[Dd]isplacement"
    )
  )


# 1 - Preparing data ------------------------------------------------------


files <- c(
    "5a_all_tracks_one_loc_per_day_bursts.rds", 
    "5a_all_tracks_one_loc_per_day_morning_bursts.rds", 
    "5b_all_tracks_max_daily_distance.rds"
  )

dfs <- files |> 
  map(~{
    
    fname <- .x
    
    target_sp |> 
      map(~{
        
        sp <- .x
        
        sp_dir <- here("Data", "Studies", sp)
        
        track <- here(sp_dir, "5_distances", fname) |> 
          read_rds() 
        
        if(str_detect(fname, "max")){
          
          track <- track |> 
            rename(t1_ = timestamp_1, t2_ = timestamp_2, sl_ = max_dist) |> 
            mutate(
              x1_ = st_coordinates(location_1)[,1], 
              y1_ = st_coordinates(location_1)[,2],
              x2_ = st_coordinates(location_2)[,1], 
              y2_ = st_coordinates(location_2)[,2]
            ) |> 
            filter(!is.na(sl_))
          
        }
          
        track <- track |> 
          mutate(row_id = str_c("r_", 1:n()))
        

        sun_df <- getSunlightTimes(
          data = tibble(
            date = date(track$t1_),
            lat = track$y1_,
            lon = track$x1_
          ),
          keep = sun_times,
          tz = "UTC"
          ) |>
          mutate(row_id = track$row_id) |> 
          rename_with(~"day_start", all_of(sun_times[["start"]])) |>
          rename_with(~"day_end", all_of(sun_times[["end"]]))
          # grouping days by light cycle
          
        track |> 
          left_join(sun_df, by = "row_id") |>
          mutate(
            day_cycle = if_else(
              t1_ < day_start, 
              str_c(yday(date-1), "_", year(date-1)),
              str_c(yday(date), "_", year(date))
            ),
            day_period = if_else(
              t1_ >= day_start & t1_ <= day_end, "day", "night"
            )
          ) |> 
          mutate(dup_days = n() > 1, .by = c(file, day_cycle)) |> 
          mutate(
            day_id = if_else(
              dup_days, str_c(day_cycle, "_", day_period), day_cycle
              )
          ) |> 
          select(
            any_of(c(dcols, "day_id")),
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_$"))
          ) |> 
          mutate(
            species = str_replace(sp, "_", " "), 
            file_id = str_remove_all(fname, "5[ab]_all_tracks_|_bursts|.rds") |> 
              str_replace_all("_", " "), 
            sl_ = as.numeric(sl_),
            sl_km = sl_/1000,
            sl_log = log10(ifelse(sl_ == 0, sl_ + 1e-5, sl_)),
            month = month(t1_, label = T)
          ) |> 
          filter(!is.na(day_id)) 
        
      }) |> 
      bind_rows() |> 
      left_join(dep_df) |> 
      filter(is.na(problematic_deployment) | problematic_deployment == F) 
    
  }, .progress = T)



rm(dep_df, files)



# 2 - Distances vs. sensor type -------------------------------------------


dfs |> 
  map(~{
    
    df <- .x
    
    f_id <- unique(df$file_id)
    
    pn <- str_c("5c_sensors_", str_replace_all(f_id, " ", "_"), ".pdf")
    pn_l <- str_c("5c_sensors_", str_replace_all(f_id, " ", "_"), "_log.pdf")
    pn_b <- str_c("5c_sensors_", str_replace_all(f_id, " ", "_"), "_box.pdf")
    
    # overview plot-------------------------------------------------------------
    
    df |> 
      ggplot() +
      geom_boxplot(
        aes(x = sl_km, y = sensor, group = sensor),  
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~species, ncol = 1, scales = "free") +
      labs(
        x = "step length [km] | binwidth = 1", 
        title = paste("Distances -", f_id,"| different sensors")
      ) +
      theme_bw()
    #scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    
    ggsave(
      here(ggraph_dir, pn_b), 
      height = 15, 
      units = "cm"
    )
    
    # plots per species---------------------------------------------------------
    df |> 
      group_split(species) |> 
      map(~{
        
        sp <- unique(.x$species)
        
        sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
        
        .x |> 
          ggplot() +
          geom_histogram(
            aes(x = sl_km),  
            fill = "gray66", color = "black", 
            binwidth = 1
          ) +
          facet_wrap(~sensor, ncol = 1, scales = "free") +
          labs(
            x = "step length [km] | binwidth = 1km", 
            title = paste(sp, "-", f_id, "| different sensors")
          ) +
          theme_bw()
          #scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
        
        ggsave(
          here(sp_dir, "Graphs", pn), 
          units = "cm"
        )
        
        .x |> 
          ggplot() +
          geom_histogram(
            aes(x = sl_log),  
            fill = "gray66", color = "black", 
            bins = 100
          ) +
          facet_wrap(~sensor, ncol = 1, scales = "free") +
          labs(
            x = "step length [m] - log scale | bins = 100", 
            title = paste(sp, "-", f_id, "| different sensors")
          ) +
          theme_bw()
        #scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
        
        ggsave(
          here(sp_dir, "Graphs", pn_l), 
          units = "cm"
        )
        
      })
    
   
    
  })



# 3 - Distances vs. months ------------------------------------------------



# 3.1. subset for europe ------------------------------------------------------

# chat gpt said this is the boundry box for europe,
# added 5 more degrees for safety 
# Latitude range: 34.8° N to 71.2° N
# Longitude range: -31.3° W to 69.1° E
eu_bb <- st_as_sfc(
  st_bbox(c(xmin = -25, xmax = 75, ymax = 30, ymin = 75), crs = st_crs(4326))
)

dfs |> 
  map(~{
    
    df <- .x |> 
      st_as_sf(coords = c("x1_", "y1_"), crs = st_crs(4326)) |>
      filter(as.vector(st_intersects(geometry, eu_bb, sparse = FALSE)))


    f_id <- unique(df$file_id)

    pn <- str_c("5c_eu_months_", str_replace_all(f_id, " ", "_"), ".pdf")
    pn_l <- str_c("5c_eu_months_", str_replace_all(f_id, " ", "_"), "_log.pdf")
    pn_b <- str_c("5c_eu_months_", str_replace_all(f_id, " ", "_"), "_box.pdf")
    
    # overview plot-------------------------------------------------------------
    df |>
      ggplot() +
      geom_boxplot(
        aes(x = sl_km, y = species),
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~month, ncol = 2, scales = "free") +
      labs(
        x = "step length [km]",
        title = paste("Distances |", f_id, "| Europe, across months"),
      ) +
      theme_bw()
    
    ggsave(
      here(ggraph_dir, pn_b),
      units = "cm",
      height = 30
    )
    
    
    # graphs by species---------------------------------------------------------

    df |>
      group_split(species) |>
      map(~{

        sp <- unique(.x$species)

        sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))

        .x |>
          ggplot() +
          geom_histogram(
            aes(x = sl_km),
            fill = "gray66", color = "black",
            binwidth = 1
          ) +
          facet_wrap(~month, ncol = 3, scales = "free") +
          labs(
            x = "step length [km] | binwidth = 1km",
            title = paste(sp, "-", f_id, "| Europe, across months")
          ) +
          theme_bw() 
          # scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))

        ggsave(
          here(sp_dir, "Graphs", pn),
          width = 25,
          units = "cm"
        )

        .x |>
          ggplot() +
          geom_histogram(
            aes(x = sl_log),
            fill = "gray66", color = "black",
            bins = 100
          ) +
          facet_wrap(~month, ncol = 3, scales = "free") +
          labs(
            x = "step length [m] - log scale | bins = 100",
            title = paste(sp, "-", f_id, "| Europe, across months")
          ) +
          theme_bw()

        ggsave(
          here(sp_dir, "Graphs", pn_l),
          width = 25,
          units = "cm"
        )
        
      })
    
  })


  

# 3.2. all tracks ---------------------------------------------------------


dfs |> 
  map(~{
    
    df <- .x 
    
    
    f_id <- unique(df$file_id)
    
    pn <- str_c("5c_months_", str_replace_all(f_id, " ", "_"), ".pdf")
    pn_l <- str_c("5c_months_", str_replace_all(f_id, " ", "_"), "_log.pdf")
    pn_b <- str_c("5c_months_", str_replace_all(f_id, " ", "_"), "_box.pdf")
    
    # overview plot-------------------------------------------------------------
    
   
    df |>
      ggplot() +
      geom_boxplot(
        aes(x = sl_km, y = species),
        fill = "gray66", color = "black"
      ) +
      facet_wrap(~month, ncol = 2, scales = "free") +
      labs(
        x = "step length [km]",
        title = paste("Distances |", f_id, "| across months"),
      ) +
      theme_bw()
    
    ggsave(
      here(ggraph_dir, pn_b),
      units = "cm",
      height = 30
    )
    
    
    # graphs by species---------------------------------------------------------
    
    df |>
      group_split(species) |>
      map(~{
        
        sp <- unique(.x$species)
        
        sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
        
        .x |>
          ggplot() +
          geom_histogram(
            aes(x = sl_km),
            fill = "gray66", color = "black",
            binwidth = 10
          ) +
          facet_wrap(~month, ncol = 3, scales = "free") +
          labs(
            x = "step length [km] | binwidth = 10km",
            title = paste(sp, "-", f_id, "by month")
          ) +
          theme_bw() 
        # scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
        
        ggsave(
          here(sp_dir, "Graphs", pn),
          width = 25,
          units = "cm"
        )
        
        .x |>
          ggplot() +
          geom_histogram(
            aes(x = sl_log),
            fill = "gray66", color = "black",
            bins = 100
          ) +
          facet_wrap(~month, ncol = 3, scales = "free") +
          labs(
            x = "step length [m] - log scale | bins = 100",
            title = paste(sp, "-", f_id, "| across months")
          ) +
          theme_bw()
        
        ggsave(
          here(sp_dir, "Graphs", pn_l),
          width = 25,
          units = "cm"
        )
        
      })
    
  })



# 4 - Different distances -----------------------------------------------------

dfs <- dfs |>
  map(~{
    
    file_id <- unique(.x$file_id)
    
    n_fix <- str_extract_all(file_id, "one|morning|max") |> 
      unlist() |> 
      str_c(collapse = "_")
    
    .x |> 
      select(-file_id) |> 
      rename_with(
        ~str_c(str_remove(., "_$"), "_", n_fix),
        any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_"))
      )
    
  })

df <- dfs[[1]] |> 
  full_join(dfs[[2]]) |>
  full_join(dfs[[3]])

rm(dfs)

df |> 
  group_split(species) |> 
  map(~{
    
    sp <- unique(.x$species)
    
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    pmo <- .x |>
      ggplot() +
      geom_point(
        aes(x = sl_km_max, y = sl_km_one),
        color = "grey55", alpha = .6, na.rm = T
      ) +
      labs(
        x = "step length [km] | daily max",
        y = "step length [km] | one loc per day",
        title = str_c(sp, "- daily distances | max vs. one loc per day")
      ) +
      theme_bw() 
    # scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_max_vs_one.pdf"),
      width = 25,
      units = "cm"
    )
    
    pmo +
      facet_wrap(~month, ncol = 3, scales = "free")
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_by_month_max_vs_one.pdf"),
      height = 30,
      units = "cm"
    )
    
    pmom <-.x |>
      ggplot() +
      geom_point(
        aes(x = sl_km_max, y = sl_km_one_morning),
        color = "grey55", alpha = .6, na.rm = T
      ) +
      labs(
        x = "step length [km] | daily max",
        y = "step length [km] | one loc per day morning",
        title = str_c(sp, "- daily distances | max vs. one loc per day")
      ) +
      theme_bw() 
    # scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_max_vs_morning.pdf"),
      width = 25,
      units = "cm"
    )
    
    pmom + 
      facet_wrap(~month, ncol = 3, scales = "free")
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_by_month_max_vs_morning.pdf"),
      height = 30,
      units = "cm"
    )
    
    poom <- .x |>
      ggplot() +
      geom_point(
        aes(x = sl_km_one, y = sl_km_one_morning),
        color = "grey55", alpha = .6, na.rm = T
      ) +
      labs(
        x = "step length [km] | one loc per day",
        y = "step length [km] | one loc per day morning",
        title = str_c(sp, "- daily distances | one loc per day vs. morning")
      ) +
      theme_bw() 
    # scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_one_vs_morning.pdf"),
      width = 25,
      units = "cm"
    )
    
    poom + 
      facet_wrap(~month, ncol = 3, scales = "free")
    
    ggsave(
      here(sp_dir, "Graphs", "5c_steps_by_month_one_vs_morning.pdf"),
      height = 30,
      units = "cm"
    )
    
    
  })
  

