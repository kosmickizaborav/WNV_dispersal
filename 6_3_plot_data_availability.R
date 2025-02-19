#' ---
#' title: "Plotting steps in time and space"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  here we plot data availabiliy for all species, using the functions
#'  saved in the script 6_2_plot_functions.R
#'  **SECTION 1 - Plot maps for all species**
#'  generate plot for every species and every night steps file we have, 
#'  the plots show spatial and temporal distribution of the data
#'  **SECTION 2 - Plot different day defintions**
#'  plot the number of deployments and night steps obtained using different 
#'  definitions of day limits
  

# 0 - Load packages -------------------------------------------------------

library(tidyverse)
library(here)
source(here("6_2_plot_functions.R"))

# get the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# 1 - Plot maps for all species -------------------------------------------

EU <- c(T, F)
tit <- "spatial and temporal distribution of tracking data used to obtain night steps"
cap <- "color of lines in timeline and points in map indicate year, see timeline for years with the most data"

target_sp |> 
  map(~{
    
    sp <- .x
    
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    files <- here(sp_dir, "6_distances") |> 
      list.files(pattern = "2_all_tracks_night_steps_")
    
    files |> 
      map(~{
        
        fin <- .x
        fout <- str_c("6_2_plot_", str_remove(fin, ".rds"), ".png")
        
        tit_full <- str_c(sp, "\n", tit)
        cap_full <- str_c("file: ", fin, "\n", cap)
        
        night_steps <- here(sp_dir, "6_distances", fin) |> 
          read_rds() |> 
          filter(night_steps_available == T) |> 
          mutate(
            day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
            yd = yday(day_cycle),
            m = month(day_cycle), 
            d = day(day_cycle), 
            y = year(day_cycle)
          )
        
        EU |> 
          map(~{
            
            eu <- .x
            
            if(eu == T){
              
              night_steps <- night_steps |> 
                filter(within_eubb == T)
              
              fout <- str_replace(fout, ".png", "_EU.png")
              tit_full <- paste(
                sp,  
                "- within the Europe bbox [34°N-81°N, 29°W-69°E]", "\n", 
                tit
              )
              
            } 
            
            # palette that will indicate different years
            pal <- get_palette(night_steps, y = y)
            
            # plots
            wmap <- plot_on_world_map(night_steps, pal = pal)
            tl <- plot_steps_timeline(night_steps, pal = pal)
            
            pout <- wmap/tl + 
              plot_annotation(
                title = tit_full, 
                caption = cap_full,
                theme = theme(
                  plot.title = element_text(face = "bold", hjust = 0.5)
                )
              ) +
              plot_layout(heights = c(1.5, 1))
            
            # save the plots
            ggsave(
              here(sp_dir, "Graphs", fout), 
              pout, 
              width = 35, height = 25, units = "cm"
            )
            
            print(fout)
            
          }) # close eu map
        
      }) # close file map
    
    print(paste(sp, "DONE!"))
    
    
  })


# 2 - Plot different day defintions ---------------------------------------

month_limits <- get_month_limits()

pal <- c("gray33", "#E9E4A6FF", "#E9A17CFF", "#1BB6AFFF")
names(pal) <- c(
  "all deployments", "[dawn, dusk]", 
  "[nauticalDawn, nauticalDusk]", "[nightEnd, night]"
)

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    files <- here(sp_dir, "6_distances") |> 
      list.files(pattern = "2_all_tracks_night_steps_")
    
    full_df <- files |> 
      map(~{
        
        day_lim = str_remove_all(.x, "2_all_tracks_night_steps_|\\.rds")
        
        here(sp_dir, "6_distances", .x) |> 
          read_rds() |> 
          mutate(
            day_cycle = as.Date(day_cycle, format = "%d%m%Y"), 
            yd = yday(day_cycle),
            day_lim = day_lim
          ) |> 
          select(track_file, yd, day_lim, night_steps_available, within_eubb)
        
      }) |> 
      list_rbind() |> 
      # modify day limits for the plot
      mutate(day_lim = str_c("[", str_replace(day_lim, "_", ", "), "]")) 
    
    zero_counts <- expand.grid(
      yd = 1:366, n_steps = 0, day_lim = unique(full_df$day_lim)
    )
    
    fout <- str_c("6_2_", str_replace(sp, " ", "_"), "_steps_availability.png")
    title_out <- str_c(sp, " - data availability")
    subtitle_out <- "night steps obtained using different day definitions"
    
    EU |> 
      map(~{
        
        eu <- .x 
        
        if(eu == T){
          
          full_df <- full_df |> 
            filter(within_eubb == T)
          
          fout <- str_replace(fout, ".png", "_EU.png")
          title_out <- paste(title_out, "- European bbox")
          
        }
        
        n_total <- n_distinct(full_df$track_file)
        
        full_df <- full_df |> 
          filter(night_steps_available == T)
        
        # getting counts per dataset 
        counts_df <- full_df |>
          summarize(
            n_deploys = n_distinct(track_file),
            n_steps = n(),
            day_lim = unique(day_lim),
            .by = c(night_steps_available, day_lim)
          ) |> 
          bind_rows(tibble(day_lim = "all deployments", n_deploys = n_total))
        
        pdc <- counts_df |> 
          mutate(day_lim = factor(day_lim, levels = names(pal))) |>
          plot_bars(
            x = n_deploys, y = day_lim, fill = day_lim, pal = pal, 
            ylab = "day definition", 
            title = "Deployments used to obtain night steps"
          )
        
        psc <- counts_df |>
          mutate(
            day_lim = if_else(day_lim == "all deployments", "", day_lim)
          ) |> 
          plot_bars(
            x = n_steps, y = day_lim, fill = day_lim, pal = pal, 
            title = "Number of steps obtained"
          ) + 
          theme(axis.text.y = element_blank())
        
        
        # getting counts of steps per day
        step_times <- full_df |> 
          summarize(n_steps = n(), .by = c(yd, day_lim)) |> 
          bind_rows(zero_counts) |>
          summarize(n_steps = sum(n_steps), .by = c(yd, day_lim))
        
        pst <- step_times |> 
          plot_steps_count(
            counts = n_steps, color = day_lim, pal = pal, linetype = "dotdash", 
            title = "Number of steps per day of the year", 
            month_limits = month_limits
          ) +
          theme(legend.position = "none") 
        
        # output plot 
        pout <- (pdc + psc) / pst +
          plot_annotation(
            title = title_out, 
            subtitle = subtitle_out, 
            theme = theme(
              plot.title = element_text(face = "bold", hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5)
            )
          ) +
          plot_layout(heights = c(1, 1))
        
        # save the plots
        ggsave(
          here("Data", "Graphs", fout), 
          pout, 
          width = 35, height = 20, units = "cm"
        )
        
        
      }) # close EU map
    
    print(paste(sp, "DONE!"))
    
  }) # close species map
