#' ---
#' title: "visualizing daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

# dialectical biology book
# the good the bad and the rarified (slobodkin)
# invention of nature book
# 

# because some species were exhibiting extreme distances that repeat exactly, 
# explored the manipulation comments, and excluded the deployments that said
# 1: "animal owned by farm and may have been restricted in movement or 
# transported for sale (see Deployment Comments)"
# 2: "displacement" / "Displacement" 

# 0 - packages and files --------------------------------------------------

# to handle movement data
library(tidyverse)
library(here)
library(sf)
library(ggpubr)
library(patchwork)
library(paletteer)

source("0_helper_functions.R")


# getting the species of interest
target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_types = F) |> 
  distinct(species) |> 
  as_vector() 

idcols <- c(
  "study_id", "individual_id", "deployment_id", "species", "file", "day_id"
  )

# summary graph directory
ggraph_dir <- here("Data",  "Graphs")



# 3 - Distances overview --------------------------------------------------


dist_files <- c(
  "5.3_all_tracks_one_loc_per_day_bursts_graph_data.rds", 
  "5.3_all_tracks_one_loc_per_day_morning_bursts_graph_data.rds", 
  "5.3_all_tracks_max_daily_distance_graph_data.rds"
  # "5.3_all_tracks_net_square_displacement_graph_data.rds"
  )

graphs <- c(
  "5.4_comparison_daily_distances.pdf",
  "5.4_europe_comparison_daily_distances.pdf"
)

dist_files |> 
  map(~{
    fname <- .x
    
    df <- target_sp |> 
      map(~{
        
        sp <- .x
        sp_dir <- here("Data", "Studies", sp)
        
        here(sp_dir, "5_distances", fname) |> 
          read_rds() |> 
          mutate(species = str_replace(sp, " ", "_"))
      }) |> 
      bind_rows()
    
    df |> 
      ggplot() + 
      stat_ecdf(
        aes(x = sl_km, color = species, linetype = manipulation_type), 
        geom = "step"
      ) +
      theme_bw() + 
      facet_wrap(~season, scales = "free", ncol = 1) +
      scale_colour_paletteer_d("awtools::ppalette")  + 
      labs(
        title = "One location per day", 
        x = "step length [km]"
      )
    
  })

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    files <- here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds")
    
    df_all <- files |> 
      map(~{
        here(sp_dir, "5_distances", .x) |> 
          read_rds() 
      }) |> 
      bind_rows()
    
    df_all |> 
      filter(file_id != "net sqare displacement") |> 
      ggplot() + 
      stat_ecdf(
        aes(x = sl_km, color = file_id, linetype = in_europe), 
        geom = "step"
      ) +
      theme_bw() + 
      facet_grid(~season, scales = "free") +
      scale_colour_paletteer_d("awtools::ppalette")  + 
      labs(
        title = "One location per day", 
        x = "step length [km]", 
        color = "distance type"
      )
    
    # printing graphs with all the data and with subset for europe
    graphs |> 
      map(~{
        
        pn <- .x
        p_title <- paste(str_replace(sp, "_", " "), "| daily distances")
        
        if(str_detect(pn, "europe")){
          
          df_all <- df_all |> 
            filter(in_europe == T)
        
          p_title <- str_c(p_title, ", Europe")
          
        }
          
          # p: hist [km] per data set
          h <- df_all |> 
            hist_km() +
            facet_wrap(~file_id, ncol = 1, scales = "free")
          
          # p: hist [log m] per data set
          hl <- df_all |> 
            hist_log() +
            facet_wrap(~file_id, ncol = 1, scales = "free")
          
          ggarrange(h, hl, nrow = 1) |> 
            annotate_figure(
              top = text_grob(
                p_title, 
                face = "bold", size = 12
                #paste(sp, "- distances |", f_id), face = "bold", size = 12
              )
            )
          
          ggsave(here(sp_dir, "Graphs", pn), width = 20, units = "cm")
          
      })
    
  })


df_all |> 
 

# 4 - Distances vs. sensors -----------------------------------------------

pn <- "5.4_sensor_comparison_daily_distances.pdf"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    files <- here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds")
    
    df_all <- files |> 
      map(~{
        here(sp_dir, "5_distances", .x) |> 
          read_rds() 
      }) |> 
      bind_rows()
    
    # p: hist [km] per data set
    h <- df_all |> 
      hist_km() +
      facet_grid(sensor~file_id, scales = "free")
    
    # p: hist [log m] per data set
    hl <- df_all |> 
      hist_log() +
      facet_grid(sensor~file_id, scales = "free")
    
    ggarrange(h, hl, ncol = 1) |> 
      annotate_figure(
        top = text_grob(
          paste(str_replace(sp, "_", " "), " | daily distances vs. sensors"), 
          face = "bold", size = 12
          #paste(sp, "- distances |", f_id), face = "bold", size = 12
        )
      )
    
    ggsave(here(sp_dir, "Graphs", pn), width = 20, units = "cm")
    
  })


# 5 - Distances vs. months ------------------------------------------------

graphs <- c(
  "5.4_monthly_",
  "5.4_europe_monthly_"
)

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    files <- here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds")
    
    files |> 
      map(~{
        
        df <- here(sp_dir, "5_distances", .x) |> 
          read_rds() 
        
        f_id <- unique(df$file_id)
        
        # printing graphs with all the data and with subset for europe
        graphs |> 
          map(~{
            
            pn <- str_c(.x, str_replace_all(f_id, " ", "_"))
            p_title <- paste(
              str_replace(sp, "_", " "), "|", f_id, "| across months"
            )
            
            if(str_detect(pn, "europe")){
              
              df <- df |> 
                filter(in_europe == T)
              
              p_title <- str_c(p_title, ", Europe")
              
            }
            
            # p: hist [km] vs. months
            df |>
              hist_km(binw = 1) +
              facet_wrap(~month, ncol = 3, scales = "free") +
              plot_annotation(
                title = p_title, 
                theme = theme(plot.title = element_text(face = "bold", size = 12))
              )
            
            ggsave(
              here(sp_dir, "Graphs", str_c(pn, "_km.pdf")), 
              height = 20, units = "cm"
            )
            
            # p: hist [m log] vs. months
            df |>
              hist_log() +
              facet_wrap(~month, ncol = 3, scales = "free") +
              plot_annotation(
                title = p_title, 
                theme = theme(plot.title = element_text(face = "bold", size = 12))
              )
            
            ggsave(
              here(sp_dir, "Graphs", str_c(pn, "_m_log.pdf")), 
              height = 20, units = "cm"
            )
            
          }) # map graphs
        
      }) # map files
    
    print(paste(sp, "DONE!"))
    
  }) # map species


# 6 - Distances vs. distances ---------------------------------------------

graphs <- c(
  "max_vs_one", "max_vs_one_morning", "one_vs_one_morning"
)

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    files <- here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds")
    
    df <- files |> 
      map(~{
        
        df <- here(sp_dir, "5_distances", .x) |> 
          read_rds() 
        
        n_fix <- unique(df$file_id) |> 
          str_extract_all("one|morning|max") |> 
          unlist() |> 
          str_c(collapse = "_")
        
        df |> 
          select(
            any_of(idcols),
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_|^dt_$"))
          ) |> 
          rename_with(
            ~str_c(str_remove(., "_$"), "_", n_fix),
            any_of(matches("^x[12]_$|^t[12]_$|^y[12]_$|^sl_|^dt_$"))
          )
      }) |> 
      reduce(full_join, by = idcols)
    
    graphs |> 
      map(~{
        
        print(.x)
        
        pn <- str_c("5.4_compare_dist_", .x, '.pdf')
        
        vars <- str_split(.x, "_vs_") |> 
          unlist() 
        
        p_title <- paste(
          sp, "- distances |", 
          str_c(str_replace(vars, "_", " "), collapse = " vs. "), 
          "loc per day"
        )
        
        c_vars <- c(str_c("sl_km_", vars), str_c("sl_log_", vars))
        
        df_plot <- df |> 
          select(any_of(c_vars)) |> 
          rename_with(~c("x", "y", "x_log", "y_log"), all_of(c_vars))
        
        pk <- df_plot |> 
          d_vs_d(x = x, y = y, xlab = c_vars[1], ylab = c_vars[2])
        
        pl <- df_plot |> 
          d_vs_d(x = x_log, y = y_log, xlab = c_vars[3], ylab = c_vars[4])
        
        ggarrange(pk, pl, nrow = 1) |> 
          annotate_figure(
            top = text_grob(
              p_title, 
              face = "bold", size = 12
              #paste(sp, "- distances |", f_id), face = "bold", size = 12
            )
          )
        
        ggsave(here(sp_dir, "Graphs", pn), width = 25, units = "cm")
      
        
      }) # map graphs
    
    print(paste(sp, "DONE!"))
    
    
  }) # map species
  



# 7 - Max distances time period -------------------------------------------

in_file <- "5.3_all_tracks_max_daily_distance_graph_data.rds"
pn <- "5.4_max_distances_vs_time_period.pdf"

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    p_title <- paste(sp, "| max distances vs. time period between the two points")
    
    df <- here(sp_dir, "5_distances", in_file) |> 
      read_rds() |> 
      mutate(dt_hour = round(as.numeric(dt_, units = "hours"), 0))

    hp <- df |> 
      mutate(
        dt_hour = str_c("time period: ", dt_hour, "h") |> 
          factor(levels = str_c("time period: ", c(1:24), "h"))
      ) |> 
      hist_km() +
      facet_wrap(~dt_hour, scales = "free", nrow = 3)
    
    hb <- df |> 
      ggplot() + 
      geom_boxplot(
        aes(x = dt_hour, y = n_locs, group = dt_hour), 
        fill = "#9BB655FF", color = "gray22"
      ) + 
      labs(
        y = "number of locations per day", 
        x = "time period between the two points of the max distance"
      )
      theme_bw()
    
    ggarrange(hp, hb, nrow = 2) |> 
      annotate_figure(top = text_grob(p_title, face = "bold", size = 12))
    
    ggsave(here(sp_dir, "Graphs", pn), width = 25, units = "cm")
                      
    
  })


# 8 - Distances vs. species ---------------------------------------------------

# distance in kilometers

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds", full.names = T) |>
      map(read_rds) |> 
      bind_rows() |> 
      hist_km() +
      facet_wrap(~file_id, nrow = 1, scales = "free") +
      labs(title = sp) + 
      theme(plot.title = element_text(face = "bold"))

  }) |> 
  reduce(`+`) + 
  plot_layout(ncol = 1) + 
  plot_annotation(
    title = "Daily distances across species [km]", 
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  ) 

# Save the combined plot
ggsave(
  here(ggraph_dir, "5.4_distances_across_species_km.pdf"), 
  height = 25, units = "cm"
  )

# distance in meters log scale

target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
    
    here(sp_dir, "5_distances") |> 
      list.files(pattern = "graph_data.rds", full.names = T) |>
      map(read_rds) |> 
      bind_rows() |> 
      hist_log() +
      facet_wrap(~file_id, nrow = 1, scales = "free") +
      labs(title = sp) + 
      theme(plot.title = element_text(face = "bold"))
    
  }) |> 
  reduce(`+`) + 
  plot_layout(ncol = 1) + 
  plot_annotation(
    title = "Daily distances across species [m - log scale]", 
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
  ) 

# Save the combined plot
ggsave(
  here(ggraph_dir, "5.4_distances_across_species_log.pdf"), 
  height = 25, units = "cm"
)


# 9 - Boxplot ---------------------------------------------------------------

graphs <- c(
  "5.4_boxsp_",
  "5.4_europe_boxsp_",
  "5.4_boxsp_q25_q75_"
)

dist_files <- c(
  "5.3_all_tracks_one_loc_per_day_bursts_graph_data.rds", 
  "5.3_all_tracks_one_loc_per_day_morning_bursts_graph_data.rds", 
  "5.3_all_tracks_max_daily_distance_graph_data.rds"
)


graphs |> 
  map(~{
    
    v <- F
    
    pstart <- .x
    
    ptitle_g <- ifelse(
      str_detect(pstart, "europe"),  
      "Daily distances across species [km] | boxplot, Europe", 
      "Daily distances across species [km] | boxplot"
      )
    
    ptitle_g <- ifelse(
        str_detect(pstart, "q25_q75"), 
        str_c(ptitle_g, ", subset by quantiles [25, 75%]"),
        ptitle_g
        )
        
                                                
    
    dist_files |> 
      map(~{
        
        fname <- .x
        
        pname <- fname |> 
          str_remove_all("5\\.3_all_tracks_|_graph_data.rds|_bursts")
        
        ptitle <- pname |> 
          str_replace_all("_", " ") 
        
        ptitle_m <- ptitle |> 
          paste("| across months and species")
        
        subt = ""
        
        df <- target_sp |> 
          map(~{
            
            sp <- .x
            sp_dir <- here("Data", "Studies", str_replace(sp, " ", "_"))
            
            here(sp_dir, "5_distances", fname) |> 
              read_rds() |> 
              mutate(species = sp)
            
          }) |> 
          bind_rows()
        
        
        if(str_detect(pstart, "europe")){
          
          df <- df |> 
            filter(in_europe == T)
        
          ptitle_m <- str_c(ptitle_m, ", Europe")
          
        }
        
        if(str_detect(pstart, "q25_q75")){
          
          v <- T
         
          df <- df |> 
            mutate(
              q25 = quantile(sl_km, p = 0.25), 
              q75 = quantile(sl_km, p = 0.75), 
              .by = c(species, file_id)
            ) |> 
            filter(sl_km >= q25, sl_km <= q75) 
          
          subt <- "species and distance data subset by quantiles [25, 75%]"
          
        }
        
        df |> 
          box_km(violin = v) + 
          facet_wrap(~month, scales = "free_x") +
          labs(title = ptitle_m, subtitle = subt) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5)
          )
        
        ggsave(
          here(ggraph_dir, str_c(pstart, "monthly_", pname, ".pdf")), 
          height = 18, units = "cm"
        )
        
        print(paste(fname, "for all species DONE!"))
        
        df |> 
          box_km(violin = v) + 
          labs(title = ptitle) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))
        
      }) |>  # dist files
      reduce(`+`) + 
      plot_layout(ncol = 1) + 
      plot_annotation(
        title = ptitle_g, 
        theme = theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
        )
      ) 
    
    ggsave(
      here(ggraph_dir, str_c(pstart, "differnet_distances_km.pdf")), 
      height = 18, units = "cm"
    )
    
})


