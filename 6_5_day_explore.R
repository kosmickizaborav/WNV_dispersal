#' ---
#' title: "day explore"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#'  **SECTION 1 -**



# 0 - Load packages -------------------------------------------------------

library(here)
library(tidyverse)
library(patchwork)
library(dispfit)
source(here("6_0_distance_functions.R"))
source(here("6_2_plot_functions.R"))

target_sp <- here("Data", "1_downloadable_studies_deployments_filtered.rds") |> 
  read_rds() |>
  distinct(species) |> 
  as_vector()

# all the files with daily distances that we have
dcp_day <- "1_all_tracks_dcp_day_steps_nauticalDawn_nauticalDusk.rds"
dcp_max_day <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk.rds" 
max_day <- "3_all_tracks_max_day_steps_nauticalDawn_nauticalDusk.rds"


pal <- c("#F38630FF", "#088BBEFF")
names(pal) <- c("resampled night track", "avaraged night position")


# FUNCTION: prepare data --------------------------------------------------

prepare_sl_df <- function(filename, dir = here(sp_dir, "6_distances")){
  
  strg <- str_remove_all(
    filename, "[143]_all_tracks|_steps_nauticalDawn_nauticalDusk.rds"
  )
  
  df <- file.path(dir, filename) |> 
    read_rds() |> 
    filter(!is.na(sl_)) |> 
    select(track_file, day_cycle, sl_) |> 
    mutate(
      sl_km = sl_/1000, 
      sl_log = log10(if_else(sl_ == 0, 0.01, sl_))
    ) |> 
    rename_with(~str_c(str_remove(., "_$"), strg), starts_with("sl_"))
  
  return(df)
}



# FUNCTION: plot_points ---------------------------------------------------

plot_point <- function(
    df, x, y1, y2, 
    xlab = NULL, ylab = NULL, title = NULL, 
    pal, legpos = "none", alpha = 0.3
  ){
 
  df |> 
    ggplot(aes(x = {{x}})) +
    geom_point(aes(y = {{y1}}, color = names(pal)[1]), alpha = alpha) +
    geom_point(aes(y = {{y2}}, color = names(pal)[2]), alpha = alpha) +
    labs(x = xlab, y = ylab) +
    scale_color_manual(values = pal) +
    theme_bw() +
    theme(legend.position = legpos)
   
}

plot_hist <- function(df, x, binwidth, xlab, fill = "gray66", alpha = NULL){
  
  df |> 
    ggplot() +
    geom_histogram(
      aes(x = {{x}}), binwidth = binwidth, fill = fill, alpha = alpha
    ) +
    labs(x = xlab) +
    theme_bw() 
  
}


# X - Data for Fede -------------------------------------------------------

# ran for fede, afterwards abandoned, just to keep track of what i sent 

if(F){
  
  fin <- max_day
  
  fede_dir <- here("Data", "For Fede")
  if(!dir.exists(fede_dir)){ dir.create(fede_dir) }
  
  target_sp |> 
    map(~{
      
      sp <- .x
      sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
      
      day_df <- here(sp_dir, "6_distances", fin) |> 
        read_rds() |> 
        filter(within_eubb == T) |> 
        select(
          track_file, individual_local_identifier, day_cycle,
          t1_, t2_, x1_, y1_, x2_, y2_, all_of(matches("sl_")), step_id, 
          manipulation_type, sensor_type
        ) 
      
      day_df |> 
        write_csv(
          here(
            fede_dir, 
            str_c(
              str_replace(sp, " ", "_"), 
              "_EU_max_day_steps_nauticalDawn_nauticalDusk.csv")
          )
        )
      
      
    }) # close map species
  
}


# 1 - Plot different daily distances --------------------------------------


target_sp |> 
  map(~{

    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    dir <- here(sp_dir, "6_distances")
    
    
    dcp_day_df <- prepare_sl_df(dcp_day, dir = dir)
    dcp_max_day_df <- prepare_sl_df(dcp_max_day, dir = dir)
    day_max_df <- prepare_sl_df(max_day, dir = dir)
    
    sl_df <- dcp_day_df |> 
      full_join(dcp_max_day_df, by = c("track_file", "day_cycle")) |> 
      full_join(day_max_df, by = c("track_file", "day_cycle")) 
    
    # Create a base plot
    plog <- plot_point(
      sl_df, 
      x = sl_log_dcp_day, 
      y1 = sl_log_max_day, 
      y2 = sl_log_dcp_max_day, 
      xlab = "daily distances [m, log10] (median night and day positions)", 
      ylab = "max daily distance [m, log10]", 
      pal = pal, 
      legpos = "bottom"
      )
      
    pkm <- plot_point(
      sl_df, 
      x = sl_km_dcp_day, 
      y1 = sl_km_max_day, 
      y2 = sl_km_dcp_max_day, 
      xlab = "daily distances [km] (median night and day positions)", 
      ylab = "max daily distance [km]", 
      pal = pal, 
      legpos = "none"
    )
    
    pout <- pkm+plog +
      plot_layout(ncol = 1) +
      plot_annotation(title = paste(sp, "- comparison of daily distances"))
    
    ggsave(
      here("Data", "Graphs", str_c("6_5.1_", sp, "day_distances_comparison.png")), 
      pout, 
      width = 25, height = 20, units = "cm"
    )
    
    dcpkm <- plot_hist(
      dcp_day_df, 
      sl_km_dcp_day, 
      binwidth = 1, 
      xlab = "daily distances [km] (median night and day positions)", 
      fill = "gray44", 
      alpha = 0.6
      )
    
    dcplg <- plot_hist(
      dcp_day_df, 
      sl_log_dcp_day, 
      binwidth = 0.1, 
      xlab = "daily distances [m, log10] (median night and day positions)", 
      fill = "gray44", 
      alpha = 0.6
    )
    
    dcpmkm <- plot_hist(
      dcp_max_day_df, 
      sl_km_dcp_max_day, 
      binwidth = 1, 
      xlab = "max daily distance [km] (median night position)", 
      fill = "#088BBEFF", 
      alpha = 0.6
    )
    
    dcpmlg <- plot_hist(
      dcp_max_day_df, 
      sl_log_dcp_max_day, 
      binwidth = 0.1, 
      xlab = "max daily distance [m, log10] (median night position)",
      fill = "#088BBEFF", 
      alpha = 0.6
    )
    
    daykm <- plot_hist(
      day_max_df, 
      sl_km_max_day, 
      binwidth = 1, 
      xlab = "max daily distance [km] (resampled night track)", 
      fill = "#F38630FF", 
      alpha = 0.6
    )
    
    daylg <- plot_hist(
      day_max_df, 
      sl_log_max_day, 
      binwidth = 0.1, 
      xlab = "max daily distance [m, log10] (resampled night track)", 
      fill = "#F38630FF", 
      alpha = 0.6
    )
    
    pout <- (dcpkm + dcplg) / (daykm + daylg) / (dcpmkm + dcpmlg) +
      plot_layout(ncol = 1) +
      plot_annotation(title = paste(sp, "- daily distances")) 
    
    ggsave(
      here(
        "Data", 
        "Graphs", 
        str_c("6_5.2_", sp, "day_distances_comparison_hist.png")
      ), 
      pout, 
      width = 30, height = 25, units = "cm"
    )
      
    cat(paste(str_replace(sp, "_", " "), "plots 1 and 2 done!\n"))
    
  })


# 2 - Distfit trial -------------------------------------------------------

dist_funs <- c(
  "rayleigh", "exponential", 
  "general normal", "2Dt", 
  "geometric", "lognormal", 
  "wald", "weibull", "gamma"
)

quantiles <- c(0.95, 0.99, 1)

# Define a safe version of the dispersal.kernel function
safe_dispersal_kernel <- safely(dispersal.kernel)

results_full <- target_sp |> 
  map(~{
    
    sp <- .x
    sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
    
    cat(paste(sp, "started!\n"))
    
    quantiles |>
      map(~{

        q <- .x
        
        c(dcp_day, dcp_max_day) |>
          map(~{

            fin <- .x
            
            dcp_day_df <- here(sp_dir, "6_distances", fin) |> 
              read_rds() |> 
              select(track_file, day_cycle, sl_) |> 
              filter(!is.na(sl_), sl_ > 0) |>
              filter(sl_ <= quantile(sl_, q))
            
            # Apply the safe function to each distribution
            kernels <- dist_funs |> 
              map(~safe_dispersal_kernel(dcp_day_df$sl_, distribution = .x)) |> 
              set_names(dist_funs) |> 
              map("result") |> 
              map(~{
                
                if(is.null(.x)){
                  
                  tibble(error_occured = T)
                  
                } else {
                  
                  .x[["values"]] |> 
                    as_tibble() |> 
                    mutate(error_occured = F)
                }
                
              }) |> 
              list_rbind(names_to = "fitted_function") |> 
              mutate(
                species = sp, 
                quant = q, 
                file = fin
              ) 

          }) |>
          list_rbind()

      }) |>
      list_rbind()

    
    # Warning messages:
    #   1: In ks.test.default(data, simul.rayleigh) :
    #   p-value will be approximate in the presence of ties
    # 2: In ks.test.default(data, simul.exponential) :
    #   p-value will be approximate in the presence of ties
    # 3: In ks.test.default(data, simul.generalnormal) :
    #   p-value will be approximate in the presence of ties
    # 4: In ks.test.default(data, simul.2dt) :
    #   p-value will be approximate in the presence of ties
    # 5: In ks.test.default(data, simul.geometric) :
    #   p-value will be approximate in the presence of ties
    # 6: In ks.test.default(data, simul.lognorm) :
    #   p-value will be approximate in the presence of ties
    # 7: In ks.test.default(data, simul.wald) :
    #   p-value will be approximate in the presence of ties
    # 8: In ks.test.default(data, simul.weibull) :
    #   p-value will be approximate in the presence of ties
    # 9: In ks.test.default(data, simul.gamma) :
    #   p-value will be approximate in the presence of ties
    # 10: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par, 
    #        NaNs produced
    # 11: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par, 
    #                                          NaNs produced
    # 
    # here were 50 or more warnings (use warnings() to see the first 50)
    # Warning messages:
    #  1: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower, 
    #                    NA/Inf replaced by maximum positive value
    #  2: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower,
    #                              NA/Inf replaced by maximum positive value
    #  3: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower,
    #                              NA/Inf replaced by maximum positive value
    
  }) |> 
  list_rbind()


results_out <- results_full |> 
  janitor::clean_names() |> 
  rename(
    AIC = aic, 
    delta_aic = AIC, 
    AICc = ai_cc, 
    delta_AICc = delta_ai_cc, 
    BIC = bic, 
    delta_BIC = delta_bic,
  )
  select(
    all_of(c("species", "quant", "file", "fitted_function", "error_occured")), 
    everything()
  ) |> 
  # looked in the source code of dist.kernel to see how this is obtained, 
  # ao the logic comes from there
  mutate(
    delta_AIC = AIC - min(AIC, na.rm = T),
    delta_BIC = BIC - min(BIC, na.rm = T), 
    delta_AICc = AICc - min(AICc, na.rm = T),
    wi <- exp(-.5*delta_AICc) / sum(exp(-.5*delta_AICc)), 
    .by = c("species", "quant", "file")
  )
