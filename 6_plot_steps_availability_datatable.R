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
#'  **SECTION 2 - Plot different day definitions**
#'  plot the number of deployments and night steps obtained using different 
#'  definitions of day limits
  

# 0 - Load packages -------------------------------------------------------

library(data.table)
source("6_plot_datatable_FUNCTIONS.R")

# get the species of interest
data_dir <- here::here("Data")
graphs_dir <- file.path(data_dir, "Graphs")

# INPUT

# 1 - Get file list -------------------------------------------------------

dist_dirs <- grep(
  "5_distances$", list.dirs(file.path(data_dir, "Studies")), value = T)

files <- unlist(lapply(dist_dirs, function(dir){
  grep("continent.rds", list.files(dir, full.names = T), value = T)
  }))

night_files <- grep("2_all_tracks_dcp_night_steps", files, value = T)
day_files <- grep("4_all_tracks_dcp_max_day_steps", files, value = T)


n_files <- length(night_files)


# create output directory

steps_plot_dir <- file.path(data_dir, "Graphs", "6_steps_availability")

if(!dir.exists(steps_plot_dir)){ 
  dir.create(steps_plot_dir)
  dir.create(file.path(steps_plot_dir, "World"))
  dir.create(file.path(steps_plot_dir, "Europe"))
}


target_sp <- unique(gsub(
  ".*/Studies/([^/]+)/5_distances/.*", "\\1", night_files))

# 2 - Plot maps for all species -------------------------------------------

cap <- "color of lines in timeline and points in map indicate the year of tracking"

lapply(seq_along(night_files), function(i){
  
  fin <- night_files[i]
  
  sp <- gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin)
  sp <- gsub("_", " ", sp)
  dl <- gsub("_", "-", gsub(".*night_steps_([^/]+)_continent.*", "\\1", fin))
  
  fout <- paste0(
    "1_", sp, gsub(".*/2_all_tracks([^/]+)\\.rds", "\\1", fin), ".png")
  fout_eu <- gsub(".png", "_europe.png", fout)
  
  if(file.exists(file.path(steps_plot_dir, "Europe", fout_eu)) &
     file.exists(file.path(steps_plot_dir, "World", fout))){
    
    cat(sprintf("\n %s, %s - Plot exists!", sp, dl))
    
    return(NULL)
    
  }
  
  night_steps <- fread(fin)
  
  n_steps <- nrow(night_steps)
  n_eu <- sum(night_steps$continent == "Europe")
  sub <- paste0("day limits: [", dl, "] | ", "in Europe: ", n_eu, "/", n_steps)
  
  tit_full <- paste(gsub("_", " ", sp), "- night steps")
  
  lapply(list(fout, fout_eu), function(f) {
    
    if(grepl("europe", f)){ 
      night_steps <- night_steps[continent == "Europe"]
      plot_dir <- file.path(steps_plot_dir, "Europe")
    } else{
      plot_dir <- file.path(steps_plot_dir, "World")
    } 
    
    if(nrow(night_steps) > 0){
      
      # plots
      wmap <- plot_on_world_map(night_steps)
      tl <- plot_steps_timeline(night_steps)
      
      pout <- wmap/tl + 
        plot_annotation(
          title = tit_full, 
          subtitle = sub, 
          caption = cap,
          theme = theme(
            plot.title = element_text(face = "bold", hjust = 0.5), 
            plot.subtitle = element_text(face = "bold", hjust = 0.5)
          )
        ) +
        plot_layout(heights = c(1.2, 1))
      
      # save the plots
      ggsave(
        file.path(plot_dir, f), 
        pout, 
        width = 18, height = 18, units = "cm"
      )
      
    }
    
  })
  
  cat(sprintf("\n %s - %s - Plot done %d | %d!", sp, dl, i, n_files))
  
})
  


# 3 - Plot different day definitions ---------------------------------------

month_limits <- get_month_limits()

pal <- c(
  "dawn-dusk" = "#FDB863",
  "nauticalDawn-nauticalDusk" = "#5BBCD6",
  "nightEnd-night" = "#362759"
)

lapply(target_sp, function(sp){
  
  fout <- paste0("2_", sp, "_night_steps_availability.png")
  fout_eu <- gsub(".png", "_europe.png", fout)
  
  if(file.exists(file.path(steps_plot_dir, "Europe", fout_eu)) &
     file.exists(file.path(steps_plot_dir, "World", fout))){
    
    cat(sprintf("\n %s - Plot exists!", sp))
    return(NULL)
  }
  
  sp_night_files <- grep(sp, night_files, value = T)
  
  full_df <- rbindlist(lapply(sp_night_files, function(f){
    
    dl <- gsub("_", "-", gsub(".*night_steps_([^/]+)_continent.*", "\\1", f))
    
    steps <- fread(f)[, .(day_cycle = day_cycle_1, file, continent)][
      , `:=` (
        yd = as.POSIXlt(as.Date(day_cycle))$yday + 1, 
        day_limit = dl)]
    
    return(steps)
    
  }))
  
  title_out <- paste(gsub("_", " ", sp), "- night steps availability")
  
  lapply(list(fout, fout_eu), function(f) {
    
    if(grepl("europe", f)){ 
      full_df <- full_df[continent == "Europe"]
      plot_dir <- file.path(steps_plot_dir, "Europe")
      title_out <- paste(title_out, "- Europe")
    } else{
      plot_dir <- file.path(steps_plot_dir, "World")
      title_out <- paste(title_out, "- World")
    } 
    
    # total counts
    counts_df <- full_df[
      , .(n_deploys = uniqueN(file), n_steps = .N), by = "day_limit"][
        , day_limit := as.factor(day_limit)]
    
    if(nrow(counts_df) > 1){
      
      pdc <- plot_bars(
        df = counts_df, x = n_deploys, y = day_limit, fill = day_limit, 
        pal = pal, 
        ylab = "day definition", 
        title = "Number of deployments with night steps") +
        theme(axis.title.y = element_blank())
      
      psc <- plot_bars(
        df = counts_df, x = n_steps, y = day_limit, fill = day_limit, 
        pal = pal, title = "Number of night steps obtained"
      ) +
        theme(axis.text.y = element_blank())
      
      # getting counts of steps per day
      step_times <- rbindlist(list(
        CJ(yd = 1:366, n_steps = 0, day_limit = unique(full_df$day_limit)), 
        full_df[, .(n_steps = .N), by = c("yd", "day_limit")]), use.names = T)[
          , .(n_steps = sum(n_steps)), by = c("yd", "day_limit")][
            , day_limit := as.factor(day_limit)]
      
      pst <- plot_steps_count(
        steps = step_times, alpha = 0.6,
        count = n_steps, color = day_limit, pal = pal, ylab = "", 
        linetype = "dotdash", 
        title = "Number of night steps per day of the year", 
        legend_position = "none"
      ) 
      
      # output plot 
      pout <- (pdc + psc) / pst +
        plot_annotation(
          title = title_out,
          theme = theme(
            plot.title = element_text(face = "bold", hjust = 0.5)
          )
        ) +
        plot_layout(heights = c(1, 1))
      
      # save the plots
      ggsave(
        file.path(plot_dir, f), 
        pout, 
        width = 25, height = 20, units = "cm"
      )
      
    }
    
  })
  
  cat(sprintf("\n %s Plot night step availability done!", sp))
  
})


# 4 - Plot data availability across species ------------------------------------

day_limits <- c("dawn_dusk", "nauticalDawn_nauticalDusk", "nightEnd_night")

deploy_info <- fread(file.path(data_dir, "2_deployments_cleaned.csv"))[
  , .(individual_local_identifier, file)][
    , file := gsub("2_cleaned", "4_resampled", file)]

step_files <- c(night_files, day_files)
n_files <- length(step_files)

night_steps_all <- rbindlist(lapply(seq_along(step_files), function(i){
  
  fin <- step_files[i]
  
  sp <- gsub("_", " ", gsub(".*/Studies/([^/]+)/5_distances/.*", "\\1", fin))
  dl <- gsub("_", "-", gsub(".*_steps_([^/]+)_continent.*", "\\1", fin))
  st <- gsub("_", " ", gsub(".*all_tracks_dcp_([^/]+)_steps.*", "\\1", fin))

    # total counts
  steps <- fread(fin)[, .(day_cycle = day_cycle_1, continent, file)]
  
  step_count <- deploy_info[steps, on = "file"][
    , `:=` (
      yd = as.POSIXlt(as.Date(day_cycle))$yday + 1, 
      day_limit = dl)][
      , .(
        #n_deploys = uniqueN(file), 
        #n_steps = .N, 
        n_ind = uniqueN(individual_local_identifier))
      , by = yd][
      , ':=' (
        day_limit = dl, 
        species = sp, 
        step_type = st
      )]
  
  cat(sprintf("\n %s - %d|%d", sp, i, n_files))
  
  return(step_count)
  
}))

# setting colors and breaks
cat_breaks <- c(0, 2, 5, 10, 25, 50, Inf)
cat_names <- c('1', "2-4", '5-9', '10-24', '25-49', '50+')
palb <- paletteer::paletteer_d("LaCroixColoR::PeachPear") 
names(palb) <- cat_names

night_steps_all[
  , ':=' (
    species = factor(species, levels = rev(sort(unique(species)))), 
    n_ind_cat = cut(n_ind, breaks = cat_breaks, labels = cat_names, 
                    include.lowest = TRUE)
  )]

y_breaks <- levels(night_steps_all$species)

# PLOT nauticalDawn-nauticalDusk counts###

lapply(steps_dts, function(dt){
  
  dl <- unique(dt$day_limit)
  st <- unique(dt$step_type)
  
  pname <- sprintf(
    "3_%s_%s_steps_availability.png", gsub("-", "_", dl), gsub(" ", "_", st))
  tirla.pn
  
  
  plot_tile_timeline(
    dt, 
    y = species,
    fill = n_ind_cat, 
    pal = palb,
    flab = "count",
    pal_type = "manual"
    ) +
    scale_y_discrete(breaks = rev(y_breaks)) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
      panel.spacing = unit(3, "mm"), 
      plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5),
    ) +
    labs(
      title = sprintf("Year round %s step availability", st), 
      subtitle = sprintf("day limits: %s", dl)
    )
  
  ggsave(
    file.path(steps_plot_dir, pname), width = 18, height = 18, units = "cm")
})



# cols_to_cat <- c("n_deploys", "n_steps", "n_ind")
# 
# night_steps_all[
#   , paste0(cols_to_cat, "_cat") := lapply(
#     .SD, cut, breaks=breaks, labels=labels, include.lowest=TRUE),
#   .SDcols= cols_to_cat]




# count_dt <- melt(
#   night_steps_all[day_limit == "nauticalDawn-nauticalDusk"],
#   measure.vars = c("n_steps", "n_ind"),
#   variable.name = "count_type",
#   value.name = "count"
# )[
#   , count_cat := cut(count, breaks = cat_breaks, labels = cat_names, 
#                      include.lowest = TRUE)]
# 
# track_count <- unique(night_steps_all[, .(species, n_deploys, day_limit, yd)])


  
  
