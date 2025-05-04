
# INFO --------------------------------------------------------------------

#' The script should be able to be rerun on any computer, except the section 1.
#' The other sections can be rerun, as long the inner structure of fede_dir,
#' is preserved, i.e:
#' - folder Sl_files with the raw step lengths data for all species 
#' - Disp_fits_EU with fits for all data from Sl_files and quantiles
#' 
#'  **SECTION 1 - Data preparation**
#'  putting the step lenght files for all species in one folder that I sent you
#'  - load the data from the folders for each species 
#'  - selection only columns of interest
#'  - add the column that indicates whether the step started in Europe
#'  - save the data in the folder "Sl_files", adding the name of the species 
#'    in front
#'  **SECTION 2 - Fit dispersal kernel**
#'  fitting dispersal kernels and saving them in "Disp_fits_EU"
#'  data for every quantile and every step lenght input is saved separately, 
#'  and is structured as a list, containing info on all the functions that 
#'  we tried to fit
#'  - define functions to fit in dist_funs and quantiles to trim step lenght 
#'    values as quantiles
#'  - for all the files in folder Sl_files generate try to fit all the 
#'    kernel functions for all quantiles and save them to a folder 
#'    "Disp_fits_EU"
#'  OUTPUT FORMAT: list of 9 elements, each representing the function used 
#'  to fit the data
#'  **SECTION 3 - All fits overview**
#'  generate a dataframe with information on all fits and their values, 
#'  majority of this table consist of values provided in the output of 
#'  dispersal.kernel, but there are also columns for orientation:
#'  - species
#'  - quantile - quantile applied to trim step lenghts before fitting 
#'  - fit_file - name of the file that contains the fits
#'  - fitted_function - name of the function that was fitted
#'  - error_occurred - if T, there was some error during fitting and we do not 
#'    have results, the error message can be accessed if we load the fit file, 
#'    and under the function that wasn't fitted we read "error" instead of the "result"
#'   **SECTION 4 - Check specific fit**
#'  explore specific fits
#'  - define the data for which we want to explore the fits, and the 
#'    quantile value of interest
#'  - find the file name of the fit for the specified data
#'  - load the fit results and/or fit errors, and check them out
#'  

# 0 - Load packages -------------------------------------------------------
# library(here) # not necessary, only used in section 1
library(tidyverse)
library(dispfit)
#library(janitor)

# define your directory
fede_dir <- "/home/nina/R_projects/WNV_dispersal/Data/For_fede"

#fede_dir <- "/home/fbartu/Research/Nina_Bogdanovic/Dispersal_Kernels/For_fede"


# # 1 - Data preparation ----------------------------------------------------
# 
# # on my computer all files are structured in the folders per species, 
# # I ran this to put them all in one folder so that they are more easily accessed
# # this part cannot be rerun because it depends on data structure
# 
# if(F){
# 
#   if(!dir.exists(fede_dir)){ 
#     dir.create(fede_dir)
#     dir.create(here(fede_dir, "Sl_files"))
#     dir.create(here(fede_dir, "Disp_fits_EU"))
#   }
#   
#   # # coordinates used to check if in Europe boundary box
#   eu_coords <- list(
#   ymin = 34.0,
#   ymax = 81.0,
#   xmin = -29.0,
#   xmax = 69.0
#   )
#   
#   # input files
#   # steps from median position of the night to median position during the day
#   dcp_day <- "1_all_tracks_dcp_day_steps_nauticalDawn_nauticalDusk.rds"
#   # steps from median position of the night to maximum position during the day
#   dcp_max_day <- "4_all_tracks_dcp_max_day_steps_nauticalDawn_nauticalDusk.rds" 
#   
#   target_sp |> 
#     map(~{
#       
#       sp <- .x
#       sp_dir <- here("Data",  "Studies", str_replace(sp, " ", "_"))
#       
#       c(dcp_day, dcp_max_day) |> 
#         map(~{
#           
#           fin <- .x
#           fout <- str_c(str_replace(sp, " ", "_"), str_remove(fin, "[1-9]"))
#           
#           day_df <- here(sp_dir, "6_distances", fin) |> 
#             read_rds() |> 
#             select(
#               track_file, day_cycle, any_of(matches("[txy][12]_|sl_")), step_id
#             ) |> 
#             mutate(
#               within_eubb = (
#                 x1_ >= eu_coords$xmin & x1_ <= eu_coords$xmax & 
#                   y1_ >= eu_coords$ymin & y1_ <= eu_coords$ymax
#               )
#             ) |> 
#             write_rds(here(fede_dir, "Sl_files", fout))
#           
#           cat("saved!\n")
#           
#         })
#       
#       cat(paste(sp, "DONE!\n"))
#       
#     }) # close map species
#   
# }
# 


# 2 - Fit dispersal kernels -----------------------------------------------

# code ran to fit the functions and save raw results
# I put it in if(F) because I sent you the results, but in case you want to 
# rerun it, you can see the process and rerun it, should work

if(F){
  
  # functions available 
  dist_funs <- c(
    "rayleigh", 
    "exponential", 
    "general normal", 
    "2Dt", 
    "geometric", 
    "lognormal", 
    "wald", 
    "weibull",
    "gamma"
  )
  
  # quantiles for cuttiong off of step lengths vector
  quantiles <- c(0.95, 0.99, 1)
  
  # a safe version of the function
  # if the error occurs the process is not stooped,
  # but skipped and the error is saved
  safe_dispersal_kernel <- safely(dispersal.kernel)
  
  # get all the files for step lengths
  files <- file.path(fede_dir, "Sl_files") |> 
    list.files()
  
  # apply the same process for every quantile
  quantiles |> 
    map(~{
      
      # quantile to be used
      q <- .x 
      
      cat(paste("Calculations for quantile", .x, "started!\n"))
      
      # for all files
      files |> 
        map(~{
          
          fin <- .x
          fout <- str_replace(fin, "all_tracks", "all_kernels_EU") |> 
            str_replace("steps_nauticalDawn_nauticalDusk", str_c("quant_", q))
          
          # load the file fin that contains step lenghts
          dcp_day_df <- file.path(fede_dir, "Sl_files", fin) |> 
            read_rds() |> 
            # keep only steps that start in europe, 
            # data that we have step lenght and it's bigger than 0
            filter(within_eubb == T, !is.na(sl_), sl_ > 0) |>
            # remove extreme values using the quantile chosen
            filter(sl_ <= quantile(sl_, q))
          
          # run safely dispersal kernel function for each distribution separately
          # the output is a list with dist_funs as names
          kernels <- dist_funs |> 
            map(~safe_dispersal_kernel(dcp_day_df$sl_, distribution = .x)) |> 
            set_names(dist_funs) 
          
          # save the raw output 
#          kernels |> 
 #           write_rds(file.path(fede_dir, "Disp_fits_EU", fout))
          
   #       cat(paste("Kernels for file", fin, "saved!\n"))
          
        })
      
    })
  
  
}


# A LOT OF WARNINGS 
# There were 50 or more warnings (use warnings() to see the first 50)
# Warning messages:
# 1: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
#   twodt: Upper CI for 'a' is not accurate, I've given up after 10000 trials.
# 2: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
#   twodt: Upper CI for 'b' is not accurate, I've given up after 10000 trials.
# 3: In confint.dispfit(dist.opt, log.dist.geometric, data = data,  ... :
#   geometric: Parameter 'a' likely diverged, skipping CI calculation
# 4: In confint.dispfit(dist.opt, log.dist.geometric, data = data,  ... :
#   geometric: Parameter 'b' likely diverged, skipping CI calculation
# 5: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
#   twodt: Upper CI for 'a' is not accurate, I've given up after 10000 trials.
# 6: In confint.dispfit(dist.opt, log.dist.2dt, data = data,  ... :
#   twodt: Upper CI for 'b' is not accurate, I've given up after 10000 trials.
# 7: In confint.dispfit(dist.opt, log.dist.geometric, data = data,  ... :
#   geometric: Parameter 'a' likely diverged, skipping CI calculation
# 8: In confint.dispfit(dist.opt, log.dist.geometric, data = data,  ... :
#   geometric: Parameter 'b' likely diverged, skipping CI calculation
# 9: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 10: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 11: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 12: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 13: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 14: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 15: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 16: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 17: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 18: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 19: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 20: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 21: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 22: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 23: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 24: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 25: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 26: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 27: In optimize(function(par) fn(par, ...)/con$fnscale, lower = lower,  ... :
#   NA/Inf replaced by maximum positive value
# 28: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 29: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 30: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 31: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 32: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 33: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 34: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 35: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 36: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 37: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 38: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 39: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties
# 40: In ks.test.default(data, simul.exponential) :
#   p-value will be approximate in the presence of ties
# 41: In ks.test.default(data, simul.generalnormal) :
#   p-value will be approximate in the presence of ties
# 42: In ks.test.default(data, simul.2dt) :
#   p-value will be approximate in the presence of ties
# 43: In ks.test.default(data, simul.geometric) :
#   p-value will be approximate in the presence of ties
# 44: In ks.test.default(data, simul.lognorm) :
#   p-value will be approximate in the presence of ties
# 45: In ks.test.default(data, simul.wald) :
#   p-value will be approximate in the presence of ties
# 46: In ks.test.default(data, simul.weibull) :
#   p-value will be approximate in the presence of ties
# 47: In ks.test.default(data, simul.gamma) :
#   p-value will be approximate in the presence of ties
# 48: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 49: In sqrt(diag(solve(numDeriv::hessian(logdistfun, x = init.pars$par,  ... :
#   NaNs produced
# 50: In ks.test.default(data, simul.rayleigh) :
#   p-value will be approximate in the presence of ties


# 3 - All fits overview---------------------------------------------------------

# extract values from each fit
if(F){
  fits_overview <- file.path(fede_dir, "Disp_fits_EU") |> 
    list.files() |> 
    map(~{
      
      fin <- .x
      
      fit_data <- file.path(fede_dir, "Disp_fits_EU", fin) |> 
        read_rds() |> 
        map("result") |> 
        map(~{
          
          result <- .x
          
          # if there is no results, return a column that says error_occured
          if(is.null(result)){
            
            tibble(
              error_occurred = T
            )
            
          } else {
            
            # if there is data extract values
            result[["values"]] |> 
              as_tibble() |> 
              mutate(error_occurred = F)
          }
          
        }) |> 
        list_rbind(names_to = "fitted_function") |> 
        mutate(fit_file = fin) 
      
    }) |>
    list_rbind() |> 
    # just renaming things
    janitor::clean_names() |> 
    rename(
      AIC = aic, 
      delta_AIC = delta_aic, 
      AICc = ai_cc, 
      delta_AICc = delta_ai_cc, 
      BIC = bic, 
      delta_BIC = delta_bic,
    ) |> 
    # adding species and quant column
    mutate(
      species = paste(
        str_split_i(fit_file, "_", 1), str_split_i(fit_file, "_", 2)
      ), 
      quant = as.numeric(str_remove(str_split_i(fit_file, "quant_", 2), ".rds")) 
    ) |> 
    # looked in the source code of dist.kernel to see how this is obtained, 
    # ao the logic comes from there
    mutate(
      delta_AIC = AIC - min(AIC, na.rm = T),
      delta_BIC = BIC - min(BIC, na.rm = T), 
      delta_AICc = AICc - min(AICc, na.rm = T),
      wi = exp(-.5*delta_AICc)/sum(exp(-.5*delta_AICc), na.rm = T), 
      .by = c(species, quant, fit_file)
    ) |> 
    # link the fits to the original step length files, in case we need them later
    mutate(
      distance_type = str_split_i(
        str_split_i(fit_file, "all_kernels_EU_", 2), 
        "_quant", 1
      )
    ) |> 
    # reordering columns for easier reading of the table
    select(
      all_of(
        c(
          "species", 
          "quant", 
          "distance_type",
          "fit_file", 
          "fitted_function", 
          "error_occurred"
        )
      ), 
      everything()
    ) |> 
    arrange(quant, species, distance_type, fit_file) |> 
    group_split(fit_file, fitted_function) |> 
    map(~{
      
      if(.x$error_occurred == T){
        
        data <- file.path(fede_dir, "Disp_fits_EU", .x$fit_file) |> 
          read_rds() 
        
        .x |> 
          mutate(
            error_message = data[[.x$fitted_function]][["error"]][["message"]]
          )
        
      } else{
        .x
      }
    }) |> 
    list_rbind() |> 
    select(
      quant, species, distance_type, fit_file, fitted_function, 
      error_occurred, error_message, everything()
    )
  
  
  fits_overview |>
    write_csv(file.path(fede_dir, "disp_fits_EU_overview.csv"))
}

# 4 - One file for all ----------------------------------------------------

# one file for all data used to fit kernels

if(F){
 
  quantiles <- c(0.95, 0.99, 1)
  
  # get all the files for step lengths
  files <- file.path(fede_dir, "Sl_files") |> 
    list.files()
  
  # apply the same process for every quantile
  data <- quantiles |> 
    map(~{
      
      # quantile to be used
      q <- .x 
      
      # for all files
      files |> 
        map(~{
          
          fin <- .x
          
          # load the file fin that contains step lenghts
          dcp_day_df <- file.path(fede_dir, "Sl_files", fin) |> 
            read_rds() |> 
            mutate(
              quant = q, 
              fin = fin
            ) |> 
            # keep only steps that start in europe, 
            # data that we have step lenght and it's bigger than 0
            filter(within_eubb == T, !is.na(sl_), sl_ > 0) |>
            # remove extreme values using the quantile chosen
            filter(sl_ <= quantile(sl_, q)) |> 
            select(quant, sl_, fin) |> 
            arrange(sl_)
          
        }) |> 
        list_rbind()
      
    }) |> 
    list_rbind() |> 
    mutate(
      species = str_replace(str_split_i(fin, "_all_tracks_", 1), "_", " "), 
      distance_type = str_remove(
        str_split_i(fin, "_all_tracks_", 2), 
        "_steps_nauticalDawn_nauticalDusk.rds"
      ), 
      fit_file = str_c(
        str_split_i(fin, "all_tracks", 1), 
        "all_kernels_EU_", 
        distance_type, 
        "_quant_", 
        quant, 
        ".rds"
        )
    ) |> 
    select(-fin) |> 
    select(quant, species, distance_type, sl_, fit_file) |>
    write_csv(file.path(fede_dir, "all_species_all_step_lengths.csv"))
   
}


# 4 - Check specific fit --------------------------------------------------

# STEP 1 - load all jumps (data used to fit the kernels), with following columns:
#  1 - "quant" - quantile used to trim the step lenghts before fitting
#  2 - "species" 
#  3 - "distance_type" - type of distance used to calculate step lenghts
#    - dcp_day - median night to median day
#    - dcp_max_day - median night to maximum day
#  4 - "sl_" - step lengths a.k.a. jumps
#  5 - "fit_file" - name of the file that contains the fits 

# all jumps for all species 
jumps <- file.path(fede_dir, "all_species_all_step_lengths.csv") |> 
  read_csv(show_col_types = F)

# STEP 2 - load the overview of all fits if you want to check them out
fits_overview <- file.path(fede_dir, "disp_fits_EU_overview.csv") |> 
  read_csv(show_col_types = F)

# STEP 3 - filter so that we can check out separate fits
# decide which species, quantile and distance type we want to check

# available species: 
target_sp <- c(
  "Anas platyrhynchos", 
  "Columba livia", 
  "Turdus merula", 
  "Circus aeruginosus", 
  "Sturnus vulgaris", 
  "Accipiter gentilis" 
)

# available quantiles
quantiles <- c(0.95, 0.99, 1)

# available distance types
dist_types <- c("dcp_day", "dcp_max_day")

# select one for each
sp <- "Anas platyrhynchos"
q <- 0.95
dst <- "dcp_day"

# jumps used to generate fits for the selected parameters
selected_jumps <- jumps |> 
  filter(species == sp, quant == q, distance_type == dst)

# overview of the fits for the selected parameters
selected_fits_overview <- fits_overview |> 
  filter(species == sp, quant == q, distance_type == dst)

# get name of the file fit
fit_file <- unique(selected_jumps$fit_file)

# load the fit results - this is the output of the dispersal.kernel function
# in it's raw format for all the functions applied (if they have result)
fit_results <- file.path(fede_dir, "Disp_fits_EU", fit_file) |> 
  read_rds() |> 
  map("result") 

# check available fits 
names(fit_results)

# if we want to check error messages for some fits
fit_errors <- file.path(fede_dir, "Disp_fits_EU", fit_file) |> 
  read_rds() |> 
  map("error") |> 
  map("message")

fit_errors


# check a specific fit
#fit_geom <- fit_results[["geometric"]]

#VISUALIZATIONS

plot(fit_results$"exponential")
plot(fit_results$"rayleigh")
plot(fit_results$"general normal")
plot(fit_results$"2Dt")
plot(fit_results$"geometric")
plot(fit_results$"lognormal")
plot(fit_results$"wald")
plot(fit_results$"weibull")
plot(fit_results$"gamma")










