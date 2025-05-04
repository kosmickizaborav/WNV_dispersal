#' ---
#' title: "Get daily distances"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' using different definitions of day and night periods, this scritp calculates
#' the distances during each period.
#'  **SECTION 1 - Distances per day period**
#' splits the track into day-cycle-periods (e.g. day1_night) and calculates
#' distances between all points avaialable in each period. after that uses that 
#' dataframe to calcualte step lenght from the median points obtained
#' **SECTION 2 - Night and day steps**
#' calculates night steps by using the last point available during the night, 
#' and then resamples to paeriod of 24 hours. the day steps are subsequently
#' calculated by taking the origin point of the night steps and then calculating
#' the distance from that point to all available points during that time. 


# 0 - Load packages -------------------------------------------------------

library(data.table)
#load the functions for distance calculations
source("5_distance_datatable_FUNCTIONS.R")
source("0_helper_functions.R")

data_dir <- here::here("Data")
# INPUT
file_resampled_report <- "4_resampled_report.csv"

resampled_tracks <- fread(file.path(data_dir, file_resampled_report))

create_dir(unique(resampled_tracks$birdlife_name), new_dir = "5_distances")


