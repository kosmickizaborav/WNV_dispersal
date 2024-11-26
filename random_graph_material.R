
# summary graph directory
ggraph_dir <- here("Data",  "Graphs")

if(!dir.exists(ggraph_dir)) { ggraph_dir |> dir.create() }

# # 5 - Plot distances for all species --------------------------------------
# 
# 
# step_df <- target_sp |> 
#   map(~{
#     
#     sp_dir <- here("Data", "Studies", .x)
#     
#     here(sp_dir, "5_distances", "5.1_all_tracks_one_loc_per_day_bursts.rds") |> 
#       read_rds() |> 
#       mutate(species = str_replace(.x, "_", " ")) |> 
#       rename(step = sl_) 
#   
#   }) |> 
#   bind_rows() 
# 
# step_df |> 
#   ggplot() +
#   geom_histogram(
#     aes(x = step), 
#     binwidth = 1000, 
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~ species, ncol = 1, scales = "free_y") +
#   labs(
#     x = bquote("step length [m] | binwidth =" ~ 10^3), 
#     title = "Tracks subset - one location per day"
#   ) +
#   theme_bw() +
#   scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
# 
# 
# ggsave(
#   here(ggraph_dir, "5.1_one_loc_per_day_steps.pdf"), 
#   height = 15,
#   units = "cm"
# )
# 
# step_df |> 
#   mutate(step_log = log10(ifelse(step == 0, step + 1e-10, step))) |> 
#   ggplot() +
#   geom_histogram(
#     aes(x = step_log), 
#     bins = 100, 
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~ species, ncol = 1, scales = "free_y") +
#   labs(
#     x = "step length [m] - log scale | bins = 100",
#     title = "Tracks subset - one location per day"
#   ) +
#   theme_bw()
# 
# ggsave(
#   here(ggraph_dir, "5.1_one_loc_per_day_steps_log10.pdf"), 
#   height = 15,
#   units = "cm"
# )
# 
# 
# 
# step_df |> 
#   ggplot() +
#   geom_boxplot(
#     aes(x = step, y = species, group = species),  
#     fill = "gray66", color = "black", 
#     notch = T
#   ) +
#   labs(
#     x = "step length [m]", 
#     title = "Tracks subset - one location per day"
#   ) +
#   theme_bw() +
#   scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
# 
# ggsave(
#   here(ggraph_dir, "5.1_one_loc_per_day_steps_box.pdf"), 
#   units = "cm"
# )



# Warning messages:
#   1: Removed 9492 rows containing non-finite values (`stat_bin()`). 
# 2: Removed 1183 rows containing non-finite values (`stat_bin()`). 
# 3: Removed 681 rows containing non-finite values (`stat_bin()`). 
# 4: Removed 195 rows containing non-finite values (`stat_bin()`). 
# 5: Removed 991 rows containing non-finite values (`stat_bin()`). 

# trying with move2 package -----------------------------------------------
# checked if the same distances and speeds are obtained with move2 package, 
# used atm because it is faster
# 
# data <- target_sp[1] |> 
#   map(~{
#     
#     ddir <- here("Data", "Studies", .x, "5_distances")
#     
#     dir.create(here(ddir, "one_loc_move2"))
#     
#     here(ddir, "5.1_all_tracks_one_loc_per_day.rds") |> 
#       read_rds() |>
#       mutate(n_burst = n(), .by = c(burst_, file)) |> 
#       filter(n_burst >= 3) |> 
#       group_split(file, burst_) |> 
#       map(~{
#         
#         file <- unique(.x$file)
#         
#         track <- .x |> 
#           mt_as_move2() 
#         
#         track |> 
#           mutate(
#             speed = mt_speed(track, units = "m/s"), 
#             distance = mt_distance(track, units = "m")
#           ) |> 
#           st_drop_geometry() |> 
#           as_tibble() 
#           #write_rds(here(ddir, "one_loc_move2", file))
#       
#         # track |> 
#         #   mutate(
#         #     speed = mt_speed(track, units = "m/s"), 
#         #     distance = mt_distance(track, units = "m")
#         #   )
#         
#       }) |> 
#       bind_rows()
#         
#   }) |> 
#   bind_rows()
#   
#   
#   
#   


# dist_df <- target_sp |> 
#   map(~{
#     
#     sp_dir <- here("Data", "Studies", .x)
#     
#     here(sp_dir, "5_distances", "5.2_all_tracks_max_daily_distance.rds") |> 
#       read_rds() |> 
#       mutate(species = str_replace(.x, "_", " ")) 
#     
#   }) |> 
#   bind_rows() |> 
#   # removing the rows that have comment "one location per day"
#   filter(is.na(comment)) |> 
#   mutate(
#     time_period = difftime(timestamp_2, timestamp_1, units = "hours") |> 
#       as.numeric(), 
#     hour_1 = format(timestamp_1, format = "%H:%M:%S"),
#     hour_2 = format(timestamp_2, format = "%H:%M:%S")
#   ) |> 
#   left_join(dep_df) |> 
#   filter(is.na(problematic_deployment) | problematic_deployment == F) |> 
#   mutate(
#     max_dist_km = max_dist/1000, 
#     max_dist_log = log10(ifelse(max_dist == 0, max_dist + 1e-5, max_dist))
#   ) |> 
#   units::drop_units() 
# 
# rm(dep_df)
# 
# 
# # p: max dist per species -----------------------------------------------
# 
# 
# h <- dist_df |> 
#   ggplot() +
#   geom_histogram(
#     aes(x = max_dist_km), 
#     binwidth = 1, 
#     fill = "gray66", color = "black"
#   ) +
#   labs(
#     x = "step length [km] | binwidth = 1km", 
#     # bquote("step length [km] | binwidth =" ~ 10^4), 
#     title = "Maximum distance per day"
#   ) +
#   theme_bw() +
#   facet_wrap(~species, ncol = 1, scales = "free_y") 
#   #scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
# 
# # ggsave(
# #   here(ggraph_dir, "5.2_max_distance_per_day.pdf"), 
# #   height = 15,
# #   units = "cm"
# # )
# 
# 
# 
# # max dist per species box ------------------------------------------------
# 
# 
# 
# b <- dist_df |> 
#   # plotting sampling rate for all species 
#   ggplot() +
#   geom_boxplot(
#     aes(x = max_dist_km, y = species, group = species),  
#     fill = "gray66", color = "black", 
#     notch = T
#   ) +
#   labs(
#     x = "step length [km]", 
#     title =  "Maximum distance per day"
#   ) +
#   theme_bw()
# #scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))
# 
# # ggsave(
# #   here(ggraph_dir, "5.2_max_distance_per_day_box.pdf"), 
# #   units = "cm"
# # )
# 
# 
# # p: max dist log scale per species---------------------------------------------
# 
# 
# hl <- dist_df |> 
#   ggplot() +
#   geom_histogram(
#     aes(x = max_dist_log), 
#     bins = 100, 
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~species, ncol = 1, scales = "free_y") +
#   labs(
#     x = "step length [m] - log scale | bins = 100",
#     title = "Maximum distance per day"
#   ) +
#   theme_bw() 
# 
# # ggsave(
# #   here(ggraph_dir, "5.2_max_distance_per_day_log10.pdf"), 
# #   height = 15,
# #   units = "cm"
# # )
# 
# 
# 
# hp <- dist_df |> 
#   ggplot() +
#   geom_histogram(
#     aes(x = time_period), 
#     binwidth = 1, 
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~ species, ncol = 1, scales = "free_y") +
#   labs(
#     x = "time period [hour] | binwidth = 1",
#     title = "Time period between the two locations"
#   ) +
#   theme_bw() 
# 
# 
# ggsave(
#   here(ggraph_dir, "5.2_max_distance_per_day_time_period.pdf"), 
#   height = 15,
#   units = "cm"
# )
# 
# dist_df |> 
#   ggplot() + 
#   geom_boxplot(
#     aes(x = time_period, y = species, group = species),  
#     fill = "gray66", color = "black", 
#     notch = T
#   ) +
#   labs(x = "time period between the points [hour]") +
#   theme_bw() 
# 
# ggsave(
#   here(ggraph_dir, "5.2_max_distance_per_day_time_period_boxplot.pdf"),
#   units = "cm"
# )
# 
# 
# dist_df |> 
#   ggplot() + 
#   geom_histogram(
#     aes(x = locs_per_day), 
#     binwidth = 1, 
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~ species, ncol = 1, scales = "free_y") +
#   # geom_segment(
#   #   aes(x = hour_1, xend = hour_2, y = locs_per_day, yend = locs_per_day), 
#   #   position = "jitter"
#   # ) +
#   # geom_point(
#   #   aes(x = hour_1, y = hour_2),  
#   #   color = "orange", 
#   #   position = "jitter"
#   # ) +
#   # geom_point(
#   #   aes(x = hour_2, y = locs_per_day),
#   #   color = "darkblue", 
#   #   position = "jitter"
#   # ) +
#   labs(
#     x = "locations per day", 
#     title =  "Number of locatioons per day"
#   ) +
#   theme_bw() 
# 
# ggsave(
#   here(ggraph_dir, "5.2_max_distance_number_of_locations.pdf"), 
#   height = 15,
#   units = "cm"
# )

# check -------------------------------------------------------------------
# 
# check_df <- target_sp |> 
#   map(~{
#     
#     sp <- .x 
#     # folder of the species data
#     sp_dir <- here("Data",  "Studies", sp)
#     
#     
#     # list all deployments
#     files <- here(sp_dir, "4_filtered_speed") |> 
#       list.files()
#     lfl <- length(files)
#     
#     files |> 
#       map(~{
#         
#         fname <- .x
#         
#         track <- here(sp_dir, "4_filtered_speed", fname) |> 
#           read_rds() |> 
#           select(all_of(id_cols) | contains("day")) |> 
#           filter(is.na(day_cycle))
#         
#         if(nrow(track) > 0){
#           tibble(
#             species = sp, 
#             file = fname, 
#             folder = here(sp_dir, "4_filtered_speed")
#           )
#         }
#           
#         }) |> 
#       bind_rows()
#     }) |> 
#   bind_rows()
# 
# check_df |> 
#   bind_rows()
# check_file <- here("Data",  "Studies", target_sp[1], )
# 


# correcting the files that had deployment_id.x and deployment_id.y column
# deploy_problem <- here("Data", "deployment_id_check.rds") |> 
#   read_rds() |> 
#   filter(id_col == "deployment") |> 
#   mutate(
#     file =  str_replace(file, "_speed_filtered.rds", "_max_dist.rds"), 
#     m_dir = here(sp_dir, "5_distances", "max_distances"), 
#     full_path = here(m_dir, file)
#     )
#   
# deploy_problem$full_path |> 
#   map(~file.remove(.x))


hp <- dist_df |> 
  ggplot() +
  geom_histogram(
    aes(x = time_period), 
    binwidth = 1, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  labs(
    x = "time period [hour] | binwidth = 1",
    title = "Time period between the two locations"
  ) +
  theme_bw() 


ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_time_period.pdf"), 
  height = 15,
  units = "cm"
)

dist_df |> 
  ggplot() + 
  geom_boxplot(
    aes(x = time_period, y = species, group = species),  
    fill = "gray66", color = "black", 
    notch = T
  ) +
  labs(x = "time period between the points [hour]") +
  theme_bw() 

ggsave(
  here(ggraph_dir, "5b_max_distance_per_day_time_period_boxplot.pdf"),
  units = "cm"
)


dist_df |> 
  ggplot() + 
  geom_histogram(
    aes(x = locs_per_day), 
    binwidth = 1, 
    fill = "gray66", color = "black"
  ) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  # geom_segment(
  #   aes(x = hour_1, xend = hour_2, y = locs_per_day, yend = locs_per_day), 
  #   position = "jitter"
  # ) +
  # geom_point(
  #   aes(x = hour_1, y = hour_2),  
  #   color = "orange", 
  #   position = "jitter"
  # ) +
  # geom_point(
  #   aes(x = hour_2, y = locs_per_day),
#   color = "darkblue", 
#   position = "jitter"
# ) +
labs(
  x = "locations per day", 
  title =  "Number of locatioons per day"
) +
  theme_bw() 

ggsave(
  here(ggraph_dir, "5b_max_distance_number_of_locations.pdf"), 
  height = 15,
  units = "cm"
)






# p: box [km] per species----------------------------------------------

# b <- .x |> 
#   ggplot() +
#   geom_boxplot(
#     aes(x = sl_km),  
#     fill = "gray66", color = "black"
#     #notch = T
#   ) +
#   labs(
#     x = "step length [km]"
#     # title = paste("Distances |", f_id)
#   ) +
#   theme_bw()
#scale_x_continuous(label = ~custom_scientific(.x, fixed_exp = 3))

# ggsave(here(ggraph_dir, pn), units = "cm")
#  
# 

# 
# # overview plot-------------------------------------------------------------
# df |>
#   ggplot() +
#   geom_boxplot(
#     aes(x = sl_km, y = species),
#     fill = "gray66", color = "black"
#   ) +
#   facet_wrap(~month, ncol = 2, scales = "free") +
#   labs(
#     x = "step length [km]",
#     title = paste("Distances |", f_id, "| Europe, across months"),
#   ) +
#   theme_bw()
# 
# ggsave(
#   here(ggraph_dir, pn_b),
#   units = "cm",
#   height = 30
# )





