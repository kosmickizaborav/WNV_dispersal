

# getting utm zone --------------------------------------------------------

# taken from:
# https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude
# get_utmzone <- function(lon,lat) {
#   ## Special Cases for Norway & Svalbard
#   if (lat > 55 & lat < 64 & lon > 2 & lon < 6){ 
#     band <- 32
#   } else {
#     if (lat > 71 & lon >= 6 & lon < 9){
#       band <- 31
#     } else {
#       if (lat > 71 & lon >= 9 & lon < 12){
#         band <- 33
#       } else {
#         if (lat > 71 & lon >= 18 & lon < 21){
#           band <- 33
#         } else {
#           if (lat > 71 & lon >= 21 & lon < 24){
#             band <- 35
#           } else {
#             if (lat > 71 & lon >= 30 & lon < 33){
#               band <- 35
#             } else {
#               ## Rest of the world
#               if (lon >= -180 & lon <= 180){
#                 band <- (floor((lon + 180)/6) %% 60) + 1
#               } else {
#                 band <- "something is wrong"
#               }}}}}}}
#   return(band)
# }


get_utmzone <- function(lon,lat) {

  df <- tibble(lon = lon, lat = lat) |> 
    mutate(
      utm_zone = case_when(
        lat > 55 & lat < 64 & lon > 2 & lon < 6 ~ 32, 
        lat > 71 & lon >= 6 & lon < 9 ~ 31, 
        lat > 71 & lon >= 9 & lon < 12 ~ 33, 
        lat > 71 & lon >= 18 & lon < 21 ~ 33, 
        lat > 71 & lon >= 21 & lon < 24 ~ 35, 
        lat > 71 & lon >= 30 & lon < 33 ~ 35, 
        lon >= -180 & lon <= 180 ~ (floor((lon + 180)/6) %% 60) + 1, 
        .default = NA 
      ) 
    ) |> 
    mutate(
      utm_zone = if_else(lat > 0, utm_zone + 32600, utm_zone + 32700)
    )

  return(df$utm_zone)
  }


# compute timelags --------------------------------------------------------


# function for comuting timelags between the subsequent observations
compute_timelags <- function(time, units = "mins") {
  as.numeric(difftime(c(time[-1], NA), time, units = units))
}


# getting move parameters -------------------------------------------------

compute_move_param <- function(
    df, 
    coords = c("lon", "lat"), 
    time = "timestamp", 
    id = "track_id", 
    proj = sf::st_crs(4326),
    units = c(t = "s", d = "m", a = "degree")) {
  
  mdf <- move2::mt_as_move2(
    df, 
    coords = coords,
    #sf_column_name = c("geometry"),
    time_column = time,
    track_id_column = id, 
    crs = proj
  ) 
  
  df |> 
    mutate(
      out_speed = move2::mt_speed(
        mdf, units = str_c(units[["d"]], "/", units[["t"]]))
      , 
      in_speed = c(NA, out_speed[-n()]), 
      turn = move2::mt_turnangle(mdf, units = units[["a"]]), 
      out_distance = move2::mt_distance(mdf, units = units[["d"]]), 
      in_distance = c(NA, out_speed[-n()]),
      n_loc = 1:n()
    )
  
}
