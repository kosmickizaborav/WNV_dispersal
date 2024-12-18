

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


# ggplot number writting-------------------------------------------------

# Custom formatter for x * 10^N
custom_scientific <- function(x, fixed_exp = NA) {
  
  if(!is.na(fixed_exp)){
    exponent = fixed_exp 
  } else {
    exponent <- floor(log10(abs(x)))
  }
  
  base <- x / 10^exponent
  parse(text = ifelse(x < 10, x, paste0(base, "%*% 10^", exponent)))
  
}