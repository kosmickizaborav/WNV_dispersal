# Install and load the necessary packages
library(sf)
library(geosphere)

# Define the starting point (longitude, latitude)

starting point <- c(45, 5)

simulated_track <- tibble(
  date = 
)




# Define the distance between points in meters
distance <- 1000  # 1000 meters

# Define the bearing (direction) in degrees
bearing <- 45  # 45 degrees (northeast)

# Define the number of points
num_points <- 10

# Initialize vectors to store the coordinates
lons <- numeric(num_points)
lats <- numeric(num_points)

# Set the starting point
lons[1] <- start_lon
lats[1] <- start_lat
bearings <- runif(num_points - 1, min = 0, max = 360)

set.seed(12345)
# Calculate the subsequent points
for (i in 2:num_points) {
  # Generate random bearings
  b <- runif(1, min = 0, max = 360)
  new_point <- destPoint(, b, distance)
  
}

# Combine the coordinates into a data frame
track_data <- data.frame(lon = lons, lat = lats)

# Convert the data frame into an sf object
track_sf <- st_as_sf(track_data, coords = c("lon", "lat"), crs = 4326)

# Print the sf object
print(track_sf)

# Plot the track
plot(track_sf, main = "Simulated Track", pch = 16, col = "blue")
lines(st_coordinates(track_sf), col = "red", lwd = 2)



# alternative aproach -----------------------------------------------------

# Function to generate tracking points
generate_tracking_data <- function(start_point, distance, n_points) {
  # Create an empty tibble to store points
  points <- tibble(
    lon = numeric(n_points),
    lat = numeric(n_points)
  )
  
  # Set the first point as the start_point
  points[1, ] <- start_point
  
  # Calculate subsequent points
  for (i in 2:n_points) {
    # Generate random direction (in radians)
    direction <- runif(1, 0, 2 * pi)
    
    # Calculate new point coordinates
    new_lon <- points$lon[i - 1] + distance * cos(direction)
    new_lat <- points$lat[i - 1] + distance * sin(direction)
    
    # Add new point to points tibble
    points[i, ] <- c(new_lon, new_lat)
  }
  
  # Convert points tibble to sf object
  tracking_data <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  
  return(tracking_data)
}

# Example usage
start_point <- c(lon = -122.4194, lat = 37.7749)  # San Francisco coordinates
distance <- 0.01  # Distance in degrees
n_points <- 10  # Number of points to generate

tracking_data <- generate_tracking_data(start_point, distance, n_points)

# Print the generated tracking data
print(tracking_data)

# Plot the tracking data
plot(tracking_data)