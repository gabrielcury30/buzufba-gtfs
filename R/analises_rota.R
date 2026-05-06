options(java.parameters = "-Xmx4G")

library(r5r)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)
library(r5rgui)

r5r_network <- build_network("data/r5r")

edif <- st_read("data/edif/edif_ufba.gpkg")

edif_points <- edif %>%
    st_transform(4326) %>%
    st_point_on_surface()

pod <- data.frame(
    id = edif_points$name,
    lon = st_coordinates(edif_points)[, 1],
    lat = st_coordinates(edif_points)[, 2]
)

departure_datetime <- as.POSIXct("2026-04-29 08:50:00", tz = "America/Bahia")
time_window <- 20L
max_rides <- 1L
mode <- c("WALK", "TRANSIT")
max_trip_duration <- 150
percentiles <- c(25, 50, 75, 90)

ttm <- travel_time_matrix(
    r5r_network = r5r_network,
    origins = pod,
    destinations = pod,
    departure_datetime = departure_datetime,
    mode = mode,
    max_rides = max_rides,
    percentiles = percentiles,
    max_trip_duration = max_trip_duration,
    time_window = time_window
)

ettm <- expanded_travel_time_matrix(
    r5r_network = r5r_network,   
    origins = pod,
    destinations = pod,    
    mode = mode,
    departure_datetime = departure_datetime,
    max_rides = max_rides,
    max_trip_duration = max_trip_duration,
    breakdown = TRUE,
    time_window = time_window
)

ettm_walk <- ettm %>% 
  filter(routes == "[WALK]") %>% 
  arrange(desc(total_time))

det <- detailed_itineraries(
  r5r_network = r5r_network,
  origins = pod,
  destinations = pod,
  mode = mode,
  departure_datetime = departure_datetime,
  max_rides = max_rides,
  max_trip_duration = max_trip_duration,
  all_to_all = TRUE,
  time_window = time_window
)

r5r_gui(r5r_network)

stop_r5(r5r_network)
