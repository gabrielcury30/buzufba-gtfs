##### Setup #####
library(tidytransit)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(gganimate)
library(ggspatial)
library(stinepack)

feed_path    <- "data/gtfs/buzufba_gtfs.zip"
transit_type <- 3   # 3 = ônibus 

# Bounding box de Salvador (lon_min, lat_min, lon_max, lat_max)
bbox <- c(-38.530, -13.012, -38.498, -12.982)

dep_date     <- Sys.Date()
min_dep_time <- "08:00:00"
max_arv_time <- "09:00:00"

min_to_hour_ratio <- 0.25 
frames_per_second <- 60

gtfs <- read_gtfs(feed_path)

feed_name <- if (!is.null(gtfs$agency) && nrow(gtfs$agency) > 0) gtfs$agency$agency_name[1] else "BuzUFBA"
tz        <- if (!is.null(gtfs$agency) && nrow(gtfs$agency) > 0) gtfs$agency$agency_timezone[1] else "America/Bahia"

route_ids <- gtfs$routes %>% filter(route_type == transit_type) %>% pull(route_id)
trip_ids  <- gtfs$trips  %>% filter(route_id %in% route_ids)  %>% pull(trip_id)

##### 1) Criar shape_dist_traveled (distância acumulada em metros) #####
haversine_m <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180; R <- 6371000
  dlat <- (lat2 - lat1) * rad; dlon <- (lon2 - lon1) * rad
  a <- sin(dlat/2)^2 + cos(lat1*rad)*cos(lat2*rad)*sin(dlon/2)^2
  2 * R * asin(sqrt(a))
}

shapes_dist <- gtfs$shapes %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  mutate(
    shape_dist_traveled = cumsum(
      c(0, haversine_m(shape_pt_lat[-n()], shape_pt_lon[-n()],
                       shape_pt_lat[-1],  shape_pt_lon[-1]))
    )
  ) %>%
  ungroup()

##### 2) Snap monotônico das paradas no shape (essencial p/ circulares) #####
snap_stops_to_shape <- function(stop_lat, stop_lon, shp) {
  n <- nrow(shp); last <- 1L
  out <- numeric(length(stop_lat))
  for (i in seq_along(stop_lat)) {
    d <- haversine_m(stop_lat[i], stop_lon[i],
                     shp$shape_pt_lat[last:n], shp$shape_pt_lon[last:n])
    j <- last + which.min(d) - 1L
    out[i] <- shp$shape_dist_traveled[j]
    last <- j   # próxima parada só procura daqui p/ frente
  }
  out
}

stops_df <- gtfs$trips %>%
  filter(trip_id %in% trip_ids) %>%
  select(trip_id, route_id, shape_id) %>%
  inner_join(gtfs$stop_times, by = "trip_id") %>%
  inner_join(gtfs$stops,      by = "stop_id") %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  group_modify(~ {
    shp <- shapes_dist %>% filter(shape_id == first(.x$shape_id))
    .x %>% mutate(dist = snap_stops_to_shape(.x$stop_lat, .x$stop_lon, shp))
  }) %>%
  ungroup()

##### 3) Interpolação #####
waypoints_df <- stops_df %>%
  distinct(trip_id, shape_id) %>%
  inner_join(
    shapes_dist %>%
      select(shape_id, lat = shape_pt_lat, lon = shape_pt_lon,
             dist = shape_dist_traveled),
    by = "shape_id"
  )

t_min <- as.POSIXct(paste(dep_date, min_dep_time), tz = tz)
t_max <- as.POSIXct(paste(dep_date, max_arv_time), tz = tz)

final_df <- stops_df %>%
  select(trip_id, route_id, arrival_time, lat = stop_lat, lon = stop_lon, dist) %>%
  bind_rows(waypoints_df %>% select(trip_id, lat, lon, dist)) %>%
  mutate(time_str = as.character(arrival_time)) %>%
  mutate(time = as.POSIXct(if_else(is.na(time_str), NA_character_,
                                   paste(dep_date, time_str)), tz = tz)) %>%
  arrange(trip_id, dist) %>%
  group_by(trip_id, dist) %>% filter(row_number() == 1) %>%  # stop fica na frente p/ vencer o dedupe
  group_by(trip_id) %>%
  filter(sum(!is.na(time)) > 1) %>%
  mutate(
    time = as.POSIXct(
      stinepack::na.stinterp(as.numeric(time), along = dist, na.rm = FALSE),
      origin = "1970-01-01", tz = tz
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(time), time >= t_min, time <= t_max)

##### 4) Geometrias p/ o mapa #####
gtfs_sf <- gtfs_as_sf(gtfs)

route_shapes <- gtfs_sf %>%
  get_route_geometry(route_ids) %>%
  left_join(gtfs$routes, by = "route_id") %>%
  mutate(route_color = c(B1="#C4342D",B2="#00539F",B3="#00944A",B4="#8A4FAF",B5="#F7941D")[route_id])

stop_shapes <- gtfs_sf$stops %>% filter(stop_id %in% unique(stops_df$stop_id))

##### 5) Animação #####
p <- ggplot(route_shapes) +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") + 
  geom_sf(data = route_shapes, aes(color = route_color), size = 1.2) +
  geom_sf(data = stop_shapes, stroke = 1.2, size = 3, shape = 21,
          color = "#000000", fill = "#ffffff") +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
  geom_point(data = final_df, aes(x = lon, y = lat, group = trip_id),
             size = 2, shape = 15) +
  scale_color_identity() +
  transition_components(time) +
  ease_aes("sine-in-out") +
  theme_void() +
  labs(title = feed_name, subtitle = "{frame_time}") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 18))

frames <- as.numeric(hms(max_arv_time) - hms(min_dep_time)) * min_to_hour_ratio
plot_mg <- animate(p, nframes = frames, fps = frames_per_second,
                   width = 800, height = 800, device = "png",
                   renderer = file_renderer(dir = "./animacao_buzufba_plots/", overwrite = TRUE))