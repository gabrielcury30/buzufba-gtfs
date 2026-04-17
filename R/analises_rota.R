options(java.parameters = "-Xmx4G")

library(r5r)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)

r5r_network <- build_network("data/r5r")

edif <- st_read("data/edif/edif_ufba.gpkg")
head(edif)

edif_points <- edif %>%
    st_transform(4326) %>%
    st_point_on_surface()
head(edif_points)

pod <- data.frame(
    id = edif_points$name,
    lon = st_coordinates(edif_points)[, 1],
    lat = st_coordinates(edif_points)[, 2]
)
head(pod)

departure_datetime <- as.POSIXct("2026-04-15 12:00:00", tz = "America/Bahia")
time_window <- 10L
max_rides <- 1L
mode <- c("WALK", "TRANSIT")

ttm <- travel_time_matrix(
    r5r_network = r5r_network,
    origins = pod,
    destinations = pod,
    departure_datetime = departure_datetime,
    mode = mode,
    max_rides = max_rides
)
head(ttm)

ettm <- expanded_travel_time_matrix(
    r5r_network = r5r_network,   
    origins = pod,
    destinations = pod,    
    mode = mode,
    departure_datetime = departure_datetime,
    max_rides = max_rides,
    breakdown = TRUE
)
head(ettm)

det <- detailed_itineraries(
  r5r_network = r5r_network,
  origins = pod,
  destinations = pod,
  mode = mode,
  departure_datetime = departure_datetime,
  max_rides = max_rides,
  all_to_all = TRUE
)
head(det)

from_pick <- "Escola Politécnica da UFBA"
to_pick   <- "Residência Universitária R2"

det_sel <- det %>%
  filter(from_id == from_pick, to_id == to_pick) %>%
  st_transform(4326) %>%
  mutate(segment_label = paste0(mode, "_seg", segment))
head(det_sel)

pod_sf <- st_as_sf(pod, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
head(pod_sf)

pt_from <- pod_sf %>% filter(id == from_pick)
pt_to   <- pod_sf %>% filter(id == to_pick)
head(pt_from)
head(pt_to)

ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 16) +
  geom_sf(data = det_sel, aes(color = mode), linewidth = 1.2) +
  geom_sf(data = pt_from, color = "black", aes(fill = "Origem"), shape = 21, size = 3, stroke = 1) +
  geom_sf(data = pt_to,   color = "black", aes(fill = "Destino"), shape = 21, size = 3, stroke = 1) +
  scale_fill_manual(
    name = "Pontos",
    values = c("Origem" = "white", "Destino" = "yellow")
  ) +
  scale_color_viridis_d(option = "turbo", end = 0.95) +
  labs(color = "mode", title = paste(from_pick, "→", to_pick), subtitle = paste("Data e hora:", format(departure_datetime, "%d/%m/%Y %H:%M"))) +
  theme_minimal()

ggsave("data/figs/itinerario.png", dpi = 500)

stop_r5(r5r_network)
