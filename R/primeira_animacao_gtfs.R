library(gtfs2gps); library(geobr); library(ggplot2); library(gganimate)
library(ggthemes); library(sf); library(viridis); library(dplyr); 
library(data.table); library(lubridate); library(ggspatial); library(plyr)

salvador <- geobr::read_municipality(code_muni = 2927408, year = 2020) %>% 
  sf::st_transform(4326)

gtfs_dt <- gtfs2gps::read_gtfs("data/gtfs/buzufba_gtfs.zip")

gtfs_dt <- gtfstools::filter_by_weekday(gtfs_dt, "monday", keep = TRUE)

shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(gtfs_dt) %>% sf::st_transform(4326)

message("Gerando pontos de GPS (Isso pode demorar um pouco)...")
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 30, parallel = FALSE)

gps_dt <- gps_dt %>% 
  dplyr::filter(!is.na(timestamp)) %>% 
  dplyr::distinct(trip_id, timestamp, .keep_all = TRUE)

gps_dt <- gps_dt[between(timestamp, as.ITime("06:00:00"), as.ITime("22:10:00"))]

gps_dt$timestamp_local <- as.POSIXct(as.character(gps_dt$timestamp), format = "%H:%M:%S", tz = "America/Bahia")

gps_sf <- sf::st_as_sf(gps_dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE)

mapa <- c("SHP_B1_CIRCULAR_N" = "SHP_B1_CIRCULAR", 
          "SHP_B2_CIRCULAR_N" = "SHP_B2_CIRCULAR",
          "SHP_B3_CIRCULAR_N" = "SHP_B3_CIRCULAR",
          "SHP_B4_CIRCULAR_N" = "SHP_B4_CIRCULAR",
          "SHP_B5_CIRCULAR_N" = "SHP_B5_CIRCULAR")

shapes_sf$shape_id <- mapvalues(
  shapes_sf$shape_id,
  from = names(mapa),
  to = mapa
)

gps_sf$shape_id <- mapvalues(
  gps_sf$shape_id,
  from = names(mapa),
  to = mapa
)

# Animação
gps_sub   <- gps_sf %>% mutate(cumdist = as.numeric(cumdist))
shapes_sub <- shapes_sf

anim <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoom = 16) +
  geom_sf(data = shapes_sub, linetype = "solid", linewidth = 0.5, color = "gray70") +
  geom_sf(data = gps_sub, size = 3, alpha = 0.8, aes(color = shape_id)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10))
  ) +
  labs(title = "Operação BuzUFBA - Segunda-Feira | Horário: {format(frame_time, '%H:%M:%S')}") +
  transition_time(timestamp_local) +
  ease_aes('linear')

message("Renderizando a animação (Isso pode levar alguns minutos)...")
renderizacao <- animate(
  anim,
  nframes = 1200, 
  fps = 30,
  width = 1920, 
  height = 1080, 
  units = "px", 
  res = 150,    
  device = "ragg_png",
  renderer = av_renderer()
)

anim_save(
  animation = renderizacao, 
  filename = "data/figs/anim_circular_2.mp4"
)
