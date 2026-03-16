# RASTREAMENTO E ANIMAÇÃO - GTFS BUZUFBA (MODELO CIRCULAR)
library(gtfs2gps); library(geobr); library(ggplot2); library(gganimate)
library(ggthemes); library(sf); library(viridis); library(gifski)
library(dplyr); library(data.table); library(lubridate)

# Download do limite de Salvador
salvador <- geobr::read_municipality(code_muni = 2927408, year = 2020) %>% 
  sf::st_transform(4326) # Padronizando projeção

###### PROCESSAMENTO DOS DADOS DO TRANSPORTE
# Lê o GTFS
gtfs_dt <- gtfs2gps::read_gtfs("data/buzufba_gtfs.zip")

# Filtrar operação da segunda-feira
gtfs_dt <- gtfstools::filter_by_weekday(gtfs_dt, "monday", keep = TRUE)

# Geometria da rede
shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(gtfs_dt) %>% sf::st_transform(4326)

# Converte GTFS para posições de GPS simuladas (Resolução de 10m para animação fluida)
message("Gerando pontos de GPS (Isso pode demorar um pouco)...")
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 10, parallel = FALSE)

# Filtrar pontos duplicados e valores ausentes no tempo
gps_dt <- gps_dt %>% 
  dplyr::filter(!is.na(timestamp)) %>% 
  dplyr::distinct(trip_id, timestamp, .keep_all = TRUE)

gps_dt <- gps_dt[between(timestamp, as.ITime("06:00:00"), as.ITime("22:10:00"))]

# Usamos lubridate para forçar o fuso correto
gps_dt$timestamp_local <- as.POSIXct(as.character(gps_dt$timestamp), format = "%H:%M:%S", tz = "America/Bahia")

# Converte o GPS nativamente para sf (Melhor prática para bounding box)
gps_sf <- sf::st_as_sf(gps_dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE)

# Animação
gps_sub   <- gps_sf %>% mutate(cumdist = as.numeric(cumdist))
shapes_sub <- shapes_sf

# ANIMAÇÃO
anim <- ggplot() +
  geom_sf(data = shapes_sub, linetype = "solid", linewidth = 0.5, color = "gray70") +
  geom_sf(data = gps_sub, color = "blue", size = 5, alpha = 0.7) +
  # sf::st_bbox() para enquadrar sem precisar de min/max manuais
  coord_sf(xlim = sf::st_bbox(gps_sub)[c("xmin", "xmax")],
           ylim = sf::st_bbox(gps_sub)[c("ymin", "ymax")]) +
  theme_map() +
  labs(title = "Rota Circular | Horário: {format(frame_time, '%H:%M:%S')}") +
  shadow_wake(wake_length = 0.05, alpha = FALSE) +
  transition_time(timestamp_local) +
  ease_aes('linear')

anim_save(animation = animate(anim, fps = 30, nframes = 300, duration = 10), 
          filename = "data_testes/anim_circular.gif", renderer = gifski_renderer())
