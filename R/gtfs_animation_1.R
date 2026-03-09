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

# Geometria da rede
shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(gtfs_dt) %>% sf::st_transform(4326)

# Converte GTFS para posições de GPS simuladas (Resolução de 10m para animação fluida)
message("Gerando pontos de GPS (Isso pode demorar um pouco)...")
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 10, parallel = FALSE)

# Filtrar pontos duplicados e valores ausentes no tempo
gps_dt <- gps_dt %>% 
  dplyr::filter(!is.na(timestamp)) %>% 
  dplyr::distinct(trip_id, timestamp, .keep_all = TRUE)

# Usamos lubridate para forçar o fuso correto
gps_dt$timestamp_local <- as.POSIXct(as.character(gps_dt$timestamp), format = "%H:%M:%S", tz = "America/Bahia")

# Converte o GPS nativamente para sf (Melhor prática para bounding box)
gps_sf <- sf::st_as_sf(gps_dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE)

###### ANÁLISE ESTÁTICA E ANIMAÇÃO - LINHA B1 (CIRCULAR)
# Filtra Trip e Shape únicos (B1 Circular às 06:10)
trip_B1 <- "B1_DIAS_UTEIS_CIRCULAR_0610"
shp_B1  <- "SHP_B1_CIRCULAR"

gps_sub_B1   <- gps_sf %>% filter(trip_id == trip_B1) %>% mutate(cumdist = as.numeric(cumdist))
shapes_sub_B1 <- shapes_sf %>% filter(shape_id == shp_B1)

# GRÁFICO ESTÁTICO B1
p_estatico_B1 <- ggplot() +
  geom_sf(data = shapes_sub_B1, linetype = "dashed", linewidth = 0.5, color = "gray50") +
  geom_sf(data = gps_sub_B1, aes(color = cumdist), size = 1.5, alpha = 0.6) +
  scale_color_viridis_c(name = "Distância (m)") +
  theme_map() +
  labs(title = "BuzUFBA B1 - Rota Circular Contínua (06:10)")
print(p_estatico_B1)

# ANIMAÇÃO B1
anim_B1 <- ggplot() +
  geom_sf(data = shapes_sub_B1, linetype = "solid", linewidth = 0.5, color = "gray70") +
  geom_sf(data = gps_sub_B1, color = "blue", size = 5, alpha = 0.7) +
  # sf::st_bbox() para enquadrar sem precisar de min/max manuais
  coord_sf(xlim = sf::st_bbox(gps_sub_B1)[c("xmin", "xmax")],
           ylim = sf::st_bbox(gps_sub_B1)[c("ymin", "ymax")]) +
  theme_map() +
  labs(title = "Rota B1 Circular | Horário: {format(frame_time, '%H:%M:%S')}") +
  transition_time(timestamp_local) +
  ease_aes('linear')

anim_save(animation = animate(anim_B1, fps = 12, nframes = 300), 
          filename = "data_testes/anim_B1_0610_circular.gif", renderer = gifski_renderer())


###### ANÁLISE ESTÁTICA E ANIMAÇÃO - LINHA B2 (CIRCULAR)
# Filtra Trip e Shape da linha B2 às 06:00
trip_B2 <- "B2_DIAS_UTEIS_CIRCULAR_0600"
shp_B2  <- "SHP_B2_CIRCULAR"

gps_sub_B2   <- gps_sf %>% filter(trip_id == trip_B2) %>% mutate(cumdist = as.numeric(cumdist))
shapes_sub_B2 <- shapes_sf %>% filter(shape_id == shp_B2)

# GRÁFICO ESTÁTICO B2
p_estatico_B2 <- ggplot() +
  geom_sf(data = shapes_sub_B2, linetype = "dashed", linewidth = 0.5, color = "gray50") +
  geom_sf(data = gps_sub_B2, aes(color = cumdist), size = 1.5, alpha = 0.6) +
  scale_color_viridis_c(option = "plasma", name = "Distância (m)") +
  theme_map() +
  labs(title = "BuzUFBA B2 - Rota Circular Contínua (06:00)")
print(p_estatico_B2)

# ANIMAÇÃO B2
anim_B2 <- ggplot() +
  geom_sf(data = shapes_sub_B2, linetype = "solid", linewidth = 0.5, color = "gray70") +
  geom_sf(data = gps_sub_B2, color = "red", size = 5, alpha = 0.7) +
  coord_sf(xlim = sf::st_bbox(gps_sub_B2)[c("xmin", "xmax")],
           ylim = sf::st_bbox(gps_sub_B2)[c("ymin", "ymax")]) +
  theme_map() +
  labs(title = "Rota B2 Circular | Horário: {format(frame_time, '%H:%M:%S')}") +
  transition_time(timestamp_local) + 
  ease_aes('linear')

anim_save(animation = animate(anim_B2, fps = 12, nframes = 300), 
          filename = "data_testes/anim_B2_0600_circular.gif", renderer = gifski_renderer())

message("Todas as animações circulares foram geradas com sucesso!")
