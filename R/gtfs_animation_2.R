
# ANIMAÇÃO 2D DE RASTREAMENTO - GTFS BUZUFBA
# Carregando Bibliotecas
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gtfstools, gtfs2gps, geobr, ggplot2, 
  gganimate, data.table, ggthemes, sf, 
  viridis, ggspatial, lubridate
)

# PROCESSAMENTO DOS DADOS DO BUZUFBA
# Lendo o GTFS
gtfs_file <- "data/buzufba_gtfs.zip"
gtfs_dt <- gtfstools::read_gtfs(gtfs_file)

# Filtrar apenas os serviços de dias úteis
gtfs_dt <- gtfstools::filter_by_service_id(gtfs_dt, service_id = "DIAS_UTEIS")

# Extrair a malha geométrica das rotas
shapes_sf <- gtfstools::convert_shapes_to_sf(gtfs_dt)
sf::st_crs(shapes_sf) <- 4326

# Converter GTFS em posições de GPS a cada 30 metros
message("Simulando GPS dos ônibus...")
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, parallel = FALSE, spatial_resolution = 10)

# Filtrar pontos duplicados e valores ausentes no tempo
gps_dt <- gps_dt %>% 
  dplyr::filter(!is.na(timestamp)) %>% 
  dplyr::distinct(trip_id, timestamp, .keep_all = TRUE)

# Filtrar janela de tempo de pico matinal (07:00 as 08:30)
gps_dt2 <- gps_dt[between(timestamp, as.ITime("07:00:00"), as.ITime("08:30:00"))]

# Converter pontos de GPS em objeto geográfico (sf) de forma nativa e segura
gps_sf <- sf::st_as_sf(gps_dt2, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)

# Criando coluna de tempo real com fuso horário da Bahia
gps_sf$datetime <- as.POSIXct(as.character(gps_sf$timestamp), format = "%H:%M:%S", tz = "America/Bahia")

# CONTEXTO ESPACIAL (Recorte da Cidade)
# Calculando a Bounding Box (Caixa limitadora) ao redor das rotas do BuzUFBA
bbox <- sf::st_bbox(shapes_sf)
bbox_poly <- sf::st_as_sfc(bbox)
bbox_buffer <- sf::st_buffer(bbox_poly, dist = 0.015) 

# Baixando e recortando o contorno de Salvador
salvador <- geobr::read_municipality(code_muni = 2927408, year = 2020) %>% 
  sf::st_transform(4326) %>% 
  sf::st_intersection(bbox_buffer)

# ANIMAÇÃO COM FUNDO VETORIAL
message("Gerando Animação 1 (Fundo Vetorial)...")

anim_vector <- ggplot() +
  geom_sf(data = salvador, color = "gray60", fill = "gray95") +
  geom_sf(data = shapes_sf, color = "gray70", linewidth = 0.5) +
  geom_sf(data = gps_sf, aes(color = trip_id), size = 3, show.legend = FALSE) +
  scale_color_viridis_d(option = "turbo") +
  
  # Focando a câmera exatamente na UFBA
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_map() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  
  # gganimate 
  labs(title = "BuzUFBA - Horário: {format(frame_time, '%H:%M')}") +
  transition_time(datetime) +
  #shadow_wake(wake_length = 0.05, alpha = FALSE) +
  ease_aes('linear')

# Salva GIF
anim_save(animation = anim_vector, "data_testes/BuzUFBA_Anim_Vector.gif", fps = 15, duration = 10, width = 600, height = 500)
