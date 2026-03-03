# VISUALIZAÇÃO 3D ANIMADA DO GTFS BUZUFBA
# Obs: Por não possuir os dados de acesso à oportunidades via transporte público,
# tentei simular com o modo 'car' para ver a simulação
# PACOTES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, sf, viridis, magrittr, dplyr, 
  ggnewscale, ggplot2, gganimate, av, 
  geobr, aopdata, gtfs2gps, lubridate, gtfstools
)

# FUNÇÕES DE TRANSFORMAÇÃO ESPACIAL
rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  # Matriz de cisalhamento (Shear) e Rotação
  shear_matrix  <- matrix(c(2, 1.2, 0, 1), 2, 2)
  rotate_matrix <- matrix(c(cos(pi/20), sin(pi/20), -sin(pi/20), cos(pi/20)), 2, 2)
  
  geom <- sf::st_geometry(data)
  
  # Multiplicação matricial da geometria
  geom_trans <- geom * shear_matrix * rotate_matrix + c(x_add, y_add)
  
  # Preserva o CRS original (EPSG 4326) para evitar warnings no ggplot
  sf::st_crs(geom_trans) <- sf::st_crs(data)
  sf::st_geometry(data) <- geom_trans
  
  return(data)
}

# LEITURA E PREPARAÇÃO DO GTFS BUZUFBA
gtfs_file <- "data/buzufba_gtfs.zip"
gtfs_raw  <- gtfs2gps::read_gtfs(gtfs_file)

# Filtramos viagens rodando no horário de pico matinal (ex: 07:00 as 09:00)
gtfs_filtrado <- gtfstools::filter_by_time_of_day(
  gtfs_raw,
  from = "07:00:00",
  to = "09:00:00",
  full_trips = TRUE
)

# Geometria das Rotas (Linhas)
routes_sf <- gtfstools::convert_shapes_to_sf(gtfs_filtrado)
sf::st_crs(routes_sf) <- 4326

# RECORTE ESPACIAL (BOUNDING BOX)
# Como Salvador é enorme, vamos focar apenas na área de operação da UFBA.
# Criamos um "buffer" de ~1.5km ao redor das rotas para o fundo do mapa.
bbox_ufba <- sf::st_bbox(routes_sf)
bbox_poly <- sf::st_as_sfc(bbox_ufba)
bbox_buffer <- sf::st_buffer(bbox_poly, dist = 0.015) # dist em graus decimais

# Baixa Salvador e recorta para a área da UFBA
city <- geobr::read_municipality(code_muni = 2927408, year = 2020) %>% 
  sf::st_transform(4326) %>% 
  sf::st_intersection(bbox_buffer)

# Baixa dados do Acesso a Oportunidades (IPEA) e recorta
# R001 = Renda; CMATT60 = Acesso a Empregos em 60 min por transporte público
aop <- aopdata::read_access(city = 'salvador', mode = 'car', geometry = TRUE) %>%
  sf::st_transform(4326) %>% 
  sf::st_intersection(bbox_buffer) %>%
  dplyr::filter(P001 > 0) # Mantém apenas hexágonos habitados

# RASTREAMENTO GPS (VEÍCULOS DO BUZUFBA)
# Transforma as escalas e trajetos GTFS em pontos simulados de GPS
gps <- gtfs2gps::gtfs2gps(gtfs_filtrado, parallel = FALSE, spatial_resolution = 30) # 30m para animação suave
gps <- gps[between(timestamp, as.ITime('07:00:00'), as.ITime('08:30:00'))]

# Converte pontos GPS para objeto espacial (sf)
gps_points <- sf::st_as_sf(gps, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)

# EMPILHAMENTO DAS CAMADAS 3D
# Calculamos o deslocamento ideal de acordo com a nossa Bounding Box da UFBA
shift <- 0.015

city_3d   <- rotate_data(city,      y_add = 0)
aop_inc   <- rotate_data(aop,       y_add = shift * 1)
aop_job   <- rotate_data(aop,       y_add = shift * 2)
routes_3d <- rotate_data(routes_sf, y_add = shift * 3)

# Rotação dos pontos de GPS e reintegração com dados tabulares para o ggplot
gps_points_3d <- rotate_data(gps_points, y_add = shift * 3)
gps_coords    <- sf::st_coordinates(gps_points_3d)

gps_df <- gps %>%
  mutate(
    x = gps_coords[, "X"],
    y = gps_coords[, "Y"],
    # Converte ITime para POSIXct válido para o gganimate
    datetime = as.POSIXct(as.character(timestamp), format = "%H:%M:%S", tz = "America/Bahia")
  ) %>%
  filter(!is.na(datetime))

# Calcula limites dinâmicos para a câmera da Animação focar na área exata
final_bbox <- sf::st_bbox(routes_3d)
x_text <- final_bbox["xmin"] - 0.01
clr <- "gray20"
sz  <- 3.5

# CONSTRUÇÃO DO PLOT
p <- ggplot() +
  
  # Camada 1: Limite da Cidade / Bairros UFBA
  geom_sf(data = city_3d, color = "gray80", fill = "gray90") +
  annotate("text", label = "Área Univ.", x = x_text, y = -12.99, hjust = 0, color = clr, size = sz, fontface = "bold") +
  
  # Camada 2: Renda da População (AOP)
  geom_sf(data = aop_inc, aes(fill = R001), color = NA, alpha = 0.9) +
  scale_fill_viridis_c(option = "E", guide = "none") +
  annotate("text", label = "Distribuição\nde Renda", x = x_text, y = -12.99 + (shift * 1), hjust = 0, color = clr, size = sz) +
  
  # Camada 3: Acessibilidade a Empregos (AOP)
  new_scale_fill() +
  geom_sf(data = aop_job, aes(fill = CMATT15), color = NA, alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno", guide = "none") +
  annotate("text", label = "Acesso a Empregos", x = x_text, y = -12.99 + (shift * 2), hjust = 0, color = clr, size = sz) +
  
  # Camada 4: Rotas BuzUFBA
  geom_sf(data = routes_3d, color = "#00539F", size = 1, alpha = 0.6) +
  annotate("text", label = "Malha\nBuzUFBA", x = x_text, y = -12.99 + (shift * 3), hjust = 0, color = clr, size = sz, fontface = "bold") +
  
  # Camada 5: Ônibus Dinâmicos (GPS Points)
  geom_point(data = gps_df, aes(x = x, y = y, color = shape_id), size = 3, show.legend = FALSE) +
  scale_color_viridis_d(option = "turbo") +
  
  # Ajustes de Layout e Enquadramento Dinâmico
  coord_sf(xlim = c(final_bbox["xmin"] - 0.02, final_bbox["xmax"] + 0.01), 
           ylim = c(final_bbox["ymin"] - 0.02, final_bbox["ymax"] + 0.01)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(t = 10, b = -20))) +
  
  # Configurações do gganimate
  labs(title = "BuzUFBA - Operação Matutina: {format(frame_time, '%H:%M')}") +
  transition_time(datetime) +
  shadow_wake(wake_length = 0.05, alpha = FALSE) +
  ease_aes('linear')

# RENDERIZAÇÃO E EXPORTAÇÃO
message("Renderizando a animação... Isso pode demorar alguns minutos.")

# Salva GIF
anim_save(animation = p, filename = "data_testes/BuzUFBA_Animacao_3D.gif", 
          fps = 10, duration = 15, width = 700, height = 600)

# Salva MP4
anim_mp4 <- animate(p, duration = 15, fps = 20, renderer = av_renderer(), width = 700, height = 600)
anim_save(animation = anim_mp4, filename = "data_testes/BuzUFBA_Animacao_3D.mp4")

message("Exportação Concluída com Sucesso!")