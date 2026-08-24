##### Setup #####

library(tidytransit)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(gganimate)
library(ggspatial)
library(stinepack)
library(geosphere)
library(prettymapr)

# ------------------------------------------------------------
# Caminho para o GTFS
# ------------------------------------------------------------
gtfs_file <- "data/gtfs/buzufba_gtfs.zip"

# Lê o GTFS diretamente do arquivo ZIP
gtfs <- read_gtfs(gtfs_file)

# ------------------------------------------------------------
# Parâmetros da animação
# ------------------------------------------------------------

# Todas as linhas de ônibus do BUZUFBA
route_ids <- gtfs$routes %>%
  filter(route_type == 3) %>%
  pull(route_id)

# Para selecionar linhas específicas, por exemplo:
# route_ids <- c("B1", "B2", "B3")

# Data da operação
#
# O seu feed possui:
# DIAS_UTEIS = segunda a sexta
# SABADO     = sábado
#
# Esta data deve estar dentro de 2026.

dep_date <- as.Date("2026-08-24")

weekday_names <- c(
  "monday",
  "tuesday",
  "wednesday",
  "thursday",
  "friday",
  "saturday",
  "sunday"
)

weekday_col <- weekday_names[
  lubridate::wday(dep_date, week_start = 1)
]

# Normaliza as datas do calendário para Date,
# independentemente de como tidytransit as importou.
calendar <- gtfs$calendar %>%
  mutate(
    start_date = as.Date(
      gsub("-", "", as.character(start_date)),
      format = "%Y%m%d"
    ),
    end_date = as.Date(
      gsub("-", "", as.character(end_date)),
      format = "%Y%m%d"
    )
  )

valid_services <- calendar %>%
  filter(
    start_date <= dep_date,
    end_date >= dep_date
  ) %>%
  filter(
    .data[[weekday_col]] == 1
  ) %>%
  pull(service_id)

if (length(valid_services) == 0) {
  stop(
    paste0(
      "Nenhum serviço do GTFS está ativo em ",
      format(dep_date, "%d/%m/%Y"),
      "."
    )
  )
  
}

##### Identificar as viagens que estarão ativas na janela #####

# Converte horários GTFS para segundos desde 00:00
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(
    arrival_sec = as.numeric(hms(arrival_time)),
    departure_sec = as.numeric(hms(departure_time))
  )

# Horário da janela da animação
min_sec <- as.numeric(hms(min_dep_time))
max_sec <- as.numeric(hms(max_arv_time))

# Calcula o primeiro horário de partida e último horário de chegada
# de cada viagem.
trip_windows <- gtfs$stop_times %>%
  group_by(trip_id) %>%
  summarise(
    first_departure = min(departure_sec, na.rm = TRUE),
    last_arrival = max(arrival_sec, na.rm = TRUE),
    .groups = "drop"
  )

# Seleciona as viagens:
# - de um serviço válido naquele dia;
# - pertencentes às linhas escolhidas;
# - que estavam em circulação em algum momento entre 08:00 e 09:00.
selected_trips <- gtfs$trips %>%
  filter(
    service_id %in% valid_services,
    route_id %in% route_ids
  ) %>%
  inner_join(
    trip_windows,
    by = "trip_id"
  ) %>%
  filter(
    first_departure <= max_sec,
    last_arrival >= min_sec
  )

if (nrow(selected_trips) == 0) {
  stop(
    "Nenhuma viagem foi encontrada dentro da janela temporal especificada."
  )
}

cat("\nViagens selecionadas:\n")
print(
  selected_trips %>%
    select(route_id, trip_id, service_id, shape_id)
)


##### Calcular distância acumulada das shapes #####

# O GTFS do BUZUFBA não possui shape_dist_traveled.
#
# Portanto, calculamos a distância acumulada ao longo de cada shape
# usando a distância Haversine entre pontos consecutivos.

calc_shape_distance <- function(df) {
  
  df <- df %>%
    arrange(shape_pt_sequence)
  
  if (nrow(df) == 1) {
    
    df$dist <- 0
    
  } else {
    
    distances <- geosphere::distHaversine(
      cbind(
        df$shape_pt_lon[-nrow(df)],
        df$shape_pt_lat[-nrow(df)]
      ),
      cbind(
        df$shape_pt_lon[-1],
        df$shape_pt_lat[-1]
      )
    )
    
    df$dist <- c(
      0,
      cumsum(distances)
    )
  }
  
  df
}


route_waypoints <- gtfs$shapes %>%
  filter(
    shape_id %in% unique(selected_trips$shape_id)
  ) %>%
  group_split(shape_id) %>%
  lapply(calc_shape_distance) %>%
  bind_rows() %>%
  rename(
    lat = shape_pt_lat,
    lon = shape_pt_lon
  )


##### Converter shapes para objeto espacial #####

gtfs_sf <- gtfs %>%
  tidytransit::gtfs_as_sf()

# Geometrias completas das rotas
route_shapes <- gtfs_sf %>%
  tidytransit::get_route_geometry(route_ids) %>%
  left_join(
    gtfs$routes,
    by = "route_id"
  ) %>%
  mutate(
    route_color = paste0("#", route_color)
  )


##### Selecionar stop_times das viagens escolhidas #####

selected_stop_times <- gtfs$stop_times %>%
  inner_join(
    selected_trips %>%
      select(
        trip_id,
        route_id,
        shape_id
      ),
    by = "trip_id"
  )


##### Associar cada parada à distância na sua shape #####

# Como o GTFS não informa shape_dist_traveled, encontramos para cada
# parada o ponto da shape geometricamente mais próximo.

shape_ids_selected <- unique(selected_trips$shape_id)

stop_shape_dist <- lapply(
  shape_ids_selected,
  function(sid) {
    
    shape_points <- route_waypoints %>%
      filter(shape_id == sid)
    
    stops_used <- selected_stop_times %>%
      filter(shape_id == sid) %>%
      distinct(stop_id)
    
    stop_points <- gtfs$stops %>%
      filter(stop_id %in% stops_used$stop_id)
    
    if (nrow(stop_points) == 0) {
      return(NULL)
    }
    
    shape_sf <- st_as_sf(
      shape_points,
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )
    
    stops_sf <- st_as_sf(
      stop_points,
      coords = c("stop_lon", "stop_lat"),
      crs = 4326,
      remove = FALSE
    )
    
    nearest_index <- st_nearest_feature(
      stops_sf,
      shape_sf
    )
    
    tibble(
      stop_id = stop_points$stop_id,
      shape_id = sid,
      dist = shape_points$dist[nearest_index]
    )
  }
) %>%
  bind_rows()


##### Criar dataframe com os horários das paradas #####

stops_df <- selected_stop_times %>%
  inner_join(
    gtfs$stops,
    by = "stop_id"
  ) %>%
  left_join(
    stop_shape_dist,
    by = c(
      "stop_id",
      "shape_id"
    )
  ) %>%
  transmute(
    route_id,
    shape_id,
    trip_id,
    stop_sequence,
    arrival_time,
    lat = stop_lat,
    lon = stop_lon,
    dist
  ) %>%
  arrange(
    trip_id,
    stop_sequence
  ) %>%
  group_by(trip_id) %>%
  mutate(
    # Garante que a distância avance ao longo do percurso.
    # Isso evita pequenos recuos causados pelo ponto de shape
    # mais próximo.
    dist = cummax(dist)
  ) %>%
  ungroup()


##### Obter todos os waypoints das viagens #####

waypoints_df <- stops_df %>%
  distinct(
    trip_id,
    shape_id
  ) %>%
  inner_join(
    route_waypoints,
    by = "shape_id"
  ) %>%
  transmute(
    trip_id,
    shape_id,
    route_id = selected_trips$route_id[
      match(trip_id, selected_trips$trip_id)
    ],
    arrival_time = NA_character_,
    lat,
    lon,
    dist
  )


##### Interpolar o horário dos veículos ao longo da rota #####

# Combina:
#   - pontos conhecidos = paradas
#   - pontos desconhecidos = waypoints da shape
#
# Depois utiliza interpolação de Stineman para descobrir
# aproximadamente em que horário o ônibus estará em cada waypoint.

##### Interpolar o horário dos veículos ao longo da rota #####

# ------------------------------------------------------------
# 1. Paradas: possuem horários conhecidos
# ------------------------------------------------------------

stops_interp <- stops_df %>%
  select(
    route_id,
    shape_id,
    trip_id,
    lat,
    lon,
    dist,
    arrival_time
  ) %>%
  mutate(
    # Converte o horário GTFS para segundos desde 00:00:00.
    # hms() funciona também com horários GTFS normais.
    time_sec = as.numeric(arrival_time)
  )


# ------------------------------------------------------------
# 2. Waypoints: horários inicialmente desconhecidos
# ------------------------------------------------------------

waypoints_interp <- waypoints_df %>%
  select(
    route_id,
    shape_id,
    trip_id,
    lat,
    lon,
    dist
  ) %>%
  mutate(
    time_sec = NA_real_
  )


# ------------------------------------------------------------
# 3. Combinar paradas + waypoints
# ------------------------------------------------------------

final_df <- bind_rows(
  stops_interp,
  waypoints_interp
) %>%
  group_by(
    trip_id
  ) %>%
  
  # Ordena os pontos pela posição ao longo da shape
  arrange(
    dist,
    .by_group = TRUE
  ) %>%
  
  # Caso existam pontos exatamente na mesma distância,
  # mantém apenas um deles.
  group_by(
    trip_id,
    dist
  ) %>%
  slice(1) %>%
  ungroup() %>%
  
  group_by(
    trip_id
  ) %>%
  
  # Só podemos interpolar uma viagem que tenha pelo menos
  # dois horários de parada conhecidos.
  filter(
    sum(!is.na(time_sec)) >= 2
  ) %>%
  
  mutate(
    # Interpolação temporal ao longo da distância.
    time_sec = stinepack::na.stinterp(
      object = time_sec,
      along = dist,
      na.rm = FALSE
    )
  ) %>%
  
  ungroup() %>%
  
  # Remove pontos sem horário após a interpolação
  filter(
    !is.na(time_sec)
  )

##### Bounding box #####

bbox <- st_bbox(route_shapes)


##### Criar a animação #####

p <- ggplot(route_shapes) +
  
  # Fundo do mapa
  annotation_map_tile(
    type = "cartolight",
    zoomin = 0,
    progress = "none"
  ) +
  
  # Linhas das rotas
  geom_sf(
    data = route_shapes,
    aes(color = route_color),
    linewidth = 1.5
  ) +
  
  # Paradas
  geom_sf(
    data = gtfs_sf$stops %>%
      filter(
        stop_id %in% unique(selected_stop_times$stop_id)
      ),
    stroke = 1.5,
    size = 4,
    shape = 21,
    color = "#000000",
    fill = "#FFFFFF"
  ) +
  
  # Ônibus
  geom_point(
    data = final_df,
    aes(
      x = lon,
      y = lat,
      group = trip_id
    ),
    size = 2.5,
    shape = 15
  ) +
  
  # Limites geográficos
  coord_sf(
    xlim = c(
      bbox["xmin"],
      bbox["xmax"]
    ),
    ylim = c(
      bbox["ymin"],
      bbox["ymax"]
    )
  ) +
  
  scale_color_identity() +
  
  transition_components(
    time_sec
  ) +
  
  ease_aes(
    "sine-in-out"
  ) +
  
  theme_void() +
  
  labs(
    title = agency_name,
    subtitle = "{frame_time}"
  ) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 24
    ),
    plot.subtitle = element_text(
      size = 18
    )
  )


##### Gerar os frames #####

frames <- as.numeric(
  hms(max_arv_time) -
    hms(min_dep_time)
) * min_to_hour_ratio

plot_mg <- animate(
  plot = p,
  nframes = 60, # frames,
  fps = 10, # frames_per_second,
  width = 800,
  height = 800,
  device = "png",
  renderer = file_renderer(
    dir = "data/gtfs/buzufba_frames",
    overwrite = TRUE
  )
)
