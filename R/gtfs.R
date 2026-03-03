# PACOTES E OPÇÕES
library(gtfstools)
library(tidyverse)
library(data.table)
library(osrm)
library(sf)
library(mapview)

# FUNÇÕES AUXILIARES
gerar_shape_via_api <- function(shape_id, sequencia_stops, df_stops) {
  message(sprintf("Gerando shape circular: %s ...", shape_id))
  
  coords <- df_stops[match(sequencia_stops, df_stops$stop_id), ]
  
  rota_sf <- osrm::osrmRoute(
    loc = coords[, c("stop_lon", "stop_lat")], 
    overview = "full"
  )
  
  pts <- sf::st_coordinates(rota_sf)
  
  df_shape <- tibble(
    shape_id = shape_id,
    shape_pt_lat = pts[, "Y"],
    shape_pt_lon = pts[, "X"],
    shape_pt_sequence = 1:nrow(pts)
  )
  
  return(df_shape)
}

gerar_dados_rota <- function(route_id, service_id, direction_id, horarios, sequencia_stops, shape_id) {
  if (length(horarios) == 0) return(NULL)
  
  mins_inter_stop <- 3 
  
  # Cria as viagens (trips) de forma circular
  trips <- tibble(
    route_id = route_id,
    service_id = service_id,
    trip_id = sprintf("%s_%s_CIRCULAR_%s", route_id, service_id, str_remove(horarios, ":")),
    direction_id = direction_id,
    shape_id = shape_id,
    horario_inicio = horarios
  )
  
  # Cria os horários de parada (stop_times) da rota circular inteira
  stop_times <- trips %>%
    mutate(stop_id = list(sequencia_stops)) %>%
    unnest(stop_id) %>%
    group_by(trip_id) %>%
    mutate(
      stop_sequence = row_number(),
      offset_min = (stop_sequence - 1) * mins_inter_stop,
      minutos_totais = (as.numeric(substr(horario_inicio, 1, 2)) * 60) + 
        as.numeric(substr(horario_inicio, 4, 5)) + offset_min,
      arrival_time = sprintf("%02d:%02d:00", floor(minutos_totais / 60), 
                             minutos_totais %% 60),
      departure_time = arrival_time
    ) %>%
    ungroup() %>%
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)
  
  trips <- trips %>% select(-horario_inicio)
  
  return(list(trips = trips, stop_times = stop_times))
}

# Remove São Lázaro à noite, 
# mas evita que a sequência fique com paradas duplicadas consecutivas
remover_sao_lazaro <- function(seq) {
  seq_limpa <- seq[seq != "SAO_LAZARO"]
  seq_limpa <- seq_limpa[c(TRUE, seq_limpa[-1] != seq_limpa[-length(seq_limpa)])]
  return(seq_limpa)
}

# Função auxiliar para fundir ida e volta perfeitamente 
# sem duplicar o ponto de encontro
unir_circular <- function(ida, volta) {
  if (tail(ida, 1) == volta[1]) return(c(ida, volta[-1]))
  return(c(ida, volta))
}


# DADOS ESTÁTICOS DO GTFS
agency <- tibble(
  agency_id       = "UFBA",
  agency_name     = "BUZUFBA - Universidade Federal da Bahia",
  agency_url      = "https://ufba.br/",
  agency_timezone = "America/Bahia",
  agency_lang     = "pt",
)

routes <- tibble(
  route_id         = c("B1", "B2", "B3", "B4", "B5"),
  agency_id        = "UFBA",
  route_short_name = route_id,
  route_long_name  = c(
    "Ondina - Canela - São Lázaro (Circular)",
    "Ondina - Canela - Vitória - Graça Longa (Circular)",
    "Ondina - Garibaldi - Canela - Av. 7 (Circular)",
    "Ondina - Piedade - Vitória - Graça (Circular)",
    "Federação - Ondina - Canela - Vitória (Circular)"
  ),
  route_type       = 3,
  route_color      = "00539F",
  route_text_color = "FFFFFF"
)

stops <- tribble(
  ~stop_id,          ~stop_name,                                              ~stop_lat, ~stop_lon,
  "SAO_LAZARO",      "Pt. Estacionamento São Lázaro",                         -13.004763, -38.512352,
  "POLITECNICA",     "Pt. Politécnica",                                       -12.999620, -38.510380,
  "ARQUITETURA",     "Pt. Arquitetura",                                       -12.997114, -38.508632,
  "RESIDENCIA5",     "Pt. Residência 5",                                      -12.999700, -38.504900,
  "CANELA_ICS",      "Campus Vale do Canela (Entrada ICS)",                   -12.994777, -38.520578,
  "ISC_CANELA",      "ISC Canela",                                            -12.994506, -38.521907,
  "ODONTO",          "P. Odontologia",                                        -12.994676, -38.522860,
  "REITORIA",        "P. Reitoria",                                           -12.992349, -38.520642,
  "CRECHE",          "P. Creche Canela",                                      -12.994740, -38.517485,
  "GRACA_R2",        "P. Graça R2 (Delicia)",                                 -12.997625, -38.519136,
  "DIREITO",         "Faculdade de Direito",                                  -12.996277, -38.521557,
  "PAF1_MAT",        "Pt. Estacionamento (PAF.1 Matemática)",                 -13.002294, -38.506550,
  "AV_7",            "Avenida 7 de Setembro",                                 -12.983191, -38.515456,
  "BELAS_ARTES",     "Belas Artes",                                           -12.991313, -38.521232,
  "RESIDENCIA1",     "Residência I - Vitória",                                -12.994064, -38.526495,
  "GEOCIENCIAS",     "Pt. Instituto de Geociências",                          -12.998493, -38.506518,
  "FACOM",           "Pt. Facom",                                             -13.001377, -38.509747,
  "PORTARIA",        "Pt. Portaria Principal",                                -13.006067, -38.510318,
  "FACED",           "Faculdade de Educação",                                 -12.995104, -38.519310,
  "PROAE",           "Pró-Reitoria (PROAE)",                                  -12.997461, -38.509326,
  "CENTRO_ESPORTES", "Centro Esportes da UFBA",                               -13.009432, -38.513788
)

calendar <- tibble(
  service_id = c("DIAS_UTEIS", "SABADO"), 
  monday = c(1, 0), tuesday = c(1, 0), wednesday = c(1, 0), 
  thursday = c(1, 0), friday = c(1, 0), saturday = c(0, 1), sunday = c(0, 0), 
  start_date = "20260101", end_date = "20261231"
)


# SEQUÊNCIAS CIRCULARES E HORÁRIOS DAS ROTAS
# Sequências Base (fundidas em uma única rota Circular por linha)
seqs <- list(
  B1 = unir_circular(
    c("SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "RESIDENCIA5", "CANELA_ICS", "ISC_CANELA", "ODONTO"),
    c("REITORIA", "CRECHE", "GRACA_R2", "DIREITO", "FACED", "PAF1_MAT", "PROAE", "POLITECNICA", "SAO_LAZARO")
  ),
  B2 = unir_circular(
    c("PAF1_MAT", "RESIDENCIA5", "PROAE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "CRECHE", "REITORIA", "BELAS_ARTES", "REITORIA", "CRECHE", "GRACA_R2", "RESIDENCIA1"),
    c("RESIDENCIA1", "DIREITO", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "RESIDENCIA5", "GEOCIENCIAS", "PAF1_MAT")
  ),
  B3 = unir_circular(
    c("PAF1_MAT", "RESIDENCIA5", "CANELA_ICS", "AV_7", "BELAS_ARTES"),
    c("REITORIA", "CRECHE", "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT")
  ),
  B4 = unir_circular(
    c("PAF1_MAT", "RESIDENCIA5", "PROAE", "POLITECNICA", "CRECHE", "REITORIA", "AV_7"),
    c("AV_7", "RESIDENCIA1", "GRACA_R2", "POLITECNICA", "SAO_LAZARO", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT")
  ),
  B5 = unir_circular(
    c("GEOCIENCIAS", "FACOM", "PORTARIA", "CENTRO_ESPORTES", "PAF1_MAT", "PROAE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "CRECHE", "REITORIA"),
    c("RESIDENCIA1", "DIREITO", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT", "PORTARIA", "FACOM", "GEOCIENCIAS")
  )
)

# Horários Base e Limites
h_base <- list(
  B1 = list(full = c("06:10","07:40","09:10","10:40","12:10","13:40","15:10","16:40","18:10","19:40","21:10","22:40"), limite_noite = "19:40", limite_sab = "13:40"),
  B2 = list(full = c("06:00","07:40","09:20","11:00","12:40","14:20","16:00","17:40","19:20","21:00","22:40"), limite_noite = "19:20", limite_sab = "14:20"),
  B3 = list(full = c("06:30","07:40","08:50","10:00","11:10","12:20","13:30","14:40","15:50","17:00","18:10","19:20","20:30","21:40","22:50"), limite_noite = "18:10", limite_sab = "14:40"),
  B4 = list(full = c("06:00","07:35","09:10","10:45","12:20","13:55","15:30","17:05","18:40","20:15","21:50"), limite_noite = "18:40", limite_sab = "13:55"),
  B5 = list(full = c("06:40","08:15","09:50","11:25","13:00","14:35","16:10","17:45","19:20","20:55","22:30"), limite_noite = "17:45", limite_sab = "14:35")
)

# GERADOR DE CONFIGURAÇÕES
config_rotas <- map_dfr(names(h_base), function(rota) {
  h <- h_base[[rota]]
  s_circular <- seqs[[rota]]
  
  horarios_dia <- h$full[h$full < h$limite_noite]
  horarios_noite <- h$full[h$full >= h$limite_noite]
  horarios_sab <- h$full[1:which(h$full == h$limite_sab)]
  
  tribble(
    ~route_id, ~service_id, ~direction_id, ~horarios,      ~sequencia_stops,                ~shape_id,
    rota,      "DIAS_UTEIS", 0,             horarios_dia,   s_circular,                      paste0("SHP_", rota, "_CIRCULAR"),
    rota,      "DIAS_UTEIS", 0,             horarios_noite, remover_sao_lazaro(s_circular),  paste0("SHP_", rota, "_CIRCULAR_N"),
    rota,      "SABADO",     0,             horarios_sab,   s_circular,                      paste0("SHP_", rota, "_CIRCULAR")
  )
})

# GERAÇÃO AUTOMATIZADA DOS DADOS
# Gera Shapes Únicos
shapes_unicos <- config_rotas %>% distinct(shape_id, sequencia_stops)

shapes_final <- map2_dfr(
  shapes_unicos$shape_id, 
  shapes_unicos$sequencia_stops, 
  ~gerar_shape_via_api(.x, .y, stops)
)

# Gera Viagens (Trips) e Horários (Stop Times)
jobs <- pmap(config_rotas, gerar_dados_rota) %>% purrr::compact()

trips_final <- map_dfr(jobs, "trips")
stop_times_final <- map_dfr(jobs, "stop_times")

# Monta as informações de Feed
feed_info <- tibble(
  feed_publisher_name = agency$agency_name,
  feed_publisher_url  = agency$agency_url,
  feed_lang           = agency$agency_lang,
  feed_start_date     = calendar$start_date[1],
  feed_end_date       = calendar$end_date[1],
  feed_version        = paste0("UFBA_CIRCULAR_", Sys.Date())
)

# MONTAGEM E VALIDAÇÃO DO OBJETO GTFS
gtfs <- list(
  agency     = as.data.table(agency),
  routes     = as.data.table(routes),
  trips      = as.data.table(trips_final),
  stop_times = as.data.table(stop_times_final),
  stops      = as.data.table(stops),
  calendar   = as.data.table(calendar),
  shapes     = as.data.table(shapes_final),
  feed_info  = as.data.table(feed_info)
)

class(gtfs) <- c("dt_gtfs", "gtfs")

gtfs$calendar[, start_date := as.Date(as.character(start_date), format = "%Y%m%d")]
gtfs$calendar[, end_date   := as.Date(as.character(end_date), format = "%Y%m%d")]
gtfs$feed_info[, feed_start_date := as.Date(as.character(feed_start_date), format = "%Y%m%d")]
gtfs$feed_info[, feed_end_date := as.Date(as.character(feed_end_date), format = "%Y%m%d")]

invalid_stops <- setdiff(gtfs$stop_times$stop_id, gtfs$stops$stop_id)
if(length(invalid_stops) > 0) {
  warning("ERRO CRÍTICO: Stop_ids usados mas não definidos em 'stops': ", paste(invalid_stops, collapse=", "))
} else {
  message("Validação de integridade: OK. Todas as paradas cadastradas.")
}

message(sprintf("Total de viagens circulares: %d\nTotal de paradas programadas: %d", 
                nrow(gtfs$trips), nrow(gtfs$stop_times)))

# EXPORTAÇÃO E VISUALIZAÇÃO INTERATIVA
## Valida o GTFS do BuzUFBA
validator_path <- download_validator(tempdir())

gtfstools::validate_gtfs(gtfs, "validation_result", validator_path)

if (!dir.exists("data")) dir.create("data")
write_gtfs(gtfs, "data/buzufba_gtfs.zip")
message("GTFS salvo em 'data/buzufba_gtfs.zip'.")

investigar_rota <- function(gtfs_data) {
  if(is.null(gtfs_data$shapes) || nrow(gtfs_data$shapes) == 0) return(message("A tabela 'shapes' está vazia."))
  
  shapes_disp <- unique(gtfs_data$shapes$shape_id)
  print(data.frame(Indice = seq_along(shapes_disp), ID = shapes_disp))
  
  n <- as.numeric(readline(prompt = "\nDigite o NÚMERO do shape para visualizar (ou 0 para sair): "))
  if (is.na(n) || n == 0 || n > length(shapes_disp)) return(message("Cancelado."))
  
  shp <- shapes_disp[n]
  mini_gtfs <- list(shapes = gtfs_data$shapes[shape_id == shp])
  class(mini_gtfs) <- c("dt_gtfs", "gtfs")
  
  linha_sf <- tryCatch(convert_shapes_to_sf(mini_gtfs), error = function(e) NULL)
  if (is.null(linha_sf)) return(message("Erro ao converter Shape."))
  
  trip_exemplo <- gtfs_data$trips[shape_id == shp]$trip_id[1]
  
  m1 <- mapview(linha_sf, color = "#00539F", lwd = 5, layer.name = "Trajeto Circular")
  
  if(!is.na(trip_exemplo)) {
    stops_da_rota <- gtfs_data$stop_times[trip_id == trip_exemplo] %>% 
      left_join(gtfs_data$stops, by = "stop_id") %>%
      st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
    
    m2 <- mapview(stops_da_rota, col.regions = "black", color = "white", cex = 6, 
                  label = paste(stops_da_rota$stop_sequence, "-", stops_da_rota$stop_name), layer.name = "Paradas")
    print(m1 + m2)
  } else {
    print(m1)
  }
}

investigar_rota(gtfs)
