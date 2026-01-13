library(gtfstools)
library(tidyverse)
library(lubridate)
library(data.table)

options(scipen = 999)

agency <- tibble::tibble(
  agency_id       = "UFBA",
  agency_name     = "BUZUFBA - Universidade Federal da Bahia",
  agency_url      = "https://ufba.br/",
  agency_timezone = "America/Bahia",
  agency_lang     = "pt",
  agency_phone    = NA_character_
)

routes <- tibble::tibble(
  route_id         = c("B1", "B2", "B3", "B4", "B5"),
  agency_id        = "UFBA",
  route_short_name = route_id,
  route_long_name  = c(
    "Ondina - Canela - São Lázaro",
    "Ondina - Canela - Vitória - Graça (Rota Longa)",
    "Ondina - Garibaldi - Canela - Av. 7 de Setembro",
    "Ondina - Piedade - Vitória - Graça",
    "Federação - Ondina - Canela - Vitória"
  ),
  route_type       = 3,
  route_color      = "00539F",
  route_text_color = "FFFFFF"
)

stops <- tibble::tribble(
  ~stop_id,          ~stop_name,                                              ~stop_lat, ~stop_lon,
  "SAO_LAZARO",      "Pt. Estacionamento São Lázaro",                         -13.004763, -38.512352,
  "POLITECNICA",     "Pt. Politécnica",                                       -12.99962, -38.51038,
  "ARQUITETURA",     "Pt. Arquitetura",                                       -12.99711353291119, -38.50863244491834,
  "RESIDENCIA5",     "Pt. Residência 5",                                      -12.9997, -38.5049,
  "CANELA_ICS",      "Campus Vale do Canela (Entrada ICS)",                   -12.99477704431149, -38.5205779814133,
  "ISC_CANELA",      "ISC Canela",                                            -12.99450640990219, -38.52190668253578,
  "ODONTO",          "P. Odontologia",                                        -12.994676023446983, -38.52285968253581,
  "REITORIA",        "P. Reitoria",                                           -12.992349341863417, -38.5206423647449,
  "CRECHE",          "P. Creche Canela",                                      -12.994739537533313, -38.51748460427259,
  "GRACA_R2",        "P. Graça R2 (Delicia)",                                 -12.997625059125205, -38.51913632820645,
  "DIREITO",         "Faculdade de Direito",                                  -12.99627718315727, -38.521557000326695,
  "PAF1_MAT",        "Pt. Estacionamento (PAF.1 Matemática)",                 -13.002294104133286, -38.5065498717973,
  "AV_7",            "Avenida 7 de Setembro",                                 -12.983190635517303, -38.51545636474487,
  "BELAS_ARTES",     "Belas Artes",                                           -12.991313182321539, -38.52123172916305,
  "RESIDENCIA1",     "Residência I - Vitória",                                -12.994064342206292, -38.52649500032668,
  "GEOCIENCIAS",     "Pt. Instituto de Geociências",                          -12.998493246598926, -38.50651847006711,
  "FACOM",           "Pt. Facom",                                             -13.00137695711786, -38.5097473647449,
  "PORTARIA",        "Pt. Portaria Principal",                                -13.006066958006857, -38.51031836474489,
  "FACED",           "Faculdade de Educação",                                 -12.995104376685708, -38.51931037628446,
  "PROAE",           "Pró-Reitoria (PROAE)",                                  -12.99746056986614, -38.509326364744844,
  "CENTRO_ESPORTES", "Centro Esportes da UFBA",                               -13.009431515646837, -38.51378775022912
)

calendar <- tibble::tibble(
  service_id = c("DIAS_UTEIS", "SABADO"), 
  monday = c(1, 0), 
  tuesday = c(1, 0), 
  wednesday = c(1, 0), 
  thursday = c(1, 0), 
  friday = c(1, 0), 
  saturday = c(0, 1), 
  sunday = c(0, 0), 
  start_date = "20260101", 
  end_date = "20261231")

gerar_dados_rota <- function(route_id, service_id, direction_id, horarios, sequencia_stops) {
  
  if(length(horarios) == 0) return(NULL)
  
  mins_inter_stop <- 3 
  
  trips <- tibble::tibble(
    route_id = route_id,
    service_id = service_id,
    trip_id = paste(route_id, service_id, direction_id, gsub(":", "", horarios), sep = "_"),
    direction_id = direction_id
  )
  
  stop_times <- trips %>%
    dplyr::select(trip_id) %>%
    dplyr::mutate(
      stop_id = list(sequencia_stops),
      stop_sequence = list(seq_along(sequencia_stops)),
      offset_minutos = list((seq_along(sequencia_stops) - 1) * mins_inter_stop)
    ) %>%
    tidyr::unnest(cols = c(stop_id, stop_sequence, offset_minutos))
  
  base_times <- tibble::tibble(
    trip_id = trips$trip_id,
    hora_base = lubridate::hm(horarios)
  )
  
  stop_times <- stop_times %>%
    dplyr::left_join(base_times, by = "trip_id") %>%
    dplyr::mutate(
      arrival_time_obj = hora_base + lubridate::minutes(offset_minutos),
      arrival_time = sprintf("%02d:%02d:00", hour(arrival_time_obj), minute(arrival_time_obj)),
      departure_time = arrival_time
    ) %>%
    dplyr::select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)
  
  return(list(trips = trips, stop_times = stop_times))
}

remover_sao_lazaro <- function(seq) { return(setdiff(seq, "SAO_LAZARO")) }

seq_b1_ida       <- c("SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "RESIDENCIA5", "CANELA_ICS", "ISC_CANELA", "ODONTO")
seq_b1_ida_noite <- remover_sao_lazaro(seq_b1_ida)
seq_b1_volta     <- c("REITORIA", "CRECHE", "GRACA_R2", "DIREITO", "FACED", "PAF1_MAT", "PROAE", "POLITECNICA", "SAO_LAZARO")
seq_b1_volta_noite <- remover_sao_lazaro(seq_b1_volta)

seq_b2_ida       <- c("PAF1_MAT", "RESIDENCIA5", "PROAE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "CRECHE", "REITORIA", "BELAS_ARTES", "REITORIA", "CRECHE", "GRACA_R2", "RESIDENCIA1")
seq_b2_ida_noite <- remover_sao_lazaro(seq_b2_ida)
seq_b2_volta     <- c("RESIDENCIA1", "DIREITO", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "RESIDENCIA5", "GEOCIENCIAS", "PAF1_MAT")
seq_b2_volta_noite <- remover_sao_lazaro(seq_b2_volta)

seq_b3_ida       <- c("PAF1_MAT", "RESIDENCIA5", "CANELA_ICS", "AV_7", "BELAS_ARTES")
seq_b3_ida_noite <- remover_sao_lazaro(seq_b3_ida)
seq_b3_volta     <- c("REITORIA", "CRECHE", "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT")
seq_b3_volta_noite <- remover_sao_lazaro(seq_b3_volta)

seq_b4_ida       <- c("PAF1_MAT", "RESIDENCIA5", "PROAE", "POLITECNICA", "CRECHE", "REITORIA", "AV_7")
seq_b4_ida_noite <- remover_sao_lazaro(seq_b4_ida)
seq_b4_volta     <- c("AV_7", "RESIDENCIA1", "GRACA_R2", "POLITECNICA", "SAO_LAZARO", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT")
seq_b4_volta_noite <- remover_sao_lazaro(seq_b4_volta)

seq_b5_ida       <- c("GEOCIENCIAS", "FACOM", "PORTARIA", "CENTRO_ESPORTES", "PAF1_MAT", "PROAE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "CRECHE", "REITORIA")
seq_b5_ida_noite <- remover_sao_lazaro(seq_b5_ida)
seq_b5_volta     <- c("RESIDENCIA1", "DIREITO", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT", "PORTARIA", "FACOM", "GEOCIENCIAS")
seq_b5_volta_noite <- remover_sao_lazaro(seq_b5_volta)

h_b1_full  <- c("06:10", "07:40", "09:10", "10:40", "12:10", "13:40", "15:10", "16:40", "18:10", "19:40", "21:10", "22:40")
h_b1_dia   <- h_b1_full[h_b1_full < "19:40"]
h_b1_noite <- h_b1_full[h_b1_full >= "19:40"]
h_b1_sab   <- c("06:10", "07:40", "09:10", "10:40", "12:10", "13:40")

h_b2_full  <- c("06:00", "07:40", "09:20", "11:00", "12:40", "14:20", "16:00", "17:40", "19:20", "21:00", "22:40")
h_b2_dia   <- h_b2_full[h_b2_full < "19:20"]
h_b2_noite <- h_b2_full[h_b2_full >= "19:20"]
h_b2_sab   <- h_b2_full[1:which(h_b2_full == "14:20")]

h_b3_full  <- c("06:30", "07:40", "08:50", "10:00", "11:10", "12:20", "13:30", "14:40", "15:50", "17:00", "18:10", "19:20", "20:30", "21:40", "22:50")
h_b3_dia   <- h_b3_full[h_b3_full < "18:10"]
h_b3_noite <- h_b3_full[h_b3_full >= "18:10"]
h_b3_sab   <- h_b3_full[1:which(h_b3_full == "14:40")]

h_b4_full  <- c("06:00", "07:35", "09:10", "10:45", "12:20", "13:55", "15:30", "17:05", "18:40", "20:15", "21:50")
h_b4_dia   <- h_b4_full[h_b4_full < "18:40"]
h_b4_noite <- h_b4_full[h_b4_full >= "18:40"]
h_b4_sab   <- h_b4_full[1:which(h_b4_full == "13:55")]

h_b5_full  <- c("06:40", "08:15", "09:50", "11:25", "13:00", "14:35", "16:10", "17:45", "19:20", "20:55", "22:30")
h_b5_dia   <- h_b5_full[h_b5_full < "17:45"]
h_b5_noite <- h_b5_full[h_b5_full >= "17:45"]
h_b5_sab   <- h_b5_full[1:which(h_b5_full == "14:35")]

job_list <- list(
  gerar_dados_rota("B1", "DIAS_UTEIS", 0, h_b1_dia,   seq_b1_ida),
  gerar_dados_rota("B1", "DIAS_UTEIS", 0, h_b1_noite, seq_b1_ida_noite),
  gerar_dados_rota("B1", "DIAS_UTEIS", 1, h_b1_dia,   seq_b1_volta),
  gerar_dados_rota("B1", "DIAS_UTEIS", 1, h_b1_noite, seq_b1_volta_noite),
  gerar_dados_rota("B1", "SABADO",     0, h_b1_sab,   seq_b1_ida),
  gerar_dados_rota("B1", "SABADO",     1, h_b1_sab,   seq_b1_volta),
  
  gerar_dados_rota("B2", "DIAS_UTEIS", 0, h_b2_dia,   seq_b2_ida),
  gerar_dados_rota("B2", "DIAS_UTEIS", 0, h_b2_noite, seq_b2_ida_noite), 
  gerar_dados_rota("B2", "DIAS_UTEIS", 1, h_b2_dia,   seq_b2_volta),
  gerar_dados_rota("B2", "DIAS_UTEIS", 1, h_b2_noite, seq_b2_volta_noite), 
  gerar_dados_rota("B2", "SABADO",     0, h_b2_sab,   seq_b2_ida),
  gerar_dados_rota("B2", "SABADO",     1, h_b2_sab,   seq_b2_volta),
  
  gerar_dados_rota("B3", "DIAS_UTEIS", 0, h_b3_dia,   seq_b3_ida),
  gerar_dados_rota("B3", "DIAS_UTEIS", 0, h_b3_noite, seq_b3_ida_noite), 
  gerar_dados_rota("B3", "DIAS_UTEIS", 1, h_b3_dia,   seq_b3_volta),
  gerar_dados_rota("B3", "DIAS_UTEIS", 1, h_b3_noite, seq_b3_volta_noite), 
  gerar_dados_rota("B3", "SABADO",     0, h_b3_sab,   seq_b3_ida),
  gerar_dados_rota("B3", "SABADO",     1, h_b3_sab,   seq_b3_volta),
  
  gerar_dados_rota("B4", "DIAS_UTEIS", 0, h_b4_dia,   seq_b4_ida),
  gerar_dados_rota("B4", "DIAS_UTEIS", 0, h_b4_noite, seq_b4_ida_noite), 
  gerar_dados_rota("B4", "DIAS_UTEIS", 1, h_b4_dia,   seq_b4_volta),
  gerar_dados_rota("B4", "DIAS_UTEIS", 1, h_b4_noite, seq_b4_volta_noite), 
  gerar_dados_rota("B4", "SABADO",     0, h_b4_sab,   seq_b4_ida),
  gerar_dados_rota("B4", "SABADO",     1, h_b4_sab,   seq_b4_volta),
  
  gerar_dados_rota("B5", "DIAS_UTEIS", 0, h_b5_dia,   seq_b5_ida),
  gerar_dados_rota("B5", "DIAS_UTEIS", 0, h_b5_noite, seq_b5_ida_noite), 
  gerar_dados_rota("B5", "DIAS_UTEIS", 1, h_b5_dia,   seq_b5_volta),
  gerar_dados_rota("B5", "DIAS_UTEIS", 1, h_b5_noite, seq_b5_volta_noite), 
  gerar_dados_rota("B5", "SABADO",     0, h_b5_sab,   seq_b5_ida),
  gerar_dados_rota("B5", "SABADO",     1, h_b5_sab,   seq_b5_volta)
)

job_list <- purrr::compact(job_list)

gtfs <- list(
  agency     = as.data.table(agency),
  routes     = as.data.table(routes),
  trips      = as.data.table(purrr::map_dfr(job_list, "trips")),
  stop_times = as.data.table(purrr::map_dfr(job_list, "stop_times")),
  stops      = as.data.table(stops),
  calendar   = as.data.table(calendar)
)

class(gtfs) <- c("dt_gtfs", "gtfs")

invalid_stops <- setdiff(gtfs$stop_times$stop_id, gtfs$stops$stop_id)

if(length(invalid_stops) > 0) {
  warning("ERRO CRÍTICO: Stop_ids usados nas rotas mas não definidos em 'stops': ", paste(invalid_stops, collapse=", "))
} else {
  message("Validação de integridade: OK. Todas as paradas das rotas estão cadastradas.")
}

message("Total de viagens geradas: ", nrow(gtfs$trips))
message("Total de paradas programadas: ", nrow(gtfs$stop_times))

gtfs$calendar[, start_date := as.Date(as.character(start_date), format = "%Y%m%d")]
gtfs$calendar[, end_date   := as.Date(as.character(end_date), format = "%Y%m%d")]

is.list(gtfs)
names(gtfs)
sapply(gtfs, class)

write_gtfs(gtfs, "buzufba_gtfs.zip")
