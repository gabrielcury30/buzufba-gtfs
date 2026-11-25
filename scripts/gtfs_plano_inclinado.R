# Gera um GTFS de cenário para o plano inclinado entre a Politécnica e Ondina.
# Preencha as coordenadas dos terminais e a sequência do shape após editar o
# trajeto no QGIS. As coordenadas devem estar em WGS84 (EPSG:4326).

localizar_diretorio_projeto <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)

  if (length(argumento_arquivo) > 0) {
    caminho_script <- normalizePath(
      sub("^--file=", "", argumento_arquivo[1]),
      mustWork = TRUE
    )
    return(normalizePath(file.path(dirname(caminho_script), ".."), mustWork = TRUE))
  }

  diretorio <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(diretorio, "buzufba-gtfs.Rproj"))) {
    diretorio_pai <- dirname(diretorio)
    if (diretorio_pai == diretorio) {
      stop("Não foi possível localizar a raiz do projeto.", call. = FALSE)
    }
    diretorio <- diretorio_pai
  }
  diretorio
}

# -----------------------------------------------------------------------------
# Configuração espacial: substitua NA_real_ pelas coordenadas dos terminais e
# cole abaixo os vértices do shape exportados do QGIS, na ordem Politécnica →
# Ondina. O retorno usa a mesma geometria em ordem inversa.
# -----------------------------------------------------------------------------
paradas <- data.frame(
  stop_id = c("T_POLI", "T_ONDI"),
  stop_name = c("Terminal Politécnica", "Terminal Ondina"),
  stop_lat = c(-12.999660, -13.000632),
  stop_lon = c(-38.509499, -38.508974),
  stringsAsFactors = FALSE
)

shape_id <- "PLANO_INCLINADO_UFBA"
shape_ida <- data.frame(
  shape_pt_lat = c(-12.99971342, -12.99989183, -13.00018100, -13.00042613, -13.00065868),
  shape_pt_lon = c(-38.50956185, -38.50945563, -38.50928289, -38.50914029, -38.50900175),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Operação e cenários
# -----------------------------------------------------------------------------
tempo_viagem_segundos <- 51L
tempo_terminal_segundos <- 30L

# Cenário moderado ativo: 30 s em cada terminal para embarque/desembarque,
# fechamento e reversão. O intervalo na mesma direção soma ida, volta e as
# duas permanências nos terminais.
intervalo_saida_segundos <- 2L * tempo_viagem_segundos + 2L * tempo_terminal_segundos

# Cenário ruim: 60 s em cada terminal (intervalo de 222 s = 3 min 42 s).
# tempo_terminal_segundos <- 60L
# intervalo_saida_segundos <- 2L * tempo_viagem_segundos + 2L * tempo_terminal_segundos
# Cenário bom: 20 s em cada terminal (intervalo de 142 s = 2 min 22 s).
# tempo_terminal_segundos <- 20L
# intervalo_saida_segundos <- 2L * tempo_viagem_segundos + 2L * tempo_terminal_segundos

inicio_dias_uteis <- "06:30:00"
fim_dias_uteis <- "22:40:00"
inicio_sabado <- "06:30:00"
fim_sabado <- "15:00:00"

# Janela de validade do calendário GTFS.
data_inicio_feed <- as.Date("2026-01-01")
data_fim_feed <- as.Date("2026-12-31")

# -----------------------------------------------------------------------------
# Geração do feed
# -----------------------------------------------------------------------------
validar_configuracao <- function() {
  if (anyNA(paradas$stop_lat) || anyNA(paradas$stop_lon)) {
    stop(
      "Preencha stop_lat e stop_lon dos dois terminais com coordenadas WGS84.",
      call. = FALSE
    )
  }
  if (anyNA(shape_ida$shape_pt_lat) || anyNA(shape_ida$shape_pt_lon)) {
    stop(
      "Cole em shape_ida os vértices do trajeto exportados do QGIS.",
      call. = FALSE
    )
  }
  if (nrow(shape_ida) < 2L) {
    stop("O shape precisa ter pelo menos dois pontos.", call. = FALSE)
  }
  if (intervalo_saida_segundos < 2L * (tempo_viagem_segundos + tempo_terminal_segundos)) {
    stop(
      "Com uma cabine, o intervalo precisa comportar a ida, a volta e os tempos nos terminais.",
      call. = FALSE
    )
  }
  if (intervalo_saida_segundos %% 1L != 0L) {
    stop("Use um intervalo de partidas em segundos inteiros.", call. = FALSE)
  }
}

hora_para_segundos <- function(hora) {
  partes <- as.integer(strsplit(hora, ":", fixed = TRUE)[[1]])
  sum(partes * c(3600L, 60L, 1L))
}

formatar_hora_gtfs <- function(segundos) {
  sprintf("%02d:%02d:%02d", segundos %/% 3600L,
    (segundos %% 3600L) %/% 60L, segundos %% 60L
  )
}

criar_viagens <- function(service_id, inicio, fim, partida_inicial,
                          incremento, sentido, paradas_sentido, shape_sentido) {
  inicio_segundos <- hora_para_segundos(inicio)
  fim_segundos <- hora_para_segundos(fim)
  primeira_partida <- inicio_segundos + partida_inicial
  partidas <- seq.int(primeira_partida, fim_segundos - tempo_viagem_segundos,
    by = incremento
  )
  if (length(partidas) == 0L) {
    return(list(trips = data.frame(), stop_times = data.frame()))
  }

  trip_ids <- sprintf("PI_%s_%s", service_id, partidas)
  chegada <- partidas + tempo_viagem_segundos
  trips <- data.frame(
    route_id = "PLANO_INCLINADO",
    service_id = service_id,
    trip_id = trip_ids,
    direction_id = sentido,
    shape_id = shape_sentido,
    stringsAsFactors = FALSE
  )
  stop_times <- data.frame(
    trip_id = rep(trip_ids, each = 2L),
    arrival_time = as.vector(rbind(
      formatar_hora_gtfs(partidas), formatar_hora_gtfs(chegada)
    )),
    departure_time = as.vector(rbind(
      formatar_hora_gtfs(partidas), formatar_hora_gtfs(chegada)
    )),
    stop_id = rep(paradas_sentido, times = length(partidas)),
    stop_sequence = rep(1:2, times = length(partidas)),
    stringsAsFactors = FALSE
  )
  list(trips = trips, stop_times = stop_times)
}

gerar_gtfs_plano_inclinado <- function() {
  validar_configuracao()

  pacotes <- c("data.table", "gtfstools")
  ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ausentes) > 0L) {
    stop("Instale os pacotes R necessários: ", paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  shape_ida_feed <- data.frame(
    shape_id = shape_id,
    shape_pt_lat = shape_ida$shape_pt_lat,
    shape_pt_lon = shape_ida$shape_pt_lon,
    shape_pt_sequence = seq_len(nrow(shape_ida)),
    stringsAsFactors = FALSE
  )
  shape_volta_feed <- shape_ida_feed[nrow(shape_ida_feed):1, , drop = FALSE]
  shape_volta_feed$shape_id <- paste0(shape_id, "_VOLTA")
  shape_volta_feed$shape_pt_sequence <- seq_len(nrow(shape_volta_feed))
  shapes <- rbind(shape_ida_feed, shape_volta_feed)

  viagens <- list(
    criar_viagens("DIAS_UTEIS", inicio_dias_uteis, fim_dias_uteis,
      0L, intervalo_saida_segundos, 0L,
      paradas$stop_id, shape_id),
    criar_viagens("DIAS_UTEIS", inicio_dias_uteis, fim_dias_uteis,
      tempo_viagem_segundos + tempo_terminal_segundos,
      intervalo_saida_segundos, 1L,
      rev(paradas$stop_id), paste0(shape_id, "_VOLTA")),
    criar_viagens("SABADO", inicio_sabado, fim_sabado,
      0L, intervalo_saida_segundos, 0L,
      paradas$stop_id, shape_id),
    criar_viagens("SABADO", inicio_sabado, fim_sabado,
      tempo_viagem_segundos + tempo_terminal_segundos,
      intervalo_saida_segundos, 1L,
      rev(paradas$stop_id), paste0(shape_id, "_VOLTA"))
  )
  trips <- do.call(rbind, lapply(viagens, `[[`, "trips"))
  stop_times <- do.call(rbind, lapply(viagens, `[[`, "stop_times"))
  paradas_nao_cadastradas <- setdiff(unique(stop_times$stop_id), paradas$stop_id)
  if (length(paradas_nao_cadastradas) > 0L) {
    stop(
      "stop_times referencia stop_id ausente em stops: ",
      paste(paradas_nao_cadastradas, collapse = ", "),
      call. = FALSE
    )
  }

  data_inicio <- format(data_inicio_feed, "%Y%m%d")
  data_fim <- format(data_fim_feed, "%Y%m%d")
  calendar <- data.frame(
    service_id = c("DIAS_UTEIS", "SABADO"),
    monday = c(1L, 0L), tuesday = c(1L, 0L), wednesday = c(1L, 0L),
    thursday = c(1L, 0L), friday = c(1L, 0L), saturday = c(0L, 1L),
    sunday = c(0L, 0L), start_date = c(data_inicio, data_inicio),
    end_date = c(data_fim, data_fim), stringsAsFactors = FALSE
  )
  agency <- data.frame(
    agency_id = "UFBA", agency_name = "Universidade Federal da Bahia",
    agency_url = "https://ufba.br/", agency_timezone = "America/Bahia",
    agency_lang = "pt", stringsAsFactors = FALSE
  )
  routes <- data.frame(
    route_id = "PLANO_INCLINADO", agency_id = "UFBA",
    route_short_name = "Plano Inclinado",
    route_long_name = "Politécnica - Ondina",
    route_type = 7L, stringsAsFactors = FALSE
  )
  feed_info <- data.frame(
    feed_publisher_name = "Universidade Federal da Bahia",
    feed_publisher_url = "https://ufba.br/", feed_lang = "pt",
    feed_start_date = data_inicio, feed_end_date = data_fim,
    feed_version = paste0("plano_inclinado_", format(Sys.Date(), "%Y%m%d")),
    stringsAsFactors = FALSE
  )

  feed <- lapply(list(
    agency = agency, routes = routes, trips = trips, stop_times = stop_times,
    stops = paradas, calendar = calendar, shapes = shapes, feed_info = feed_info
  ), data.table::as.data.table)
  class(feed) <- c("tidygtfs", "dt_gtfs", "gtfs")
  feed$calendar[, c("start_date", "end_date") := lapply(.SD, as.Date,
    format = "%Y%m%d"
  ), .SDcols = c("start_date", "end_date")]
  feed$feed_info[, c("feed_start_date", "feed_end_date") := lapply(.SD, as.Date,
    format = "%Y%m%d"
  ), .SDcols = c("feed_start_date", "feed_end_date")]

  diretorio_projeto <- localizar_diretorio_projeto()
  diretorio_saida <- file.path(diretorio_projeto, "data", "processed")
  dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
  caminho_saida <- file.path(diretorio_saida, "plano_inclinado_ufba_gtfs.zip")
  gtfstools::write_gtfs(feed, caminho_saida)
  message("GTFS salvo em: ", caminho_saida)
  invisible(feed)
}

gerar_gtfs_plano_inclinado()
