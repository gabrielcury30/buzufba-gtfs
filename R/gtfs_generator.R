# Orquestração da geração e exportação do feed GTFS BUZUFBA.

encontrar_shapes_manuais <- function(diretorio_projeto) {
  candidatos <- file.path(diretorio_projeto, c("data/raw/shapes_rotas_buzufba.gpkg"))
  encontrados <- candidatos[file.exists(candidatos)]
  if (length(encontrados) == 0) {
    return(NULL)
  }
  encontrados[1]
}

validar_dependencias_gtfs <- function() {
  pacotes <- c(
    "data.table", "dplyr", "gtfstools", "httr2", "jsonlite", "purrr",
    "sf", "tibble", "units"
  )
  ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ausentes) > 0) {
    stop("Instale os pacotes R necessários: ", paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }
}

gerar_shapes_e_tempos <- function(
  configuracoes_rotas, paradas, urls_valhalla, caminho_shapes_manuais,
  crs_metrico, margem_parada_min
) {
  shapes_unicos <- dplyr::distinct(configuracoes_rotas, shape_id, sequencia_stops)
  if (!is.null(caminho_shapes_manuais)) {
    message(sprintf("Modo A: shapes manuais (%s) + tempos do Valhalla.", caminho_shapes_manuais))
    shapes_manuais <- carregar_shapes_manuais(caminho_shapes_manuais)
    faltando <- shapes_unicos$shape_id[!(shapes_unicos$shape_id %in% names(shapes_manuais) |
      sub("_N$", "", shapes_unicos$shape_id) %in% names(shapes_manuais))]
    if (length(faltando) > 0) {
      stop("Faltam shapes no arquivo manual: ", paste(faltando, collapse = ", "),
        call. = FALSE
      )
    }
    resultados <- purrr::map2(shapes_unicos$shape_id, shapes_unicos$sequencia_stops,
      processar_rota_manual,
      paradas = paradas, shapes_manuais = shapes_manuais,
      url_trace = urls_valhalla$trace, url_route = urls_valhalla$route, crs_metrico = crs_metrico,
      margem_parada_min = margem_parada_min
    )
  } else {
    message("Modo B: shapes manuais não encontrados. Valhalla gera shapes e tempos (perfil bus).")
    resultados <- purrr::map2(shapes_unicos$shape_id, shapes_unicos$sequencia_stops,
      processar_rota_valhalla,
      paradas = paradas, url_route = urls_valhalla$route,
      margem_parada_min = margem_parada_min
    )
  }
  list(shapes = purrr::map_dfr(resultados, "shape"), tempos = stats::setNames(purrr::map(
    resultados,
    "tempos"
  ), shapes_unicos$shape_id))
}

formatar_horarios_gtfs <- function(segundos) {
  sprintf("%02d:%02d:%02d", segundos %/% 3600L, (segundos %% 3600L) %/% 60L, segundos %% 60L)
}

gerar_dados_rota <- function(
  route_id, service_id, direction_id, horarios, sequencia_stops,
  shape_id, dicionario_tempos
) {
  if (length(horarios) == 0) {
    return(NULL)
  }
  numero_viagens <- length(horarios)
  numero_paradas <- length(sequencia_stops)
  trips <- tibble::tibble(
    route_id = route_id, service_id = service_id, trip_id = sprintf(
      "%s_%s_CIRCULAR_%s",
      route_id, service_id, sub(":", "", horarios)
    ), direction_id = direction_id,
    shape_id = shape_id
  )
  segundos_iniciais <- as.numeric(substr(horarios, 1, 2)) * 3600 + as.numeric(substr(
    horarios,
    4, 5
  )) * 60
  segundos_totais <- as.integer(round(outer(
    dicionario_tempos[[shape_id]] * 60,
    segundos_iniciais, "+"
  )))
  horarios_gtfs <- formatar_horarios_gtfs(segundos_totais)
  list(trips = trips, stop_times = data.table::data.table(
    trip_id = rep(trips$trip_id,
      each = numero_paradas
    ), arrival_time = horarios_gtfs, departure_time = horarios_gtfs,
    stop_id = rep(sequencia_stops, times = numero_viagens), stop_sequence = rep(seq_len(numero_paradas),
      times = numero_viagens
    )
  ))
}

calcular_intervalo_entre_saidas <- function(horarios) {
  minutos <- as.numeric(substr(horarios, 1, 2)) * 60 + as.numeric(substr(
    horarios,
    4, 5
  ))
  if (length(minutos) > 1) {
    min(diff(minutos))
  } else {
    NA_real_
  }
}

criar_diagnostico_tempos <- function(configuracoes_rotas, dicionario_tempos, margem_parada_min) {
  configuracoes_rotas |>
    dplyr::mutate(intervalo_min = vapply(
      horarios, calcular_intervalo_entre_saidas,
      numeric(1)
    ), n_trechos = lengths(sequencia_stops) - 1L, total_min = vapply(
      shape_id,
      function(id) max(dicionario_tempos[[id]]), numeric(1)
    ), so_valhalla_min = total_min -
      n_trechos * margem_parada_min) |>
    dplyr::group_by(route_id, shape_id, n_trechos, total_min, so_valhalla_min) |>
    dplyr::summarise(
      intervalo_min = suppressWarnings(min(intervalo_min, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(folga_min = intervalo_min - total_min)
}

montar_feed_gtfs <- function(dados_estaticos, trips, stop_times, shapes) {
  feed_info <- tibble::tibble(
    feed_publisher_name = dados_estaticos$agency$agency_name,
    feed_publisher_url = dados_estaticos$agency$agency_url, feed_lang = dados_estaticos$agency$agency_lang,
    feed_start_date = dados_estaticos$calendar$start_date[1], feed_end_date = dados_estaticos$calendar$end_date[1],
    feed_version = paste0("UFBA_OSRM_", Sys.Date())
  )
  feed <- lapply(list(
    agency = dados_estaticos$agency, routes = dados_estaticos$routes,
    trips = trips, stop_times = stop_times, stops = dados_estaticos$stops, calendar = dados_estaticos$calendar,
    shapes = shapes, feed_info = feed_info
  ), data.table::as.data.table)
  class(feed) <- c("tidygtfs", "dt_gtfs", "gtfs")
  feed$calendar[, c("start_date", "end_date") := lapply(.SD, as.Date, format = "%Y%m%d"),
    .SDcols = c("start_date", "end_date")
  ]
  feed$feed_info[, c("feed_start_date", "feed_end_date") := lapply(.SD, as.Date,
    format = "%Y%m%d"
  ), .SDcols = c("feed_start_date", "feed_end_date")]
  feed
}

gerar_gtfs <- function(
  diretorio_projeto, servidor_valhalla = "http://localhost:8002",
  margem_parada_min = 0.5, noturno_sem_sao_lazaro = FALSE, crs_metrico = 31984
) {
  validar_dependencias_gtfs()
  dados_estaticos <- criar_dados_estaticos_gtfs()
  configuracoes_rotas <- criar_configuracoes_rotas(noturno_sem_sao_lazaro)
  resultados <- gerar_shapes_e_tempos(
    configuracoes_rotas, dados_estaticos$stops,
    criar_urls_valhalla(servidor_valhalla), encontrar_shapes_manuais(diretorio_projeto),
    crs_metrico, margem_parada_min
  )
  diagnostico <- criar_diagnostico_tempos(
    configuracoes_rotas, resultados$tempos,
    margem_parada_min
  )
  message("Diagnóstico de tempos (margem = ", margem_parada_min, " min/trecho):")
  print(as.data.frame(diagnostico), digits = 4)
  trabalhos <- purrr::pmap(configuracoes_rotas, gerar_dados_rota, dicionario_tempos = resultados$tempos) |>
    purrr::compact()
  feed <- montar_feed_gtfs(dados_estaticos, data.table::rbindlist(purrr::map(
    trabalhos,
    "trips"
  )), data.table::rbindlist(purrr::map(trabalhos, "stop_times")), resultados$shapes)
  diretorio_saida <- file.path(diretorio_projeto, "data", "processed")
  dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
  caminho_saida <- file.path(diretorio_saida, "buzufba_gtfs.zip")
  gtfstools::write_gtfs(feed, caminho_saida)
  message("GTFS salvo em '", caminho_saida, "'.")
  invisible(feed)
}
