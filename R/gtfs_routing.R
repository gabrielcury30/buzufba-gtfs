# Funções de roteamento e processamento de shapes para o GTFS BUZUFBA.

criar_urls_valhalla <- function(servidor = "http://localhost:8002") {
  servidor <- sub("/+$", "", servidor)
  list(route = paste0(servidor, "/route"), trace = paste0(servidor, "/trace_route"))
}

enviar_requisicao_valhalla <- function(url, payload, tentativas = 2L) {
  executar_requisicao <- function() {
    resposta <- httr2::request(url) |>
      httr2::req_body_json(payload, auto_unbox = TRUE) |>
      httr2::req_error(is_error = function(resposta) FALSE) |>
      httr2::req_perform()
    corpo <- httr2::resp_body_string(resposta)

    if (httr2::resp_status(resposta) >= 400) {
      stop(sprintf("HTTP %s: %s", httr2::resp_status(resposta), corpo), call. = FALSE)
    }

    jsonlite::fromJSON(corpo)
  }

  for (tentativa in seq_len(tentativas)) {
    resultado <- tryCatch(executar_requisicao(), error = identity)
    if (!inherits(resultado, "error")) {
      return(resultado)
    }
    if (tentativa < tentativas) {
      Sys.sleep(1)
    }
  }

  stop(conditionMessage(resultado), call. = FALSE)
}

processar_rota_valhalla <- function(
  shape_id, sequencia_stops, paradas, url_route,
  margem_parada_min
) {
  message(sprintf(
    "Consultando Valhalla (rota completa, perfil bus) para: %s ...",
    shape_id
  ))
  coordenadas <- paradas[match(sequencia_stops, paradas$stop_id), ]
  locations <- lapply(seq_len(nrow(coordenadas)), function(indice) {
    list(
      lat = coordenadas$stop_lat[indice], lon = coordenadas$stop_lon[indice],
      type = "break"
    )
  })

  resposta <- tryCatch(
    enviar_requisicao_valhalla(url_route, list(
      locations = locations,
      costing = "bus", directions_type = "none", format = "osrm", shape_format = "geojson"
    )),
    error = function(erro) {
      stop(sprintf("Valhalla falhou para %s: %s", shape_id, conditionMessage(erro)),
        call. = FALSE
      )
    }
  )

  if (is.null(resposta$routes) || length(resposta$routes) == 0) {
    stop(sprintf("Valhalla não retornou uma rota para %s.", shape_id), call. = FALSE)
  }

  duracao_trechos <- round(resposta$routes$legs[[1]]$duration / 60, 1) + margem_parada_min
  pontos <- resposta$routes$geometry$coordinates[[1]]

  list(shape = tibble::tibble(
    shape_id = shape_id, shape_pt_lat = pontos[, 2],
    shape_pt_lon = pontos[, 1], shape_pt_sequence = seq_len(nrow(pontos))
  ), tempos = c(
    0,
    cumsum(duracao_trechos)
  ))
}

carregar_shapes_manuais <- function(caminho_shapes) {
  shapes <- sf::st_read(caminho_shapes, quiet = TRUE) |>
    sf::st_zm()

  if (!"shape_id" %in% names(shapes)) {
    stop("O arquivo de shapes manuais precisa ter uma coluna 'shape_id'.", call. = FALSE)
  }

  duplicados <- unique(shapes$shape_id[duplicated(shapes$shape_id)])
  if (length(duplicados) > 0) {
    stop("shape_id duplicado no arquivo de shapes manuais: ", paste(duplicados,
      collapse = ", "
    ), call. = FALSE)
  }

  shapes <- sf::st_transform(shapes, 4326)
  geometrias <- lapply(seq_len(nrow(shapes)), function(indice) {
    geometria <- sf::st_sfc(sf::st_geometry(shapes)[[indice]], crs = 4326)
    if (inherits(geometria[[1]], "MULTILINESTRING")) {
      geometria <- sf::st_line_merge(geometria)
    }
    if (!inherits(geometria[[1]], "LINESTRING")) {
      stop(sprintf("O shape '%s' não é uma linha contínua.", shapes$shape_id[indice]),
        call. = FALSE
      )
    }
    geometria
  })

  stats::setNames(geometrias, shapes$shape_id)
}

projetar_paradas_no_shape <- function(
  coordenadas_linha, coordenadas_paradas, nomes_paradas,
  tolerancia_m = 30
) {
  numero_vertices <- nrow(coordenadas_linha)
  indices <- integer(nrow(coordenadas_paradas))
  indice_anterior <- 1L

  for (indice_parada in seq_len(nrow(coordenadas_paradas))) {
    distancias <- sqrt((coordenadas_linha[, 1] - coordenadas_paradas[
      indice_parada,
      1
    ])^2 + (coordenadas_linha[, 2] - coordenadas_paradas[
      indice_parada,
      2
    ])^2)
    candidatos <- indice_anterior:numero_vertices
    proximos <- candidatos[distancias[candidatos] <= tolerancia_m]

    if (length(proximos) > 0) {
      inicio <- proximos[1]
      fim <- inicio
      while (fim < numero_vertices && distancias[fim + 1] <= tolerancia_m) {
        fim <- fim +
          1
      }
      janela <- inicio:fim
      indices[indice_parada] <- janela[which.min(distancias[janela])]
    } else {
      indices[indice_parada] <- candidatos[which.min(distancias[candidatos])]
      warning(sprintf(
        "Parada '%s' (#%d) fica a %.0f m do shape (tolerância %d m).",
        nomes_paradas[indice_parada], indice_parada, distancias[indices[indice_parada]],
        tolerancia_m
      ), call. = FALSE)
    }

    indice_anterior <- indices[indice_parada]
  }

  indices
}

calcular_tempo_trecho <- function(
  pontos_linha, parada_inicial, parada_final, url_trace,
  url_route
) {
  if (nrow(pontos_linha) < 2) {
    return(0)
  }

  payload_trace <- list(shape = lapply(seq_len(nrow(pontos_linha)), function(indice) {
    list(lat = pontos_linha[indice, 2], lon = pontos_linha[indice, 1])
  }), costing = "bus", shape_match = "map_snap", directions_type = "none")

  tryCatch(enviar_requisicao_valhalla(url_trace, payload_trace)$trip$summary$time,
    error = function(erro) {
      warning("map_snap falhou num trecho; usando /route entre as paradas. ",
        conditionMessage(erro),
        call. = FALSE
      )
      enviar_requisicao_valhalla(url_route, list(
        locations = list(list(
          lat = parada_inicial[1],
          lon = parada_inicial[2]
        ), list(lat = parada_final[1], lon = parada_final[2])),
        costing = "bus", directions_type = "none"
      ))$trip$summary$time
    }
  )
}

processar_rota_manual <- function(
  shape_id, sequencia_stops, paradas, shapes_manuais,
  url_trace, url_route, crs_metrico, margem_parada_min, passo_m = 10, tolerancia_m = 30
) {
  id_geometria <- shape_id
  if (!id_geometria %in% names(shapes_manuais)) {
    id_diurno <- sub("_N$", "", shape_id)
    if (!id_diurno %in% names(shapes_manuais)) {
      stop(sprintf("Não há shape manual para '%s' no arquivo.", shape_id),
        call. = FALSE
      )
    }
    message(sprintf("Sem shape manual para %s; usando %s.", shape_id, id_diurno))
    id_geometria <- id_diurno
  }

  message(sprintf(
    "Shape manual '%s' + tempos do Valhalla para: %s ...", id_geometria,
    shape_id
  ))
  linha_wgs84 <- shapes_manuais[[id_geometria]]
  linha_metrica <- sf::st_transform(linha_wgs84, crs_metrico) |>
    sf::st_segmentize(units::set_units(passo_m, "m"))
  coordenadas_m <- sf::st_coordinates(linha_metrica)[, 1:2]
  coordenadas_wgs84 <- sf::st_coordinates(sf::st_transform(linha_metrica, 4326))[
    ,
    1:2
  ]
  coordenadas_paradas <- paradas[match(sequencia_stops, paradas$stop_id), ]
  paradas_metricas <- sf::st_as_sf(coordenadas_paradas, coords = c(
    "stop_lon",
    "stop_lat"
  ), crs = 4326) |>
    sf::st_transform(crs_metrico) |>
    sf::st_coordinates()
  indices <- projetar_paradas_no_shape(
    coordenadas_m, paradas_metricas, sequencia_stops,
    tolerancia_m
  )

  duracoes_segundos <- vapply(seq_len(length(indices) - 1), function(indice) {
    calcular_tempo_trecho(
      coordenadas_wgs84[indices[indice]:indices[indice +
        1], , drop = FALSE], c(coordenadas_paradas$stop_lat[indice], coordenadas_paradas$stop_lon[indice]),
      c(coordenadas_paradas$stop_lat[indice + 1], coordenadas_paradas$stop_lon[indice +
        1]), url_trace, url_route
    )
  }, numeric(1))
  duracoes_minutos <- round(duracoes_segundos / 60, 1) + margem_parada_min
  pontos <- sf::st_coordinates(linha_wgs84)[, 1:2]

  list(shape = tibble::tibble(
    shape_id = shape_id, shape_pt_lat = pontos[, 2],
    shape_pt_lon = pontos[, 1], shape_pt_sequence = seq_len(nrow(pontos))
  ), tempos = c(
    0,
    cumsum(duracoes_minutos)
  ))
}
