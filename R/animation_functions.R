calcular_distancia_haversine <- function(lat1, lon1, lat2, lon2) {
  radianos <- pi / 180
  raio_terra_m <- 6371000
  diferenca_lat <- (lat2 - lat1) * radianos
  diferenca_lon <- (lon2 - lon1) * radianos
  a <- sin(diferenca_lat / 2)^2 + cos(lat1 * radianos) * cos(lat2 * radianos) * sin(diferenca_lon / 2)^2
  2 * raio_terra_m * asin(sqrt(a))
}

adensar_shape <- function(shape, passo_m = 10) {
  numero_pontos <- nrow(shape)
  if (numero_pontos < 2) {
    return(shape[, c("shape_pt_lat", "shape_pt_lon")])
  }

  distancias <- calcular_distancia_haversine(
    shape$shape_pt_lat[-numero_pontos], shape$shape_pt_lon[-numero_pontos],
    shape$shape_pt_lat[-1], shape$shape_pt_lon[-1]
  )
  subdivisoes <- pmax(1L, as.integer(ceiling(distancias / passo_m)))
  segmentos <- rep(seq_len(numero_pontos - 1), subdivisoes)
  fracoes <- (sequence(subdivisoes) - 1) / subdivisoes[segmentos]

  tibble::tibble(
    shape_pt_lat = c(shape$shape_pt_lat[segmentos] + fracoes * (shape$shape_pt_lat[segmentos + 1] - shape$shape_pt_lat[segmentos]), shape$shape_pt_lat[numero_pontos]),
    shape_pt_lon = c(shape$shape_pt_lon[segmentos] + fracoes * (shape$shape_pt_lon[segmentos + 1] - shape$shape_pt_lon[segmentos]), shape$shape_pt_lon[numero_pontos])
  )
}

encaixar_paradas_no_shape <- function(stop_lat, stop_lon, shape, tolerancia_m = 30) {
  numero_pontos <- nrow(shape)
  ultimo_indice <- 1L
  distancias_paradas <- numeric(length(stop_lat))

  for (indice in seq_along(stop_lat)) {
    distancias <- calcular_distancia_haversine(
      stop_lat[indice], stop_lon[indice],
      shape$shape_pt_lat[ultimo_indice:numero_pontos],
      shape$shape_pt_lon[ultimo_indice:numero_pontos]
    )
    proximos <- which(distancias <= tolerancia_m)

    if (length(proximos) > 0) {
      inicio <- proximos[1]
      fim <- inicio
      while (fim < length(distancias) && distancias[fim + 1] <= tolerancia_m) fim <- fim + 1
      indice_local <- inicio - 1L + which.min(distancias[inicio:fim])
    } else {
      indice_local <- which.min(distancias)
      warning(sprintf("Parada #%d fica a %.0f m do shape '%s'.", indice, distancias[indice_local], shape$shape_id[1]), call. = FALSE)
    }

    indice_shape <- ultimo_indice + indice_local - 1L
    distancias_paradas[indice] <- shape$shape_dist_traveled[indice_shape]
    ultimo_indice <- indice_shape
  }

  distancias_paradas
}

criar_pontos_animacao <- function(viagem, shapes_dist, paradas_viagens, data_partida,
                                  passo_segundos, duracao_rastro_segundos, cores_rotas,
                                  fuso = "America/Bahia") {
  shape <- shapes_dist |>
    dplyr::filter(shape_id == viagem$shape_id[1]) |>
    dplyr::select(dist = shape_dist_traveled, lat = shape_pt_lat, lon = shape_pt_lon) |>
    dplyr::mutate(time = as.POSIXct(NA_real_, origin = "1970-01-01"))

  paradas <- paradas_viagens |>
    dplyr::filter(trip_id == viagem$trip_id[1]) |>
    dplyr::mutate(
      time = as.POSIXct(paste(data_partida, as.character(arrival_time)), tz = fuso),
      lat = stop_lat,
      lon = stop_lon
    ) |>
    dplyr::select(dist, time, lat, lon)

  combinados <- dplyr::bind_rows(paradas, shape) |>
    dplyr::arrange(dist) |>
    dplyr::group_by(dist) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup()

  if (sum(!is.na(combinados$time)) < 2) {
    warning(sprintf("Viagem '%s' ignorada: paradas encaixadas no mesmo ponto do shape.", viagem$trip_id[1]), call. = FALSE)
    return(NULL)
  }

  combinados$time_i <- as.numeric(stinepack::na.stinterp(as.numeric(combinados$time), along = combinados$dist, na.rm = FALSE))
  coordenadas <- combinados |>
    dplyr::filter(!is.na(time_i), !is.na(lat)) |>
    dplyr::distinct(time_i, .keep_all = TRUE)
  if (nrow(coordenadas) < 2) {
    return(NULL)
  }

  tempos <- seq(min(coordenadas$time_i), max(coordenadas$time_i), by = passo_segundos)
  latitude <- approx(coordenadas$time_i, coordenadas$lat, xout = tempos)$y
  longitude <- approx(coordenadas$time_i, coordenadas$lon, xout = tempos)$y

  data.frame(
    lat = latitude,
    lon = longitude,
    start = format(as.POSIXct(tempos, origin = "1970-01-01", tz = fuso), "%Y-%m-%dT%H:%M:%S"),
    end = format(as.POSIXct(tempos + duracao_rastro_segundos, origin = "1970-01-01", tz = fuso), "%Y-%m-%dT%H:%M:%S"),
    color = unname(cores_rotas[viagem$route_id[1]]),
    route_id = viagem$route_id[1]
  )
}
