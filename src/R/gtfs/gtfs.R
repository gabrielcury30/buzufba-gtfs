# PACOTES E OPÇÕES
# Utiliza pacotes para manipulação de dados GTFS (gtfstools), 
# processamento de dados (tidyverse, data.table), 
# roteamento (Valhalla) e manipulação/visualização espacial (sf, mapview).
library(gtfstools)
library(tidyverse)
library(data.table)
library(sf)
library(mapview)
library(httr2)

# Servidor local do Valhalla (Docker, porta padrão 8002).
valhalla_server <- "http://localhost:8002"
valhalla_route_url <- paste0(sub("/+$", "", valhalla_server), "/route")
valhalla_trace_url <- paste0(sub("/+$", "", valhalla_server), "/trace_route")

# --- MODO DE GERAÇÃO DOS SHAPES ---
# Se algum dos arquivos abaixo existir, o GTFS usa os shapes desenhados à mão
# e o Valhalla só calcula os tempos entre paradas (endpoint /trace_route).
# Se nenhum existir, o Valhalla gera os shapes E os tempos (endpoint /route).
# Cada arquivo deve ter UMA linha (LINESTRING) por shape_id, com a coluna "shape_id".
shapes_manuais_candidatos <- c("data/shapes/shapes_manuais.gpkg",
                               "data/shapes/shapes_manuais.geojson")
shapes_manuais_path <- shapes_manuais_candidatos[file.exists(shapes_manuais_candidatos)][1]
usar_shapes_manuais <- !is.na(shapes_manuais_path)

# Para forçar o modo 100% Valhalla mesmo com o arquivo presente, descomente:
# usar_shapes_manuais <- FALSE

# CRS métrico usado para medir distâncias (SIRGAS 2000 / UTM 24S - Salvador)
crs_metrico <- 31984

# --- PARÂMETROS DE TEMPO E DE OPERAÇÃO ---

# Margem de tempo (minutos) somada a CADA trecho entre paradas adjacentes.
# O Valhalla calcula apenas o tempo de deslocamento: não inclui o tempo parado
# para embarque/desembarque. Esta margem representa essa parada (e a
# aceleração/frenagem). Valor inicial herdado da época do OSRM (perfil carro);
# o ideal é calibrá-lo com o tempo real de uma volta completa
# (ver o diagnóstico 'diagnostico_tempos', mais abaixo).
margem_parada_min <- 0.5

# Viagens noturnas (a partir de 'limite_noite'):
#   FALSE -> usam o mesmo shape e a mesma sequência de paradas do diurno
#            (passam por SAO_LAZARO). Use enquanto o trajeto noturno real
#            não for confirmado.
#   TRUE  -> pulam SAO_LAZARO e usam o shape próprio "..._CIRCULAR_N".
noturno_sem_sao_lazaro <- FALSE

# --- FUNÇÕES AUXILIARES DE PROCESSAMENTO ---

# processar_rota_valhalla:
# Calcula o trajeto real (shape) e os tempos acumulados até cada parada.
# Envia a rota COMPLETA (todas as paradas, na ordem) em uma única requisição.
# Assim o roteador enxerga o percurso inteiro e decide a direção correta em
# cada parada, em vez de tratar cada trecho como uma viagem independente.
processar_rota_valhalla <- function(shape_id, sequencia_stops, df_stops) {
  message(sprintf("Consultando Valhalla (rota completa, perfil bus) para: %s ...", 
                  shape_id))
  
  # Ordena as coordenadas conforme a sequência definida para a rota
  coords <- df_stops[match(sequencia_stops, df_stops$stop_id), ]
  
  # Monta todas as paradas como locations do Valhalla.
  # 'break' garante que cada parada gere um leg independente entre
  # duas paradas adjacentes.
  locations <- lapply(seq_len(nrow(coords)), function(i) {
    list(
      lat = coords$stop_lat[i],
      lon = coords$stop_lon[i],
      type = "break"
    )
  })
  
  # O formato de saída 'osrm' é solicitado apenas para manter uma estrutura
  # de resposta equivalente à que o código já consumia (routes/legs/geometry).
  # O roteamento, porém, é integralmente realizado pelo Valhalla com costing=bus.
  payload <- list(
    locations = locations,
    costing = "bus",
    directions_type = "none",
    format = "osrm",
    shape_format = "geojson"
  )
  
  # POST com corpo JSON: sem limite de tamanho de URL
  chamar_valhalla <- function() {
    resp <- httr2::request(valhalla_route_url) |>
      httr2::req_body_json(payload, auto_unbox = TRUE) |>
      httr2::req_error(is_error = \(r) FALSE) |>   # deixa tratarmos o erro abaixo
      httr2::req_perform()
    
    status <- httr2::resp_status(resp)
    corpo  <- httr2::resp_body_string(resp)
    
    if (status >= 400) {
      stop(sprintf("Valhalla retornou HTTP %s para %s: %s",
                   status, shape_id, corpo))
    }
    jsonlite::fromJSON(corpo)
  }
  
  resposta <- tryCatch(
    chamar_valhalla(),
    error = function(e) {
      message("  -> Falha no Valhalla (", conditionMessage(e), "). Tentando novamente...")
      Sys.sleep(2)
      chamar_valhalla()
    }
  )
  
  # Em caso de falha de roteamento, o Valhalla pode não retornar 'routes'.
  if (is.null(resposta$routes) || length(resposta$routes) == 0) {
    stop(
      sprintf(
        "Valhalla não retornou uma rota para %s. Resposta: %s",
        shape_id,
        paste(capture.output(str(resposta)), collapse = " ")
      )
    )
  }
  
  # Duração de cada trecho (leg) entre paradas adjacentes: o Valhalla informa
  # em segundos; converte para minutos (1 casa decimal, como o código anterior)
  # e adiciona a margem por trecho (margem_parada_min). O acumulado começa no ZERO.
  duracao_trechos <- round(resposta$routes$legs[[1]]$duration / 60, 1) + margem_parada_min
  tempos_acumulados <- c(0, cumsum(duracao_trechos))
  
  # Traçado contínuo da rota inteira (matriz com colunas lon, lat)
  matriz_pts <- resposta$routes$geometry$coordinates[[1]]
  
  df_shape <- tibble::tibble(
    shape_id = shape_id,
    shape_pt_lat = matriz_pts[, 2],
    shape_pt_lon = matriz_pts[, 1],
    shape_pt_sequence = seq_len(nrow(matriz_pts))
  )
  
  # Retorna uma lista contendo:
  # 1. 'shape': O traçado geométrico.
  # 2. 'tempos': Os tempos acumulados (min) para cada parada, na ordem da rota.
  list(shape = df_shape, tempos = tempos_acumulados)
}

# --- FUNÇÕES DO MODO "SHAPES MANUAIS + VALHALLA SÓ PARA TEMPOS" ---

# carregar_shapes_manuais:
# Lê o arquivo de shapes desenhados à mão e devolve uma lista nomeada
# (shape_id -> geometria LINESTRING em WGS84). Valida duplicatas e geometrias.
carregar_shapes_manuais <- function(path) {
  shp <- sf::st_read(path, quiet = TRUE) |> sf::st_zm()
  if (!"shape_id" %in% names(shp)) {
    stop("O arquivo de shapes manuais precisa ter uma coluna 'shape_id'.")
  }
  
  duplicados <- unique(shp$shape_id[duplicated(shp$shape_id)])
  if (length(duplicados) > 0) {
    stop("shape_id duplicado no arquivo de shapes manuais: ",
         paste(duplicados, collapse = ", "),
         ". Mantenha apenas uma feição por shape_id.")
  }
  
  shp <- sf::st_transform(shp, 4326)
  
  geoms <- lapply(seq_len(nrow(shp)), function(i) {
    g <- sf::st_sfc(sf::st_geometry(shp)[[i]], crs = 4326)
    # Une partes de uma MULTILINESTRING em uma única linha, se possível
    if (inherits(g[[1]], "MULTILINESTRING")) g <- sf::st_line_merge(g)
    if (!inherits(g[[1]], "LINESTRING")) {
      stop(sprintf("O shape '%s' não é uma linha contínua. Verifique o desenho.",
                   shp$shape_id[i]))
    }
    g
  })
  setNames(geoms, shp$shape_id)
}

# post_valhalla: POST genérico com uma nova tentativa em caso de falha.
post_valhalla <- function(url, payload) {
  chamar <- function() {
    resp <- httr2::request(url) |>
      httr2::req_body_json(payload, auto_unbox = TRUE) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    corpo <- httr2::resp_body_string(resp)
    if (httr2::resp_status(resp) >= 400) {
      stop(sprintf("HTTP %s: %s", httr2::resp_status(resp), corpo))
    }
    jsonlite::fromJSON(corpo)
  }
  tryCatch(chamar(), error = function(e) { Sys.sleep(1); chamar() })
}

# projetar_paradas:
# Para cada parada (na ordem da rota), acha o índice do vértice da linha onde ela
# "encosta". A busca é sempre para frente (>= parada anterior), o que é essencial
# em rotas circulares que passam duas vezes pelo mesmo lugar. Usa a primeira
# passagem da linha a menos de 'tol_m' metros da parada; se nenhuma estiver
# dentro da tolerância, pega a mais próxima e emite um aviso.
projetar_paradas <- function(xy_linha, xy_stops, nomes, tol_m = 30) {
  n <- nrow(xy_linha)
  idx <- integer(nrow(xy_stops))
  prev <- 1L
  for (i in seq_len(nrow(xy_stops))) {
    d <- sqrt((xy_linha[, 1] - xy_stops[i, 1])^2 +
                (xy_linha[, 2] - xy_stops[i, 2])^2)
    cand  <- prev:n
    perto <- cand[d[cand] <= tol_m]
    if (length(perto) > 0) {
      ini <- perto[1]; fim <- ini
      while (fim < n && d[fim + 1] <= tol_m) fim <- fim + 1
      janela <- ini:fim
      idx[i] <- janela[which.min(d[janela])]
    } else {
      idx[i] <- cand[which.min(d[cand])]
      warning(sprintf(
        "Parada '%s' (#%d) fica a %.0f m do shape (tolerância %d m). Confira o desenho.",
        nomes[i], i, d[idx[i]], tol_m), call. = FALSE)
    }
    prev <- idx[i]
  }
  idx
}

# tempo_trecho_seg:
# Tempo (s) para percorrer um trecho do shape manual. O /trace_route com
# map_snap cola o traçado na malha viária e calcula o tempo com o perfil bus,
# sem alterar o caminho desenhado. Se falhar, usa /route entre as duas paradas.
tempo_trecho_seg <- function(pts_ll, p_ini, p_fim) {
  if (nrow(pts_ll) < 2) return(0)
  
  payload <- list(
    shape = lapply(seq_len(nrow(pts_ll)), function(k) {
      list(lat = pts_ll[k, 2], lon = pts_ll[k, 1])
    }),
    costing = "bus",
    shape_match = "map_snap",
    directions_type = "none"
    # Se houver muitas falhas: trace_options = list(search_radius = 50, gps_accuracy = 10)
  )
  
  tryCatch(
    post_valhalla(valhalla_trace_url, payload)$trip$summary$time,
    error = function(e) {
      warning("map_snap falhou num trecho; usando /route entre as paradas. ",
              conditionMessage(e), call. = FALSE)
      r <- post_valhalla(valhalla_route_url, list(
        locations = list(list(lat = p_ini[1], lon = p_ini[2]),
                         list(lat = p_fim[1], lon = p_fim[2])),
        costing = "bus", directions_type = "none"))
      r$trip$summary$time
    }
  )
}

# processar_rota_manual:
# Equivalente a processar_rota_valhalla(), mas o shape vem do arquivo manual e o
# Valhalla só calcula os tempos. Mesma saída: list(shape, tempos).
processar_rota_manual <- function(shape_id, sequencia_stops, df_stops,
                                  shapes_manuais, passo_m = 10, tol_m = 30) {
  # Shape noturno (_N) sem desenho próprio: reaproveita o diurno
  id_geom <- shape_id
  if (!id_geom %in% names(shapes_manuais)) {
    base <- sub("_N$", "", shape_id)
    if (base %in% names(shapes_manuais)) {
      message(sprintf("  Sem shape manual para %s; usando %s.", shape_id, base))
      id_geom <- base
    } else {
      stop(sprintf("Não há shape manual para '%s' no arquivo.", shape_id))
    }
  }
  message(sprintf("Shape manual '%s' + tempos do Valhalla para: %s ...",
                  id_geom, shape_id))
  
  linha_ll <- shapes_manuais[[id_geom]]
  
  # Versão métrica e "adensada" (vértice a cada ~passo_m) usada só para
  # posicionar as paradas sobre a linha
  linha_m <- sf::st_transform(linha_ll, crs_metrico) |>
    sf::st_segmentize(units::set_units(passo_m, "m"))
  xy_m  <- sf::st_coordinates(linha_m)[, 1:2]
  xy_ll <- sf::st_coordinates(sf::st_transform(linha_m, 4326))[, 1:2]
  
  # Paradas na ordem da rota
  coords  <- df_stops[match(sequencia_stops, df_stops$stop_id), ]
  stops_m <- sf::st_as_sf(coords, coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    sf::st_transform(crs_metrico) |>
    sf::st_coordinates()
  
  idx <- projetar_paradas(xy_m, stops_m, sequencia_stops, tol_m)
  
  # Tempo (s) de cada trecho entre paradas consecutivas
  duracao_s <- vapply(seq_len(length(idx) - 1), function(i) {
    tempo_trecho_seg(
      xy_ll[idx[i]:idx[i + 1], , drop = FALSE],
      c(coords$stop_lat[i],     coords$stop_lon[i]),
      c(coords$stop_lat[i + 1], coords$stop_lon[i + 1]))
  }, numeric(1))
  
  # Mesma regra do modo Valhalla: minutos (1 casa) + margem por trecho
  duracao_trechos   <- round(duracao_s / 60, 1) + margem_parada_min
  tempos_acumulados <- c(0, cumsum(duracao_trechos))
  
  # Shape do GTFS = exatamente os vértices desenhados à mão
  m <- sf::st_coordinates(linha_ll)[, 1:2]
  df_shape <- tibble::tibble(
    shape_id          = shape_id,
    shape_pt_lat      = m[, 2],
    shape_pt_lon      = m[, 1],
    shape_pt_sequence = seq_len(nrow(m))
  )
  
  list(shape = df_shape, tempos = tempos_acumulados)
}

# gerar_dados_rota:
# Cria as tabelas 'trips' e 'stop_times' para uma rota específica,
# injetando os tempos calculados pelo Valhalla. 
# Totalmente vetorizada: não há loops nem agrupamentos por viagem.
gerar_dados_rota <- function(route_id, service_id, direction_id, horarios, 
                             sequencia_stops, shape_id, dicionario_tempos) {
  if (length(horarios) == 0) return(NULL)
  
  n_viagens <- length(horarios)
  n_paradas <- length(sequencia_stops)
  
  # Define metadados da viagem (trips)
  trips <- tibble::tibble(
    route_id     = route_id,
    service_id   = service_id,
    trip_id      = sprintf("%s_%s_CIRCULAR_%s", route_id, service_id, 
                           sub(":", "", horarios)),
    direction_id = direction_id,
    shape_id     = shape_id
  )
  
  # Horário de início de cada viagem, em segundos desde 00:00
  segundos_iniciais <- as.numeric(substr(horarios, 1, 2)) * 3600 + 
    as.numeric(substr(horarios, 4, 5)) * 60
  
  # Matriz paradas x viagens: soma o deslocamento (offset do Valhalla, em segundos)
  # de cada parada ao horário de início de cada viagem. 
  # as.vector() empilha viagem por viagem, na mesma ordem de 'rep()' abaixo.
  segundos_totais <- as.integer(round(
    outer(dicionario_tempos[[shape_id]] * 60, segundos_iniciais, "+")
  ))
  
  # Formata para padrão GTFS (HH:MM:SS)
  horario_gtfs <- sprintf("%02d:%02d:%02d", 
                          segundos_totais %/% 3600L, 
                          (segundos_totais %% 3600L) %/% 60L, 
                          segundos_totais %% 60L)
  
  # Define horários nas paradas (stop_times)
  stop_times <- data.table::data.table(
    trip_id        = rep(trips$trip_id, each = n_paradas),
    arrival_time   = horario_gtfs,
    departure_time = horario_gtfs,
    stop_id        = rep(sequencia_stops, times = n_viagens),
    stop_sequence  = rep(seq_len(n_paradas), times = n_viagens)
  )
  
  list(trips = trips, stop_times = stop_times)
}

# Funções de limpeza e união de rotas circulares
remover_sao_lazaro <- function(sequencia) {
  # Remove a parada e as duplicatas consecutivas que possam surgir após isso
  rle(sequencia[sequencia != "SAO_LAZARO"])$values
}

unir_circular <- function(ida, volta) {
  # Evita duplicar o ponto de encontro entre ida e volta
  c(ida, if (tail(ida, 1) == volta[1]) volta[-1] else volta)
}


# --- DADOS ESTÁTICOS DO GTFS ---

# Define os arquivos mandatórios e opcionais do formato GTFS
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
  route_type       = 3, # Ônibus
  route_color      = "00539F",
  route_text_color = "FFFFFF"
)

# Cadastro geográfico das paradas (stops)
stops <- tribble(
  ~stop_id,          ~stop_name,                                              ~stop_lat, ~stop_lon,
  "SAO_LAZARO",      "Pt. Estacionamento São Lázaro",                         -13.004774, -38.512385,
  "POLITECNICA",     "Pt. Politécnica",                                       -12.998718, -38.511790,
  "ARQUITETURA",     "Pt. Arquitetura",                                       -12.997140, -38.508629,
  "RESIDENCIA5",     "Pt. Residência 5",                                      -12.998336, -38.505939,
  "CANELA_ICS",      "Campus Vale do Canela (Entrada ICS)",                   -12.994839, -38.520590,
  "ISC_CANELA",      "ISC Canela",                                            -12.994563, -38.521921,
  "ODONTO",          "P. Odontologia",                                        -12.994676, -38.522890,
  "REITORIA",        "P. Reitoria",                                           -12.992404, -38.520654,
  "CRECHE",          "P. Creche Canela",                                      -12.994758, -38.517496,
  "GRACA_R2",        "P. Graça R2 (Delicia)",                                 -12.997615, -38.519141,
  "DIREITO",         "Faculdade de Direito",                                  -12.996353, -38.521559,
  "DIREITO_B5",      "Faculdade de Direito B5",                               -12.996190, -38.521341,
  "PAF1_MAT",        "Pt. Estacionamento (PAF.1 Matemática)",                 -13.001760, -38.506922,
  "PAF1_B1",         "PAF.1 Matemática B1",                                   -13.002414, -38.506589,
  "AV_7",            "Avenida 7 de Setembro / Faculdade de Economia",         -12.983266, -38.515419,
  "ECONOMIA",        "Faculdade de Economia Pt. 2",                           -12.983818, -38.515236,
  "BELAS_ARTES",     "Belas Artes",                                           -12.991215, -38.521153,
  "RESIDENCIA1",     "Residência I - Vitória",                                -12.994041, -38.526425,
  "GEOCIENCIAS",     "Pt. Instituto de Geociências",                          -12.998508, -38.506513,
  "FACOM",           "Pt. Facom",                                             -13.001510, -38.509824,
  "PORTARIA",        "Pt. Portaria Principal",                                -13.006104, -38.510314,
  "FACED",           "Faculdade de Educação",                                 -12.995148, -38.519300,
  "PROAE",           "Pró-Reitoria (PROAE)",                                  -12.997538, -38.509394,
  "CENTRO_ESPORTES", "Centro Esportes da UFBA",                               -13.009416, -38.513782,
  "POLITECNICA_VOLTA", "Pt. Politécnica Volta",                               -12.999561, -38.511507,
  "GARIBALDI",        "Pt. Av. Garibaildi",                                   -12.999217, -38.505917,
  "REITORIA_2",       "Pt. Reitoria Ida Economia",                            -12.992131, -38.520303,
  "GEOCIENCIAS_INTERNO", "Pt. Geociência Interno",                            -12.998233, -38.507374,
  "RESIDENCIA1_IDA",  "Pt. Residência I - Vitória Ida",                       -12.994062, -38.526339
)

# Define o calendário de operação
calendar <- tibble(
  service_id = c("DIAS_UTEIS", "SABADO"), 
  monday = c(1, 0), tuesday = c(1, 0), wednesday = c(1, 0), 
  thursday = c(1, 0), friday = c(1, 0), saturday = c(0, 1), sunday = c(0, 0), 
  start_date = "20260101", end_date = "20261231"
)


# --- CONFIGURAÇÃO DE ROTAS E HORÁRIOS ---

# Define as sequências de paradas de cada rota circular
seqs <- list(
  B1 = unir_circular(
    c("SAO_LAZARO", "POLITECNICA_VOLTA", "ARQUITETURA", "RESIDENCIA5", "CANELA_ICS", 
      "ISC_CANELA", "ODONTO"),
    c("REITORIA", "CRECHE", "GRACA_R2", "DIREITO", "FACED", "PAF1_B1", "PROAE", 
      "POLITECNICA_VOLTA", "SAO_LAZARO")
  ),
  B2 = unir_circular(
    c("PAF1_MAT", "GARIBALDI", "PROAE", "POLITECNICA_VOLTA", "SAO_LAZARO", 
      "POLITECNICA", "CRECHE", "REITORIA_2", "BELAS_ARTES", "REITORIA", "CRECHE",
      "GRACA_R2", "RESIDENCIA1"),
    c("RESIDENCIA1_IDA", "DIREITO", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", 
      "POLITECNICA", "SAO_LAZARO", "POLITECNICA_VOLTA", "ARQUITETURA", "RESIDENCIA5",
      "GEOCIENCIAS", "PAF1_MAT")
  ),
  B3 = unir_circular(
    c("PAF1_MAT", "GARIBALDI", "CANELA_ICS", "AV_7", "BELAS_ARTES"),
    c("REITORIA", "CRECHE", "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS",
      "PAF1_MAT")
  ),
  B4 = unir_circular(
    c("PAF1_MAT", "GARIBALDI", "PROAE", "POLITECNICA", "CRECHE", "REITORIA_2", 
      "ECONOMIA"),
    c("RESIDENCIA1", "GRACA_R2", "POLITECNICA_VOLTA", "SAO_LAZARO", 
      "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT")
  ),
  B5 = unir_circular(
    c("GEOCIENCIAS_INTERNO", "FACOM", "PORTARIA", "CENTRO_ESPORTES", "PAF1_B1", 
      "GARIBALDI", "PROAE", "POLITECNICA_VOLTA", "SAO_LAZARO", "POLITECNICA_VOLTA",
      "POLITECNICA", "CRECHE", "REITORIA_2"),
    c("RESIDENCIA1", "DIREITO_B5", "ISC_CANELA", "ODONTO", "REITORIA", "CRECHE", 
      "POLITECNICA", "POLITECNICA_VOLTA", "SAO_LAZARO", "POLITECNICA_VOLTA", 
      "ARQUITETURA", "GEOCIENCIAS", "PAF1_B1", "PORTARIA", "FACOM", "GEOCIENCIAS_INTERNO")
  )
)

# Horários base de operação e regras de restrição (noite/sábado)
h_base <- list(
  B1 = list(full = c("06:10","07:40","09:10","10:40","12:10","13:40","15:10",
                     "16:40","18:10","19:40","21:10","22:40"), 
            limite_noite = "19:40", limite_sab = "13:40"),
  B2 = list(full = c("06:00","07:40","09:20","11:00","12:40","14:20","16:00",
                     "17:40","19:20","21:00","22:40"), limite_noite = "19:20", 
            limite_sab = "14:20"),
  B3 = list(full = c("06:30","07:40","08:50","10:00","11:10","12:20","13:30",
                     "14:40","15:50","17:00","18:10","19:20","20:30","21:40",
                     "22:50"), limite_noite = "18:10", limite_sab = "14:40"),
  B4 = list(full = c("06:00","07:35","09:10","10:45","12:20","13:55","15:30",
                     "17:05","18:40","20:15","21:50"), limite_noite = "18:40", 
            limite_sab = "13:55"),
  B5 = list(full = c("06:40","08:15","09:50","11:25","13:00","14:35","16:10",
                     "17:45","19:20","20:55","22:30"), limite_noite = "17:45", 
            limite_sab = "14:35")
)

# --- GERADOR DE CONFIGURAÇÕES DE VIAGENS ---
# Mapeia as rotas para gerar uma tabela mestre de configurações 
# (combinações de rota, serviço e horário)
config_rotas <- map_dfr(names(h_base), function(rota) {
  h <- h_base[[rota]]
  s_circular <- seqs[[rota]]
  
  horarios_dia <- h$full[h$full < h$limite_noite]
  horarios_noite <- h$full[h$full >= h$limite_noite]
  horarios_sab <- h$full[1:which(h$full == h$limite_sab)]
  
  # Noturno: igual ao diurno ou sem SAO_LAZARO, conforme 'noturno_sem_sao_lazaro'
  s_noite  <- if (noturno_sem_sao_lazaro) remover_sao_lazaro(s_circular) else s_circular
  id_dia   <- paste0("SHP_", rota, "_CIRCULAR")
  id_noite <- if (noturno_sem_sao_lazaro) paste0(id_dia, "_N") else id_dia
  
  tribble(
    ~route_id, ~service_id, ~direction_id, ~horarios,      ~sequencia_stops,                ~shape_id,
    rota,      "DIAS_UTEIS", 0,             horarios_dia,   s_circular,  id_dia,
    rota,      "DIAS_UTEIS", 0,             horarios_noite, s_noite,     id_noite,
    rota,      "SABADO",     0,             horarios_sab,   s_circular,  id_dia
  )
})

# --- GERAÇÃO AUTOMATIZADA DOS DADOS GTFS ---

# 1. Gera os shapes únicos e os tempos entre paradas.
#    Modo A (arquivo de shapes manuais presente): shape desenhado à mão,
#            Valhalla (/trace_route, perfil bus) só calcula os tempos.
#    Modo B (sem arquivo): Valhalla (/route, perfil bus) gera shape e tempos.
shapes_unicos <- config_rotas %>% dplyr::distinct(shape_id, sequencia_stops)

if (usar_shapes_manuais) {
  message(sprintf("\nModo A: shapes manuais (%s) + tempos do Valhalla.", 
                  shapes_manuais_path))
  shapes_manuais <- carregar_shapes_manuais(shapes_manuais_path)
  
  # Confere se todos os shape_id necessários existem (aceita o diurno como
  # substituto do noturno "_N") antes de iniciar as consultas ao Valhalla
  faltando <- shapes_unicos$shape_id[
    !(shapes_unicos$shape_id %in% names(shapes_manuais) | 
        sub("_N$", "", shapes_unicos$shape_id) %in% names(shapes_manuais))]
  if (length(faltando) > 0) {
    stop("Faltam shapes no arquivo manual: ", paste(faltando, collapse = ", "))
  }
  
  resultados_valhalla <- purrr::map2(shapes_unicos$shape_id, 
                                     shapes_unicos$sequencia_stops, 
                                     processar_rota_manual, 
                                     df_stops = stops, 
                                     shapes_manuais = shapes_manuais)
} else {
  message("\nModo B: arquivo de shapes manuais não encontrado. ",
          "Valhalla gera shapes e tempos (perfil bus).")
  resultados_valhalla <- purrr::map2(shapes_unicos$shape_id, 
                                     shapes_unicos$sequencia_stops, 
                                     processar_rota_valhalla, df_stops = stops)
}

# Separa as tabelas de shape
shapes_final <- purrr::map_dfr(resultados_valhalla, "shape")

# Cria o Dicionário de Tempos (associa shape_id aos tempos entre paradas)
dicionario_tempos <- setNames(purrr::map(resultados_valhalla, "tempos"), 
                              shapes_unicos$shape_id)

# Diagnóstico: duração de uma volta completa x intervalo entre saídas.
#   total_min       = tempo de uma volta, com a margem
#   so_valhalla_min = tempo de uma volta, sem a margem (deslocamento puro)
#   folga_min       = intervalo entre saídas - volta (negativo = uma saída
#                     começaria antes de a anterior terminar)
# Compare 'total_min' com o tempo real medido de uma volta para calibrar
# 'margem_parada_min':
#   margem ideal ~ (tempo_real - so_valhalla_min) / n_trechos
intervalo_entre_saidas <- function(h) {
  m <- as.numeric(substr(h, 1, 2)) * 60 + as.numeric(substr(h, 4, 5))
  if (length(m) > 1) min(diff(m)) else NA_real_
}

diagnostico_tempos <- config_rotas %>%
  dplyr::mutate(
    intervalo_min   = vapply(horarios, intervalo_entre_saidas, numeric(1)),
    n_trechos       = lengths(sequencia_stops) - 1L,
    total_min       = vapply(shape_id, function(id) max(dicionario_tempos[[id]]), 
                             numeric(1)),
    so_valhalla_min = total_min - n_trechos * margem_parada_min
  ) %>%
  dplyr::group_by(route_id, shape_id, n_trechos, total_min, so_valhalla_min) %>%
  dplyr::summarise(intervalo_min = suppressWarnings(min(intervalo_min, na.rm = TRUE)),
                   .groups = "drop") %>%
  dplyr::mutate(folga_min = intervalo_min - total_min)

message("\nDiagnóstico de tempos (margem = ", margem_parada_min, " min/trecho):")
print(as.data.frame(diagnostico_tempos), digits = 4)

# 2. Gera as tabelas 'trips' e 'stop_times' para todas as rotas configuradas
#    (pmap casa as colunas de 'config_rotas' com os argumentos de mesmo nome)
jobs <- purrr::pmap(config_rotas, gerar_dados_rota, 
                    dicionario_tempos = dicionario_tempos) %>% 
  purrr::compact()

trips_final      <- rbindlist(purrr::map(jobs, "trips"))
stop_times_final <- rbindlist(purrr::map(jobs, "stop_times"))

# 3. Monta as informações de Feed (metadados do arquivo GTFS)
feed_info <- tibble::tibble(
  feed_publisher_name = agency$agency_name,
  feed_publisher_url  = agency$agency_url,
  feed_lang           = agency$agency_lang,
  feed_start_date     = calendar$start_date[1],
  feed_end_date       = calendar$end_date[1],
  feed_version        = paste0("UFBA_OSRM_", Sys.Date())
)

# --- MONTAGEM E VALIDAÇÃO DO OBJETO GTFS ---

# Estrutura o GTFS final como um objeto de classe 'dt_gtfs' e 'gtfs'
gtfs <- lapply(
  list(agency     = agency,
       routes     = routes,
       trips      = trips_final,
       stop_times = stop_times_final,
       stops      = stops,
       calendar   = calendar,
       shapes     = shapes_final,
       feed_info  = feed_info),
  as.data.table
)

class(gtfs) <- c("tidygtfs", "dt_gtfs", "gtfs")

# Formata datas para o padrão esperado pelo gtfstools
gtfs$calendar[, c("start_date", "end_date") := 
                lapply(.SD, as.Date, format = "%Y%m%d"), 
              .SDcols = c("start_date", "end_date")]
gtfs$feed_info[, c("feed_start_date", "feed_end_date") := 
                 lapply(.SD, as.Date, format = "%Y%m%d"), 
               .SDcols = c("feed_start_date", "feed_end_date")]

# Exporta o arquivo final para uso no r5r
write_gtfs(gtfs, "data/gtfs/buzufba_gtfs.zip")
message("GTFS salvo em 'data/gtfs/buzufba_gtfs.zip'.")