validar_caminhos_entrada <- function(caminhos_rede, caminhos_edificacoes) {
  for (cenario in names(caminhos_rede)) {
    pasta <- caminhos_rede[[cenario]]

    if (!dir.exists(pasta)) {
      stop(
        "A pasta de rede do cenário '", cenario, "' não existe: ", pasta,
        call. = FALSE
      )
    }

    arquivos_pbf <- list.files(pasta, pattern = "\\.pbf$", ignore.case = TRUE)
    arquivos_gtfs <- list.files(pasta, pattern = "\\.zip$", ignore.case = TRUE)
    arquivos_tif <- list.files(pasta, pattern = "\\.tif$", ignore.case = TRUE)

    if (length(arquivos_pbf) != 1L) {
      stop(
        "A pasta ", pasta,
        " deve conter exatamente um arquivo .pbf de rede viária.",
        call. = FALSE
      )
    }
    if (length(arquivos_gtfs) < 1L) {
      stop(
        "A pasta ", pasta, " precisa conter pelo menos um GTFS em .zip.",
        call. = FALSE
      )
    }
    if (length(arquivos_tif) > 1L) {
      stop(
        "A pasta ", pasta,
        " deve conter no máximo um arquivo .tif de elevação.",
        call. = FALSE
      )
    }
  }

  for (cenario in names(caminhos_edificacoes)) {
    caminho <- caminhos_edificacoes[[cenario]]

    if (!file.exists(caminho)) {
      stop(
        "O GeoPackage de edificações do cenário '", cenario,
        "' não foi encontrado: ", caminho,
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

ler_unidades <- function(caminho, cenario) {
  edificacoes <- sf::st_read(caminho, quiet = TRUE)

  if (!"name" %in% names(edificacoes)) {
    stop(
      "O GeoPackage do cenário '", cenario,
      "' precisa ter um campo chamado 'name'.",
      call. = FALSE
    )
  }
  if (is.na(sf::st_crs(edificacoes))) {
    stop(
      "O GeoPackage do cenário '", cenario, "' não informa seu CRS.",
      call. = FALSE
    )
  }

  nomes <- trimws(as.character(edificacoes$name))

  if (any(is.na(nomes) | nomes == "")) {
    stop(
      "Há valores de 'name' vazios no GeoPackage do cenário '", cenario,
      "'. Preencha os nomes antes de comparar os cenários.",
      call. = FALSE
    )
  }
  if (anyDuplicated(nomes)) {
    repetidos <- unique(nomes[duplicated(nomes)])
    stop(
      "O campo 'name' precisa identificar cada unidade de forma única. ",
      "Duplicados em '", cenario, "': ",
      paste(head(repetidos, 10), collapse = ", "),
      call. = FALSE
    )
  }

  # Calcula o ponto-na-superfície em CRS projetado e depois usa WGS84.
  pontos <- edificacoes |>
    dplyr::mutate(.nome_unidade = nomes) |>
    sf::st_transform(31984) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)

  coordenadas <- sf::st_coordinates(pontos)

  data.frame(
    unidade = pontos$.nome_unidade,
    lon = coordenadas[, 1],
    lat = coordenadas[, 2],
    stringsAsFactors = FALSE
  )
}

validar_unidades_cenarios <- function(unidades_atual, unidades_intervencao) {
  nomes_atual <- sort(unidades_atual$unidade)
  nomes_intervencao <- sort(unidades_intervencao$unidade)

  if (!identical(nomes_atual, nomes_intervencao)) {
    faltam_na_intervencao <- setdiff(nomes_atual, nomes_intervencao)
    faltam_no_atual <- setdiff(nomes_intervencao, nomes_atual)
    detalhes <- c(
      if (length(faltam_na_intervencao)) {
        paste0(
          "Presentes no atual, mas ausentes na intervenção: ",
          paste(head(faltam_na_intervencao, 10), collapse = ", ")
        )
      },
      if (length(faltam_no_atual)) {
        paste0(
          "Presentes na intervenção, mas ausentes no atual: ",
          paste(head(faltam_no_atual, 10), collapse = ", ")
        )
      }
    )

    stop(
      "Os GeoPackages não contêm o mesmo conjunto de unidades. ",
      "Para comparar os cenários com o mesmo denominador, padronize o ",
      "campo 'name' nos dois arquivos. ",
      paste(detalhes, collapse = " | "),
      call. = FALSE
    )
  }

  invisible(nomes_atual)
}

criar_cadastro_unidades <- function(unidades_atual) {
  nomes_atual <- sort(unidades_atual$unidade)

  # IDs estáveis baseados no nome ordenado, sem depender da ordem das feições.
  data.frame(
    id = sprintf("UFBA_%03d", seq_along(nomes_atual)),
    unidade = nomes_atual,
    stringsAsFactors = FALSE
  )
}

preparar_origens_destinos <- function(unidades, cadastro) {
  indices <- match(unidades$unidade, cadastro$unidade)

  data.frame(
    id = cadastro$id[indices],
    lon = unidades$lon,
    lat = unidades$lat,
    stringsAsFactors = FALSE
  )
}

criar_ids_representantes <- function(cadastro, unidades_representativas) {
  ids_representantes <- cadastro$id[
    match(unname(unidades_representativas), cadastro$unidade)
  ]
  names(ids_representantes) <- names(unidades_representativas)

  if (anyNA(ids_representantes)) {
    faltantes <- names(unidades_representativas)[is.na(ids_representantes)]
    nomes_procurados <- unname(unidades_representativas)[
      is.na(ids_representantes)
    ]

    stop(
      "Não encontrei no campo 'name' das edificações estas unidades ",
      "representativas: ",
      paste(paste0(faltantes, " = '", nomes_procurados, "'"), collapse = "; "),
      ". Corrija 'unidades_representativas' no início do script para usar ",
      "os nomes exatos do GeoPackage.",
      call. = FALSE
    )
  }

  ids_representantes
}

selecionar_pontos_representativos <- function(origens_destinos,
                                              ids_representantes,
                                              cenario) {
  indices <- match(unname(ids_representantes), origens_destinos$id)

  if (anyNA(indices)) {
    stop(
      "Não foi possível selecionar todos os pontos representativos no cenário ",
      cenario, ".",
      call. = FALSE
    )
  }

  pontos <- origens_destinos[indices, c("id", "lon", "lat")]
  rownames(pontos) <- NULL
  pontos
}

criar_fluxos_alvo <- function(ids_representantes) {
  fluxos_alvo <- data.frame(
    origem_regiao = c(
      "Ondina", "Engenharia", "Ondina", "Canela", "Ondina", "Arquitetura"
    ),
    destino_regiao = c(
      "Engenharia", "Ondina", "Canela", "Ondina", "Arquitetura", "Ondina"
    ),
    stringsAsFactors = FALSE
  )

  fluxos_alvo$from_id <- unname(ids_representantes[fluxos_alvo$origem_regiao])
  fluxos_alvo$to_id <- unname(ids_representantes[fluxos_alvo$destino_regiao])
  fluxos_alvo$fluxo <- paste(
    fluxos_alvo$origem_regiao, "→", fluxos_alvo$destino_regiao
  )

  fluxos_alvo
}
