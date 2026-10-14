# Comparação de acessibilidade entre dois cenários da UFBA
# Cenário atual vs. cenário com intervenção.
#
# Cada cenário utiliza:
#   1) sua própria pasta de rede (GTFS + PBF + TIF opcional);
#   2) seu próprio GeoPackage de edificações.
#
# Os dois GeoPackages devem conter o campo 'name' com os mesmos nomes únicos
# de unidades. As geometrias podem ser diferentes; os nomes são usados para
# identificar a mesma unidade nos dois cenários e comparar os resultados.
#
# Saídas: apenas dois PNGs comparativos. Nenhum CSV/RDS é exportado.

options(java.parameters = "-Xmx4G")

library(r5r)
library(sf)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Configuração: ajuste os caminhos para os seus arquivos
# -----------------------------------------------------------------------------
caminhos_rede <- c(
  Atual = "data/r5r_atual",
  Intervencao = "data/r5r_intervencao"
)

caminhos_edificacoes <- c(
  Atual = "data/edificacoes/edificacoes.gpkg",
  Intervencao = "data/edificacoes/intervencoes.gpkg"
)

pasta_saida <- "data/img/comparacao_cenarios"

data_hora_saida <- as.POSIXct("2026-10-13 08:50:00", tz = "America/Bahia")
janela_minutos <- 60L
percentil <- 50L
modos <- c("WALK", "TRANSIT")
max_viagens_transporte <- 1L
# 31 min mantém viagens de exatamente 30 minutos disponíveis para o corte <= 30.
duracao_max_acessibilidade <- 31L

# FALSE reutiliza network.dat, se existir na pasta do cenário.
# Altere para TRUE se você modificou algum GTFS, PBF ou TIF e precisa reconstruir.
reconstruir_redes <- FALSE

if (!dir.exists(pasta_saida)) dir.create(pasta_saida, recursive = TRUE)

# Verifica os caminhos das redes e os arquivos necessários para cada cenário.
for (cenario in names(caminhos_rede)) {
  pasta <- caminhos_rede[[cenario]]
  if (!dir.exists(pasta)) {
    stop("A pasta de rede do cenário '", cenario, "' não existe: ", pasta)
  }
  
  arquivos_pbf <- list.files(pasta, pattern = "\\.pbf$", ignore.case = TRUE)
  arquivos_gtfs <- list.files(pasta, pattern = "\\.zip$", ignore.case = TRUE)
  arquivos_tif <- list.files(pasta, pattern = "\\.tif$", ignore.case = TRUE)
  
  if (length(arquivos_pbf) != 1L) {
    stop("A pasta ", pasta, " deve conter exatamente um arquivo .pbf de rede viária.")
  }
  if (length(arquivos_gtfs) < 1L) {
    stop("A pasta ", pasta, " precisa conter pelo menos um GTFS em .zip.")
  }
  if (length(arquivos_tif) > 1L) {
    stop("A pasta ", pasta, " deve conter no máximo um arquivo .tif de elevação.")
  }
}

for (cenario in names(caminhos_edificacoes)) {
  caminho <- caminhos_edificacoes[[cenario]]
  if (!file.exists(caminho)) {
    stop("O GeoPackage de edificações do cenário '", cenario, "' não foi encontrado: ", caminho)
  }
}

# -----------------------------------------------------------------------------
# 2. Ler cada GeoPackage e criar pontos representativos de suas edificações
# -----------------------------------------------------------------------------
ler_unidades <- function(caminho, cenario) {
  edificacoes <- sf::st_read(caminho, quiet = TRUE)
  
  if (!"name" %in% names(edificacoes)) {
    stop("O GeoPackage do cenário '", cenario, "' precisa ter um campo chamado 'name'.")
  }
  if (is.na(sf::st_crs(edificacoes))) {
    stop("O GeoPackage do cenário '", cenario, "' não informa seu CRS.")
  }
  
  nomes <- trimws(as.character(edificacoes$name))
  if (any(is.na(nomes) | nomes == "")) {
    stop("Há valores de 'name' vazios no GeoPackage do cenário '", cenario,
         "'. Preencha os nomes antes de comparar os cenários.")
  }
  if (anyDuplicated(nomes)) {
    repetidos <- unique(nomes[duplicated(nomes)])
    stop("O campo 'name' precisa identificar cada unidade de forma única. Duplicados em '",
         cenario, "': ", paste(head(repetidos, 10), collapse = ", "))
  }
  
  # Salvador: calcular o ponto-na-superfície em CRS projetado e depois usar WGS84.
  pontos <- edificacoes |>
    mutate(.nome_unidade = nomes) |>
    sf::st_transform(31984) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)
  
  coords <- sf::st_coordinates(pontos)
  data.frame(
    unidade = pontos$.nome_unidade,
    lon = coords[, 1],
    lat = coords[, 2],
    stringsAsFactors = FALSE
  )
}

unidades_atual <- ler_unidades(caminhos_edificacoes[["Atual"]], "Atual")
unidades_intervencao <- ler_unidades(caminhos_edificacoes[["Intervencao"]], "Intervencao")

# Para comparar os cenários de forma direta, os conjuntos de unidades precisam
# ser iguais. As geometrias podem variar, mas cada unidade precisa existir nos dois.
nomes_atual <- sort(unidades_atual$unidade)
nomes_intervencao <- sort(unidades_intervencao$unidade)

if (!identical(nomes_atual, nomes_intervencao)) {
  faltam_na_intervencao <- setdiff(nomes_atual, nomes_intervencao)
  faltam_no_atual <- setdiff(nomes_intervencao, nomes_atual)
  detalhes <- c(
    if (length(faltam_na_intervencao)) paste0(
      "Presentes no atual, mas ausentes na intervenção: ",
      paste(head(faltam_na_intervencao, 10), collapse = ", ")
    ),
    if (length(faltam_no_atual)) paste0(
      "Presentes na intervenção, mas ausentes no atual: ",
      paste(head(faltam_no_atual, 10), collapse = ", ")
    )
  )
  stop(
    "Os GeoPackages não contêm o mesmo conjunto de unidades. Para comparar os cenários ",
    "com o mesmo denominador, padronize o campo 'name' nos dois arquivos. ",
    paste(detalhes, collapse = " | ")
  )
}

# IDs estáveis gerados a partir do nome ordenado, e não da ordem das feições.
# Assim, uma mesma unidade recebe o mesmo ID mesmo que a ordem no GeoPackage mude.
cadastro_unidades <- data.frame(
  id = sprintf("UFBA_%03d", seq_along(nomes_atual)),
  unidade = nomes_atual,
  stringsAsFactors = FALSE
)

preparar_origens_destinos <- function(unidades, cadastro) {
  idx <- match(unidades$unidade, cadastro$unidade)
  data.frame(
    id = cadastro$id[idx],
    lon = unidades$lon,
    lat = unidades$lat,
    stringsAsFactors = FALSE
  )
}

origens_destinos_atual <- preparar_origens_destinos(unidades_atual, cadastro_unidades)
origens_destinos_intervencao <- preparar_origens_destinos(unidades_intervencao, cadastro_unidades)

# -----------------------------------------------------------------------------
# 3. Calcular os tempos de viagem em cada rede, separadamente
# -----------------------------------------------------------------------------
calcular_tempo_viagem <- function(caminho_rede, origens_destinos, cenario) {
  message("Construindo/carregando a rede do cenário ", cenario, "...")
  
  # É ESTE argumento que indica ao r5r qual pasta utilizar.
  # Dentro dela, o r5r lê o PBF e o(s) GTFS ZIP (e o TIF, se existir).
  # O GeoPackage de edificações não é usado para construir a rede viária.
  rede <- r5r::build_network(
    data_path = caminho_rede,
    verbose = FALSE,
    overwrite = reconstruir_redes
  )
  
  tryCatch(
    r5r::travel_time_matrix(
      r5r_network = rede,
      origins = origens_destinos,
      destinations = origens_destinos,
      departure_datetime = data_hora_saida,
      mode = modos,
      max_rides = max_viagens_transporte,
      percentiles = percentil,
      max_trip_duration = duracao_max_acessibilidade,
      time_window = janela_minutos,
      verbose = FALSE,
      progress = TRUE
    ),
    finally = r5r::stop_r5(rede)
  )
}

message("Calculando cenário atual...")
ttm_atual <- calcular_tempo_viagem(
  caminho_rede = caminhos_rede[["Atual"]],
  origens_destinos = origens_destinos_atual,
  cenario = "Atual"
)

message("Calculando cenário com intervenção...")
ttm_intervencao <- calcular_tempo_viagem(
  caminho_rede = caminhos_rede[["Intervencao"]],
  origens_destinos = origens_destinos_intervencao,
  cenario = "Intervenção"
)

# -----------------------------------------------------------------------------
# 4. Indicadores de acessibilidade por unidade, em cada cenário
# -----------------------------------------------------------------------------
criar_indicadores <- function(ttm, cadastro) {
  resumo <- ttm |>
    filter(from_id != to_id, !is.na(travel_time_p50)) |>
    group_by(from_id) |>
    summarise(
      unidades_ate_15 = sum(travel_time_p50 <= 15, na.rm = TRUE),
      unidades_ate_30 = sum(travel_time_p50 <= 30, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(id = from_id)
  
  total_outras_unidades <- max(1L, nrow(cadastro) - 1L)
  
  cadastro |>
    left_join(resumo, by = "id") |>
    mutate(
      unidades_ate_15 = coalesce(as.integer(unidades_ate_15), 0L),
      unidades_ate_30 = coalesce(as.integer(unidades_ate_30), 0L),
      percentual_ate_15 = round(100 * unidades_ate_15 / total_outras_unidades, 1),
      percentual_ate_30 = round(100 * unidades_ate_30 / total_outras_unidades, 1)
    )
}

indicadores_atual <- criar_indicadores(ttm_atual, cadastro_unidades)
indicadores_intervencao <- criar_indicadores(ttm_intervencao, cadastro_unidades)

# Compara pelo ID estável de cada unidade e calcula a diferença em pontos percentuais.
comparacao <- indicadores_atual |>
  transmute(
    id, unidade,
    percentual_15_atual = percentual_ate_15,
    percentual_30_atual = percentual_ate_30
  ) |>
  left_join(
    indicadores_intervencao |>
      transmute(
        id,
        percentual_15_intervencao = percentual_ate_15,
        percentual_30_intervencao = percentual_ate_30
      ),
    by = "id"
  ) |>
  mutate(
    diferenca_15_pp = round(percentual_15_intervencao - percentual_15_atual, 1),
    diferenca_30_pp = round(percentual_30_intervencao - percentual_30_atual, 1)
  )

# -----------------------------------------------------------------------------
# 5. Gráficos comparativos: cenário atual (claro) vs. intervenção (escuro)
# -----------------------------------------------------------------------------
subtitulo <- paste0(
  "Caminhada + transporte público | saída em ",
  format(data_hora_saida, "%d/%m/%Y às %H:%M"),
  " | mediana de partidas numa janela de ", janela_minutos, " min"
)

criar_grafico_comparacao <- function(dados, limite = 15L) {
  if (limite == 15L) {
    coluna_atual <- "percentual_15_atual"
    coluna_intervencao <- "percentual_15_intervencao"
    coluna_diferenca <- "diferenca_15_pp"
    cores <- c("Cenário atual" = "#9ECAE1", "Intervenção" = "#2171B5")
    corte <- "15 minutos"
    nome_arquivo <- "grafico_acessibilidade_15min.png"
  } else {
    coluna_atual <- "percentual_30_atual"
    coluna_intervencao <- "percentual_30_intervencao"
    coluna_diferenca <- "diferenca_30_pp"
    cores <- c("Cenário atual" = "#FDBB84", "Intervenção" = "#E6550D")
    corte <- "30 minutos"
    nome_arquivo <- "grafico_acessibilidade_30min.png"
  }
  
  dados_plot <- dados |>
    arrange(.data[[coluna_diferenca]], unidade) |>
    mutate(unidade_plot = factor(unidade, levels = unique(unidade)))
  
  grafico <- ggplot(dados_plot, aes(y = unidade_plot)) +
    geom_segment(
      aes(x = .data[[coluna_atual]], xend = .data[[coluna_intervencao]],
          yend = unidade_plot),
      color = "#C8D0D9", linewidth = 1.1
    ) +
    geom_point(
      aes(x = .data[[coluna_atual]], color = "Cenário atual"),
      size = 3.2
    ) +
    geom_point(
      aes(x = .data[[coluna_intervencao]], color = "Intervenção"),
      size = 3.5
    ) +
    scale_color_manual(name = "Cenário", values = cores) +
    scale_x_continuous(
      breaks = seq(0, 100, by = 20),
      labels = function(x) paste0(x, "%"),
      limits = c(-10, 110),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = paste0("Acessibilidade em até ", corte, ": atual vs. intervenção"),
      subtitle = subtitulo,
      x = "Percentual de outras unidades alcançáveis",
      y = NULL,
      caption = paste0(
        "Cada ponto representa o percentual de unidades alcançáveis. ",
        "A linha conecta os cenários; diferença = intervenção − atual (pontos percentuais)."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 17, color = "#172B4D"),
      plot.subtitle = element_text(size = 10.5, color = "#52606D"),
      axis.text.y = element_text(size = 9, color = "#243447"),
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      plot.caption = element_text(size = 8.5, color = "#52606D", hjust = 0),
      plot.margin = margin(14, 22, 14, 14)
    )
  
  altura <- max(7, 2.5 + nrow(dados_plot) * 0.30)
  ggsave(
    filename = file.path(pasta_saida, nome_arquivo),
    plot = grafico,
    width = 12,
    height = altura,
    units = "in",
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
  
  grafico
}

grafico_15 <- criar_grafico_comparacao(comparacao, limite = 15L)
grafico_30 <- criar_grafico_comparacao(comparacao, limite = 30L)

message("Comparação concluída. Gráficos salvos em: ", normalizePath(pasta_saida))
