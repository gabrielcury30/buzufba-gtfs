# Comparação de acessibilidade entre os cenários atual e com intervenção da UFBA

options(java.parameters = "-Xmx4G")

localizar_raiz_projeto <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)
  caminho_script <- NULL

  if (length(argumento_arquivo) > 0) {
    caminho_script <- sub("^--file=", "", argumento_arquivo[1])
    if (identical(caminho_script, "-") || !file.exists(caminho_script)) {
      caminho_script <- NULL
    }
  }

  if (is.null(caminho_script)) {
    arquivos_origem <- vapply(
      sys.frames(),
      function(quadro) {
        if (is.null(quadro$ofile)) "" else quadro$ofile
      },
      character(1)
    )
    arquivos_origem <- arquivos_origem[nzchar(arquivos_origem)]

    if (length(arquivos_origem) > 0) {
      caminho_script <- tail(arquivos_origem, 1)
    }
  }

  if (is.null(caminho_script)) {
    diretorio <- normalizePath(getwd(), mustWork = TRUE)
  } else {
    diretorio <- dirname(normalizePath(caminho_script, mustWork = TRUE))
  }

  repeat {
    if (file.exists(file.path(diretorio, "buzufba-gtfs.Rproj"))) {
      return(diretorio)
    }

    diretorio_pai <- dirname(diretorio)
    if (identical(diretorio_pai, diretorio)) {
      stop(
        "Não foi possível localizar a raiz do projeto BUZUFBA GTFS. ",
        "Execute o script dentro do projeto ou por meio do caminho do arquivo.",
        call. = FALSE
      )
    }

    diretorio <- diretorio_pai
  }
}

diretorio_projeto <- localizar_raiz_projeto()

pacotes_necessarios <- c("dplyr", "ggplot2", "r5r", "sf")
pacotes_ausentes <- pacotes_necessarios[
  !vapply(pacotes_necessarios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacotes_ausentes) > 0) {
  stop(
    "Instale os pacotes R necessários: ",
    paste(pacotes_ausentes, collapse = ", "),
    call. = FALSE
  )
}

arquivos_funcoes <- c(
  "analise_dados.R",
  "analise_calculos.R",
  "analise_graficos.R"
)

for (arquivo in arquivos_funcoes) {
  source(file.path(diretorio_projeto, "R", arquivo), local = FALSE)
}

# Configuração: ajuste os dados de entrada e parâmetros da análise, se necessário
caminhos_rede <- c(
  Atual = file.path(diretorio_projeto, "data", "processed", "r5r"),
  Intervencao = file.path(diretorio_projeto, "data", "processed", "r5r_intervencao")
)

# Prepare cada pasta de rede com exatamente um PBF, pelo menos um GTFS ZIP e,
# opcionalmente, um TIF de elevação. Os arquivos salvador.osm.pbf e
# salvador_mde.tif estão em data/raw; use o GTFS gerado ou o arquivo de cada
# cenário. O R5 cria/reutiliza network.dat nessas pastas locais não versionadas.
caminhos_edificacoes <- c(
  Atual = file.path(
    diretorio_projeto, "data", "raw", "edificacoes_ufba.gpkg"
  ),
  Intervencao = file.path(
    diretorio_projeto, "data", "raw", "intervencoes_edificacoes_ufba.gpkg"
  )
)

pasta_figuras <- file.path(diretorio_projeto, "figures")

data_hora_saida <- as.POSIXct("2026-10-13 08:50:00", tz = "America/Bahia")
janela_minutos <- 60L
percentil <- 50L
modos <- c("WALK", "TRANSIT")
max_viagens_transporte <- 1L

# 31 min mantém viagens de exatamente 30 minutos disponíveis para o corte <= 30.
duracao_max_acessibilidade <- 31L
duracao_max_espera <- 120L

# Os valores à direita devem corresponder exatamente ao campo "name" dos
# GeoPackages.
unidades_representativas <- c(
  Ondina = "PAF I",
  Engenharia = "Escola Politécnica",
  Canela = "PAC",
  Arquitetura = "Faculdade de Arquitetura e Urbanismo"
)

# TRUE reconstrói network.dat quando as entradas mudam; FALSE reutiliza o
# arquivo.
reconstruir_redes <- TRUE

for (pasta_rede in caminhos_rede) {
  dir.create(pasta_rede, recursive = TRUE, showWarnings = FALSE)
}
dir.create(pasta_figuras, recursive = TRUE, showWarnings = FALSE)

validar_caminhos_entrada(caminhos_rede, caminhos_edificacoes)

# Preparação dos pontos representativos das unidades
unidades_atual <- ler_unidades(caminhos_edificacoes[["Atual"]], "Atual")
unidades_intervencao <- ler_unidades(
  caminhos_edificacoes[["Intervencao"]],
  "Intervenção"
)

validar_unidades_cenarios(
  unidades_atual,
  unidades_intervencao
)
cadastro_unidades <- criar_cadastro_unidades(unidades_atual)

origens_destinos_atual <- preparar_origens_destinos(
  unidades_atual,
  cadastro_unidades
)
origens_destinos_intervencao <- preparar_origens_destinos(
  unidades_intervencao,
  cadastro_unidades
)

# Matrizes de tempo de viagem e indicadores de acessibilidade
message("Calculando cenário atual...")
ttm_atual <- calcular_tempo_viagem(
  caminho_rede = caminhos_rede[["Atual"]],
  origens_destinos = origens_destinos_atual,
  cenario = "Atual",
  reconstruir_redes = reconstruir_redes,
  data_hora_saida = data_hora_saida,
  modos = modos,
  max_viagens_transporte = max_viagens_transporte,
  percentil = percentil,
  duracao_max_acessibilidade = duracao_max_acessibilidade,
  janela_minutos = janela_minutos
)

message("Calculando cenário com intervenção...")
ttm_intervencao <- calcular_tempo_viagem(
  caminho_rede = caminhos_rede[["Intervencao"]],
  origens_destinos = origens_destinos_intervencao,
  cenario = "Intervenção",
  reconstruir_redes = reconstruir_redes,
  data_hora_saida = data_hora_saida,
  modos = modos,
  max_viagens_transporte = max_viagens_transporte,
  percentil = percentil,
  duracao_max_acessibilidade = duracao_max_acessibilidade,
  janela_minutos = janela_minutos
)

indicadores_atual <- criar_indicadores(ttm_atual, cadastro_unidades)
indicadores_intervencao <- criar_indicadores(
  ttm_intervencao,
  cadastro_unidades
)
comparacao <- comparar_indicadores(
  indicadores_atual,
  indicadores_intervencao
)

subtitulo <- paste0(
  "Caminhada + transporte público | saída em ",
  format(data_hora_saida, "%d/%m/%Y às %H:%M"),
  " | mediana de partidas numa janela de ", janela_minutos, " min"
)

grafico_15 <- criar_grafico_comparacao(
  dados = comparacao,
  subtitulo = subtitulo,
  pasta_figuras = pasta_figuras,
  limite = 15L
)
grafico_30 <- criar_grafico_comparacao(
  dados = comparacao,
  subtitulo = subtitulo,
  pasta_figuras = pasta_figuras,
  limite = 30L
)

# Composição do tempo de espera nos fluxos representativos
ids_representantes <- criar_ids_representantes(
  cadastro_unidades,
  unidades_representativas
)

pontos_fluxos_atual <- selecionar_pontos_representativos(
  origens_destinos_atual,
  ids_representantes,
  "Atual"
)
pontos_fluxos_intervencao <- selecionar_pontos_representativos(
  origens_destinos_intervencao,
  ids_representantes,
  "Intervenção"
)
fluxos_alvo <- criar_fluxos_alvo(ids_representantes)

espera_atual <- calcular_espera_fluxos(
  caminho_rede = caminhos_rede[["Atual"]],
  pontos = pontos_fluxos_atual,
  cenario = "Atual",
  data_hora_saida = data_hora_saida,
  modos = modos,
  max_viagens_transporte = max_viagens_transporte,
  duracao_max_espera = duracao_max_espera,
  janela_minutos = janela_minutos
) |>
  dplyr::rename(
    percentual_espera_atual = percentual_espera,
    observacoes_atual = observacoes
  )

espera_intervencao <- calcular_espera_fluxos(
  caminho_rede = caminhos_rede[["Intervencao"]],
  pontos = pontos_fluxos_intervencao,
  cenario = "Intervenção",
  data_hora_saida = data_hora_saida,
  modos = modos,
  max_viagens_transporte = max_viagens_transporte,
  duracao_max_espera = duracao_max_espera,
  janela_minutos = janela_minutos
) |>
  dplyr::rename(
    percentual_espera_intervencao = percentual_espera,
    observacoes_intervencao = observacoes
  )

comparacao_espera <- comparar_espera_fluxos(
  fluxos_alvo,
  espera_atual,
  espera_intervencao
)
avisar_fluxos_sem_rota(comparacao_espera)

subtitulo_espera <- paste0(
  "Caminhada + transporte público | saída em ",
  format(data_hora_saida, "%d/%m/%Y às %H:%M"),
  " | janela de ", janela_minutos,
  " min | mediana do percentual por partida"
)

grafico_espera <- criar_grafico_espera(
  comparacao_espera = comparacao_espera,
  subtitulo_espera = subtitulo_espera,
  max_viagens_transporte = max_viagens_transporte,
  pasta_figuras = pasta_figuras
)

message(
  "Comparação concluída. Gráficos salvos em: ",
  normalizePath(pasta_figuras)
)
