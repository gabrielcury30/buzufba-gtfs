# Ponto de entrada para gerar o feed GTFS do BUZUFBA

localizar_diretorio_projeto <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)

  if (length(argumento_arquivo) > 0) {
    caminho_script <- normalizePath(sub("^--file=", "", argumento_arquivo[1]),
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

diretorio_projeto <- localizar_diretorio_projeto()
arquivos_modulos <- file.path(diretorio_projeto, "R", c(
  "gtfs_data.R", "gtfs_routing.R",
  "gtfs_generator.R"
))

for (arquivo_modulo in arquivos_modulos) {
  if (!file.exists(arquivo_modulo)) {
    stop(sprintf("Módulo GTFS não encontrado: %s", arquivo_modulo), call. = FALSE)
  }
  source(arquivo_modulo, local = FALSE)
}

gerar_gtfs(diretorio_projeto)
