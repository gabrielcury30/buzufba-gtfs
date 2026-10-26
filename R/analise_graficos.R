criar_grafico_comparacao <- function(dados,
                                     subtitulo,
                                     pasta_figuras,
                                     limite = 15L) {
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
    dplyr::arrange(.data[[coluna_diferenca]], unidade) |>
    dplyr::mutate(unidade_plot = factor(unidade, levels = unique(unidade)))

  grafico <- ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(y = unidade_plot)
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data[[coluna_atual]],
        xend = .data[[coluna_intervencao]],
        yend = unidade_plot
      ),
      color = "#C8D0D9",
      linewidth = 1.1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data[[coluna_atual]], color = "Cenário atual"),
      size = 3.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data[[coluna_intervencao]], color = "Intervenção"),
      size = 3.5
    ) +
    ggplot2::scale_color_manual(name = "Cenário", values = cores) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, by = 20),
      labels = function(x) paste0(x, "%"),
      limits = c(-10, 110),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = paste0(
        "Acessibilidade em até ", corte, ": atual vs. intervenção"
      ),
      subtitle = subtitulo,
      x = "Percentual de outras unidades alcançáveis",
      y = NULL,
      caption = paste0(
        "Cada ponto representa o percentual de unidades alcançáveis. ",
        "A linha conecta os cenários; diferença = intervenção − atual ",
        "(pontos percentuais)."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 17, color = "#172B4D"
      ),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#52606D"),
      axis.text.y = ggplot2::element_text(size = 9, color = "#243447"),
      axis.title.x = ggplot2::element_text(
        face = "bold", margin = ggplot2::margin(t = 10)
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      plot.caption = ggplot2::element_text(
        size = 8.5, color = "#52606D", hjust = 0
      ),
      plot.margin = ggplot2::margin(14, 22, 14, 14)
    )

  altura <- max(7, 2.5 + nrow(dados_plot) * 0.30)

  ggplot2::ggsave(
    filename = file.path(pasta_figuras, nome_arquivo),
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

criar_grafico_tempo_total_fluxos <- function(comparacao_tempo_total,
                                             subtitulo_tempo_total,
                                             max_viagens_transporte,
                                             duracao_max_viagem,
                                             pasta_figuras) {
  grafico_tempo_total <- ggplot2::ggplot(
    comparacao_tempo_total,
    ggplot2::aes(y = fluxo_plot)
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = tempo_total_atual,
        xend = tempo_total_intervencao,
        yend = fluxo_plot
      ),
      color = "#C8D0D9",
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = tempo_total_atual, color = "Cenário atual"),
      size = 3.2,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = tempo_total_intervencao,
        color = "Intervenção"
      ),
      size = 3.5,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      name = "Cenário",
      values = c("Cenário atual" = "#9ECAE1", "Intervenção" = "#2171B5")
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, " min"),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.06))
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "Tempo total da viagem por fluxo: atual vs. intervenção",
      subtitle = subtitulo_tempo_total,
      x = "Tempo total da viagem (minutos)",
      y = NULL,
      caption = paste0(
        "Fluxos ida e volta entre Ondina e Engenharia, Canela e Arquitetura. ",
        "\n",
        "PAF 1 representa Ondina; Politécnica representa Engenharia; PAC ",
        "representa Canela; e a unidade selecionada representa Arquitetura.\n",
        "Cada ponto resume pela mediana o total_time, em minutos, das rotas ",
        "mais rápidas por partida.\n",
        "São consideradas caminhada e transporte público, incluindo viagens ",
        "só a pé (n_rides = 0); max_rides = ", max_viagens_transporte,
        "; duração máxima = ", duracao_max_viagem, " min",
        ". Pontos ausentes indicam ausência de viagem elegível no cenário."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 17, color = "#172B4D"
      ),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#52606D"),
      axis.text.y = ggplot2::element_text(size = 10, color = "#243447"),
      axis.title.x = ggplot2::element_text(
        face = "bold", margin = ggplot2::margin(t = 10)
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      plot.caption = ggplot2::element_text(
        size = 8.5, color = "#52606D", hjust = 0
      ),
      plot.margin = ggplot2::margin(14, 22, 14, 14)
    )

  ggplot2::ggsave(
    filename = file.path(
      pasta_figuras,
      "grafico_tempo_total_viagem_fluxos_ufba.png"
    ),
    plot = grafico_tempo_total,
    width = 14,
    height = 7,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  grafico_tempo_total
}
