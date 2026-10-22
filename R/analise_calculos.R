calcular_tempo_viagem <- function(caminho_rede,
                                  origens_destinos,
                                  cenario,
                                  reconstruir_redes,
                                  data_hora_saida,
                                  modos,
                                  max_viagens_transporte,
                                  percentil,
                                  duracao_max_acessibilidade,
                                  janela_minutos) {
  message("Construindo/carregando a rede do cenário ", cenario, "...")

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

criar_indicadores <- function(ttm, cadastro) {
  resumo <- ttm |>
    dplyr::filter(from_id != to_id, !is.na(travel_time_p50)) |>
    dplyr::group_by(from_id) |>
    dplyr::summarise(
      unidades_ate_15 = sum(travel_time_p50 <= 15, na.rm = TRUE),
      unidades_ate_30 = sum(travel_time_p50 <= 30, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(id = from_id)

  total_outras_unidades <- max(1L, nrow(cadastro) - 1L)

  cadastro |>
    dplyr::left_join(resumo, by = "id") |>
    dplyr::mutate(
      unidades_ate_15 = dplyr::coalesce(as.integer(unidades_ate_15), 0L),
      unidades_ate_30 = dplyr::coalesce(as.integer(unidades_ate_30), 0L),
      percentual_ate_15 = round(
        100 * unidades_ate_15 / total_outras_unidades, 1
      ),
      percentual_ate_30 = round(
        100 * unidades_ate_30 / total_outras_unidades, 1
      )
    )
}

comparar_indicadores <- function(indicadores_atual, indicadores_intervencao) {
  indicadores_atual |>
    dplyr::transmute(
      id,
      unidade,
      percentual_15_atual = percentual_ate_15,
      percentual_30_atual = percentual_ate_30
    ) |>
    dplyr::left_join(
      indicadores_intervencao |>
        dplyr::transmute(
          id,
          percentual_15_intervencao = percentual_ate_15,
          percentual_30_intervencao = percentual_ate_30
        ),
      by = "id"
    ) |>
    dplyr::mutate(
      diferenca_15_pp = round(
        percentual_15_intervencao - percentual_15_atual, 1
      ),
      diferenca_30_pp = round(
        percentual_30_intervencao - percentual_30_atual, 1
      )
    )
}

calcular_espera_fluxos <- function(caminho_rede,
                                   pontos,
                                   cenario,
                                   data_hora_saida,
                                   modos,
                                   max_viagens_transporte,
                                   duracao_max_espera,
                                   janela_minutos) {
  message(
    "Calculando composição do tempo de viagem para os fluxos de ",
    cenario, "..."
  )

  # Reutiliza o network.dat construído na etapa de acessibilidade.
  rede <- r5r::build_network(
    data_path = caminho_rede,
    verbose = FALSE,
    overwrite = FALSE
  )

  tryCatch({
    ettm <- r5r::expanded_travel_time_matrix(
      r5r_network = rede,
      origins = pontos,
      destinations = pontos,
      departure_datetime = data_hora_saida,
      mode = modos,
      max_rides = max_viagens_transporte,
      max_trip_duration = duracao_max_espera,
      time_window = janela_minutos,
      breakdown = TRUE,
      verbose = FALSE,
      progress = TRUE
    )

    colunas_necessarias <- c(
      "from_id", "to_id", "wait_time", "total_time", "n_rides"
    )

    if (!all(colunas_necessarias %in% names(ettm))) {
      stop(
        "A matriz expandida do cenário '", cenario,
        "' não contém as colunas esperadas. Recebi: ",
        paste(names(ettm), collapse = ", "),
        call. = FALSE
      )
    }

    # Resume pela mediana a proporção de espera de cada fluxo.
    ettm |>
      dplyr::filter(
        !is.na(n_rides), n_rides > 0,
        is.finite(wait_time), wait_time >= 0,
        is.finite(total_time), total_time > 0
      ) |>
      dplyr::mutate(percentual_espera = 100 * wait_time / total_time) |>
      dplyr::filter(is.finite(percentual_espera)) |>
      dplyr::group_by(from_id, to_id) |>
      dplyr::summarise(
        percentual_espera = median(percentual_espera, na.rm = TRUE),
        observacoes = dplyr::n(),
        .groups = "drop"
      )
  }, finally = r5r::stop_r5(rede))
}

comparar_espera_fluxos <- function(fluxos_alvo,
                                   espera_atual,
                                   espera_intervencao) {
  fluxos_alvo |>
    dplyr::left_join(espera_atual, by = c("from_id", "to_id")) |>
    dplyr::left_join(espera_intervencao, by = c("from_id", "to_id")) |>
    dplyr::mutate(
      diferenca_espera_pp = round(
        percentual_espera_intervencao - percentual_espera_atual, 1
      ),
      fluxo_plot = factor(fluxo, levels = rev(fluxos_alvo$fluxo))
    )
}

avisar_fluxos_sem_rota <- function(comparacao_espera) {
  sem_rota <- comparacao_espera |>
    dplyr::filter(
      is.na(percentual_espera_atual) |
        is.na(percentual_espera_intervencao)
    )

  if (nrow(sem_rota) > 0) {
    message(
      "Atenção: não foi possível calcular a espera em ambos os cenários para: ",
      paste(sem_rota$fluxo, collapse = "; "),
      ". Confira se há itinerários de transporte público viáveis ",
      "na janela selecionada."
    )
  }

  invisible(sem_rota)
}
