# Análise de acessibilidade entre unidades da UFBA
# Indicador: número de OUTRAS unidades alcançáveis em até 15 e 30 minutos,
# usando o tempo mediano (percentil 50) de viagens iniciadas numa janela de 60 min.

# Defina a memória antes de carregar o r5r, pois o motor utiliza Java.
options(java.parameters = "-Xmx4G")

library(r5r)
library(sf)
library(dplyr)
library(ggplot2)


# Caminhos e parâmetros do cenário
caminho_rede <- "data/r5r"
caminho_edificacoes <- "data/edificacoes/intervencoes.gpkg"
pasta_saida <- "data/img/resultados_acessibilidade"

# mantenha uma data válida para o calendário do GTFS (calendar.txt).
data_hora_saida <- as.POSIXct("2026-10-13 08:50:00", tz = "America/Bahia")
janela_minutos <- 60L
percentil <- 50L
modos <- c("WALK", "TRANSIT")
max_viagens_transporte <- 1L
duracao_max_acessibilidade <- 31L  # suficiente para os limites de 15 e 30 min
duracao_max_espera <- 120L        # permite examinar viagens mais longas na análise de espera

if (!dir.exists(caminho_rede)) {
  stop("A pasta da rede não foi encontrada: ", caminho_rede)
}
if (!file.exists(caminho_edificacoes)) {
  stop("O arquivo de edificações não foi encontrado: ", caminho_edificacoes)
}
if (!dir.exists(pasta_saida)) dir.create(pasta_saida, recursive = TRUE)


# Ler as unidades e gerar um ponto representativo por polígono
edif <- st_read(caminho_edificacoes, quiet = TRUE)

if (!"name" %in% names(edif)) {
  stop("O GeoPackage precisa ter um campo chamado 'name' com o nome da unidade.")
}
if (is.na(st_crs(edif))) {
  stop("O GeoPackage não informa seu sistema de coordenadas (CRS).")
}

# Salvador está na zona UTM 24S; calcular o ponto em CRS projetado evita
# calcular ponto-na-superfície diretamente em coordenadas geográficas.
edif_points <- edif |>
  st_transform(31984) |>
  st_point_on_surface() |>
  st_transform(4326)

coordenadas <- st_coordinates(edif_points)
nome_unidade <- trimws(as.character(edif_points$name))
sem_nome <- is.na(nome_unidade) | nome_unidade == ""
nome_unidade[sem_nome] <- paste0("Unidade ", which(sem_nome))

# IDs técnicos únicos mesmo quando há nomes repetidos no GeoPackage.
id_unidade <- sprintf("UFBA_%03d", seq_len(nrow(edif_points)))
rotulo_unidade <- make.unique(nome_unidade, sep = " — ")

origens_destinos <- data.frame(
  id = id_unidade,
  lon = coordenadas[, 1],
  lat = coordenadas[, 2]
)

cadastro_unidades <- data.frame(
  id = id_unidade,
  unidade = rotulo_unidade
)


# Calcular a matriz simples de acessibilidade e a matriz expandida de espera.
# A rede permanece aberta para as duas chamadas e é encerrada ao final.
r5r_network <- build_network(caminho_rede, verbose = FALSE)

tryCatch({
  # Para os limites de 15 e 30 min, não é necessário calcular viagens mais longas.
  # 31 min mantém viagens de exatamente 30 min disponíveis para a contagem (<= 30).
  ttm <- r5r::travel_time_matrix(
    r5r_network = r5r_network,
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
  )

  # A matriz expandida fornece tempos por partida simulada. breakdown = TRUE
  # inclui wait_time, total_time e os demais componentes da viagem.
  ettm <- r5r::expanded_travel_time_matrix(
    r5r_network = r5r_network,
    origins = origens_destinos,
    destinations = origens_destinos,
    departure_datetime = data_hora_saida,
    mode = modos,
    max_rides = max_viagens_transporte,
    max_trip_duration = duracao_max_espera,
    time_window = janela_minutos,
    breakdown = TRUE,
    verbose = FALSE,
    progress = TRUE
  )
}, finally = r5r::stop_r5(r5r_network))


# Indicadores por unidade: destinos distintos alcançáveis em até 15/30 min
# O próprio ponto de origem é excluído (from_id != to_id), para não contar a
# unidade como uma oportunidade acessada a partir dela mesma.
resumo <- ttm |>
  filter(from_id != to_id, !is.na(travel_time_p50)) |>
  group_by(from_id) |>
  summarise(
    unidades_ate_15 = sum(travel_time_p50 <= 15, na.rm = TRUE),
    unidades_ate_30 = sum(travel_time_p50 <= 30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(id = from_id)

indicadores <- cadastro_unidades |>
  left_join(resumo, by = "id") |>
  mutate(
    unidades_ate_15 = coalesce(as.integer(unidades_ate_15), 0L),
    unidades_ate_30 = coalesce(as.integer(unidades_ate_30), 0L),
    percentual_ate_15 = round(100 * unidades_ate_15 / max(1, n() - 1), 1),
    percentual_ate_30 = round(100 * unidades_ate_30 / max(1, n() - 1), 1)
  ) |>
  arrange(desc(unidades_ate_30), desc(unidades_ate_15))

# CSV com os indicadores absolutos e percentuais para cada unidade.
# write.csv2(
#   indicadores |> select(unidade, unidades_ate_15, unidades_ate_30,
#                         percentual_ate_15, percentual_ate_30),
#   file = file.path(pasta_saida, "indicadores_acessibilidade_ufba.csv"),
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )

# Gráfico comparativo pronto para compartilhar
ordem <- indicadores$unidade[order(indicadores$unidades_ate_30,
                                    indicadores$unidades_ate_15)]
indicadores$unidade_plot <- factor(indicadores$unidade, levels = ordem)

subtitulo <- paste0(
  "Caminhada + transporte público | partida em ",
  format(data_hora_saida, "%d/%m/%Y às %H:%M"),
  " | mediana de uma janela de ", janela_minutos, " minutos"
)

legenda_metodo <- paste0(
  "Cada unidade conta como 1 destino potencial; a própria unidade é excluída. "
)

grafico <- ggplot(indicadores, aes(y = unidade_plot)) +
  geom_segment(
    aes(x = percentual_ate_15, xend = percentual_ate_30,
        yend = unidade_plot),
    color = "#D0D7DE", linewidth = 1.1
  ) +
  geom_point(aes(x = percentual_ate_15, color = "Até 15 min"), size = 3.2) +
  geom_point(aes(x = percentual_ate_30, color = "Até 30 min"), size = 3.5) +
  scale_color_manual(
    name = "Tempo de viagem",
    values = c("Até 15 min" = "#D97706", "Até 30 min" = "#0077B6")
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Acessibilidade entre unidades da UFBA",
    subtitle = subtitulo,
    x = "Percentual de outras unidades alcançáveis",
    y = NULL,
    caption = legenda_metodo
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#172B4D"),
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

altura <- max(7, 2.5 + nrow(indicadores) * 0.30)
ggsave(
  filename = file.path(pasta_saida, "grafico_acessibilidade_ufba.png"),
  plot = grafico,
  width = 12,
  height = altura,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

# Análise de espera: qual parcela do tempo total é consumida esperando?
# Considera viagens entre unidades diferentes que usaram transporte público.
# Em cada partida simulada: percentual_espera = wait_time / total_time * 100.
colunas_necessarias <- c(
  "from_id", "to_id", "wait_time", "total_time", "n_rides",
  "access_time", "ride_time", "transfer_time", "egress_time"
)

if (!all(colunas_necessarias %in% names(ettm))) {
  stop(
    "A matriz expandida não contém todas as colunas esperadas. Colunas recebidas: ",
    paste(names(ettm), collapse = ", ")
  )
}

espera_por_par <- ettm |>
  filter(
    from_id != to_id,
    n_rides > 0,
    !is.na(wait_time),
    !is.na(total_time),
    total_time > 0
  ) |>
  mutate(percentual_espera = 100 * wait_time / total_time) |>
  group_by(from_id, to_id) |>
  summarise(
    espera_min_p50 = median(wait_time, na.rm = TRUE),
    tempo_total_min_p50 = median(total_time, na.rm = TRUE),
    percentual_espera_p50 = median(percentual_espera, na.rm = TRUE),
    percentual_espera_max = max(percentual_espera, na.rm = TRUE),
    acesso_min_p50 = median(access_time, na.rm = TRUE),
    deslocamento_veiculo_min_p50 = median(ride_time, na.rm = TRUE),
    transferencia_min_p50 = median(transfer_time, na.rm = TRUE),
    caminhada_final_min_p50 = median(egress_time, na.rm = TRUE),
    observacoes_simuladas = n(),
    .groups = "drop"
  ) |>
  left_join(
    cadastro_unidades |> rename(from_id = id, origem = unidade),
    by = "from_id"
  ) |>
  left_join(
    cadastro_unidades |> rename(to_id = id, destino = unidade),
    by = "to_id"
  ) |>
  mutate(viagem = paste(origem, "→", destino)) |>
  arrange(desc(percentual_espera_max))

if (nrow(espera_por_par) == 0) {
  stop(
    "Nenhuma viagem elegível com transporte público foi encontrada na matriz expandida. ",
    "Confira a data do GTFS, o intervalo de horários, os pontos e a duração máxima."
  )
}

# CSV completo: uma linha por par origem-destino, com a mediana e o pico observado.
# write.csv2(
#   espera_por_par |>
#     select(
#       origem, destino, espera_min_p50, tempo_total_min_p50,
#       percentual_espera_p50, percentual_espera_max,
#       acesso_min_p50, deslocamento_veiculo_min_p50,
#       transferencia_min_p50, caminhada_final_min_p50,
#       observacoes_simuladas
#     ),
#   file = file.path(pasta_saida, "indicadores_espera_ufba.csv"),
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )

# View(espera_por_par |>
#        select(
#          origem, destino, espera_min_p50, tempo_total_min_p50,
#          percentual_espera_p50, percentual_espera_max,
#          acesso_min_p50, deslocamento_veiculo_min_p50,
#          transferencia_min_p50, caminhada_final_min_p50,
#          observacoes_simuladas
#        ))
# 
# Gráfico compartilhável: 20 pares com maior percentual de espera observado.
# O ponto laranja é a mediana entre as partidas; o azul é o maior valor da janela.
top_espera <- espera_por_par |>
  slice_max(order_by = percentual_espera_max, n = 20, with_ties = FALSE) |>
  arrange(percentual_espera_max) |>
  mutate(
    viagem_plot = make.unique(as.character(viagem), sep = " — "),
    viagem_plot = factor(viagem_plot, levels = viagem_plot)
  )

subtitulo_espera <- paste0(
  "Caminhada + transporte público | partida a partir de ",
  format(data_hora_saida, "%d/%m/%Y às %H:%M"),
  " | janela de ", janela_minutos, " min | máximo de ",
  duracao_max_espera, " min por viagem"
)

legenda_espera <- paste0(
  "Somente viagens com transporte público entre unidades distintas. ",
  "Percentual = wait_time / total_time. Cada par resume as partidas simuladas na janela."
)

grafico_espera <- ggplot(top_espera, aes(y = viagem_plot)) +
  geom_segment(
    aes(x = percentual_espera_p50, xend = percentual_espera_max,
        yend = viagem_plot),
    color = "#D0D7DE", linewidth = 1.1
  ) +
  geom_point(
    aes(x = percentual_espera_p50, color = "Mediana na janela"),
    size = 3.2
  ) +
  geom_point(
    aes(x = percentual_espera_max, color = "Maior valor observado"),
    size = 3.5
  ) +
  geom_text(
    aes(x = percentual_espera_max, label = paste0(round(percentual_espera_max, 1), "%")),
    hjust = 1.2, size = 3, color = "#243447"
  ) +
  scale_color_manual(
    name = "Parcela do tempo total em espera",
    values = c("Mediana na janela" = "#D97706", "Maior valor observado" = "#0077B6")
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Quanto tempo das viagens é consumido esperando?",
    subtitle = subtitulo_espera,
    x = "Espera como percentual do tempo total da viagem",
    y = NULL,
    caption = legenda_espera
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#172B4D"),
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

altura_espera <- max(7, 2.5 + nrow(top_espera) * 0.36)
ggsave(
  filename = file.path(pasta_saida, "grafico_tempo_espera_ufba.png"),
  plot = grafico_espera,
  width = 14,
  height = altura_espera,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

message("Análises concluídas. Arquivos salvos em: ", normalizePath(pasta_saida))
