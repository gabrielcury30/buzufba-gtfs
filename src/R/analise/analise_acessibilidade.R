# Configuração de memória para o motor Java (necessário para r5r)
options(java.parameters = "-Xmx4G")

# Carregamento de bibliotecas para análise espacial, 
# manipulação de dados e visualização
library(r5r)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)
library(r5rgui)
library(tidyr)
library(scales)
library(forcats)

# Construção da rede de transporte a partir dos dados locais
r5r_network <- build_network("data/r5r")

# Leitura do arquivo geopackage com as unidades da UFBA
edif <- st_read("data/edificacoes/edif_ufba.gpkg")

# Criação de um objeto 'sf' apenas com os campos de interesse
edif_sf <- st_sf(
  osm_id = edif$osm_id,
  name = edif$name,
  geom = edif$geom
)

# Geração de pontos (centroides) para representar as unidades
edif_points <- edif %>%
  st_transform(4326) %>%
  st_point_on_surface()

# Criação de dataframe de origens/destinos (formatado para o r5r)
pod <- data.frame(
  id  = edif_points$name,
  lon = st_coordinates(edif_points)[, 1],
  lat = st_coordinates(edif_points)[, 2]
)

# Parâmetros de simulação de viagem
departure_datetime <- as.POSIXct("2026-06-04 10:40:00", tz = "America/Bahia")
time_window        <- 60L
max_rides          <- 1L
mode               <- c("WALK", "TRANSIT")
max_trip_duration  <- 120
percentiles        <- c(25, 50, 75, 90)

# Cálculo da matriz de tempo de viagem entre unidades
ttm <- travel_time_matrix(
  r5r_network       = r5r_network,
  origins           = pod,
  destinations      = pod,
  departure_datetime = departure_datetime,
  mode              = mode,
  max_rides         = max_rides,
  percentiles       = percentiles,
  max_trip_duration = max_trip_duration,
  time_window       = time_window
)

# Cálculo da matriz detalhada (componentes do tempo de viagem)
ettm <- expanded_travel_time_matrix(
  r5r_network        = r5r_network,
  origins            = pod,
  destinations       = pod,
  mode               = mode,
  departure_datetime = departure_datetime,
  max_rides          = max_rides,
  max_trip_duration  = max_trip_duration,
  breakdown          = TRUE,
  time_window        = time_window
)

# Extração de itinerários detalhados
det <- detailed_itineraries(
  r5r_network        = r5r_network,
  origins            = pod,
  destinations       = pod,
  mode               = mode,
  departure_datetime = departure_datetime,
  max_rides          = max_rides,
  max_trip_duration  = max_trip_duration,
  all_to_all         = TRUE,
  time_window        = time_window
)

# --- ANÁLISE — MATRIZ O-D: PERCENTIS E IMPREVISIBILIDADE ---
# Objetivo: Identificar pares de origens/destinos com alta variação no tempo 
# de viagem
# (p90 vs p25), revelando rotas onde a chegada é imprevisível.

# Cálculo da amplitude (imprevisibilidade)
ttm_analise <- ttm %>%
  mutate(amplitude_ip = travel_time_p90 - travel_time_p25)

# Ordenação das unidades pela mediana da amplitude para o heatmap
ordem_amplitude <- ttm_analise %>%
  group_by(from_id) %>%
  summarise(amplitude_mediana = median(amplitude_ip, na.rm = TRUE)) %>%
  arrange(desc(amplitude_mediana)) %>%
  pull(from_id)

# Aplicação da ordem aos dados para o plot
ttm_heatmap_amplitude <- ttm_analise %>%
  mutate(
    from_id = factor(from_id, levels = ordem_amplitude),
    to_id   = factor(to_id,   levels = ordem_amplitude)
  )

# Criação do heatmap de amplitude
plot_heatmap_amplitude <- ttm_heatmap_amplitude %>%
  ggplot(aes(x = to_id, y = from_id, fill = amplitude_ip)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "Amplitude\n(p90 - p25, min)",
    option = "inferno",
    na.value = "grey90"
  ) +
  labs(
    title    = "Amplitude do Tempo de Viagem entre Unidades da UFBA",
    subtitle = paste("Partida:", format(departure_datetime, "%d/%m/%Y %H:%M")),
    x        = "Destino",
    y        = "Origem"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    axis.text.y  = element_text(size = 7),
    plot.title   = element_text(face = "bold"),
    legend.position = "right"
  )

# Salvando o gráfico de amplitude
ggsave("data/img/heatmap_amplitude.png", plot_heatmap_amplitude,
       width = 14, height = 12, dpi = 150)

# Ordenação das unidades pela mediana do tempo (p50)
ordem_mediana <- ttm_analise %>%
  group_by(from_id) %>%
  summarise(mediana_global = median(travel_time_p50, na.rm = TRUE)) %>%
  arrange(mediana_global) %>%
  pull(from_id)

# Criação do heatmap de tempo mediano (p50)
plot_heatmap_p50 <- ttm_analise %>%
  mutate(from_id = factor(from_id, levels = ordem_mediana),
         to_id = factor(to_id, levels = ordem_mediana)) %>% 
  ggplot(aes(x = to_id, y = from_id, fill = travel_time_p50)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "",
    option = "inferno",
    na.value = "grey90"
  ) +
  labs(
    title    = "Tempo de Viagem Mediano(p50) entre Unidades da UFBA",
    subtitle = paste("Partida:", format(departure_datetime, "%d/%m/%Y %H:%M")),
    x        = "Destino",
    y        = "Origem"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title  = element_text(face = "bold")
  )

# Salvando o gráfico de tempo mediano
ggsave("data/img/heatmap_p50.png", plot_heatmap_p50,
       width = 14, height = 12, dpi = 150)

# --- ANÁLISE — ÍNDICE DE ACESSIBILIDADE ACUMULATIVA POR UNIDADE ---
# Objetivo: Contar quantas unidades são alcançáveis em cortes de tempo 
# (15, 30, 45 min).

cortes <- c(15, 30, 45)

# Cálculo de unidades alcançáveis por corte de tempo
acessibilidade_acumulativa <- lapply(cortes, function(corte) {
  ttm_analise %>%
    filter(travel_time_p50 <= corte, from_id != to_id) %>%
    count(from_id, name = "unidades_acessiveis") %>%
    mutate(corte_min = corte)
}) %>%
  bind_rows()

# Preparação de dataframe completo para preencher zeros
todas_unidades <- data.frame(from_id = unique(pod$id))

acessibilidade_acumulativa_completo <- expand.grid(
  from_id   = unique(pod$id),
  corte_min = cortes,
  stringsAsFactors = FALSE
) %>%
  left_join(acessibilidade_acumulativa, by = c("from_id", "corte_min")) %>%
  mutate(unidades_acessiveis = replace_na(unidades_acessiveis, 0))

n_unidades_total <- nrow(pod) - 1  # Exclui a própria unidade

# Criação do gráfico de barras de acessibilidade acumulativa
plot_acumulativo <- acessibilidade_acumulativa_completo %>%
  mutate(
    pct_acessivel = unidades_acessiveis / n_unidades_total * 100,
    corte_label   = paste0(corte_min, " min")
  ) %>%
  ggplot(aes(
    x    = reorder(from_id, unidades_acessiveis),
    y    = pct_acessivel,
    fill = corte_label
  )) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_viridis_d(name = "", option = "plasma") +
  coord_flip() +
  facet_wrap(~corte_label, nrow = 1) + 
  labs(
    title    = "Índice de Acessibilidade Acumulativa por Unidade da UFBA",
    subtitle = "% de outras unidades alcançáveis dentro de cada corte de tempo 
    (p50)",
    x        = "Unidade",
    y        = "% de unidades alcançáveis"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 10) 
  )

# Salvando o gráfico de acessibilidade
ggsave("data/img/acessibilidade_acumulativa.png"plot_acumulativo,
       width = 12, height = 10, dpi = 150)

# --- ANÁLISE — PENALIDADE DE ESPERA ---
# Objetivo: Identificar rotas onde o tempo de espera é uma proporção alta 
# do total,
# indicando ineficiência do sistema de ônibus.

ettm_transit <- ettm %>%
  filter(routes != "[WALK]") %>%
  filter(!is.na(total_time), total_time > 0) %>%
  mutate(
    proporcao_espera = wait_time / total_time,
    proporcao_caminhada = access_time / total_time
  )

# Criação do histograma da proporção de tempo de espera
plot_espera <- ettm_transit %>%
  ggplot(aes(x = proporcao_espera)) +
  geom_histogram(binwidth = 0.05, fill = "#2980B9", color = "white", 
                 alpha = 0.85) +
  geom_vline(xintercept = 0.4, color = "#C0392B", linetype = "dashed", 
             linewidth = 1) +
  annotate("text", x = 0.42, y = Inf, label = "Limiar 40%",
           vjust = 2, hjust = 0, color = "#C0392B", size = 3.5) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title    = "Distribuição da Proporção de Tempo de Espera em Rotas 
    com BuzUFBA",
    x        = "Tempo de espera / Tempo total de viagem",
    y        = "Frequência"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Salvando o gráfico de espera
ggsave("data/img/tempo_espera.png", plot_espera,
       width = 10, height = 6, dpi = 150)

# --- ANÁLISE — MODAL INEFICIENTE: TRANSIT MAIS LENTO QUE CAMINHADA ---
# Objetivo: Comparar tempo de viagem entre ônibus e caminhada.

# Cálculo do tempo mediano a pé
ettm_walk_summary <- ettm %>%
  filter(routes == "[WALK]") %>%
  group_by(from_id, to_id) %>%
  summarise(tempo_walk = median(total_time, na.rm = TRUE), .groups = "drop")

# Cálculo do tempo mediano com trânsito
ettm_transit_summary <- ettm_transit %>%
  group_by(from_id, to_id) %>%
  summarise(tempo_transit = median(total_time, na.rm = TRUE), .groups = "drop")

# Comparação entre os modais
comparacao_modal <- ettm_walk_summary %>%
  inner_join(ettm_transit_summary, by = c("from_id", "to_id")) %>%
  mutate(
    diferenca         = tempo_transit - tempo_walk,   
    transit_mais_lento = diferenca > 0
  )

# Criação do gráfico de dispersão da comparação modal
plot_modal <- comparacao_modal %>%
  ggplot(aes(x = tempo_walk, y = tempo_transit, color = diferenca)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "black", linewidth = 0.8) +
  scale_color_gradient2(
    low      = "#27AE60",
    mid      = "grey80",
    high     = "#C0392B",
    midpoint = 0,
    name     = "Diferença\n(transit - walk, min)"
  ) +
  labs(
    title    = "Comparação Modal: Ônibus vs. Caminhada",
    x        = "Tempo mediano a pé (min)",
    y        = "Tempo mediano com trânsito (min)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Salvando o gráfico de comparação modal
ggsave("data/img/comparacao_modal.png", plot_modal,
       width = 10, height = 8, dpi = 150)

# Encerramento do processo r5
stop_r5(r5r_network)
