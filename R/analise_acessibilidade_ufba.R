options(java.parameters = "-Xmx4G")

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

r5r_network <- build_network("data/r5r")

edif <- st_read("data/edif/edif_ufba.gpkg")

edif_points <- edif %>%
  st_transform(4326) %>%
  st_point_on_surface()

pod <- data.frame(
  id  = edif_points$name,
  lon = st_coordinates(edif_points)[, 1],
  lat = st_coordinates(edif_points)[, 2]
)

departure_datetime <- as.POSIXct("2026-06-04 10:40:00", tz = "America/Bahia")
time_window        <- 20L
max_rides          <- 1L
mode               <- c("WALK", "TRANSIT")
max_trip_duration  <- 120
percentiles        <- c(25, 50, 75, 90)

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

# ANÁLISE — MATRIZ O-D: PERCENTIS E IMPREVISIBILIDADE
# Objetivo: identificar pares com alto tempo e/ou alta amplitude entre
# percentis (p25 vs p90), revelando rotas onde a chegada no horário
# é imprevisível — crítico para intervalos curtos entre aulas.

ordem_amplitude <- ttm_analise %>%
  group_by(from_id) %>%
  summarise(amplitude_mediana = median(amplitude_ip, na.rm = TRUE)) %>%
  arrange(desc(amplitude_mediana)) %>%
  pull(from_id)

ttm_analise <- ttm_analise %>%
  mutate(
    from_id = factor(from_id, levels = ordem_amplitude),
    to_id   = factor(to_id,   levels = ordem_amplitude)  
  )

plot_heatmap_amplitude <- ttm_analise %>%
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

ggsave("data/figs/heatmap_amplitude_quin0850.png", plot_heatmap_amplitude,
       width = 14, height = 12, dpi = 150)

ordem_mediana <- ttm_analise %>%
  group_by(from_id) %>%
  summarise(mediana_global = median(travel_time_p50, na.rm = TRUE)) %>%
  arrange(mediana_global) %>%
  pull(from_id)

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

ggsave("data/figs/heatmap_p50_quin0850.png", plot_heatmap_p50,
       width = 14, height = 12, dpi = 150)

# ANÁLISE — ÍNDICE DE ACESSIBILIDADE ACUMULATIVA POR UNIDADE
# Objetivo: para cada unidade, contar quantas outras unidades são alcançáveis
# dentro de cortes de tempo (15, 30, 45 min). Gera um índice por unidade.

cortes <- c(15, 30, 45)

acessibilidade_acumulativa <- lapply(cortes, function(corte) {
  ttm_analise %>%
    filter(travel_time_p50 <= corte, from_id != to_id) %>%
    count(from_id, name = "unidades_acessiveis") %>%
    mutate(corte_min = corte)
}) %>%
  bind_rows()

todas_unidades <- data.frame(from_id = unique(pod$id))

acessibilidade_acumulativa_completo <- expand.grid(
  from_id   = unique(pod$id),
  corte_min = cortes,
  stringsAsFactors = FALSE
) %>%
  left_join(acessibilidade_acumulativa, by = c("from_id", "corte_min")) %>%
  mutate(unidades_acessiveis = replace_na(unidades_acessiveis, 0))

n_unidades_total <- nrow(pod) - 1  # excluindo a própria unidade

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
    subtitle = "% de outras unidades alcançáveis dentro de cada corte de tempo (p50)",
    x        = "Unidade",
    y        = "% de unidades alcançáveis"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 10) 
  )

ggsave("data/figs/bar_acumulativo_quin0850.png", plot_acumulativo,
       width = 12, height = 10, dpi = 150)

# ANÁLISE — PENALIDADE DE ESPERA (waiting_time / total_time)
# Objetivo: para rotas que usam trânsito, identificar onde o tempo de espera
# representa proporção alta do total — ineficiência do sistema de ônibus.

ettm_transit <- ettm %>%
  filter(routes != "[WALK]") %>%
  filter(!is.na(total_time), total_time > 0) %>%
  mutate(
    proporcao_espera = wait_time / total_time,
    proporcao_caminhada = access_time / total_time
  )

penalidade_alta <- ettm_transit %>%
  filter(proporcao_espera >= 0.4) %>%
  arrange(desc(proporcao_espera)) %>%
  select(from_id, to_id, routes, total_time, wait_time, proporcao_espera, access_time)

plot_espera <- ettm_transit %>%
  ggplot(aes(x = proporcao_espera)) +
  geom_histogram(binwidth = 0.05, fill = "#2980B9", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0.4, color = "#C0392B", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.42, y = Inf, label = "Limiar 40%",
           vjust = 2, hjust = 0, color = "#C0392B", size = 3.5) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title    = "Distribuição da Proporção de Tempo de Espera em Rotas com BuzUFBA",
    x        = "Tempo de espera / Tempo total de viagem",
    y        = "Frequência"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("data/figs/hist_proporcao_espera_quin0850.png", plot_espera,
       width = 10, height = 6, dpi = 150)

# ANÁLISE — MODAL INEFICIENTE: TRANSIT MAIS LENTO QUE CAMINHADA
# Objetivo: identificar pares onde usar o ônibus não economiza tempo
# versus ir a pé — evidência de ineficiência da oferta de transporte interno.

ettm_walk_summary <- ettm %>%
  filter(routes == "[WALK]") %>%
  group_by(from_id, to_id) %>%
  summarise(tempo_walk = median(total_time, na.rm = TRUE), .groups = "drop")

ettm_transit_summary <- ettm_transit %>%
  group_by(from_id, to_id) %>%
  summarise(tempo_transit = median(total_time, na.rm = TRUE), .groups = "drop")

comparacao_modal <- ettm_walk_summary %>%
  inner_join(ettm_transit_summary, by = c("from_id", "to_id")) %>%
  mutate(
    diferenca         = tempo_transit - tempo_walk,   
    transit_mais_lento = diferenca > 0
  )

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

ggsave("data/figs/scatter_modal_quin0850.png", plot_modal,
       width = 10, height = 8, dpi = 150)

r5r_gui(r5r_network)

stop_r5(r5r_network)
