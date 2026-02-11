# BUZUFBA GTFS

Dados GTFS do sistema de transporte interno da Universidade Federal da Bahia (UFBA), gerados programaticamente em R.

## Sobre o BUZUFBA

O BUZUFBA (Bus UFBA) é o sistema de ônibus gratuito que conecta os diversos campi e unidades da UFBA em Salvador-BA. Ele opera em dias úteis e sábados, atendendo estudantes, servidores e visitantes ao longo de 5 rotas que passam por faculdades, institutos e residências universitárias.

## Rotas

| Rota | Itinerário |
|------|-----------|
| **B1** | Ondina - Canela - São Lázaro |
| **B2** | Ondina - Canela - Vitória - Graça (Rota Longa) |
| **B3** | Ondina - Garibaldi - Canela - Av. 7 de Setembro |
| **B4** | Ondina - Piedade - Vitória - Graça |
| **B5** | Federação - Ondina - Canela - Vitória |

## Paradas

O sistema atende 21 paradas:

| Parada | Nome |
|--------|------|
| SAO_LAZARO | Pt. Estacionamento São Lázaro |
| POLITECNICA | Pt. Politécnica |
| ARQUITETURA | Pt. Arquitetura |
| RESIDENCIA5 | Pt. Residência 5 |
| CANELA_ICS | Campus Vale do Canela (Entrada ICS) |
| ISC_CANELA | ISC Canela |
| ODONTO | P. Odontologia |
| REITORIA | P. Reitoria |
| CRECHE | P. Creche Canela |
| GRACA_R2 | P. Graça R2 (Delicia) |
| DIREITO | Faculdade de Direito |
| PAF1_MAT | Pt. Estacionamento (PAF.1 Matemática) |
| AV_7 | Avenida 7 de Setembro |
| BELAS_ARTES | Belas Artes |
| RESIDENCIA1 | Residência I - Vitória |
| GEOCIENCIAS | Pt. Instituto de Geociências |
| FACOM | Pt. Facom |
| PORTARIA | Pt. Portaria Principal |
| FACED | Faculdade de Educação |
| PROAE | Pró-Reitoria (PROAE) |
| CENTRO_ESPORTES | Centro Esportes da UFBA |

## Estrutura do projeto

```
buzufba-gtfs/
├── R/
│   ├── script-gtfs.R          # Script principal: gera o feed GTFS completo
│   └── gtfs-animation.R       # Script de visualização: gera GIF animado das rotas
├── data/
│   ├── buzufba_gtfs.zip       # Feed GTFS gerado
│   └── gtfs-anim.gif          # Animação das rotas
├── README.Rmd             # Fonte do README (R Markdown)
├── README.md              # Gerado a partir do README.Rmd
├── buzufba-gtfs.Rproj     # Projeto RStudio
└── CLAUDE.md
```

## Pré-requisitos

### Geração do GTFS (`R/script-gtfs.R`)

```r
install.packages(c("gtfstools", "tidyverse", "lubridate", "data.table", "osrm", "sf", "mapview", "dplyr"))
```

### Animação (`R/gtfs-animation.R`)

```r
install.packages(c("gtfs2gps", "geobr", "ggplot2", "gganimate", "data.table", "ggthemes", "sf", "viridis", "sfheaders", "units", "gifski"))
```

## Como usar

### Gerar o feed GTFS

O script `R/script-gtfs.R` monta todas as tabelas do GTFS (agency, routes, stops, trips, stop_times, calendar, shapes, feed_info) e exporta como `.zip`:

```r
source("R/script-gtfs.R")
```

> O script utiliza a API do OSRM para traçar os shapes das rotas, então requer conexão com a internet.

### Gerar a animação

Após gerar o GTFS, o script `R/gtfs-animation.R` cria um GIF animado mostrando os ônibus percorrendo as rotas ao longo do dia:

```r
source("R/gtfs-animation.R")
```

O GIF será salvo em `data/gtfs-anim.gif`.

## Sobre o GTFS

O [GTFS (General Transit Feed Specification)](https://gtfs.org/) é um formato padrão para dados de transporte público. Ele permite que sistemas como Google Maps, OpenTripPlanner e outros aplicativos de mobilidade consumam informações de rotas, horários e paradas de forma padronizada.
