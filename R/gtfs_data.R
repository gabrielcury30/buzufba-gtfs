# Dados estáticos e configurações de operação do GTFS BUZUFBA.

unir_circular <- function(ida, volta) {
  c(ida, if (utils::tail(ida, 1) == volta[1]) volta[-1] else volta)
}

remover_sao_lazaro <- function(sequencia) {
  rle(sequencia[sequencia != "SAO_LAZARO"])$values
}

criar_dados_estaticos_gtfs <- function() {
  agency <- tibble::tibble(
    agency_id = "UFBA", agency_name = "BUZUFBA - Universidade Federal da Bahia",
    agency_url = "https://ufba.br/", agency_timezone = "America/Bahia", agency_lang = "pt"
  )

  routes <- tibble::tibble(
    route_id = c("B1", "B2", "B3", "B4", "B5"), agency_id = "UFBA",
    route_short_name = route_id, route_long_name = c(
      "Ondina - Canela - São Lázaro (Circular)",
      "Ondina - Canela - Vitória - Graça Longa (Circular)", "Ondina - Garibaldi - Canela - Av. 7 (Circular)",
      "Ondina - Piedade - Vitória - Graça (Circular)", "Federação - Ondina - Canela - Vitória (Circular)"
    ),
    route_type = 3, route_color = "00539F", route_text_color = "FFFFFF"
  )

  stops <- tibble::tribble(
    ~stop_id, ~stop_name, ~stop_lat, ~stop_lon, "SAO_LAZARO",
    "Pt. Estacionamento São Lázaro", -13.004774, -38.512385, "POLITECNICA",
    "Pt. Politécnica", -12.998718, -38.51179, "ARQUITETURA", "Pt. Arquitetura",
    -12.99714, -38.508629, "RESIDENCIA5", "Pt. Residência 5", -12.998336, -38.505939,
    "CANELA_ICS", "Campus Vale do Canela (Entrada ICS)", -12.994839, -38.52059,
    "ISC_CANELA", "ISC Canela", -12.994563, -38.521921, "ODONTO", "P. Odontologia",
    -12.994676, -38.52289, "REITORIA", "P. Reitoria", -12.992404, -38.520654,
    "CRECHE", "P. Creche Canela", -12.994758, -38.517496, "GRACA_R2", "P. Graça R2 (Delicia)",
    -12.997615, -38.519141, "DIREITO", "Faculdade de Direito", -12.996353, -38.521559,
    "DIREITO_B5", "Faculdade de Direito B5", -12.99619, -38.521341, "PAF1_MAT",
    "Pt. Estacionamento (PAF.1 Matemática)", -13.00176, -38.506922, "PAF1_B1",
    "PAF.1 Matemática B1", -13.002414, -38.506589, "AV_7", "Avenida 7 de Setembro / Faculdade de Economia",
    -12.983266, -38.515419, "ECONOMIA", "Faculdade de Economia Pt. 2", -12.983818,
    -38.515236, "BELAS_ARTES", "Belas Artes", -12.991215, -38.521153, "RESIDENCIA1",
    "Residência I - Vitória", -12.994041, -38.526425, "GEOCIENCIAS", "Pt. Instituto de Geociências",
    -12.998508, -38.506513, "FACOM", "Pt. Facom", -13.00151, -38.509824, "PORTARIA",
    "Pt. Portaria Principal", -13.006104, -38.510314, "FACED", "Faculdade de Educação",
    -12.995148, -38.5193, "PROAE", "Pró-Reitoria (PROAE)", -12.997538, -38.509394,
    "CENTRO_ESPORTES", "Centro Esportes da UFBA", -13.009416, -38.513782, "POLITECNICA_VOLTA",
    "Pt. Politécnica Volta", -12.999561, -38.511507, "GARIBALDI", "Pt. Av. Garibaildi",
    -12.999217, -38.505917, "REITORIA_2", "Pt. Reitoria Ida Economia", -12.992131,
    -38.520303, "GEOCIENCIAS_INTERNO", "Pt. Geociência Interno", -12.998233,
    -38.507374, "RESIDENCIA1_IDA", "Pt. Residência I - Vitória Ida", -12.994062,
    -38.526339
  )

  calendar <- tibble::tibble(service_id = c("DIAS_UTEIS", "SABADO"), monday = c(
    1,
    0
  ), tuesday = c(1, 0), wednesday = c(1, 0), thursday = c(1, 0), friday = c(
    1,
    0
  ), saturday = c(0, 1), sunday = c(0, 0), start_date = "20260101", end_date = "20261231")

  list(agency = agency, routes = routes, stops = stops, calendar = calendar)
}

criar_configuracoes_rotas <- function(noturno_sem_sao_lazaro = FALSE) {
  sequencias <- list(B1 = unir_circular(c(
    "SAO_LAZARO", "POLITECNICA_VOLTA", "ARQUITETURA",
    "RESIDENCIA5", "CANELA_ICS", "ISC_CANELA", "ODONTO"
  ), c(
    "REITORIA", "CRECHE",
    "GRACA_R2", "DIREITO", "FACED", "PAF1_B1", "PROAE", "POLITECNICA_VOLTA",
    "SAO_LAZARO"
  )), B2 = unir_circular(c(
    "PAF1_MAT", "GARIBALDI", "PROAE", "POLITECNICA_VOLTA",
    "SAO_LAZARO", "POLITECNICA", "CRECHE", "REITORIA_2", "BELAS_ARTES", "REITORIA",
    "CRECHE", "GRACA_R2", "RESIDENCIA1"
  ), c(
    "RESIDENCIA1_IDA", "DIREITO", "ISC_CANELA",
    "ODONTO", "REITORIA", "CRECHE", "POLITECNICA", "SAO_LAZARO", "POLITECNICA_VOLTA",
    "ARQUITETURA", "RESIDENCIA5", "GEOCIENCIAS", "PAF1_MAT"
  )), B3 = unir_circular(c(
    "PAF1_MAT",
    "GARIBALDI", "CANELA_ICS", "AV_7", "BELAS_ARTES"
  ), c(
    "REITORIA", "CRECHE",
    "POLITECNICA", "ARQUITETURA", "GEOCIENCIAS", "PAF1_MAT"
  )), B4 = unir_circular(
    c(
      "PAF1_MAT",
      "GARIBALDI", "PROAE", "POLITECNICA", "CRECHE", "REITORIA_2", "ECONOMIA"
    ),
    c(
      "RESIDENCIA1", "GRACA_R2", "POLITECNICA_VOLTA", "SAO_LAZARO", "ARQUITETURA",
      "GEOCIENCIAS", "PAF1_MAT"
    )
  ), B5 = unir_circular(c(
    "GEOCIENCIAS_INTERNO",
    "FACOM", "PORTARIA", "CENTRO_ESPORTES", "PAF1_B1", "GARIBALDI", "PROAE",
    "POLITECNICA_VOLTA", "SAO_LAZARO", "POLITECNICA_VOLTA", "POLITECNICA", "CRECHE",
    "REITORIA_2"
  ), c(
    "RESIDENCIA1", "DIREITO_B5", "ISC_CANELA", "ODONTO", "REITORIA",
    "CRECHE", "POLITECNICA", "POLITECNICA_VOLTA", "SAO_LAZARO", "POLITECNICA_VOLTA",
    "ARQUITETURA", "GEOCIENCIAS", "PAF1_B1", "PORTARIA", "FACOM", "GEOCIENCIAS_INTERNO"
  )))

  horarios_base <- list(
    B1 = list(
      full = c(
        "06:10", "07:40", "09:10", "10:40",
        "12:10", "13:40", "15:10", "16:40", "18:10", "19:40", "21:10", "22:40"
      ),
      limite_noite = "19:40", limite_sab = "13:40"
    ), B2 = list(full = c(
      "06:00",
      "07:40", "09:20", "11:00", "12:40", "14:20", "16:00", "17:40", "19:20", "21:00",
      "22:40"
    ), limite_noite = "19:20", limite_sab = "14:20"), B3 = list(full = c(
      "06:30",
      "07:40", "08:50", "10:00", "11:10", "12:20", "13:30", "14:40", "15:50", "17:00",
      "18:10", "19:20", "20:30", "21:40", "22:50"
    ), limite_noite = "18:10", limite_sab = "14:40"),
    B4 = list(
      full = c(
        "06:00", "07:35", "09:10", "10:45", "12:20", "13:55",
        "15:30", "17:05", "18:40", "20:15", "21:50"
      ), limite_noite = "18:40",
      limite_sab = "13:55"
    ), B5 = list(
      full = c(
        "06:40", "08:15", "09:50",
        "11:25", "13:00", "14:35", "16:10", "17:45", "19:20", "20:55", "22:30"
      ),
      limite_noite = "17:45", limite_sab = "14:35"
    )
  )

  purrr::map_dfr(names(horarios_base), function(rota) {
    horario <- horarios_base[[rota]]
    sequencia_diurna <- sequencias[[rota]]
    sequencia_noturna <- if (noturno_sem_sao_lazaro) {
      remover_sao_lazaro(sequencia_diurna)
    } else {
      sequencia_diurna
    }
    shape_diurno <- paste0("SHP_", rota, "_CIRCULAR")
    shape_noturno <- if (noturno_sem_sao_lazaro) {
      paste0(shape_diurno, "_N")
    } else {
      shape_diurno
    }

    tibble::tribble(
      ~route_id, ~service_id, ~direction_id, ~horarios, ~sequencia_stops,
      ~shape_id, rota, "DIAS_UTEIS", 0, horario$full[horario$full < horario$limite_noite],
      sequencia_diurna, shape_diurno, rota, "DIAS_UTEIS", 0, horario$full[horario$full >=
        horario$limite_noite], sequencia_noturna, shape_noturno, rota, "SABADO",
      0, horario$full[seq_len(match(horario$limite_sab, horario$full))], sequencia_diurna,
      shape_diurno
    )
  })
}
