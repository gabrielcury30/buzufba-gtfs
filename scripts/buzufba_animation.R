##### Setup e Carregamento de Dados #####
localizar_diretorio_projeto <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)
  if (length(argumento_arquivo) > 0) {
    caminho_script <- normalizePath(sub("^--file=", "", argumento_arquivo[1]), mustWork = TRUE)
    return(normalizePath(file.path(dirname(caminho_script), ".."), mustWork = TRUE))
  }

  diretorio <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(diretorio, "buzufba-gtfs.Rproj"))) {
    diretorio_pai <- dirname(diretorio)
    if (diretorio_pai == diretorio) stop("Não foi possível localizar a raiz do projeto.", call. = FALSE)
    diretorio <- diretorio_pai
  }
  diretorio
}

diretorio_projeto <- localizar_diretorio_projeto()
source(file.path(diretorio_projeto, "R", "animation_functions.R"), local = FALSE)

pacotes <- c("dplyr", "geojsonio", "htmlwidgets", "leaflet", "leaftime", "sf", "stinepack", "tidytransit")
ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(ausentes) > 0) stop("Instale os pacotes R necessários: ", paste(ausentes, collapse = ", "), call. = FALSE)

library(dplyr)
library(leaflet)
library(sf)

feed_path <- file.path(diretorio_projeto, "data", "processed", "buzufba_gtfs.zip")
buildings_path <- file.path(diretorio_projeto, "data", "raw", "edificacoes_ufba.gpkg")
output_path <- file.path(diretorio_projeto, "data", "processed", "animacao_buzufba.html")
transit_type <- 3

if (!file.exists(buildings_path)) {
  stop(sprintf(
    "Arquivo de edificações não encontrado: '%s'. Ajuste buildings_path.",
    buildings_path
  ))
}

edificacoes_sf <- sf::st_read(buildings_path, quiet = TRUE)
if (!"name" %in% names(edificacoes_sf)) {
  stop("O arquivo de edificações precisa conter uma coluna chamada 'name'.")
}

edificacoes_sf <- edificacoes_sf %>%
  mutate(name = ifelse(
    is.na(name) | trimws(as.character(name)) == "",
    "Edificação sem nome",
    as.character(name)
  )) %>%
  st_transform(crs = 4326)

building_fill <- "#E9C46A"
building_border <- "#725C2D"
building_group <- "Edificações UFBA"

dep_date <- as.Date("2026-09-21")
min_dep_time <- "06:00:00"
max_arv_time <- "23:30:00"

# Parâmetros de interpolação: passos de tempo e tamanho do rastro (tail)
step_sec <- 5
trail_sec <- 20

route_cols <- c(
  B1 = "#E41A1C", B2 = "#377EB8", B3 = "#4DAF4A", B4 = "#984EA3",
  B5 = "#FF7F00"
)

route_colors_js <- paste0(
  "{", paste0('"', names(route_cols), '":"', unname(route_cols), '"', collapse = ","), "}"
)

layer_cols <- c(route_cols, setNames(building_fill, building_group))
layer_colors_js <- paste0(
  "{", paste0('"', names(layer_cols), '":"', unname(layer_cols), '"', collapse = ","), "}"
)

gtfs <- tidytransit::read_gtfs(feed_path)
route_ids <- gtfs$routes %>%
  filter(route_type == transit_type) %>%
  pull(route_id)
trip_ids <- gtfs$trips %>%
  filter(route_id %in% route_ids) %>%
  pull(trip_id)

##### 1) Distância acumulada do shape (haversine + cumsum) #####
shapes_dist <- gtfs$shapes %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  group_modify(~ adensar_shape(.x, passo_m = 10)) %>%
  mutate(
    shape_pt_sequence = row_number(),
    shape_dist_traveled = cumsum(c(0, calcular_distancia_haversine(
      shape_pt_lat[-n()], shape_pt_lon[-n()],
      shape_pt_lat[-1], shape_pt_lon[-1]
    )))
  ) %>%
  ungroup()

##### 2) Snap monotônico das paradas no shape #####
trips_shapes <- gtfs$trips %>%
  filter(trip_id %in% trip_ids) %>%
  select(trip_id, route_id, shape_id)

st <- gtfs$stop_times %>%
  filter(trip_id %in% trip_ids) %>%
  inner_join(gtfs$stops, by = "stop_id") %>%
  inner_join(trips_shapes, by = "trip_id") %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  group_modify(~ mutate(.x, dist = encaixar_paradas_no_shape(
    .x$stop_lat, .x$stop_lon,
    shapes_dist %>% filter(shape_id == first(.x$shape_id))
  ))) %>%
  ungroup()

unique_stops <- st %>%
  distinct(stop_id, stop_name, stop_lat, stop_lon)

##### Prepara geometrias das rotas (shapes) #####
route_shapes_sf <- gtfs$shapes %>%
  filter(shape_id %in% trips_shapes$shape_id) %>%
  inner_join(trips_shapes %>% distinct(shape_id, route_id), by = "shape_id") %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(route_id, shape_id) %>%
  summarise(do_union = FALSE, .groups = "drop") %>%
  st_cast("LINESTRING") %>%
  st_simplify(dTolerance = 0.0005) %>% # Simplifica geometria para performance
  mutate(color = unname(route_cols[route_id]))

##### 3) Posições interpoladas para animação (com interpolação Stine) #####
t0 <- format(
  as.POSIXct(paste(dep_date, min_dep_time), tz = "America/Bahia"),
  "%Y-%m-%dT%H:%M:%S"
)
t1 <- format(
  as.POSIXct(paste(dep_date, max_arv_time), tz = "America/Bahia"),
  "%Y-%m-%dT%H:%M:%S"
)

pts <- bind_rows(lapply(split(trips_shapes, trips_shapes$trip_id), function(viagem) {
  criar_pontos_animacao(viagem, shapes_dist, st, dep_date, step_sec, trail_sec, route_cols)
})) %>%
  filter(start >= t0, start <= t1)

### 4) Criação do mapa interativo com timeline #####
map <- leaflet() %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/",
      "light_all/{z}/{x}/{y}.png?key=",
      "INSIRA_SUA_CHAVE_AQUI"
    ),
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">',
      "OpenStreetMap</a>, ",
      '&copy; <a href="https://carto.com/attributions">',
      "CARTO</a>"
    ),
    options = tileOptions(
      subdomains = "abcd",
      maxZoom = 20
    )
  ) %>%
  setView(lng = -38.510, lat = -12.999, zoom = 13)

# Camada opcional das edificações. É adicionada antes das rotas para que
# os traçados dos ônibus permaneçam visíveis por cima dos polígonos.
map <- map %>% addPolygons(
  data = edificacoes_sf,
  fillColor = building_fill,
  color = building_border,
  weight = 1,
  opacity = 0.95,
  fillOpacity = 0.5,
  label = ~name,
  labelOptions = labelOptions(
    direction = "auto",
    textsize = "12px",
    style = list("font-weight" = "normal", padding = "3px 8px")
  ),
  group = building_group
)

for (r_id in names(route_cols)) {
  map <- map %>% addPolylines(
    data = route_shapes_sf %>% filter(route_id == r_id),
    color = unname(route_cols[r_id]),
    weight = 3, opacity = 0.3, group = r_id
  )
}

map <- map %>% leaftime::addTimeline(
  data = geojsonio::geojson_json(pts, lat = "lat", lon = "lon"),
  timelineOpts = leaftime::timelineOptions(
    pointToLayer = htmlwidgets::JS("
function(data, latlng) {
  // Guarda o instante de cada posição para calcular dinamicamente a idade
  // do ponto e reduzir seu tamanho conforme ele fica para trás no rastro.
  var marker = L.circleMarker(latlng, {
    radius: 7, weight: 1,
    color: data.properties.color,
    fillColor: data.properties.color,
    fillOpacity: 0.75,
    className: 'rt-' + data.properties.route_id
  });
  marker._busTrailPoint = true;
  marker._busTrailStart = new Date(data.properties.start).getTime();
  return marker;
}")
  ),
  sliderOpts = leaftime::sliderOptions(
    position = "bottomleft",
    duration = 525000,
    steps = 200000,
    showTicks = FALSE,
    formatOutput = htmlwidgets::JS("
      function(date) {
        var d = new Date(date);
        return ('0' + d.getHours()).slice(-2) + ':' + ('0' +
        d.getMinutes()).slice(-2);
      }")
  )
)

button_jump_sec <- 50

map <- map %>%
  addCircleMarkers(
    data = unique_stops, lng = ~stop_lon, lat = ~stop_lat,
    radius = 5, color = "#2C3E50", fillColor = "#FFFFFF",
    fillOpacity = 0.9, weight = 1.5, label = ~stop_name
  ) %>%
  addLayersControl(
    overlayGroups = c(names(route_cols), building_group),
    options = layersControlOptions(collapsed = FALSE)
  )

map <- map %>%
  htmlwidgets::onRender(htmlwidgets::JS(paste0("
function(el, x) {
  var routeColors = ", route_colors_js, ";
  var layerColors = ", layer_colors_js, ";
  var styleEl = document.createElement('style');
  styleEl.setAttribute('id', 'routeHideStyle');
  document.head.appendChild(styleEl);

  var refresh = function() {
    var inputs = el.querySelectorAll('.leaflet-control-layers input[type=checkbox]');
    var css = '';
    for (var i = 0; i < inputs.length; i++) {
      if (inputs[i].checked) continue;
      var lbl = inputs[i].parentElement;
      while (lbl && lbl.tagName !== 'LABEL') lbl = lbl.parentElement;
      if (!lbl) continue;
      var name = (lbl.textContent || '').trim();
      // Só as rotas usam classes rt-<ID>; a camada de edificações é
      // controlada diretamente pelo LayersControl do Leaflet.
      if (routeColors[name]) {
        css += '.rt-' + name + '{display:none!important;}';
      }
    }
    styleEl.textContent = css;
  };

  var addSwatches = function() {
    var inputs = el.querySelectorAll('.leaflet-control-layers-overlays input[type=checkbox]');
    for (var i = 0; i < inputs.length; i++) {
      var lbl = inputs[i].parentElement;
      while (lbl && lbl.tagName !== 'LABEL') lbl = lbl.parentElement;
      if (!lbl) continue;
      var name = (lbl.textContent || '').trim();
      var color = layerColors[name];
      if (!color) continue;
      var swatch = document.createElement('span');
      swatch.style.display = 'inline-block';
      swatch.style.width = '10px';
      swatch.style.height = '10px';
      swatch.style.marginLeft = '4px';
      swatch.style.marginRight = '4px';
      swatch.style.borderRadius = '2px';
      swatch.style.backgroundColor = color;
      swatch.style.verticalAlign = 'middle';
      inputs[i].insertAdjacentElement('afterend', swatch);
    }
  };
  addSwatches();

  el.addEventListener('change', function(ev) {
    var t = ev.target;
    if (!t || t.tagName !== 'INPUT' || t.type !== 'checkbox') return;
    refresh();
  });

  refresh();
}
"))) %>%
  htmlwidgets::onRender(htmlwidgets::JS(paste0("
function(el, x) {
  var map = this;

  var opacityControl = L.control({position: 'bottomright'});
  opacityControl.onAdd = function() {
    var div = L.DomUtil.create('div', 'opacity-control');
    div.style.background = 'white';
    div.style.padding = '8px 10px';
    div.style.borderRadius = '4px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.3)';
    div.style.fontSize = '12px';
    div.innerHTML =
      '<div style=\"margin-bottom:6px;\">' +
        '<label>Transparência das Rotas: <input id=\"routeOpacity\" type=\"range\" ' +
        'min=\"0\" max=\"1\" step=\"0.05\" value=\"0.3\"></label>' +
      '</div>' +
      '<div>' +
        '<label>Transparência do Mapa: <input id=\"baseOpacity\" type=\"range\" ' +
        'min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>' +
      '</div>';
    // Impede que arrastar o slider também arraste/zoom o mapa
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  opacityControl.addTo(map);

  document.getElementById('routeOpacity').addEventListener('input', function(e) {
    var val = parseFloat(e.target.value);
    map.eachLayer(function(layer) {
      // Em Leaflet, polígonos também herdam de L.Polyline; excluí-los
      // evita que o slider das rotas altere a transparência das edificações.
      if (layer instanceof L.Polyline && !(layer instanceof L.Polygon)) {
        layer.setStyle({ opacity: val });
      }
    });
  });

  document.getElementById('baseOpacity').addEventListener('input', function(e) {
    var val = parseFloat(e.target.value);
    map.eachLayer(function(layer) {
      if (layer instanceof L.TileLayer) {
        layer.setOpacity(val);
      }
    });
  });

  // Rastro de um ônibus: o ponto mais recente fica maior; os anteriores
  // diminuem progressivamente de acordo com sua idade no instante atual.
  var trailDurationMs = ", trail_sec * 1000, ";
  var leaderWindowMs = ", step_sec * 1000, ";
  var timelineSlider = el.querySelector('.leaflet-timeline-control input.time-slider');

  function updateTrailLayer(layer, currentTime) {
    if (!layer) return;

    if (layer._busTrailPoint && typeof layer.setRadius === 'function') {
      var ageMs = currentTime - layer._busTrailStart;
      var tailFraction = 0;
      var radius;

      // Por um intervalo de step_sec, o ponto líder mantém o tamanho máximo.
      // Depois disso, os pontos do rastro encolhem gradualmente até 2 px.
      if (ageMs >= 0 && ageMs < leaderWindowMs) {
        radius = 7;
      } else {
        tailFraction = Math.max(0, Math.min(1,
          (ageMs - leaderWindowMs) / Math.max(1, trailDurationMs - leaderWindowMs)
        ));
        radius = 5.5 - (3.5 * tailFraction);  // de 5,5 px até 2 px
      }

      var fillOpacity = 0.75 - (0.45 * tailFraction);
      var strokeOpacity = 0.90 - (0.40 * tailFraction);

      layer.setRadius(radius);
      layer.setStyle({
        fillOpacity: fillOpacity,
        opacity: strokeOpacity,
        weight: 0.6 + (0.4 * (1 - tailFraction))
      });
    }

    if (typeof layer.eachLayer === 'function') {
      layer.eachLayer(function(child) {
        updateTrailLayer(child, currentTime);
      });
    }
  }

  function updateBusTrail() {
    if (!timelineSlider) return;
    var currentTime = parseFloat(timelineSlider.value);
    if (!isFinite(currentTime)) return;

    map.eachLayer(function(layer) {
      updateTrailLayer(layer, currentTime);
    });
  }

  if (timelineSlider) {
    timelineSlider.addEventListener('input', updateBusTrail);
    timelineSlider.addEventListener('change', updateBusTrail);
  }

  window.setInterval(updateBusTrail, 100);
  updateBusTrail();
}
"))) %>%
  htmlwidgets::onRender(htmlwidgets::JS(paste0("
function(el, x) {
  var jumpMs = ", button_jump_sec * 1000, ";

  var slider  = el.querySelector('.leaflet-timeline-control input.time-slider');
  var prevBtn = el.querySelector('.leaflet-timeline-control button.prev');
  var nextBtn = el.querySelector('.leaflet-timeline-control button.next');
  if (!slider || !prevBtn || !nextBtn) return;

  // Clonar remove os listeners originais da biblioteca (prev()/next() internos)
  var newPrev = prevBtn.cloneNode(true);
  var newNext = nextBtn.cloneNode(true);
  prevBtn.parentNode.replaceChild(newPrev, prevBtn);
  nextBtn.parentNode.replaceChild(newNext, nextBtn);

  // Evita que o clique nos botões arraste/zoome o mapa por baixo
  L.DomEvent.disableClickPropagation(newPrev);
  L.DomEvent.disableClickPropagation(newNext);

  function jump(delta) {
    var min = parseFloat(slider.min), max = parseFloat(slider.max);
    var val = Math.max(min, Math.min(max, parseFloat(slider.value) + delta));
    slider.value = val;
    slider.dispatchEvent(new Event('change', { bubbles: true }));
  }

  newPrev.addEventListener('click', function() { jump(-jumpMs); });
  newNext.addEventListener('click', function() { jump(jumpMs); });
}
")))

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
htmlwidgets::saveWidget(map, output_path, selfcontained = TRUE)
message("Animação salva em '", output_path, "'.")
