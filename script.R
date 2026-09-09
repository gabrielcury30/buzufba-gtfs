##### Setup #####
library(tidytransit)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaftime)
library(geojsonio)
library(htmlwidgets)
library(stinepack)
library(sf)

feed_path    <- "data/gtfs/buzufba_gtfs.zip"
transit_type <- 3

dep_date     <- as.Date("2021-03-02")
min_dep_time <- "06:00:00"
max_arv_time <- "09:00:00"

step_sec  <- 2
trail_sec <- 8

route_cols <- c(B1="#E41A1C", B2="#377EB8", B3="#4DAF4A", B4="#984EA3", B5="#FF7F00")

gtfs <- read_gtfs(feed_path)
route_ids <- gtfs$routes %>% filter(route_type == transit_type) %>% pull(route_id)
trip_ids  <- gtfs$trips  %>% filter(route_id %in% route_ids)  %>% pull(trip_id)

##### 1) Distância acumulada do shape (haversine + cumsum) #####
haversine_m <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180; R <- 6371000
  dlat <- (lat2 - lat1) * rad; dlon <- (lon2 - lon1) * rad
  a <- sin(dlat/2)^2 + cos(lat1*rad)*cos(lat2*rad)*sin(dlon/2)^2
  2 * R * asin(sqrt(a))
}

shapes_dist <- gtfs$shapes %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  mutate(shape_dist_traveled = cumsum(c(0, haversine_m(
    shape_pt_lat[-n()], shape_pt_lon[-n()],
    shape_pt_lat[-1],  shape_pt_lon[-1])))) %>%
  ungroup()

##### 2) Snap monotônico das paradas no shape (rotas circulares) #####
snap_stops <- function(stop_lat, stop_lon, shp) {
  last <- 1L; out <- numeric(length(stop_lat))
  for (i in seq_along(stop_lat)) {
    d <- haversine_m(stop_lat[i], stop_lon[i],
                     shp$shape_pt_lat[last:nrow(shp)],
                     shp$shape_pt_lon[last:nrow(shp)])
    j <- last + which.min(d) - 1L
    out[i] <- shp$shape_dist_traveled[j]; last <- j
  }
  out
}

trips_shapes <- gtfs$trips %>% filter(trip_id %in% trip_ids) %>%
  select(trip_id, route_id, shape_id)

st <- gtfs$stop_times %>% filter(trip_id %in% trip_ids) %>%
  inner_join(gtfs$stops, by = "stop_id") %>%
  inner_join(trips_shapes, by = "trip_id") %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  group_modify(~ mutate(.x, dist = snap_stops(
    .x$stop_lat, .x$stop_lon,
    shapes_dist %>% filter(shape_id == first(.x$shape_id))))) %>%
  ungroup()

##### Adiciona paradas únicas #####
unique_stops <- st %>%
  distinct(stop_id, stop_name, stop_lat, stop_lon)

##### Prepara geometrias das rotas (shapes) #####
# Converte para sf, agrupa por rota e simplifica
route_shapes_sf <- gtfs$shapes %>%
  filter(shape_id %in% trips_shapes$shape_id) %>%
  inner_join(trips_shapes %>% distinct(shape_id, route_id), by = "shape_id") %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(route_id, shape_id) %>%
  summarise(do_union = FALSE, .groups = "drop") %>%
  st_cast("LINESTRING") %>%
  st_simplify(dTolerance = 0.0005) %>% # Aumentada tolerância para mais simplificação
  mutate(color = unname(route_cols[route_id]))

##### 3) Posições interpoladas em passos regulares (com rastro) #####
make_points <- function(tr) {
  # ... (mantendo o código original para interpolação)
  # Apenas ajustando o retorno para incluir route_id
  
  shp <- shapes_dist %>% filter(shape_id == tr$shape_id[1]) %>%
    select(dist = shape_dist_traveled,
           lat = shape_pt_lat, lon = shape_pt_lon) %>%
    mutate(time = as.POSIXct(NA_real_, origin = "1970-01-01"))
  
  stops <- st %>% filter(trip_id == tr$trip_id[1]) %>%
    mutate(time = as.POSIXct(paste(dep_date, as.character(arrival_time)),
                             tz = "America/Bahia"),
           lat = stop_lat, lon = stop_lon) %>%
    select(dist, time, lat, lon)
  
  comb <- bind_rows(stops, shp) %>%
    arrange(dist) %>% group_by(dist) %>% filter(row_number() == 1) %>% ungroup()
  
  comb$time_i <- as.numeric(stinepack::na.stinterp(
    as.numeric(comb$time), along = comb$dist, na.rm = FALSE))
  
  cc <- comb %>% filter(!is.na(time_i), !is.na(lat)) %>%
    distinct(time_i, .keep_all = TRUE)
  if (nrow(cc) < 2) return(NULL)
  
  xs <- seq(min(cc$time_i), max(cc$time_i), by = step_sec)
  la <- approx(cc$time_i, cc$lat, xout = xs)$y
  lo <- approx(cc$time_i, cc$lon, xout = xs)$y
  
  data.frame(
    lat = la, lon = lo,
    start = format(as.POSIXct(xs,             origin = "1970-01-01", tz = "America/Bahia"),
                   "%Y-%m-%dT%H:%M:%S"),
    end   = format(as.POSIXct(xs + trail_sec, origin = "1970-01-01", tz = "America/Bahia"),
                   "%Y-%m-%dT%H:%M:%S"),
    color = unname(route_cols[tr$route_id[1]]),
    route_id = tr$route_id[1] # Adicionado
  )
}

t0 <- format(as.POSIXct(paste(dep_date, min_dep_time), tz = "America/Bahia"), "%Y-%m-%dT%H:%M:%S")
t1 <- format(as.POSIXct(paste(dep_date, max_arv_time), tz = "America/Bahia"), "%Y-%m-%dT%H:%M:%S")

pts <- bind_rows(lapply(split(trips_shapes, trips_shapes$trip_id), make_points)) %>%
  filter(start >= t0, start <= t1)

##### 4) Mapa com timeline #####
map <- leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = -38.510, lat = -12.999, zoom = 13)

# Adiciona geometrias das rotas por grupo (para layersControl)
for (r_id in names(route_cols)) {
  map <- map %>% addPolylines(
    data = route_shapes_sf %>% filter(route_id == r_id),
    color = unname(route_cols[r_id]),
    weight = 3, opacity = 0.3, group = r_id
  )
}

# Adiciona UM player para todos os pontos animados
map <- map %>% addTimeline(
  data = geojsonio::geojson_json(pts, lat = "lat", lon = "lon"),
  timelineOpts = timelineOptions(
    pointToLayer = htmlwidgets::JS("
function(data, latlng) {
  var m = L.circleMarker(latlng, {
    radius: 6, weight: 1,
    color: data.properties.color,
    fillColor: data.properties.color,
    fillOpacity: 0.7
  });
  var rid = data.properties.route_id;
  window.__routeMarkers = window.__routeMarkers || {};
  (window.__routeMarkers[rid] = window.__routeMarkers[rid] || []).push(m);
  return m;
}")
  ),
  sliderOpts = sliderOptions(
    position   = "bottomleft",
    duration   = 90000,
    step       = 60000,
    showTicks  = FALSE,
    formatOutput = htmlwidgets::JS("
      function(date) {
        var d = new Date(date);
        return ('0' + d.getHours()).slice(-2) + ':' + ('0' + d.getMinutes()).slice(-2);
      }")
  )
)

map <- map %>%
  addCircleMarkers(
    data = unique_stops, lng = ~stop_lon, lat = ~stop_lat,
    radius = 5, color = "#2C3E50", fillColor = "#FFFFFF",
    fillOpacity = 0.9, weight = 1.5, label = ~stop_name
  ) %>%
  addLayersControl(
    overlayGroups = names(route_cols),
    options = layersControlOptions(collapsed = FALSE)
  )

map

map <- map %>% htmlwidgets::onRender(htmlwidgets::JS("
function(el, x) {
  var setVisible = function(rid, vis) {
    var reg = window.__routeMarkers || {};
    var ms  = reg[rid] || [];
    for (var i = 0; i < ms.length; i++) {
      if (vis) {
        ms[i].setStyle({ stroke: true,  fill: true,  opacity: 1, fillOpacity: 0.7 });
      } else {
        ms[i].setStyle({ stroke: false, fill: false, opacity: 0, fillOpacity: 0 });
      }
    }
  };
  el.addEventListener('change', function(ev) {
    var t = ev.target;
    if (!t || t.tagName !== 'INPUT' || t.type !== 'checkbox') return;
    var lbl = t.parentElement;
    while (lbl && lbl.tagName !== 'LABEL') lbl = lbl.parentElement;
    if (!lbl) return;
    var name = (lbl.textContent || '').trim();
    setVisible(name, t.checked);
  });
}
"))

htmlwidgets::saveWidget(map, "buzufba_timeline.html", selfcontained = TRUE)
