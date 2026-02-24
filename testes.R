library(gtfs2gps); library(geobr); library(ggplot2); library(gganimate)
library(ggthemes); library(sf); library(viridis)
library(sfheaders); library(units); library(gifski); library(ggspatial);
library(dplyr); library(data.table)

# download Salvador border for spatial context
salvador <- geobr::read_municipality(code_muni = 2927408)

###### 1. process public transport data  ------------------

# read gtfs
gtfs_dt <- gtfs2gps::read_gtfs("data/buzufba_gtfs.zip")

# get transport network as sf object
shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(gtfs_dt)

# Convert GTFS data into a data.table with GPS-like records
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 10, parallel = FALSE)
nrow(gps_dt)
head(gps_dt)

###### 2. gif with blank background [fully reproducible] ------------------
View(shapes_sf)
View(gps_dt)

# B1 06:10
## Observar abaixo que os horários de início do de ida e do de volta estão iguais
## (horário de fim da IDA deveria ser horário de início da VOLTA, não?)
gps_dt |> filter(trip_id %in% c("B1_DIAS_UTEIS_0_0610","B1_DIAS_UTEIS_1_0610")) |> View("B1_0610")

ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 15) +
  geom_sf(data = shapes_sf |> filter(shape_id %in% c("SHP_B1_IDA","SHP_B1_VOLTA")), 
          aes(linetype=shape_id), linewidth=0.5) +
  # scale_color_manual(values = c('blue','red')) +
  geom_point(data = gps_dt |> filter(trip_id %in% "B1_DIAS_UTEIS_1_0610") |>
               mutate(cumdist = as.numeric(cumdist)),
             aes(x = shape_pt_lon, y=shape_pt_lat, color = cumdist),
             size=1.5, alpha = 0.5, show.legend = FALSE) +
  scale_color_viridis_c() 

# B2 06:00
## Observar abaixo que os horários de início do de ida e do de volta estão iguais
## (horário de fim da IDA deveria ser horário de início da VOLTA, não?)
gps_dt |> filter(trip_id %in% c("B2_DIAS_UTEIS_0_0600","B2_DIAS_UTEIS_1_0600")) |> View("B2_0600")

ggplot() +
  # geom_sf(data = shapes_sf |> filter(shape_id == "SHP_B2_IDA")) +
  geom_sf(data = shapes_sf |> filter(shape_id == "SHP_B2_VOLTA")) +
  geom_sf(data = shapes_sf |> filter(shape_id %in% c("SHP_B2_IDA","SHP_B2_VOLTA")),
          aes(linetype=shape_id), linewidth=0.5) +
  geom_point(data = gps_dt |> filter(trip_id %in% "B2_DIAS_UTEIS_1_0600") |>
               mutate(cumdist = as.numeric(cumdist)),
             aes(x = shape_pt_lon, y=shape_pt_lat, color = cumdist),
             size=1.5, alpha = 0.5, show.legend = FALSE) +
  scale_color_viridis_c() 


### Animação
## B1:
gps_dt_sub <- gps_dt |> 
  filter(trip_id %in% c("B1_DIAS_UTEIS_0_0610","B1_DIAS_UTEIS_1_0610"))
shapes_sf_sub <- shapes_sf |> 
  filter(shape_id %in% c("SHP_B1_IDA","SHP_B1_VOLTA"))

### GIF:
anim <- ggplot() +
  geom_sf(data = shapes_sf_sub, aes(linetype=shape_id),
          linewidth=0.5) +
  geom_point(data = gps_dt_sub, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), 
             size=5, alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = c('blue','red')) + 
  coord_sf(xlim = c(min(gps_dt_sub$shape_pt_lon), max(gps_dt_sub$shape_pt_lon)),
           ylim = c(min(gps_dt_sub$shape_pt_lat), max(gps_dt_sub$shape_pt_lat)) ) +
  theme_map() +
  
  # gganimate specificatons
  labs(title = "Time: {format(frame_time, '%H:%M:%S')}") +
  transition_time(as.POSIXct(timestamp) + 10800) +  # need to fix issue with time zone
  # shadow_wake(wake_length = 0.015, alpha = FALSE) +
  ease_aes('linear')

# gps_dt_sub |> 
#   distinct(timestamp, .keep_all = T) |> View()

anim_B1_0610 <- animate(anim, fps = 12, nframes = 600)

# Salva gif
anim_save(animation = anim_B1_0610, "data_testes/anim_B1_0610.gif", fps = 6, nframes = 600,
          renderer = gifski_renderer())

## Obs.: verificar se não está duplicado o B1 da VOLTA
## (às vezes aparecem 2 pontos, apesar de remover o shadow_wake)

## B2:
gps_dt_sub <- gps_dt |> 
  filter(trip_id %in% c("B2_DIAS_UTEIS_0_0600","B2_DIAS_UTEIS_1_0600"))
shapes_sf_sub <- shapes_sf |> 
  filter(shape_id %in% c("SHP_B2_IDA","SHP_B2_VOLTA"))

### GIF:
anim <- ggplot() +
  geom_sf(data = shapes_sf_sub, aes(linetype=shape_id),
          linewidth=0.5) +
  geom_point(data = gps_dt_sub, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), 
             size=5, alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = c('blue','red')) + 
  coord_sf(xlim = c(min(gps_dt_sub$shape_pt_lon), max(gps_dt_sub$shape_pt_lon)),
           ylim = c(min(gps_dt_sub$shape_pt_lat), max(gps_dt_sub$shape_pt_lat)) ) +
  theme_map() +
  
  # gganimate specificatons
  labs(title = "Time: {format(frame_time, '%H:%M:%S')}") +
  transition_time(as.POSIXct(timestamp) + 10800) +  # need to fix issue with time zone
  # shadow_wake(wake_length = 0.015, alpha = FALSE) +
  ease_aes('linear')

anim_B2_0600 <- animate(anim, fps = 12, nframes = 600)

# Salva gif
anim_save(animation = anim_B2_0600, "data_testes/anim_B2_0600.gif", fps = 6, nframes = 600,
          renderer = gifski_renderer())
