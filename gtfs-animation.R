# load libraries
library(gtfs2gps)
library(geobr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
library(viridis)
library(sfheaders)
library(units)

###### 1. process public transport data  ------------------

# read gtfs
gtfs_dt <- gtfs2gps::read_gtfs("data/gtfs/buzufba_gtfs.zip")

# get transport network as sf object
shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(gtfs_dt)

# Convert GTFS data into a data.table with GPS-like records
gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 100, parallel = T)
head(gps_dt)

# Convert "GPS" points into sf
gps_sf <- sfheaders::sf_point(gps_dt, x = "shape_pt_lon" , y = "shape_pt_lat", keep = T)
sf::st_crs(gps_sf) <- 4326
head(gps_sf)

###### 2. gif with blank background [fully reproducible] ------------------

# download Salvador border for spatial context
salvador <- geobr::read_municipality(code_muni = 2927408)

anim <- ggplot() +
  geom_sf(data = salvador, color='gray50', fill=NA) +
  geom_sf(data = shapes_sf, color='gray90', size=0.01) +
  geom_point(data = gps_dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), size=1.5, alpha = 0.5, show.legend = FALSE) +
  scale_colour_viridis( discrete = T ) + coord_sf(xlim = c(min(gps_dt$shape_pt_lon), max(gps_dt$shape_pt_lon)), ylim = c(min(gps_dt$shape_pt_lat), max(gps_dt$shape_pt_lat)) ) +
  theme_map() +
  
  # gganimate specificatons
  labs(title = 'Time: {frame_time}') +
  transition_time(as.POSIXct(timestamp) + 10800) +  # need to fix issue with time zone
  shadow_wake(wake_length = 0.015, alpha = FALSE) +
  ease_aes('linear')


# save gif
anim_save(animation = anim, "data/gtfs/gtfs-anim.gif", fps = 20)

animate(anim, fps = 20)
