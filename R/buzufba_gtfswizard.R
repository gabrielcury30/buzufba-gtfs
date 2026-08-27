library(GTFSwizard);

gtfs <- GTFSwizard::read_gtfs("data/gtfs/buzufba_gtfs.zip");

summary(gtfs);

GTFSwizard::explore_gtfs(gtfs);

# GTFSwizard::get_servicepattern(gtfs);
# 
# GTFSwizard::get_frequency(gtfs, method = "by_route");
# 
# GTFSwizard::plot_routefrequency(gtfs, route = gtfs$routes$route_id[3]);
# 
# GTFSwizard::plot_calendar(gtfs, facet_by_year = TRUE);
# 
# GTFSwizard::get_headways(gtfs, method = "by_hour");
# 
# GTFSwizard::get_durations(gtfs, method = 'detailed', trips = 'all');
# GTFSwizard::get_distances(gtfs, method = 'by_trip', trips = 'all');
# GTFSwizard::get_distances(gtfs, method = 'by_route', trips = 'all');
# GTFSwizard::get_speeds(gtfs, method = 'by_route', trips = 'all');
# GTFSwizard::get_fleet(gtfs, method = 'peak');
# GTFSwizard::get_1stdeparture(gtfs);
# 
# # GTFSwizard::get_corridor(gtfs, i = .01, min_length = 1500);
# # GTFSwizard::plot_corridor(gtfs);
# 
# GTFSwizard::get_hubs(gtfs);
# GTFSwizard::plot_hubs(gtfs);
# 
# filtered_gtfs <- GTFSwizard::filter_servicepattern(gtfs, "servicepattern-2");
# 
# filtered_gtfs <- GTFSwizard::filter_date(gtfs, "2026-08-25");
# 
# filtered_gtfs <- GTFSwizard::filter_route(gtfs, gtfs$routes$route_id[1:2]);
# 
# filtered_gtfs <- GTFSwizard::filter_trip(gtfs, gtfs$trips$trip_id[1:2], FALSE);
# 
# filtered_gtfs <- GTFSwizard::filter_time(gtfs = gtfs, "06:30:00", "10:00:00");
# 
# # spatial.filter <- GTFSwizard::get_shapes_sf(gtfs$shapes);
# #
# # stops <- sf::st_filter(GTFSwizard::get_stops_sf(gtfs$stops),
# #                        spatial.filter) |>
# #   dplyr::pull(stop_id);
# #
# # filtered_gtfs <- GTFSwizard::filter_stop(gtfs, stops);
# 
# grouped_gtfs <- GTFSwizard::selection(
#   gtfs,
#   route_id,
#   direction_id
# );
# 
# selected_gtfs <- GTFSwizard::selection(
#   gtfs,
#   route_id,
#   route_id %in% for_bus_gtfs$routes$route_id[1:3]
# );
# 
# # attr(gtfs, "selection")$groups;
# selected_gtfs <- GTFSwizard::unselection(selected_gtfs);
# 
# GTFSwizard::plot_frequency(gtfs);
# GTFSwizard::plot_routefrequency(gtfs,
#                                 route = gtfs$routes$route_id[4:5]);
# 
# GTFSwizard::plot_headways(gtfs);
# 
# GTFSwizard::plot_servicespan(gtfs);
# GTFSwizard::plot_serviceheatmap(gtfs);
# GTFSwizard::plot_routeduration(gtfs);
# GTFSwizard::plot_servicesupply(gtfs);
# 
# GTFSwizard::tidy_raptor(gtfs, min_departure = '06:20:00', max_arrival = '09:40:00',
#                         dates = "2026-08-24", max_transfers = 2, keep = "all",
#                         stop_ids = "POLITECNICA");
# 
# gtfs$shapes <- NULL;
# 
# gtfs <- GTFSwizard::get_shapes(gtfs);
# 
# GTFSwizard::get_shapes_sf(gtfs$shapes);
# 
# GTFSwizard::get_stops_sf(gtfs$stops);
# 
# GTFSwizard::latlon2epsg(get_shapes_sf(gtfs)$shapes);
# 
# plot(gtfs);