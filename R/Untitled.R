library(GTFSwizard);

gtfs <- GTFSwizard::read_gtfs("data/gtfs/buzufba_gtfs.zip");

summary(gtfs);

GTFSwizard::explore_gtfs(gtfs);

GTFSwizard::get_servicepattern(gtfs);
