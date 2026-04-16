# Load bathymetry contours for creating eE stations
bathy.ctd <- st_read(here("Data/GIS/bathy_us_wc_50m.shp"))
transects.ctd <- transects %>% 
  filter(Type %in% c("Adaptive","Compulsory"))
# mapview::mapview(bathy.eE, zcol = "CONTOUR")

# Create eE stations as intersection between transects and isobaths
ctd <- st_intersection(st_transform(bathy.ctd, crs.geog), 
                       st_transform(transects.ctd, crs.geog)) %>% 
  st_cast("POINT") %>% 
  mutate(Transect = sprintf("%03d", Transect),
         depth = abs(CONTOUR),
         name = paste0("CTD", Transect,"_", sprintf("%04d", depth)),
         lon = as.data.frame(st_coordinates(.))$X,
         lat  = as.data.frame(st_coordinates(.))$Y) %>% 
  select(Transect, name, Region, lon, lat, depth, CONTOUR) %>% 
  filter(as.numeric(Transect) %in% ctd.tx.range)

# mapview(bathy.ctd, zcol = "CONTOUR") + mapview(transects, color = "black") + mapview(ctd, zcol = "CONTOUR")

# Get first waypoint for each transect
eDNA.starts <- wpts %>% 
  filter(Type %in% c("Compulsory","Adaptive"),
         Transect %in% edna.tx.range) %>%
  group_by(Transect) %>% 
  slice(1) %>% 
  mutate(id = 0) %>% 
  select(Transect, Type, lon = long, lat, id)

# Create stations along each transect
eDNA <- transects %>%
  filter(Type %in% c("Compulsory","Adaptive"),
         Transect %in% edna.tx.range) %>%
  st_transform(crs.proj) %>%
  st_line_sample(density = 1/units::set_units(edna.spacing * 1852, m)) %>%
  st_cast("POINT") %>% st_as_sf() %>% st_transform(crs.geog) %>% 
  mutate(
    lon = as.data.frame(st_coordinates(.))$X,
    lat  = as.data.frame(st_coordinates(.))$Y) %>% 
  rename(geometry = x)

uctd <- transects %>%
  filter(Type %in% c("Compulsory","Adaptive"),
         Transect %in% uctd.tx.range) %>%
  st_transform(crs.proj) %>%
  st_line_sample(density = 1/units::set_units(uctd.spacing * 1852, m)) %>%
  st_cast("POINT") %>% st_as_sf() %>% st_transform(crs.geog) %>% 
  mutate(
    lon = as.data.frame(st_coordinates(.))$X,
    lat  = as.data.frame(st_coordinates(.))$Y)

# Buffer transects to get transect info for eDNA points
transects.buffer <- transects %>% 
  filter(Type %in% c("Compulsory","Adaptive")) %>%
  st_transform(crs.proj) %>%
  st_buffer(units::set_units(1 * 1852, m)) %>% 
  select(Type, Transect) %>% 
  st_transform(crs.geog)

# Add transect info to station waypoints and create names
eDNA <- eDNA %>%
  st_intersection(transects.buffer) %>% 
  mutate(id = seq_along(Transect)) %>% 
  bind_rows(eDNA.starts) %>% 
  arrange(Transect, id) %>% 
  group_by(Transect) %>% 
  mutate(Station = rank(id)) %>% 
  ungroup() %>% 
  mutate(Transect = sprintf("%03d", Transect),
         name = paste0("eDNA", Transect,"_", sprintf("%03d", Station), "S")) %>% 
  select(Transect, name, Type, lon, lat, everything())

uctd <- uctd %>%
  st_intersection(transects.buffer) %>% 
  mutate(id = seq_along(Transect)) %>% 
  group_by(Transect) %>% 
  mutate(Station = rank(id)) %>% 
  ungroup() %>% 
  mutate(Transect = sprintf("%03d", Transect),
         name = paste0("UCTD", Transect,"_", sprintf("%03d", Station))) %>% 
  select(Transect, name, Type, lon, lat, everything())

# eDNA.buff <- st_buffer(eDNA, units::set_units(edna.spacing * 1852, m)/2)
# mapview(transects, zcol = "Type") + mapview(eDNA.buff) + mapview(eDNA)
# mapview(transects) + mapview(eDNA, color = "black") + mapview(uctd, zcol = "Transect")

# Write waypoints to GPX
eDNA %>% 
  select(name) %>% 
  st_write(dsn = "Output/waypoints_updated/eDNA_waypoints.gpx", driver = "GPX", delete_layer = TRUE)

uctd %>% 
  select(name) %>% 
  st_write(dsn = "Output/waypoints_updated/uctd_waypoints.gpx", driver = "GPX", delete_layer = TRUE)

ctd %>% 
  select(name) %>% 
  st_write(dsn = "Output/waypoints_updated/ctd_waypoints.gpx", driver = "GPX", delete_layer = TRUE)
