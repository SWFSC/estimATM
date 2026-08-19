library(tidyverse)
library(sf)
library(here)
library(mapview)
library(atm)
library(rnaturalearth)
library(smoothr)

# Define CRS
crs.geog <- 4326
crs.proj <- 3310

wpt.regions   <- c("Central CA", "S. CA Bight", "WA/OR", "Santa Cruz Island", "Santa Catalina Island") # "Vancouver Is."        

# Define transect buffering preferences for stratum polygon creation
na.buffer.dist <- 1 # Distance (nmi) to buffer N. American land mask for core strata masking
na.buffer.dist.ns <- 5 # Distance (nmi) to buffer N. American land mask for core strata masking
ci.buffer.dist <- 2.5 # Distance (nmi) to buffer Channel Island land mask for nearshore strata masking
ci.clip.dist   <- 0.1 # Distance (nmi) to buffer Channel Island land mask for nearshore strata clipping

# Get countries, buffer by a small amount, and remove interior holes
na_landmask <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(name_en %in% c("United States of America", "Canada")) %>% 
  st_union() %>% 
  st_transform(crs.proj) %>% 
  st_buffer(dist = na.buffer.dist*1852) %>% 
  smoothr::fill_holes(threshold = units::set_units(Inf, "m^2"))

# Buffer the na_landmask by the typical nearshore transect distance
na_buffer <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(name_en %in% c("United States of America", "Canada")) %>% 
  st_union() %>% 
  st_transform(crs.proj) %>% 
  st_buffer(dist = na.buffer.dist.ns*1852)

bathy_clip <- st_read(here("Data/GIS/bathy_5m_wc.shp")) %>% 
  st_cast("POLYGON") %>% 
  st_transform(crs.proj)

# # Read Channel Island shapefile and buffer by 2.5 nmi
# ci_buffer <- st_read(here("Data/GIS/channel_islands.shp")) %>%
#   filter(name %in% wpt.regions) %>%
#   st_union() %>% 
#   st_transform(crs = crs.proj) %>%
#   st_buffer(dist = ci.buffer.dist * 1852)

# Read US EEZ shapefile
eez_usa <- st_read(here("Data/GIS/eez_us.shp")) %>% 
  st_transform(crs.proj)

# Get nearshore survey footprint
ns_footprint <- na_buffer %>% 
  # st_union(ci_buffer, na_buffer) %>% 
  st_intersection(eez_usa) %>% 
  st_difference(na_landmask) %>% 
  st_difference(bathy_clip) %>% 
  st_simplify()

# Read offshore clip polygon
offshore.clip <- st_read(here("Data/GIS/waypoint_polygon.shp")) %>% 
  st_transform(crs.proj)

# Get the intersection with the US EEZ
sd_footprint <- st_intersection(offshore.clip, eez_usa) %>% 
  st_difference(na_landmask) %>% 
  st_difference(bathy_clip) %>% 
  st_simplify()

# Write to shapefile
st_write(ns_footprint, here("Data/GIS/Permits/sampling_area_nearshore.shp"), 
         delete_layer = TRUE)

st_write(sd_footprint, here("Data/GIS/Permits/sampling_area_saildrone.shp"), 
         delete_layer = TRUE)

# mapview(sd_footprint) + mapview(ns_footprint)
