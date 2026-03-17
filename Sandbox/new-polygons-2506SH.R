library(tidyverse)
library(sf)
library(here)
library(mapview)
library(atm)

# Define CRSs
crs.geog <- crs.geog
crs.proj <- crs.proj

# Load nearshore strata and nasc
load(here("Output/strata_nearshore_final.Rdata"))
load(here("Output/nasc_nearshore_final.Rdata"))

# Read North America land mask shapefile
na_landmask <- st_read(here("Data/GIS/na_landmask_final.shp")) %>% 
  st_union() %>% 
  st_transform(crs = crs.proj)

nasc.plot <- nasc.nearshore %>% 
  select(filename, vessel.name, transect, transect.name, int, lat, long, cps.nasc) %>% 
  group_by(filename, vessel.name, transect.name, transect, int) %>% 
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc)) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

# Draw pseudo-transects --------------------------------------------------------
# Get transect ends, calculate bearing, and add transect spacing
tx.ends.ns <- nasc.nearshore %>% 
  group_by(transect.name, transect, vessel.name) %>% 
  summarise(
    lat.i  = lat[which.max(long)],
    long.i = max(long),
    lat.o  = lat[which.min(long)],
    long.o = min(long)) %>% 
  mutate(
    brg = swfscMisc::bearing(lat.i, long.i,
                             lat.o, long.o)[1]) %>% 
  arrange(transect)

tx.ends.ns.long <- tx.ends.ns %>% 
  select(vessel.name, transect, lat = lat.i, long = long.i, brg) %>%
  bind_rows(select(tx.ends.ns, vessel.name, transect, lat = lat.o, long = long.o, brg)) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

tx.lines.ns <- tx.ends.ns.long %>% 
  group_by(transect.name, vessel.name, transect) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957)) %>% 
  st_transform(crs.proj) %>% 
  arrange(vessel.name, transect)
  

# Load 5 m bathy
bathy_5m <- st_read(here("Data/GIS/bathy_5m_wc_poly.shp"))
ns_mask <- st_cast(bathy_5m, "POLYGON")

# Get offshore most point for each transect
nasc.offshore <- nasc.nearshore %>% 
  group_by(transect, vessel.name) %>% 
  summarize(long = min(long),
            lat = lat[which.min(long)]) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

mapview(na_landmask) + mapview(bathy_5m, color = "black") + mapview(strata.nearshore) + mapview(nasc.plot) + mapview(nasc.offshore, zcol = "transect")


# Example Line
line <- st_sfc(st_linestring(matrix(c(0,0, 10,10), ncol=2, byrow=TRUE)))

# 1. Get points
first_point <- st_coordinates(tx.lines.ns[1])[1, 1:2]
last_point  <- st_coordinates(tx.lines.ns[1])[2, 1:2]
# last_point <- st_coordinates(tx.lines.ns)[nrow(st_coordinates(tx.lines.ns)), 1:2]

# 2. Calculate direction
direction <- last_point - first_point
direction <- direction / sqrt(sum(direction^2))

# 3. Define extension distance (e.g., 10% of line length)
ext_dist <- as.numeric(st_length(tx.lines.ns)[1])*.25
new_first_point <- first_point - (ext_dist * direction)
new_last_point <- last_point   + (ext_dist * direction)

# 4. Create new line
extended_line <- st_sfc(st_linestring(rbind(new_first_point, new_last_point)))
st_crs(extended_line) <- st_crs(tx.lines.ns) # Maintain CRS

mapview(strata.nearshore[21,]) + mapview(extended_line) + mapview(tx.lines.ns[1,], color = "black")

mapview(strata.nearshore)

# Select test polygon
test.poly <- strata.nearshore[21,] %>% 
  st_transform(crs.proj)

test.split <- test.poly %>%
  lwgeom::st_split(extended_line) %>%
  st_collection_extract("POLYGON") %>% mutate(area = st_area(.)) %>% 
  filter(as.numeric(area) > 100000)

result <- test.split[st_within(tx.lines.ns, test.split), ]


mapview(test.split, zcol = "area") + mapview(extended_line, color = "black")

# Test buffering
buff.dist <- 1852 * 3.75

## Round endCap
buff <- st_buffer(extended_line, dist = buff.dist)
mapview(buff) + mapview(extended_line, color = "black")

buff_sq <- st_buffer(extended_line, dist = buff.dist, endCapStyle = "SQUARE")
mapview(buff_sq) + mapview(extended_line, color = "black")

buff_fl <- st_buffer(extended_line, dist = buff.dist, endCapStyle = "FLAT")
mapview(buff_fl) + mapview(extended_line, color = "black")

buff.clip <- buff %>% 
  st_transform(crs.geog) %>% 
  st_difference(bathy_5m)

mapview(buff_fl) + mapview(bathy_5m) + mapview(extended_line, color = "black")

# Buffer multiple transects
tx.sub <- tx.lines.ns[1:10,]

tx.buff <-  st_buffer(tx.sub, dist = buff.dist, endCapStyle = "SQUARE")
mapview(tx.buff)

tx.trim <- st_difference(tx.buff, st_transform(ns_mask, crs = crs.proj))

tx.trim2 <- st_intersection(tx.trim, st_transform(ns_buff, crs = crs.proj))

ns_buff <- st_buffer(st_transform(ns_mask, crs.proj), dist = 1852 * 5) 

mapview(ns_buff) + mapview(tx.trim2)

# -------------------------------------
wpt.filename  <- "waypoints_2506SH.csv"
wpt.types     <- c(Adaptive = "Adaptive", Carranza = "Carranza",
                   Compulsory = "Compulsory", Franklin = "Franklin",
                   Nearshore = "Nearshore", Offshore = "Offshore", 
                   Saildrone = "Saildrone")
wpt.regions   <- c("Central CA", "S. CA Bight", "WA/OR", "Santa Cruz Island", "Santa Catalina Island") # "Vancouver Is."        

# Read transect waypoints
wpts <- read_csv(here("Data/Nav", wpt.filename))

# Convert planned transects to sf; CRS = crs.geog
wpts.sf <- wpts %>% 
  filter(Type %in% wpt.types, Region %in% wpt.regions) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) 

transects.sf <- wpts.sf %>% 
  group_by(Type, Transect, Region) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(ext_pct = case_when(
    str_detect(tolower(Region), "island") ~ 0.25,
    TRUE ~ 2
  ))

transects.proj <- st_transform(transects.sf, crs = crs.proj)

rm(transects.ext)

for (i in unique(transects.proj$Type)) {
  for (j in unique(transects.proj$Transect)){
    # i = "Compulsory"
    # j = 2
    
    tx.sub <- filter(transects.proj, Type == i, Transect == j) 
    n_wpts <- nrow(data.frame(st_coordinates(tx.sub)))
    
    if (nrow(tx.sub) > 0) {
      # 1. Get end points
      first_point <- st_coordinates(tx.sub)[1, 1:2]
      last_point  <- st_coordinates(tx.sub)[n_wpts, 1:2]
      
      # 2. Calculate direction
      direction <- last_point - first_point
      direction <- direction / sqrt(sum(direction^2))
      
      # 3. Define extension distance (e.g., 25% of line length)
      ext_dist <- as.numeric(st_length(tx.sub))*tx.sub$ext_pct
      new_first_point <- first_point - (ext_dist * direction)
      new_last_point <- last_point   + (ext_dist * direction)
      
      # 4. Create new line
      extended_line <- st_sfc(st_linestring(rbind(new_first_point, new_last_point))) %>% 
        st_as_sf()
      st_crs(extended_line) <- st_crs(tx.lines.ns) # Maintain CRS
      
      # 5. Add attributes
      extended_line <- extended_line %>% 
        mutate(Type = tx.sub$Type,
               Transect = tx.sub$Transect,
               Region = tx.sub$Region)
      
      if (exists("transects.ext")) {
        transects.ext <- bind_rows(transects.ext, extended_line)
      } else {
        transects.ext <- extended_line
      }
      
      # Check result
      # mapview(extended_line) + mapview(tx.sub, color = "black")
      
    }
    
  }
}

mapview(transects.ext, zcol = "Type")

transects.ext <- transects.ext %>% 
  mutate(buff_dist = case_when(
    Type == "Nearshore" & str_detect(tolower(Region), "island") ~ 1.25,
    Type == "Nearshore" & !str_detect(tolower(Region), "island") ~ 3.75,
    TRUE ~ 7.5
  ))

# Resume here
transects.ext.buff <- st_buffer(transects.ext, dist = buff_dist)


# 1. Get points
first_points <- st_coordinates(tx.lines.ns)[, 1:2]
last_points  <- st_coordinates(tx.lines.ns)[2, 1:2]
# last_point <- st_coordinates(tx.lines.ns)[nrow(st_coordinates(tx.lines.ns)), 1:2]

first_points <- st_coordinates(tx.lines.ns) %>% data.frame() %>% 
  group_by(L1) %>% 
  slice(1)

last_points <- st_coordinates(tx.lines.ns) %>% data.frame() %>% 
  group_by(L1) %>% 
  slice(2)


# 2. Calculate direction
direction <- last_point - first_point
direction <- direction / sqrt(sum(direction^2))

# 3. Define extension distance (e.g., 10% of line length)
ext_dist <- as.numeric(st_length(tx.lines.ns)[1])*.25
new_first_point <- first_point - (ext_dist * direction)
new_last_point <- last_point   + (ext_dist * direction)

# 4. Create new line
extended_line <- st_sfc(st_linestring(rbind(new_first_point, new_last_point)))
st_crs(extended_line) <- st_crs(tx.lines.ns) # Maintain CRS


