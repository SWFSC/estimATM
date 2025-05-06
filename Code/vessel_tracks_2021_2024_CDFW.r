# Compile survey tracks for Lasker, LBC, and LM from 2021-2024 for CDFW

library(tidyverse)
library(here)
library(sf)
library(fs)

dir_create(here("Output/CDFW"))

# Compile Lasker data
## 2021 Data
load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Backscatter/nasc_all.Rdata")
nasc.2021 <- nasc %>% 
  select(transect, transect.name, vessel.name, datetime, lat, long) %>% 
  write_csv(here("Output/CDFW/nav_core_2107RL.csv"))

load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Trawl/all_trawl_data-final.Rdata")
haul.paths <- select(haul, haul, lat = startLatDecimal, long = startLongDecimal) %>% 
  bind_rows(select(haul, haul, lat = stopLatDecimal, long = stopLongDecimal)) %>% 
  arrange(haul) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(haul) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

st_write(haul.paths, here("Output/CDFW/trawl_paths_2107RL.shp"), 
         delete_layer = TRUE)

ggplot() + 
  geom_path(data = nasc.2021, aes(long, lat, group = transect.name, colour = vessel.name)) + 
  geom_sf(data = haul.paths) + coord_sf()

ggsave(here("Output/CDFW/nav_core_2107RL.png"))

## 2022 Data
load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Backscatter/nasc_all.Rdata")
nasc.2022 <- nasc %>% 
  select(transect, transect.name, vessel.name, datetime, lat, long) %>% 
  write_csv(here("Output/CDFW/nav_core_2207RL.csv"))

load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Trawl/all_trawl_data-final.Rdata")
haul.paths <- select(haul, haul, lat = startLatDecimal, long = startLongDecimal) %>% 
  bind_rows(select(haul, haul, lat = stopLatDecimal, long = stopLongDecimal)) %>% 
  arrange(haul) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(haul) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

st_write(haul.paths, here("Output/CDFW/trawl_paths_2207RL.shp"), 
         delete_layer = TRUE)

ggplot() + 
  geom_path(data = nasc.2022, aes(long, lat, group = transect.name, colour = vessel.name)) + 
  geom_sf(data = haul.paths) + coord_sf()

ggsave(here("Output/CDFW/nav_core_2207RL.png"))

## 2023 Data
load("C:/KLS/CODE/Github/estimATM/2307RL/Data/Backscatter/nasc_all.Rdata")
nasc.2023 <- nasc %>% 
  select(transect, transect.name, vessel.name, datetime, lat, long) %>% 
  write_csv(here("Output/CDFW/nav_core_2307RL.csv"))

load("C:/KLS/CODE/Github/estimATM/2307RL/Data/Trawl/all_trawl_data-final.Rdata")
haul.paths <- select(haul, haul, lat = startLatDecimal, long = startLongDecimal) %>% 
  bind_rows(select(haul, haul, lat = stopLatDecimal, long = stopLongDecimal)) %>% 
  arrange(haul) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(haul) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

st_write(haul.paths, here("Output/CDFW/trawl_paths_2307RL.shp"), 
         delete_layer = TRUE)

ggplot() + 
  geom_path(data = nasc.2023, aes(long, lat, group = transect.name, colour = vessel.name)) + 
  geom_sf(data = haul.paths) + coord_sf()

ggsave(here("Output/CDFW/nav_core_2307RL.png"))

## 2024 Data
load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Backscatter/nasc_all.Rdata")
nasc.2024 <- nasc %>% 
  select(transect, transect.name, vessel.name, datetime, lat, long) %>% 
  write_csv(here("Output/CDFW/nav_core_2407RL.csv"))

load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Trawl/all_trawl_data-final.Rdata")
haul.paths <- select(haul, haul, lat = startLatDecimal, long = startLongDecimal) %>% 
  bind_rows(select(haul, haul, lat = stopLatDecimal, long = stopLongDecimal)) %>% 
  arrange(haul) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(haul) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

st_write(haul.paths, here("Output/CDFW/trawl_paths_2407RL.shp"), 
         delete_layer = TRUE)

ggplot() + 
  geom_path(data = nasc.2024, aes(long, lat, group = transect.name, colour = vessel.name)) + 
  geom_sf(data = haul.paths) + coord_sf()

ggsave(here("Output/CDFW/nav_core_2407RL.png"))

# Compile nearshore data
## 2021 Data
nav.lm <- read_rds("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_vessel_LM.rds")
nav.lbc <- read_rds("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_vessel_LBC.rds")
nav.ns <- bind_rows(nav.lbc, nav.lm) %>% filter(lat != 999, long != 999) %>% 
  select(datetime, long, lat, vessel.name, transect)
write_csv(nav.ns, here("Output/CDFW/nav_nearshore_2107RL.csv"))
ggplot(nav.ns, aes(long, lat, colour = vessel.name, group = transect)) + geom_path() + coord_map()
ggsave(here("Output/CDFW/nav_nearshore_2107RL.png"))

## 2022 Data
nav.lm <- read_rds("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_vessel_LM.rds")
nav.lbc <- read_rds("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_vessel_LBC.rds")
nav.ns <- bind_rows(nav.lbc, nav.lm) %>% filter(lat != 999, long != 999) %>% 
  select(datetime, long, lat, vessel.name, transect)
write_csv(nav.ns, here("Output/CDFW/nav_nearshore_2207RL.csv"))
ggplot(nav.ns, aes(long, lat, colour = vessel.name, group = transect)) + geom_path() + coord_map()
ggsave(here("Output/CDFW/nav_nearshore_2207RL.png"))

## 2023 Data
nav.lm <- read_rds("C:/KLS/CODE/Github/estimATM/2307RL/Data/Nav/nav_vessel_LM.rds")
nav.lbc <- read_rds("C:/KLS/CODE/Github/estimATM/2307RL/Data/Nav/nav_vessel_LBC.rds")
nav.ns <- bind_rows(nav.lbc, nav.lm) %>% filter(lat != 999, long != 999) %>% 
  select(datetime, long, lat, vessel.name, transect)
write_csv(nav.ns, here("Output/CDFW/nav_nearshore_2307RL.csv"))
ggplot(nav.ns, aes(long, lat, colour = vessel.name, group = transect)) + geom_path() + coord_map()
ggsave(here("Output/CDFW/nav_nearshore_2307RL.png"))

## 2024 Data
nav.lm <- read_rds("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_vessel_LM.rds")
nav.lbc <- read_rds("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_vessel_LBC.rds")
nav.ns <- bind_rows(nav.lbc, nav.lm) %>% filter(lat != 999, long != 999) %>% 
  select(datetime, long, lat, vessel.name, transect)
write_csv(nav.ns, here("Output/CDFW/nav_nearshore_2407RL.csv"))
ggplot(nav.ns, aes(long, lat, colour = vessel.name, group = transect)) + geom_path() + coord_map()
ggsave(here("Output/CDFW/nav_nearshore_2407RL.png"))
