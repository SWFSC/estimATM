library(tidyverse)
library(here)
library(tmaptools)
library(atm)
library(sf)


# Read GPX file
route <- read_GPX(here("Data/Nav/surface eDNA.gpx"))

# Create data frame of waypoints
wpts <- route$waypoints %>% 
  project_sf(crs = 4326) %>% 
  select(lon = X, lat = Y, name) %>% 
  mutate(name = str_replace(name, "x", "eDNA0")) %>% 
  mutate(name = str_replace(name, "_50S", "_0050S")) %>% 
  mutate(name = str_replace(name, "_150S", "_0150S")) %>% 
  mutate(name = str_replace(name, "_300S", "_0300S")) %>% 
  mutate(name = str_replace(name, "_500S", "_0500S")) %>%  
  mutate(name = str_replace(name, "_800S", "_0800S")) %>% 
  mutate(name = str_replace(name, "0S", "0")) %>% 
  arrange(name)

write_csv(select(wpts, name, lat, lon), here("Data/Nav/surface_eDNA.csv"))

