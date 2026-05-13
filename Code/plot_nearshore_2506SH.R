library(tidyverse)
library(sf)
library(mapview)
library(atmData)
library(here)

theme_set(theme_bw())

# Define data range
range.lat <- c(33, 37)
range.long <- c(-123, -119)

# Load backscatter data
data("nasc_density_2506SH")
load("C:/KLS/CODE/Github/estimATM/2506SH/Output/nasc_final.Rdata")
load("C:/KLS/CODE/Github/estimATM/2506SH/Output/nasc_nearshore_final.Rdata")

# Load nav data
load("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_data.Rdata")
nav_lbc <- readRDS(here("Data/Nav/nav_vessel_LBC.rds"))
nav_lm <- readRDS(here("Data/Nav/nav_vessel_LM.rds"))

# Summarise nasc data for plotting
nasc <- nasc %>%
  group_by(transect, transect.orig, int) %>%
  summarise(
    bins    = length(int),
    bin.mid = as.integer(round(bins / 2)),
    lat     = lat[1],
    long    = long[1],
    NASC    = mean(cps.nasc)
  ) %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat)))

nasc.ns <- nasc.nearshore %>%
  group_by(transect, transect.orig, int) %>%
  summarise(
    bins    = length(int),
    bin.mid = as.integer(round(bins / 2)),
    lat     = lat[1],
    long    = long[1],
    NASC    = mean(cps.nasc)) %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat)))

# Filter density and nav data
dens <- nasc_density_2506SH %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat)))

nav <- nav %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat))) %>% 
  arrange(time)

nav_lbc <- nav_lbc %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat)))
  arrange(datetime)

nav_lm <- nav_lm %>% 
  filter(between(long, min(range.long), max(range.long)),
         between(lat, min(range.lat), max(range.lat)))
  arrange(datetime)

# Quick plot of biomass density from 2025
ggplot() +
  geom_path(data = nav_lbc, aes(long, lat), 
            linetype = "dashed", colour = "gray20") +
  geom_path(data = nav, aes(long, lat), 
            linetype = "dashed", colour = "gray20") +
  geom_point(data = filter(dens, anch.dens > 0), 
             aes(long, lat, size = anch.dens), 
             fill = "green", shape = 21, alpha = 0.5,
             show.legend = FALSE) +
  geom_point(data = filter(dens, sar.dens > 0),
             aes(long, lat, size = sar.dens), 
             fill = "red", shape = 21, alpha = 0.5,
             show.legend = FALSE) +
  geom_point(data = filter(dens, jack.dens > 0),
             aes(long, lat, size = jack.dens), 
             fill = "blue", shape = 21, alpha = 0.5,
             show.legend = FALSE) +
  guides(fill = guide_legend(), size = guide_legend()) +
  coord_map() +
  labs(title = "CPS biomass density")

ggplot() +
  geom_path(data = nav_lbc, aes(long, lat), 
            linetype = "dashed", colour = "gray20") +
  geom_path(data = nav, aes(long, lat), 
            linetype = "dashed", colour = "gray20") +
  geom_point(data = filter(nasc, NASC > 0), 
             aes(long, lat, size = NASC), 
             fill = "gray50", shape = 21, alpha = 0.5,
             show.legend = FALSE) +
  geom_point(data = filter(nasc.ns, NASC > 0), 
             aes(long, lat, size = NASC), 
             fill = "red", shape = 21, alpha = 0.5,
             show.legend = FALSE) +
  guides(fill = guide_legend(), size = guide_legend()) +
  coord_map() +
  labs(title = "CPS backscatter")
