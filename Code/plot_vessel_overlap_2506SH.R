# Load packages
pacman::p_load(tidyverse, lubridate, here, sf, gganimate, scatterpie,tmaptools)

# Install and load required packages from Github -------------------------------
if (!require("atm")) pkg_install("SWFSC/atm")
pacman::p_load_gh("SWFSC/atm")
 
# theme_set(theme_bw())

# Load nav data
load(here("Data/Nav/nav_data_scs.Rdata"))

# Extract tracklines from GPX file
nav.lm <- read_GPX(here("Data/Nav/lm_nav.gpx"))$tracks
nav.lbc <- read_GPX(here("Data/Nav/lbc_nav.gpx"))$tracks

# Extract trackline points from GPX file
nav.lm.points <- read_GPX(here("Data/Nav/lm_nav.gpx"))$track_points %>% 
  project_sf(crs = 4326) %>% 
  select(time, lat = Y, long = X)

nav.lbc.points <- read_GPX(here("Data/Nav/lbc_nav.gpx"))$track_points %>% 
  project_sf(crs = 4326) %>% 
  select(time, lat = Y, long = X)

# Load backscatter data
nasc.sh <- readRDS(here("Data/Backscatter/SH/nasc_vessel_SH.rds"))
nasc.lbc <- readRDS(here("Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds"))
# nasc.lm <- readRDS(here("Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds"))

# Combine backscatter data
nasc <- nasc.sh %>% 
  # bind_rows(nasc.lm) %>% 
  bind_rows(nasc.lbc)

# # Quick plot
# ggplot() +
#   geom_path(data = nav, aes(long, lat)) +
#   geom_sf(data = nav.lm, colour = "green") +
#   geom_sf(data = nav.lbc, colour = "red") +
#   coord_sf()

# Summarize latitude by date
nasc.summ.all <- nasc %>% 
  mutate(date.pdt = with_tz(datetime, tzone = "America/Los_Angeles")) %>%
  mutate(date = date(date.pdt)) %>% 
  mutate(vessel = case_when(
    vessel.name == "SH" ~ "Shimada",
    vessel.name == "LM" ~ "Lisa Marie",
    vessel.name == "LBC" ~ "Long Beach Carnage")) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(key = paste(vessel, date))

nav.summ <- nav %>% 
  mutate(time.pdt = with_tz(time, tzone = "America/Los_Angeles")) %>% 
  mutate(vessel = "Shimada",
         date = date(time.pdt)) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(key = paste(vessel, date)) %>% 
  mutate(sampling = case_when(
    key %in% unique(nasc.summ.all$key) ~ TRUE,
    TRUE ~ FALSE))

nav.summ.lbc <- nav.lbc.points %>%
  mutate(vessel = "Long Beach Carnage",
         date = date(time),
         key = paste(vessel, date),
         sampling = case_when(
           key %in% unique(nasc.summ.all$key) ~ TRUE,
           TRUE ~ FALSE)) %>%
  group_by(vessel, date, sampling) %>%
  summarise(lat = mean(lat))

nav.summ.lm <- nav.lm.points %>%
  mutate(vessel = "Lisa Marie",
         date = date(time),
         key = paste(vessel, date),
         sampling = case_when(
           key %in% unique(nasc.summ.all$key) ~ TRUE,
           TRUE ~ FALSE)) %>%
  group_by(vessel, date, sampling) %>%
  summarise(lat = mean(lat))

nav.summ.all <- nav.summ %>% bind_rows(nav.summ.lbc) %>% bind_rows(nav.summ.lm) 

# Add leg breaks
# Use start dates of each leg + end date of last leg
leg.breaks <- data.frame(
  date = lubridate::ymd(c("2025-06-11","2025-06-26", 
                          "2025-07-15", "2025-08-03", 
                          "2025-08-23", "2025-09-13")),
  leg = c("Start", "Leg 1","Leg 2","Leg 3","Leg 4","Leg 5"))

landmarks <- data.frame(
  lat = c(36.646, 40.50),
  name = c("Monterey","Cape Mendocino"),
  date = date("2025-07-07"))

# Read transect waypoints
wpt.filename  <- "waypoints_2506SH.csv"
wpt.types     <- c(Adaptive = "Adaptive", Carranza = "Carranza",
                   Compulsory = "Compulsory", Franklin = "Franklin",
                   Nearshore = "Nearshore", Offshore = "Offshore", 
                   Saildrone = "Saildrone")
wpt.regions   <- c("Central CA", "S. CA Bight", "WA/OR") # "Vancouver Is."

wpts <- read_csv(here("Data/Nav", wpt.filename))

# Convert planned transects to sf; CRS = crs.geog
wpts.sf <- wpts %>% 
  filter(Type %in% wpt.types, Region %in% wpt.regions) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  mutate(
    label = paste("Transect", Transect),
    popup = paste('<b>Transect:</b>', Transect, Type)
  )

transects.sf <- wpts.sf %>% 
  group_by(Type, Transect, Region) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(
    distance = round(as.numeric(st_length(.))/1852,1),
    label    = paste("Transect", Transect),
    popup    = paste('<b>Transect:</b>', Transect, Type, '<br/>',
                     'Distance:', distance, 'nmi<br/>')
  )

# Anticipated progress through the transect plan
# Leg 1:1-12, Leg 2:13-28, Leg 3:29-45, Leg 4: 46-59, Leg 5: 60-67 + EB cal
tx.breaks <- c(2, 12, 28, 45, 59, 67)

wpt.summ <- wpts %>% 
  filter(Type == "Compulsory", Region %in% wpt.regions) %>%
  group_by(Transect) %>% 
  slice(which.max(Longitude))

tx.plan <- read_csv(here("Data/Nav/transect_plan_2506SH.csv")) %>% 
  mutate(date = mdy(date),
         Transect = floor(tx.plan)) %>% 
  left_join(select(wpt.summ, Transect, Latitude))

tx.plan2 <- read_csv(here("Data/Nav/transect_plan_2506SH.csv")) %>% 
  mutate(date = mdy(date),
         Transect = floor(tx.plan2)) %>% 
  left_join(select(wpt.summ, Transect, Latitude))

goals <- wpts %>% 
  filter(Transect %in% tx.breaks, Type %in% c("Compulsory")) %>% 
  group_by(Transect) %>% 
  slice(which.max(Longitude)) %>% 
  ungroup() %>% 
  mutate(label = paste("Leg", seq_along(Transect)-1),
         date = date("2025-06-01")) %>% 
  mutate(label = case_when(
    label == "Leg 0" ~ "Start",
    TRUE ~ label
  ))
  
vessel.coord.plot <- ggplot() +  #nav.summ.all, aes(date, lat, group = vessel, colour = vessel)
  geom_line(linewidth = 1, linetype = "dashed") + 
  # planned transects
  geom_line(data = tx.plan, aes(date, Latitude), inherit.aes = FALSE,
            linewidth = 1, linetype = "dashed", colour = "gray50") +
  geom_point(data = tx.plan, aes(date, Latitude), inherit.aes = FALSE,
             size = 2, shape = 21, colour = "gray50", fill = "white") +
  # planned transects - revised
  geom_line(data = tx.plan2, aes(date, Latitude), inherit.aes = FALSE,
            linewidth = 1, linetype = "dashed", colour = "blue") +
  geom_point(data = tx.plan2, aes(date, Latitude), inherit.aes = FALSE,
             size = 2, shape = 21, colour = "blue", fill = "white") +
  geom_vline(xintercept = leg.breaks$date, linetype = "dashed") +
  geom_hline(yintercept = goals$Latitude, linetype = "dashed") +
  geom_text(data = leg.breaks, aes(date, 31, label = leg), inherit.aes = FALSE) +
  geom_text(data = goals, aes(date, Latitude + 0.4, label = label), inherit.aes = FALSE) +
  # geom_text(data = landmarks, aes(date, lat+0.25, label = name), inherit.aes = FALSE) +
  geom_line(data = nav.summ.all, aes(date, lat, group = vessel, colour = vessel)) + 
  # geom_line(data = nav.summ.all, aes(date, lat, group = vessel, colour = vessel),
  #           linewidth = 1, linetype = "dashed") + 
  geom_point(data = nav.summ.all, aes(date, lat, group = vessel, colour = vessel, fill = sampling), 
             size = 2, shape = 21) +
  # geom_point(data = filter(nav.summ.all, SOG < 1), 
  #            aes(date, lat, group = vessel, colour = vessel), 
  #            fill = "blue", size = 2, shape = 21, show.legend = FALSE) +
  scale_colour_discrete(name = "Vessel") +
  scale_fill_manual(name = "Sampling", values = c("TRUE" = 'black', "FALSE" = 'white')) +
  scale_x_date(date_breaks = "10 days") + 
  labs(x = "Date", y = "Latitude") + 
  theme_bw()

ggsave(vessel.coord.plot, 
       filename = here("Figs/fig_vessel_coordination_all.png"), 
       height = 7, width = 12)
