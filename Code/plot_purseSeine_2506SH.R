# This script imports and processes purse seine data from fishing vessels -------
# Source following the section entitled estimateNearshore in estimateBiomass ----

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,readr, lubridate, here, plotly, sf, mapview,
               scatterpie,tmaptools)

# Install and load required packages from Github -------------------------------
if (!require("atm")) pak::pkg_install("SWFSC/atm")
if (!require("surveyR")) pak::pkg_install("SWFSC/surveyR")
pacman::p_load_gh("SWFSC/atm")
pacman::p_load_gh("SWFSC/surveyR")

# Get project settings----------------------------------------------------------
# Get project name from directory
prj.name <- last(unlist(str_split(here(),"/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here("Doc/settings", prj.settings))

# Import data ------------------------------------------------------------------

## Import set data 
load(here("Data/Seine/seine_data_raw.Rdata"))

## LM (update after receiving QA/QC'd data)
lm.sets <- sets.all %>% 
  filter(ship == "LM", cruise == cruise.name) %>% 
  mutate(date = mdy(date),
         vessel_name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set))

## LBC (uncomment once data is available)
lbc.sets <- sets.all %>% 
  filter(ship == "LBC", cruise == cruise.name) %>% 
  mutate(date = mdy(date),
         vessel_name = "Long Beach Carnage",
         vessel.name = "LBC",
         key.set = paste(vessel.name, date, set))

sets <- bind_rows(lbc.sets, lm.sets)

# Save results
save(sets, lm.sets, lbc.sets, file = here("Output/purse_seine_sets.Rdata"))

## Import catch data -----------------------------------------------------------
### LM
lm.catch <- set.catch.all %>% 
  filter(ship == "LM", cruise == cruise.name) %>% 
  left_join(select(spp.codes, species, scientificName, commonName)) %>% 
  filter(scientificName %in% cps.spp) %>%
  mutate(date = mdy(date),
         vessel.name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set)) %>% 
  filter(!is.na(scientificName)) %>% 
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalNum = sum(totalNum),
            totalWeight = sum(totalWeight))

### LBC
lbc.catch <- set.catch.all %>% 
  filter(ship == "LBC", cruise == cruise.name) %>% 
  left_join(select(spp.codes, species, scientificName, commonName)) %>% 
  filter(scientificName %in% cps.spp) %>%
  mutate(date = mdy(date),
         vessel.name = "LBC",
         vessel.name = "LBC",
         key.set = paste(vessel.name, date, set)) %>% 
  filter(!is.na(scientificName)) %>% 
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalNum = sum(totalNum),
            totalWeight = sum(totalWeight))

# Save results
save(lm.catch, lbc.catch, file = here("Output/purse_seine_catch.Rdata"))

## Import specimen data ----------------------------------------------------
### LM
lm.lengths <- set.lengths.all %>% 
  filter(ship == "LM", cruise == cruise.name) %>% 
  left_join(select(spp.codes, species, scientificName, commonName)) %>%
  filter(scientificName %in% cps.spp) %>%
  mutate(date = mdy(date),
         vessel.name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set),
         label = paste("Date:", date, "Set:", set, "Fish num:", specimenNumber)) %>% 
  mutate(
    totalLength_mm = case_when(
      scientificName == "Clupea pallasii" ~ 
        convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
      scientificName == "Engraulis mordax" ~ 
        convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Scomber japonicus" ~ 
        convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"))) %>% 
  mutate(missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))

### LBC
lbc.lengths <- set.lengths.all %>% 
  filter(ship == "LBC", cruise == cruise.name) %>% 
  left_join(select(spp.codes, species, scientificName, commonName)) %>%
  filter(scientificName %in% cps.spp) %>%
  mutate(date = mdy(date),
         vessel.name = "Long Beach Carnage",
         vessel.name = "LBC",
         key.set = paste(vessel.name, date, set),
         label = paste("Date:", date, "Set:", set, "Fish num:", specimenNumber)) %>% 
  mutate(
    totalLength_mm = case_when(
      scientificName == "Clupea pallasii" ~ 
        convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
      scientificName == "Engraulis mordax" ~ 
        convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Scomber japonicus" ~ 
        convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"))) %>% 
  mutate(missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))

# Save results
save(lm.lengths, lbc.lengths, file = here("Output/purse_seine_specimens.Rdata"))

# Combine nearshore lengths from LM and LBC
lengths.ns <- bind_rows(lbc.lengths, lm.lengths)

saveRDS(lengths.ns, here("output/lw_data_nearshore.rds"))

### FSV
lengths.fsv <- readRDS(here("Output/lw_data_checkTrawls.rds")) %>% 
  filter(scientificName %in% unique(lengths.ns$scientificName))

# Get max TL for plotting L/W models
L.max.ns <- select(lengths.ns, scientificName, totalLength_mm) %>%
  bind_rows(select(lengths.fsv, scientificName, totalLength_mm)) %>% 
  # Add LBC data
  filter(scientificName %in%  cps.spp) %>%
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

# Plot LW data from specimens --------------------------------------------------
# Data frame for storing results
lw.df.ns <- data.frame()

# Generate length/weight curves
for (i in unique(L.max.ns$scientificName)) {
  # Create a length vector for each species
  totalLength_mm <- seq(0, L.max.ns$max.TL[L.max.ns$scientificName == i])
  
  # Calculate weights from lengths
  weightg <- estimate_weight(i, totalLength_mm, season = tolower(survey.season))
  
  # Combine results
  lw.df.ns <- bind_rows(lw.df.ns, data.frame(scientificName = i, weightg, totalLength_mm))
}

# Convert lengths for plotting
lw.df.ns <- lw.df.ns %>% 
  mutate(
    forkLength_mm     = convert_length(scientificName, totalLength_mm, "TL", "FL"),
    standardLength_mm = convert_length(scientificName, totalLength_mm, "TL", "SL")
  )

# Plot L/W data
# Examine length differences by leg
lw.plot.ns <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = lengths.ns,
             aes(totalLength_mm, weightg, group = sex, colour = sex), alpha = 0.75) +
  # Plot individuals with missing lengths and weights
  geom_point(data = filter(lengths.ns, missing.length == TRUE),
             aes(totalLength_mm, weightg),
             shape = 21, fill = 'red',  size = 2.5) +
  geom_point(data = filter(lengths.ns, missing.weight == TRUE),
             aes(totalLength_mm, weightg),
             shape = 21, fill = 'blue', size = 2.5) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free") +
  scale_colour_manual(name = "Sex", values = c("female" = "pink", "male" = "lightblue",
                                             "unknown" = "#FFC926", "Not Sexed" = "purple")) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

# ggplotly(lw.plot.ns)

# Save length/weight plot
ggsave(lw.plot.ns, filename = here("Figs/fig_LW_plots_ns.png"),
       width = 10, height = 7) 

# Compare lengths and weights between LM/LBC/FSV
lw.plot.comp <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = lengths.fsv,
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  geom_point(data = lengths.ns,
             aes(totalLength_mm, weightg, colour = ship), alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free") +
  # scale_colour_manual(name = "Sex", values = c("Female" = "pink", "Male" = "lightblue",
  #                                              "Unknown" = "#FFC926", "Not Sexed" = "purple")) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

lw.plot.comp.ns <- ggplot() +
  # Plot L/W data for current survey
  # geom_point(data = lengths.fsv,
  #            aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  geom_point(data = lengths.ns,
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free", nrow = 1) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  ggtitle("Nearshore") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

lw.plot.comp.fsv <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = lengths.fsv,
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  # geom_point(data = lengths.ns,
  #            aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free", nrow = 1) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  ggtitle("FSV") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

lw.plot.comp.grid <- cowplot::plot_grid(lw.plot.comp.ns, lw.plot.comp.fsv,
          nrow = 2)

ggsave(lw.plot.comp, filename = here("Figs/fig_LW_plots_NS-FSV.png"),
       width = 10, height = 6)

ggsave(lw.plot.comp.grid, filename = here("Figs/fig_LW_plots_grid_NS-FSV.png"),
       width = 10, height = 6)

# Summarize specimen data ------------------------------------------------
lm.catch.summ <- lm.catch %>%
  select(key.set, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) %>% 
  right_join(select(lm.sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) 

# Summarize catch from all species
lbc.catch.summ <- lbc.catch %>% 
  select(key.set, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) %>% 
  right_join(select(lbc.sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set)

lm.spec.summ <- lm.lengths %>%
  group_by(key.set, scientificName) %>%
  summarise(totalWeight = sum(weightg, na.rm = TRUE)) %>%
  tidyr::spread(scientificName, totalWeight) %>%
  right_join(select(lm.sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) %>%
  ungroup()

lbc.spec.summ <- lbc.lengths %>%
  group_by(key.set, scientificName) %>%
  summarise(totalWeight = sum(weightg, na.rm = TRUE)) %>%
  tidyr::spread(scientificName, totalWeight) %>%
  right_join(select(lm.sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) %>%
  ungroup()

# Make specimen summaries spatial -----------------------------------------
lm.catch.summ.sf <- lm.catch.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

lbc.catch.summ.sf <- lbc.catch.summ %>%
  st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
# So far, LM catch is from the specimen data (no bucket sample info, yet)
# LBC catch is from bucket samples (no specimen info, yet)
set.summ.wt <- lm.catch.summ %>% 
  bind_rows(lbc.catch.summ) 

# Add species with zero total weight
if (!has_name(set.summ.wt, "Engraulis mordax"))      {set.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt, "Sardinops sagax"))       {set.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt, "Scomber japonicus"))     {set.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt, "Trachurus symmetricus")) {set.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt, "Clupea pallasii"))       {set.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt, "Etrumeus acuminatus"))   {set.summ.wt$`Etrumeus acuminatus`   <- 0}
if (!has_name(set.summ.wt, "Atherinopsis californiensis")) {set.summ.wt$`Atherinopsis californiensis` <- 0}

# Calculate total weight of all CPS species
set.summ.wt <- set.summ.wt %>%  
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  select(key.set, vessel.name, lat, long, everything()) %>% 
  mutate(AllCPS = rowSums(select(., -(key.set:long)))) %>%
  # mutate(AllCPS = rowSums(select(., -key.set, -totalCount, -lat, -long))) %>%
  # mutate(AllCPS = rowSums(.[, 3:ncol(.)])) %>%
  rename("Jacksmelt"  = "Atherinopsis californiensis",
         "PacHerring" = "Clupea pallasii",
         "Anchovy"    = "Engraulis mordax",
         "Sardine"    = "Sardinops sagax",
         "PacMack"    = "Scomber japonicus",
         "RndHerring" = "Etrumeus acuminatus",
         "JackMack"   = "Trachurus symmetricus") 

set.pie <- set.summ.wt %>% 
  select(key.set, vessel.name, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, RndHerring, Sardine, AllCPS) %>% 
  atm::project_df(to = 3310) %>% 
  mutate(
    label = paste("Set", key.set),
    popup = paste('<b>Set:', key.set, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'R. herring:', RndHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

# Load nearshore backscatter data -----------------------------------------
load(here("Output/cps_nasc_prop_ns.Rdata"))

# Summarize transects -----------------------------------------------------
# Summarize nasc data
nasc.summ.ns <- nasc.nearshore %>%
  group_by(vessel.name, transect.name, transect) %>%
  summarise(
    start     = min(datetime),
    end       = max(datetime),
    duration  = difftime(end, start, units = "hours"),
    n_int     = length(Interval),
    distance  = length(Interval)*nasc.interval/1852,
    lat       = lat[which.min(long)],
    long      = long[which.min(long)],
    mean_nasc = mean(cps.nasc)) %>%
  arrange(vessel.name, start)

save(nasc.summ.ns, file = here("Output/nasc_summ_tx_ns.Rdata"))
# 
# # Summarize nasc for plotting
nasc.plot.ns <- nasc.nearshore %>%
  select(filename, transect, transect.name, int, lat, long, cps.nasc) %>%
  group_by(filename, transect, transect.name, int) %>%
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc)) %>%
  # Create bins for defining point size in NASC plots%>%
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>%
  ungroup() %>%
  project_df(to = crs.proj)

nasc.paths.ns <- nasc.plot.ns %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(transect.name) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") %>%
  ungroup()

# Get nav data
if (nav.source.ns == "ERDDAP") {
  # Source code to get nav data from ERDDAP
  source(here("Code/get_nav_erddap.R"))
} else if (nav.source.ns == "SCS") {
  # Source code to get nav data from SCS
  # Not working with SCS post-2022 (new HTML-based SCS on FSVs); must update
  source(here("Code", scs.nav.script))
} else if (nav.source.ns == "GPX") {
  # Extract tracklines from GPX file
  nav.ns <- tmaptools::read_GPX(here("Data/Nav", seine.gpx.name))$tracks
  
  # Extract trackline points from GPX file
  nav.ns.points <- tmaptools::read_GPX(here("Data/Nav", seine.gpx.name))$track_points %>% 
    project_sf(crs = 4326) %>% 
    select(time, lat = Y, long = X)
}

# Create data frame of waypoints
## LM
lm.points <- nav.ns.points %>% 
  filter(lat > 37.18) %>% 
  arrange(time) %>% 
  project_df(to = crs.proj)

lm.points.sf <- lm.points %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

lm.path <- lm.points.sf %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs.proj)

## LBC
lbc.points <- nav.ns.points %>% 
  filter(lat < 37.18) %>% 
  arrange(time) %>% 
  project_df(to = crs.proj)

lbc.points.sf <- lbc.points %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

lbc.path <- lbc.points.sf %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs.proj)

# Load FSV nav
load(here("Data/Nav/nav_data.Rdata"))

# Process waypoints and transects
wpts.sf <- read_csv(here("Data/Nav", wpt.filename)) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) 

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

map.bounds.ns <- wpts.sf %>%
  filter(Region !="Vancouver Is.") %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

map.bounds.lm <- lm.path %>%
  st_transform(crs = 3310) %>%
  st_bbox()

map.bounds.lbc <- lbc.path %>%
  st_transform(crs = 3310) %>%
  st_bbox()

# Calculate pie radius based on latitude range
pie.radius.ns  <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)
pie.radius.lm  <- as.numeric(abs(map.bounds.lm$ymin - map.bounds.lm$ymax)*pie.scale)
pie.radius.lbc <- as.numeric(abs(map.bounds.lbc$ymin - map.bounds.lbc$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$r    <- pie.radius.ns*set.pie$bin
} else {
  set.pie$r    <- pie.radius.ns
}

# Filter for empty trawls
set.zero    <- filter(set.pie, AllCPS == 0)

# Replace zeros with minimally small value for scatterpie plotting
set.pos <- filter(set.pie, AllCPS > 0) %>% 
  replace(. == 0, 0.0000001) %>% 
  arrange(desc(X))

# Save output
save(set.pie, set.zero, set.pos, 
     file = here("Output/purse_set_pies.Rdata"))

# Load basemap
load(here("Data/Map/basemap.Rdata"))
# Load trawl pie data
load(here("Output/trawl_pie_plotSurvey.Rdata"))

# Get species present in the seine catches
pie.spp.lm  <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax","Trachurus symmetricus")
pie.spp.lbc <- c("Engraulis mordax","Sardinops sagax","Scomber japonicus","Trachurus symmetricus")

set.pies.lm <- base.map + 
  # Plot nav data
  geom_sf(data = lm.path, colour = "gray30") +
  geom_sf(data = filter(transects.sf, Type == "Nearshore")) + 
  # Plot purse seine pies
  geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM")), 
                  aes(X, Y, group = key.set, r = pie.radius.ns),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.lm],
                  color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.lm]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.lm])) +
  geom_point(data = filter(set.zero, vessel.name == "LM"), aes(X, Y)) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]),
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]))

ggsave(set.pies.lm, filename = here("Figs/fig_seine_proportion_set_wt_LisaMarie.png"),
       height = 10, width = 6)

set.pies.lbc <- base.map + 
  # Plot nav data
  geom_sf(data = lbc.path, colour = "gray") +
  geom_sf(data = filter(transects.sf, Type == "Nearshore")) + 
  # Plot purse seine pies
  geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LBC")), 
                  aes(X, Y, group = key.set, r = pie.radius.ns),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.lbc],
                  color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.lbc]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.lbc])) +
  geom_point(data = filter(set.zero, vessel.name == "LBC"), aes(X, Y)) +
  # ggtitle("Long Beach Carnage") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"])) 

ggsave(set.pies.lbc, filename = here("Figs/fig_seine_proportion_set_wt_LongBeachCarnage.png"),
       height = 10, width = 10)

# Get species present in the trawl catch
# pie.spp <- sort(unique(catch$scientificName))

haul.pies <- base.map + 
  # # Plot nav data
  geom_path(data = project_df(nav, to = crs.proj), aes(X, Y), colour = "gray30") +
  # Plot haul pies
  geom_scatterpie(data = haul.pos, 
                  aes(X, Y, group = haul, r = pie.radius.ns),
                  cols = pie.cols[names(pie.cols) %in% pie.spp],
                  color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp])) +
  geom_point(data = haul.zero, aes(X, Y)) +
  # ggtitle("Reuben Lasker") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]))

ggsave(haul.pies, filename = here("Figs/fig_seine_proportion_set_wt_FSV.png"),
       height = 10, width = 6)

set.haul.combo <- cowplot::plot_grid(set.pies.lm, set.pies.lbc, haul.pies, 
                                     nrow = 1, align = "hv", labels = c("a)","b)","c)"))

ggsave(set.haul.combo, filename = here("Figs/fig_seine_proportion_set_wt_LM-LBC-FSV.png"),
       height = 8, width = 12)

# Plot Lisa Marie data ----------------------------------------------------
# Select plot levels for LM backscatter data
nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, "LM"))

nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
nasc.labels.all <- nasc.labels[nasc.levels.all]
nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
nasc.colors.all <- nasc.colors[nasc.levels.all]

# Map backscatter
nasc.map.ns.lm <- base.map +
  # Plot transects data
  # geom_sf(data = filter(transects.sf, Type == "Nearshore"),
  #         linewidth = 0.5, colour = "gray70",
  #         alpha = 0.75, linetype = "dashed") +
  # Plot NASC data
  geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name),
            colour = "gray50", linewidth = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  # ggtitle("CPS Backscatter-Lisa Marie") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]))

ggsave(nasc.map.ns.lm, filename = here("Figs/fig_backscatter_cps_LisaMarie.png"),
       height = 10, width = 6)

nasc.set.wt.combo <- cowplot::plot_grid(nasc.map.ns.lm, set.pies.lm, nrow = 1, align = "hv",
                               labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.set.wt.combo, filename = here("Figs/fig_nasc_seine_proportion_set_wt_LisaMarie.png"),
       height = 7, width = 6.5)

# Plot Long Beach Carnage data ----------------------------------------------------
# Select plot levels for backscatter data
nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, "LBC"))

nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
nasc.labels.all <- nasc.labels[nasc.levels.all]
nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
nasc.colors.all <- nasc.colors[nasc.levels.all]

# Map backscatter
nasc.map.ns.lbc <- base.map +
  # Plot transects data
  geom_sf(data = filter(transects.sf, Type == "Nearshore"),
          linewidth = 0.5, colour = "gray70",
          alpha = 0.75, linetype = "dashed") +
  # Plot NASC data
  geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name),
            colour = "gray50", linewidth = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]))

ggsave(nasc.map.ns.lbc, filename = here("Figs/fig_backscatter_cps_LongBeachCarnage.png"),
       height = 6, width = 10)

nasc.set.wt.combo <- cowplot::plot_grid(nasc.map.ns.lbc, set.pies.lbc, nrow = 1,
                               labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.set.wt.combo, filename = here("Figs/fig_nasc_seine_proportion_set_wt_LongBeachCarnage.png"),
       height = 6, width = 10)

# Combine all backscatter figures -----------------------------------------
nasc.ns.combo <- cowplot::plot_grid(nasc.map.ns.lm, nasc.map.ns.lbc,
                           align = "v", nrow = 1, 
                           labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.ns.combo, filename = here("Figs/fig_backscatter_cps_AllNearshore.png"),
       height = 10, width = 10)

# # Get max TL for plotting L/W models
# L.max <- select(lm.lengths, scientificName, totalLength_mm) %>%
#   bind_rows(select(lbc.lengths, scientificName, totalLength_mm)) %>% 
#   group_by(scientificName) %>% 
#   summarise(max.TL = max(totalLength_mm, na.rm = TRUE))
# 
# # Data frame for storing results
# lw.df <- data.frame()
# 
# # Generate length/weight curves
# for (i in unique(L.max$scientificName)) {
#   # Create a length vector for each species
#   L <- seq(0, L.max$max.TL[L.max$scientificName == i])
#   
#   # Calculate weights from lengths
#   W <- estimate_weight(i, L, season = tolower(survey.season))
#   
#   # Combine results
#   lw.df <- bind_rows(lw.df, data.frame(scientificName = i, L, W))
# }
# 
# # Estimate missing weights from lengths -------------------------------------------------------
# # Add a and b to length data frame and calculate missing lengths/weights
# lm.lengths <- lm.lengths %>% 
#   mutate(
#     weightg = case_when(
#       is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
#       TRUE  ~ weightg),
#     totalLength_mm = case_when(
#       is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
#       TRUE ~ totalLength_mm),
#     K = round((weightg/totalLength_mm*10^3)*100))
# 
# lbc.lengths <- lbc.lengths %>% 
#   mutate(
#     weightg = case_when(
#       is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
#       TRUE  ~ weightg),
#     totalLength_mm = case_when(
#       is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
#       TRUE ~ totalLength_mm),
#     K = round((weightg/totalLength_mm*10^3)*100))
# 
# 
# # Plot specimen L/W for Lisa Marie ----------------------------- 
# lw.plot.lm <- ggplot() + 
#   # Plot seasonal length models for each species
#   geom_line(data = lw.df, aes(L, W), linetype = 'dashed') +
#   geom_point(data = lm.lengths, aes(forkLength_mm, weightg, label = label), 
#              colour = "blue") + 
#   geom_point(data = lm.lengths, aes(standardLength_mm, weightg, label = label), 
#              colour = "red") + 
#   geom_point(data = lm.lengths, aes(totalLength_mm, weightg, label = label), 
#              colour = "green") + 
#   facet_wrap(~scientificName, scales = "free") +
#   xlab("Length (mm)") +
#   ylab("Weight (g)") +
#   theme_bw()  +
#   theme(strip.background.x = element_blank(),
#         strip.text.x = element_text(face = "italic"))
# 
# ggsave(lw.plot.lm, filename = here("Figs/fig_LW_plots_LisaMarie.png"),
#        width = 10, height = 7)
# 
# # Plot speciment L/W for Long Beach Carnage ----------------------------- 
# lw.plot.lbc <- ggplot() + 
#   # Plot seasonal length models for each species
#   geom_line(data = lw.df, aes(L, W), linetype = 'dashed') +
#   geom_point(data = lbc.lengths, aes(forkLength_mm, weightg, label = label), 
#              colour = "blue") + 
#   geom_point(data = lbc.lengths, aes(standardLength_mm, weightg, label = label), 
#              colour = "red") + 
#   geom_point(data = lbc.lengths, aes(totalLength_mm, weightg, label = label), 
#              colour = "green") + 
#   facet_wrap(~scientificName, scales = "free") +
#   xlab("Length (mm)") +
#   ylab("Weight (g)") +
#   theme_bw() +
#   theme(strip.background.x = element_blank(),
#         strip.text.x = element_text(face = "italic"))
# 
# ggsave(lw.plot.lbc, filename = here("Figs/fig_LW_plots_LongBeachCarnage.png"),
#        width = 10, height = 7)
