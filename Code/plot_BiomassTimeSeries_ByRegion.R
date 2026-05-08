library(tidyverse)
library(here)

theme_set(theme_bw())

# Load time series data
load(here("Output/biomass_timeseries_final.Rdata"))

# Get years with only survey totals
biomass.all <-  biomass.ts.summ %>% 
  filter(stratum == "All", season == "Summer") %>% 
  arrange(survey, species)

# Get all other years and summarize by region
biomass.ts.all <- biomass.ts %>% 
  filter(season == "Summer") %>% 
  group_by(survey, year, species, stock, region) %>% 
  summarise(biomass = sum(biomass)) 

# Combine all surveys
biomass <- biomass.all %>% 
  bind_rows(biomass.ts.all) %>% 
  arrange(species, survey, region, year) %>% 
  select(species, stock, survey, region, year, biomass) %>% 
  mutate(group = paste(species, stock, sep = "-"))

# Summarize biomass by survey and species group
biomass.summ <- biomass %>% 
  group_by(year, species, stock, group) %>% 
  summarise(biomass.tot = sum(biomass)) %>% 
  arrange(species, stock, year) 

# Add totals to time series and compute pct of total
biomass <- left_join(biomass, biomass.summ) %>% 
  mutate(biomass.pct = biomass/biomass.tot*100)

# Plot absolute biomass time series
ggplot(biomass, aes(x = year, y = biomass, group = region, colour = region)) + 
  geom_line() + geom_point() +
  # geom_line(data = biomass.summ, aes(year, biomass.tot)) +
  # facet_grid(species~stock, scales="free_y")
  facet_wrap(~group, scales="free_y") +
  labs(title = "Absolute biomass per region",
       y = "Biomass (t)",
       x = "Year")

ggsave(here("Figs/fig_biomass_ts_regions_absolute.png"),
       height = 5, width = 9)

# Plot percent biomass time series
ggplot(biomass, aes(x = year, y = biomass.pct, group = region, colour = region)) + 
  geom_line() + geom_point() + 
  # facet_grid(species~stock, scales="free_y")
  facet_wrap(~group, scales="free_y") + 
  labs(title = "Pct. total biomass per region",
       y = "Biomass (%)",
       x = "Year")

ggsave(here("Figs/fig_biomass_ts_regions_pct_total.png"),
       height = 5, width = 9)

