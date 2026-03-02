if (get.db) {
  # Configure ODBC connection to AST database ------------------------------------
  ast.con  <- dbConnect(odbc(), 
                        Driver = "SQL Server", 
                        Server = "161.55.235.187", 
                        Database = "AST", 
                        Trusted_Connection = "True")
  
  # Import past estimates --------------------------------------------------------
  biomass.ts      <- tbl(ast.con, "tbl_ATM_BIOMASS") %>% collect()
  survey.info     <- tbl(ast.con, "tbl_SURVEY_LOG") %>% collect() #%>% mutate_if(is.character, str_trim)
  
  # Close database channel
  dbDisconnect(ast.con)  
  
  # Save data
  save(biomass.ts, survey.info,
       file = here("Output/biomass_database.Rdata"))
} else {
  # Load data
  load(here("Output/biomass_database.Rdata"))
}

# Load present year estimates
load(here("Output/biomass_timeseries_export.Rdata"))

# Add missing columns
be.db.export <- be.db.export %>% 
  mutate(us_waters = TRUE,
         country   = "USA") %>% 
  filter(region %in% estimate.regions)

# Combine with current survey results
biomass.ts <- biomass.ts %>%
  # Remove unwanted strata (e.g., offshore)
  filter(include_ts == TRUE) %>% 
  # Remove any estimates from the current survey
  filter(!survey %in% unique(be.db.export$survey)) %>%
  # Add results from current survey
  bind_rows(be.db.export)

# # Check results
# biomass.ts %>%
#   # filter(survey == "2107RL") %>%
#   filter(species == "Sardinops sagax") %>%
#   View()
# 
# biomass.ts %>% group_by(species, country) %>% tally() %>% View()

# Identify surveys with individual strata in the time series
multi.estimates <- biomass.ts %>% 
  filter_out(stratum == "All") %>% 
  pull(survey) %>% unique()

# Select surveys where individual strata are not available (i.e., pre-2015, or estimATM)
biomass.ts.single <- biomass.ts %>% 
  filter_out(survey %in% multi.estimates) %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>% 
  filter(season == "Summer")

# Select summer surveys with individual strata estimates (2015-present)
## This contains individual strata only (no "All")
biomass.ts <- biomass.ts %>% 
  filter(survey %in% multi.estimates) %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>% 
  filter(season == "Summer") %>% 
  filter_out(stratum == "All")

# Summarise results for all CPS across regions
biomass.ts.summ <- biomass.ts %>% 
  # select(survey, species, stock, biomass, biomass_sd, biomass_ci_lower, biomass_ci_upper) %>% 
  group_by(survey, date_start, year, species, stock) %>%
  summarise(area = sum(area),
            nStrata = n(),
            nClusters = sum(nClusters),
            nIndiv = sum(nIndiv),
            biomass = sum(biomass),
            biomass_sd = sqrt(sum(biomass_sd^2)),
            biomass_ci_lower = sum(biomass_ci_lower),
            biomass_ci_upper = sum(biomass_ci_upper),
            biomass_cv = biomass_sd/biomass*100) %>% 
  mutate(group = paste(species, stock, sep = "-")) %>% 
  bind_rows(biomass.ts.single) %>% 
  arrange(species, stock, survey)

# # Plot all species/stocks
# ggplot(biomass.ts.summ,
#        aes(date_start, biomass, group = group, colour = group)) +
#   geom_path() + geom_point()

# Summarise results for all sardine stocks in US waters
biomass.ts.summ.sar.us <- biomass.ts %>% 
  filter(species == "Sardinops sagax", us_waters == TRUE) %>%
  group_by(survey, date_start, year, species, stock) %>%
  summarise(area = sum(area),
            nStrata = n(),
            nClusters = sum(nClusters),
            nIndiv = sum(nIndiv),
            biomass = sum(biomass),
            biomass_sd = sqrt(sum(biomass_sd^2)),
            biomass_ci_lower = sum(biomass_ci_lower),
            biomass_ci_upper = sum(biomass_ci_upper),
            biomass_cv = biomass_sd/biomass*100) %>% 
  mutate(group = paste(species, stock, sep = "-")) %>% 
  bind_rows(biomass.ts.single) %>% 
  arrange(species, stock, survey)

# Summarise all sardine results in US waters, by stock and combined
biomass.ts.summ.sar <- biomass.ts %>% 
  filter(species == "Sardinops sagax", us_waters == TRUE) %>% 
  group_by(survey, date_start, year, species) %>%
  summarise(area = sum(area),
            nStrata = n(),
            nClusters = sum(nClusters),
            nIndiv = sum(nIndiv),
            biomass = sum(biomass),
            biomass_sd = sqrt(sum(biomass_sd^2)),
            biomass_ci_lower = sum(biomass_ci_lower),
            biomass_ci_upper = sum(biomass_ci_upper),
            biomass_cv = biomass_sd/biomass*100) %>% 
  # Resume here; need to remove duplicate values in time series
  bind_rows(filter(biomass.ts.single, species == "Sardinops sagax")) %>% 
  mutate(stock = "Combined",
         group = paste(species, stock, sep = "-")) %>% 
  bind_rows(filter(biomass.ts.summ.sar.us, species == "Sardinops sagax", year >= 2015)) %>% 
  arrange(species, stock, survey) 

## Plot all species/stocks
# ggplot(biomass.ts.summ.sar,
#        aes(date_start, biomass, group = group, colour = group)) + 
#   geom_path() + geom_point()

# Summarize community biomass by year
biomass.comm.summ <- biomass.ts %>% 
  # filter(group != "Sardinops sagax-Southern") %>% 
  group_by(year, survey) %>% 
  summarise(biomass.total = sum(biomass))

# Summarize species biomass per year
biomass.spp.summ <- biomass.ts %>% 
  group_by(year, survey, species, stock) %>% 
  summarise(biomass = sum(biomass)) %>% 
  left_join(biomass.comm.summ) %>% 
  mutate(biomass.pct = biomass/biomass.total*100)

save(biomass.ts, biomass.comm.summ, biomass.spp.summ, biomass.ts.summ.sar,
     biomass.ts.summ, biomass.ts.summ.sar,
     file = here("Output/biomass_timeseries_final.Rdata"))

# Create plot ------------------------------------------------------------------
# Create line plot - single
biomass.ts.line <- ggplot(biomass.ts.summ, 
                          aes(x = date_start, y = biomass, 
                              colour = group, shape = group,
                              group = group)) +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  geom_path() +
  geom_point(size = 2, fill = "white") +
  scale_colour_manual(name = 'Species',
                    labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                               "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color, "#93F09F",
                               rnd.herring.color, sardine.color, "#FF7256",
                               pac.mack.color, jack.mack.color)) +
  scale_shape_manual(name = 'Species',
                      labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                 "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                 "Scomber japonicus", "Trachurus symmetricus"),
                      values = c(21, 22, 23,
                                 21, 22, 23,
                                 21, 22)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.line, 
       filename = here("Figs/fig_biomass_ts_line.png"),
       width = 8, height = 4)

# Combine with data from prior to 2015 (stocks not separated)
# Add jitter to dates for different stocks
# 
# biomass.ts.stock <- biomass.ts %>% 
#   filter(species == "Sardinops sagax", year >= 2015) %>% 
#   left_join(biomass.ts.sar.rm) %>% 
#   # Remove non-US biomass from NSP and SSP
#   replace_na(list(biomass.rm = 0, cil.rm = 0, ciu.rm = 0)) %>% 
#   mutate(biomass = biomass - biomass.rm,
#          biomass_ci_lower = biomass_ci_lower - cil.rm,
#          biomass_ci_upper = biomass_ci_upper - ciu.rm) 

biomass.ts.summ.sar <- biomass.ts.summ.sar %>% 
  # bind_rows(filter(biomass.ts.stock, species == "Sardinops sagax", year >= 2015)) %>% 
  # filter(year >= 2015) %>% 
  mutate(date_start = case_when(
    str_detect(group, "Northern") ~ date_start + days(30),
    str_detect(group, "Southern") ~ date_start - days(30),
               TRUE ~ date_start))

# Create line plot - single (All sardine in U.S. waters)
biomass.ts.line.sar <- ggplot(filter(biomass.ts.summ.sar, biomass != 0), 
                          aes(x = date_start, y = biomass, 
                              colour = group, shape = group,
                              group = group)) +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  geom_path() +
  geom_point(size = 2, fill = "white") +
  scale_colour_manual(name = 'Species',
                      labels = c("Sardinops sagax (Combined)", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)"),
                      values = c("purple", sardine.color, "blue")) +
  scale_shape_manual(name = 'Species',
                     labels = c("Sardinops sagax (Combined)", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)"),
                     values = c(21, 22, 23)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  theme_bw() +
  theme(
    legend.text = element_text(face = "italic"),
    # Position the legend inside the plot area
    legend.position = c(1, 1),
    # Anchor the legend box's top-right corner to the plot's top-right corner
    legend.justification = c("right", "top"),
    # Optional: adjust the background of the legend box
    legend.background = element_rect(fill = "transparent", color = NA),
    # Optional: add slight margins so the legend is not exactly on the edge
    legend.margin = margin(t = 5, r = 5, unit = "pt")
  )

# Save figure
ggsave(biomass.ts.line.sar, 
       filename = here("Figs/fig_biomass_ts_line_all_US_sardine.png"),
       width = 8, height = 4)

# Create line plot - faceted
biomass.ts.line.facet <- ggplot(biomass.ts.summ,
                                aes(x = date_start, y = biomass, 
                                    shape = group, colour = group, group = group)) +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  geom_path() +
  geom_point(fill = "white") +
  facet_wrap(~group, scales = "free_y") + 
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  scale_colour_manual(name = 'Species',
                      labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                 "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                 "Scomber japonicus", "Trachurus symmetricus"),
                      values = c(pac.herring.color, anchovy.color, "#93F09F",
                                 rnd.herring.color, sardine.color, "#FF7256",
                                 pac.mack.color, jack.mack.color)) +
  scale_shape_manual(name = 'Species',
                     labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                "Scomber japonicus", "Trachurus symmetricus"),
                     values = c(21, 22, 23,
                                21, 22, 23,
                                21, 22)) +
  theme_bw() +
  theme(axis.text.x        = element_text(angle = 45, vjust = 0.5),
        strip.background.x = element_blank(),
        strip.text.x       = element_text(face = "italic"),
        legend.position    = "none")

# Save figure
ggsave(biomass.ts.line.facet, 
       filename = here("Figs/fig_biomass_ts_line_facet.png"),
       width = 12, height = 7)

# Create stacked bar plot
biomass.ts.bar <- ggplot(biomass.ts.summ, 
                         aes(x = date_start, y = biomass, fill = group)) + 
  geom_bar(colour = "black", position = "stack", stat = "identity") +
  scale_fill_manual(name = 'Species',
                      labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                 "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                 "Scomber japonicus", "Trachurus symmetricus"),
                      values = c(pac.herring.color, anchovy.color, "#93F09F",
                                 rnd.herring.color, sardine.color, "#FF7256",
                                 pac.mack.color, jack.mack.color)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  ylab(expression(Biomass~(italic(t)))) +
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 0),
        legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.bar, 
       filename = here("Figs/fig_biomass_ts_bar.png"),
       width = 8, height = 4)

# Combine plots into one using {patchwork}
biomass.ts.combo <- biomass.ts.line / biomass.ts.bar +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# Save figure
ggsave(biomass.ts.combo, 
       filename = here("Figs/fig_biomass_ts_combo.png"),
       width = 8, height = 8)
