if (get.db.ts) {
  # Configure ODBC connection to AST database ------------------------------------
  ast.con  <- dbConnect(odbc(), 
                        Driver = "SQL Server", 
                        Server = trawl.db.server, 
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
  mutate(us_waters  = TRUE,
         country = "USA")

# Combine with current survey results
biomass.ts <- biomass.ts %>% 
  filter(!survey %in% unique(be.db.export$survey)) %>%
  bind_rows(filter(be.db.export, region %in% estimate.regions))

# # Check results
# biomass.ts %>%
#   # filter(survey == "2107RL") %>%
#   filter(species == "Sardinops sagax") %>%
#   View()
# 
# biomass.ts %>% group_by(species, country) %>% tally()

# All sardine biomass estimates, in US waters only
biomass.ts.sar <- biomass.ts %>% 
  filter(species == "Sardinops sagax") %>% 
  filter(us_waters == TRUE)

# Compute portion of sardine to remove from total estimate
biomass.ts.sar.rm <- biomass.ts %>% 
  filter(species == "Sardinops sagax") %>% 
  filter(us_waters != TRUE) %>% 
  group_by(survey, stock) %>% 
  summarise(biomass.rm = sum(biomass),
            cil.rm = sum(biomass_ci_lower),
            ciu.rm = sum(biomass_ci_upper))

# biomass.ts.sar <- biomass.ts.sar %>% 
#   left_join(biomass.ts.sar.rm)

# Summarise results across regions
biomass.ts.var <- biomass.ts %>% 
  filter(stratum == "All") %>% 
  select(survey, species, stock, biomass_sd) %>% 
  group_by(survey, species, stock) %>%
  summarise(biomass_sd = sqrt(sum(biomass_sd^2)))

# All sardine variances only
biomass.ts.var.sar <- biomass.ts.sar %>% 
  filter(stratum == "All") %>%
  select(survey, species, biomass_sd) %>%
  group_by(survey, species) %>%
  summarise(biomass_sd = sqrt(sum(biomass_sd^2))) 

biomass.ts <- biomass.ts %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>%
  filter(stratum == "All", include_ts == TRUE) %>%
  filter(season == "Summer", stratum == "All", include_ts == TRUE) %>%
  select(-season, -region, -stratum, -biomass_sd, -biomass_cv, -date_start, 
         -group, -year, -include_ts, -us_waters, -country) %>%
  group_by(survey, species, stock) %>% 
  summarise_all(list(sum)) %>% 
  left_join(biomass.ts.var) %>% 
  mutate(biomass_cv = biomass_sd/biomass*100)

# All sardine results combined
biomass.ts.sar <- biomass.ts.sar %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>%
  filter(season == "Summer", stratum == "All", include_ts == TRUE) %>%
  select(-stock, -season, -region, -stratum, -biomass_sd, -biomass_cv, -date_start, 
         -group, -year, -include_ts, -us_waters, -country) %>%
  group_by(survey, species) %>% 
  summarise_all(list(sum)) %>% 
  left_join(biomass.ts.var.sar) %>% 
  mutate(biomass_cv = biomass_sd/biomass*100)

# Format data ------------------------------------------------------------------
biomass.ts <- biomass.ts %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>% 
  # filter(!group %in% c("Sardinops sagax-Southern","Engraulis mordax-Northern")) %>% 
  filter(!biomass == 0)

biomass.ts.sar <- biomass.ts.sar %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, "All", sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>% 
  # Combine with df to remove biomass
  left_join(select(biomass.ts.sar.rm, -stock)) %>%
  replace_na(list(biomass.rm = 0, cil.rm = 0, ciu.rm = 0)) %>% 
  mutate(biomass = biomass - biomass.rm,
         biomass_ci_lower = biomass_ci_lower - cil.rm,
         biomass_ci_upper = biomass_ci_upper - ciu.rm) %>% 
  # filter(!group %in% c("Sardinops sagax-Southern","Engraulis mordax-Northern")) %>% 
  filter(!biomass == 0) 

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

save(biomass.ts, biomass.comm.summ, biomass.spp.summ, biomass.ts.sar,
     file = here("Output/biomass_timeseries_final.Rdata"))

# Create plot ------------------------------------------------------------------
# Create line plot - single
biomass.ts.line <- ggplot(filter(biomass.ts, biomass != 0), 
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

biomass.ts.stock <- biomass.ts %>% 
  filter(species == "Sardinops sagax", year >= 2015) %>% 
  left_join(biomass.ts.sar.rm) %>% 
  # Remove non-US biomass from NSP and SSP
  replace_na(list(biomass.rm = 0, cil.rm = 0, ciu.rm = 0)) %>% 
  mutate(biomass = biomass - biomass.rm,
         biomass_ci_lower = biomass_ci_lower - cil.rm,
         biomass_ci_upper = biomass_ci_upper - ciu.rm) 

biomass.ts.sar <- biomass.ts.sar %>% 
  bind_rows(filter(biomass.ts.stock, species == "Sardinops sagax", year >= 2015)) %>% 
  # filter(year >= 2015) %>% 
  mutate(date_start = case_when(
    str_detect(group, "Northern") ~ date_start + days(30),
    str_detect(group, "Southern") ~ date_start - days(30),
               TRUE ~ date_start))

# Create line plot - single (All sardine in U.S. waters)
biomass.ts.line.sar <- ggplot(filter(biomass.ts.sar, biomass != 0), 
                          aes(x = date_start, y = biomass, 
                              colour = group, shape = group,
                              group = group)) +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  geom_path() +
  geom_point(size = 2, fill = "white") +
  scale_colour_manual(name = 'Species',
                      labels = c("Sardinops sagax (All)", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)"),
                      values = c("purple", sardine.color, "blue")) +
  scale_shape_manual(name = 'Species',
                     labels = c("Sardinops sagax (All)", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)"),
                     values = c(21, 22, 23)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.line.sar, 
       filename = here("Figs/fig_biomass_ts_line_all_US_sardine.png"),
       width = 8, height = 4)

# Create line plot - faceted
biomass.ts.line.facet <- ggplot(biomass.ts,
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
biomass.ts.bar <- ggplot(biomass.ts, 
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
biomass.ts.combo <- biomass.ts.line/biomass.ts.bar +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# Save figure
ggsave(biomass.ts.combo, 
       filename = here("Figs/fig_biomass_ts_combo.png"),
       width = 8, height = 8)
