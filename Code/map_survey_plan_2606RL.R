# # Map Long Beach Carnage and Lisa Marie transects for 
# Summer 2026 Integrated West Coast Pelagics Survey (2606RL)

# Define nearshore transects
lbc.transects <- c(1:57, 284:347)
lbc.transects.jz <- c(1:27) # Subset for Juan
lm.transects  <- c(58:152)

# Map LBC transects
# Set padding around data
map.bounds <- transects %>%
  filter(Type == "Nearshore", Transect %in% lbc.transects) %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries,
                        landmarks, bathy, map.bounds, crs = crs.proj)

if (nrow(filter(transects, Type == "Nearshore", Transect %in% lbc.transects))>0) {
  lbc.map <- base.map +
    # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
    geom_sf(data = filter(transects, Type != "Nearshore"),
            aes(colour = Type, linetype = Type),
            show.legend = "line") +
    geom_sf(data = filter(transects, Type == "Nearshore", Transect %in% lbc.transects),
            aes(colour = Type, linetype = Type),
            show.legend = "line") +
    # Plot acoustic transect labels N of Cape Flattery
    geom_shadowtext(data = tx.labels,
                    aes(X, Y, label = transect.name,
                        angle = brg, colour = Type),
                    size = 2, fontface = 'bold.italic',
                    bg.colour = "white") +
    scale_colour_manual("Type",
                        values = wpt.colors) +
    scale_linetype_manual("Type",
                          values = wpt.linetypes) +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

  # Save the base map
  ggsave(lbc.map, file = here("Figs", paste0("fig_survey_plan_LBC.png")),
         height = map.height.region["south"], width = map.width["south"])
}

# Map subset of LBC transects for Juan's proposal
# Set padding around data
map.bounds <- transects %>%
  filter(Type == "Nearshore", Transect %in% lbc.transects.jz) %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries,
                        landmarks, bathy, map.bounds, crs = crs.proj)

if (nrow(filter(transects, Type == "Nearshore", Transect %in% lbc.transects.jz))>0) {
  lbc.map.jz <- base.map +
    # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
    # geom_sf(data = filter(transects, Type != "Nearshore"),
    #         aes(colour = Type, linetype = Type),
    #         show.legend = "line") +
    geom_sf(data = filter(transects, Type == "Nearshore", Transect %in% lbc.transects.jz),
            aes(colour = Type, linetype = Type),
            show.legend = "line") +
    # # Plot acoustic transect labels N of Cape Flattery
    # geom_shadowtext(data = filter(tx.labels, Type == "Nearshore"),
    #                 aes(X, Y, label = transect.name,
    #                     angle = brg, colour = Type),
    #                 size = 2, fontface = 'bold.italic',
    #                 bg.colour = "white") +
    scale_colour_manual("Type",
                        values = wpt.colors) +
    scale_linetype_manual("Type",
                          values = wpt.linetypes) +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
  
  # Save the base map
  ggsave(lbc.map.jz, file = here("Figs", paste0("fig_survey_plan_LBC_JZ.png")),
         height = map.height.region["south"]*0.5, width = map.width["south"]*0.5)
}

# Map Lisa Marie transects for Summer 2025 CCE Survey (2506SH)
# Set padding around data
map.bounds <- transects %>%
  filter(Type == "Compulsory", Transect %in% c(30:87)) %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- 5
# map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries,
                        landmarks, bathy, map.bounds, crs = crs.proj)

if (nrow(filter(transects, Type == "Nearshore", Transect %in% lm.transects))>0) {
  lm.map <- base.map +
    geom_sf(data = filter(transects, Type != "Nearshore"),
            aes(colour = Type, linetype = Type),
            show.legend = "line") +
    geom_sf(data = filter(transects, Type == "Nearshore", Transect %in% lm.transects),
            aes(colour = Type, linetype = Type),
            show.legend = "line") +
    # Plot acoustic transect labels N of Cape Flattery
    geom_shadowtext(data = tx.labels,
                    aes(X, Y, label = transect.name,
                        angle = brg, colour = Type),
                    size = 2, fontface = 'bold.italic',
                    bg.colour = "white") +
    scale_colour_manual("Type",
                        values = wpt.colors) +
    scale_linetype_manual("Type",
                          values = wpt.linetypes) +
    theme(legend.position.inside = c(1,0.5),
          legend.justification = c(0,0.5)) +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

  # Save the base map
  ggsave(lm.map, file = here("Figs", paste0("fig_survey_plan_LM.png")),
         height = map.height.region, width = map.width)
}
