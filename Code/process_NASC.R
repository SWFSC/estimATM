if (process.csv) {
  if (!process.csv.all) {
    # Load already processed CSV files 
    if (file.exists(here("Output/processed_csv.Rdata"))) {
      load(here("Output/processed_csv.Rdata"))
    }
    
    # Load already processed backscatter data
    load(here("Data/Backscatter/nasc_all.Rdata"))
  }
  
  # Process CSV files for each vessel
  for (i in nasc.vessels) {
    # List all CSV files
    nasc.files <- dir_ls(file.path(here("Data/Backscatter", i)),
                         regexp = nasc.pattern.cps[i],
                         recurse = nasc.recurse[i],
                         ignore.case = TRUE)
    
    if (!process.csv.all) {
      # List only new CSV files
      nasc.files <- nasc.files[!fs::path_file(nasc.files) %in% processed.csv]
      
      # Load already processed vessel NASC data
      if (file.exists(here("Data/Backscatter", i, paste0("nasc_vessel_", i, "_RAW.rds")))) {
        nasc.vessel <- readRDS(here("Data/Backscatter", i, paste0("nasc_vessel_", i, "_RAW.rds")))
      }
    } else {
      # Remove existing nasc.vessel
      if (exists("nasc.vessel")) rm(nasc.vessel)
    }
    
    # Create or load vessel nasc data
    if (length(nasc.files) > 0) {
      # Configure progress bar
      pb <- tkProgressBar("R Progress Bar", "CSV File Processing", 0, 100, 0)
      
      # Process all .CSV files
      for (ii in seq_along(nasc.files)) {
        # Extract vessel nasc
        nasc.vessel.temp <- extract_csv(nasc.files[ii]) %>% 
          mutate(vessel.name = i,
                 sounder = sounder.type[i],
                 transect = str_replace(transect, nasc.pattern.cps[i],""))
        
        # Combine results
        if (exists("nasc.vessel")) {
          nasc.vessel <- bind_rows(nasc.vessel, nasc.vessel.temp)
        } else {
          nasc.vessel <- nasc.vessel.temp
        }
        
        # Update the progress bar
        pb.prog <- round(ii/length(nasc.files)*100)
        info <- sprintf("%d%% done", pb.prog)
        setTkProgressBar(pb, pb.prog, sprintf("CSV processing - %s (%s)", i, info), info)
      }
      
      # Close progress bar
      close(pb)
      
      # Save/load nasc.vessel (for debugging)
      saveRDS(nasc.vessel, 
              file = here("Data/Backscatter", i, 
                          paste0("nasc_vessel_", i, "_RAW.rds")))
      
      processed.csv.vessel <- unique(fs::path_file(nasc.vessel$filename))
      
      # Get intervals with bad lat/long values
      bad.nasc <- filter(nasc.vessel, lat == 999, long == 999) %>% 
        arrange(filename, datetime)
      
      if (nrow(bad.nasc) > 0) {
        write_csv(bad.nasc, here("Data/Backscatter", i, 
                                 paste0("nasc_vessel_", i, "_Bad.csv")))  
      }
      
      # Remove bad intervals
      nasc.vessel <- nasc.vessel %>% 
        filter(lat != 999, long != 999)
      
      # Save original transect name before modifying
      # Convert date and time to POSIXct
      nasc.vessel <- nasc.vessel %>% 
        mutate(datetime      = ymd_hms(paste(date, time), tz = "UTC"),
               leg = cut(as.numeric(date(datetime)), leg.breaks, labels = FALSE),
               transect.orig = transect)
      
      if (i == "SD") {
        # Set Saildrone number as vessel name
        nasc.vessel <- nasc.vessel %>% 
          mutate(vessel.orig = as.factor(str_extract(filename, "SD\\d{4}")),
                 id = seq_along(Interval)) %>% 
          filter(fct_match(vessel.orig, paste("SD", sd.numbers, sep = ""))) %>% 
          filter(between(long, min.long, max.long)) %>% 
          filter(between(lat,  min.lat,  max.lat))
      }
      
      # Remove letters from transect names (e.g., 105A, 105B, etc.)
      if (strip.tx.chars[i]) {
        nasc.vessel$transect <- gsub(tx.char.pattern[i], "", nasc.vessel$transect)  
      }
      
      # Remove numbers from transect names (e.g., 105-1, 105-2, etc.)
      if (strip.tx.nums[i]) {
        nasc.vessel$transect <- gsub(tx.num.pattern[i], "", nasc.vessel$transect)
      }
      
      # Remove transit data
      if (rm.transit[i]) {
        # Filter based on transit pattern
        nasc.transit <- nasc.vessel %>% 
          # Remove based on transit pattern
          filter(str_detect(transect.orig, nasc.pattern.transit[i])) %>%
          # Remove inter-transects e.g., 102.5
          filter(!str_detect(transect.orig, "\\d\\.5"))
        
        if (nrow(nasc.transit) > 0) {
          saveRDS(nasc.transit, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_transit.rds")))
          
          # Filter transit
          nasc.vessel <- nasc.vessel %>% 
            # Remove based on transit pattern
            filter(str_detect(transect.orig, nasc.pattern.transit[i], negate = TRUE)) %>%
            # Remove inter-transects e.g., 102.5
            filter(str_detect(transect.orig, "\\d\\.5", negate = TRUE)) 
        }
      }
      
      # Remove offshore transects data
      # If proper offshore sampling was conducted, biomass will be estimated
      # in later steps. If offshore biomass is to be included in the final
      # estimates, combine.regions == TRUE and "Offshore" should be included in
      # estimate.regions
      if (rm.offshore[i]) {
        # Filter based on offshore pattern
        nasc.offshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.offshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.offshore) > 0) {
          saveRDS(nasc.offshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_offshore.rds")))
          
          # Filter offshore transects
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.offshore[i], negate = TRUE)) 
        }
      }
      
      # Remove nearshore transects data
      if (rm.inshore[i]) {
        
        # Filter based on offshore pattern
        nasc.inshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.inshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.inshore) > 0) {
          saveRDS(nasc.inshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_inshore.rds")))
          
          # Filter offshore transects
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.inshore[i], negate = TRUE)) 
        }
      }
      
      # Remove nearshore transects data
      # If proper nearshore sampling was conducted, biomass will be estimated
      # in later steps. If nearshore biomass is to be included in the final
      # estimates, combine.regions == TRUE and "Nearshore" should be included in
      # estimate.regions
      if (rm.nearshore[i]) {
        nasc.nearshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.nearshore[i]) |
                   str_detect(tolower(filename), "nearshore")) 
        
        if (nrow(nasc.nearshore) > 0) {
          # Save processed CSV data from each survey vessel
          saveRDS(nasc.nearshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_nearshore.rds")))
          
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.nearshore[i], negate = TRUE)) %>% 
            filter(str_detect(tolower(filename), "nearshore", negate = TRUE))
        }
      }
      
      # Identify acoustic transects to include in processing, based on length
      tx.include <- nasc.vessel %>% 
        group_by(transect) %>% 
        summarise(L = length(Interval)/10*0.539957) %>%
        arrange(L) %>% 
        filter(L > min.tx.length[i])
      
      tx.short <- nasc.vessel %>% 
        group_by(transect) %>% 
        summarise(L = length(Interval)/10*0.539957) %>%
        arrange(L) %>% 
        filter(L <= min.tx.length[i])
      
      # Remove backscatter data from short transects
      nasc.vessel <- nasc.vessel %>% 
        filter(transect %in% tx.include$transect)
      
      if (nrow(nasc.vessel) > 0) {
        # Create new interval for summarizing and plotting backscatter 
        # and add vessel abbreviation to transect name
        nasc.vessel <- nasc.vessel %>% 
          mutate(int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                                         nasc.summ.interval),
                           labels = FALSE, include.lowest = TRUE),
                 transect.name = paste(vessel.name, transect))
        
        # Filter NASC per vessel -------------------------------------------------
        # Manually remove individual transects
        if (!is.na(tx.rm[i])) {
          nasc.vessel <- filter(nasc.vessel, !transect.orig %in% unlist(tx.rm[i])) 
        }
        
        # Set stop integration depth
        if (source.cps.nasc[i]) {
          # Use externally supplied cps.nasc with variable integration depth (from CTD.app)
          # Read file and create unique key for joining with nasc.vessel
          if (survey.name %in% c("1507SH","1606RL")) {
            cps.nasc.temp <- read.csv(data.cps.nasc[i]) %>% 
              mutate(key = paste(transect, interval))
            # mutate(key = paste(interval, round(dist_m, 6))) 
            
            if (survey.name == c("1507SH")) {
              # Join nasc.vessel and cps.nasc on transect number and interval
              nasc.vessel <- nasc.vessel %>% 
                mutate(time.align  = align.time(datetime, 1),
                       transect.num = as.numeric(substr(transect.name, 4, 5)),
                       key = paste(transect.num, Interval)) %>% 
                filter(!str_detect(tolower(filename), "krill")) %>% 
                left_join(select(cps.nasc.temp, key, cps.nasc)) %>% 
                mutate(cps.nasc.diff = NASC.50 - cps.nasc) %>% 
                mutate(cps.nasc = case_when(
                  is.na(cps.nasc) ~ NASC.50,
                  TRUE ~ cps.nasc))  
            } else if (survey.name == c("1606RL")) {
              # Join nasc.vessel and cps.nasc on transect number and interval
              nasc.vessel <- nasc.vessel %>% 
                mutate(time.align  = align.time(datetime, 1),
                       transect.num = as.numeric(substr(transect.name, 4, 6)),
                       key = paste(transect.num, Interval)) %>% 
                filter(!str_detect(tolower(filename), "krill")) %>% 
                left_join(select(cps.nasc.temp, key, cps.nasc)) %>% 
                mutate(cps.nasc.diff = NASC.50 - cps.nasc) %>% 
                mutate(cps.nasc = case_when(
                  is.na(cps.nasc) ~ NASC.50,
                  TRUE ~ cps.nasc))
            }
          } else {
            cps.nasc.temp <- read.csv(data.cps.nasc[i]) %>% 
              mutate(key = paste(lat, long, dist_m),
                     datetime      = ymd_hms(paste(date, time), tz = "UTC"),
                     time.align  = align.time(datetime, 1)) %>% 
              # Remove data from krill files (1807RL)
              filter(!str_detect(tolower(filename), "krill")) 
            
            # Join nasc.vessel and cps.nasc on datetime
            nasc.vessel <- nasc.vessel %>% 
              mutate(time.align  = align.time(datetime, 1)) %>% 
              # select(-cps.nasc) %>% 
              filter(!str_detect(tolower(filename), "krill")) %>% 
              left_join(select(cps.nasc.temp, time.align, cps.nasc))  
          }
        } else {
          # Use cps.nasc extracted using extract_cps_nasc.R
          if ("cps.nasc" %in% colnames(nasc.vessel)) {
            nasc.vessel$cps.nasc.source <- "cps.nasc"
          } else {
            # If cps.NASC not extracted, use fixed depth (nasc.depth.cps) defined in settings
            # nasc.vessel$cps.nasc <- purrr::pluck(nasc.vessel, nasc.depth.cps)
            nasc.vessel <- nasc.vessel %>% 
              mutate(cps.nasc = purrr::pluck(., nasc.depth.cps),
                     cps.nasc.source = nasc.depth.cps)
          }
          
          # If files are not passed through extract_CPS_NASC.R, then cps.nasc = NA
          nasc.vessel <- nasc.vessel %>% 
            # Create variable containing the source of cps.nasc data, either 
            # cps.nasc (from extract_CPS_NASC.R) or the value of nasc.depth.cps
            mutate(cps.nasc.source = case_when(
              is.na(cps.nasc) ~ nasc.depth.cps,
              TRUE ~ cps.nasc.source)) %>% 
            # Replace missing cps.nasc values with backscatter down to nasc.depth.cps
            mutate(cps.nasc = case_when(
              is.na(cps.nasc) ~ purrr::pluck(., nasc.depth.cps),
              TRUE ~ cps.nasc))
          
          # Remove surface noise, often from Saildrone backscatter
          if (rm.surface[i]) {
            nasc.vessel <- mutate(nasc.vessel, cps.nasc = cps.nasc - NASC.5)
          }
        }
        
        # Save processed CSV data from each survey vessel
        saveRDS(nasc.vessel, file = here("Data/Backscatter", i, 
                                         paste0("nasc_vessel_", i, ".rds")))
        
        nasc.vessel.comp <- project_df(nasc.vessel, to = crs.proj)
        
        # Map a comparison of cps.nasc to nasc.depth.cps and
        # indicating the source of the backscatter data
        compare.cps.nasc.map <- base.map + 
          geom_point(data = nasc.vessel.comp, aes(X, Y, size = NASC.250)) +
          geom_point(data = nasc.vessel.comp, 
                     aes(X, Y, size = cps.nasc, colour = cps.nasc.source),
                     shape = 21, fill = NA) +
          coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
                   xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
                   ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
        
        # Save figure
        ggsave(compare.cps.nasc.map, 
               filename = here("Data/Backscatter", i, paste0("nasc_vessel_map_", i, ".png")), 
               height = map.height, width = map.width)
        
        # Scatter plot comparing cps.nasc to nasc.depth.cps and
        # indicating the source of the backscatter data
        compare.cps.nasc.scatter <- ggplot() + 
          geom_point(data = nasc.vessel.comp, 
                     aes(cps.nasc, NASC.250, colour = cps.nasc.source)) + 
          geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          facet_wrap(~cps.nasc.source) +
          coord_equal() + 
          theme_bw()
        
        # Save figure
        ggsave(compare.cps.nasc.scatter, 
               filename = here("Data/Backscatter", i, paste0("nasc_vessel_scatter_", i, ".png")))
        
        # Combine results from different vessels
        if (exists("nasc")) {
          nasc <- bind_rows(nasc, nasc.vessel)
        } else {
          nasc <- nasc.vessel
        }
      }
      
      # Combine processed.csv.vessel
      if (exists("processed.csv")) {
        processed.csv <- unique(sort(c(processed.csv, processed.csv.vessel)))
      } else {
        processed.csv <- processed.csv.vessel
      }
    }
  }  
  
  # Save processed NASC file import
  save(nasc, file = here("Data/Backscatter/nasc_all.Rdata"))
  
  # Save processed CSV file names
  save(processed.csv, file =  here("Output/processed_csv.Rdata"))
  
} else {
  load(here("Data/Backscatter/nasc_all.Rdata"))
}
