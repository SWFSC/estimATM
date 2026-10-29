# Code to extrapolate core area biomass into the nearshore region
## Generate multiple bootstrap biomass estimates
if (do.bootstrap) {
  # Create data frame for biomass estimates
  bootstrap.estimates.nse <- data.frame()
  # Create data frame for abundance estimates by length
  abundance.estimates.nse <- data.frame()
  # Create data frame for stratum summaries
  survey.summary.nse <- data.frame()
  # Create data frame for catch summaries
  catch.summary.nse <- data.frame()
  # Create data frame for stratum summaries
  stratum.summary.nse <- data.frame()
  # Configure progress bar
  pb1 <- tkProgressBar("R Progress Bar", 
                       "Multiple Bootstrap Estimation (Nearshore) - Species", 0, 100, 0)
  spp.counter <- 1
  
  for (i in unique(strata.nse$scientificName)) {
    # Get vector of lengths from clf.df column names
    L.cols  <- grep("L\\d", names(cluster.final[[i]]))
    L.vec   <- sort(as.numeric(str_extract(names(cluster.final[[i]][L.cols]),"\\d{1,2}")))
    
    # Create data frame with stratum and area (m^2)
    stratum.info.nse <- strata.nse %>% 
      filter(scientificName == i) %>% 
      select(stratum, area) %>%
      mutate(area = as.numeric(area)) %>% 
      st_set_geometry(NULL)
    
    # Subset strata for species i
    strata.temp <- filter(strata.final, scientificName == i) %>% 
      select(transect, stratum) %>% 
      left_join(filter(strata.summ.nse, scientificName == i)) %>% 
      filter(!is.na(area))
    
    # Add stratum numbers to nasc and remove transects outside of defined strata
    nasc.temp <- nasc %>%
      left_join(strata.temp) %>% 
      filter(!is.na(stratum))
    
    # Get nearshore nasc data
    nasc.nse <- estimate_inshore(nasc.temp, bathy_5m_df) 
    
    # Summarize nasc.temp to get strata to merge with pos.clusters below
    nasc.temp.summ <- nasc.nse %>% 
      group_by(stratum, cluster) %>% 
      summarise(n = n_distinct(cluster))
    
    # Summarize length data to get number of individuals
    lf.summ.cluster <- lf.final %>% 
      filter(scientificName == i) %>% 
      group_by(cluster) %>% 
      summarise(counts = sum(counts))
    
    # Summarize stratum clusters for all CPS
    stratum.cluster.cps <- nasc.nse %>% 
      group_by(cluster, stratum) %>% 
      summarise(nIntervals = n()) %>% 
      left_join(lf.summ.cluster)
    
    # Summarize positive clusters per species
    pos.cluster.spp <- pos.clusters %>%
      filter(cluster %in% nasc.temp$cluster & scientificName == i) %>% 
      inner_join(select(nasc.temp.summ,-n)) %>% 
      as.data.frame()
    
    # Summarize positive clusters per strata
    stratum.cluster.spp <- pos.cluster.spp %>% 
      group_by(scientificName, stratum) %>% 
      summarise(nClusters = n_distinct(cluster))
    
    # Summarize stratum statistics
    survey.summ.temp <- nasc.nse %>% 
      group_by(stratum, stock) %>% 
      summarise(
        nTransects = n_distinct(transect),
        Distance   = length(Interval)*100/1852) %>% 
      mutate(Species = i) %>% 
      left_join(stratum.cluster.spp) %>% 
      rename(Stratum = stratum)
    
    # Summarize catch statistics by stratum
    catch.summ.temp <- n.summ.haul %>% 
      left_join(select(stratum.cluster.cps, cluster, stratum)) %>% 
      filter(scientificName == i, !is.na(stratum)) %>%
      group_by(scientificName, stratum) %>% 
      summarise(nIndiv = sum(num)) %>% 
      as.data.frame()
    
    # Configure progress bar
    pb2 <- tkProgressBar("R Progress Bar", "Multiple Bootstrap Estimation - Stratum", 0, 100, 0)
    # Initialize species counter
    stratum.counter <- 1
    # Estimate biomass for each stratum
    for (j in unique(nasc.nse$stratum)) {
      # Extract stratum area
      stratum.area <- as.numeric(strata.nse$area[strata.nse$scientificName == i & 
                                                   strata.nse$stratum == j])
      # Calculate biomass using bootstrap function ----
      set.seed(1) # Set seed for repeatable results
      boot.df <- estimate_bootstrap(nasc.nse, cluster.final[[i]], j, 
                                    stratum.area = stratum.area, 
                                    species = i, do.lf = do.lf, 
                                    boot.number = boot.num)$data.frame
      # Extract biomass estimates; remove first (point) estimate
      boot.temp <- data.frame(Species = i, Stratum = j, Area = stratum.area,
                              Sample = seq(1,boot.num), boot.df[2:nrow(boot.df), ])
      # Combine results
      bootstrap.estimates.nse <- bind_rows(bootstrap.estimates.nse, boot.temp)
      
      # Calculate abundance by length class using bootstrap function ----
      abund.vec <- estimate_bootstrap(nasc.nse, cluster.final[[i]], j, 
                                      stratum.area = stratum.area, 
                                      species = i, do.lf = do.lf, 
                                      boot.number = 0)$abundance.vector
      # Extract abundance estimates
      abundance.temp <- data.frame(Species = i, Stratum = j,
                                   SL = L.vec, freq = abund.vec)
      # Combine results
      abundance.estimates.nse <- bind_rows(abundance.estimates.nse, abundance.temp)
      
      # Update the progress bar
      pb.prog2 <- round(stratum.counter/n_distinct(nasc.nse$stratum)*100)
      info2 <- sprintf("%d%% done", pb.prog2)
      setTkProgressBar(pb2, pb.prog2, sprintf("Bootstrap - Stratum (%s)", info2), info2)
      
      # Update stratum counter
      stratum.counter <- stratum.counter + 1      
    }
    
    # Close the stratum counter
    close(pb2)
    
    # Update the progress bar
    pb.prog1 <- round(spp.counter/length(bootstrap.est.spp)*100)
    info1    <- sprintf("%d%% done", pb.prog1)
    setTkProgressBar(pb1, pb.prog1, sprintf("Bootstrap - Species (%s)", info1), info1)
    
    # Update the species counter
    spp.counter     <- spp.counter + 1
    # Combine survey summary by species
    survey.summary.nse  <- bind_rows(survey.summary.nse, survey.summ.temp)
    # Combine survey summary by species
    catch.summary.nse   <- bind_rows(catch.summary.nse, catch.summ.temp)
    # Combine stratum summary
    stratum.summary.nse <- bind_rows(stratum.summary.nse, pos.cluster.spp)
  }
  
  # Close the species counter
  close(pb1)
  
  # Save bootstrap results
  save(bootstrap.estimates.nse, abundance.estimates.nse, survey.summary.nse, 
       catch.summary.nse, stratum.summary.nse,
       file = (here("Output/biomass_bootstrap_est_nearshore.Rdata")))
} else{
  # Save bootstrap results
  load(here("Output/biomass_bootstrap_est_nearshore.Rdata"))
}

# Rename scientificName column
catch.summary.nse <- catch.summary.nse %>% 
  left_join(strata.summ.nse) %>%
  rename(Stock = stock)

# Summarise abundance across strata
abund.summ.nse <- abundance.estimates.nse %>%
  left_join(strata.summ.nse, by = c("Species" = "scientificName",
                                    "Stratum" = "stratum")) %>%
  group_by(Species, Stock = stock, SL) %>% 
  summarise(abundance = sum(freq)) %>% 
  mutate(TL = SL)  

# Calculate estimated biomass from from TL and estimated.wg --------------------
# CURRENTLY USING ESTIMATED.WG ESTIMATED FROM TOTAL LENGTH
# MUST UPDATE TO USE ESTIMATED.WG FROM STANDARD LENGTH
abund.summ.nse <- abund.summ.nse %>% 
  mutate(estimated.wg = estimate_ts(Species, TL, units = "cm")$estimated.wg,
         biomass      = abundance * estimated.wg)

# Add stock designations to bootstrap estimates
bootstrap.estimates.nse <- bootstrap.estimates.nse %>% 
  left_join(strata.summ.nse, by = c("Species" = "scientificName",
                                    "Stratum" = "stratum")) %>% 
  rename(Stock = stock)

# Summarize results from bootstrap per species and stratum
be.stratum.nse <- bootstrap.estimates.nse %>% 
  group_by(Species, Stock, Stratum) %>% 
  summarise(
    n.samples         = n(),
    Area              = Area[1]*2.915533e-07, # Convert m^2 to nmi^2
    biomass.mean.boot = mean(biomass)*10^3,
    biomass.sd        = sd(biomass)*10^3,
    biomass.se        = biomass.sd/sqrt(n.samples),
    abund.mean.boot   = mean(abundance),
    abund.sd          = sd(abundance),
    abund.se          = abund.sd/sqrt(n.samples),
    lower.ci.B        = quantile(biomass,probs = 0.025)*10^3,
    upper.ci.B        = quantile(biomass,probs = 0.975)*10^3) %>% 
  mutate(pct.tot.B    = biomass.mean.boot/sum(biomass.mean.boot)*100) %>%
  left_join(survey.summary.nse, by = c("Species","Stratum")) %>%
  left_join(catch.summary.nse,  by = c("Species" = "scientificName",
                                       "Stratum" = "stratum", "Stock")) %>% 
  arrange(Species, Stratum) %>%
  select(
    Species, Stock, Stratum, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd, pct.tot.B)

# Save results
save(be.stratum.nse, 
     file = here("Output/biomass_bootstrap_estimates_stratum_nearshore.Rdata"))

# Add stock designations to stratum summary
stratum.summary.nse <- stratum.summary.nse %>% 
  left_join(strata.summ.nse) %>% 
  rename(Stock = stock)

# Summarize clusters for each species for entire survey
cluster.summary.total.nse <- stratum.summary.nse %>%
  group_by(scientificName, Stock) %>% 
  summarise(nClusters = n_distinct(cluster)) %>% 
  rename(Species = scientificName)

# Summarize sampling for entire survey
survey.summary.total.nse <- survey.summary.nse %>% 
  group_by(Species, Stock = stock) %>% 
  summarise(
    Strata     = n(),
    nTransects = sum(nTransects),
    Distance   = sum(Distance)) %>% 
  left_join(cluster.summary.total.nse)

# Summarise catch for entire survey
catch.summary.total.nse <- catch.summary.nse %>% 
  group_by(Species = scientificName, Stock) %>% 
  summarise(nIndiv = sum(nIndiv))

# Summarize results from bootstrap per species across all samples
be.sample.nse <- bootstrap.estimates.nse %>% 
  group_by(Species, Stock, Sample) %>% 
  summarise(area    = sum(Area)*2.915533e-07,
            biomass = sum(biomass))

# Summarize results from bootstrap per species across all strata
be.survey.nse <- be.sample.nse %>%
  group_by(Species, Stock) %>%
  summarise(
    Area = area[1],
    biomass.mean.boot = mean(biomass) * 10^3,
    biomass.sd = sd(biomass) * 10^3,
    lower.ci.B = quantile(biomass, probs = 0.025) * 10^3,
    upper.ci.B = quantile(biomass, probs = 0.975) * 10^3) %>%
  left_join(survey.summary.total.nse) %>%
  left_join(catch.summary.total.nse) %>%
  arrange(Species) %>%
  select(
    Species, Stock, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd
  )

# Save results
write.csv(be.survey.nse, 
          file  = here("Output/biomass_bootstrap_estimates_survey_nearshore.csv"),
          quote = FALSE,row.names = FALSE)

save(be.survey.nse, file = here("Output/biomass_bootstrap_estimates_survey_nearshore.Rdata"))
