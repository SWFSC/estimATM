# Process and format trawl data.

# Formats raw data from database for use in other analyses, depending on database source
# Trawl data are extracted using Code/collect_trawl_database.R
# Intended to be run following collect_trawl_database.R in scripts that also load settings from Doc/settings

if (trawl.source == "Access") {
  # Reformat haul data to match SQL
  haul.all <- haul.all %>% 
    arrange(haul) %>% 
    mutate(
      startLatDecimal  =   startLatitudeDegrees + (startLatitudeMinutes/60),
      startLongDecimal = -(startLongitudeDegrees + (startLongitudeMinutes/60)),
      stopLatDecimal   =   stopLatitudeDegrees + (stopLatitudeMinutes/60),
      stopLongDecimal  = -(stopLongitudeDegrees + (stopLongitudeMinutes/60))) %>%
    mutate(haulBackTime = case_when(
      haulBackTime < equilibriumTime ~ haulBackTime + days(1),
      TRUE ~ haulBackTime)) %>% 
    rename(duration = Duration, notes = Notes) %>% 
    mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
           recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
           evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"))
  
  # Identify hauls where date of equilibriumTime or haulBackTime is incorrect
  eq.fix <- which(c(0, diff(haul.all$equilibriumTime)) < 0)
  hb.fix <- which(c(0, diff(haul.all$haulBackTime)) < 0)
  
  # Correct equilibriumTime or haulBackTime
  haul.all$equilibriumTime[eq.fix] <- haul.all$equilibriumTime[eq.fix] + days(1)
  haul.all$haulBackTime[eq.fix]    <- haul.all$haulBackTime[eq.fix] + days(1)
  
  # Reformat length frequency data to match SQL
  if (exists("lengthFreq.all")) {
    lengthFreq.all <- lengthFreq.all %>% 
      rename(length = Length, lengthType = LengthType, 
             sexUnknown = NotDetermined, male = Male, activeFemale = ActiveFemale, 
             inactiveFemale = InactiveFemale, totalFemale = TotalFemale, 
             subSampleNumber = SubSampleNumber)
  }
  
} else if (trawl.source == "SQL") {
  haul.all <- haul.all %>% 
    arrange(haul) %>% 
    mutate(
      netInWaterTime  = ymd_hms(netInWaterTime),
      equilibriumTime = ymd_hms(equilibriumTime),
      haulBackTime    = ymd_hms(haulBackTime),
      netOnDeckTime   = ymd_hms(netOnDeckTime)) %>% 
    mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
           recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
           evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"))
  
} else if (trawl.source == "Excel") {
  # Format haul data
  haul.all <- haul.all %>% 
    mutate(
      startLatDecimal  =   DecLatitude,
      startLongDecimal =   DecLongitude,
      stopLatDecimal   =   DecLatitude,
      stopLongDecimal  =   DecLongitude,
      equilibriumTime  =   mdy_hms(paste(as.character(trawlDate),
                                         format(haul.all$EquilibriumTime, 
                                                format = "%H:%M:%S"))),
      haulBackTime     =   equilibriumTime + minutes(`Duration(dec)`*60))
  
  # Filter haul data for current survey
  haul.all <- haul.all %>% 
    select(cruise = Cruise,ship = Ship,haul = Haul,collection = Collection,
           startLatDecimal,startLongDecimal,stopLatDecimal,
           stopLongDecimal,equilibriumTime,haulBackTime) 
  
} else if (trawl.source %in% c("CLAMS-Oracle", "CLAMS-SQLite")){
  
  # Load trawl data
  # load(here("Data/Trawl/trawl_data_raw.Rdata"))
  
  # Integration key
  # https://docs.google.com/spreadsheets/d/1a2Qe6STQWJbz5mqPAcpNBdEVMY-HcoIFwTF3dLM7Zjg/edit?gid=1810649095#gid=1810649095
  
  # Rename all the table columns - Will hopefully make subsequent processing more intuitive --------
  # Extract ITIS codes from SPECIES_DATA
  itis.codes <- itis.codes %>% 
    pivot_wider(names_from = SPECIES_PARAMETER, values_from = PARAMETER_VALUE) %>%
    select(species_code = SPECIES_CODE, species = ITIS_Code) %>% 
    mutate(species = as.numeric(species))
  
  # Format SWFSC species codes table
  spp.codes <- spp.codes %>% 
    rename(species_code = SPECIES_CODE, 
           scientificName = SCIENTIFIC_NAME, commonName = COMMON_NAME) %>% 
    left_join(itis.codes) %>% 
    filter(!is.na(species))
  
  # Format ships table
  ships <- ships %>% 
    rename(ship = VESSEL_CODE, ship.name = NAME, ship.desc = DESCRIPTION)
  
  # Format surveys table
  surveys <- surveys %>% 
    filter(SURVEY == cruise.name)
  
  # Format event performance table
  event.perf <- event.perf %>% 
    rename(PERFORMANCE_DESC = DESCRIPTION) %>% 
    mutate(trawlPerformance = case_when(
      PERFORMANCE_CODE == 0 ~ "Good",
      str_detect(PERFORMANCE_DESC, "Abort*") ~ "Aborted",
      TRUE ~ PERFORMANCE_DESC))
  
  # Format events and event.data table
  ## Start with event.data table
  event.data <- event.data %>% 
    pivot_wider(names_from = EVENT_PARAMETER, values_from = PARAMETER_VALUE) %>% 
    filter(PARTITION == "MainTrawl") %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, 
           gearType = Gear,
           collection = Collection, operator = Operator, fishingMode = FishingMode,
           state = State, country = Country,
           netInWaterTime = NetInWater, equilibriumTime = EQ, 
           # wireOutLengthMeters = WireOut, # Comes from SCS; currently unavailable
           downswellTow = DownswellTow, 
           arcedTow = ArcedTow,
           seaCondition = SeaCondition,
           cloudCondition = Clouds,
           haulBackTime = Haulback, netOnDeckTime = NetOnDeck) %>% 
    mutate(countryState = paste(country, state)) %>%
    # Convert times to POSIXct and PDT time zone
    mutate(netInWaterTime  = mdy_hms(netInWaterTime,  tz = "America/Los_Angeles"),
           equilibriumTime = mdy_hms(equilibriumTime, tz = "America/Los_Angeles"),
           haulBackTime    = mdy_hms(haulBackTime,    tz = "America/Los_Angeles"),
           netOnDeckTime   = mdy_hms(netOnDeckTime,   tz = "America/Los_Angeles"),
           EQ10Min         = mdy_hms(EQ10Min,         tz = "America/Los_Angeles"),
           EQ20Min         = mdy_hms(EQ20Min,         tz = "America/Los_Angeles"))
  
  ## Join with event.data to create final events
  events <- events %>% 
    filter(SURVEY == cruise.name) %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, gearType = GEAR, notes = COMMENTS) %>% 
    left_join(select(ships, SHIP, ship)) %>%
    left_join(event.perf) %>% 
    left_join(event.data) %>% 
    select(cruise, ship, haul, collection, everything()) 
  
  if (!"startLatDecimal" %in% names(events))
    events <- events %>%
    mutate(startLatDecimal = NA, startLongDecimal = NA,
           stopLatDecimal = NA, stopLongDecimal = NA)
  
  # Format catch data
  catch.data <- catch.data %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, species_code = SPECIES_CODE) %>%
    left_join(select(ships, SHIP, ship)) %>% 
    left_join(select(events, cruise, ship, haul, collection)) %>% # Add collection
    left_join(itis.codes) %>% 
    # left_join(spp.codes) %>% 
    select(cruise, ship, haul, collection, everything())
  
  # Format gear accessory table
  gear.accy <- gear.accy %>% 
    filter(SURVEY == cruise.name) %>% 
    pivot_wider(names_from = GEAR_ACCESSORY, values_from = GEAR_ACCESSORY_OPTION) %>% 
    left_join(select(ships, SHIP, ship)) %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, isTDRonHeadrope = HeadropeTDR, isTDRonFootrope = FootropeTDR) 
  
  # # Create missing variable if missing
  if (!"dna_finclip_number" %in% names(measurements))
    measurements$dna_finclip_number <- NA_character_
  if (!"fish_condition" %in% names(measurements))
    measurements$fish_condition <- NA_character_
  if (!"adipose_condition" %in% names(measurements))
    measurements$adipose_condition <- NA_character_
  if (!"head_taken" %in% names(measurements))
    measurements$head_taken <- NA_character_
  
  # Format measurements table
  measurements <- measurements %>% 
    filter(SURVEY == cruise.name) %>% 
    select(-DEVICE_ID) %>%
    left_join(select(ships, SHIP, ship)) %>% 
    pivot_wider(names_from = "MEASUREMENT_TYPE", values_from = "MEASUREMENT_VALUE") %>%
    rename(cruise = SURVEY, haul = EVENT_ID,
           individual_ID = alpha_barcode, weightg = weight_g,
           standardLength_mm = standard_length_mm, forkLength_mm = fork_length_mm,
           DNAtrayNumber = dna_tray_number, DNAvialNumber = dna_vial_number, isGonadSaved = ovary_taken,
           isAlive = fish_condition, adiposeCondition = adipose_condition,
           visualMaturity = maturity, hasTag = head_taken) %>% 
    left_join(select(events, cruise, ship, haul, collection)) %>% # Add collection
    mutate(totalLength_mm = NA, mantleLength_mm = NA) %>% 
    mutate(hasDNAfinClip = case_when(dna_finclip_number == "None" ~ "N", 
                                     !is.na(dna_finclip_number) ~ "Y",
                                     TRUE ~ NA_character_),
           individual_ID = case_when(!is.na(dna_finclip_number) ~ dna_finclip_number, 
                                     .default=individual_ID),
           individual_ID = replace(individual_ID, individual_ID == "None", NA)) %>% 
    select(cruise, ship, haul, collection, everything())
  
  # RESUME HERE
  
  # Format samples table
  ## First entry for species caught and entered into Catch form
  samples <- samples %>%
    filter(SURVEY == cruise.name) %>% 
    left_join(select(ships, SHIP, ship)) %>%
    rename(cruise = SURVEY, haul = EVENT_ID, 
           species_code = SPECIES_CODE, datetime = TIME_STAMP, notes = COMMENTS) %>% 
    # Separate these steps from the "renaming" portion of the code?
    left_join(select(events, cruise, ship, haul, collection)) %>% # Add collection
    left_join(itis.codes) %>% 
    # left_join(spp.codes) %>% 
    select(cruise, ship, haul, collection, everything())
  
  specimens <- specimens %>% 
    filter(SURVEY == cruise.name) %>% 
    left_join(select(ships, SHIP, ship)) %>%
    rename(cruise = SURVEY, haul = EVENT_ID, datetime = TIME_STAMP, notes = COMMENTS) %>% 
    left_join(select(events, cruise, ship, haul, collection)) %>% # Add collection
    select(cruise, ship, haul, collection, everything()) %>% 
    mutate(isRandomSample=case_when(SAMPLING_METHOD=="random" ~ "Y",
                                    SAMPLING_METHOD=="non_random" ~ "N")) %>% 
    left_join(measurements) %>% 
    left_join(select(samples, ship, cruise, haul, SAMPLE_ID, species_code, species)) %>%
    group_by(haul, SAMPLE_ID) %>%
    arrange(haul, SAMPLE_ID, SPECIMEN_ID) %>%
    mutate(specimenNumber      = seq(1,length(SAMPLE_ID), 1),
           # Only want otolith number = specimen number for fish with an individual id
           otolithNumber       = case_when(
             !is.na(individual_ID) ~ specimenNumber,
             is.na(individual_ID) ~ NA), 
           selectionReason     = NA_character_, flaggedData = NA_character_,
           visMaturityAssessor = case_when(!is.na(visualMaturity) ~ SCIENTIST),
           isGonadSaved        = recode(isGonadSaved, "Yes" = "Y", "No" = "N"),
           sex                 = case_when(is.na(sex) ~ "unknown", .default = sex),
           sex                 = recode(sex, "Female" = "female", "Male" = "male", "Unknown" = "unknown"),
           visualMaturity      = case_when(is.na(visualMaturity) ~ "notOpened", .default=visualMaturity),
           visualMaturity      = recode(visualMaturity, "Immature" = "immature", "Intermediate" = "intermediate",
                                        "Active" = "active", "Hydrated" = "hydrated", "Not Opened" = "notOpened"),
           isAlive             = recode(isAlive, "Yes" = "Y", "No" = "N"),
           hasTag              = recode(hasTag, "Yes" = "Y", "No" = "N"),
           IDmethod            = "visualObservation",
           netSampleType       = "codend") %>%
    ungroup(SAMPLE_ID) %>%
    mutate_at(vars(haul, collection, species, standardLength_mm, forkLength_mm,
                   weightg, DNAtrayNumber, DNAvialNumber), as.numeric) %>% 
    mutate(weightg = weightg*1000) %>% # CLAMS specimen weights are in kg; convert to g 
    select(cruise, ship, haul, collection, species, specimenNumber, 
           standardLength_mm, forkLength_mm, totalLength_mm, mantleLength_mm,
           sex, visualMaturity, visMaturityAssessor, isGonadSaved, weightg,
           otolithNumber, individual_ID, isRandomSample, selectionReason,
           DNAtrayNumber, DNAvialNumber, 
           isAlive, adiposeCondition, hasDNAfinClip,
           hasTag, IDmethod, notes, flaggedData) 
  
  # Build catch table
  ## Get presence-only samples
  catch.present <- samples %>%
    filter(SAMPLE_TYPE=="Present") %>% 
    mutate(presenceOnly = "Y")
  
  ## Get all other catch samples
  catch.summ <- catch.data %>%
    mutate(presenceOnly = "N") 
  
  ## Combine catch and presence-only
  catch.all <- catch.summ %>% 
    bind_rows(catch.present) %>%
    arrange(haul) 
  
  # need to get subSampleWt of random individuals measured 
  sp_subsampleWt_count <- specimens %>%
    group_by(cruise, ship, haul, collection, species) %>%
    filter(isRandomSample == "Y")%>%
    summarise(subSampleWtkg = sum(weightg, na.rm = TRUE)/1000, # Check the na.rm
              subSampleCount = length(weightg))
  
  # Create final tables -------------------------------------------
  ## Final haul table 
  haul.all <- events %>% 
    mutate_at(vars(haul, collection), as.numeric) %>%
    mutate(orderOcc = haul, flaggedData = NA_character_) %>% 
    mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
           recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
           evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"))
  
  ## Final catch table 
  catch.all <- catch.all %>%
    mutate(selectionReason = NA_character_, 
           flaggedData = NA_character_,
           netSampleType="codend") %>%
    mutate_at(vars(haul, collection, species), as.numeric) %>% 
    left_join(sp_subsampleWt_count) %>%
    mutate(countcheck = SAMPLED_NUMBER == subSampleCount,
           remainingSubSampleWtkg = round(WEIGHT_IN_HAUL - SAMPLED_WEIGHT, 3)) %>% 
    mutate(flaggedData = NA_character_,
           hasLF = case_when(species %in% c(161729, 161828, 172412, 168586, 164792, 161746, 551209,
                                            161974, 161975, 161976, 161977, 161979, 161980, 161989) ~ "Y",
                             .default="N"), # Only will have lengths for CPS and salmon 
           isWtEstimated = "N") %>%
    select(cruise, ship, haul, collection, netSampleType, species,
           subSampleCount, subSampleWtkg, remainingSubSampleWtkg,
           hasLF, isWtEstimated, presenceOnly, flaggedData, notes)
  
  ## Final specimen table 
  lengths.all <- specimens %>% ungroup()
}

# Classify hauls by season (spring or summer)
haul.all <- haul.all %>% 
  mutate(season = case_when(
    month(equilibriumTime) < 6 ~ "spring",
    TRUE ~ "summer"))

# Compute totalWeight and totalNum, which don't reliably exist in either database
catch.all <- catch.all %>% 
  replace_na(list(remainingSubSampleWtkg = 0)) %>% 
  mutate(
    totalWeight = subSampleWtkg + remainingSubSampleWtkg,
    totalNum    = (subSampleCount/subSampleWtkg)*totalWeight)

# Unused code -----------------------------

# CLAMS
# Probably don't need; use averages in other tables
# event.stream <- event.stream %>% 
#   pivot_wider(names_from = MEASUREMENT_TYPE, values_from = MEASUREMENT_VALUE) %>% 
#   rename(surfaceTempC = SurfaceTemp, 
#          windDirection = WindDirection, windSpeedKnots = WindSpeed,
#          # salinityPPM = TBD, aveBottomDepthMeters = TBD, 
#          # shipSpeedOverGround = SOG, shipSpeedThroughWater = TBD,
#          # startLatDecimal = Latitude, startLongDecimal = Longitude,
#          # stopLatDecimal = Latitude, stopLongDecimal = Longitude,
#          # doorSpreadMetersEQ = DoorSpread, doorSpreadMeters10 = DoorSpread, 
#          # doorSpreadMeters20 = DoorSpread, doorSpreadMetersHB = DoorSpread,
#          # footRopeDepthEQ = Footrope , footRopeDepth10 = Footrope , 
#          # footRopeDepth20 = Footrope , footRopeDepthHB = Footrope
#          )
