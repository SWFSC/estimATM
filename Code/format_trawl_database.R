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
  
} else if (trawl.source == "CLAMS"){
  # Rename all the table columns - Will hopefully make subsequent processing more intuitive --------
  
  
  # Extract ITIS codes from SPECIES_DATA
  itis.codes <- itis.codes %>% 
    pivot_wider(names_from = SPECIES_PARAMETER, values_from = PARAMETER_VALUE)%>%
    select(species_code = SPECIES_CODE, species = ITIS_Code) # species is SWFSC species code
  
  ships <- ships %>% 
    rename(ship = VESSEL_CODE, ship.name = NAME, ship.desc = DESCRIPTION)
  
  # Format events and event.data df
  events <- events %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, gearType = GEAR, notes = COMMENTS) %>% 
    left_join(select(ships, SHIP, ship)) 
  
  event.data <- event.data %>% 
    pivot_wider(names_from = EVENT_PARAMETER, values_from = PARAMETER_VALUE) %>%
    rename(collection = Collection, operator = Operator, fishingMode = FishingMode,
           state = State, country = Country, 
           netInWaterTime = NetInWater, equilibriumTime = EQ, wireOutLengthMeters = WireOut,
           downswellToe = DownswellTow, arcedTow = ArcedTow,
           seaCondition = SeaCondition, cloudCondition = Clouds,
           haulBackTime = Haulback, netOnDeckTime = NetOnDeck) %>% 
    mutate(countryState = paste(country, state))
  
  event.stream <- event.stream %>% 
    pivot_wider(names_from = MEASUREMENT_TYPE, values_from = MEASUREMENT_VALUE) %>% 
    rename(surfaceTempC = SurfaceTemp, 
           windDirection = WindDirection, windSpeedKnots = WindSpeed,
           # salinityPPM = TBD, aveBottomDepthMeters = TBD, 
           # shipSpeedOverGround = SOG, shipSpeedThroughWater = TBD,
           # startLatDecimal = Latitude, startLongDecimal = Longitude,
           # stopLatDecimal = Latitude, stopLongDecimal = Longitude,
           # doorSpreadMetersEQ = DoorSpread, doorSpreadMeters10 = DoorSpread, 
           # doorSpreadMeters20 = DoorSpread, doorSpreadMetersHB = DoorSpread,
           # footRopeDepthEQ = Footrope , footRopeDepth10 = Footrope , 
           # footRopeDepth20 = Footrope , footRopeDepthHB = Footrope
           )
  
  gear.accy <- gear.accy %>% 
    pivot_wider(names_from = GEAR_ACCESSORY, values_from = GEAR_ACCESSORY_OPTION) %>% 
    left_join(select(ships, SHIP, ship)) %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, isTDRonHeadrope = HeadropeTDR, isTDRonFootrope = FootropeTDR)
  
  measurements <- measurements %>% 
    left_join(select(ships, SHIP, ship)) %>% 
    pivot_wider(names_from = "MEASUREMENT_TYPE", values_from = "MEASUREMENT_VALUE") %>%
    rename(cruise = SURVEY, haul = EVENT_ID,
           individual_ID = alpha_barcode, standardLength_mm = standard_length_mm, forkLength_mm = fork_length_mm,
           DNAtrayNumber = dna_tray_number, DNAvialNumber = dna_vial_number, isGonadSaved = ovary_taken,
           # isAlive = fish_condition, adiposeCondition = adipose_condition, 
           hasTag = head_taken)
  
  specimens <- specimens %>% 
    select(-c(WORKSTATION_ID, TIME_STAMP)) %>%
    rename(notes = "COMMENTS") %>%
    mutate(isRandomSample = case_when(
      SAMPLING_METHOD == "random" ~ "Y",
      SAMPLING_METHOD == "non_random" ~ "N"))
  
  # RESUME HERE
  
  
  # Build haul table ----------------------------------------------
  haul.all <- events %>% 
    left_join(event.data) %>% 
    rename(cruise = SURVEY, haul = EVENT_ID, collection = Collection, 
           operator = Operator, fishingMode = FishingMode,
           gearType = GEAR, netInWaterTime = NetInWater, equilibriumTime = EQ,
           haulBackTime = Haulback, netOnDeckTime = NetOnDeck, wireOutLengthMeters = WireOut,
           # startLatDecimal = TBD, startLongDecimal = TBD,
           # stopLatDecimal = TBD, stopLongDecimal = TBD,
           # surfaceTempC = AvgSurfaceTemp,
           # salinityPPM = TBD, aveBottomDepthMeters = TBD,
           # isTDRonHeadrope = HeadropeTDR, isTDRonFootrope = FootropeTDR,
           # windDirection = WindDirection, windSpeedKnots = WindSpeed,
           downswellTow = DownswellTow, arcedTow = ArcedTow,
           trawlPerformance = PERFORMANCE_CODE,
           seaCondition = SeaCondition, 
           cloudCondition = Clouds,
           # shipSpeedThroughWater = TBD, shipSpeedOverGround = TBD
           ) %>% 
    mutate(orderOcc = haul, flaggedData = NA_character_)
  
  # Format event.stream data
  ## Work in progress

  
  # Build catch table
  ## samples table is the precursor to the catch table
  ## need this to get species collected
  samples <- samples %>% 
    rename(ship_id = SHIP) %>%
    left_join(select(ships, ship_id = SHIP, ship = VESSEL_CODE)) %>%
    rename(cruise = SURVEY, haul = EVENT_ID) %>%
    left_join(select(haul.all, cruise, ship_id, ship, haul, collection)) %>% # Add collection
    left_join(itis.codes) %>% 
    left_join(spp.codes) %>% 
    rename(species = ITIS_Code, scientificName = SCIENTIFIC_NAME, commonName = COMMON_NAME)
  
  # str(samples)
  
  # Create specimens table
  specimens <- specimens %>% 
    # select(-c(WORKSTATION_ID, TIME_STAMP)) %>%
    # rename(notes = "COMMENTS") %>%
    # mutate(isRandomSample = case_when(
    #   SAMPLING_METHOD == "random" ~ "Y",
    #   SAMPLING_METHOD == "non_random" ~ "N")) %>% 
    left_join(select(samples, ship, cruise, haul, SAMPLE_ID)) %>% # What does this do?
    left_join(select(haul.all, cruise, ship_id, ship, haul, collection)) # Add collection
  
  # str(specimens2)
  
  # Create measurements
  ## This table provides any measurements for a specimen 
  ## Currently lacking DNA fin clip and individual info, also isAlive and adiposeCondition
  measurements <- measurements %>%
    left_join(ships) %>% 
    # select(-DEVICE_ID) %>%
    # rename(ship_id = SHIP, ship = VESSEL_CODE, cruise = SURVEY, haul = EVENT_ID) %>% 
    pivot_wider(names_from = "MEASUREMENT_TYPE", values_from = "MEASUREMENT_VALUE") %>%
    rename(individual_ID = alpha_barcode, standardLength_mm = standard_length_mm, forkLength_mm = fork_length_mm,
           DNAtrayNumber = dna_tray_number, DNAvialNumber = dna_vial_number, isGonadSaved = ovary_taken,
           # isAlive = fish_condition, adiposeCondition = adipose_condition, 
           hasTag = head_taken) # %>%
    # mutate(hasDNAfinClip = case_when(dna_finclip_number == "None" ~ "N",
    #                                  !is.na(dna_finclip_number) ~ "Y"),
    #        individual_ID = case_when(!is.na(dna_finclip_number) ~ dna_finclip_number,
    #                                  TRUE ~ individual_ID),
    #        individual_ID = replace(individual_ID, individual_ID == "None", NA))  
  
  specimens <- specimens %>%
    left_join(ships) %>% # Add ship info
    left_join(measurements) %>% # Add specimen measurements
    left_join(spp.codes) # Add ITIS code
  
  #### Code specimen number 
  specimens <- specimens %>%
    group_by(EVENT_ID, SAMPLE_ID) %>%
    arrange(EVENT_ID, SAMPLE_ID, SPECIMEN_ID)%>%
    mutate(specimenNumber = seq(1, length(SAMPLE_ID), 1),
           otolithNumber = specimenNumber)
  
  # creating final data set 
  specimens <- specimens %>%
    rename(species = ITIS_Code, weightg = weight_g, visualMaturity = maturity) %>%
    mutate(selectionReason = NA, flaggedData = NA) %>%
    mutate(
      visMaturityAssessor = case_when(
        !is.na(visualMaturity) ~ SCIENTIST),
      isGonadSaved = recode(isGonadSaved, "Yes" = "Y", "No" = "N"),
      sex = case_when(
        is.na(sex) ~ "unknown", TRUE ~ sex),
      sex = recode(sex, "Female" = "female", "Male" = "male", "Unknown" = "unknown"),
      visualMaturity = case_when(
        is.na(visualMaturity) ~ "notOpened", TRUE ~ visualMaturity),
      visualMaturity = recode(visualMaturity, "Immature" = "immature", "Intermediate" = "intermediate",
                              "Active" = "active", "Hydrated" = "hydrated", "Not Opened" = "notOpened"),
      # isAlive  = recode(isAlive, "Yes" = "Y", "No" = "N"),
      hasTag   = recode(hasTag, "Yes" = "Y","No" = "N"),
      IDmethod ="visualObservation",
      netSampleType = "codend") %>%
    ungroup(SAMPLE_ID, EVENT_ID) %>%
    select(cruise, ship, haul, collection, species, specimenNumber, standardLength_mm, forkLength_mm, weightg,
           sex, visualMaturity, visMaturityAssessor, isGonadSaved, otolithNumber, individual_ID, isRandomSample, selectionReason,
           DNAtrayNumber, DNAvialNumber, 
           # isAlive, adiposeCondition, hasDNAfinClip, 
           hasTag, IDmethod, notes) %>%
    mutate_at(vars(haul, collection, species, standardLength_mm, forkLength_mm,
                   weightg, DNAtrayNumber, DNAvialNumber), as.numeric)
  
  ## Building catch table ########
  # We created samples table in the specimen section above
  # The samples table will provide all species caught
  
  # Getting present only fish from samples table 
  present.catch <- samples %>%
    filter(SAMPLE_TYPE == "Present")
  
  # Need to get common and scientific name for checks 
  spp.codes     <- spp.codes %>%  select(-PARENT_TAXON)
  present.catch <- left_join(present.catch, spp.codes) %>%
    mutate(presenceOnly = "Y")
  
  # The catch summary table will get us individuals we have basket weights for
  catch_sum <- catch.data %>%
    mutate(presenceOnly = "N")
  
  catch_sum <-left_join(catch_sum, itis.codes) # get ITIS_code 
  
  catch_sum_present <- catch_sum %>% 
    bind_rows(present.catch) %>%
    arrange(EVENT_ID) %>% 
    left_join(ships) %>% # get ship code
    left_join(select(haul.all, cruise, ship_id, ship, haul, collection)) # get collection number

  # need to get suSampleWt of random individuals measured 
  sp_subsampleWt_count <- specimens %>%
    group_by(cruise, ship, haul, species) %>%
    filter(isRandomSample == "Y")%>%
    summarise(subSampleWtkg  = sum(weightg)/1000,
              subSampleCount = length(weightg))
  
  catch <- catch_sum_present %>%
    rename(species = ITIS_Code, notes = COMMENTS)%>%
    add_column(selectionReason = NA, flaggedData = NA) %>%
    mutate(netSampleType = "codend")%>%
    mutate_at(vars(haul, collection, species), as.numeric) %>% 
    left_join(sp_subsampleWt_count) %>%
    mutate(countcheck = SAMPLED_NUMBER == subSampleCount,
           remainingSubSampleWtkg = round(WEIGHT_IN_HAUL - SAMPLED_WEIGHT, 3))
  
  catch_final <- catch %>%
    mutate(hasLF = case_when(species %in% c(161729,161828,172412,168586,164792,161746,551209,
                                            161974,161975,161976,161977,161979,161980,161989) ~ "Y",
                             TRUE ~ "N"), # only will have lengths for CPS and salmon 
           isWtEstimated = "N") %>%
    select(cruise, ship, haul, collection, netSampleType, species,
           subSampleCount, subSampleWtkg, remainingSubSampleWtkg,
           hasLF, isWtEstimated, presenceOnly, flaggedData, notes) %>% View()
  
  # unique(catch$countcheck) #checking count columns, want only TRUE and NA
  
  # RESUME HERE
  
  # https://docs.google.com/spreadsheets/d/1a2Qe6STQWJbz5mqPAcpNBdEVMY-HcoIFwTF3dLM7Zjg/edit?gid=1810649095#gid=1810649095
  
  
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
