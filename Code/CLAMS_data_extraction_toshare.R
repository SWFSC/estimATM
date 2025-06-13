library(RODBC)
library(tidyverse)

# connecting to whole database 
clams_db <- odbcConnect("",uid="",pw="",believeNRows=FALSE)

# meta data tables needed by all #####
# bring in ship code 
ship <- sqlQuery(clams_db, 'SELECT * FROM SHIPS') %>%
  select(SHIP,VESSEL_CODE)
# need to get our itis code 
itis_code <- sqlQuery(clams_db, 'SELECT * FROM SPECIES_DATA')%>%
  pivot_wider(names_from = SPECIES_PARAMETER, values_from = PARAMETER_VALUE)%>%
  select(SPECIES_CODE,ITIS_Code)

## building haul table WORK IN PROGRESS #####
# bring in survey data 
events <- sqlQuery(clams_db, 'SELECT * FROM EVENTS')

# get vessel code in events 
events <- left_join(events,ship)

event_data <- sqlQuery(clams_db, 'SELECT * FROM EVENT_DATA') %>%
  select(-PARTITION) %>%   # don't need partition weight 
  pivot_wider(names_from=EVENT_PARAMETER,values_from=PARAMETER_VALUE)

events_event_data <- left_join(events,event_data) %>%
  rename(collection=Collection,operator=Operator,fishingMode=FishingMode,
         gearType=GEAR,netInWaterTime=NetInWater,equilibriumTime=EQ,
         haulBackTime=Haulback,netOnDeckTime=NetOnDeck,
         downswellTow=DownswellTow)

### WORK IN PROGRESS PART 
event_stream_data <- sqlQuery(clams_db, 'SELECT * FROM EVENT_STREAM_DATA')
  

# renaming columns for end
# need to make order occ = haul #
# rename("cruise"="SURVEY","ship"="SHIP","haul"="EVENT_ID")
# mutate(orderOcc = haul)


### building specimen table #####
# need to get collection number for event 
events.catch <- events_event_data %>%
  select(SURVEY,SHIP,VESSEL_CODE,EVENT_ID,collection)

# samples table is the first entry for species caught and entered into Catch form
# need this to get species_code 
samples <- sqlQuery(clams_db, 'SELECT * FROM SAMPLES')
samples <- left_join(samples, events.catch) # events.catch can be found in building specimen table section
samples <- left_join(samples, itis_code)

# specimen table comes first, then merge with measurements table 
# need to get itis code for later 
samples.catch <- samples %>%
  select(SHIP,SURVEY,EVENT_ID,SAMPLE_ID,SPECIES_CODE,ITIS_Code)

# this is base of specimen table   
specimen_original <- sqlQuery(clams_db, 'SELECT * FROM SPECIMEN')%>%
  select(-c(WORKSTATION_ID,TIME_STAMP))%>%
  rename(notes="COMMENTS")%>%
  mutate(isRandomSample=case_when(SAMPLING_METHOD=="random" ~ "Y",
                                  SAMPLING_METHOD=="non_random" ~ "N"))
specimen_original <- left_join(specimen_original,events.catch) # Add collection

# this table provides any measurments for a specimen 
measurements<- sqlQuery(clams_db, 'SELECT * FROM MEASUREMENTS')%>%
  select(-DEVICE_ID)%>%
  pivot_wider(names_from = "MEASUREMENT_TYPE",values_from = "MEASUREMENT_VALUE")%>%
  rename(individual_ID=alpha_barcode,standardLength_mm=standard_length_mm,forkLength_mm=fork_length_mm,
         DNAtrayNumber=dna_tray_number, DNAvialNumber=dna_vial_number,isGonadSaved=ovary_taken,
    isAlive=fish_condition,adiposeCondition=adipose_condition,hasTag=head_taken)%>% # done
  # dna_finclip_number not in test version of CLAMS - KLS
  mutate(hasDNAfinClip = case_when(dna_finclip_number=="None"~"N",!is.na(dna_finclip_number)~"Y"),
         individual_ID = case_when(!is.na(dna_finclip_number)~dna_finclip_number,.default=individual_ID),
         individual_ID = replace(individual_ID, individual_ID == "None", NA)) 

specimen <- left_join(specimen_original,measurements) 
specimen <- left_join(specimen,samples.catch)

#### Code specimen number 
specimen <- specimen %>%
group_by(EVENT_ID,SAMPLE_ID) %>%
  arrange(EVENT_ID,SAMPLE_ID,SPECIMEN_ID)%>%
  mutate(specimenNumber = seq(1,length(SAMPLE_ID),1),
         otolithNumber = specimenNumber)

# creating final dataset 
specimen_final <- specimen %>%
  rename(cruise=SURVEY,haul=EVENT_ID,ship=VESSEL_CODE,species=ITIS_Code,
         visualMaturity=maturity,weightg=weight_g)%>%
  add_column(selectionReason=NA,flaggedData=NA) %>%
  mutate(visMaturityAssessor = case_when(!is.na(visualMaturity)~SCIENTIST),
         isGonadSaved = recode(isGonadSaved, "Yes"="Y","No"="N"),
         sex = case_when(is.na(sex)~"unknown",.default=sex),
         sex = recode(sex, "Female"="female","Male"="male","Unknown"="unknown"),
         visualMaturity = case_when(is.na(visualMaturity)~"notOpened",.default=visualMaturity),
         visualMaturity = recode(visualMaturity,"Immature"="immature","Intermediate"="intermediate",
         "Active"="active","Hydrated"="hydrated","Not Opened"="notOpened"),
         isAlive = recode(isAlive, "Yes"="Y","No"="N"),
         hasTag = recode(hasTag, "Yes"="Y","No"="N"),
         IDmethod="visualObservation",
         netSampleType="codend")%>%
  ungroup(SAMPLE_ID) %>%
    select(cruise,ship,haul,collection,species,specimenNumber,standardLength_mm,forkLength_mm,weightg,
         sex,visualMaturity,visMaturityAssessor,isGonadSaved,otolithNumber,individual_ID,isRandomSample,selectionReason,
         DNAtrayNumber,DNAvialNumber,isAlive,adiposeCondition,hasDNAfinClip,hasTag,
         IDmethod,notes) %>%
  mutate_at(vars(haul,collection,species,standardLength_mm,forkLength_mm,
                 weightg,DNAtrayNumber,DNAvialNumber),as.numeric)


## building catch table ########
# we created samples table in the specimen section above
# the samples table will provide all species caught

# getting present only fish from samples table 
present.catch <- samples %>%
  filter(SAMPLE_TYPE=="Present")

# need to get common and scientific name for checks 
species_names <- sqlQuery(clams_db, 'SELECT * FROM SPECIES') %>% select(-PARENT_TAXON)
present.catch <- left_join(present.catch,species_names) %>%
  mutate(presenceOnly="Y")

# the catch summary table will get us individuals we have basket weights for
catch_sum <- sqlQuery(clams_db, 'SELECT * FROM CATCH_SUMMARY') %>%
  mutate(presenceOnly="N") 
catch_sum <-left_join(catch_sum, itis_code) # get itis_code 

catch_sum_present <- bind_rows(catch_sum,present.catch) %>%
  arrange(EVENT_ID)
catch_sum_present <- left_join(catch_sum_present,ship) # get ship code
catch_sum_present <- left_join(catch_sum_present,events.catch) # get collection number

# need to get subSampleWt of random individuals measured 
sp_subsampleWt_count <- specimen_final %>%
  group_by(cruise,ship,haul,species)%>%
  filter(isRandomSample=="Y")%>%
  summarise(subSampleWtkg=sum(weightg)/1000,
            subSampleCount=length(weightg))

catch <- catch_sum_present %>%
  rename(cruise=SURVEY,haul=EVENT_ID,ship=VESSEL_CODE,species=ITIS_Code,notes=COMMENTS)%>%
  add_column(selectionReason=NA,flaggedData=NA) %>%
  mutate(netSampleType="codend")%>%
  mutate_at(vars(haul,collection,species),as.numeric)


catch <- left_join(catch,sp_subsampleWt_count) %>%
  mutate(countcheck = SAMPLED_NUMBER==subSampleCount,
         remainingSubSampleWtkg = round(WEIGHT_IN_HAUL - SAMPLED_WEIGHT,3))
unique(catch$countcheck) #checking count columns, want only TRUE and NA

catch_final <- catch %>%
  add_column(flaggedData=NA)%>%
  mutate(hasLF = case_when(species %in% c(161729,161828,172412,168586,164792,161746,551209,
                                          161974,161975,161976,161977,161979,161980,161989)~"Y",.default="N"), #only will ahve lengths for CPS and salmon 
         isWtEstimated="N")%>%
  select(cruise,ship,haul,collection,netSampleType,species,
       subSampleCount,subSampleWtkg,remainingSubSampleWtkg,
       hasLF,isWtEstimated,presenceOnly,flaggedData,notes)
  


