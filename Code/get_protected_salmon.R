salmon.sp <- c(161931,161974,161975,161976,161977,161979,161980,161989,161983) #trouts&salmon,Pacific salmon unid,pink,chum,coho,sockeye,Chinook,steelhead, cutthroat trout
# add common name to catch and specimen tables
specimens <- left_join(specimens,sp.code)

# combing haul and specimen data 
sp.haul <- left_join(specimens,haul, by=c("cruise","ship","haul","collection"))

# 202506
# filtering by salmon
events_event_data_tojoin <- events_event_data%>%
  rename(cruise=SURVEY,haul=EVENT_ID,ship=VESSEL_CODE,surfaceTempC=AvgSST-TSG45,salinityPPM=AvgSalinity-TSG45)%>%
  mutate(collection=as.numeric(collection))

salmon.spec.2506 <- specimen_final %>%
  filter(species %in% salmon.sp)%>%
  left_join(events_event_data_tojoin)%>%
  left_join(sp.code.sql)%>%
  select(cruise,ship,haul,collection,netInWaterTime,equilibriumTime,haulBackTime,surfaceTempC,salinityPPM,species,scientificName,commonName,specimenNumber,individual_ID,forkLength_mm,weightg,isAlive,adiposeCondition,hasDNAfinClip,hasTag)

events_event_data <- events %>%
  left_joint(ships)%>%
  left_join(event.data) %>%
  rename(collection=Collection,operator=Operator,fishingMode=FishingMode,
         gearType=GEAR,netInWaterTime=NetInWater,equilibriumTime=EQ,
         haulBackTime=Haulback,netOnDeckTime=NetOnDeck,
         downswellTow=DownswellTow)%>%
  arrange(EVENT_ID)
