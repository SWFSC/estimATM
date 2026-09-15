# Collect seine data from the trawl database. 

# Extracts all data in all specified tables and saves to an output file
# Seine data are further processed and formatted in Code/format_seine_database.R
# Intended to be run in scripts that load settings from Doc/settings

if (seine.source == "Excel") {
  # Extract tables from Excel source file
  sets.all        <- readxl::read_xlsx(file.path(survey.dir[survey.vessel.primary],
                                                 seine.dir, seine.xlsx.name), sheet = "sets")
  set.catch.all   <- readxl::read_xlsx(file.path(survey.dir[survey.vessel.primary],
                                                 seine.dir, seine.xlsx.name), sheet = "catch")
  set.lengths.all <- readxl::read_xlsx(file.path(survey.dir[survey.vessel.primary],
                                                 seine.dir, seine.xlsx.name), sheet = "specimen")
  spp.codes       <- readxl::read_xlsx(file.path(survey.dir[survey.vessel.primary],
                                                 seine.dir, seine.xlsx.name), sheet = "species_codes")
} else {
  # Extract tables from appropriate database
  if (seine.source == "SQL") {
    # Configure ODBC connection to TRAWL database
    seine.con  <- DBI::dbConnect(odbc::odbc(),
                                 DRIVER="SQL Server",
                                 Encrypt = "Optional",
                                 DATABASE="Trawl",
                                 Trusted_Connection= "Yes",
                                 SERVER = trawl.db.server)
  } else if (seine.source == "SQL-dev") {
    # Configure ODBC connection to TRAWL database
    seine.con  <- DBI::dbConnect(odbc::odbc(),
                                 DRIVER="SQL Server",
                                 Encrypt = "Optional",
                                 DATABASE="Trawl_dev",
                                 Trusted_Connection= "Yes",
                                 SERVER = trawl.db.server)
  } else if (seine.source == "Access") {
    # Copy trawl Access database
    seine.db <- fs::dir_ls(file.path(survey.dir[survey.vessel.primary],
                                     seine.dir),
                           regexp = seine.db.name)
    
    fs::file_copy(seine.db, here::here("Data/Seine"), overwrite = TRUE)
    
    # Configure ODBC connection to TRAWL database
    seine.con  <- DBI::dbConnect(odbc::odbc(), 
                                 Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                                 DBQ = file.path(here::here("Data/Seine"), seine.db.name))
  } 
  
  # List database tables
  table.list <- DBI::dbListTables(seine.con)
  
  # Import trawl database tables
  sets.all        <- dplyr::tbl(seine.con, grep("Nearshore_Set", table.list, value = TRUE)) %>% dplyr::collect()
  set.catch.all   <- dplyr::tbl(seine.con, grep("Nearshore_Catch", table.list, value = TRUE)) %>% dplyr::collect()
  set.lengths.all <- dplyr::tbl(seine.con, grep("Nearshore_Specimen", table.list, value = TRUE)) %>% dplyr::collect()
  spp.codes       <- dplyr::tbl(seine.con, grep("SpeciesCodes", table.list, value = TRUE)) %>% dplyr::collect()
  
  # Close database channel
  DBI::dbDisconnect(seine.con)
}


# Create directory for saving seine data
fs::dir_create(here("Data/Seine"))

# Save imported database data to .Rdata file
if (exists("lengthFreq.all")) {
  save(sets.all, set.catch.all, set.lengths.all, spp.codes, lengthFreq.all, 
       file = here::here("Data/Seine/seine_data_raw.Rdata"))
} else {
  save(sets.all, set.catch.all, set.lengths.all, spp.codes,
       file = here::here("Data/Seine/seine_data_raw.Rdata"))
}
