# List all trawl databases
haul.db.info <- dir_ls(file.path(survey.dir[survey.vessel.primary], trawl.dir),
                       regexp = trawl.db.ext) %>% 
  tibble(file_info(.))

# Copy most recently modified trawl database
if (nrow(haul.db.info) > 0) {
  file_copy(haul.db.info$path[which.max(haul.db.info$modification_time)], 
            here("Data/Trawl", trawl.db.name), overwrite = overwrite.files)
}
