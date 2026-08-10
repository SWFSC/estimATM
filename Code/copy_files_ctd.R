# Copy CTD data files -------------------------------------
# List raw CTD ASCII files
ctd.hdr <- dir_ls(ctd.hdr.dir, regexp = ctd.hdr.pattern, recurse = TRUE) %>% 
  path_filter(regexp = "_processed", invert = TRUE)

# Exclude certain files defined by exclude.uctd
ctd.files <- ctd.files[which(!path_file(ctd.files) %in% exclude.ctd)]

# Copy files to plotCTD directory
file_copy(ctd.hdr, here("Data/CTD"), 
          overwrite = overwrite.files)

# Location of processed CTD files on survey directory
ctd.proc <- dir_ls(ctd.dir, regexp = ctd.cast.pattern, recurse = TRUE) %>% 
  path_filter(regexp = "_processed")

# Copy files to plotCTD directory
file_copy(ctd.proc, here("Data/CTD"),
          overwrite = overwrite.files)
