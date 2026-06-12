# Copy UCTD data files -------------------------------------
if (uctd.type %in% c("MVP","Valeport")) {
  # Location of header files on survey directory
  uctd.files <- dir_ls(uctd.dir, regexp = uctd.cast.pattern, recurse = TRUE) 
  
  # Copy files to estimATM directory
  file_copy(uctd.files, here("Data/UCTD"), 
            overwrite = overwrite.files) 
} else {
  # Copy Oceansciences UCTD data ------------------------------------
  # Location of header files on survey directory
  uctd.hdr <- dir_ls(uctd.dir, regexp = uctd.hdr.pattern, recurse = TRUE) %>% 
    path_filter(regexp = "_processed", invert = TRUE)
  
  # Copy files to estimATM directory
  file_copy(uctd.hdr, here("Data/UCTD"), 
            overwrite = overwrite.files)
  
  # Location of processed UCTD files on survey directory
  uctd.proc <- dir_ls(uctd.dir, regexp = uctd.cast.pattern, recurse = TRUE) %>% 
    path_filter(regexp = "_processed")
  
  # Copy files to estimATM directory
  file_copy(uctd.proc, here("Data/UCTD"), 
            overwrite = overwrite.files)      
}
