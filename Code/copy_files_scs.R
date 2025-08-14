# List MOA Continuous .elg files from the appropriate locations
log.files <- dir_ls(scs.nav.path, regexp = scs.nav.pattern, recurse = scs.nav.recurse) %>% 
  str_subset(scs.nav.dir)

file_copy(log.files, here("Data/SCS"), overwrite = overwrite.files)
