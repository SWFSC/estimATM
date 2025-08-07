# Copy CUFES database -------------------------------------
cufes.file <- dir_ls(file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES"), 
                     regexp = cufes.db.sqlite)

file_copy(cufes.file, here("Data/CUFES"), overwrite = overwrite.files)
