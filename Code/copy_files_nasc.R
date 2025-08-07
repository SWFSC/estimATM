# Copy CSV files for CPS and krill
pb <- tkProgressBar("R Progress Bar", "CSV File Copying", 0, 100, 0)

for (d in seq_along(nasc.vessels)) {
  # Get vessel name
  dd <- nasc.vessels[d]
  
  # List CSV files for CPS and krill
  csv.files.cps <- dir_ls(file.path(survey.dir[dd], 
                                    nasc.dir[dd]), 
                          regexp = nasc.pattern.cps[dd],
                          recurse = nasc.recurse[dd],
                          ignore.case = TRUE)
  
  csv.files.krill <- dir_ls(file.path(survey.dir[dd], 
                                      nasc.dir[dd]), 
                            regexp = nasc.pattern.krill[dd],
                            recurse = nasc.recurse[dd],
                            ignore.case = TRUE)
  
  # Copy CSV files
  if (overwrite.csv) {
    # Copy all files
    file_copy(csv.files.cps, here("Data/Backscatter", dd, path_file(csv.files.cps)), 
              overwrite = overwrite.csv)
    
    file_copy(csv.files.krill, here("Data/Backscatter", dd, path_file(csv.files.krill)), 
              overwrite = overwrite.csv)
  } else {
    # List existing files
    files.cps <- path_file(dir_ls(here("Data/Backscatter", dd),
                                  regexp = nasc.pattern.cps[dd],
                                  recurse = nasc.recurse[dd],
                                  ignore.case = TRUE))
    
    files.krill <- path_file(dir_ls(here("Data/Backscatter", dd),
                                    regexp = nasc.pattern.krill[dd],
                                    recurse = nasc.recurse[dd],
                                    ignore.case = TRUE))
    
    # Copy new files only
    file_copy(str_subset(csv.files.cps, files.cps, negate = TRUE), 
              here("Data/Backscatter", dd))
    
    file_copy(str_subset(csv.files.krill, files.krill, negate = TRUE), 
              here("Data/Backscatter", dd))
  }
  
  # Update the progress bar
  pb.prog <- round(d/length(nasc.vessels)*100)
  info <- sprintf("%d%% done", pb.prog)
  setTkProgressBar(pb, pb.prog, sprintf("CSV Copying - (%s)", info), info)
}

close(pb)
