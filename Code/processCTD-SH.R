# This script is used to batch process uCTD raw data files, then use the results
# from the CTD cast to create Echoview-calibration files (.ecs) containing the
# appropriate temperature, salinity, absorption depths, and sound speed.

# Load required packages --------------------------------------------------

library(readr)    # For reading and writing plain text files
library(stringr)  # For processing strings

# User Settings -----------------------------------------------------------

# On AST4----------------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20230703_SHIMADA_SummerHake\\DATA\\CTD\\CTD_to_Process\\'

# Directory to store processed data results
dir.output <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\'

# Directory containing SBEDataProcessing Program Setup (.psa) files
dir.PSA <- paste0(normalizePath(file.path(getwd(), 'CODE/PSA/')),'\\')

# # CTD configuration file
# file.con <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20230703_SHIMADA_SummerHake\\DATA\\CTD\\_2307SH.XMLCON'

# Directory of Seabird SBEDataProcessing programs
dir.SBE <- 'C:\\Program Files (x86)\\Sea-Bird\\SBEDataProcessing-Win32\\'

# Template ECS file
ECS.template <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20230703_SAILDRONE_SummerCPS\\PROCESSED\\EV\\ECS\\1048\\_2307SD_1048_Template.ecs'

# ECS output directory
dir.ECS <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20230703_SAILDRONE_SummerCPS\\PROCESSED\\EV\\ECS\\1048\\Shimada_Casts\\TEMP\\'

# Time to pause between SBADataProcessing programs, in seconds
pause <- 5

# Local-------------------------------------------
# # Directory of CTD files to process
# dir.CTD <- 'C:\\SURVEY\\2307SH\\DATA\\CTD\\CTD_to_Process\\'
# # Directory to store processed data results
# dir.output <- 'C:\\SURVEY\\2307SH\\DATA\\CTD\\PROCESSED\\'
# # Directory containing SBEDataProcessing Program Setup (.psa) files
# dir.PSA <- paste0(normalizePath(file.path(getwd(), 'CODE/PSA/')),'\\')
# # # CTD configuration file
# # file.con <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20230703_SHIMADA_SummerHake\\DATA\\CTD\\_2307SH.XMLCON'
# # Directory of Seabird SBEDataProcessing programs
# dir.SBE <- 'C:\\Program Files (x86)\\Sea-Bird\\SBEDataProcessing-Win32\\'
# # Template ECS file
# ECS.template <- 'C:\\SURVEY\\2307SH\\PROCESSED\\EV\\ECS\\_2307SH_Template.ecs'
# # ECS output directory
# dir.ECS <- 'C:\\SURVEY\\2307SH\\PROCESSED\\EV\\ECS\\'
# # Time to pause between SBADataProcessing programs, in seconds
# pause <- 2

# Process CTD data --------------------------------------------------------

# Find all raw data files in CTD directory
files.CTD <- list.files(path = dir.CTD, pattern = "*.hex")

# Loop through each file
for (i in files.CTD) {
  
  # Retain just the file name (i.e., remove extension)
  file.name <- tools::file_path_sans_ext(i)
  
  # Use DatCnv to convert from .hex to .cnv
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DatCnvW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
                 paste(dir.CTD, file.name, '.hex', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'DatCnv.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause + 2)
  
  # Perform Filter
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'FilterW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Filter.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Loop Edit
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'LoopEditW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'LoopEdit.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Derive to obtain depth
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'DeriveDepth.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Bin Average to average into 1-m depth cells
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'BinAvgW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'BinAvg.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Derive to obtain salinity, sound speed, average sound speed, and
  # density
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Derive.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform ASCII Out
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'ASCII_OutW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '_processed.asc', sep = ''),
                 paste(dir.PSA, 'AsciiOut.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform SeaPlot to generate plot of CTD profile
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'SeaPlotW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.jpg', sep = ''),
                 paste(dir.PSA, 'SeaPlot.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Load results from processed CTD data
  data <- read.csv(paste(dir.output, file.name, '_processed.asc', sep = ''), 
                   header = T, sep = "\t")
  
  # Perform basic data error checks
  idx <- data$DepSM < 0 | data$T090C <= 0 | data$Sal00 < 0
  data[idx,] <- NA
  
  # For CPS, take the average sound velocity at 70 m then calculate the average
  # temperature, salinity, and depth
  idx <- data$DepSM <= 70
  avgSoundSpeed.CPS <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.CPS <- mean(data$T090C[idx], na.rm = T)
  avgSalinity.CPS <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.CPS <- mean(data$DepSM[idx], na.rm = T)
  
  # For krill, take the average sound velocity at 350 m then calculate the
  # average temperature, salinity, and depth
  idx <- data$DepSM <= 350
  avgSoundSpeed.Krill <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.Krill <- mean(data$T090C[idx], na.rm = T)
  avgSalinity.Krill <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.Krill <- mean(data$DepSM[idx], na.rm = T)
  
  
  
  # Create ECS file ---------------------------------------------------------
  
  # Read template ECS file
  ECS <- read_file(ECS.template)
  
  # Get sound speed from template
  c_0 <- as.numeric(str_match(ECS, "SoundSpeed\\s*=\\s*([^\\s]+)")[,2])
  
  # Get calibration parameters that can be adjusted with sound speed
  g_0 <- as.numeric(str_match_all(ECS, "TransducerGain\\s*=\\s*([^\\s]+)")[[1]][,2])
  EBA_0 <- as.numeric(str_match_all(ECS, "TwoWayBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  BW_minor_0 <- as.numeric(str_match_all(ECS, "MinorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  BW_major_0 <- as.numeric(str_match_all(ECS, "MajorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  
  # Create CPS and Krill-specific ECS files
  ECS.CPS <- ECS
  ECS.Krill <- ECS
  
  # For both CPS and Krill, replace the calibration parameters by compensating
  # for changes in sound speed
  T_vars = c(1,2)
  for (j in 1:length(g_0)){
    
    # Compensate gain
    pattern <- paste("(?s)SourceCal T1 \\(channel ", T_vars[j], 
                     ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(g_0[j] + 20*log10(c_0 / avgSoundSpeed.CPS), fmt = '%#.4f'),
                      str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                        sprintf(g_0[j] + 20*log10(c_0 / avgSoundSpeed.Krill), fmt = '%#.4f'),
                        str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate EBA
    pattern <- paste("(?s)SourceCal T1 \\(channel ", T_vars[j], 
                     ".*?TwoWayBeamAngle\\s*=\\s*(-\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(EBA_0[j] + 20*log10(avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                      str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                        sprintf(EBA_0[j] + 20*log10(avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                        str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate Alongship (Minor) Beamwidth
    pattern <- paste("(?s)SourceCal T1 \\(channel ", T_vars[j], 
                     ".*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_minor_0[j] * (avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                      str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                        sprintf(BW_minor_0[j] * (avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                        str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate Athwarthip (Major) Beamwidth
    pattern <- paste("(?s)SourceCal T1 \\(channel ", T_vars[j], 
                     ".*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_major_0[j] * (avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                      str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                        sprintf(BW_major_0[j] * (avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                        str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
  }
  
  # Replace the sound speed
  ECS.CPS <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
                  sprintf('SoundSpeed = %.2f ', avgSoundSpeed.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
                    sprintf('SoundSpeed = %.2f ', avgSoundSpeed.Krill),
                    ECS.Krill)
  
  # Replace the temperature
  ECS.CPS <- gsub('Temperature\\s*=\\s*[^#]*', 
                  sprintf('Temperature = %.3f ', avgTemperature.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Temperature\\s*=\\s*[^#]*', 
                    sprintf('Temperature = %.3f ', avgTemperature.Krill),
                    ECS.Krill)
  
  # Replace the salinity
  ECS.CPS <- gsub('Salinity\\s*=\\s*[^#]*', 
                  sprintf('Salinity = %.3f ', avgSalinity.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Salinity\\s*=\\s*[^#]*', 
                    sprintf('Salinity = %.3f ', avgSalinity.Krill),
                    ECS.Krill)
  
  # Replace the average absorption depth
  ECS.CPS <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                  sprintf('AbsorptionDepth = %.3f ', avgDepth.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                    sprintf('AbsorptionDepth = %.3f ', avgDepth.Krill),
                    ECS.Krill)
  
  # Write new ECS files
  # write_file(ECS, paste(dir.ECS, file.name, "_CPS.ecs", sep = ''))
  write_file(ECS.CPS, paste(dir.ECS, file.name, "_CPS.ecs", sep = ''))
  write_file(ECS.Krill, paste(dir.ECS, file.name, "_Krill.ecs", sep = ''))
  
  # Write simple text file describing differences in CPS and Krill sound speeds
  # and the ratio to use for adjusting the Integration Stop line in Echoview
  tmp <- paste(sprintf('CPS average sound speed = %.2f m/s\n', avgSoundSpeed.CPS),
               sprintf('Krill average sound speed = %.2f m/s\n', avgSoundSpeed.Krill),
               sprintf('Krill/CPS sound speed ratio = %.6f', avgSoundSpeed.Krill/avgSoundSpeed.CPS),
               sep = '')
  write_file(tmp, paste(dir.output, file.name, '_SoundSpeedRatio.txt', sep = ''))
  
  # Copy CTD file to the PROCESSED directory
  file.copy(file.path(dir.CTD, i), dir.output)
}
