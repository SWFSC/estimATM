# =====================================================================
# MVP TO ECHOVIEW ECS PIPELINE
# Ingests raw .m1 casts, computes TEOS-10 physics, and outputs strictly 
# .ecs calibration files by compensating a base ECS template.
# =====================================================================

# Load required packages ----------------------------------------------
library(readr)    # For reading/writing plain text files
library(stringr)  # For processing strings and regex
library(dplyr)    # For vector/dataframe manipulation and binning
library(gsw)      # For TEOS-10 oceanographic mathematical models

# =====================================================================
# SECTION 1: HELPER FUNCTIONS & MATH ENGINES
# =====================================================================

calculate_fg_absorption <- function(T, S, D, f) {
  T_K <- T + 273.15
  c <- 1412.0 + 3.21 * T + 1.19 * S + 0.0167 * D
  
  f1 <- 2.8 * sqrt (S/ 35.0) * 10^(4.0-1245.0 / T_K)
  A1 <- (8.86 / c) * 10^(0.78* 8.0 - 5.0)
  
  f2 <- (21.9 * 10^(6.0 - 1960.0 / T_K)) / (1.0 + 10^(3.0 - 0.03 * T))
  A2 <- 21.4 * (S / 35.0) * (1.0 + 0.025 * T)/c
  P2 <- 1.0 - 1.37e-4 * D + 6.2e-9 * D^2
  
  A3_low_temp <- 4.937e-4 - 2.59e-5 * T + 9.11e-7 * T^2 - 1.5e-8 * T^3
  A3_high_temp <- 1.809e-4 - 5.64e-6 * T + 6.88e-8 * T^2
  A3 <- ifelse(T <= 20, A3_low_temp, A3_high_temp)
  P3 <- 1.0 - 3.83e-5 * D + 4.9e-9 * D^2
  
  alpha <- ((A1 * f1 * f^2) / (f^2 + f1^2)) + ((A2 * P2 * f2 * f^2) / (f^2 + f2^2)) + (A3 * P3 * f^2)
  return(alpha)
}

calculate_depth_integrated_average <- function(df, value_col, depth_col = 'Depth') {
  # Trapezoidal Integration Engine
  df_clean <- df[!is.na(df[[value_col]]) & !is.na(df[[depth_col]]), ]
  df_sorted <- df_clean[order(df_clean[[depth_col]]), ]
  
  if (nrow(df_sorted) == 0) return(0.0)
  
  z <- df_sorted[[depth_col]]
  val <- df_sorted[[value_col]]
  
  dz <- diff(z)
  if (length(dz) == 0) return(val[1])
  
  mid_vals <- (val[-length(val)] + val[-1]) / 2.0
  total_integral <- sum(mid_vals * dz)
  total_depth_range <- z[length(z)] - z[1]
  
  if (total_depth_range > 0) {
    return(total_integral / total_depth_range)
  } else {
    return(val[1])
  }
}
  
parse_mvp_coordinate <- function(coord_str) {
  # Decodes custom geographic tracking strings from MVP file headers.
  if (is.null(coord_str) || is.na(coord_str) || coord_str == "") return(NA)
  
  clean_str <- trimws(coord_str)
  match <- stringr::str_match(clean_str, "([0-9.]+),([NSEW])")
  if (is.na(match[1,1])) return(NA)
  
  val <- as.numeric(match[1, 2])
  direction <- match[1, 3]
  
  degrees <- floor(val / 100)
  minutes <- val - (degrees * 100)
  decimal_degrees <- degrees + (minutes / 60.0)
  
  if (direction %in% c('S', 'W')) decimal_degrees <- -decimal_degrees
  return(decimal_degrees)
}

# =====================================================================
# SECTION 2: DATA INGESTION & TEOS-10 PROCESSING
# =====================================================================

process_mvp_file <- function(file_path, sampling_rate_hz = 25.0) {
  # 1. Parse header and isolate data payload
  lines <- readLines(file_path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  header_end_idx <- which(grepl("<END_OF_HEADER>", lines))
  metadata <- list()
  
  if (length(header_end_idx) > 0 && header_end_idx[1] > 1) {
    header_lines <- lines[1:(header_end_idx[1] - 1)]
    for (line in header_lines) {
      if (grepl(":", line)) {
        parts <- str_split_fixed(line, ":", 2)
        metadata[[trimws(parts[1])]] <- trimws(parts[2])
      }
    }
    
    data_lines <- lines[(header_end_idx[1] + 1):length(lines)]
    
    # Safely remove the units row by checking if the second line contains 'dbar' or 'm'
    if (length(data_lines) > 1 && grepl("dbar|m/s", data_lines[2])) {
      data_to_read <- c(data_lines[1], data_lines[3:length(data_lines)])
    } else {
      data_to_read <- data_lines
    }
    
    df <- read.csv(text = paste(data_to_read, collapse = "\n"), header = TRUE, check.names = FALSE, strip.white = TRUE)
    names(df) <- trimws(names(df))
  } else {
    cat("   -> Failed: Could not locate <END_OF_HEADER> tag.\n")
    return(NULL)
  }
  
  # Safety Check: Did the columns parse correctly?
  if (nrow(df) == 0 || !"Depth" %in% names(df)) {
    cat("   -> Failed: 'Depth' column missing or file is empty.\n")
    return(NULL) 
  }
  
  # 2. Isolate free-fall descent over 0.5 m/s
  dt <- 1.0 / sampling_rate_hz
  max_depth_idx <- which.max(df$Depth)
  
  # EXPLICITLY CREATE descent_df
  descent_df <- df[1:max_depth_idx, ]
  
  if (nrow(descent_df) == 0) {
    cat("   -> Failed: Descent data array is empty.\n")
    return(NULL)
  }
  
  descent_df$Descent_Speed_m_s <- c(NA, diff(descent_df$Depth)) / dt
  if (nrow(descent_df) > 1) descent_df$Descent_Speed_m_s[1] <- descent_df$Descent_Speed_m_s[2]
  
  cleaned_df <- descent_df[!is.na(descent_df$Descent_Speed_m_s) & descent_df$Descent_Speed_m_s > 0.5, ]
  if (nrow(cleaned_df) == 0) {
    cat("   -> Failed: No data left after filtering for 0.5 m/s descent speed.\n")
    return(NULL)
  }
  
  # 3. Compute TEOS-10 properties and Multi-Frequency Arrays
  lat_raw <- metadata[["LAT ( ddmm.mmmmmmm,N)"]]
  lon_raw <- metadata[["LON (dddmm.mmmmmmm,E)"]]
  lat <- ifelse(is.na(parse_mvp_coordinate(lat_raw)), 32.7221, parse_mvp_coordinate(lat_raw))
  lon <- ifelse(is.na(parse_mvp_coordinate(lon_raw)), -117.4006, parse_mvp_coordinate(lon_raw))
  
  cleaned_df$Absolute_Salinity <- gsw::gsw_SA_from_SP(cleaned_df$Sal, cleaned_df$Press, lon, lat)
  cleaned_df$Conservative_Temperature <- gsw::gsw_CT_from_t(cleaned_df$Absolute_Salinity, cleaned_df$Temp, cleaned_df$Press)
  cleaned_df$Potential_Density_Anomaly <- gsw::gsw_sigma0(cleaned_df$Absolute_Salinity, cleaned_df$Conservative_Temperature)
  cleaned_df$Sound_Speed_m_s <- gsw::gsw_sound_speed(cleaned_df$Absolute_Salinity, cleaned_df$Conservative_Temperature, cleaned_df$Press)
  
  cleaned_df$Alpha_18kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 18.0)
  cleaned_df$Alpha_38kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 38.0)
  cleaned_df$Alpha_70kHz  <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 70.0)
  cleaned_df$Alpha_120kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 120.0)
  cleaned_df$Alpha_200kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 200.0)
  cleaned_df$Alpha_333kHz <- calculate_fg_absorption(cleaned_df$Temp, cleaned_df$Sal, cleaned_df$Depth, 333.0)
  
  # 4. Bin into uniform 1-meter layers
  min_b <- floor(min(cleaned_df$Depth, na.rm = TRUE))
  max_b <- ceiling(max(cleaned_df$Depth, na.rm = TRUE))
  bin_edges <- seq(min_b, max_b + 1.0, by = 1.0)
  bin_labels <- bin_edges[-length(bin_edges)] + 0.5
  
  cleaned_df$Depth_Bin <- cut(cleaned_df$Depth, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE)
  
  binned <- cleaned_df %>%
    group_by(Depth_Bin) %>%
    summarise(
      Temp_Conservative_C = mean(Conservative_Temperature, na.rm = TRUE),
      Salinity_Absolute_g_kg = mean(Absolute_Salinity, na.rm = TRUE),
      Density_SigmaTheta_kg_m3 = mean(Potential_Density_Anomaly, na.rm = TRUE),
      Sound_Speed_m_s = mean(Sound_Speed_m_s, na.rm = TRUE),
      Absorption_18kHz_dB_km = mean(Alpha_18kHz, na.rm = TRUE),
      Absorption_38kHz_dB_km = mean(Alpha_38kHz, na.rm = TRUE),
      Absorption_70kHz_dB_km = mean(Alpha_70kHz, na.rm = TRUE),
      Absorption_120kHz_dB_km = mean(Alpha_120kHz, na.rm = TRUE),
      Absorption_200kHz_dB_km = mean(Alpha_200kHz, na.rm = TRUE),
      Absorption_333kHz_dB_km = mean(Alpha_333kHz, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(Sound_Speed_m_s))
  
  binned$Depth_m <- as.numeric(as.character(binned$Depth_Bin))
  
  return(list(raw_descent = cleaned_df, binned = binned, meta = metadata))
}

# =====================================================================
# SECTION 3: ECHOVIEW .ECS EXPORT ENGINE
# =====================================================================

export_template_ecs <- function(raw_descent_df, binned_df, cast_id, output_dir, template_path, tx_depth = 2) {
  
  # 1. Calculate integrated volumetric averages for the file
  spec_temp <- calculate_depth_integrated_average(raw_descent_df, 'Conservative_Temperature')
  spec_sal  <- calculate_depth_integrated_average(raw_descent_df, 'Absolute_Salinity')
  spec_dens <- calculate_depth_integrated_average(raw_descent_df, 'Potential_Density_Anomaly')
  spec_sv   <- calculate_depth_integrated_average(raw_descent_df, 'Sound_Speed_m_s')
  spec_depth <- mean(raw_descent_df$Depth, na.rm = TRUE)
  
  spec_a18  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_18kHz')
  spec_a38  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_38kHz')
  spec_a70  <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_70kHz')
  spec_a120 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_120kHz')
  spec_a200 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_200kHz')
  spec_a333 <- calculate_depth_integrated_average(raw_descent_df, 'Alpha_333kHz')
  
  # 2. Read Base Template
  ECS <- read_file(template_path)
  
  # 3. Extract nominal parameters from template to serve as baseline
  c_0 <- as.numeric(str_match(ECS, "\\bSoundSpeed\\s*=\\s*([^\\s]+)")[,2])
  g_0 <- as.numeric(str_match_all(ECS, "\\bTransducerGain\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  EBA_0 <- as.numeric(str_match_all(ECS, "\\bTwoWayBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  BW_minor_0 <- as.numeric(str_match_all(ECS, "\\bMinorAxis3dbBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  BW_major_0 <- as.numeric(str_match_all(ECS, "\\bMajorAxis3dbBeamAngle\\s*=\\s*(-?\\d*\\.?\\d+)")[[1]][,2])
  
  # 4. Find Transducer Sound Speed for Parameter Compensation
  idx <- which.min(abs(raw_descent_df$Depth - tx_depth))
  txdcr_c <- raw_descent_df$Sound_Speed_m_s[idx]
  if(is.na(c_0) || length(c_0) == 0) c_0 <- txdcr_c 
  
  ECS_new <- ECS
  
  # 5. Compensate Gain and Beam Angles
  if (length(g_0) > 0) {
    for (j in 1:length(g_0)) {
      
      pattern <- paste("(?s)SourceCal T", j, 
                       ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
      
      # Gain Compensation
      pattern <- paste("(?s)SourceCal T", j, ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', g_0[j] + 20*log10(c_0 / txdcr_c)),
                          str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }
      
      # EBA Compensation
      pattern <- paste0("(?s)SourceCal T", j, "*?TwoWayBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', EBA_0[j] + 20*log10(txdcr_c / c_0)),
                          str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }
      
      # Minor BW Compensation
      pattern <- paste0("(?s)SourceCal T", j, ".*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', BW_minor_0[j] * (txdcr_c / c_0)),
                          str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }
      
      # Major BW Compensation
      pattern <- paste0("(?s)SourceCal T", j, ".*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)")
      temp <- regexec(pattern, ECS_new, perl = TRUE)
      if (temp[[1]][1] != -1) {
        ECS_new <- paste0(str_sub(ECS_new, 1, temp[[1]][2]-1),
                          sprintf('%#.4f', BW_major_0[j] * (txdcr_c / c_0)),
                          str_sub(ECS_new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
      }
    }
  }
  
  # 6. Inject FILESET profiles and averages
  ECS_new <- gsub('\\bCtdDepthProfile\\s*=\\s*[^#\n]*', paste('CtdDepthProfile = ', paste(sprintf("%.2f", binned_df$Depth_m), collapse = ';'), ' '), ECS_new)
  ECS_new <- gsub('\\bSoundSpeedProfile\\s*=\\s*[^#\n]*', paste('SoundSpeedProfile = ', paste(sprintf("%.2f", binned_df$Sound_Speed_m_s), collapse = ';'), ' '), ECS_new)
  
  ECS_new <- gsub('\\bTemperature\\s*=\\s*[^#\n]*', sprintf('Temperature = %.2f ', spec_temp), ECS_new)
  ECS_new <- gsub('\\bSalinity\\s*=\\s*[^#\n]*', sprintf('Salinity = %.2f ', spec_sal), ECS_new)
  ECS_new <- gsub('\\bAbsorptionDepth\\s*=\\s*[^#\n]*', sprintf('AbsorptionDepth = %.2f ', spec_depth), ECS_new)
  ECS_new <- gsub('\\bSoundSpeed\\s*=\\s*[^#\n]*', sprintf('SoundSpeed = %.2f ', spec_sv), ECS_new)
  
  # 7. Create Comment Header Block for Acoustic Variables
  summary_txt <- paste0(
    "# =====================================================================#\n",
    "# COMPENSATED WATER COLUMN VOLUMETRIC AVERAGES (TRAPEZOIDAL INTEGRATION):\n",
    sprintf("# Avg Temperature (CT):            %.4f C\n", spec_temp),
    sprintf("# Avg Salinity (SA):               %.4f g/kg\n", spec_sal),
    sprintf("# Avg Density Anomaly (sigma0):    %.4f kg/m3\n", spec_dens),
    sprintf("# Avg Sound Speed (TEOS10):        %.4f m/s\n", spec_sv),
    sprintf("# Avg Acoustic Absorption 18kHz:   %.4f dB/km\n", spec_a18),
    sprintf("# Avg Acoustic Absorption 38kHz:   %.4f dB/km\n", spec_a38),
    sprintf("# Avg Acoustic Absorption 70kHz:   %.4f dB/km\n", spec_a70),
    sprintf("# Avg Acoustic Absorption 120kHz:  %.4f dB/km\n", spec_a120),
    sprintf("# Avg Acoustic Absorption 200kHz:  %.4f dB/km\n", spec_a200),
    sprintf("# Avg Acoustic Absorption 333kHz:  %.4f dB/km\n", spec_a333),
    "# =====================================================================#\n\n",
    "#========================================================================================#\n",
    "#                                    FILESET SETTINGS                                    #\n"
  )
  
  # Inject the summary block directly above the FILESET SETTINGS banner
  target_anchor <- "#========================================================================================#\r?\n#                                    FILESET SETTINGS                                    #\r?\n"
  ECS_final <- sub(target_anchor, summary_txt, ECS_new)
  
  # 8. Write the final ECS file
  ecs_filename <- paste0(cast_id, ".ecs")
  write_file(ECS_final, file.path(output_dir, ecs_filename))
  cat(sprintf("     -> Created Template .ecs: '%s'\n", ecs_filename))
}


# =====================================================================
# SECTION 4: RUNTIME WORKFLOW ROUTINE
# =====================================================================

if (!interactive() || sys.nframe() == 0) {
  
  # ---------------- USER SETTINGS ----------------
  DATA_DIR <- "Y:\\2606RL\\Cruise Data\\RAW\\MVP\\ToProcess"
  OUTPUT_DIR <- "C:\\SURVEY\\2606RL\\PROCESSED\\EV\\ECS"
  ECS_TEMPLATE <- "C:\\SURVEY\\2606RL\\PROCESSED\\EV\\ECS\\_2606RL_Template.ecs"
  TX_DEPTH <- 7.35  
  # -----------------------------------------------
  
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  m1_files <- list.files(path = DATA_DIR, pattern = "\\.m1$", full.names = TRUE)
  
  cat(sprintf("Starting Pipeline. Found %d MVP profiles...\n", length(m1_files)))
  
  for (file_path in m1_files) {
    cast_id <- tools::file_path_sans_ext(basename(file_path))
    cat(sprintf("\nProcessing MVP Cast: %s\n", cast_id))
    
    processed_data <- process_mvp_file(file_path)
    
    if (is.null(processed_data)) {
      next
    }
    
    # Run the genuine ECS Template Export
    export_template_ecs(
      raw_descent_df = processed_data$raw_descent, 
      binned_df = processed_data$binned, 
      cast_id = cast_id, 
      output_dir = OUTPUT_DIR,
      template_path = ECS_TEMPLATE,
      tx_depth = TX_DEPTH
    )
  }
  
  cat("\nProcessing complete.\n")
}
