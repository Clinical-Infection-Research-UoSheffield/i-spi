looks_like_file_path <- function(x) {
  # TRUE if:
  # - contains a directory separator ("/" for Unix/Mac, "\\" for Windows), OR
  # - starts with a drive letter on Windows (e.g. "C:/"), AND
  # - ends with a file extension (e.g. ".csv", ".txt")
  (grepl("[/\\\\]", x) | grepl("^[A-Za-z]:[\\\\/]", x)) ##&& grepl("\\.[A-Za-z0-9]+$", x)
}

# Validate the RP1 Volts and RP1 Target
# Only one decimal point is allowed
check_rp1_numeric <- function(x) {
  grepl("^\\d+(\\.\\d+)?$", x)
}

## Validate Time is in correct format to store in database
#DD-MMM-YYYY, HH:MM AM/PM
check_time_format <- function(x) {
  grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}, \\d{2}:\\d{2} (AM|PM)$", x)
}

# Capitalize AM /PM
capitalize_am_pm <- function(text) {
  matches <- gregexpr("\\b(am|pm)\\b", text, ignore.case = TRUE, perl = TRUE)
  regmatches(text, matches) <- lapply(regmatches(text, matches), toupper)
  return(text)
}

#' Normalize acquisition date strings to the database format DD-MMM-YYYY, HH:MM AM/PM
#'
#' Accepts a wide range of date/time formats from North American and European
#' instruments. Handles non-English month abbreviations (Dutch, German, French,
#' Spanish, Italian, Portuguese) by mapping them to English before parsing.
#'
#' @param x Character vector of date strings
#' @return Character vector in the format "DD-MMM-YYYY, HH:MM AM/PM",
#'         or the original value if parsing fails (so validation can flag it)
normalize_acquisition_date <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val))) return(NA_character_)

    original <- val
    val <- trimws(val)

    # Already in target format? Return as-is.
    val_check <- capitalize_am_pm(val)
    if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}, \\d{2}:\\d{2} (AM|PM)$", val_check)) {
      return(val_check)
    }

    # ------------------------------------------------------------------
    # Map non-English month abbreviations to English
    # ------------------------------------------------------------------
    month_map <- c(
      # Dutch
      "jan" = "Jan", "feb" = "Feb", "mrt" = "Mar", "apr" = "Apr",
      "mei" = "May", "jun" = "Jun", "jul" = "Jul", "aug" = "Aug",
      "sep" = "Sep", "okt" = "Oct", "nov" = "Nov", "dec" = "Dec",
      # German
      "m\u00e4r" = "Mar", "mae" = "Mar", "mai" = "May",
      "dez" = "Dec",
      # French
      "f\u00e9v" = "Feb", "fev" = "Feb", "mar" = "Mar", "avr" = "Apr",
      "jui" = "Jun", "juil" = "Jul", "ao\u00fb" = "Aug", "aou" = "Aug",
      # Spanish
      "ene" = "Jan", "abr" = "Apr", "ago" = "Aug", "dic" = "Dec",
      # Italian
      "gen" = "Jan", "mag" = "May", "giu" = "Jun", "lug" = "Jul",
      "set" = "Sep", "ott" = "Oct",
      # Portuguese
      "fev" = "Feb", "mar" = "Mar", "abr" = "Apr", "mai" = "May",
      "ago" = "Aug", "out" = "Oct", "dez" = "Dec"
    )

    val_lower <- val
    for (foreign in names(month_map)) {
      pattern <- paste0("(?i)\\b", foreign, "\\b")
      if (grepl(pattern, val_lower, perl = TRUE)) {
        val_lower <- sub(pattern, month_map[[foreign]], val_lower, perl = TRUE)
        break
      }
    }
    val <- val_lower

    # ------------------------------------------------------------------
    # Try parsing with many common format strings
    # Order matters: more specific formats first
    # ------------------------------------------------------------------
    formats <- c(
      # Target format (with comma)
      "%d-%b-%Y, %I:%M %p",
      "%d-%b-%Y, %H:%M",
      # Variations without comma
      "%d-%b-%Y %I:%M %p",
      "%d-%b-%Y %H:%M",
      "%d-%b-%Y %H:%M:%S",
      # US: M/D/YYYY with 12-hour
      "%m/%d/%Y %I:%M %p",
      "%m/%d/%Y %I:%M:%S %p",
      # US: M/D/YYYY with 24-hour
      "%m/%d/%Y %H:%M",
      "%m/%d/%Y %H:%M:%S",
      # European: D/M/YYYY
      "%d/%m/%Y %H:%M",
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %I:%M %p",
      # Dot-separated (common in Germany, Netherlands)
      "%d.%m.%Y %H:%M",
      "%d.%m.%Y %H:%M:%S",
      "%d.%m.%Y %I:%M %p",
      # Dash-separated numeric
      "%d-%m-%Y %H:%M",
      "%d-%m-%Y %H:%M:%S",
      "%d-%m-%Y %I:%M %p",
      "%m-%d-%Y %I:%M %p",
      "%m-%d-%Y %H:%M",
      "%m-%d-%Y %H:%M:%S",
      # ISO
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M",
      # 2-digit year formats (common in ELISA instruments)
      "%y-%m-%d %H:%M:%S",
      "%y-%m-%d %H:%M",
      "%d-%m-%y %H:%M:%S",
      "%d-%m-%y %H:%M",
      "%m-%d-%y %H:%M:%S",
      "%m-%d-%y %H:%M",
      "%d/%m/%y %H:%M:%S",
      "%d/%m/%y %H:%M",
      "%m/%d/%y %H:%M:%S",
      "%m/%d/%y %H:%M",
      "%y/%m/%d %H:%M:%S",
      "%y/%m/%d %H:%M",
      # Date-only (assume midnight)
      "%d-%b-%Y",
      "%m/%d/%Y",
      "%d/%m/%Y",
      "%Y-%m-%d",
      "%d.%m.%Y",
      "%y-%m-%d",
      "%d-%m-%y",
      "%m/%d/%y"
    )

    parsed <- NA
    for (fmt in formats) {
      attempt <- tryCatch(
        as.POSIXct(val, format = fmt, tz = "UTC"),
        error = function(e) NA
      )
      if (!is.na(attempt)) {
        # Sanity check: year should be between 1990 and 2100
        yr <- as.integer(format(attempt, "%Y"))
        if (!is.na(yr) && yr >= 1990 && yr <= 2100) {
          parsed <- attempt
          break
        }
      }
    }

    # For ambiguous M/D vs D/M: if the first US parse gave a future year
    # or implausible month, try European interpretation
    if (is.na(parsed)) {
      return(original)
    }

    # Format to target: DD-MMM-YYYY, HH:MM AM/PM
    format(parsed, "%d-%b-%Y, %I:%M %p")
  }, character(1), USE.NAMES = FALSE)
}

#' Standardize acquisition dates for PostgreSQL insertion
#'
#' Parses dates in any supported format (including non-English months,
#' 2-digit years, and various international formats) and outputs them
#' in ISO 8601 format "YYYY-MM-DD HH:MM:SS" which PostgreSQL always accepts.
#'
#' This should be called on acquisition_date columns BEFORE database insertion
#' for both ELISA and bead array upload paths.
#'
#' @param x Character vector of date strings
#' @return Character vector in "YYYY-MM-DD HH:MM:SS" format
#'
standardize_date_for_postgres <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val))) return(NA_character_)

    val <- trimws(val)

    # ---- Map non-English month abbreviations to English ----
    month_map <- c(
      # Dutch
      "jan" = "Jan", "feb" = "Feb", "mrt" = "Mar", "apr" = "Apr",
      "mei" = "May", "jun" = "Jun", "jul" = "Jul", "aug" = "Aug",
      "sep" = "Sep", "okt" = "Oct", "nov" = "Nov", "dec" = "Dec",
      # German
      "m\u00e4r" = "Mar", "mae" = "Mar", "mai" = "May", "dez" = "Dec",
      # French
      "f\u00e9v" = "Feb", "fev" = "Feb", "mar" = "Mar", "avr" = "Apr",
      "jui" = "Jun", "juil" = "Jul", "ao\u00fb" = "Aug", "aou" = "Aug",
      # Spanish
      "ene" = "Jan", "abr" = "Apr", "ago" = "Aug", "dic" = "Dec",
      # Italian
      "gen" = "Jan", "mag" = "May", "giu" = "Jun", "lug" = "Jul",
      "set" = "Sep", "ott" = "Oct",
      # Portuguese
      "out" = "Oct"
    )

    for (foreign in names(month_map)) {
      pattern <- paste0("(?i)\\b", foreign, "\\b")
      if (grepl(pattern, val, perl = TRUE)) {
        val <- sub(pattern, month_map[[foreign]], val, perl = TRUE)
        break
      }
    }

    # Capitalize AM/PM
    val <- gsub("\\bam\\b", "AM", val, ignore.case = TRUE)
    val <- gsub("\\bpm\\b", "PM", val, ignore.case = TRUE)

    # ---- Try parsing with many common format strings ----
    formats <- c(
      # DD-MMM-YYYY formats (from normalize_acquisition_date output)
      "%d-%b-%Y, %I:%M %p",
      "%d-%b-%Y, %H:%M",
      "%d-%b-%Y %I:%M %p",
      "%d-%b-%Y %H:%M",
      "%d-%b-%Y %H:%M:%S",
      # US: M/D/YYYY
      "%m/%d/%Y %I:%M %p",
      "%m/%d/%Y %I:%M:%S %p",
      "%m/%d/%Y %H:%M",
      "%m/%d/%Y %H:%M:%S",
      # European: D/M/YYYY
      "%d/%m/%Y %H:%M",
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %I:%M %p",
      # Dot-separated
      "%d.%m.%Y %H:%M",
      "%d.%m.%Y %H:%M:%S",
      "%d.%m.%Y %I:%M %p",
      # Dash-separated numeric 4-digit year
      "%d-%m-%Y %H:%M",
      "%d-%m-%Y %H:%M:%S",
      "%d-%m-%Y %I:%M %p",
      "%m-%d-%Y %I:%M %p",
      "%m-%d-%Y %H:%M",
      "%m-%d-%Y %H:%M:%S",
      # ISO 4-digit year
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M",
      # 2-digit year formats (common in ELISA instruments)
      "%y-%m-%d %H:%M:%S",
      "%y-%m-%d %H:%M",
      "%d-%m-%y %H:%M:%S",
      "%d-%m-%y %H:%M",
      "%m-%d-%y %H:%M:%S",
      "%m-%d-%y %H:%M",
      "%d/%m/%y %H:%M:%S",
      "%d/%m/%y %H:%M",
      "%m/%d/%y %H:%M:%S",
      "%m/%d/%y %H:%M",
      "%y/%m/%d %H:%M:%S",
      "%y/%m/%d %H:%M",
      # Date-only (assume midnight)
      "%d-%b-%Y",
      "%m/%d/%Y",
      "%d/%m/%Y",
      "%Y-%m-%d",
      "%d.%m.%Y",
      "%y-%m-%d",
      "%d-%m-%y",
      "%m/%d/%y"
    )

    parsed <- NA
    for (fmt in formats) {
      attempt <- tryCatch(
        as.POSIXct(val, format = fmt, tz = "UTC"),
        error = function(e) NA
      )
      if (!is.na(attempt)) {
        yr <- as.integer(format(attempt, "%Y"))
        if (!is.na(yr) && yr >= 1990 && yr <= 2100) {
          parsed <- attempt
          break
        }
      }
    }

    if (is.na(parsed)) {
      # Could not parse - return original and let PostgreSQL try
      cat("    ⚠ Could not parse date for DB:", val, "\n")
      return(val)
    }

    # Output ISO 8601 format that PostgreSQL always accepts
    format(parsed, "%Y-%m-%d %H:%M:%S")
  }, character(1), USE.NAMES = FALSE)
}


# Type column must be in correct format
check_type_column <- function(df) {
bad_rows <- df[!grepl("^[BXCS][0-9]*$", df$Type), ]

if (nrow(bad_rows) > 0) {
  message <-  paste(
    "Need to correct the type column for the following entries: Well",
    paste(bad_rows$Well, "| Value:", bad_rows$Type, collapse = ", ")
  )
  #cat("need to correct type column for the following entries:", paste(bad_rows$Type, collapse = ","))
  return(list(FALSE, message))
} else {
  return(list(TRUE))
}

}

# if false procceed
check_blank_in_sample_boolean <- function(df) {
bad_rows <- which(
  grepl("Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)
)

if (length(bad_rows) > 0) {
  #cat("Upload blocked: Found 'Blank' in description with type starting not with 'B'.\n")
  return(FALSE)
} else {
  return(TRUE)
}
}

# check general description pattern for datatypes
check_sample_description <- function(df) {

 # check description pattern for the samples
  df <- df[grepl("^X", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("[A-Za-z0-9]+[ _/\\\\:;|\\-][A-Za-z0-9]+[ _/\\\\:;|\\-][A-Za-z0-9]+", df$Description), ]
  # df <- df[grepl("^X", df$Type) & !grepl("^\\[A-Za-z0-9_]+_[A-Za-z0-9_]+_\\[A-Za-z0-9_]+$", df$Description), ]
  if (nrow(df) > 0) {
     sample_message <- paste(
      "Need to modify the Sample description column to include a minimum of [ID]_[timeperiod]_[dilution_factor]: Well",
      paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
     return(list(FALSE, sample_message))
  } else {
    return(TRUE)
  }
}

check_standard_description <- function(df) {
  # check description pattern for the standards
  df <- df[grepl("^S", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9_]+_\\d+$", df$Description), ]
  if (nrow(df) > 0) {
    standards_message <- paste(
      "Need to modify the Standard description column to be [source]_[dilution_factor] e.g. NIBSC_40: Well",
      paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
    return(list(FALSE, standards_message))
  } else {
    return(TRUE)
  }
}



validate_batch_plate_metadata <- function(plate_metadata, plate_id_data) {
  cat("\n>>> ENTERING validate_batch_plate_metadata <<<\n")
  cat("plate_metadata rows:", nrow(plate_metadata), "\n")
  cat("plate_id_data rows:", nrow(plate_id_data), "\n")

  message_list <- c()

  # Check if uploaded files are in layout
  check_uploaded_file_in_layout <- plate_metadata$file_name %in% plate_id_data$plate_filename
  if (!all(check_uploaded_file_in_layout)) {
    missing_files <- plate_metadata$file_name[!check_uploaded_file_in_layout]
    message_list <- c(message_list, paste0(
      "LAYOUT MISMATCH: The following uploaded plate files are not listed in the layout file:\n",
      paste("  - ", missing_files, collapse = "\n"),
      "\n\nPlease add these files to the 'plate_id' sheet in your layout file."
    ))
  }

  # validate the required columns
  required_cols <- c("file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")
  missing_cols <- setdiff(required_cols, names(plate_metadata))

  if (length(missing_cols) > 0) {
    message_list <- c(
      message_list,
      paste0(
        "MISSING METADATA COLUMNS: The following required columns are missing from plate metadata:\n",
        paste("  - ", missing_cols, collapse = "\n"),
        "\n\nThese columns must be present in the 'plate_id' sheet of your layout file."
      )
    )
    # If critical metadata is missing, return early
    cat(">>> EXITING validate_batch_plate_metadata - is_valid: FALSE <<<\n")
    return(list(
      is_valid = FALSE,
      messages = message_list
    ))
  }

  # check to see if all files pass file Path validation
  pass_file_path <- all(looks_like_file_path(plate_metadata$file_name))
  if (!pass_file_path) {
    invalid_paths <- plate_metadata$file_name[!looks_like_file_path(plate_metadata$file_name)]
    message_list <- c(message_list, paste0(
      "INVALID FILE PATHS: The following file paths are incorrectly formatted:\n",
      paste("  - ", invalid_paths, collapse = "\n"),
      "\n\nFile paths must include directory separators (/ or \\) and be complete paths to the files."
    ))
  }

  # Check RP1 PMT Volts
  pass_rp1_pmt_volts <- all(check_rp1_numeric(plate_metadata$rp1_pmt_volts))
  is_numeric <- check_rp1_numeric(plate_metadata$rp1_pmt_volts)
  if (!pass_rp1_pmt_volts) {
    bad_rp1_pmt_volts <- plate_metadata[!is_numeric, c("plateid", "rp1_pmt_volts")]
    message_list <- c(message_list, paste0(
      "INVALID RP1 PMT (Volts): The following plates have invalid RP1 PMT voltage values:\n",
      paste(sprintf("  - Plate '%s': Value '%s'",
                    bad_rp1_pmt_volts$plateid,
                    bad_rp1_pmt_volts$rp1_pmt_volts),
            collapse = "\n"),
      "\n\nRP1 PMT (Volts) must be numeric with at most one decimal point (e.g., 500 or 500.5)."
    ))
  }

  # Check RP1 Target
  pass_rp1_target <- all(check_rp1_numeric(plate_metadata$rp1_target))
  is_target_numeric <- check_rp1_numeric(plate_metadata$rp1_target)
  if (!pass_rp1_target) {
    invalid_rp1_target <- plate_metadata[!is_target_numeric, c("plateid", "rp1_target")]
    message_list <- c(message_list, paste0(
      "INVALID RP1 TARGET: The following plates have invalid RP1 Target values:\n",
      paste(sprintf("  - Plate '%s': Value '%s'",
                    invalid_rp1_target$plateid,
                    invalid_rp1_target$rp1_target),
            collapse = "\n"),
      "\n\nRP1 Target must be numeric with at most one decimal point (e.g., 100 or 100.5)."
    ))
  }

  # Check acquisition date format — normalize first, then validate
  plate_metadata$acquisition_date <- normalize_acquisition_date(plate_metadata$acquisition_date)
  is_time_format <- check_time_format(capitalize_am_pm(plate_metadata$acquisition_date))
  pass_time_format <- all(is_time_format)
  if (!pass_time_format) {
    invalid_time_format <- plate_metadata[!is_time_format, c("plateid", "acquisition_date")]
    message_list <- c(message_list, paste0(
      "INVALID ACQUISITION DATE FORMAT: The following plates have dates that could not be parsed:\n",
      paste(sprintf("  - Plate '%s': Value '%s'",
                    invalid_time_format$plateid,
                    invalid_time_format$acquisition_date),
            collapse = "\n"),
      "\n\nAccepted formats include: DD-MMM-YYYY HH:MM AM/PM, M/D/YYYY H:MM AM/PM, ",
      "DD/MM/YYYY HH:MM, YYYY-MM-DD HH:MM:SS, DD.MM.YYYY HH:MM, and most common ",
      "European and North American date/time formats."
    ))
  }

  is_valid <- length(message_list) == 0

  cat(">>> EXITING validate_batch_plate_metadata - is_valid:", is_valid, "<<<\n")

  return(list(
    is_valid = is_valid,
    messages = message_list,
    normalized_metadata = plate_metadata
  ))
}



validate_batch_bead_array_data <- function(combined_plate_data, antigen_import_list, blank_keyword) {
  cat("\n>>> ENTERING validate_batch_bead_array_data <<<\n")
  cat("combined_plate_data rows:", nrow(combined_plate_data), "\n")
  cat("antigen_import_list rows:", nrow(antigen_import_list), "\n")
  cat("blank_keyword:", blank_keyword, "\n")

  # Convert to data.frame to avoid tibble subscripting issues throughout
  if (inherits(combined_plate_data, "tbl_df")) {
    combined_plate_data <- as.data.frame(combined_plate_data)
    cat("names combined_plate_data", "\n")
    cat(names(combined_plate_data), "\n")
  }

  message_list <- c()
  unique_plates <- unique(combined_plate_data$source_file)

  # obtain antigens from the layout file that are labeled on the plate
  valid_antigens <- unique(antigen_import_list$antigen_label_on_plate)
  cat("valid_antigens", "\n")
  cat(valid_antigens, "\n")

  check_antigens <- all(valid_antigens %in% names(combined_plate_data))
  if (!check_antigens) {
    missing_antigens <- valid_antigens[!valid_antigens %in% names(combined_plate_data)]
    message_list <- c(message_list,  paste(
      "ANTIGEN MISMATCH: The following antigens are missing in the uploaded data files:",
      paste(missing_antigens, collapse = ", "),
      "\nPlease ensure these antigens exist in your plate data files."
    ))
  }

  cat("after antigens")

  # Check % Agg Beads column presence
  pass_agg_bead_check <- check_batch_agg_bead_column(combined_plate_data)
  if (!pass_agg_bead_check$result) {
    message_list <- c(message_list, paste(
      "MISSING COLUMN:",
      pass_agg_bead_check$message
    ))
  } else {
    # Check bead count format for each plate
    for (plate in unique_plates) {
      plate_data <- combined_plate_data[combined_plate_data$source_file == plate,]
      pass_bead_count_check <- check_bead_count(plate_data)

      # FIX: Access list element by index [[2]] not by name $message
      if (!pass_bead_count_check[[1]]) {
        error_details <- if(length(pass_bead_count_check) > 1) pass_bead_count_check[[2]] else "Unknown bead count error"
        message_list <- c(message_list, paste0(
          "BEAD COUNT FORMAT ERROR in file '", plate, "':\n",
          error_details,
          "\n\nExpected format: MFI_value (bead_count), e.g., '123.45 (50)'"
        ))
      }
    }
    cat("after bead count check")
  }

  # examine blanks in type column
  for (plate in unique_plates) {
    plate_data <- combined_plate_data[combined_plate_data$source_file == plate,]
    procceed_to_blank_check <- check_blank_in_sample_boolean(df = plate_data)
    cat("blank_check")

    if (!procceed_to_blank_check) {
      plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
    }

    pass_blank_description <- check_blank_description_batch(plate_data)

    # FIX: Access list element by index and add better context
    if (!pass_blank_description[[1]]) {
      error_details <- if(length(pass_blank_description) > 1) pass_blank_description[[2]] else "Invalid blank description"
      message_list <- c(
        message_list,
        paste0("BLANK DESCRIPTION ERROR in file '", plate, "':\n", error_details)
      )
    }

    # Write updated rows back into the combined df
    combined_plate_data[combined_plate_data$source_file == plate, ] <- plate_data
  }

  is_valid <- length(message_list) == 0

  cat(">>> EXITING validate_batch_bead_array_data - is_valid:", is_valid, "<<<\n")

  return(list(
    is_valid = is_valid,
    messages = message_list
  ))
}

create_batch_invalid_message_table <- function(validation_result, bead_array_validation) {
  # Combine messages from both validation sources
  combined_messages <- c(validation_result$messages, unlist(bead_array_validation$messages))

  # Create a more user-friendly message table
  if (length(combined_messages) == 0) {
    return(data.frame(
      "Status" = "All validations passed",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  message_table <- data.frame(
    "Error #" = seq_along(combined_messages),
    "Validation Error Details" = combined_messages,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  return(message_table)
}

# check description of the blank
check_blank_description <- function(df) {
  df <- df[grepl("^B", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9]+_\\d+$", df$Description),]
  if(nrow(df) > 0) {
    blank_message <- paste("Need to modify the Blank description column to be like [source]_[dilution_factor] e.g. PBS_1: Well",
                           paste(df$Well, "| Value:", df$Description, collapse = ", ")
                           )
    return(list(FALSE, blank_message))
  } else {
    return(TRUE)
  }
}

# return type differs
check_blank_description_batch <- function(df) {
  df <- df[grepl("^B", df$Type) &
             !grepl("Blank", df$Description) &
             !grepl("^[A-Za-z0-9]+_\\d+$", df$Description),]
  if(nrow(df) > 0) {
    blank_message <- paste("Need to modify the Blank description column to be like [source]_[dilution_factor] e.g. PBS_1: Well",
                           paste(df$Well, "| Value:", df$Description, collapse = ", ")
    )
    return(list(FALSE, blank_message))
  } else {
    return(list(TRUE))
  }

}

# blank keyword can either be 'empty_well' or 'use_as_blank'
check_blank_in_sample <- function(df, blank_keyword) {
  # Find rows where description contains "Blank" and type starts with not B
  bad_rows <- which(
    grepl("Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)
  )

  if (length(bad_rows) > 0) {
    #cat("Upload blocked: Found 'Blank' in description with type starting not with 'B'.\n")
    # print(df[bad_rows, ])
    # Ask user to choose replacement keyword
    # repeat {
    #   choice <- readline(
    #     "Replace 'Blank' with one of: 'empty_well' or 'use_as_blank': "
    #   )
    #   if (choice %in% c("empty_well", "use_as_blank")) break
    #   cat("Invalid choice. Please type 'empty_well' or 'use_as_blank'.\n")
    # }

    # df$description[bad_rows] <- choice

    # If use_as_blank -> set type = "B"
    if (blank_keyword == "use_as_blank") {
      df$Type[bad_rows] <- "B"
    }

    # If empty_well -> drop rows
    if (blank_keyword == "empty_well") {
      #  df <- df[-bad_rows, ]
      #df <- df[df$description != "Blank" & df$type != "B",]
      df <- df[!(grepl("^Blank", df$Description, ignore.case = TRUE) & !grepl("^B", df$Type)),]

    }

    return(df)
  } else {

    cat("Check passed: No invalid 'Blank' entries with type 'B'.\n")
    #invisible(TRUE)
    return(df)
  }
}

check_agg_bead_column <- function(df) {
  required_cols <- c("X..Agg.Beads")
  result <- required_cols %in% names(df)
  if (!result) {
    message <- "Ensure there is a % Agg Beads column after the last antigen."
    return(list(result, message))
  } else {
    return(result)
  }
}

check_batch_agg_bead_column <- function(df) {
  required_cols <- c("% Agg Beads", "X..Agg.Beads", "%.Agg.Beads")
  result <- any(required_cols %in% names(df))

  if (!result) {
    message <- "Ensure there is a % Agg Beads column after the last antigen."
    return(list(result = result, message = message))
  } else {
    return(list(result = result, message = NULL))
  }
}

check_bead_count <- function(df) {

  start_col <- which(names(df) == "Description")
  possible_end_names <- c("% Agg Beads", "X..Agg.Beads", "%.Agg.Beads")
  end_col <- which(names(df) %in% possible_end_names)

  # Validate column indices

  if (length(start_col) == 0 || length(end_col) == 0) {
    return(list(FALSE, "Could not find Description or % Agg Beads columns"))
  }

  if (end_col <= start_col + 1) {
    return(list(FALSE, "No antigen columns found between Description and % Agg Beads"))
  }

  # Subset the columns of interest and convert to data.frame to avoid tibble issues
  subset_df <- as.data.frame(df[, (start_col + 1):(end_col - 1)])

  # Apply regex check to all cells in those columns
  # This creates a logical matrix (same size as subset_df)
  match_matrix <- apply(subset_df, 2, function(col) {
    grepl("^\\d+(\\.\\d+)?\\s*\\(\\d+\\)$", as.character(col))
  })

  # Ensure match_matrix is a matrix even with single column
  if (!is.matrix(match_matrix)) {
    match_matrix <- matrix(match_matrix, ncol = 1)
    colnames(match_matrix) <- names(subset_df)
  }

  if (all(match_matrix)) {
    return(list(TRUE))
  } else {
    # Find failed positions using which with arr.ind
    failed_positions <- which(!match_matrix, arr.ind = TRUE)

    # Handle case where failed_positions might be empty or malformed
    if (length(failed_positions) == 0 || nrow(failed_positions) == 0) {
      return(list(FALSE, "Unknown bead count format error"))
    }

    # Build failed report manually to avoid tibble subscripting issues
    failed_report <- data.frame(
      row = failed_positions[, "row"],
      col_idx = failed_positions[, "col"],
      stringsAsFactors = FALSE
    )

    # Get values and column names safely
    failed_report$antigen <- sapply(failed_report$col_idx, function(idx) {
      col_name <- colnames(subset_df)[idx]
      # Remove bead number suffix if present
      sub("\\s*\\(\\d+\\)$", "", col_name)
    })

    failed_report$value <- mapply(function(r, c) {
      as.character(subset_df[r, c])
    }, failed_report$row, failed_report$col_idx)

    # Create message
    msg <- apply(failed_report, 1, function(x) {
      paste0("Row ", x["row"],
             " | Antigen: ", x["antigen"],
             " | Value: ", x["value"])
    })

    # Limit message length to avoid overwhelming output
    if (length(msg) > 20) {
      msg <- c(head(msg, 20), sprintf("... and %d more errors", length(msg) - 20))
    }

    final_message <- paste(msg, collapse = "\n")

    return(list(FALSE, final_message))
  }
}

#' Validate required columns are not null
#'
#' @param data Data frame to validate
#' @param required_cols Vector of required column names
#' @param table_name Name of target table for error messages
#'
#' @return List with valid status, missing columns, and null columns
validate_required_columns <- function(data, required_cols, table_name) {

  result <- list(
    valid = TRUE,
    missing_cols = character(0),
    null_cols = character(0),
    null_row_counts = list(),
    message = ""
  )

  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    result$valid <- FALSE
    result$missing_cols <- missing_cols
  }

  # Check for NULL values in required columns that exist
  existing_required <- intersect(required_cols, names(data))
  for (col in existing_required) {
    null_count <- sum(is.na(data[[col]]))
    if (null_count > 0) {
      result$valid <- FALSE
      result$null_cols <- c(result$null_cols, col)
      result$null_row_counts[[col]] <- null_count
    }
  }

  # Build message
  msg_parts <- c()
  if (length(result$missing_cols) > 0) {
    msg_parts <- c(msg_parts,
                   sprintf("Missing columns: %s", paste(result$missing_cols, collapse = ", ")))
  }
  if (length(result$null_cols) > 0) {
    null_details <- sapply(result$null_cols, function(col) {
      sprintf("%s (%d nulls)", col, result$null_row_counts[[col]])
    })
    msg_parts <- c(msg_parts,
                   sprintf("Columns with NULL values: %s", paste(null_details, collapse = ", ")))
  }

  result$message <- if (length(msg_parts) > 0) {
    sprintf("Validation failed for %s: %s", table_name, paste(msg_parts, collapse = "; "))
  } else {
    sprintf("Validation passed for %s", table_name)
  }

  return(result)
}

#' Get rows with null values in specified columns
#'
#' @param data Data frame to check
#' @param columns Columns to check for nulls
#'
#' @return Data frame with rows containing nulls
get_null_rows <- function(data, columns) {

  existing_cols <- intersect(columns, names(data))

  if (length(existing_cols) == 0) {
    return(data[0, ])
  }

  has_null <- apply(data[, existing_cols, drop = FALSE], 1, function(row) any(is.na(row)))

  return(data[has_null, ])
}

#' Debug sample data preparation
#'
#' @param sample_plate_map Sample plate map
#' @param combined_plate_data Combined plate data
#' @param batch_metadata Batch metadata
#' @param antigen_import_list Antigen list
#' @param subject_map Subject map
#'
#' @return List with debug information
debug_sample_preparation <- function(sample_plate_map, combined_plate_data,
                                     batch_metadata, antigen_import_list, subject_map) {

  debug_info <- list(
    sample_plate_map = list(
      nrow = nrow(sample_plate_map),
      columns = names(sample_plate_map),
      head = head(sample_plate_map)
    ),
    combined_plate_data = list(
      nrow = nrow(combined_plate_data),
      columns = names(combined_plate_data),
      head = head(combined_plate_data)
    ),
    batch_metadata = list(
      nrow = nrow(batch_metadata),
      columns = names(batch_metadata),
      head = head(batch_metadata)
    ),
    antigen_import_list = list(
      nrow = nrow(antigen_import_list),
      columns = names(antigen_import_list),
      head = head(antigen_import_list)
    ),
    subject_map = list(
      nrow = nrow(subject_map),
      columns = names(subject_map),
      head = head(subject_map)
    )
  )

  return(debug_info)
}

#' Check if plates exist in database
#'
#' @param conn Database connection
#' @param project_id project ID
#' @param study_accession study accession ID
#' @param experiment_accession experiment accession ID
#' @param plate_ids Vector of plate IDs to check
#'
#' @return Data frame of existing plates
check_existing_plates <- function(conn, project_id, study_accession, experiment_accession, plateids) {
  cat("Checking for existing uploaded plate in xmap_header \n")
  # Properly quote each plate ID
  plate_ids <- as.character(plateids)
  quoted_plateids <- sapply(plateids, function(x) DBI::dbQuoteString(conn, x))
  plateids_string <- paste(quoted_plateids, collapse = ", ")
  cat("Current plate list:", plateids_string, "\n")
  existing_plate_query <- glue::glue_sql("
    SELECT
      xmap_header_id,
      project_id,
      study_accession,
      experiment_accession,
      plate_id,
      file_name,
      acquisition_date,
      reader_serial_number,
      rp1_pmt_volts,
      rp1_target,
      auth0_user,
      workspace_id,
      plateid,
      plate,
      nominal_sample_dilution,
      n_wells
    FROM madi_results.xmap_header
    WHERE project_id = {project_id}
    AND study_accession = {study_accession}
    AND experiment_accession = {experiment_accession}
    AND plate_id IN ({plate_ids*});
  ", .con = conn)
  cat("Formed glue query to check for existing plates: \n")
  existing_plate_query <- paste(as.character(existing_plate_query), collapse = "\n")
  cat(existing_plate_query)
  existing_plates <- DBI::dbGetQuery(conn, existing_plate_query)
  cat("\n number of existing matches in plates:", nrow(existing_plates), "\n")
  return(existing_plates)
}

#' Get existing antigens for study/experiment
#'
#' @param conn Database connection
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#'
#' @return Data frame of existing antigens
get_existing_antigens <- function(conn, study_accession, experiment_accession) {

  query <- glue::glue_sql("
    SELECT study_accession, experiment_accession, antigen
    FROM madi_results.xmap_antigen_family
    WHERE study_accession = {study_accession}
      AND experiment_accession = {experiment_accession}
  ", .con = conn)

  DBI::dbGetQuery(conn, query)
}

#' Get existing planned visits for study
#'
#' @param conn Database connection
#' @param study_accession Study accession ID
#'
#' @return Data frame of existing visits
get_existing_visits <- function(conn, study_accession) {

  query <- glue::glue_sql("
    SELECT study_accession, timepoint_name
    FROM madi_results.xmap_planned_visit
    WHERE study_accession = {study_accession}
  ", .con = conn)

  DBI::dbGetQuery(conn, query)
}

#' Insert new rows that don't exist in database
#'
#' @param conn Database connection
#' @param schema Schema name
#' @param table Table name
#' @param new_data Data frame to insert
#' @param existing_data Data frame of existing rows
#' @param join_keys Column names to use for deduplication
#' @param label Label for log messages
#'
#' @return Number of rows inserted
insert_new_rows <- function(conn, schema, table, new_data, existing_data, join_keys, label) {

  to_insert <- dplyr::anti_join(new_data, existing_data, by = join_keys)

  if (nrow(to_insert) > 0) {
    DBI::dbAppendTable(conn, DBI::Id(schema = schema, table = table), to_insert)
    cat(sprintf("Inserted %d new %s entries\n", nrow(to_insert), label))
  } else {
    cat(sprintf("No new %s entries to insert\n", label))
  }

  nrow(to_insert)
}


#' Insert data frame to database table with validation
#'
#' @param conn Database connection
#' @param schema Schema name
#' @param table Table name
#' @param data Data frame to insert
#' @param required_cols Required columns that cannot be NULL
#' @param label Label for log messages
#'
#' @return List with success status and row count
insert_to_table <- function(conn, schema, table, data, label, required_cols = NULL) {

  result <- list(
    success = TRUE,
    rows_inserted = 0,
    message = "",
    null_rows = NULL
  )

  if (nrow(data) == 0) {
    result$message <- sprintf("No %s rows to insert", label)
    cat(result$message, "\n")
    return(result)
  }

  # Validate required columns if specified
  if (!is.null(required_cols)) {
    validation <- validate_required_columns(data, required_cols, table)

    if (!validation$valid) {
      result$success <- FALSE
      result$message <- validation$message
      result$null_rows <- get_null_rows(data, required_cols)

      cat("VALIDATION ERROR:", validation$message, "\n")
      cat("Sample of problematic rows:\n")
      print(head(result$null_rows, 5))

      return(result)
    }
  }

  if (table == "xmap_header") {
    nk_cols <- c("study_accession", "experiment_accession", "plate_id")
    if (all(nk_cols %in% names(data))) {
      n_before <- nrow(data)
      data <- data[!duplicated(data[, nk_cols, drop = FALSE]), ]
      n_after <- nrow(data)
      if (n_before != n_after) {
        cat("WARNING: Removed", n_before - n_after, "duplicate header rows\n")
      }
    }
  }

  # Attempt insert
  tryCatch({
    DBI::dbAppendTable(conn, DBI::Id(schema = schema, table = table), data)
    result$rows_inserted <- nrow(data)
    result$message <- sprintf("Inserted %d %s rows", nrow(data), label)
    cat(result$message, "\n")
  }, error = function(e) {
    result$success <<- FALSE
    result$message <<- sprintf("Error inserting %s: %s", label, e$message)
    cat("INSERT ERROR:", result$message, "\n")
  })

  return(result)
}


#' Upload specimen data with validation
#'
#' @param conn Database connection
#' @param plates_map Plates map data frame
#' @param specimen_type Specimen type code (X, S, B, C)
#' @param combined_plate_data Combined plate data
#' @param batch_metadata Batch metadata
#' @param antigen_import_list Antigen list data
#' @param subject_map Subject map data (only needed for samples)
#'
#' @return List with success status and row count
upload_specimen_data <- function(conn, plates_map, specimen_type, combined_plate_data,
                                 batch_metadata, antigen_import_list, subject_map = NULL) {

  # Define required columns for each specimen type
  required_columns <- list(
    "X" = c("sampleid", "study_accession", "plate_id", "plateid", "project_id", "nominal_sample_dilution"),  # Add all NOT NULL columns
    "S" = c("study_accession", "plate_id", "plateid", "project_id", "nominal_sample_dilution"),
    "B" = c("study_accession", "plate_id", "plateid", "project_id", "nominal_sample_dilution"),
    "C" = c("study_accession", "plate_id", "plateid", "project_id", "nominal_sample_dilution")
  )

  # Filter plates map for specimen type
  specimen_map <- plates_map[plates_map$specimen_type == specimen_type, ]

  if (nrow(specimen_map) == 0) {
    return(list(success = TRUE, rows_inserted = 0, message = "No data to insert"))
  }

  # Prepare data based on specimen type
  data <- NULL
  table_name <- NULL
  label <- NULL

  tryCatch({
    switch(
      specimen_type,
      "X" = {
        # Debug before preparation
        cat("\n=== DEBUG: Sample Preparation Inputs ===\n")
        cat("specimen_map rows:", nrow(specimen_map), "\n")
        cat("specimen_map columns:",paste(names(specimen_map), collapse = ", "), "\n")
        cat("combined_plate_data rows:", nrow(combined_plate_data), "\n")
        cat("combined_plate_data columns:",paste(names(combined_plate_data), collapse = ", "), "\n")
        cat("subject_map rows:", nrow(subject_map), "\n")
        cat("subject_map columns:", paste(names(subject_map), collapse = ", "), "\n")

        data <- prepare_batch_bead_assay_samples(
          sample_plate_map = specimen_map,
          combined_plate_data = combined_plate_data,
          batch_metadata = batch_metadata,
          antigen_import_list = antigen_import_list,
          subject_map = subject_map
        )

        # Debug after preparation
        cat("\n=== DEBUG: Prepared Sample Data ===\n")
        cat("Prepared data rows:", nrow(data), "\n")
        cat("Prepared data columns:", paste(names(data), collapse = ", "), "\n")
        cat("sampleid column exists:", "sampleid" %in% names(data), "\n")

        if ("sampleid" %in% names(data)) {
          cat("sampleid null count:", sum(is.na(data$sampleid)), "\n")
          cat("sampleid unique values:", length(unique(data$sampleid)), "\n")
          cat("sampleid sample:", head(data$sampleid, 10), "\n")
        }

        table_name <- "xmap_sample"
        label <- "sample"
      },
      "S" = {
        data <- prepare_batch_bead_assay_standards(
          standard_plate_map = specimen_map,
          combined_plate_data = combined_plate_data,
          antigen_import_list = antigen_import_list,
          batch_metadata = batch_metadata
        )
        table_name <- "xmap_standard"
        label <- "standard"
      },
      "B" = {
        data <- prepare_batch_bead_assay_blanks(
          blanks_plate_map = specimen_map,
          combined_plate_data = combined_plate_data,
          antigen_import_list = antigen_import_list,
          batch_metadata = batch_metadata
        )
        table_name <- "xmap_buffer"
        label <- "blank"
      },
      "C" = {
        data <- prepare_batch_bead_assay_controls(
          controls_plate_map = specimen_map,
          combined_plate_data = combined_plate_data,
          antigen_import_list = antigen_import_list,
          batch_metadata = batch_metadata
        )
        table_name <- "xmap_control"
        label <- "control"
      }
    )
  }, error = function(e) {
    cat("ERROR preparing", specimen_type, "data:", e$message, "\n")
    return(list(success = FALSE, rows_inserted = 0, message = e$message))
  })

  if (is.null(data)) {
    return(list(success = TRUE, rows_inserted = 0, message = "No data prepared"))
  }

  # Insert with validation
  insert_result <- insert_to_table(
    conn = conn,
    schema = "madi_results",
    table = table_name,
    data = data,
    label = label,
    required_cols = required_columns[[specimen_type]]
  )

  return(insert_result)
}


#' Upload antigen family data with deduplication
#'
#' @param conn Database connection
#' @param antigen_import_list Antigen list data
#' @param project_id project ID
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#'
#' @return Number of rows inserted
upload_antigen_family <- function(conn, antigen_import_list, project_id, study_accession, experiment_accession) {

  antigen_family_df <- prepare_batch_antigen_family(antigen_import_list)
  existing_antigens <- get_existing_antigens(conn, study_accession, experiment_accession)

  insert_new_rows(
    conn = conn,
    schema = "madi_results",
    table = "xmap_antigen_family",
    new_data = antigen_family_df,
    existing_data = existing_antigens,
    join_keys = c("study_accession", "experiment_accession", "antigen"),
    label = "antigen family"
  )
}


#' Upload planned visits data with deduplication
#'
#' @param conn Database connection
#' @param timepoint_map Timepoint map data
#' @param study_accession Study accession ID
#'
#' @return Number of rows inserted
upload_planned_visits <- function(conn, timepoint_map, study_accession) {

  planned_visits_df <- prepare_planned_visits(timepoint_map = timepoint_map)
  existing_visits <- get_existing_visits(conn, study_accession)

  insert_new_rows(
    conn = conn,
    schema = "madi_results",
    table = "xmap_planned_visit",
    new_data = planned_visits_df,
    existing_data = existing_visits,
    join_keys = c("study_accession", "timepoint_name"),
    label = "planned visit"
  )
}


#' Serves ALL bead-array upload paths: Raw File AND xPONENT.
#' curve_lookup registration is performed here so it is automatic for
#' every current and future bead-array assay type.
#'
#' @param conn          Active DBI connection
#' @param batch_plates  Combined wide-format plate data
#' @param metadata_batch Batch metadata (from plate_id sheet)
#' @param layout_sheets  List of validated layout template sheets
#'
#' @return List with success status and upload details
upload_batch_to_database <- function(conn, batch_plates, metadata_batch,
                                     layout_sheets) {

  result <- list(
    success        = FALSE,
    already_exists = FALSE,
    counts         = list(
      header    = 0L,
      samples   = 0L,
      standards = 0L,
      blanks    = 0L,
      controls  = 0L,
      antigens  = 0L,
      visits    = 0L,
      curves    = 0L   # NEW: track curve_lookup insertions
    ),
    errors  = list(),
    message = ""
  )

  # ── Unpack layout sheets ────────────────────────────────────────────────
  plates_map        <- layout_sheets[["plates_map"]]
  antigen_import_list <- layout_sheets[["antigen_list"]]
  subject_map       <- layout_sheets[["subject_groups"]]
  timepoint_map     <- layout_sheets[["timepoint"]]

  # ── Derive shared identifiers ───────────────────────────────────────────
  project_id           <- unique(metadata_batch$project_id)
  study_accession      <- unique(metadata_batch$study_name)
  experiment_accession <- unique(metadata_batch$experiment_name)

  cat("\n=== upload_batch_to_database ===\n")
  cat("  project_id:           ", project_id, "\n")
  cat("  study_accession:      ", study_accession, "\n")
  cat("  experiment_accession: ", experiment_accession, "\n")
  cat("  plates in metadata:   ", nrow(metadata_batch), "\n")

  # ── Combine batch plates with source_file tracking ──────────────────────
  batch_plates_combined <- batch_plates
  cat("  batch_plates_combined rows:", nrow(batch_plates_combined), "\n")
  cat("  batch_plates_combined cols:",
      paste(names(batch_plates_combined), collapse = ", "), "\n")

  # ── Upload header ────────────────────────────────────────────────────────
  upload_metadata_df <- prepare_batch_header(metadata_batch)
  header_result <- insert_to_table(
    conn, "madi_results", "xmap_header", upload_metadata_df, "header",
    required_cols = c("project_id", "study_accession", "plate_id")
  )
  result$counts$header <- header_result$rows_inserted

  if (!header_result$success) {
    result$errors$header <- header_result$message
    result$message <- "Failed to upload header"
    return(result)
  }

  # ── Upload samples ───────────────────────────────────────────────────────
  sample_result <- upload_specimen_data(
    conn                = conn,
    plates_map          = plates_map,
    specimen_type       = "X",
    combined_plate_data = batch_plates_combined,
    batch_metadata      = metadata_batch,
    antigen_import_list = antigen_import_list,
    subject_map         = subject_map
  )
  result$counts$samples <- sample_result$rows_inserted

  if (!sample_result$success) {
    result$errors$samples <- sample_result$message
    result$message <- "Failed to upload samples"
    return(result)
  }

  # ── Upload standards ─────────────────────────────────────────────────────
  standards_result <- upload_specimen_data(
    conn                = conn,
    plates_map          = plates_map,
    specimen_type       = "S",
    combined_plate_data = batch_plates_combined,
    batch_metadata      = metadata_batch,
    antigen_import_list = antigen_import_list
  )
  result$counts$standards <- standards_result$rows_inserted

  if (!standards_result$success) {
    result$errors$standards <- standards_result$message
    # Non-fatal for the overall upload — log but continue
    cat("  ⚠ Standards upload issue:", standards_result$message, "\n")
  }

  # ── Register new curve combinations in curve_lookup ──────────────────────
  # Placed immediately after standards are committed so we only register
  # curves that are confirmed in xmap_standard.
  # Works identically for Raw File and xPONENT because both paths produce
  # the same standards data via upload_specimen_data().
  # Non-fatal: a curve_lookup failure never blocks or rolls back data upload.
  if (result$counts$standards > 0) {

    cat("\n  Registering curves in curve_lookup...\n")

    tryCatch({

      # Re-prepare the standards data frame in DB column-name form.
      # We call prepare_batch_bead_assay_standards() which is the same
      # function upload_specimen_data() uses internally for type "S",
      # so the column names are already mapped to DB schema names.
      standard_map <- plates_map[
        substr(plates_map$specimen_type, 1, 1) == "S", , drop = FALSE
      ]

      if (nrow(standard_map) > 0) {

        standards_for_curves <- prepare_batch_bead_assay_standards(
          standard_plate_map  = standard_map,
          combined_plate_data = batch_plates_combined,
          antigen_import_list = antigen_import_list,
          batch_metadata      = metadata_batch
        )

        if (!is.null(standards_for_curves) && nrow(standards_for_curves) > 0) {

          cl_result <- register_curve_lookup(
            conn         = conn,
            standards_df = standards_for_curves,
            project_id   = project_id
          )

          result$counts$curves <- cl_result$rows_inserted

          if (cl_result$success) {
            cat("    → curve_lookup:", cl_result$message, "\n")
          } else {
            cat("    ⚠ curve_lookup warning:", cl_result$message, "\n")
            result$errors$curve_lookup <- cl_result$message
          }

        } else {
          cat("    → curve_lookup: no standard rows prepared — skipping\n")
        }
      }

    }, error = function(e_cl) {
      # Fully isolated — a curve_lookup error must never cause the
      # surrounding upload to appear as failed to the user.
      cat("    ⚠ curve_lookup non-fatal error:", conditionMessage(e_cl), "\n")
      result$errors$curve_lookup <<- conditionMessage(e_cl)
    })
  } else {
    cat("  → curve_lookup: no standards inserted — skipping registration\n")
  }
  # ── End curve_lookup registration ────────────────────────────────────────

  # ── Upload blanks ────────────────────────────────────────────────────────
  blanks_result <- upload_specimen_data(
    conn                = conn,
    plates_map          = plates_map,
    specimen_type       = "B",
    combined_plate_data = batch_plates_combined,
    batch_metadata      = metadata_batch,
    antigen_import_list = antigen_import_list
  )
  result$counts$blanks <- blanks_result$rows_inserted

  if (!blanks_result$success) {
    result$errors$blanks <- blanks_result$message
  }

  # ── Upload controls ──────────────────────────────────────────────────────
  controls_result <- upload_specimen_data(
    conn                = conn,
    plates_map          = plates_map,
    specimen_type       = "C",
    combined_plate_data = batch_plates_combined,
    batch_metadata      = metadata_batch,
    antigen_import_list = antigen_import_list
  )
  result$counts$controls <- controls_result$rows_inserted

  if (!controls_result$success) {
    result$errors$controls <- controls_result$message
  }

  # ── Upload antigen family ────────────────────────────────────────────────
  result$counts$antigens <- upload_antigen_family(
    conn                 = conn,
    antigen_import_list  = antigen_import_list,
    project_id           = project_id,
    study_accession      = study_accession,
    experiment_accession = experiment_accession
  )

  # ── Upload planned visits ────────────────────────────────────────────────
  result$counts$visits <- upload_planned_visits(
    conn            = conn,
    timepoint_map   = timepoint_map,
    study_accession = study_accession
  )

  # ── Final status ─────────────────────────────────────────────────────────
  # curve_lookup errors are explicitly excluded from the fatal error check
  # because data is already committed by the time curve_lookup runs.
  fatal_errors <- result$errors[
    !names(result$errors) %in% c("curve_lookup", "blanks", "controls")
  ]

  result$success <- length(fatal_errors) == 0

  result$message <- if (result$success) {
    paste0(
      "Batch uploaded successfully — ",
      "header: ",    result$counts$header,    ", ",
      "samples: ",   result$counts$samples,   ", ",
      "standards: ", result$counts$standards, ", ",
      "blanks: ",    result$counts$blanks,    ", ",
      "controls: ",  result$counts$controls,  ", ",
      "curves registered: ", result$counts$curves
    )
  } else {
    paste("Upload completed with errors:",
          paste(names(fatal_errors), collapse = ", "))
  }

  cat("\n  Result:", result$message, "\n")
  cat("================================\n\n")

  return(result)
}
# upload_batch_to_database <- function(conn, batch_plates, metadata_batch, layout_sheets) {
#
#   # Extract layout sheets
#   timepoint_map <- layout_sheets[["timepoint"]]
#   subject_map <- layout_sheets[["subject_groups"]]
#   antigen_import_list <- layout_sheets[["antigen_list"]]
#   plates_map <- layout_sheets[["plates_map"]]
#
#   # add feature to antigen import list
#   features <- data.frame(feature = unique(plates_map$feature))
#   antigen_import_list <- merge(antigen_import_list, features, by = NULL)
#
#   # Get unique identifiers
#   project_id <- unique(metadata_batch$project_id)
#   study_accession <- unique(metadata_batch$study_name)
#   experiment_accession <- unique(metadata_batch$experiment_name)
#
#   # The database xmap_header table stores the full cleaned plate identifier in 'plate_id'
#   # Using 'plateid' (which may have different values) caused duplicate uploads
#   plateids <- unique(metadata_batch$plate_id)
#   # plateids <- unique(metadata_batch$plateid)
#
#   # Initialize result tracking
#   result <- list(
#     success = FALSE,
#     already_exists = FALSE,
#     counts = list(
#       header = 0,
#       samples = 0,
#       standards = 0,
#       blanks = 0,
#       controls = 0,
#       antigens = 0,
#       visits = 0
#     ),
#     errors = list(),
#     message = ""
#   )
#
#   # Check for existing plates
#   existing_plates <- check_existing_plates(conn = conn,
#                                            project_id = userWorkSpaceID(),
#                                            study_accession = study_accession,
#                                            experiment_accession = experiment_accession,
#                                            plateids = plateids)
#
#   if (nrow(existing_plates) > 0) {
#     result$already_exists <- TRUE
#     result$message <- "These plates already exist for the study and experiment"
#     return(result)
#   }
#
#   # Upload header
#   upload_metadata_df <- prepare_batch_header(metadata_batch)
#   header_result <- insert_to_table(
#     conn, "madi_results", "xmap_header", upload_metadata_df, "header",
#     required_cols = c("project_id","study_accession", "plate_id")
#   )
#   result$counts$header <- header_result$rows_inserted
#
#   if (!header_result$success) {
#     result$errors$header <- header_result$message
#     result$message <- "Failed to upload header"
#     return(result)
#   }
#
#   # # Join plates with source file
#   # batch_plates_combined <- merge(
#   #   batch_plates,
#   #   metadata_batch[, c("source_file", "plate")],
#   #   by.x = "source_file",
#   #   all.x = TRUE
#   # )
#
#   batch_plates_combined <- batch_plates
#   cat("After join batch\n")
#
#   # Debug: Print combined data info
#   cat("\n=== DEBUG: batch_plates_combined ===\n")
#   cat("Rows:", nrow(batch_plates_combined), "\n")
#   cat("Columns:", paste(names(batch_plates_combined), collapse = ", "), "\n")
#
#   # Upload samples
#   sample_result <- upload_specimen_data(
#     conn = conn,
#     plates_map = plates_map,
#     specimen_type = "X",
#     combined_plate_data = batch_plates_combined,
#     batch_metadata = metadata_batch,
#     antigen_import_list = antigen_import_list,
#     subject_map = subject_map
#   )
#   result$counts$samples <- sample_result$rows_inserted
#
#   if (!sample_result$success) {
#     result$errors$samples <- sample_result$message
#     result$message <- "Failed to upload samples"
#     return(result)
#   }
#
#   # Upload standards
#   standards_result <- upload_specimen_data(
#     conn = conn,
#     plates_map = plates_map,
#     specimen_type = "S",
#     combined_plate_data = batch_plates_combined,
#     batch_metadata = metadata_batch,
#     antigen_import_list = antigen_import_list
#   )
#   result$counts$standards <- standards_result$rows_inserted
#
#   if (!standards_result$success) {
#     result$errors$standards <- standards_result$message
#   }
#
#   # Upload blanks
#   blanks_result <- upload_specimen_data(
#     conn = conn,
#     plates_map = plates_map,
#     specimen_type = "B",
#     combined_plate_data = batch_plates_combined,
#     batch_metadata = metadata_batch,
#     antigen_import_list = antigen_import_list
#   )
#   result$counts$blanks <- blanks_result$rows_inserted
#
#   if (!blanks_result$success) {
#     result$errors$blanks <- blanks_result$message
#   }
#
#   # Upload controls
#   controls_result <- upload_specimen_data(
#     conn = conn,
#     plates_map = plates_map,
#     specimen_type = "C",
#     combined_plate_data = batch_plates_combined,
#     batch_metadata = metadata_batch,
#     antigen_import_list = antigen_import_list
#   )
#   result$counts$controls <- controls_result$rows_inserted
#
#   if (!controls_result$success) {
#     result$errors$controls <- controls_result$message
#   }
#
#   # Upload antigen family
#   result$counts$antigens <- upload_antigen_family(
#     conn = conn,
#     antigen_import_list = antigen_import_list,
#     study_accession = study_accession,
#     experiment_accession = experiment_accession
#   )
#
#   # Upload planned visits
#   result$counts$visits <- upload_planned_visits(
#     conn = conn,
#     timepoint_map = timepoint_map,
#     study_accession = study_accession
#   )
#
#   result$success <- length(result$errors) == 0
#   result$message <- if (result$success) {
#     "Batch uploaded successfully"
#   } else {
#     paste("Upload completed with errors:", paste(names(result$errors), collapse = ", "))
#   }
#
#   return(result)
# }



# Overall function to call plate validation
plate_validation <- function(plate_metadata, plate_data, blank_keyword) {
  message_list <- c()

  # validate the required columns
  required_cols <- c("file_name", "rp1_pmt_volts", "rp1_target", "acquisition_date")
  missing_cols <- setdiff(required_cols, names(plate_metadata))

  if (length(missing_cols) > 0) {
    message_list <- c(
      message_list,
      paste("The following required plate metadata columns are missing so further parsing cannot be conducted:",
            paste(missing_cols, collapse = ", "))
    )
    # If critical metadata is missing, return early
    return(list(
      is_valid = FALSE,
      messages = message_list
    ))
  }

  # pass_required_metadata_variables <- validate_metadata_variables(plate_metadata)
  # if (!pass_required_metadata_variables[[1]]) {
  #   message_list <- c(message_list, pass_required_metadata_variables[[2]])
  # }

  # check to see if it passes file Path
  pass_file_path <- looks_like_file_path(plate_metadata$file_name)
  if (!pass_file_path) {
     message_list <- c(message_list, "Ensure the file path has foward or backward slashes based on Mac or Windows")
  }

  pass_rp1_pmt_volts <- check_rp1_numeric(plate_metadata$rp1_pmt_volts)
  if (!pass_rp1_pmt_volts) {
    message_list <- c(message_list, paste("Ensure that the RP1 PMT (Volts) field is numeric and if it is a decimal only one period is present. Value:",plate_metadata$rp1_pmt_volts, sep = " "))
  }

  pass_rp1_target <- check_rp1_numeric(plate_metadata$rp1_target)
  if (!pass_rp1_target) {
    message_list <- c(message_list, paste("Ensure that the RP1 Target is numeric and if it is a decimal only one period is present. Value:", plate_metadata$rp1_target, sep = " "))
  }

  plate_metadata$acquisition_date <- normalize_acquisition_date(plate_metadata$acquisition_date)
  pass_time_format <- check_time_format(capitalize_am_pm(plate_metadata$acquisition_date))
  if (!pass_time_format) {
    message_list <- c(message_list, paste("Could not parse the acquisition date. Please check the value:",
                                          plate_metadata$acquisition_date, sep = " "))
  }


  # validate main data set

  pass_type_col <- check_type_column(plate_data)
  if (!pass_type_col[[1]]) {
    message_list <- c(message_list, pass_type_col[[2]])
  }
  pass_description <- check_sample_description(plate_data)
  if (!pass_description[[1]]) {
    message_list <- c(message_list, pass_description[[2]])
  }

  pass_standard_description <- check_standard_description(plate_data)
  if (!pass_standard_description[[1]]) {
    message_list <- c(message_list, pass_standard_description[[2]])
  }

  pass_agg_bead_check <- check_agg_bead_column(plate_data)
  if (!pass_agg_bead_check[[1]]) {
    message_list <- c(message_list, pass_agg_bead_check[[2]])
  } else {
    # check blanks if aggregate column is present
    pass_bead_count_check <- check_bead_count(plate_data)
    if (!pass_bead_count_check[[1]]) {
      message_list <- c(message_list, paste("Ensure the bead count is present after all MFI values in parentheses for: \n", pass_bead_count_check[[2]], sep = ""))
    }
  }
 # examine blanks in type column
 procceed_to_blank_check <- check_blank_in_sample_boolean(plate_data)
 if (!procceed_to_blank_check) {
    # Update Plate Data based on keyword choice
    plate_data <- check_blank_in_sample(plate_data, blank_keyword = blank_keyword)
 }

 # if blanks are processed still check it
 pass_blank_description <- check_blank_description(plate_data)
 if (!pass_blank_description[[1]]) {
   message_list <- c(message_list, pass_blank_description[[2]])
 }

 # if no invalid messages then it is good to pass
 is_valid <- length(message_list) == 0

 if (is_valid)  {
   return(list(
     is_valid = is_valid,
     messages = message_list,
     updated_plate_data = plate_data
   ))
} else {
  return(list(
    is_valid = is_valid,
    messages = message_list
  ))
}
 # if (pass_file_path && pass_bead_count_check[[1]] && procceed_to_blank_check) {
 #   return(list(
 #     is_valid = TRUE
 #   )
 #   )
 # } else {
 #
 # return(list(
 #   is_valid = FALSE,
 #   messages = message_list,
 #   updated_plate_data = plate_data
 # ))
 #
 # }
}

createValidateBadge <- function(is_validated) {

  if (is_validated) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Plate Validated", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Validated")
    )
  }
}

createValidateBatchBadge <- function(is_validated) {

  if (is_validated) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Batch Validated", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Batch Not Validated")
    )
  }
}
createUploadedBatchBadge <- function(is_uploded) {

  if (is_uploded) {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Batch Uploaded", sep = ""))
    )
  } else {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Batch Not Uploaded")
    )
  }
}

createOptimizedBadge <- function(is_optimized) {

  if (is.null(is_optimized) || length(is_optimized) == 0 || !isTRUE(is_optimized)) {
    # Not Uploaded badge (grey)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #6c757d; color: white;",
      tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Optimized")
    )
  } else {
    # Completed Upload badge (green)
    span(
      class = "badge",
      style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
               background-color: #28a745; color: white;",
      tagList(tags$i(class = "fa fa-check"), paste("Plate Optimized", sep = ""))
    )
  }
  # if (isTRUE(is_optimized)) {
  #   # Completed Upload badge (green)
  #   span(
  #     class = "badge",
  #     style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
  #              background-color: #28a745; color: white;",
  #     tagList(tags$i(class = "fa fa-check"), paste("Plate Optimized", sep = ""))
  #   )
  # } else {
  #   # Not Uploaded badge (grey)
  #   span(
  #     class = "badge",
  #     style = "padding: 3px 8px; border-radius: 10px; margin-left: 10px;
  #              background-color: #6c757d; color: white;",
  #     tagList(tags$i(class = "fa fa-exclamation-circle"), "Plate Not Optimized")
  #   )
  # }
}
