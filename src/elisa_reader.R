# ---- ELISA Reader Module ----

# This module provides ELISA file upload, parsing, layout template generation,
# validation, and database upload functionality. It mirrors the bead array
# workflow but handles ELISA-specific data structures:
#   - Simplex assay (one antigen, one feature per experiment)
#   - Absorbance readings at multiple wavelengths (not MFI + bead counts)
#   - Results arranged in plate grids with wavelength headers
#   - plate_map sheet with Antigen, Feature, Plate, SType, Well, Description




# ---- Reactive values for ELISA workflow ----
elisa_plate_data <- reactiveVal(NULL)
elisa_header_list <- reactiveVal(list())
elisa_plate_list <- reactiveVal(list())
elisa_assay_response_long <- reactiveVal(NULL)
elisa_plate_map_raw <- reactiveVal(NULL)
elisa_parsed_results <- reactiveVal(NULL)

elisa_validation_state <- reactiveVal(list(
  is_validated = FALSE,
  is_uploaded = FALSE,
  validation_time = NULL,
  upload_time = NULL,
  metadata_result = NULL,
  data_result = NULL,
  data_stored = FALSE
))

elisa_description_status <- reactiveVal(list(
  has_content = TRUE,
  has_sufficient_elements = TRUE,
  min_elements_found = 0,
  required_elements = 3,
  checked = FALSE,
  message = NULL
))



# ---- ELISA FILE PARSING FUNCTIONS ----

#' Parse a single ELISA results sheet
#'
#' Reads the results sheet from an ELISA Excel file and extracts:
#' - Plate-level metadata (wavelengths, plate numbers)
#' - Absorbance grids at each wavelength
#' - Sample identity grids
#' - Autoloading range annotation
#'
#' @param file_path Path to the Excel file
#' @param results_sheet_name Name of the results sheet (default: "results")
#' @return List with parsed plate data, headers, and metadata
#'
parse_elisa_results_sheet <- function(file_path, results_sheet_name = "results") {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  PARSING ELISA RESULTS SHEET                             ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Read the entire sheet as character to preserve all formatting
  raw_data <- tryCatch({
    readxl::read_excel(file_path, sheet = results_sheet_name,
                       col_names = FALSE, col_types = "text")
  }, error = function(e) {
    # Try alternate name
    readxl::read_excel(file_path, sheet = "results",
                       col_names = FALSE, col_types = "text")
  })

  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)
  cat("  Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "cols\n")

  # Extract file-level metadata from header rows
  file_metadata <- extract_elisa_file_metadata(raw_data)
  cat("  File metadata extracted:\n")
  cat("    → Filename:", file_metadata$filename, "\n")
  cat("    → Acquisition date:", file_metadata$acquisition_date, "\n")

  # Find all plate blocks
  plate_blocks <- find_elisa_plate_blocks(raw_data)
  cat("  Found", length(plate_blocks), "plate block(s)\n")

  # Extract autoloading range
  autoloading_range <- extract_autoloading_range(raw_data)
  cat("  Autoloading range:", autoloading_range, "\n")

  # Parse each plate block
  parsed_plates <- list()
  for (block in plate_blocks) {
    cat("  Parsing Plate", block$plate_number, "...\n")
    parsed <- parse_elisa_plate_block(raw_data, block)
    parsed_plates[[as.character(block$plate_number)]] <- parsed
    cat("    → Wavelengths:", paste(names(parsed$absorbance_grids), collapse = ", "), "\n")
    cat("    → Sample grid:", if (!is.null(parsed$sample_grid)) "present" else "missing", "\n")
  }

  list(
    file_metadata = file_metadata,
    parsed_plates = parsed_plates,
    autoloading_range = autoloading_range,
    raw_data = raw_data
  )
}


#' Extract file-level metadata from ELISA results header
#'
#' Parses the first few rows of the results sheet to extract:
#' - Original filename
#' - Acquisition datetime
#' - Absorbance method identifier
#'
#' @param raw_data Data frame of the raw results sheet
#' @return List with filename, acquisition_date, absorbance_id
#'
extract_elisa_file_metadata <- function(raw_data) {

  metadata <- list(
    filename = NA_character_,
    acquisition_date = NA_character_,
    absorbance_id = NA_character_
  )

  # Search the first 10 rows for metadata patterns
  search_rows <- min(10, nrow(raw_data))

  for (i in seq_len(search_rows)) {
    row_vals <- as.character(unlist(raw_data[i, ]))
    row_vals <- row_vals[!is.na(row_vals)]
    row_text <- paste(row_vals, collapse = " ")

    # Look for filename (typically contains .skax or file extension)
    if (any(grepl("\\.(skax|xlsx|xls|csv)$", row_vals, ignore.case = TRUE))) {
      metadata$filename <- row_vals[grepl("\\.(skax|xlsx|xls|csv)$", row_vals, ignore.case = TRUE)][1]
    }

    # Look for datetime pattern (e.g., "13-02-26 15:39:01" or similar)
    date_pattern <- "\\d{2,4}[-/.]\\d{2}[-/.]\\d{2,4}\\s+\\d{2}:\\d{2}(:\\d{2})?"
    if (any(grepl(date_pattern, row_vals))) {
      metadata$acquisition_date <- row_vals[grepl(date_pattern, row_vals)][1]
    }

    # Look for "Absorbance" identifier
    if (any(grepl("^Absorbance", row_vals, ignore.case = TRUE))) {
      metadata$absorbance_id <- row_vals[grepl("^Absorbance", row_vals, ignore.case = TRUE)][1]
    }
  }

  metadata
}


#' Find all plate blocks in ELISA results sheet
#'
#' Scans the raw data to identify plate blocks. Each plate block begins with
#' a "Wavelength:" line followed by a "Plate #" line. A plate may have
#' multiple wavelength sections and one Sample section.
#'
#' @param raw_data Data frame of the raw results sheet
#' @return List of plate block descriptors with row indices
#'
find_elisa_plate_blocks <- function(raw_data) {

  plate_blocks <- list()
  current_plate <- NULL
  n_rows <- nrow(raw_data)

  for (i in seq_len(n_rows)) {
    row_vals <- as.character(unlist(raw_data[i, ]))
    row_vals <- trimws(row_vals[!is.na(row_vals)])

    if (length(row_vals) == 0) next

    # Check for "Wavelength: ### nm" pattern
    wavelength_match <- grep("^Wavelength:\\s*\\d+\\s*nm", row_vals, value = TRUE)
    if (length(wavelength_match) > 0) {
      wavelength_val <- as.numeric(gsub("[^0-9]", "", wavelength_match[1]))

      # Look ahead for Plate # on the next line(s)
      for (j in (i + 1):min(i + 3, n_rows)) {
        next_vals <- as.character(unlist(raw_data[j, ]))
        next_vals <- trimws(next_vals[!is.na(next_vals)])
        plate_match <- grep("^Plate\\s+\\d+", next_vals, value = TRUE)

        if (length(plate_match) > 0) {
          plate_num <- as.integer(gsub("[^0-9]", "", plate_match[1]))

          # Check if this plate already exists in our blocks
          plate_key <- as.character(plate_num)
          if (is.null(plate_blocks[[plate_key]])) {
            plate_blocks[[plate_key]] <- list(
              plate_number = plate_num,
              wavelength_sections = list(),
              sample_section = NULL
            )
          }

          # Find the grid start (look for "Abs" in column 1)
          grid_start <- NULL
          for (k in (j + 1):min(j + 3, n_rows)) {
            k_vals <- as.character(unlist(raw_data[k, ]))
            if (!is.na(k_vals[1]) && trimws(k_vals[1]) == "Abs") {
              grid_start <- k
              break
            }
          }

          if (!is.null(grid_start)) {
            # Grid is 8 rows (A-H) for 96-well, 16 rows for 384-well
            # Detect grid size by counting rows with row labels (A, B, C, ...)
            grid_end <- grid_start
            for (k in (grid_start + 1):min(grid_start + 20, n_rows)) {
              k_val <- trimws(as.character(raw_data[k, 1]))
              if (!is.na(k_val) && grepl("^[A-P]$", k_val)) {
                grid_end <- k
              } else {
                break
              }
            }

            plate_blocks[[plate_key]]$wavelength_sections[[as.character(wavelength_val)]] <- list(
              wavelength = wavelength_val,
              wavelength_row = i,
              plate_row = j,
              grid_start = grid_start,
              grid_end = grid_end
            )
          }
          break
        }
      }
    }

    # Check for "Sample" grid header
    if (any(row_vals == "Sample")) {
      sample_col <- which(row_vals == "Sample")[1]

      # Find which plate this Sample section belongs to
      # Look backward for the most recent Plate reference
      # Or look at the column numbers to match with wavelength grids
      sample_grid_start <- i
      sample_grid_end <- i
      for (k in (i + 1):min(i + 20, n_rows)) {
        k_val <- trimws(as.character(raw_data[k, 1]))
        if (!is.na(k_val) && grepl("^[A-P]$", k_val)) {
          sample_grid_end <- k
        } else {
          break
        }
      }

      # Associate with the most recently started plate
      # Find the plate whose last wavelength section is closest above this row
      best_plate <- NULL
      best_dist <- Inf
      for (pk in names(plate_blocks)) {
        ws <- plate_blocks[[pk]]$wavelength_sections
        for (wk in names(ws)) {
          dist <- i - ws[[wk]]$grid_end
          if (dist > 0 && dist < best_dist) {
            best_dist <- dist
            best_plate <- pk
          }
        }
      }

      if (!is.null(best_plate)) {
        plate_blocks[[best_plate]]$sample_section <- list(
          grid_start = sample_grid_start,
          grid_end = sample_grid_end
        )
      }
    }
  }

  plate_blocks
}


#' Extract autoloading range from ELISA results
#'
#' @param raw_data Data frame of the raw results sheet
#' @return Character string with autoloading range or NA
#'
extract_autoloading_range <- function(raw_data) {
  n_rows <- nrow(raw_data)

  # Search from the bottom of the file
  for (i in n_rows:max(1, n_rows - 20)) {
    row_vals <- as.character(unlist(raw_data[i, ]))
    row_vals <- row_vals[!is.na(row_vals)]
    row_text <- paste(row_vals, collapse = " ")

    if (grepl("Autoloading\\s*range", row_text, ignore.case = TRUE)) {
      # Extract the range string
      range_match <- gsub(".*Autoloading\\s*range\\s*", "", row_text)
      return(trimws(range_match))
    }
  }

  NA_character_
}


#' Parse a single plate block from ELISA data
#'
#' Extracts absorbance grids for each wavelength and the sample identity grid.
#'
#' @param raw_data Full raw data frame
#' @param block Plate block descriptor from find_elisa_plate_blocks()
#' @return List with absorbance_grids (named by wavelength), sample_grid
#'
parse_elisa_plate_block <- function(raw_data, block) {

  result <- list(
    plate_number = block$plate_number,
    absorbance_grids = list(),
    sample_grid = NULL
  )

  # Parse each wavelength grid
  for (wl_key in names(block$wavelength_sections)) {
    section <- block$wavelength_sections[[wl_key]]
    grid <- extract_plate_grid(raw_data, section$grid_start, section$grid_end)
    result$absorbance_grids[[wl_key]] <- grid
  }

  # Parse sample grid
  if (!is.null(block$sample_section)) {
    result$sample_grid <- extract_plate_grid(
      raw_data,
      block$sample_section$grid_start,
      block$sample_section$grid_end
    )
  }

  result
}


#' Extract a plate grid from raw data
#'
#' Reads an 8x12 (or larger) grid from the raw data starting at the
#' specified row. The first row contains column headers (1-12),
#' subsequent rows have row labels (A-H) in column 1 and values in columns 2+.
#'
#' @param raw_data Full raw data frame
#' @param grid_start Row index where the grid header begins (Abs/Sample row)
#' @param grid_end Row index of the last data row
#' @return Data frame with well, row_label, col_number, value columns
#'
extract_plate_grid <- function(raw_data, grid_start, grid_end) {

  # Header row has column numbers
  header_row <- as.character(unlist(raw_data[grid_start, ]))

  # Find column number positions (skip first column which has "Abs" or "Sample")
  col_positions <- which(!is.na(header_row) & grepl("^\\d+$", trimws(header_row)))
  col_numbers <- as.integer(trimws(header_row[col_positions]))

  # Parse data rows
  wells <- data.frame(
    well = character(),
    row_label = character(),
    col_number = integer(),
    value = character(),
    stringsAsFactors = FALSE
  )

  for (i in (grid_start + 1):grid_end) {
    row_data <- as.character(unlist(raw_data[i, ]))
    row_label <- trimws(row_data[1])

    if (is.na(row_label) || !grepl("^[A-P]$", row_label)) next

    for (j in seq_along(col_positions)) {
      col_idx <- col_positions[j]
      col_num <- col_numbers[j]
      value <- trimws(row_data[col_idx])

      well_id <- paste0(row_label, col_num)

      wells <- rbind(wells, data.frame(
        well = well_id,
        row_label = row_label,
        col_number = col_num,
        value = value,
        stringsAsFactors = FALSE
      ))
    }
  }

  wells
}


#' Parse ELISA plate_map sheet
#'
#' Reads the plate_map sheet which contains:
#' Antigen, Feature, Plate, SType, Well, Description
#' And optionally: ID, Specimen_id
#'
#' @param file_path Path to the Excel file
#' @param plate_map_sheet Name of the plate_map sheet
#' @return Data frame with plate_map data
#'
parse_elisa_plate_map <- function(file_path, plate_map_sheet = "plate_map") {

  cat("\n  Parsing ELISA plate_map sheet...\n")

  plate_map <- tryCatch({
    readxl::read_excel(file_path, sheet = plate_map_sheet)
  }, error = function(e) {
    stop("Could not read plate_map sheet: ", e$message)
  })

  plate_map <- as.data.frame(plate_map, stringsAsFactors = FALSE)

  # Normalize column names
  names(plate_map) <- trimws(names(plate_map))

  # Validate required columns

  required_cols <- c("Antigen", "Feature", "Plate", "SType", "Well")
  # Allow case-insensitive matching
  matched_cols <- sapply(required_cols, function(rc) {
    idx <- grep(paste0("^", rc, "$"), names(plate_map), ignore.case = TRUE)
    if (length(idx) > 0) names(plate_map)[idx[1]] else NA
  })

  missing <- required_cols[is.na(matched_cols)]
  if (length(missing) > 0) {
    stop("plate_map sheet is missing required columns: ", paste(missing, collapse = ", "))
  }

  # Standardize column names
  name_mapping <- setNames(required_cols, matched_cols[!is.na(matched_cols)])
  for (original in names(name_mapping)) {
    if (original != name_mapping[original]) {
      names(plate_map)[names(plate_map) == original] <- name_mapping[original]
    }
  }

  # Check for optional columns
  if (!"Description" %in% names(plate_map)) {
    desc_col <- grep("^description$", names(plate_map), ignore.case = TRUE, value = TRUE)
    if (length(desc_col) > 0) {
      names(plate_map)[names(plate_map) == desc_col[1]] <- "Description"
    } else if ("ID" %in% names(plate_map)) {
      # Map "ID" column to "Description" when Description is not present
      # The ID column often contains the same delimited format as Description
      plate_map$Description <- plate_map$ID
      cat("    → Mapped 'ID' column to 'Description'\n")
    }
  }

  if (!"Specimen_id" %in% names(plate_map)) {
    spec_col <- grep("^specimen_id$", names(plate_map), ignore.case = TRUE, value = TRUE)
    if (length(spec_col) > 0) {
      names(plate_map)[names(plate_map) == spec_col[1]] <- "Specimen_id"
    }
  }

  if (!"ID" %in% names(plate_map)) {
    id_col <- grep("^id$", names(plate_map), ignore.case = TRUE, value = TRUE)
    if (length(id_col) > 0) {
      names(plate_map)[names(plate_map) == id_col[1]] <- "ID"
    }
  }

  # Ensure Plate is integer
  plate_map$Plate <- as.integer(plate_map$Plate)

  # Create well in standard format (e.g., A1, B12)
  plate_map$Well <- toupper(trimws(plate_map$Well))

  cat("    → Rows:", nrow(plate_map), "\n")
  cat("    → Columns:", paste(names(plate_map), collapse = ", "), "\n")
  cat("    → Plates:", paste(sort(unique(plate_map$Plate)), collapse = ", "), "\n")
  cat("    → Antigen(s):", paste(unique(plate_map$Antigen), collapse = ", "), "\n")
  cat("    → Feature(s):", paste(unique(plate_map$Feature), collapse = ", "), "\n")
  cat("    → STypes:", paste(sort(unique(plate_map$SType)), collapse = ", "), "\n")

  plate_map
}


#' Combine ELISA parsed results into a long-format data frame
#'
#' Merges the absorbance grids with sample identity information
#' from the plate_map to create a long-format data frame suitable
#' for the layout template.
#'
#' @param parsed_results Output from parse_elisa_results_sheet()
#' @param plate_map Output from parse_elisa_plate_map()
#' @param file_name Original file name
#' @return Data frame in long format with columns:
#'   plateid, plate_number, well, wavelength, absorbance,
#'   sample_id, stype, antigen, feature, description
#'
combine_elisa_data <- function(parsed_results, plate_map, file_name) {

  cat("\n  Combining ELISA parsed data...\n")

  all_rows <- list()

  for (plate_key in names(parsed_results$parsed_plates)) {
    plate <- parsed_results$parsed_plates[[plate_key]]
    plate_num <- plate$plate_number

    # Get plate_map entries for this plate
    pm_plate <- plate_map[plate_map$Plate == plate_num, ]

    if (nrow(pm_plate) == 0) {
      cat("    ⚠ No plate_map entries for Plate", plate_num, "\n")
      next
    }

    # Get the antigen and feature for this plate (ELISA is simplex)
    antigen <- unique(pm_plate$Antigen)[1]
    feature <- unique(pm_plate$Feature)[1]

    # For each wavelength, create rows
    for (wl_key in names(plate$absorbance_grids)) {
      abs_grid <- plate$absorbance_grids[[wl_key]]
      wavelength <- as.numeric(wl_key)

      for (r in seq_len(nrow(abs_grid))) {
        well <- abs_grid$well[r]
        # Force double precision: as.numeric() alone can produce integer-class
        # values (e.g. 1, 0) that survive bind_rows and reach dbAppendTable as
        # INTEGER, causing PostgreSQL to truncate all decimal places.
        abs_value <- round(as.double(abs_grid$value[r]), 4)

        # Look up from plate_map
        pm_row <- pm_plate[pm_plate$Well == well, ]

        stype <- if (nrow(pm_row) > 0) pm_row$SType[1] else NA_character_
        description <- if (nrow(pm_row) > 0 && "Description" %in% names(pm_row)) {
          pm_row$Description[1]
        } else NA_character_
        specimen_id <- if (nrow(pm_row) > 0 && "Specimen_id" %in% names(pm_row)) {
          pm_row$Specimen_id[1]
        } else NA_character_

        # Also get sample identity from sample_grid if available
        sample_label <- NA_character_
        if (!is.null(plate$sample_grid)) {
          sg_row <- plate$sample_grid[plate$sample_grid$well == well, ]
          if (nrow(sg_row) > 0) {
            sample_label <- sg_row$value[1]
          }
        }

        all_rows[[length(all_rows) + 1]] <- data.frame(
          source_file = file_name,
          plateid = paste0("plate_", plate_num),
          plate_number = paste0("plate_", plate_num),
          plate = plate_num,
          well = well,
          wavelength = wavelength,
          absorbance = abs_value,
          sample_label = sample_label,
          stype = stype,
          antigen = antigen,
          feature = feature,
          description = description,
          specimen_id = specimen_id,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  combined <- as.data.frame(dplyr::bind_rows(all_rows), stringsAsFactors = FALSE)

  cat("    → Combined rows:", nrow(combined), "\n")
  cat("    → Unique plates:", length(unique(combined$plateid)), "\n")
  cat("    → Wavelengths:", paste(sort(unique(combined$wavelength)), collapse = ", "), "\n")

  combined
}


#' Process ELISA file(s) into standardized format
#'
#' Main entry point for processing one or more ELISA Excel files.
#' Reads both the results and plate_map sheets, parses the data,
#' and returns combined results in a format compatible with the
#' layout template generation.
#'
#' @param upload_df Data frame from Shiny fileInput (columns: name, datapath)
#' @return List with combined_data, header_list, plate_map, file_metadata
#'
process_elisa_files <- function(upload_df) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  PROCESSING ELISA FILES                                  ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("  Files to process:", nrow(upload_df), "\n")

  all_combined <- list()
  all_headers <- list()
  all_plate_maps <- list()

  for (i in seq_len(nrow(upload_df))) {
    file_path <- upload_df$datapath[i]
    file_name <- upload_df$name[i]
    cat("\n  Processing file:", file_name, "\n")

    tryCatch({
      # Check for required sheets
      sheets <- readxl::excel_sheets(file_path)
      cat("    → Sheets:", paste(sheets, collapse = ", "), "\n")

      has_results <- any(grepl("^results$", sheets, ignore.case = TRUE))
      has_plate_map <- any(grepl("^plate_map$", sheets, ignore.case = TRUE))

      if (!has_results) {
        warning("File ", file_name, " missing 'results' sheet")
        next
      }
      if (!has_plate_map) {
        warning("File ", file_name, " missing 'plate_map' sheet")
        next
      }

      results_sheet <- sheets[grepl("^results$", sheets, ignore.case = TRUE)][1]
      pm_sheet <- sheets[grepl("^plate_map$", sheets, ignore.case = TRUE)][1]

      # Parse results
      parsed_results <- parse_elisa_results_sheet(file_path, results_sheet)

      # Parse plate_map
      plate_map <- parse_elisa_plate_map(file_path, pm_sheet)

      # Combine
      combined <- combine_elisa_data(parsed_results, plate_map, file_name)

      # Build header info - construct incrementally to avoid data.frame recycling issues
      plate_ids <- unique(combined$plateid)
      header <- data.frame(
        plateid = plate_ids,
        stringsAsFactors = FALSE
      )
      header$source_file          <- file_name
      header$file_name            <- file_name
      header$acquisition_date     <- parsed_results$file_metadata$acquisition_date %||% NA_character_
      header$reader_serial_number <- NA_character_
      header$rp1_pmt_volts        <- NA_character_
      header$rp1_target           <- NA_character_
      header$autoloading_range    <- parsed_results$autoloading_range %||% NA_character_
      header$absorbance_id        <- parsed_results$file_metadata$absorbance_id %||% NA_character_
      header$original_filename    <- parsed_results$file_metadata$filename %||% file_name

      all_combined[[file_name]] <- combined
      all_headers[[file_name]] <- header
      all_plate_maps[[file_name]] <- plate_map

    }, error = function(e) {
      cat("    ✗ ERROR:", conditionMessage(e), "\n")
      showNotification(
        paste("Failed to read ELISA file:", file_name, "-", conditionMessage(e)),
        type = "error", duration = 10
      )
    })
  }

  # Combine across files
  combined_data <- as.data.frame(dplyr::bind_rows(all_combined), stringsAsFactors = FALSE)
  header_list <- all_headers
  plate_map_combined <- as.data.frame(dplyr::bind_rows(all_plate_maps), stringsAsFactors = FALSE)

  cat("\n  Total combined rows:", nrow(combined_data), "\n")
  cat("  Total plates:", length(unique(combined_data$plateid)), "\n")

  list(
    combined_data = combined_data,
    header_list = header_list,
    plate_map = plate_map_combined,
    all_plate_maps = all_plate_maps
  )
}



# ---- ELISA LAYOUT TEMPLATE GENERATION ----

#' Generate ELISA Layout Template
#'
#' Creates an Excel layout template for ELISA batch uploads.
#' The structure mirrors the bead array layout template but adapted for
#' ELISA-specific data (simplex, absorbance values, wavelengths).
#'
#' @param combined_data Combined ELISA data from process_elisa_files()
#' @param plate_map Raw plate_map from the ELISA file
#' @param header_list Header metadata list
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#' @param n_wells Number of wells on the plate
#' @param output_file Path to save the Excel template
#' @param project_id Project identifier
#' @param description_status Description parsing status
#' @param delimiter Delimiter for description parsing
#' @param element_order Element order for sample descriptions
#' @param bcs_element_order Element order for blank/control/standard descriptions
#'
generate_elisa_layout_template <- function(combined_data,
                                           plate_map,
                                           header_list,
                                           study_accession,
                                           experiment_accession,
                                           n_wells,
                                           output_file,
                                           project_id = NULL,
                                           description_status = NULL,
                                           delimiter = "_",
                                           element_order = NULL,
                                           bcs_element_order = NULL) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  GENERATING ELISA LAYOUT TEMPLATE                        ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # Defensive: ensure plain data.frames (not tibbles) throughout
  combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)
  plate_map     <- as.data.frame(plate_map, stringsAsFactors = FALSE)

  # Defensive: ensure scalar arguments are truly scalar
  study_accession      <- as.character(study_accession)[1]
  experiment_accession <- as.character(experiment_accession)[1]
  n_wells              <- as.integer(n_wells)[1]
  if (is.null(project_id)) project_id <- NA_character_
  project_id           <- as.character(project_id)[1]

  wb <- createWorkbook()
  bold_style <- createStyle(textDecoration = "bold")
  italic_style <- createStyle(textDecoration = "italic")

  # Resolve description config
  if (is.null(description_status)) {
    description_status <- list(
      has_content = TRUE,
      has_sufficient_elements = TRUE,
      min_elements_found = 3,
      required_elements = 3,
      checked = FALSE
    )
  }

  if (is.null(delimiter)) delimiter <- "_"
  if (is.null(element_order)) element_order <- c("PatientID", "TimePeriod", "DilutionFactor")
  if (is.null(bcs_element_order)) bcs_element_order <- c("Source", "DilutionFactor")

  use_defaults <- !description_status$has_content || !description_status$has_sufficient_elements

  # ---- SHEET 1: plate_id ----
  cat("  Building plate_id sheet...\n")
  plate_id_df <- tryCatch(
    build_elisa_plate_id(
      combined_data = combined_data,
      header_list = header_list,
      study_accession = study_accession,
      experiment_accession = experiment_accession,
      n_wells = n_wells,
      project_id = project_id
    ),
    error = function(e) {
      cat("  ERROR in build_elisa_plate_id:", conditionMessage(e), "\n")
      cat("    combined_data class:", paste(class(combined_data), collapse=", "), "rows:", nrow(combined_data), "\n")
      cat("    header_list length:", length(header_list), "\n")
      cat("    project_id class:", class(project_id), "value:", deparse(project_id), "\n")
      stop(e)
    }
  )

  # ---- SHEET 2: plates_map ----
  cat("  Building plates_map sheet...\n")
  plates_map_df <- tryCatch(
    build_elisa_plates_map(
    combined_data = combined_data,
    plate_map = plate_map,
    plate_id_df = plate_id_df,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id,
    description_status = description_status,
    delimiter = delimiter,
    element_order = element_order,
    bcs_element_order = bcs_element_order
  ),
    error = function(e) {
      cat("  ERROR in build_elisa_plates_map:", conditionMessage(e), "\n")
      stop(e)
    }
  )

  # ---- SHEET 3: subject_groups ----
  cat("  Building subject_groups sheet...\n")
  subject_groups_df <- build_elisa_subject_groups(
    plates_map = plates_map_df,
    study_accession = study_accession,
    description_status = description_status,
    delimiter = delimiter,
    element_order = element_order
  )

  # ---- SHEET 4: timepoint ----
  cat("  Building timepoint sheet...\n")
  timepoint_df <- build_elisa_timepoint(
    plates_map = plates_map_df,
    study_accession = study_accession
  )

  # ---- SHEET 5: antigen_list ----
  cat("  Building antigen_list sheet...\n")
  antigen_list_df <- build_elisa_antigen_list(
    combined_data = combined_data,
    study_accession = study_accession,
    experiment_accession = experiment_accession,
    project_id = project_id
  )

  # ---- SHEET 6: assay_response_long ----
  cat("  Building assay_response_long sheet...\n")
  assay_response_long_df <- build_elisa_assay_response_long(
    combined_data = combined_data,
    plate_id_df = plate_id_df,
    project_id = project_id,
    study_accession = study_accession,
    experiment_accession = experiment_accession
  )

  # ---- Calculate nominal_sample_dilution ----
  cat("  Calculating nominal_sample_dilution...\n")
  nsd <- calculate_elisa_nominal_sample_dilution(plates_map_df, project_id,
                                                 study_accession, experiment_accession)
  if (!is.null(nsd) && nrow(nsd) > 0) {
    cat("    → nsd:", nrow(nsd), "rows, merging into plate_id and plates_map\n")

    # Drop existing nominal_sample_dilution before merge to avoid .x/.y collision
    plate_id_df$nominal_sample_dilution <- NULL
    plate_id_df <- merge(plate_id_df, nsd,
                         by = c("project_id", "study_name", "experiment_name", "plateid"),
                         all.x = TRUE)
    plate_id_df$nominal_sample_dilution[is.na(plate_id_df$nominal_sample_dilution)] <- "1"

    # plates_map_df already has nominal_sample_dilution = "1" from builder
    plates_map_df$nominal_sample_dilution <- NULL
    plates_map_df <- merge(plates_map_df, nsd,
                           by = c("project_id", "study_name", "experiment_name", "plateid"),
                           all.x = TRUE)
    plates_map_df$nominal_sample_dilution[is.na(plates_map_df$nominal_sample_dilution)] <- "1"
  } else {
    cat("    → nsd: NULL or empty, keeping defaults\n")
  }

  # ---- SHEET 8: cell_valid ----
  cell_valid_table <- data.frame(
    l_asy_constraint_method = c("default", "user_defined", "range_of_blanks", "geometric_mean_of_blanks")
  )

  # ---- Write all sheets ----
  workbook <- list(
    plate_id = plate_id_df,
    subject_groups = subject_groups_df,
    timepoint = timepoint_df,
    antigen_list = antigen_list_df,
    plates_map = plates_map_df,
    assay_response_long = assay_response_long_df,
    cell_valid = cell_valid_table
  )

  for (sheet_name in names(workbook)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, workbook[[sheet_name]])
    addStyle(wb, sheet_name, style = bold_style, rows = 1,
             cols = 1:ncol(workbook[[sheet_name]]), gridExpand = TRUE)
  }

  # ---- Add README if needed ----
  if (use_defaults) {
    readme_lines <- c(
      "IMPORTANT: Default values have been applied to sample wells.",
      "",
      "Please review and update the following sheets:",
      "1. plates_map - verify subject_id, specimen_dilution_factor, timepoint",
      "2. subject_groups - verify subject_id, update group assignments",
      "3. timepoint - ensure all timepoints are listed",
      "",
      paste("Generated:", Sys.time()),
      paste("Study:", study_accession),
      paste("Experiment:", experiment_accession)
    )
    readme_df <- data.frame(Notes = readme_lines, stringsAsFactors = FALSE)
    addWorksheet(wb, "README_IMPORTANT")
    writeData(wb, "README_IMPORTANT", readme_df, startRow = 1, startCol = 1, colNames = FALSE)
  }

  saveWorkbook(wb, output_file, overwrite = TRUE)

  cat("\n  ✓ ELISA layout template saved to:", output_file, "\n")
  cat("    → Sheets:", paste(names(workbook), collapse = ", "), "\n")
  cat("    → assay_response_long:", nrow(assay_response_long_df), "rows\n")
}



# ---- ELISA LAYOUT TEMPLATE HELPER FUNCTIONS ----

#' Build plate_id sheet for ELISA layout template
#'
#' Follows the same pattern as the bead array build_plate_id_df():
#' starts from header_list via do.call(rbind, ...), then adds scalar columns
#' incrementally. ELISA-specific columns (wavelengths, autoloading_range,
#' original_filename, absorbance_id) replace bead-array columns
#' (reader_serial_number, rp1_pmt_volts, rp1_target).
build_elisa_plate_id <- function(combined_data, header_list, study_accession,
                                 experiment_accession, n_wells, project_id) {

  cat("    [build_elisa_plate_id] Starting...\n")

  # --- Build base from header_list (mirrors bead array approach) ---
  plate_id <- tryCatch({
    do.call(rbind, header_list)
  }, error = function(e) {
    cat("    [build_elisa_plate_id] do.call(rbind, header_list) failed:", conditionMessage(e), "\n")
    cat("    Falling back to combined_data extraction\n")
    cd <- as.data.frame(combined_data, stringsAsFactors = FALSE)
    unique(cd[, intersect(c("plateid", "source_file", "plate"), names(cd)), drop = FALSE])
  })

  plate_id <- as.data.frame(plate_id, stringsAsFactors = FALSE)
  rownames(plate_id) <- NULL

  cat("    [build_elisa_plate_id] Base plate_id:", nrow(plate_id), "rows,",
      "cols:", paste(names(plate_id), collapse = ", "), "\n")

  # --- Add scalar columns incrementally ---
  plate_id$study_name       <- study_accession
  plate_id$experiment_name  <- experiment_accession
  plate_id$number_of_wells  <- n_wells
  plate_id$project_id       <- project_id

  # --- Ensure required columns exist ---
  if (!"plateid" %in% names(plate_id)) {
    if ("source_file" %in% names(plate_id)) {
      plate_id$plateid <- plate_id$source_file
    } else {
      plate_id$plateid <- paste0("plate_", seq_len(nrow(plate_id)))
    }
  }

  if (!"plate_number" %in% names(plate_id)) {
    plate_id$plate_number <- plate_id$plateid
  }

  # plate_filename
  if ("file_name" %in% names(plate_id)) {
    names(plate_id)[names(plate_id) == "file_name"] <- "plate_filename"
  } else if (!"plate_filename" %in% names(plate_id)) {
    if ("source_file" %in% names(plate_id)) {
      plate_id$plate_filename <- plate_id$source_file
    } else {
      plate_id$plate_filename <- names(header_list)
    }
  }

  # plate_id column (unique identifier)
  if (!"plate_id" %in% names(plate_id)) {
    plate_id$plate_id <- paste0(plate_id$plate_filename, "_", plate_id$plateid)
  }

  # ELISA-specific metadata (fill from header_list if present, otherwise NA)
  if (!"acquisition_date" %in% names(plate_id))    plate_id$acquisition_date <- NA_character_
  if (!"autoloading_range" %in% names(plate_id))   plate_id$autoloading_range <- NA_character_
  if (!"original_filename" %in% names(plate_id))   plate_id$original_filename <- NA_character_
  if (!"absorbance_id" %in% names(plate_id))       plate_id$absorbance_id <- NA_character_

  # Wavelengths: computed from combined_data (pipe-delimited per plate)
  combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)
  wl_agg <- aggregate(
    wavelength ~ plateid,
    data = combined_data,
    FUN = function(x) paste(sort(unique(x)), collapse = "|")
  )
  names(wl_agg)[2] <- "wavelengths"
  plate_id <- merge(plate_id, wl_agg, by = "plateid", all.x = TRUE)
  if (!"wavelengths" %in% names(plate_id)) plate_id$wavelengths <- NA_character_

  # Remove bead-array-specific columns if they exist
  bead_array_cols <- c("reader_serial_number", "rp1_pmt_volts", "rp1_target")
  plate_id[bead_array_cols] <- NULL

  # --- Subset to required columns ---
  required_cols <- c("project_id", "study_name", "experiment_name", "number_of_wells",
                     "plate_number", "plateid", "plate_id", "plate_filename",
                     "acquisition_date", "wavelengths", "autoloading_range",
                     "original_filename", "absorbance_id")

  missing_cols <- setdiff(required_cols, names(plate_id))
  for (mc in missing_cols) {
    plate_id[[mc]] <- NA_character_
  }
  plate_id <- plate_id[, required_cols, drop = FALSE]

  cat("    → plate_id:", nrow(plate_id), "plates,", ncol(plate_id), "cols\n")
  plate_id
}


#' Build plates_map sheet for ELISA layout template
build_elisa_plates_map <- function(combined_data, plate_map, plate_id_df,
                                   study_accession, experiment_accession, project_id,
                                   description_status, delimiter, element_order,
                                   bcs_element_order) {

  # Create one row per plate x well from the plate_map
  # Ensure plain data.frames throughout
  plate_map <- as.data.frame(plate_map, stringsAsFactors = FALSE)
  combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)

  unique_plate_wells <- unique(plate_map[, c("Plate", "Well", "SType",
                                             "Antigen", "Feature"), drop = FALSE])

  # Add Description if present
  if ("Description" %in% names(plate_map)) {
    unique_plate_wells <- unique(plate_map[, c("Plate", "Well", "SType", "Antigen",
                                               "Feature", "Description"), drop = FALSE])
  }

  # Build plateid
  unique_plate_wells$plateid <- paste0("plate_", unique_plate_wells$Plate)
  unique_plate_wells$well <- unique_plate_wells$Well

  # Map specimen_type from SType
  unique_plate_wells$specimen_type <- unique_plate_wells$SType

  # Parse descriptions to extract subject_id, dilution, timepoint
  if ("Description" %in% names(unique_plate_wells)) {
    parsed <- parse_elisa_descriptions(
      descriptions = unique_plate_wells$Description,
      stypes = unique_plate_wells$SType,
      delimiter = delimiter,
      element_order = element_order,
      bcs_element_order = bcs_element_order,
      use_defaults = !description_status$has_content || !description_status$has_sufficient_elements
    )
    unique_plate_wells$subject_id <- parsed$subject_id
    unique_plate_wells$specimen_dilution_factor <- parsed$specimen_dilution_factor
    unique_plate_wells$timepoint_tissue_abbreviation <- parsed$timepoint_tissue_abbreviation
    unique_plate_wells$specimen_source <- parsed$specimen_source
  } else {
    unique_plate_wells$subject_id <- "1"
    unique_plate_wells$specimen_dilution_factor <- 1
    unique_plate_wells$timepoint_tissue_abbreviation <- "T0"
    # Default specimen_source based on SType
    unique_plate_wells$specimen_source <- ifelse(
      substr(unique_plate_wells$SType, 1, 1) == "X", "sample", ""
    )
  }

  # Add Specimen_id if available from plate_map
  if ("Specimen_id" %in% names(plate_map)) {
    spec_lookup <- unique(plate_map[, c("Plate", "Well", "Specimen_id"), drop = FALSE])
    spec_lookup$plateid <- paste0("plate_", spec_lookup$Plate)
    unique_plate_wells <- merge(unique_plate_wells, spec_lookup[, c("plateid", "Well", "Specimen_id")],
                                by.x = c("plateid", "Well"), by.y = c("plateid", "Well"),
                                all.x = TRUE)
    unique_plate_wells$biosample_id_barcode <- unique_plate_wells$Specimen_id
  } else {
    unique_plate_wells$biosample_id_barcode <- NA_character_
  }

  # Fill biosample_id_barcode from Sample grid identifiers in the results sheet.
  # combined_data$sample_label contains values like "Std0081", "Blank1", etc.
  # from the well-position grid labelled "Sample" in the results file.
  if ("sample_label" %in% names(combined_data)) {
    sl_lookup <- unique(combined_data[, c("plateid", "well", "sample_label"), drop = FALSE])
    sl_lookup <- sl_lookup[!is.na(sl_lookup$sample_label) & trimws(sl_lookup$sample_label) != "", ]
    if (nrow(sl_lookup) > 0) {
      # Merge sample_label into unique_plate_wells
      unique_plate_wells <- merge(unique_plate_wells, sl_lookup,
                                  by.x = c("plateid", "well"), by.y = c("plateid", "well"),
                                  all.x = TRUE)
      # Fill biosample_id_barcode where it's still empty/NA
      empty_barcode <- is.na(unique_plate_wells$biosample_id_barcode) |
                       trimws(unique_plate_wells$biosample_id_barcode) == ""
      if (any(empty_barcode)) {
        unique_plate_wells$biosample_id_barcode[empty_barcode] <-
          unique_plate_wells$sample_label[empty_barcode]
      }
      # Clean up the temp column
      unique_plate_wells$sample_label <- NULL
    }
  }

  # Final fallback: use well position for any still-empty biosample_id_barcode
  still_empty <- is.na(unique_plate_wells$biosample_id_barcode) |
                 trimws(unique_plate_wells$biosample_id_barcode) == ""
  if (any(still_empty)) {
    unique_plate_wells$biosample_id_barcode[still_empty] <- unique_plate_wells$well[still_empty]
  }

  # Build final plates_map incrementally (avoid data.frame recycling issues)
  plates_map <- data.frame(
    well = unique_plate_wells$well,
    stringsAsFactors = FALSE
  )
  plates_map$project_id                     <- project_id
  plates_map$study_name                     <- study_accession
  plates_map$plate_number                   <- unique_plate_wells$plateid
  plates_map$nominal_sample_dilution        <- "1"
  plates_map$specimen_type                  <- unique_plate_wells$specimen_type
  plates_map$specimen_source                <- unique_plate_wells$specimen_source
  plates_map$specimen_dilution_factor       <- unique_plate_wells$specimen_dilution_factor
  plates_map$experiment_name                <- experiment_accession
  plates_map$feature                        <- unique_plate_wells$Feature
  plates_map$subject_id                     <- unique_plate_wells$subject_id
  plates_map$biosample_id_barcode           <- unique_plate_wells$biosample_id_barcode
  plates_map$timepoint_tissue_abbreviation  <- unique_plate_wells$timepoint_tissue_abbreviation
  plates_map$plateid                        <- unique_plate_wells$plateid
  plates_map$plate_id                       <- paste0(study_accession, "_", experiment_accession, "_", unique_plate_wells$plateid)
  # Join plate_id from plate_id_df
  if ("plate_id" %in% names(plate_id_df)) {
    pid_lookup <- unique(plate_id_df[, c("plateid", "plate_id"), drop = FALSE])
    plates_map$plate_id <- NULL
    plates_map <- merge(plates_map, pid_lookup, by = "plateid", all.x = TRUE)
  }

  cat("    → plates_map:", nrow(plates_map), "rows\n")
  # propagate plate column
  plates_map$plate <- plates_map$plateid

  plates_map <- plates_map[
    , append(names(plates_map)[names(plates_map) != "plate"],
             "plate",
             after = match("plateid", names(plates_map)))
  ]

  plates_map
}


#' Parse ELISA descriptions
#'
#' Mirrors parse_all_descriptions() but adapted for ELISA plate_map format
parse_elisa_descriptions <- function(descriptions, stypes, delimiter = "_",
                                     element_order = c("PatientID", "TimePeriod", "DilutionFactor"),
                                     bcs_element_order = c("Source", "DilutionFactor"),
                                     use_defaults = FALSE) {

  n <- length(descriptions)
  result <- list(
    subject_id = rep(NA_character_, n),
    specimen_dilution_factor = rep(1, n),
    timepoint_tissue_abbreviation = rep(NA_character_, n),
    specimen_source = rep(NA_character_, n)
  )

  for (i in seq_len(n)) {
    desc <- descriptions[i]
    stype <- stypes[i]

    if (is.na(desc) || trimws(desc) == "") {
      result$subject_id[i] <- if (use_defaults) "1" else NA_character_
      result$specimen_dilution_factor[i] <- 1
      result$timepoint_tissue_abbreviation[i] <- if (use_defaults) "T0" else NA_character_
      result$specimen_source[i] <- if (substr(stype, 1, 1) == "X") "sample" else ""
      next
    }

    parts <- trimws(strsplit(desc, delimiter, fixed = TRUE)[[1]])

    if (substr(stype, 1, 1) == "X") {
      # Sample: use element_order
      # Default specimen_source to "sample" for X-type
      result$specimen_source[i] <- "sample"
      for (j in seq_along(element_order)) {
        if (j <= length(parts)) {
          elem <- element_order[j]
          if (elem == "PatientID") result$subject_id[i] <- parts[j]
          else if (elem == "TimePeriod") result$timepoint_tissue_abbreviation[i] <- parts[j]
          else if (elem == "DilutionFactor") {
            val <- suppressWarnings(as.numeric(parts[j]))
            if (!is.na(val)) result$specimen_dilution_factor[i] <- val
          }
        }
      }
    } else if (substr(stype, 1, 1) %in% c("S", "B", "C")) {
      # Standard/Blank/Control: use bcs_element_order
      for (j in seq_along(bcs_element_order)) {
        if (j <= length(parts)) {
          elem <- bcs_element_order[j]
          if (elem == "Source") result$specimen_source[i] <- parts[j]
          else if (elem == "DilutionFactor") {
            val <- suppressWarnings(as.numeric(parts[j]))
            if (!is.na(val)) result$specimen_dilution_factor[i] <- val
          }
        }
      }
    }

    # Apply defaults where still NA
    if (use_defaults) {
      if (is.na(result$subject_id[i])) result$subject_id[i] <- "1"
      if (is.na(result$timepoint_tissue_abbreviation[i])) result$timepoint_tissue_abbreviation[i] <- "T0"
      if (is.na(result$specimen_dilution_factor[i])) result$specimen_dilution_factor[i] <- 1
    }
  }

  result
}


#' Build subject_groups sheet for ELISA
build_elisa_subject_groups <- function(plates_map, study_accession,
                                       description_status, delimiter, element_order) {

  sample_rows <- plates_map[plates_map$specimen_type == "X", , drop = FALSE]

  if (nrow(sample_rows) == 0) {
    sg <- data.frame(subject_id = "1", stringsAsFactors = FALSE)
    sg$study_name <- study_accession
    sg$groupa <- "Unknown"
    sg$groupb <- "Unknown"
    return(sg)
  }

  unique_subjects <- unique(sample_rows$subject_id)
  unique_subjects <- unique_subjects[!is.na(unique_subjects) & unique_subjects != ""]

  if (length(unique_subjects) == 0) unique_subjects <- "1"

  sg <- data.frame(subject_id = unique_subjects, stringsAsFactors = FALSE)
  sg$study_name <- study_accession
  sg$groupa <- "Unknown"
  sg$groupb <- "Unknown"
  sg
}


#' Build timepoint sheet for ELISA
build_elisa_timepoint <- function(plates_map, study_accession) {

  sample_rows <- plates_map[plates_map$specimen_type == "X", , drop = FALSE]

  timepoints <- unique(sample_rows$timepoint_tissue_abbreviation)
  timepoints <- timepoints[!is.na(timepoints) & timepoints != ""]

  if (length(timepoints) == 0) timepoints <- "T0"

  tp <- data.frame(
    timepoint_tissue_abbreviation = timepoints,
    description = timepoints,
    stringsAsFactors = FALSE
  )
  tp$study_name          <- study_accession
  tp$tissue_type         <- "blood"
  tp$tissue_subtype      <- "serum"
  tp$min_time_since_day_0 <- 0
  tp$max_time_since_day_0 <- 0
  tp$time_unit           <- "day"

  tp
}


#' Build antigen_list sheet for ELISA
build_elisa_antigen_list <- function(combined_data, study_accession,
                                     experiment_accession, project_id) {

  antigens <- unique(combined_data$antigen)
  antigens <- antigens[!is.na(antigens)]

  # Build incrementally to avoid recycling issues
  antigen_df <- data.frame(
    antigen_label_on_plate = antigens,
    antigen_abbreviation = antigens,
    stringsAsFactors = FALSE
  )
  antigen_df$project_id                       <- project_id
  antigen_df$study_name                       <- study_accession
  antigen_df$experiment_name                  <- experiment_accession
  antigen_df$standard_curve_max_concentration <- 100000
  antigen_df$l_asy_constraint_method          <- "default"
  antigen_df$l_asy_min_constraint             <- 0
  antigen_df$l_asy_max_constraint             <- 0
  antigen_df$antigen_family                   <- NA_character_
  antigen_df$antigen_name                     <- NA_character_
  antigen_df$virus_bacterial_strain           <- NA_character_
  antigen_df$antigen_source                   <- NA_character_
  antigen_df$catalog_number                   <- NA_character_

  antigen_df
}


#' Build assay_response_long sheet for ELISA
#'
#' Each row represents one well's absorbance reading at a specific wavelength.
#' For ELISA, the feature column encodes the wavelength.
build_elisa_assay_response_long <- function(combined_data, plate_id_df,
                                            project_id, study_accession,
                                            experiment_accession) {

  # Ensure plain data.frame
  combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)

  # Build incrementally to avoid recycling issues with scalar project_id
  assay_response <- data.frame(
    plateid = combined_data$plateid,
    well = combined_data$well,
    antigen = combined_data$antigen,
    feature = combined_data$feature,
    wavelength = combined_data$wavelength,
    assay_response = round(as.double(combined_data$absorbance), 4),
    assay_bead_count = NA_integer_,  # Not applicable for ELISA
    stringsAsFactors = FALSE
  )
  assay_response$project_id       <- project_id
  assay_response$study_name       <- study_accession
  assay_response$experiment_name  <- experiment_accession

  cat("    → assay_response_long:", nrow(assay_response), "rows\n")
  assay_response
}


#' Build ELISA-specific metadata sheet (DEPRECATED)
#'
#' NOTE: As of the current version, ELISA metadata fields (wavelengths,
#' autoloading_range, original_filename, absorbance_id) are now included
#' directly in the plate_id sheet. This function is retained for backward
#' compatibility if reading older layout templates that still have a
#' separate elisa_metadata sheet.
build_elisa_metadata_sheet <- function(combined_data, header_list) {

  combined_data <- as.data.frame(combined_data, stringsAsFactors = FALSE)

  # Collect unique wavelengths per plate
  wavelength_summary <- aggregate(
    wavelength ~ plateid + source_file,
    data = combined_data,
    FUN = function(x) paste(sort(unique(x)), collapse = "|")
  )
  names(wavelength_summary)[3] <- "wavelengths"

  # PRE-INITIALIZE columns to NA before the loop.
  # This prevents the R bug where df$new_col[idx] <- value on a non-existent
  # column creates a vector of length max(idx) instead of nrow(df).
  wavelength_summary$autoloading_range <- NA_character_
  wavelength_summary$original_filename <- NA_character_
  wavelength_summary$absorbance_id     <- NA_character_

  # Now fill from header_list
  for (fname in names(header_list)) {
    h <- as.data.frame(header_list[[fname]], stringsAsFactors = FALSE)
    for (j in seq_len(nrow(h))) {
      match_idx <- which(wavelength_summary$source_file == fname &
                           wavelength_summary$plateid == h$plateid[j])
      if (length(match_idx) > 0) {
        if ("autoloading_range" %in% names(h)) {
          wavelength_summary$autoloading_range[match_idx] <- h$autoloading_range[j]
        }
        if ("original_filename" %in% names(h)) {
          wavelength_summary$original_filename[match_idx] <- h$original_filename[j]
        }
        if ("absorbance_id" %in% names(h)) {
          wavelength_summary$absorbance_id[match_idx] <- h$absorbance_id[j]
        }
      }
    }
  }

  cat("    → elisa_metadata:", nrow(wavelength_summary), "rows\n")
  wavelength_summary
}


#' Calculate nominal sample dilution for ELISA plates
calculate_elisa_nominal_sample_dilution <- function(plates_map, project_id,
                                                    study_accession, experiment_accession) {

  sample_rows <- plates_map[
    substr(plates_map$specimen_type, 1, 1) == "X" &
      !is.na(plates_map$specimen_dilution_factor),
  ]

  if (nrow(sample_rows) == 0) return(NULL)

  nsd_df <- aggregate(
    specimen_dilution_factor ~ project_id + study_name + experiment_name + plateid,
    data = sample_rows,
    FUN = function(x) paste(sort(unique(x)), collapse = "|")
  )
  names(nsd_df)[names(nsd_df) == "specimen_dilution_factor"] <- "nominal_sample_dilution"

  nsd_df
}




# ---- ELISA VALIDATION FUNCTIONS ----
# ---- Validate ELISA plate metadata (plate_id sheet) ----
#'
#' ELISA-specific replacement for validate_batch_plate_metadata().
#' Key differences from bead array validation:
#'   - Does NOT require rp1_pmt_volts or rp1_target (bead-array-specific)
#'   - Does NOT require plate_id to match filename (ELISA has multiple plates per file)
#'   - DOES check ELISA-specific fields: wavelengths, autoloading_range
#'   - Validates acquisition_date format
#'   - Checks plate consistency between plate_id and assay data
#'
#' @param plate_id_data Data frame from the plate_id sheet
#' @param assay_response_long Optional assay response data for cross-checking
#' @return List with is_valid, messages, warnings
#'
validate_elisa_plate_metadata <- function(plate_id_data, assay_response_long = NULL) {

  cat("\n>>> ENTERING validate_elisa_plate_metadata <<<\n")
  cat("  plate_id_data rows:", nrow(plate_id_data), "\n")
  cat("  plate_id_data cols:", paste(names(plate_id_data), collapse = ", "), "\n")

  message_list <- c()
  warning_list <- c()

  # Check 1: Required columns for ELISA plate_id
  required_cols <- c("plateid", "plate_id", "acquisition_date")
  # file_name or plate_filename - accept either
  has_filename <- "file_name" %in% names(plate_id_data) || "plate_filename" %in% names(plate_id_data)

  missing_cols <- setdiff(required_cols, names(plate_id_data))
  if (length(missing_cols) > 0) {
    message_list <- c(message_list, paste0(
      "MISSING COLUMNS: The following required columns are missing from plate_id:\n",
      paste("  - ", missing_cols, collapse = "\n")
    ))
  }
  if (!has_filename) {
    message_list <- c(message_list,
      "MISSING COLUMN: plate_id sheet must have either 'file_name' or 'plate_filename' column."
    )
  }

  # Check 2: No empty/NA plateids
  if ("plateid" %in% names(plate_id_data)) {
    empty_plates <- is.na(plate_id_data$plateid) | trimws(plate_id_data$plateid) == ""
    if (any(empty_plates)) {
      message_list <- c(message_list, paste0(
        "EMPTY PLATE IDS: ", sum(empty_plates), " row(s) in plate_id have empty plateid values."
      ))
    }
  }

  # Check 3: No duplicate plateids
  if ("plateid" %in% names(plate_id_data)) {
    dup_plates <- duplicated(plate_id_data$plateid)
    if (any(dup_plates)) {
      message_list <- c(message_list, paste0(
        "DUPLICATE PLATE IDS: The following plateids appear more than once: ",
        paste(unique(plate_id_data$plateid[dup_plates]), collapse = ", ")
      ))
    }
  }

  # Check 4: Wavelengths present and reasonable (ELISA-specific)
  if ("wavelengths" %in% names(plate_id_data)) {
    empty_wl <- is.na(plate_id_data$wavelengths) | trimws(plate_id_data$wavelengths) == ""
    if (any(empty_wl)) {
      warning_list <- c(warning_list, paste0(
        "MISSING WAVELENGTHS: ", sum(empty_wl), " plate(s) have no wavelength information."
      ))
    }
  } else {
    warning_list <- c(warning_list,
      "No 'wavelengths' column in plate_id. Wavelength info will not be stored."
    )
  }

  # Check 5: Acquisition date format (reuse existing helpers if available)
  if ("acquisition_date" %in% names(plate_id_data)) {
    acq_dates <- plate_id_data$acquisition_date
    empty_dates <- is.na(acq_dates) | trimws(acq_dates) == ""
    if (all(empty_dates)) {
      warning_list <- c(warning_list,
        "All acquisition dates are empty. Consider adding acquisition timestamps."
      )
    }
  }

  # Check 6: Cross-check with assay_response_long if provided
  if (!is.null(assay_response_long) && "plateid" %in% names(plate_id_data)) {
    response_plates <- unique(assay_response_long$plateid)
    layout_plates <- unique(plate_id_data$plateid)

    missing_in_layout <- setdiff(response_plates, layout_plates)
    if (length(missing_in_layout) > 0) {
      message_list <- c(message_list, paste0(
        "PLATE MISMATCH: The following plates are in assay_response_long but not in plate_id:\n",
        paste("  - ", missing_in_layout, collapse = "\n")
      ))
    }

    extra_in_layout <- setdiff(layout_plates, response_plates)
    if (length(extra_in_layout) > 0) {
      warning_list <- c(warning_list, paste0(
        "Extra plates in plate_id not found in assay_response_long: ",
        paste(extra_in_layout, collapse = ", ")
      ))
    }
  }

  is_valid <- length(message_list) == 0

  cat("  ELISA plate metadata validation:\n")
  cat("    → Valid:", is_valid, "\n")
  if (length(message_list) > 0) cat("    → Errors:", paste(message_list, collapse = "; "), "\n")
  if (length(warning_list) > 0) cat("    → Warnings:", paste(warning_list, collapse = "; "), "\n")
  cat(">>> EXITING validate_elisa_plate_metadata - is_valid:", is_valid, "<<<\n")

  list(
    is_valid = is_valid,
    messages = message_list,
    warnings = warning_list
  )
}


# ---- Validate ELISA layout template data ----
#'
#' Validates the uploaded ELISA layout template before database upload.
#'
#' @param assay_response_long Assay response data
#' @param antigen_list Antigen list from layout
#' @param plates_map Plates map from layout
#' @param elisa_metadata ELISA metadata sheet
#' @return List with is_valid, messages, warnings
#'
validate_elisa_layout_data <- function(assay_response_long, antigen_list,
                                       plates_map, elisa_metadata = NULL) {

  result <- list(
    is_valid = TRUE,
    messages = c(),
    warnings = c()
  )

  # Check 1: assay_response_long exists and has data
  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages, "assay_response_long sheet is empty or missing")
    return(result)
  }

  # Check 2: Required columns in assay_response_long
  required_cols <- c("plateid", "well", "antigen", "assay_response", "wavelength")
  missing <- setdiff(required_cols, names(assay_response_long))
  if (length(missing) > 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages,
                         paste("assay_response_long missing columns:", paste(missing, collapse = ", ")))
  }

  # Check 3: Antigen alignment
  if (!is.null(antigen_list) && nrow(antigen_list) > 0) {
    layout_antigens <- unique(antigen_list$antigen_label_on_plate)
    response_antigens <- unique(assay_response_long$antigen)

    missing_antigens <- setdiff(layout_antigens, response_antigens)
    if (length(missing_antigens) > 0) {
      result$warnings <- c(result$warnings,
                           paste("Antigens in layout but not in data:", paste(missing_antigens, collapse = ", ")))
    }
  }

  # Check 4: All plates in assay_response have matching plates_map entries
  if (!is.null(plates_map) && nrow(plates_map) > 0) {
    response_plates <- unique(assay_response_long$plateid)
    map_plates <- unique(plates_map$plateid)

    missing_plates <- setdiff(response_plates, map_plates)
    if (length(missing_plates) > 0) {
      result$is_valid <- FALSE
      result$messages <- c(result$messages,
                           paste("Plates in response but not in plates_map:", paste(missing_plates, collapse = ", ")))
    }
  }

  # Check 5: Non-negative absorbance values (warn if negative)
  if ("assay_response" %in% names(assay_response_long)) {
    neg_count <- sum(assay_response_long$assay_response < 0, na.rm = TRUE)
    if (neg_count > 0) {
      result$warnings <- c(result$warnings,
                           paste(neg_count, "negative absorbance values detected"))
    }
  }

  # Check 6: Wavelength values are reasonable (typically 200-900 nm)
  if ("wavelength" %in% names(assay_response_long)) {
    wl <- unique(assay_response_long$wavelength)
    wl <- wl[!is.na(wl)]
    bad_wl <- wl[wl < 200 | wl > 900]
    if (length(bad_wl) > 0) {
      result$warnings <- c(result$warnings,
                           paste("Unusual wavelength values:", paste(bad_wl, collapse = ", ")))
    }
  }

  # Check 7: Validate specimen types
  if ("specimen_type" %in% names(plates_map)) {
    valid_types <- c("X", "S", "B", "C")
    actual_types <- unique(substr(plates_map$specimen_type, 1, 1))
    bad_types <- setdiff(actual_types, c(valid_types, NA, ""))
    if (length(bad_types) > 0) {
      result$is_valid <- FALSE
      result$messages <- c(result$messages,
                           paste("Invalid specimen types:", paste(bad_types, collapse = ", ")))
    }
  }

  cat("  ELISA Validation:\n")
  cat("    → Valid:", result$is_valid, "\n")
  if (length(result$messages) > 0) cat("    → Errors:", paste(result$messages, collapse = "; "), "\n")
  if (length(result$warnings) > 0) cat("    → Warnings:", paste(result$warnings, collapse = "; "), "\n")

  result
}



# ---- ELISA SHINY OBSERVERS ----
# ---- Observer: ELISA file upload ----
observeEvent(input$upload_elisa_experiment_files, {
  req(input$upload_elisa_experiment_files)
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  cat("\n======================================================\n")
  cat("         NEW ELISA EXPERIMENT FILES UPLOAD\n")
  cat("======================================================\n")
  cat("Study:", input$readxMap_study_accession, "\n")
  cat("Experiment:", input$readxMap_experiment_accession_import, "\n")
  cat("Files:", nrow(input$upload_elisa_experiment_files), "\n\n")

  # Reset state
  elisa_plate_data(NULL)
  elisa_header_list(list())
  elisa_plate_list(list())
  elisa_assay_response_long(NULL)
  elisa_plate_map_raw(NULL)
  elisa_parsed_results(NULL)
  elisa_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    data_result = NULL,
    data_stored = FALSE
  ))

  tryCatch({
    processed <- process_elisa_files(input$upload_elisa_experiment_files)

    elisa_plate_data(processed$combined_data)
    elisa_header_list(processed$header_list)
    elisa_plate_map_raw(processed$plate_map)

    # Check description field
    desc_data <- processed$plate_map
    if ("Description" %in% names(desc_data)) {
      delimiter <- input$elisa_description_delimiter %||% "_"

      # Only check X-type (sample) descriptions for element sufficiency.
      # B/S/C types use a different, shorter format (bcs_element_order)
      # and should not reduce the element count.
      x_rows <- desc_data[!is.na(desc_data$SType) & substr(desc_data$SType, 1, 1) == "X", , drop = FALSE]
      x_descs <- if (nrow(x_rows) > 0 && "Description" %in% names(x_rows)) {
        x_rows$Description[!is.na(x_rows$Description) & trimws(x_rows$Description) != ""]
      } else {
        character(0)
      }

      all_descs <- desc_data$Description[!is.na(desc_data$Description)]
      has_any_content <- length(all_descs) > 0

      if (length(x_descs) > 0) {
        n_elements <- sapply(x_descs, function(d) length(strsplit(d, delimiter, fixed = TRUE)[[1]]))
        min_elements <- min(n_elements)
        elisa_description_status(list(
          has_content = has_any_content,
          has_sufficient_elements = min_elements >= 3,
          min_elements_found = min_elements,
          required_elements = 3,
          checked = TRUE,
          message = NULL
        ))
      } else if (has_any_content) {
        # Has descriptions but no X-type rows (all B/S/C)
        elisa_description_status(list(
          has_content = TRUE,
          has_sufficient_elements = FALSE,
          min_elements_found = 0,
          required_elements = 3,
          checked = TRUE,
          message = "No sample (X) descriptions found to parse"
        ))
      } else {
        elisa_description_status(list(
          has_content = FALSE,
          has_sufficient_elements = FALSE,
          min_elements_found = 0,
          required_elements = 3,
          checked = TRUE,
          message = "No descriptions found"
        ))
      }
    }

    # Summary
    cat("\n  ELISA upload summary:\n")
    cat("    → Total rows:", nrow(processed$combined_data), "\n")
    cat("    → Plates:", paste(unique(processed$combined_data$plateid), collapse = ", "), "\n")
    cat("    → Antigen(s):", paste(unique(processed$combined_data$antigen), collapse = ", "), "\n")
    cat("    → Feature(s):", paste(unique(processed$combined_data$feature), collapse = ", "), "\n")
    cat("    → Wavelengths:", paste(sort(unique(processed$combined_data$wavelength)), collapse = ", "), "\n")

    showNotification(
      paste("Successfully processed", nrow(input$upload_elisa_experiment_files), "ELISA file(s)"),
      type = "message", duration = 3
    )

  }, error = function(e) {
    cat("  ELISA upload failed:", conditionMessage(e), "\n")
    showNotification(
      paste("ELISA upload failed:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})


# ---- Download handler: ELISA layout template generation ----
output$elisa_blank_layout_file <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_",
           input$readxMap_experiment_accession_import,
           "_elisa_layout_template.xlsx")
  },
  content = function(file) {
    req(elisa_plate_data())

    cat("\n======================================================\n")
    cat("         GENERATING ELISA LAYOUT TEMPLATE\n")
    cat("======================================================\n")

    combined_data <- elisa_plate_data()
    plate_map <- elisa_plate_map_raw()
    desc_status <- elisa_description_status()

    generate_elisa_layout_template(
      combined_data = combined_data,
      plate_map = plate_map,
      header_list = elisa_header_list(),
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$elisa_n_wells_on_plate %||% 96,
      output_file = file,
      project_id = userWorkSpaceID(),
      description_status = desc_status,
      delimiter = input$elisa_description_delimiter %||% "_",
      element_order = if (desc_status$has_sufficient_elements) input$elisa_XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = if (desc_status$has_content) input$elisa_BCSElementOrder else c("Source", "DilutionFactor")
    )

    cat("  ELISA layout template generated!\n")
    cat("======================================================\n\n")
  }
)


# ---- Observer: ELISA layout file upload ----
observeEvent(input$upload_elisa_layout_file, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  project_id <- userWorkSpaceID()
  study_accession <- input$readxMap_study_accession
  experiment_accession <- input$readxMap_experiment_accession_import
  workspace_id <- userWorkSpaceID()
  current_user <- currentuser()

  cat("\n======================================================\n")
  cat("     PROCESSING ELISA LAYOUT FILE UPLOAD\n")
  cat("======================================================\n")
  cat("Study:", study_accession, "\n")
  cat("Experiment:", experiment_accession, "\n")

  # Reset validation state
  elisa_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    data_result = NULL,
    data_stored = FALSE
  ))

  input_upload_layout_file <- input$upload_elisa_layout_file

  if (is.null(input_upload_layout_file)) {
    cat("  No file uploaded\n")
    return(NULL)
  }

  cat("Layout file:", input_upload_layout_file$name, "\n\n")

  tryCatch({
    # Read layout file sheets
    sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
    cat("  Sheets found:", paste(sheets, collapse = ", "), "\n")

    # Check for required sheets
    required_sheets <- c("plates_map", "plate_id", "antigen_list", "assay_response_long")
    missing_sheets <- setdiff(required_sheets, sheets)

    if (length(missing_sheets) > 0) {
      showNotification(
        paste("Missing required sheets:", paste(missing_sheets, collapse = ", ")),
        type = "error", duration = 10
      )
      return(NULL)
    }

    # Read all sheets
    all_sheets <- list()
    for (s in sheets) {
      all_sheets[[s]] <- as.data.frame(readxl::read_excel(input_upload_layout_file$datapath, sheet = s),
                                       stringsAsFactors = FALSE)
    }

    plates_map <- all_sheets[["plates_map"]]
    plate_id_sheet <- all_sheets[["plate_id"]]
    antigen_list <- all_sheets[["antigen_list"]]
    assay_response_long <- all_sheets[["assay_response_long"]]
    subject_map <- all_sheets[["subject_groups"]]
    timepoint_map <- all_sheets[["timepoint"]]
    elisa_metadata <- all_sheets[["elisa_metadata"]]  # may be NULL for new templates

    features <- data.frame(feature = unique(plates_map$feature))
    antigen_list <- merge(antigen_list, features, by = NULL)

    # Fix: readxl can guess assay_response as integer when cells carry no numFmt
    # style and all sampled values happen to look whole-number.  Force double here
    # so the column never reaches dbAppendTable as integer.
    if ("assay_response" %in% names(assay_response_long)) {
      assay_response_long$assay_response <- round(as.double(assay_response_long$assay_response), 4)
    }

    cat("  ✓ All sheets read successfully\n")

    # Add project/user metadata to plate_id_sheet
    plate_id_sheet$workspace_id <- workspace_id
    plate_id_sheet$auth0_user <- current_user

    if ("plate_filename" %in% names(plate_id_sheet)) {
      names(plate_id_sheet)[names(plate_id_sheet) == "plate_filename"] <- "file_name"
    }

    # Add project_id if missing
    if (!"project_id" %in% names(plates_map)) plates_map$project_id <- project_id
    if (!"project_id" %in% names(plate_id_sheet)) plate_id_sheet$project_id <- project_id

    # Store layout sheets (elisa_metadata is now part of plate_id;
    # include it only if present for backward compatibility with older templates)
    elisa_layout_sheets <- list(
      plates_map = plates_map,
      plate_id = plate_id_sheet,
      antigen_list = antigen_list,
      assay_response_long = assay_response_long,
      subject_groups = subject_map,
      timepoint = timepoint_map
    )
    if (!is.null(elisa_metadata)) {
      elisa_layout_sheets[["elisa_metadata"]] <- elisa_metadata
    }
    layout_template_sheets(elisa_layout_sheets)

    # Validate ELISA plate metadata (uses ELISA-specific rules, not bead array)
    validate_metadata_result <- validate_elisa_plate_metadata(
      plate_id_data = plate_id_sheet,
      assay_response_long = assay_response_long
    )

    # Validate ELISA data
    elisa_data_validation <- validate_elisa_layout_data(
      assay_response_long = assay_response_long,
      antigen_list = antigen_list,
      plates_map = plates_map,
      elisa_metadata = elisa_metadata
    )

    # Update validation state
    if (elisa_data_validation$is_valid && validate_metadata_result$is_valid) {
      elisa_validation_state(list(
        is_validated = TRUE,
        is_uploaded = FALSE,
        validation_time = Sys.time(),
        upload_time = NULL,
        metadata_result = validate_metadata_result,
        data_result = elisa_data_validation,
        data_stored = FALSE
      ))

      batch_metadata(plate_id_sheet)

      cat("✓ ELISA VALIDATION PASSED!\n")
      showNotification(
        "ELISA layout validated successfully! Ready to upload.",
        type = "message", duration = 5
      )
    } else {
      elisa_validation_state(list(
        is_validated = FALSE,
        is_uploaded = FALSE,
        validation_time = Sys.time(),
        upload_time = NULL,
        metadata_result = validate_metadata_result,
        data_result = elisa_data_validation,
        data_stored = FALSE
      ))

      cat("✗ ELISA VALIDATION FAILED\n")
      showNotification(
        "ELISA layout validation failed. Please review error messages.",
        type = "error", duration = 10
      )
    }

    cat("======================================================\n\n")

  }, error = function(e) {
    cat("  ELISA layout import error:", conditionMessage(e), "\n")
    showNotification(
      paste("Error importing ELISA layout file:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})


# ---- Observer: ELISA upload to database ----
observeEvent(input$upload_elisa_batch_data, {
  req(elisa_validation_state()$is_validated)
  req(layout_template_sheets())

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  UPLOADING ELISA DATA TO DATABASE                        ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  layout_sheets        <- layout_template_sheets()
  study_accession      <- input$readxMap_study_accession
  experiment_accession <- input$readxMap_experiment_accession_import
  workspace_id         <- userWorkSpaceID()
  auth0_user           <- currentuser()

  tryCatch({

    # ── Get required sheets ───────────────────────────────────────────────
    assay_response <- layout_sheets[["assay_response_long"]]
    plates_map     <- layout_sheets[["plates_map"]]
    plate_id_sheet <- layout_sheets[["plate_id"]]
    antigen_list   <- layout_sheets[["antigen_list"]]
    subject_map    <- layout_sheets[["subject_groups"]]
    timepoint_map  <- layout_sheets[["timepoint"]]
    elisa_metadata <- layout_sheets[["elisa_metadata"]]  # may be NULL

    # Guard: ensure assay_response is double-precision before rename to
    # antibody_mfi — an integer column would cause truncated whole numbers.
    if ("assay_response" %in% names(assay_response)) {
      assay_response$assay_response <- round(
        as.double(assay_response$assay_response), 4
      )
    }

    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    # ── Check for existing plates ─────────────────────────────────────────
    plates_to_upload <- unique(plate_id_sheet$plate_id)
    existing_query <- glue::glue_sql("
      SELECT plate_id
      FROM madi_results.xmap_header
      WHERE study_accession = {study_accession}
        AND experiment_accession IN ({experiment_accession*})
        AND plate_id IN ({plates_to_upload*});
    ", .con = conn)
    existing_plates <- DBI::dbGetQuery(conn, existing_query)

    if (nrow(existing_plates) > 0) {
      showNotification(
        paste("These plates already exist:",
              paste(existing_plates$plate_id, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }

    # ── Prepare header data ───────────────────────────────────────────────
    header_data <- plate_id_sheet
    header_data$study_accession         <- study_accession
    header_data$experiment_accession    <- experiment_accession
    header_data$auth0_user              <- auth0_user
    header_data$workspace_id            <- workspace_id
    header_data$assay_response_variable    <- "absorbance"
    header_data$assay_independent_variable <- "concentration"

    if ("acquisition_date" %in% names(header_data)) {
      cat("  Standardizing acquisition dates for PostgreSQL...\n")
      header_data$acquisition_date <- standardize_date_for_postgres(
        header_data$acquisition_date
      )
      cat("    → Dates:",
          paste(unique(header_data$acquisition_date), collapse = ", "), "\n")
    }

    if (!"n_wells" %in% names(header_data) &&
        "number_of_wells" %in% names(header_data)) {
      header_data$n_wells <- header_data$number_of_wells
    }

    if (!"plate" %in% names(header_data) &&
        "plate_number" %in% names(header_data)) {
      header_data$plate <- header_data$plate_number
    }

    header_cols <- c(
      "study_accession", "experiment_accession", "plate_id", "file_name",
      "acquisition_date", "absorbance_id", "wavelengths", "autoloading_range",
      "auth0_user", "workspace_id", "plateid", "plate",
      "n_wells", "assay_response_variable", "assay_independent_variable",
      "nominal_sample_dilution", "project_id"
    )
    available_header_cols <- intersect(header_cols, names(header_data))
    header_data <- header_data[, available_header_cols, drop = FALSE]

    # ── Shared column mapping (used for every specimen type) ──────────────
    col_mapping <- c(
      "study_name"                   = "study_accession",
      "experiment_name"              = "experiment_accession",
      "specimen_type"                = "stype",
      "specimen_dilution_factor"     = "dilution",
      "subject_id"                   = "patientid",
      "timepoint_tissue_abbreviation" = "timeperiod",
      "assay_response"               = "antibody_mfi",
      "assay_bead_count"             = "antibody_n",
      "specimen_source"              = "source",
      "wavelength"                   = "wavelength"
    )

    apply_col_mapping <- function(df, mapping) {
      for (old_name in names(mapping)) {
        new_name <- mapping[old_name]
        if (old_name %in% names(df) && !new_name %in% names(df)) {
          names(df)[names(df) == old_name] <- new_name
        }
      }
      df
    }

    # Natural key for joining assay_response to plates_map
    natural_key <- c("plateid", "well")

    # ── Insert header ─────────────────────────────────────────────────────
    cat("  Inserting header...\n")
    DBI::dbAppendTable(
      conn,
      DBI::Id(schema = "madi_results", table = "xmap_header"),
      header_data
    )
    cat("    → Inserted", nrow(header_data), "header rows\n")

    # ── Samples ───────────────────────────────────────────────────────────
    sample_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "X", ]
    if (nrow(sample_map) > 0) {
      cat("  Preparing samples...\n")

      assay_cols <- intersect(
        c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"),
        names(assay_response)
      )
      samples_to_upload <- merge(
        sample_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )

      if (!"plate_id" %in% names(samples_to_upload) &&
          "plate_id" %in% names(plate_id_sheet)) {
        pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        samples_to_upload <- merge(samples_to_upload, pid_lookup,
                                   by = "plateid", all.x = TRUE)
      }

      samples_to_upload$study_accession      <- study_accession
      samples_to_upload$experiment_accession <- experiment_accession
      samples_to_upload$project_id           <- workspace_id
      samples_to_upload <- apply_col_mapping(samples_to_upload, col_mapping)

      if ("biosample_id_barcode" %in% names(samples_to_upload))
        samples_to_upload$sampleid <- samples_to_upload$biosample_id_barcode

      if ("sampleid" %in% names(samples_to_upload)) {
        empty_sid <- is.na(samples_to_upload$sampleid) |
          trimws(samples_to_upload$sampleid) == ""
        if (any(empty_sid))
          samples_to_upload$sampleid[empty_sid] <- samples_to_upload$well[empty_sid]
      } else {
        samples_to_upload$sampleid <- samples_to_upload$well
      }

      sample_cols <- c(
        "project_id", "study_accession", "experiment_accession", "timeperiod",
        "patientid", "well", "stype", "sampleid", "agroup", "dilution",
        "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
        "feature", "plate", "nominal_sample_dilution", "plateid", "plate_id",
        "source", "wavelength"
      )
      samples_to_upload <- samples_to_upload[,
                                             intersect(sample_cols, names(samples_to_upload)), drop = FALSE]

      if ("antibody_mfi" %in% names(samples_to_upload))
        samples_to_upload$antibody_mfi <- round(
          as.double(samples_to_upload$antibody_mfi), 4)

      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_sample"),
        samples_to_upload
      )
      cat("    → Inserted", nrow(samples_to_upload), "sample rows\n")
    }

    # ── Standards ─────────────────────────────────────────────────────────
    standard_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "S", ]
    if (nrow(standard_map) > 0) {
      cat("  Preparing standards...\n")

      assay_cols <- intersect(
        c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"),
        names(assay_response)
      )
      standards_to_upload <- merge(
        standard_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )

      if (!"plate_id" %in% names(standards_to_upload) &&
          "plate_id" %in% names(plate_id_sheet)) {
        pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        standards_to_upload <- merge(standards_to_upload, pid_lookup,
                                     by = "plateid", all.x = TRUE)
      }

      standards_to_upload$study_accession      <- study_accession
      standards_to_upload$experiment_accession <- experiment_accession
      standards_to_upload$project_id           <- workspace_id
      standards_to_upload <- apply_col_mapping(standards_to_upload, col_mapping)

      if ("biosample_id_barcode" %in% names(standards_to_upload))
        standards_to_upload$sampleid <- standards_to_upload$biosample_id_barcode

      if ("sampleid" %in% names(standards_to_upload)) {
        empty_sid <- is.na(standards_to_upload$sampleid) |
          trimws(standards_to_upload$sampleid) == ""
        if (any(empty_sid))
          standards_to_upload$sampleid[empty_sid] <-
            standards_to_upload$well[empty_sid]
      } else {
        standards_to_upload$sampleid <- standards_to_upload$well
      }

      standard_cols <- c(
        "project_id", "study_accession", "experiment_accession", "plate_id",
        "well", "stype", "sampleid", "source", "dilution", "pctaggbeads",
        "samplingerrors", "antigen", "antibody_mfi", "antibody_n", "feature",
        "plateid", "nominal_sample_dilution", "plate", "wavelength"
      )
      standards_to_upload <- standards_to_upload[,
                                                 intersect(standard_cols, names(standards_to_upload)), drop = FALSE]

      if ("antibody_mfi" %in% names(standards_to_upload))
        standards_to_upload$antibody_mfi <- round(
          as.double(standards_to_upload$antibody_mfi), 4)

      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_standard"),
        standards_to_upload
      )
      cat("    → Inserted", nrow(standards_to_upload), "standard rows\n")

      # ── Register new curve combinations in curve_lookup ─────────────────
      # Runs after a successful xmap_standard insert.
      # Non-fatal: a curve_lookup failure never rolls back the data upload.
      # ELISA standards already carry 'wavelength' and 'source' natively so
      # build_curve_lookup_candidates() will pick them up without any extra
      # column work here.
      cat("  Registering curves in curve_lookup...\n")
      tryCatch({
        cl_result <- register_curve_lookup(
          conn         = conn,
          standards_df = standards_to_upload,  # already in DB column names
          project_id   = workspace_id
        )
        if (cl_result$success) {
          cat("    → curve_lookup:", cl_result$message, "\n")
        } else {
          # Non-fatal warning — data is already committed
          cat("    ⚠ curve_lookup warning:", cl_result$message, "\n")
          showNotification(
            paste("curve_lookup warning:", cl_result$message),
            type    = "warning",
            duration = 8
          )
        }
      }, error = function(e_cl) {
        # Isolate from the outer tryCatch so a curve_lookup error never
        # triggers the "ELISA upload failed" notification.
        cat("    ⚠ curve_lookup non-fatal error:", conditionMessage(e_cl), "\n")
        showNotification(
          paste("curve_lookup (non-fatal):", conditionMessage(e_cl)),
          type    = "warning",
          duration = 8
        )
      })
      # ── End curve_lookup registration ────────────────────────────────────
    }

    # ── Blanks ────────────────────────────────────────────────────────────
    blank_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "B", ]
    if (nrow(blank_map) > 0) {
      cat("  Preparing blanks...\n")

      assay_cols <- intersect(
        c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"),
        names(assay_response)
      )
      blanks_to_upload <- merge(
        blank_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )

      if (!"plate_id" %in% names(blanks_to_upload) &&
          "plate_id" %in% names(plate_id_sheet)) {
        pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        blanks_to_upload <- merge(blanks_to_upload, pid_lookup,
                                  by = "plateid", all.x = TRUE)
      }

      blanks_to_upload$study_accession      <- study_accession
      blanks_to_upload$experiment_accession <- experiment_accession
      blanks_to_upload$project_id           <- workspace_id
      blanks_to_upload <- apply_col_mapping(blanks_to_upload, col_mapping)

      if ("biosample_id_barcode" %in% names(blanks_to_upload))
        blanks_to_upload$sampleid <- blanks_to_upload$biosample_id_barcode

      if ("sampleid" %in% names(blanks_to_upload)) {
        empty_sid <- is.na(blanks_to_upload$sampleid) |
          trimws(blanks_to_upload$sampleid) == ""
        if (any(empty_sid))
          blanks_to_upload$sampleid[empty_sid] <- blanks_to_upload$well[empty_sid]
      } else {
        blanks_to_upload$sampleid <- blanks_to_upload$well
      }

      blank_cols <- c(
        "study_accession", "experiment_accession", "plate_id", "well",
        "stype", "sampleid", "dilution", "pctaggbeads", "samplingerrors",
        "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
        "plateid", "nominal_sample_dilution", "source", "plate", "wavelength"
      )
      blanks_to_upload <- blanks_to_upload[,
                                           intersect(blank_cols, names(blanks_to_upload)), drop = FALSE]

      if ("antibody_mfi" %in% names(blanks_to_upload))
        blanks_to_upload$antibody_mfi <- round(
          as.double(blanks_to_upload$antibody_mfi), 4)

      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_buffer"),
        blanks_to_upload
      )
      cat("    → Inserted", nrow(blanks_to_upload), "blank rows\n")
    }

    # ── Controls ──────────────────────────────────────────────────────────
    control_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "C", ]
    if (nrow(control_map) > 0) {
      cat("  Preparing controls...\n")

      assay_cols <- intersect(
        c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"),
        names(assay_response)
      )
      controls_to_upload <- merge(
        control_map,
        assay_response[, assay_cols, drop = FALSE],
        by = natural_key, all.x = TRUE
      )

      if (!"plate_id" %in% names(controls_to_upload) &&
          "plate_id" %in% names(plate_id_sheet)) {
        pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
        controls_to_upload <- merge(controls_to_upload, pid_lookup,
                                    by = "plateid", all.x = TRUE)
      }

      controls_to_upload$study_accession      <- study_accession
      controls_to_upload$experiment_accession <- experiment_accession
      controls_to_upload$project_id           <- workspace_id
      controls_to_upload <- apply_col_mapping(controls_to_upload, col_mapping)

      if ("biosample_id_barcode" %in% names(controls_to_upload))
        controls_to_upload$sampleid <- controls_to_upload$biosample_id_barcode

      if ("sampleid" %in% names(controls_to_upload)) {
        empty_sid <- is.na(controls_to_upload$sampleid) |
          trimws(controls_to_upload$sampleid) == ""
        if (any(empty_sid))
          controls_to_upload$sampleid[empty_sid] <-
            controls_to_upload$well[empty_sid]
      } else {
        controls_to_upload$sampleid <- controls_to_upload$well
      }

      control_cols <- c(
        "study_accession", "experiment_accession", "plate_id", "well",
        "stype", "sampleid", "dilution", "pctaggbeads", "samplingerrors",
        "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
        "plateid", "nominal_sample_dilution", "plate", "source", "wavelength"
      )
      controls_to_upload <- controls_to_upload[,
                                               intersect(control_cols, names(controls_to_upload)), drop = FALSE]

      if ("antibody_mfi" %in% names(controls_to_upload))
        controls_to_upload$antibody_mfi <- round(
          as.double(controls_to_upload$antibody_mfi), 4)

      DBI::dbAppendTable(
        conn,
        DBI::Id(schema = "madi_results", table = "xmap_control"),
        controls_to_upload
      )
      cat("    → Inserted", nrow(controls_to_upload), "control rows\n")
    }

    # ── Antigen family ────────────────────────────────────────────────────
    if (!is.null(antigen_list) && nrow(antigen_list) > 0) {
      cat("  Uploading antigen family...\n")
      upload_antigen_family(conn, antigen_list, workspace_id,
                            study_accession, experiment_accession)
    }

    # ── Planned visits ────────────────────────────────────────────────────
    if (!is.null(timepoint_map) && nrow(timepoint_map) > 0) {
      cat("  Uploading planned visits...\n")
      names(timepoint_map)[names(timepoint_map) == "time_unit"] <- "timepoint_unit"
      upload_planned_visits(conn, timepoint_map, study_accession)
    }

    # ── Update reactive state ─────────────────────────────────────────────
    elisa_validation_state(list(
      is_validated    = TRUE,
      is_uploaded     = TRUE,
      validation_time = elisa_validation_state()$validation_time,
      upload_time     = Sys.time(),
      metadata_result = elisa_validation_state()$metadata_result,
      data_result     = elisa_validation_state()$data_result,
      data_stored     = TRUE
    ))

    showNotification(
      "ELISA data uploaded successfully!",
      type = "message", duration = 5
    )
    cat("\n  ✓ ELISA upload complete!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")

  }, error = function(e) {
    cat("  ELISA upload error:", conditionMessage(e), "\n")
    showNotification(
      paste("ELISA upload failed:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})
# observeEvent(input$upload_elisa_batch_data, {
#   req(elisa_validation_state()$is_validated)
#   req(layout_template_sheets())
#
#   cat("\n╔══════════════════════════════════════════════════════════╗\n")
#   cat("║  UPLOADING ELISA DATA TO DATABASE                        ║\n")
#   cat("╚══════════════════════════════════════════════════════════╝\n")
#
#   layout_sheets <- layout_template_sheets()
#   study_accession <- input$readxMap_study_accession
#   experiment_accession <- input$readxMap_experiment_accession_import
#   workspace_id <- userWorkSpaceID()
#   auth0_user <- currentuser()
#
#   tryCatch({
#     # Get required sheets
#     assay_response <- layout_sheets[["assay_response_long"]]
#     plates_map <- layout_sheets[["plates_map"]]
#     plate_id_sheet <- layout_sheets[["plate_id"]]
#     antigen_list <- layout_sheets[["antigen_list"]]
#     subject_map <- layout_sheets[["subject_groups"]]
#     timepoint_map <- layout_sheets[["timepoint"]]
#     elisa_metadata <- layout_sheets[["elisa_metadata"]]  # may be NULL; ELISA fields now in plate_id
#
#     # Guard: ensure assay_response is double-precision before it is renamed to
#     # antibody_mfi and inserted into PostgreSQL.  An integer-class column would
#     # cause the DB to receive truncated whole numbers.
#     if ("assay_response" %in% names(assay_response)) {
#       assay_response$assay_response <- round(as.double(assay_response$assay_response), 4)
#     }
#
#     conn <- get_db_connection()
#     on.exit(DBI::dbDisconnect(conn), add = TRUE)
#
#     # Check for existing plates
#     plates_to_upload <- unique(plate_id_sheet$plate_id)
#     existing_query <- glue::glue_sql("
#       SELECT plate_id
#       FROM madi_results.xmap_header
#       WHERE study_accession = {study_accession}
#         AND experiment_accession IN ({experiment_accession*})
#         AND plate_id IN ({plates_to_upload*});
#     ", .con = conn)
#     existing_plates <- DBI::dbGetQuery(conn, existing_query)
#
#     if (nrow(existing_plates) > 0) {
#       showNotification(
#         paste("These plates already exist:", paste(existing_plates$plate_id, collapse = ", ")),
#         type = "error", duration = 10
#       )
#       return()
#     }
#
#     # Prepare header data
#     header_data <- plate_id_sheet
#     header_data$study_accession <- study_accession
#     header_data$experiment_accession <- experiment_accession
#     header_data$auth0_user <- auth0_user
#     header_data$workspace_id <- workspace_id
#     header_data$assay_response_variable <- "absorbance"
#     header_data$assay_independent_variable <- "concentration"
#
#     # Standardize acquisition_date to ISO format for PostgreSQL
#     if ("acquisition_date" %in% names(header_data)) {
#       cat("  Standardizing acquisition dates for PostgreSQL...\n")
#       header_data$acquisition_date <- standardize_date_for_postgres(header_data$acquisition_date)
#       cat("    → Dates:", paste(unique(header_data$acquisition_date), collapse = ", "), "\n")
#     }
#
#     if (!"n_wells" %in% names(header_data) && "number_of_wells" %in% names(header_data)) {
#       header_data$n_wells <- header_data$number_of_wells
#     }
#
#     if (!"plate" %in% names(header_data) && "plate_number" %in% names(header_data)) {
#       header_data$plate <- header_data$plate_number
#     }
#
#     # Select available header columns
#     header_cols <- c(
#       "study_accession", "experiment_accession", "plate_id", "file_name",
#       "acquisition_date", "absorbance_id", "wavelengths", "autoloading_range",
#       "auth0_user", "workspace_id", "plateid", "plate",
#       "n_wells", "assay_response_variable", "assay_independent_variable",
#       "nominal_sample_dilution", "project_id"
#     )
#     available_header_cols <- intersect(header_cols, names(header_data))
#     header_data <- header_data[, available_header_cols, drop = FALSE]
#
#     # Use the same column mapping as bead array
#     col_mapping <- c(
#       "study_name" = "study_accession",
#       "experiment_name" = "experiment_accession",
#       "specimen_type" = "stype",
#       "specimen_dilution_factor" = "dilution",
#       "subject_id" = "patientid",
#       "timepoint_tissue_abbreviation" = "timeperiod",
#       "assay_response" = "antibody_mfi",
#       "assay_bead_count" = "antibody_n",
#       "specimen_source" = "source",
#       "wavelength" = "wavelength"
#     )
#
#     apply_col_mapping <- function(df, mapping) {
#       for (old_name in names(mapping)) {
#         new_name <- mapping[old_name]
#         if (old_name %in% names(df) && !new_name %in% names(df)) {
#           names(df)[names(df) == old_name] <- new_name
#         }
#       }
#       df
#     }
#
#     # Natural key for joining
#     natural_key <- c("plateid", "well")
#
#     # Upload header
#     cat("  Inserting header...\n")
#     DBI::dbAppendTable(conn, DBI::Id(schema = "madi_results", table = "xmap_header"), header_data)
#     cat("    → Inserted", nrow(header_data), "header rows\n")
#
#     # Prepare and upload samples
#     sample_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "X", ]
#     if (nrow(sample_map) > 0) {
#       cat("  Preparing samples...\n")
#       assay_cols <- intersect(c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"), names(assay_response))
#       samples_to_upload <- merge(sample_map, assay_response[, assay_cols, drop = FALSE], by = natural_key, all.x = TRUE)
#
#       if (!"plate_id" %in% names(samples_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#         pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#         samples_to_upload <- merge(samples_to_upload, pid_lookup, by = "plateid", all.x = TRUE)
#       }
#
#       samples_to_upload$study_accession <- study_accession
#       samples_to_upload$experiment_accession <- experiment_accession
#       samples_to_upload$project_id <- workspace_id
#       samples_to_upload <- apply_col_mapping(samples_to_upload, col_mapping)
#
#       if ("biosample_id_barcode" %in% names(samples_to_upload)) {
#         samples_to_upload$sampleid <- samples_to_upload$biosample_id_barcode
#       }
#       # Fallback: use well position if sampleid is still empty
#       if ("sampleid" %in% names(samples_to_upload)) {
#         empty_sid <- is.na(samples_to_upload$sampleid) | trimws(samples_to_upload$sampleid) == ""
#         if (any(empty_sid)) samples_to_upload$sampleid[empty_sid] <- samples_to_upload$well[empty_sid]
#       } else {
#         samples_to_upload$sampleid <- samples_to_upload$well
#       }
#
#       sample_cols <- c("project_id", "study_accession", "experiment_accession", "timeperiod",
#                        "patientid", "well", "stype", "sampleid", "agroup", "dilution",
#                        "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
#                        "feature", "plate", "nominal_sample_dilution", "plateid", "plate_id", "source", "wavelength")
#       available_cols <- intersect(sample_cols, names(samples_to_upload))
#       samples_to_upload <- samples_to_upload[, available_cols, drop = FALSE]
#       # Last-mile precision guard: ensure antibody_mfi is double before DB insert
#       if ("antibody_mfi" %in% names(samples_to_upload))
#         samples_to_upload$antibody_mfi <- round(as.double(samples_to_upload$antibody_mfi), 4)
#
#       DBI::dbAppendTable(conn, DBI::Id(schema = "madi_results", table = "xmap_sample"), samples_to_upload)
#       cat("    → Inserted", nrow(samples_to_upload), "sample rows\n")
#     }
#
#     # Prepare and upload standards
#     standard_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "S", ]
#     if (nrow(standard_map) > 0) {
#       cat("  Preparing standards...\n")
#       assay_cols <- intersect(c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"), names(assay_response))
#       standards_to_upload <- merge(standard_map, assay_response[, assay_cols, drop = FALSE], by = natural_key, all.x = TRUE)
#
#       if (!"plate_id" %in% names(standards_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#         pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#         standards_to_upload <- merge(standards_to_upload, pid_lookup, by = "plateid", all.x = TRUE)
#       }
#
#       standards_to_upload$study_accession <- study_accession
#       standards_to_upload$experiment_accession <- experiment_accession
#       standards_to_upload$project_id <- workspace_id
#       standards_to_upload <- apply_col_mapping(standards_to_upload, col_mapping)
#
#       # Set sampleid from biosample_id_barcode (populated from Sample grid)
#       if ("biosample_id_barcode" %in% names(standards_to_upload)) {
#         standards_to_upload$sampleid <- standards_to_upload$biosample_id_barcode
#       }
#       # Fallback: use well position if sampleid is still empty
#       if ("sampleid" %in% names(standards_to_upload)) {
#         empty_sid <- is.na(standards_to_upload$sampleid) | trimws(standards_to_upload$sampleid) == ""
#         if (any(empty_sid)) standards_to_upload$sampleid[empty_sid] <- standards_to_upload$well[empty_sid]
#       } else {
#         standards_to_upload$sampleid <- standards_to_upload$well
#       }
#
#       standard_cols <- c("project_id", "study_accession", "experiment_accession", "plate_id",
#                          "well", "stype", "sampleid", "source", "dilution", "pctaggbeads",
#                          "samplingerrors", "antigen", "antibody_mfi", "antibody_n", "feature",
#                          "plateid", "nominal_sample_dilution", "plate", "wavelength")
#       available_cols <- intersect(standard_cols, names(standards_to_upload))
#       standards_to_upload <- standards_to_upload[, available_cols, drop = FALSE]
#       # Last-mile precision guard: ensure antibody_mfi is double before DB insert
#       if ("antibody_mfi" %in% names(standards_to_upload))
#         standards_to_upload$antibody_mfi <- round(as.double(standards_to_upload$antibody_mfi), 4)
#
#       DBI::dbAppendTable(conn, DBI::Id(schema = "madi_results", table = "xmap_standard"), standards_to_upload)
#       cat("    → Inserted", nrow(standards_to_upload), "standard rows\n")
#     }
#
#     # Prepare and upload blanks
#     blank_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "B", ]
#     if (nrow(blank_map) > 0) {
#       cat("  Preparing blanks...\n")
#       assay_cols <- intersect(c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"), names(assay_response))
#       blanks_to_upload <- merge(blank_map, assay_response[, assay_cols, drop = FALSE], by = natural_key, all.x = TRUE)
#
#       if (!"plate_id" %in% names(blanks_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#         pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#         blanks_to_upload <- merge(blanks_to_upload, pid_lookup, by = "plateid", all.x = TRUE)
#       }
#
#       blanks_to_upload$study_accession <- study_accession
#       blanks_to_upload$experiment_accession <- experiment_accession
#       blanks_to_upload$project_id <- workspace_id
#       blanks_to_upload <- apply_col_mapping(blanks_to_upload, col_mapping)
#
#       # Set sampleid from biosample_id_barcode (populated from Sample grid)
#       if ("biosample_id_barcode" %in% names(blanks_to_upload)) {
#         blanks_to_upload$sampleid <- blanks_to_upload$biosample_id_barcode
#       }
#       if ("sampleid" %in% names(blanks_to_upload)) {
#         empty_sid <- is.na(blanks_to_upload$sampleid) | trimws(blanks_to_upload$sampleid) == ""
#         if (any(empty_sid)) blanks_to_upload$sampleid[empty_sid] <- blanks_to_upload$well[empty_sid]
#       } else {
#         blanks_to_upload$sampleid <- blanks_to_upload$well
#       }
#
#       blank_cols <- c("study_accession", "experiment_accession", "plate_id", "well",
#                       "stype", "sampleid", "dilution", "pctaggbeads", "samplingerrors",
#                       "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
#                       "plateid", "nominal_sample_dilution", "source", "plate", "wavelength")
#       available_cols <- intersect(blank_cols, names(blanks_to_upload))
#       blanks_to_upload <- blanks_to_upload[, available_cols, drop = FALSE]
#       # Last-mile precision guard: ensure antibody_mfi is double before DB insert
#       if ("antibody_mfi" %in% names(blanks_to_upload))
#         blanks_to_upload$antibody_mfi <- round(as.double(blanks_to_upload$antibody_mfi), 4)
#
#       DBI::dbAppendTable(conn, DBI::Id(schema = "madi_results", table = "xmap_buffer"), blanks_to_upload)
#       cat("    → Inserted", nrow(blanks_to_upload), "blank rows\n")
#     }
#
#     # Prepare and upload controls
#     control_map <- plates_map[substr(plates_map$specimen_type, 1, 1) == "C", ]
#     if (nrow(control_map) > 0) {
#       cat("  Preparing controls...\n")
#       assay_cols <- intersect(c(natural_key, "antigen", "assay_response", "assay_bead_count", "wavelength"), names(assay_response))
#       controls_to_upload <- merge(control_map, assay_response[, assay_cols, drop = FALSE], by = natural_key, all.x = TRUE)
#
#       if (!"plate_id" %in% names(controls_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#         pid_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#         controls_to_upload <- merge(controls_to_upload, pid_lookup, by = "plateid", all.x = TRUE)
#       }
#
#       controls_to_upload$study_accession <- study_accession
#       controls_to_upload$experiment_accession <- experiment_accession
#       controls_to_upload$project_id <- workspace_id
#       controls_to_upload <- apply_col_mapping(controls_to_upload, col_mapping)
#
#       # Set sampleid from biosample_id_barcode (populated from Sample grid)
#       if ("biosample_id_barcode" %in% names(controls_to_upload)) {
#         controls_to_upload$sampleid <- controls_to_upload$biosample_id_barcode
#       }
#       if ("sampleid" %in% names(controls_to_upload)) {
#         empty_sid <- is.na(controls_to_upload$sampleid) | trimws(controls_to_upload$sampleid) == ""
#         if (any(empty_sid)) controls_to_upload$sampleid[empty_sid] <- controls_to_upload$well[empty_sid]
#       } else {
#         controls_to_upload$sampleid <- controls_to_upload$well
#       }
#
#       control_cols <- c("study_accession", "experiment_accession", "plate_id", "well",
#                         "stype", "sampleid", "dilution", "pctaggbeads", "samplingerrors",
#                         "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
#                         "plateid", "nominal_sample_dilution", "plate", "source", "wavelength")
#       available_cols <- intersect(control_cols, names(controls_to_upload))
#       controls_to_upload <- controls_to_upload[, available_cols, drop = FALSE]
#       # Last-mile precision guard: ensure antibody_mfi is double before DB insert
#       if ("antibody_mfi" %in% names(controls_to_upload))
#         controls_to_upload$antibody_mfi <- round(as.double(controls_to_upload$antibody_mfi), 4)
#
#       DBI::dbAppendTable(conn, DBI::Id(schema = "madi_results", table = "xmap_control"), controls_to_upload)
#       cat("    → Inserted", nrow(controls_to_upload), "control rows\n")
#     }
#
#     # Upload antigen family
#     if (!is.null(antigen_list) && nrow(antigen_list) > 0) {
#       cat("  Uploading antigen family...\n")
#       upload_antigen_family(conn, antigen_list, workspace_id, study_accession, experiment_accession)
#     }
#
#     # Upload planned visits
#     if (!is.null(timepoint_map) && nrow(timepoint_map) > 0) {
#       cat("  Uploading planned visits...\n")
#       names(timepoint_map)[names(timepoint_map) == "time_unit"] <- "timepoint_unit"
#       upload_planned_visits(conn, timepoint_map, study_accession)
#     }
#
#     # Update state
#     elisa_validation_state(list(
#       is_validated = TRUE,
#       is_uploaded = TRUE,
#       validation_time = elisa_validation_state()$validation_time,
#       upload_time = Sys.time(),
#       metadata_result = elisa_validation_state()$metadata_result,
#       data_result = elisa_validation_state()$data_result,
#       data_stored = TRUE
#     ))
#
#     showNotification(
#       "ELISA data uploaded successfully!",
#       type = "message", duration = 5
#     )
#
#     cat("\n  ✓ ELISA upload complete!\n")
#     cat("╚══════════════════════════════════════════════════════════╝\n\n")
#
#   }, error = function(e) {
#     cat("  ELISA upload error:", conditionMessage(e), "\n")
#     showNotification(
#       paste("ELISA upload failed:", conditionMessage(e)),
#       type = "error", duration = 10
#     )
#   })
# })


# ---- UI Outputs for ELISA ----

output$elisa_file_summary <- renderPrint({
  req(elisa_plate_data())
  d <- elisa_plate_data()
  cat("ELISA Data Summary\n")
  cat("==================\n")
  cat("Total rows:", nrow(d), "\n")
  cat("Plates:", paste(sort(unique(d$plateid)), collapse = ", "), "\n")
  cat("Antigen:", paste(unique(d$antigen), collapse = ", "), "\n")
  cat("Feature:", paste(unique(d$feature), collapse = ", "), "\n")
  cat("Wavelengths:", paste(sort(unique(d$wavelength)), collapse = ", "), "nm\n")
  cat("STypes:", paste(sort(unique(d$stype[!is.na(d$stype)])), collapse = ", "), "\n")
})


output$elisa_description_warning_ui <- renderUI({
  ds <- elisa_description_status()
  if (!ds$has_content || !ds$has_sufficient_elements) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      if (!ds$has_content) {
        "Description fields are empty. Default values will be used for subject_id, timepoint, and dilution."
      } else {
        paste0("Description fields have only ", ds$min_elements_found,
               " elements (expected ", ds$required_elements,
               "). Some values may need manual review.")
      }
    )
  }
})


output$elisa_validation_status <- renderUI({
  state <- elisa_validation_state()

  if (state$is_uploaded) {
    div(
      class = "alert alert-success",
      icon("check-circle"),
      " ELISA data uploaded successfully!",
      if (!is.null(state$upload_time))
        paste(" (", format(state$upload_time, "%Y-%m-%d %H:%M:%S"), ")")
    )
  } else if (state$is_validated) {
    div(
      class = "alert alert-info",
      icon("check"),
      " ELISA layout validated. Ready to upload.",
      actionButton("upload_elisa_batch_data", "Upload ELISA Data to Database",
                   class = "btn-success", style = "margin-left: 20px;")
    )
  } else if (!is.null(state$metadata_result) || !is.null(state$data_result)) {
    msgs <- c()
    if (!is.null(state$metadata_result) && !state$metadata_result$is_valid) {
      msgs <- c(msgs, state$metadata_result$messages)
    }
    if (!is.null(state$data_result) && !state$data_result$is_valid) {
      msgs <- c(msgs, state$data_result$messages)
    }

    div(
      class = "alert alert-danger",
      icon("times-circle"),
      " Validation failed:",
      tags$ul(lapply(msgs, tags$li))
    )
  }
})


output$elisa_order_input_ui <- renderUI({
  req(elisa_description_status()$has_sufficient_elements)

  all_elements <- c("PatientID", "TimePeriod", "DilutionFactor")
  optional <- input$elisa_optional_elements
  if (!is.null(optional)) {
    all_elements <- c("PatientID", optional, "TimePeriod", "DilutionFactor")
  }

  orderInput(
    inputId = "elisa_XElementOrder",
    label = "Description Label: Sample Elements (drag and drop to reorder)",
    items = all_elements,
    width = "100%",
    item_class = "primary"
  )
})


output$elisa_bcsorder_input_ui <- renderUI({
  req(elisa_description_status()$has_content)

  orderInput(
    inputId = "elisa_BCSElementOrder",
    label = "Description Label: Blank/Standard/Control Elements (drag and drop to reorder)",
    items = c("Source", "DilutionFactor"),
    width = "100%",
    item_class = "info"
  )
})


# Conditional output flags for ELISA
output$hasElisaData <- reactive({
  !is.null(elisa_plate_data())
})
outputOptions(output, "hasElisaData", suspendWhenHidden = FALSE)

output$hasElisaLayoutSheets <- reactive({
  sheets <- layout_template_sheets()
  !is.null(sheets) && "assay_response_long" %in% names(sheets) &&
    "plate_id" %in% names(sheets)
})
outputOptions(output, "hasElisaLayoutSheets", suspendWhenHidden = FALSE)

# ---- ELISA Layout Sheet Viewer Observers ----

observeEvent(input$view_elisa_plate_id, {
  showModal(modalDialog(
    title = "ELISA - Plate ID Sheet",
    rhandsontable::rHandsontableOutput("elisa_layout_plate_id"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_plate_id <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["plate_id"]])
    rhandsontable::rhandsontable(layout_template_sheets()[["plate_id"]], readOnly = TRUE)
  })
})

observeEvent(input$view_elisa_plates_map, {
  showModal(modalDialog(
    title = "ELISA - Plates Map Sheet",
    rhandsontable::rHandsontableOutput("elisa_layout_plates_map"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_plates_map <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["plates_map"]])
    rhandsontable::rhandsontable(layout_template_sheets()[["plates_map"]], readOnly = TRUE)
  })
})

observeEvent(input$view_elisa_subject_groups, {
  showModal(modalDialog(
    title = "ELISA - Subject Groups",
    rhandsontable::rHandsontableOutput("elisa_layout_subjects"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_subjects <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["subject_groups"]])
    rhandsontable::rhandsontable(layout_template_sheets()[["subject_groups"]], readOnly = TRUE)
  })
})

observeEvent(input$view_elisa_timepoint, {
  showModal(modalDialog(
    title = "ELISA - Timepoint",
    rhandsontable::rHandsontableOutput("elisa_layout_timepoint"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_timepoint <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["timepoint"]])
    rhandsontable::rhandsontable(layout_template_sheets()[["timepoint"]], readOnly = TRUE)
  })
})

observeEvent(input$view_elisa_antigen_list, {
  showModal(modalDialog(
    title = "ELISA - Antigen List",
    rhandsontable::rHandsontableOutput("elisa_layout_antigens"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_antigens <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["antigen_list"]])
    rhandsontable::rhandsontable(layout_template_sheets()[["antigen_list"]], readOnly = TRUE)
  })
})

observeEvent(input$view_elisa_metadata, {
  showModal(modalDialog(
    title = "ELISA - Metadata (Wavelengths, Autoloading Range)",
    rhandsontable::rHandsontableOutput("elisa_layout_metadata"),
    size = "l", easyClose = TRUE
  ))
  output$elisa_layout_metadata <- rhandsontable::renderRHandsontable({
    sheets <- layout_template_sheets()
    # For backward compatibility: show elisa_metadata sheet if present,
    # otherwise extract ELISA-specific columns from plate_id
    if (!is.null(sheets[["elisa_metadata"]])) {
      rhandsontable::rhandsontable(sheets[["elisa_metadata"]], readOnly = TRUE)
    } else {
      req(sheets[["plate_id"]])
      pid <- sheets[["plate_id"]]
      elisa_cols <- intersect(c("plateid", "plate_id", "wavelengths", "autoloading_range",
                                "original_filename", "absorbance_id", "acquisition_date"),
                              names(pid))
      rhandsontable::rhandsontable(pid[, elisa_cols, drop = FALSE], readOnly = TRUE)
    }
  })
})
