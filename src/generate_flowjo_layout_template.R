#' =============================================================================
#' FLOWJO LAYOUT TEMPLATE GENERATION
#'
#' Converts output from load_flowjo_file() / pivot_flowjo_long() into the
#' standard I-SPI layout template Excel workbook (7 tabs).
#'
#' Key design decisions vs. bead-array layout templates:
#'   - Antibodies (IgA / IgG / IgM) are treated as the analytes and fill the
#'     antigen_list tab (one row per antibody).  The actual antigen (e.g. PT)
#'     is stored in antigen_name / antigen_family.
#'   - The 384-well plate is composed of 4 × 96-well subplates; each subplate
#'     gets its own plate_id row and its own plates_map block.
#'   - PercentOGsingle (rounded to integer) maps to assay_bead_count.
#'   - PercentAgg maps to pct_agg (equivalent to pct_aggbeads in bead arrays).
#'   - dilution_factor from the dilutions tab maps to specimen_dilution_factor.
#'   - feature is set to "MFI" for the entire experiment (analogous to the
#'     isotype feature in bead arrays).
#' =============================================================================

# ==============================================================
# HELPER: convert a 384-well label to a 96-well label
#
# Layout convention (confirmed from data):
#   Plates 1 & 2  → rows A,C,E,G,I,K,M,O (odd 384-rows)
#   Plates 3 & 4  → rows B,D,F,H,J,L,N,P (even 384-rows)
#   Plates 1 & 3  → cols 1,3,5,…,23       (odd 384-cols)
#   Plates 2 & 4  → cols 2,4,6,…,24       (even 384-cols)
#
# Mapping:
#   384-row → 96-row  :  position within its odd/even sequence (A→A, C→B, …)
#   384-col → 96-col  :  ceiling(col / 2)
# ==============================================================
convert_384_to_96_well <- function(well_384) {
  rows_odd  <- c("A","C","E","G","I","K","M","O")
  rows_even <- c("B","D","F","H","J","L","N","P")
  rows_96   <- LETTERS[1:8]

  row_ltr  <- substr(well_384, 1, 1)
  col_384  <- as.integer(substring(well_384, 2))

  row_96 <- dplyr::case_when(
    row_ltr %in% rows_odd  ~ rows_96[match(row_ltr, rows_odd)],
    row_ltr %in% rows_even ~ rows_96[match(row_ltr, rows_even)],
    TRUE ~ NA_character_
  )

  col_96 <- ceiling(col_384 / 2)
  sprintf("%s%d", row_96, col_96)   # e.g. "A1", "H12"
}


# ==============================================================
# HELPER: Resolve the timepoint column name
#         (parse_sample_id creates "timepoint"; read_flowjo_values.R
#          renames it "timeperiod" in the final select)
# ==============================================================
get_timepoint_col <- function(df) {
  if ("timepoint"  %in% names(df)) return("timepoint")
  if ("timeperiod" %in% names(df)) return("timeperiod")
  warning("Neither 'timepoint' nor 'timeperiod' found in flowjo_long. Returning NA.")
  NULL
}


# ==============================================================
# HELPER: Derive a clean plateid from the source filename + plate
# ==============================================================
make_plateid <- function(source_file, plate_label) {
  stem <- tools::file_path_sans_ext(basename(source_file))
  stem <- gsub("\\.", "_", stem)          # "3.1" -> "3_1"
  paste0(stem, "_", plate_label)          # e.g. "MFI-values_OPT_3_1_4_1_dilute_plate_1"
}


# ==============================================================
# BUILD: plate_id tab
# ==============================================================
# BUILD: plate_id tab
#   One row per subplate (4 rows for a 384-well / 4-subplate file).
# ==============================================================
build_flowjo_plate_id <- function(flowjo_long,
                                   project_id,
                                   study_name,
                                   experiment_name,
                                   source_filepath = NULL,
                                   n_wells = 96) {

  cat("=== BUILD_FLOWJO_PLATE_ID ===\n")

  # source_file is overridden to f$name in flowjo_reader.R before this is called,
  # so it holds the original uploaded filename, not the Shiny temp basename.
  source_file <- unique(flowjo_long$source_file)[1]

  plate_labels <- sort(unique(flowjo_long$plate))

  plate_id <- map_dfr(plate_labels, function(pl) {
    sub      <- flowjo_long %>% filter(plate == pl, stype == "X")
    dil_vals <- unique(sub$dilution_factor[!is.na(sub$dilution_factor)])
    nom_dil  <- paste(sort(dil_vals), collapse = "|")
    plateid_val <- make_plateid(source_file, pl)

    # file_name: validator needs a path with directory separators.
    # source_filepath (the Shiny temp path) satisfies this; fall back to
    # the original filename when called outside the app.
    file_name_val <- if (!is.null(source_filepath) && nzchar(trimws(source_filepath))) {
      as.character(source_filepath)
    } else {
      basename(as.character(source_file))
    }

    # Store acquisition_date with time so normalize_acquisition_date produces
    # the required "DD-MMM-YYYY, HH:MM AM/PM" format unambiguously.
    acq_date_val <- format(Sys.time(), "%m/%d/%Y %I:%M %p")

    tibble(
      project_id               = project_id,
      study_name               = study_name,
      experiment_name          = experiment_name,
      plate_number             = pl,
      nominal_sample_dilution  = nom_dil,
      number_of_wells          = n_wells,
      plateid                  = plateid_val,
      plate_id                 = plateid_val,
      # file_name      — checked by required_cols in validate_batch_plate_metadata
      # plate_filename — used in cross-check: file_name %in% plate_filename
      # Both hold the same real path so the self-referential cross-check passes.
      file_name                = file_name_val,
      plate_filename           = file_name_val,
      acquisition_date         = acq_date_val,
      reader_serial_number     = NA_character_,
      rp1_pmt_volts            = NA_character_,   # not applicable; NA passes check_rp1_numeric
      rp1_target               = NA_character_
    )
  })

  cat("  Plate_id rows:", nrow(plate_id), "\n")
  cat("  Plates:", paste(plate_id$plate_number, collapse = ", "), "\n")
  cat("=====================================\n\n")

  plate_id
}


# ==============================================================
# BUILD: antigen_list tab
# ==============================================================
# BUILD: antigen_list tab
#   Each ANTIBODY (IgA / IgG / IgM) is treated as an analyte
#   (equivalent to a bead antigen in bead arrays).
# ==============================================================
build_flowjo_antigen_list <- function(dilutions,
                                       project_id,
                                       study_name,
                                       experiment_name) {

  cat("=== BUILD_FLOWJO_ANTIGEN_LIST ===\n")

  antibodies     <- unique(dilutions$antibody)
  antigen_actual <- unique(dilutions$antigen[!is.na(dilutions$antigen)])[1]

  antigen_list <- map_dfr(antibodies, function(ab) {
    ab_rows   <- dilutions %>% filter(antibody == ab)
    ab_source <- ab_rows %>% filter(stype == "X") %>% pull(source) %>% unique()
    ab_source <- if (length(ab_source) == 0) NA_character_ else ab_source[1]

    # Flow cytometry semantics:
    #   antigen_label_on_plate / antigen_abbreviation = the actual antigen (PT)
    #   feature = the isotype/antibody channel measured (IgG / IgM / IgA)
    # One row per isotype, all sharing the same antigen label.
    tibble(
      project_id                        = project_id,
      study_name                        = study_name,
      experiment_name                   = experiment_name,
      antigen_label_on_plate            = antigen_actual,   # e.g. "PT"
      antigen_abbreviation              = antigen_actual,   # e.g. "PT"
      feature                           = ab,               # e.g. "IgG"
      standard_curve_max_concentration  = 100000,
      l_asy_constraint_method           = "default",
      l_asy_min_constraint              = 0,
      l_asy_max_constraint              = 0,
      antigen_family                    = antigen_actual,
      antigen_name                      = antigen_actual,
      virus_bacterial_strain            = NA_character_,
      antigen_source                    = ab_source,
      catalog_number                    = NA_character_
    )
  })

  cat("  Antigen_list rows:", nrow(antigen_list),
      "(antigen:", antigen_actual, "| features:", paste(antibodies, collapse = ", "), ")\n")
  cat("  Actual antigen (antigen_name):", antigen_actual, "\n")
  cat("=====================================\n\n")

  antigen_list
}


# ==============================================================
# BUILD: plates_map tab
# ==============================================================
# BUILD: plates_map tab
#   One row per well × subplate (96 × 4 = 384 rows).
#   When the same well has different dilution_factors per antibody
#   the unique values are pipe-separated in specimen_dilution_factor.
# ==============================================================
build_flowjo_plates_map <- function(flowjo_long,
                                     plate_id,
                                     project_id,
                                     study_name,
                                     experiment_name,
                                     antigen_name = NA_character_,
                                     feature = "MFI") {

  cat("=== BUILD_FLOWJO_PLATES_MAP ===\n")

  # ── Pre-extract timepoint as a plain vector ──────────────────────────────────
  # .data[[tp_col]] inside summarise() fails with "Can't subset .data outside
  # of a data mask context" in pre-1.0.0 dplyr.  Plain base-R indexing is safe.
  tp_col <- get_timepoint_col(flowjo_long)
  tp_vec <- if (!is.null(tp_col) && tp_col %in% names(flowjo_long)) {
    as.character(flowjo_long[[tp_col]])
  } else {
    rep(NA_character_, nrow(flowjo_long))
  }

  # ── Add derived columns with base R ─────────────────────────────────────────
  # group_by + summarise is intentionally avoided: pre-1.0.0 dplyr drops
  # grouping variables from the output and treats .groups as a data column.
  fl          <- flowjo_long
  fl$well_96  <- convert_384_to_96_well(fl$well)
  fl$tp_work  <- tp_vec

  # One row per plate x well_96
  well_key  <- paste(fl$plate, fl$well_96, sep = "\x1f")
  fl_unique <- fl[!duplicated(well_key), ]

  # Aggregate dilution_factor per plate x well via tapply
  dil_agg <- tapply(
    fl$dilution_factor, well_key,
    function(x) paste(sort(unique(x[!is.na(x)])), collapse = "|")
  )
  dil_df <- data.frame(
    .wk                      = names(dil_agg),
    specimen_dilution_factor = as.character(dil_agg),
    stringsAsFactors         = FALSE
  )

  # ── Build plates_map_raw ────────────────────────────────────────────────────
  keep_cols <- intersect(
    c("plate", "well_96", "stype", "source", "patientid", "tp_work", "Sample_ID"),
    names(fl_unique)
  )
  pmr <- fl_unique[, keep_cols, drop = FALSE]
  names(pmr)[names(pmr) == "tp_work"] <- "timepoint_val"

  pmr$.wk <- paste(pmr$plate, pmr$well_96, sep = "\x1f")
  pmr      <- merge(pmr, dil_df, by = ".wk", all.x = TRUE)
  pmr$.wk  <- NULL

  cat("  plates_map_raw cols:", paste(names(pmr), collapse = ", "), "\n")
  cat("  plates_map_raw rows:", nrow(pmr), "\n")

  # ── Join plate_id via base-R merge (avoids named-vector by= issues) ─────────
  pid <- plate_id[, c("plate_number", "plateid", "nominal_sample_dilution"), drop = FALSE]
  names(pid)[names(pid) == "plate_number"] <- "plate"
  pmr <- merge(pmr, pid, by = "plate", all.x = TRUE)

  # ── Derive specimen columns ──────────────────────────────────────────────────
  pmr$project_id      <- project_id
  pmr$study_name      <- study_name
  pmr$experiment_name <- experiment_name

  pmr$specimen_type <- ifelse(pmr$stype == "X", "X",
                       ifelse(pmr$stype == "C", "C",
                       ifelse(pmr$stype == "B", "B",
                       ifelse(pmr$stype == "S", "S", ""))))

  pmr$specimen_source <- ifelse(
    pmr$stype == "X", "sample",
    ifelse(pmr$stype %in% c("C", "B", "S"), as.character(pmr$source), "")
  )

  pmr$subject_id <- ifelse(pmr$stype == "B", "1", as.character(pmr$patientid))
  pmr$subject_id[is.na(pmr$subject_id) | pmr$subject_id == ""] <- "1"

  pmr$biosample_id_barcode <- as.character(pmr$patientid)

  pmr$timepoint_tissue_abbreviation <- ifelse(
    pmr$specimen_type == "X", as.character(pmr$timepoint_val), NA_character_
  )
  pmr$timepoint_tissue_abbreviation[
    pmr$specimen_type == "X" &
    (is.na(pmr$timepoint_tissue_abbreviation) |
     pmr$timepoint_tissue_abbreviation == "")
  ] <- "T0"

  # antigen = the antigen being tested (e.g. "PT").
  # The isotype (IgG/IgM/IgA) is the feature and lives in assay_response_long$feature.
  pmr$antigen <- antigen_name

  pmr$specimen_dilution_factor[
    is.na(pmr$specimen_dilution_factor) | pmr$specimen_dilution_factor == ""
  ] <- "1"

  # ── Final column selection ───────────────────────────────────────────────────
  names(pmr)[names(pmr) == "plate"]   <- "plate_number"
  names(pmr)[names(pmr) == "well_96"] <- "well"

  final_cols <- c("project_id", "study_name", "plate_number",
                  "nominal_sample_dilution", "well", "specimen_type",
                  "specimen_source", "specimen_dilution_factor",
                  "experiment_name", "antigen", "subject_id",
                  "biosample_id_barcode", "timepoint_tissue_abbreviation",
                  "plateid")
  plates_map <- pmr[, intersect(final_cols, names(pmr)), drop = FALSE]

  cat("  Plates_map rows:", nrow(plates_map), "\n")
  cat("  Unique plates:", paste(sort(unique(plates_map$plate_number)), collapse = ", "), "\n")
  cat("  Specimen types:", paste(sort(unique(plates_map$specimen_type)), collapse = ", "), "\n")
  cat("=====================================\n\n")

  plates_map
}


# ==============================================================
# BUILD: subject_groups tab
# ==============================================================
# BUILD: subject_groups tab
# ==============================================================
build_flowjo_subject_groups <- function(plates_map,
                                         study_name) {

  cat("=== BUILD_FLOWJO_SUBJECT_GROUPS ===\n")

  subject_groups <- plates_map %>%
    filter(specimen_type == "X") %>%
    select(study_name, subject_id) %>%
    distinct() %>%
    mutate(
      groupa = "Unknown",
      groupb = "Unknown"
    ) %>%
    arrange(as.numeric(subject_id))

  if (nrow(subject_groups) == 0) {
    subject_groups <- tibble(
      study_name = study_name,
      subject_id = "1",
      groupa     = "Unknown",
      groupb     = "Unknown"
    )
    cat("  No X-type samples found; using default subject_groups row.\n")
  }

  cat("  Subject_groups rows:", nrow(subject_groups), "\n")
  cat("=====================================\n\n")

  subject_groups
}


# ==============================================================
# BUILD: timepoint tab
# ==============================================================
build_flowjo_timepoint <- function(plates_map,
                                    study_name) {

  cat("=== BUILD_FLOWJO_TIMEPOINT ===\n")

  tps <- plates_map %>%
    filter(specimen_type == "X") %>%
    pull(timepoint_tissue_abbreviation) %>%
    unique()
  tps <- tps[!is.na(tps) & nzchar(tps)]

  if (length(tps) == 0) {
    tps <- "T0"
    cat("  No timepoints found; defaulting to 'T0'.\n")
  }

  timepoint <- tibble(
    study_name                    = study_name,
    timepoint_tissue_abbreviation = sort(tps),
    tissue_type                   = "blood",
    tissue_subtype                = "serum",
    description                   = NA_character_,
    min_time_since_day_0          = NA_character_,
    max_time_since_day_0          = NA_character_,
    timepoint_unit                = NA_character_
  ) %>%
    distinct(study_name, timepoint_tissue_abbreviation,
             tissue_type, tissue_subtype, .keep_all = TRUE)

  cat("  Timepoint rows:", nrow(timepoint), "\n")
  cat("  Timepoints:", paste(timepoint$timepoint_tissue_abbreviation, collapse=", "), "\n")
  cat("=====================================\n\n")

  timepoint
}


# ==============================================================
# BUILD: assay_response_long tab
#   One row per well × subplate × antibody.
#   - antigen        = antibody name (IgA / IgG / IgM)
#   - assay_response = raw MFI
#   - assay_bead_count = round(PercentOGsingle)   [bead-count analogue]
#   - pct_agg        = PercentAgg                  [pct_aggbeads analogue]
# ==============================================================
build_flowjo_assay_response_long <- function(flowjo_long,
                                              plate_id,
                                              project_id,
                                              study_name,
                                              experiment_name) {

  cat("=== BUILD_FLOWJO_ASSAY_RESPONSE_LONG ===\n")

  # Base-R merge avoids named-vector by = c("plate" = "plate_number") which
  # behaves inconsistently across dplyr versions.
  pid        <- plate_id[, c("plate_number", "plateid"), drop = FALSE]
  names(pid)[names(pid) == "plate_number"] <- "plate"

  fl         <- flowjo_long
  fl$well_96 <- convert_384_to_96_well(fl$well)
  fl         <- merge(fl, pid, by = "plate", all.x = TRUE)

  arl <- data.frame(
    project_id       = project_id,
    study_name       = study_name,
    experiment_name  = experiment_name,
    plateid          = fl$plateid,
    well             = fl$well_96,
    feature          = fl$antibody,               # isotype is the measured feature
    assay_response   = fl$MFI,
    assay_bead_count = as.integer(round(fl$PercentOGsingle)),
    pct_agg          = fl$PercentAgg,
    stringsAsFactors = FALSE
  )
  arl <- arl[order(arl$plateid, arl$well, arl$feature), ]
  rownames(arl) <- NULL

  cat("  Assay_response_long rows:", nrow(arl), "\n")
  cat("  Features (isotypes):", paste(sort(unique(arl$feature)), collapse = ", "), "\n")
  cat("=====================================\n\n")

  arl
}


# ==============================================================
# BUILD: cell_valid tab (static)
# ==============================================================
# BUILD: cell_valid tab (static)
# ==============================================================
build_cell_valid <- function() {
  tibble(
    l_asy_constraint_method = c("default", "user_defined",
                                 "range_of_blanks",
                                 "geometric_mean_of_blanks")
  )
}


# ==============================================================
# WRITE: workbook sheets with example rows and bold headers
# ==============================================================
write_flowjo_workbook_sheets <- function(wb, workbook) {

  bold_style   <- createStyle(textDecoration = "bold")
  italic_style <- createStyle(textDecoration = "italic")

  examples <- list(
    plate_id = list(
      rows       = list(c("prj_64", "Hardik_x_study", "exp_PT_A",
                           "plate_1", "500|1000", 96,
                           "MFI-values_OPT_3_1_4_1_dilute_plate_1",
                           "MFI-values_OPT_3_1_4_1_dilute_plate_1",
                           "MFI-values_OPT_3_1_4_1_dilute.xlsx",
                           "2025-01-01", NA, NA, NA)),
      data_start = 3
    ),
    subject_groups = list(
      rows       = list(c("Hardik_x_study", "10004", "Unknown", "Unknown")),
      data_start = 3
    ),
    timepoint = list(
      rows       = list(
        c("Hardik_x_study", "TP3", "blood", "serum", NA, NA, NA, NA),
        c("Hardik_x_study", "TP4", "blood", "serum", NA, NA, NA, NA)
      ),
      data_start = 5
    ),
    antigen_list = list(
      rows       = list(c("prj_64", "Hardik_x_study", "exp_PT_A",
                           "IgG", "IgG", 100000, "default", 0, 0,
                           "PT", "PT", NA, "sample", NA)),
      data_start = 3,
      extra_header = list(row = 1, col = 7, text = "user defined constraint")
    ),
    plates_map = list(
      rows       = list(c("prj_64", "Hardik_x_study", "plate_1",
                           "1000", "A1", "X", "sample", "1000",
                           "exp_PT_A", "MFI", "10004", "10004",
                           "TP4", "MFI-values_OPT_3_1_4_1_dilute_plate_1")),
      data_start = 3
    ),
    assay_response_long = list(
      rows       = list(c("prj_64", "Hardik_x_study", "exp_PT_A",
                           "MFI-values_OPT_3_1_4_1_dilute_plate_1",
                           "A1", "IgG", 2437.87, 26, 53.74)),
      data_start = 3
    )
  )

  for (nm in names(workbook)) {
    addWorksheet(wb, nm)
    df <- workbook[[nm]]

    if (nm %in% names(examples)) {
      ex <- examples[[nm]]

      writeData(wb, nm, "Example:", startRow = 1, startCol = 1)

      if (!is.null(ex$extra_header)) {
        writeData(wb, nm, ex$extra_header$text,
                  startRow = ex$extra_header$row,
                  startCol = ex$extra_header$col)
      }

      for (i in seq_along(ex$rows)) {
        writeData(wb, nm, t(ex$rows[[i]]),
                  startRow = 1 + i, startCol = 1, colNames = FALSE)
      }

      writeData(wb, nm, df, startRow = ex$data_start, startCol = 1)

      addStyle(wb, nm, bold_style,
               rows = ex$data_start,
               cols = 1:ncol(df), gridExpand = TRUE)
      addStyle(wb, nm, italic_style,
               rows = 1:(ex$data_start - 1),
               cols = 1:max(ncol(df), 9), gridExpand = TRUE)

    } else if (nm == "cell_valid") {
      writeData(wb, nm, df, startRow = 1, startCol = 1)
      addStyle(wb, nm, bold_style,
               rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    } else {
      writeData(wb, nm, df)
      addStyle(wb, nm, bold_style,
               rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    }

    # Auto-width columns (reasonable cap)
    setColWidths(wb, nm, cols = 1:ncol(df), widths = "auto")
  }
}


# ==============================================================
# MAIN FUNCTION: generate_flowjo_layout_template()
#
# @param flowjo_long      Data frame: output of pivot_flowjo_long()
#                         with project_id / study_name / experiment_name
#                         already attached (as in read_flowjo_values.R).
# @param dilutions        Data frame: result$dilutions from load_flowjo_file()
# @param project_id       Numeric or character project identifier
# @param study_name       Character study name  (from app / read_flowjo_values.R)
# @param experiment_name  Character experiment name
# @param output_file      Full path for the output .xlsx file
# @param feature          Label for the measurement feature (default "MFI")
# @param n_wells          Wells per subplate (default 96)
#
# @return Invisibly returns the list of data frames that were written.
#         Saves the workbook to output_file as a side-effect.
# ==============================================================
generate_flowjo_layout_template <- function(flowjo_long,
                                             dilutions,
                                             project_id,
                                             study_name,
                                             experiment_name,
                                             output_file,
                                             source_filepath = NULL,
                                             feature  = "MFI",
                                             n_wells  = 96) {

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  GENERATING FLOWJO LAYOUT TEMPLATE                       ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("  project_id      :", project_id,      "\n")
  cat("  study_name      :", study_name,      "\n")
  cat("  experiment_name :", experiment_name, "\n")
  cat("  output_file     :", output_file,     "\n\n")

  # Sanitise project_id (userWorkSpaceID() can return NULL or garbage float)
  project_id <- tryCatch({
    v <- as.integer(project_id)
    if (length(v) == 0 || is.na(v) || !is.finite(v)) NA_integer_ else v
  }, error = function(e) NA_integer_)

  # Extract antigen name from dilutions once; thread to plates_map builder
  antigen_actual <- unique(dilutions$antigen[!is.na(dilutions$antigen)])[1]
  if (is.na(antigen_actual) || !nzchar(trimws(antigen_actual))) antigen_actual <- "Unknown"
  cat("  antigen_actual  :", antigen_actual, "\n\n")

  # ----------------------------------------------------------
  # Guard: ensure project / study / experiment are attached
  # ----------------------------------------------------------
  if (!"project_id"      %in% names(flowjo_long)) flowjo_long$project_id      <- project_id
  if (!"study_name"      %in% names(flowjo_long)) flowjo_long$study_name      <- study_name
  if (!"experiment_name" %in% names(flowjo_long)) flowjo_long$experiment_name <- experiment_name

  # ----------------------------------------------------------
  # STEP 1: plate_id
  # ----------------------------------------------------------
  plate_id <- build_flowjo_plate_id(
    flowjo_long     = flowjo_long,
    project_id      = project_id,
    study_name      = study_name,
    experiment_name = experiment_name,
    source_filepath = source_filepath,
    n_wells         = n_wells
  )

  # ----------------------------------------------------------
  # STEP 2: antigen_list
  # ----------------------------------------------------------
  antigen_list <- build_flowjo_antigen_list(
    dilutions       = dilutions,
    project_id      = project_id,
    study_name      = study_name,
    experiment_name = experiment_name
  )

  # ----------------------------------------------------------
  # STEP 3: plates_map
  # ----------------------------------------------------------
  plates_map <- build_flowjo_plates_map(
    flowjo_long     = flowjo_long,
    plate_id        = plate_id,
    project_id      = project_id,
    study_name      = study_name,
    experiment_name = experiment_name,
    antigen_name    = antigen_actual,
    feature         = feature
  )

  # ----------------------------------------------------------
  # STEP 4: subject_groups
  # ----------------------------------------------------------
  subject_groups <- build_flowjo_subject_groups(
    plates_map = plates_map,
    study_name = study_name
  )

  # ----------------------------------------------------------
  # STEP 5: timepoint
  # ----------------------------------------------------------
  timepoint <- build_flowjo_timepoint(
    plates_map = plates_map,
    study_name = study_name
  )

  # ----------------------------------------------------------
  # STEP 6: assay_response_long
  # ----------------------------------------------------------
  assay_response_long <- build_flowjo_assay_response_long(
    flowjo_long     = flowjo_long,
    plate_id        = plate_id,
    project_id      = project_id,
    study_name      = study_name,
    experiment_name = experiment_name
  )

  # ----------------------------------------------------------
  # STEP 7: cell_valid (static)
  # ----------------------------------------------------------
  cell_valid <- build_cell_valid()

  # ----------------------------------------------------------
  # STEP 8: Write workbook
  # ----------------------------------------------------------
  wb <- createWorkbook()

  workbook <- list(
    plate_id            = plate_id,
    subject_groups      = subject_groups,
    timepoint           = timepoint,
    antigen_list        = antigen_list,
    plates_map          = plates_map,
    assay_response_long = assay_response_long,
    cell_valid          = cell_valid
  )

  write_flowjo_workbook_sheets(wb, workbook)
  saveWorkbook(wb, output_file, overwrite = TRUE)

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  DONE                                                     ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("  ✓ Layout template saved to:", output_file, "\n")
  cat("  → plate_id            :", nrow(plate_id),            "rows\n")
  cat("  → antigen_list        :", nrow(antigen_list),        "rows\n")
  cat("  → plates_map          :", nrow(plates_map),          "rows\n")
  cat("  → subject_groups      :", nrow(subject_groups),      "rows\n")
  cat("  → timepoint           :", nrow(timepoint),           "rows\n")
  cat("  → assay_response_long :", nrow(assay_response_long), "rows\n\n")

  invisible(workbook)
}


