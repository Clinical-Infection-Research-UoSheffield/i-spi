# ---- Wavelength sentinel — used instead of NA/NULL so that SQL UNIQUE constraints ----
# and R joins work correctly for bead-array data (no wavelength).
# Must match the value used in the DB migration.
# 
WL_NONE <- "__none__"

#' Replace NA or empty wavelength values with the sentinel
normalize_wavelength <- function(x) {
 ifelse(is.na(x) | trimws(x) == "", WL_NONE, as.character(x))
}

fetch_study_parameters <- function(study_accession, param_user, param_group = "standard_curve_options", project_id = userWorkSpaceID(), conn) {
  query <- glue("
  SELECT study_accession, param_name, param_boolean_value, param_character_value
	FROM madi_results.xmap_study_config
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND param_user = '{param_user}'
  AND param_group = '{param_group}';
")
  study_parameters <- dbGetQuery(conn, query)
  return(list(
    applyProzone = study_parameters[study_parameters$param_name=="applyProzone", "param_boolean_value"],
    blank_option = study_parameters[study_parameters$param_name=="blank_option", "param_character_value"],
    standard_source = study_parameters[study_parameters$param_name=="default_source", "param_character_value"],
    is_log_response = study_parameters[study_parameters$param_name=="is_log_mfi_axis", "param_boolean_value"],
    is_log_independent = TRUE,
    mean_mfi = study_parameters[study_parameters$param_name=="mean_mfi", "param_boolean_value"]
  ))
}

fetch_antigen_parameters <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
  SELECT
    xmap_antigen_family_id,
    study_accession,
    experiment_accession,
    antigen,
    l_asy_min_constraint,
    l_asy_max_constraint,
    l_asy_constraint_method,
    standard_curve_concentration,
    pcov_threshold
  FROM madi_results.xmap_antigen_family
  WHERE project_id = {project_id}
  AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'
  AND l_asy_constraint_method IS NOT NULL;
")
  antigen_constraints <- dbGetQuery(conn, query)
  return(antigen_constraints=antigen_constraints)
}


fetch_db_header <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution,plate_id,
  assay_response_variable, assay_independent_variable, nominal_sample_dilution, project_id
  FROM madi_results.xmap_header
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_header_experiments <- function(study_accession, conn, verbose = TRUE) {
  query <- glue("SELECT study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, plate_id,
  assay_response_variable, assay_independent_variable
  FROM madi_results.xmap_header
WHERE study_accession = '{study_accession}'
")
  header_data <- dbGetQuery(conn, query)
  header_data <- distinct(header_data)
  return(header_data)
}

fetch_db_standards <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, feature, plate_id, stype, source, wavelength, sampleid, well, dilution, antigen, antibody_mfi AS mfi, nominal_sample_dilution
  FROM madi_results.xmap_standard
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  standard_df  <- dbGetQuery(conn, query)
  standard_df <- distinct(standard_df)
  return(standard_df)
}

fetch_db_buffer <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, stype, source, wavelength, well, antigen, dilution, 
  feature, antibody_mfi AS mfi, nominal_sample_dilution 
  FROM madi_results.xmap_buffer
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  blank_data <- dbGetQuery(conn, query)
  blank_data <- distinct(blank_data)
  return(blank_data)
}

fetch_db_controls <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession, experiment_accession, plate_id, well, stype, source, wavelength, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi as MFI, antibody_n
                    feature, project_id, plateid, nominal_sample_dilution, plate
                  	FROM madi_results.xmap_control
              WHERE project_id = {project_id}
              AND study_accession = '{study_accession}'
              AND experiment_accession = '{experiment_accession}';")

  control_data <- dbGetQuery(conn, query)
  control_data <- distinct(control_data)

  return(control_data)
}

fetch_db_samples <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT study_accession,
experiment_accession, plate_id, timeperiod, patientid,
well, stype, source, wavelength, sampleid,  agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi AS mfi,
antibody_n, nominal_sample_dilution, feature FROM madi_results.xmap_sample
WHERE project_id = {project_id}
AND study_accession = '{study_accession}'
AND experiment_accession = '{experiment_accession}'
")
  sample_data <- dbGetQuery(conn, query)
  sample_data <- distinct(sample_data)
  return(sample_data)
}

fix_source_nom <- function(df, std_prefix) {
  
  suffix <- sub("^[^|]*", "", df$source_nom)
  df$source_nom <- paste0(std_prefix, suffix)
  
  df
}

pull_data <- function(study_accession, experiment_accession, project_id, conn = conn) {
  plates <- fetch_db_header(study_accession = study_accession,
                            experiment_accession = experiment_accession,
                            project_id = project_id,
                            conn = conn)
  plates$plate_id <- trimws(plates$plate_id)

  plates$plate_nom <- paste(plates$plate, plates$nominal_sample_dilution, sep = "-")


  antigen_constraints <- fetch_antigen_parameters(study_accession = study_accession,
                                                  experiment_accession = experiment_accession,
                                                  project_id = project_id,
                                                  conn = conn)

  standard_curve_data <- fetch_db_standards(study_accession = study_accession,
                                            experiment_accession = experiment_accession,
                                            project_id = project_id,
                                            conn = conn)
  standard_curve_data$plate_id <- trimws(standard_curve_data$plate_id)

  # cat("plates\n")
  # print(names(plates))
  # cat("standard_curve data\n")
  # print(names(standard_curve_data))

  standards <- inner_join(standard_curve_data, plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
   ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))[ ,c("project_id", "study_accession","experiment_accession","feature", "source", "wavelength","plateid",
                                                                                                                        "plate", "stype", "nominal_sample_dilution",
                                                                                                                        "sampleid","well","dilution","antigen","mfi",
                                                                                                                        "assay_response_variable", "assay_independent_variable")]
  if (nrow(standards) == 0) {
    warning(paste("[pull_data] No standards found after join for study:",
                  study_accession, "experiment:", experiment_accession,
                  "- returning NULL"))
    return(NULL)
  }
 standards$plate_nom <- paste(standards$plate, standards$nominal_sample_dilution, sep = "-")

  blanks <- inner_join(fetch_db_buffer(study_accession = study_accession,
                                       experiment_accession = experiment_accession,
                                       project_id = project_id,
                                       conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                       plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
                                  ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))

  blanks$plate_nom <- paste(blanks$plate, blanks$nominal_sample_dilution, sep = "-")

  samples <- inner_join(fetch_db_samples(study_accession = study_accession,
                                         experiment_accession = experiment_accession,
                                         project_id = project_id,
                                         conn = conn) %>% dplyr::mutate(plate_id = trimws(as.character(plate_id))),
                        plates[, c("study_accession", "experiment_accession" ,"plateid", "plate", "plate_id" , "assay_response_variable" ,"assay_independent_variable"
                                   ,"project_id")], by = c("study_accession", "experiment_accession","plate_id"))

  samples$plate_nom <- paste(samples$plate, samples$nominal_sample_dilution, sep = "-")

  response_var <- unique(plates$assay_response_variable)
  response_var <- response_var[!is.na(response_var) & nzchar(response_var)]
  if (length(response_var) == 0L) response_var <- "mfi"
  if (length(response_var)  > 1L) response_var <- response_var[[1L]]

  indep_var <- unique(plates$assay_independent_variable)
  indep_var <- indep_var[!is.na(indep_var) & nzchar(indep_var)]
  if (length(indep_var) == 0L) indep_var <- "concentration"
  if (length(indep_var)  > 1L) indep_var <- indep_var[[1L]]

  # ====================================================================
  # RESPONSE VARIABLE COLUMN ALIGNMENT
  # ====================================================================
  # The DB always stores the measurement in 'antibody_mfi' (aliased as 'mfi').
  # For bead arrays, assay_response_variable = "mfi" → column name matches.
  # For ELISA, assay_response_variable = "absorbance" → column name mismatch.
  # Rename the data column so downstream code can use response_var directly.
  if (length(response_var) == 1 && response_var != "mfi") {
    if ("mfi" %in% names(standards)) names(standards)[names(standards) == "mfi"] <- response_var
    if ("mfi" %in% names(blanks))    names(blanks)[names(blanks) == "mfi"] <- response_var
    if ("mfi" %in% names(samples))   names(samples)[names(samples) == "mfi"] <- response_var
    cat("  ✓ Renamed 'mfi' column to '", response_var, "' for ELISA compatibility\n", sep = "")
  }

  # ====================================================================
  # WAVELENGTH DETECTION AND source_nom CONSTRUCTION
  # ====================================================================
  # For ELISA, wavelength identifies distinct measurement channels.
  # For bead array, wavelength is not applicable.
  # Try to get wavelength from the data; if not available, derive from context.
  
  # Check if wavelength column exists in any of the data frames
  has_wavelength_std <- "wavelength" %in% names(standards)
  has_wavelength_blk <- "wavelength" %in% names(blanks)
  has_wavelength_smp <- "wavelength" %in% names(samples)
  
  # Add wavelength column if not present
  if (!has_wavelength_std) standards$wavelength <- WL_NONE
  if (!has_wavelength_blk) blanks$wavelength    <- WL_NONE
  if (!has_wavelength_smp) samples$wavelength   <- WL_NONE
  
  # Normalize any NA/empty wavelength values from DB to sentinel
  standards$wavelength <- normalize_wavelength(standards$wavelength)
  blanks$wavelength    <- normalize_wavelength(blanks$wavelength)
  samples$wavelength   <- normalize_wavelength(samples$wavelength)
  
  # Construct source_nom: combines source with wavelength for ELISA
  # For bead array (wavelength == WL_NONE), source_nom = source
  build_source_nom <- function(df) {
    src <- if ("source" %in% names(df)) as.character(df$source) else rep(NA_character_, nrow(df))
    src[is.na(src) | trimws(src) == ""] <- "unknown"
    wl <- as.character(df$wavelength)
    ifelse(
      is.na(wl) | trimws(wl) == "" | wl == WL_NONE,
      src,
      paste0(src, "|", wl, "_nm")
    )
  }
  
  standards$source_nom <- build_source_nom(standards)
  blanks$source_nom    <- build_source_nom(blanks)
  samples$source_nom   <- build_source_nom(samples)
  
  cat("  source_nom values (standards):", paste(unique(standards$source_nom), collapse = ", "), "\n")
  
  # ====================================================================
  # CHECK IF SOURCE PREFIXES DIFFER FROM STANDARDS
  # ====================================================================
  
  std_prefix <- unique(sub("\\|.*$", "", standards$source_nom))
  
  if (length(std_prefix) == 1) {
    
    sample_prefix <- unique(sub("\\|.*$", "", samples$source_nom))
    blank_prefix  <- unique(sub("\\|.*$", "", blanks$source_nom))
    
    needs_fix <- !(all(sample_prefix == std_prefix) & all(blank_prefix == std_prefix))
    
    if (needs_fix) {
      
      
      samples <- fix_source_nom(samples, std_prefix)
      blanks  <- fix_source_nom(blanks, std_prefix)
      
      cat("✓ source_nom prefixes updated to match standards:", std_prefix, "\n")
      
    } else {
      cat("✓ source_nom prefixes already match standards\n")
    }
    
  } else {
    cat("⚠ Multiple standard source prefixes detected:",
        paste(std_prefix, collapse = ", "), "\n")
  }
  
  mcmc_samples <- fetch_best_sample_robust_concentrations(study_accession = study_accession,
                                                               experiment_accession = experiment_accession,
                                                               project_id = project_id,
                                                               conn = conn)
  
  #mcmc_samples_read <<- mcmc_samples
  
  mcmc_pred <- fetch_best_pred_robust_concentrations(study_accession = study_accession,
                                                     experiment_accession = experiment_accession,
                                                     project_id = project_id,
                                                     conn = conn)
  
  # ── Normalize MCMC data for ELISA wavelength compatibility ──────────
  # Apply the same wavelength normalization and source_nom construction
  # that standards/blanks/samples receive above, so that
  # select_antigen_plate can filter MCMC data consistently.
  if (nrow(mcmc_samples) > 0) {
    if (!"wavelength" %in% names(mcmc_samples)) {
      mcmc_samples$wavelength <- WL_NONE
    }
    mcmc_samples$wavelength <- normalize_wavelength(mcmc_samples$wavelength)
    mcmc_samples$source_nom <- build_source_nom(mcmc_samples)
    
    # Ensure plate_nom exists and is consistent
    if (all(c("plate", "nominal_sample_dilution") %in% names(mcmc_samples))) {
      mcmc_samples$plate_nom <- paste(
        mcmc_samples$plate, mcmc_samples$nominal_sample_dilution, sep = "-"
      )
    }
    
    # Fix source_nom prefix to match standards (same logic as above)
    if (length(std_prefix) == 1) {
      mcmc_samp_prefix <- unique(sub("\\|.*$", "", mcmc_samples$source_nom))
      if (!all(mcmc_samp_prefix == std_prefix)) {
        mcmc_samples <- fix_source_nom(mcmc_samples, std_prefix)
        cat("  ✓ mcmc_samples source_nom prefixes updated to match standards\n")
      }
    }
    
    # Rename response column for ELISA compatibility
    if (length(response_var) == 1 && response_var != "mfi" &&
        "mfi" %in% names(mcmc_samples)) {
      names(mcmc_samples)[names(mcmc_samples) == "mfi"] <- response_var
    }
  }
  
  if (nrow(mcmc_pred) > 0) {
    if (!"wavelength" %in% names(mcmc_pred)) {
      mcmc_pred$wavelength <- WL_NONE
    }
    mcmc_pred$wavelength <- normalize_wavelength(mcmc_pred$wavelength)
    mcmc_pred$source_nom <- build_source_nom(mcmc_pred)
    
    if (all(c("plate", "nominal_sample_dilution") %in% names(mcmc_pred))) {
      mcmc_pred$plate_nom <- paste(
        mcmc_pred$plate, mcmc_pred$nominal_sample_dilution, sep = "-"
      )
    }
    
    if (length(std_prefix) == 1) {
      mcmc_pred_prefix <- unique(sub("\\|.*$", "", mcmc_pred$source_nom))
      if (!all(mcmc_pred_prefix == std_prefix)) {
        mcmc_pred <- fix_source_nom(mcmc_pred, std_prefix)
        cat("  ✓ mcmc_pred source_nom prefixes updated to match standards\n")
      }
    }
  }
  
  #mcmc_samples_2 <<- mcmc_samples
  
  return(list(plates=plates, standards=standards,
              blanks=blanks, samples=samples,
              mcmc_samples = mcmc_samples,
              mcmc_pred = mcmc_pred,
              antigen_constraints=antigen_constraints,
              response_var = response_var,
              indep_var = indep_var)
  )
}

apply_source_nom <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Normalize wavelength
  if ("wavelength" %in% names(df)) {
    df$wavelength <- normalize_wavelength(df$wavelength)
  } else {
    df$wavelength <- WL_NONE
  }
  
  # Build source_nom (same as pull_data)
  if ("source" %in% names(df)) {
    df$source_nom <- build_source_nom(df$source, df$wavelength)
  }
  
  # Build plate_nom if plate and nominal_sample_dilution exist
  if (all(c("plate", "nominal_sample_dilution") %in% names(df))) {
    df$plate_nom <- paste0(df$plate, "-", df$nominal_sample_dilution)
  }
  
  df
}

#' Overwrite source with source|wavelength for ELISA data
#' For bead array (wavelength == WL_NONE or missing), source is left unchanged.
enrich_source_with_wavelength <- function(df) {
  if (!"wavelength" %in% names(df) || !"source" %in% names(df)) return(df)
  
  src <- as.character(df$source)
  src[is.na(src) | trimws(src) == ""] <- "unknown"
  wl  <- as.character(df$wavelength)
  
  df$source <- ifelse(
    is.na(wl) | trimws(wl) == "" | wl == WL_NONE,
    src,
    paste0(src, "|", wl, "_nm")
  )
  
  df
}


# align_source_prefixes <- function(standards_df, target_df) {
#   if (is.null(target_df) || nrow(target_df) == 0) return(target_df)
#   if (is.null(standards_df) || nrow(standards_df) == 0) return(target_df)
#   
#   std_prefixes <- unique(sub("\\|.*$", "", standards_df$source_nom))
#   tgt_prefixes <- unique(sub("\\|.*$", "", target_df$source_nom))
#   
#   if (length(std_prefixes) == 1 && length(tgt_prefixes) == 1 &&
#       std_prefixes != tgt_prefixes) {
#     message(sprintf("  Aligning source_nom prefix: '%s' -> '%s'", 
#                     tgt_prefixes, std_prefixes))
#     target_df$source_nom <- sub(
#       paste0("^", gsub("([.|()\\^{}+$*?])", "\\\\\\1", tgt_prefixes)),
#       std_prefixes,
#       target_df$source_nom
#     )
#   }
#   
#   target_df
# }

shiny_notify <- function(session = shiny::getDefaultReactiveDomain()) {
  function(msg) {
    shiny::showNotification(
      msg,
      type = "message",
      session = session
    )
  }
}

# ---- DIAGNOSTIC: Pre-upsert data inspector ----
#' ---- Diagnose potential duplicate-key issues before upserting ----
#' Call this immediately before upsert_best_curve() to see exactly what
#' the data looks like in the natural-key space.  The output goes to the
#' R console (message()) and is also returned invisibly as a list.
#'
#' @param df         data.frame to be upserted
#' @param table      target table name (used to look up natural keys)
#' @param conn       optional DBI connection – if supplied the function also
#'                   checks which rows already exist in the DB on the NK
#' @param schema     DB schema (default "madi_results")
#' @return invisibly, a list with elements: nk, n_rows, n_distinct_nk,
#'         dup_keys (data.frame of duplicated NK combos), wavelength_info
diagnose_upsert_data <- function(df, table, conn = NULL, schema = "madi_results") {

  nk <- get_natural_keys(table)
  if (is.null(nk)) {
    message("[diagnose] Unknown table: ", table)
    return(invisible(NULL))
  }

  hdr <- paste0(
    "\n",
    strrep("=", 70), "\n",
    "  UPSERT DIAGNOSTIC — table: ", table, "\n",
    strrep("=", 70)
  )
  message(hdr)

  # ----- basic shape -----
  message("  Incoming rows        : ", nrow(df))
  message("  Incoming columns     : ", paste(names(df), collapse = ", "))
  message("  Natural key columns  : ", paste(nk, collapse = ", "))

  missing_nk <- setdiff(nk, names(df))
  if (length(missing_nk) > 0) {
    message("  ** MISSING NK cols   : ", paste(missing_nk, collapse = ", "))
  }

  present_nk <- intersect(nk, names(df))
  
  message("\n  --- HEAD of incoming data ---")
  print(head(df))

  # ----- NA audit per NK column -----
  for (col in present_nk) {
    n_na <- sum(is.na(df[[col]]))
    n_empty <- if (is.character(df[[col]])) sum(trimws(df[[col]]) == "", na.rm = TRUE) else 0
    uniq <- length(unique(df[[col]]))
    message(sprintf("  %-30s  NA=%d  empty=%d  unique=%d  sample: %s",
                    col, n_na, n_empty, uniq,
                    paste(head(unique(df[[col]]), 4), collapse = ", ")))
  }

  # ----- wavelength-specific diagnostics -----
  wl_info <- NULL
  if ("wavelength" %in% names(df)) {
    wl_vals    <- unique(df$wavelength)
    wl_na      <- sum(is.na(df$wavelength))
    wl_sentinel <- sum(df$wavelength == WL_NONE, na.rm = TRUE)
    wl_info    <- list(values = wl_vals, n_na = wl_na, n_sentinel = wl_sentinel)
    message("  wavelength values    : ", paste(wl_vals, collapse = ", "),
            "  (NA: ", wl_na, ", sentinel '", WL_NONE, "': ", wl_sentinel, ")")
    if (wl_na > 0) {
      message("  ** WARNING: wavelength has NA values — these should be '", WL_NONE, "'")
    }
  } else {
    message("  wavelength column    : NOT PRESENT in data")
  }

  # ----- duplicate detection on NK -----
  if (length(present_nk) > 0) {
    nk_df   <- df[, present_nk, drop = FALSE]
    dup_idx <- duplicated(nk_df)
    n_dup   <- sum(dup_idx)
    message("  Distinct NK combos   : ", nrow(unique(nk_df)))
    message("  Duplicate NK rows    : ", n_dup)

    if (n_dup > 0) {
      dup_keys <- unique(nk_df[dup_idx, , drop = FALSE])
      message("  ** DUPLICATE NK combos (first 5):")
      print(head(dup_keys, 5))
    } else {
      dup_keys <- data.frame()
    }
  } else {
    dup_keys <- data.frame()
  }

  # ----- check for within-data duplicates on the DB constraint (6-col) -----
  # This catches the case where NK in R has more columns than DB constraint
  nk_no_wl <- setdiff(present_nk, "wavelength")
  if (length(nk_no_wl) > 0 && "wavelength" %in% present_nk) {
    nk6_df <- df[, nk_no_wl, drop = FALSE]
    dup6   <- sum(duplicated(nk6_df))
    message("  Distinct NK combos WITHOUT wavelength : ", nrow(unique(nk6_df)))
    message("  Duplicate rows WITHOUT wavelength     : ", dup6)
    if (dup6 > 0) {
      message("  ** WARNING: Data has rows that differ ONLY by wavelength.")
      message("     If the DB unique constraint does NOT include wavelength,")
      message("     the INSERT will fail with a duplicate-key violation.")
    }
  }

  # ----- optional: check DB for existing rows -----
  if (!is.null(conn) && DBI::dbIsValid(conn) && length(present_nk) > 0) {
    tryCatch({
      # Sample first NK combo to check
      sample_row <- df[1, present_nk, drop = FALSE]
      conditions <- vapply(present_nk, function(col) {
        val <- sample_row[[col]]
        if (is.na(val)) {
          paste0(col, " IS NULL")
        } else {
          paste0(col, " = '", gsub("'", "''", as.character(val)), "'")
        }
      }, character(1))
      check_sql <- sprintf(
        "SELECT COUNT(*) AS n FROM %s.%s WHERE %s",
        schema, table, paste(conditions, collapse = " AND ")
      )
      existing_n <- DBI::dbGetQuery(conn, check_sql)$n
      message("  Existing DB rows matching first NK combo: ", existing_n)
    }, error = function(e) {
      message("  DB check skipped: ", conditionMessage(e))
    })
  }

  message(strrep("=", 70), "\n")

  invisible(list(
    nk             = nk,
    n_rows         = nrow(df),
    n_distinct_nk  = if (length(present_nk) > 0) nrow(unique(df[, present_nk, drop = FALSE])) else NA,
    dup_keys       = dup_keys,
    wavelength_info = wl_info
  ))
}

upsert_best_curve <- function(conn,
                              df,
                              schema = "madi_results",
                              table  = "best_plate_all",
                              notify = NULL,
                              quiet  = FALSE,
                              batch_size = 50000,
                              use_copy = TRUE,
                              skip_index_check = FALSE,
                              use_on_conflict = TRUE,
                              shiny_mode = TRUE) {
  
  if (is.null(notify)) {
    notify <- function(msg) if (!quiet) message(Sys.time(), " - ", msg)
  }
  bail <- function(msg) {
    notify(msg)
    invisible(FALSE)
  }
  
  if (!DBI::dbIsValid(conn)) return(bail("Database connection is not valid."))
  if (!is.data.frame(df) || nrow(df) == 0) return(bail("No data provided."))
  
  old_names <- names(df)
  names(df) <- tolower(names(df))
  changed <- old_names != names(df)
  if (any(changed)) {
    message(sprintf(
      "[upsert_best_curve] Lowercased %d column name(s) for table '%s': %s",
      sum(changed), table,
      paste(sprintf("%s -> %s", old_names[changed], names(df)[changed]), collapse = ", ")
    ))
  }
  
  # Remove obsolete column if present
  if ("sample_dilution_factor" %in% names(df)) {
    message(sprintf("[upsert_best_curve] Removing obsolete 'sample_dilution_factor' from %s", table))
    df$sample_dilution_factor <- NULL
  }
  
  # Ensure 'feature' column exists (required by NK for most tables)
  nk <- get_natural_keys(table)
  if (!is.null(nk) && "feature" %in% nk && !"feature" %in% names(df)) {
    message(sprintf("[upsert_best_curve] 'feature' missing from %s — filling FEAT_NONE.", table))
    df$feature <- FEAT_NONE
  }
  
  # Normalize feature values (NA/empty → sentinel) for consistent NK matching
  if ("feature" %in% names(df)) {
    df$feature <- normalize_feature(df$feature)
  }
  
  ##
  ## Defensive column filter: drop any R-only columns not in the DB table.
  ## Prevents errors from internal routing columns (source_nom, plate_nom, etc.)
  ##
  tryCatch({
    db_cols_query <- sprintf(
      "SELECT column_name FROM information_schema.columns WHERE table_schema = '%s' AND table_name = '%s'",
      schema, table
    )
    db_cols <- DBI::dbGetQuery(conn, db_cols_query)$column_name
    if (length(db_cols) > 0) {
      extra_cols <- setdiff(names(df), db_cols)
      if (length(extra_cols) > 0) {
        message(sprintf(
          "[upsert_best_curve] Dropping %d column(s) not in %s.%s: %s",
          length(extra_cols), schema, table,
          paste(extra_cols, collapse = ", ")
        ))
        df <- df[, names(df) %in% db_cols, drop = FALSE]
      }
    }
  }, error = function(e) {
    message(sprintf("[upsert_best_curve] Column introspection failed for %s: %s — skipping filter.",
                    table, conditionMessage(e)))
  })
  
  ##
  ## Run diagnostics before any DB operations
  ##
  diag <- diagnose_upsert_data(df, table, conn = conn, schema = schema)

  ##
  ## De-duplicate incoming data on natural keys
  ## This prevents within-batch duplicate-key violations
  ##

  cols <- names(df)
  nk   <- get_natural_keys(table)
  pk   <- get_primary_key(table)
  
  if (is.null(nk)) stop("Unknown table: ", table)

  present_nk <- intersect(nk, cols)
  if (length(present_nk) > 0) {
    n_before <- nrow(df)
    df <- df[!duplicated(df[, present_nk, drop = FALSE]), , drop = FALSE]
    n_after <- nrow(df)
    if (n_before != n_after) {
      message(sprintf("[upsert_best_curve] De-duplicated %s: %d -> %d rows (removed %d NK dupes)",
                      table, n_before, n_after, n_before - n_after))
    }
  }

  missing_keys <- setdiff(nk, cols)
  if (length(missing_keys) > 0) {
    stop("Missing natural-key columns: ", paste(missing_keys, collapse = ", "))
  }
  
  nk_has_na <- sapply(df[, nk, drop = FALSE], function(x) any(is.na(x)))
  if (any(nk_has_na)) {
    cols_with_na    <- names(nk_has_na)[nk_has_na]
    rows_per_col    <- colSums(is.na(df[, cols_with_na, drop = FALSE]))
    problematic_rows <- which(rowSums(is.na(df[, cols_with_na, drop = FALSE])) > 0)
    
    msg <- paste0(
      "Natural-key column(s) contain NA in table `", table, "`:\n",
      paste0(" - ", cols_with_na, ": ", rows_per_col, " NA(s)"),
      collapse = "\n"
    )
    if (length(problematic_rows) > 0) {
      shown <- head(problematic_rows, 3)
      msg <- paste0(
        msg,
        "\nFirst ", length(shown), " problematic row(s): ",
        paste(shown, collapse = ", "),
        if (length(problematic_rows) > 3) " …"
      )
    }
    return(bail(msg))
  }
  
  sql_parts <- build_sql_components_pk(conn, schema, table, cols, nk, pk)

  ##
  ## Select upsert strategy
  ## Phase 1 (default): scoped DELETE + bulk INSERT — works with nullable NKs
  ## Phase 2 (use_on_conflict = TRUE): INSERT ... ON CONFLICT — requires NOT NULL NKs
  ##
  upsert_fn <- if (use_on_conflict) upsert_batch_on_conflict else upsert_batch_pk
  strategy_label <- if (use_on_conflict) "ON CONFLICT" else "scoped DELETE"
  message(sprintf("[upsert_best_curve] %s: %d rows, strategy=%s", table, nrow(df), strategy_label))

  ##
  ## Batch processing for large datasets
  ##
  n_rows <- nrow(df)

  if (n_rows > batch_size) {
    notif_id <- paste0("upsert_", table)
    
    if (shiny_mode) {
      showNotification(
        sprintf("Processing %d rows in %d batches", n_rows, ceiling(n_rows / batch_size)),
        id       = notif_id,
        duration = 3,
        type     = "message"
      )
    } else {
      message(sprintf("Processing %d rows in %d batches", n_rows, ceiling(n_rows / batch_size)))
    }
    
    batches <- split(df, ceiling(seq_len(n_rows) / batch_size))
    
    results <- tryCatch({
      vapply(seq_along(batches), function(i) {
        if (shiny_mode) {
          showNotification(
            sprintf("Batch %d/%d (%d rows)", i, length(batches), nrow(batches[[i]])),
            id       = notif_id,
            duration = NULL,
            type     = "message"
          )
          # upsert_fn(conn, batches[[i]], table, sql_parts, use_copy, notify)
        } else {
          message(sprintf("Batch %d/%d (%d rows)", i, length(batches), nrow(batches[[i]])))
        }
        upsert_batch_pk(conn, batches[[i]], table, sql_parts, use_copy, notify,
                        shiny_mode = shiny_mode)

      }, logical(1))
    }, error = function(e) {
      if (shiny_mode) {
        showNotification(
          sprintf("Error during batch processing: %s", conditionMessage(e)),
          id       = notif_id,
          duration = NULL,
          type     = "error"
        )
      } else {
        message(sprintf("Error during batch processing: %s", conditionMessage(e)))
      }
      return(NULL)
    })
    
    if (is.null(results)) return(invisible(FALSE))
    
    if (all(results)) {
      if (shiny_mode) {
        removeNotification(notif_id)
        showNotification(
          sprintf("All %d batches completed successfully", length(batches)),
          duration = 5,
          type     = "message"
        )
      } else {
        message(sprintf("All %d batches completed successfully", length(batches)))
      }
      return(invisible(TRUE))
    } else {
      if (shiny_mode) {
        showNotification(
          sprintf("%d/%d batches failed", sum(!results), length(batches)),
          id       = notif_id,
          duration = NULL,
          type     = "error"
        )
      } else {
        message(sprintf("%d/%d batches failed", sum(!results), length(batches)))
      }
      return(invisible(FALSE))
    }
  }
  
  # Single batch
  ok <- tryCatch({
    # upsert_fn(conn, df, table, sql_parts, use_copy, notify)
    upsert_batch_pk(conn, df, table, sql_parts, use_copy, notify,
                    shiny_mode = shiny_mode)
  }, error = function(e) {
    if (shiny_mode) {
      showNotification(
        sprintf("Upsert failed: %s", conditionMessage(e)),
        duration = NULL,
        type     = "error"
      )
    } else {
      message(sprintf("Upsert failed: %s", conditionMessage(e)))
    }
    return(FALSE)
  })
  
  if (ok) {
    if (shiny_mode) {
      showNotification(
        paste0(table, " upsert completed (", n_rows, " rows)."),
        duration = 5,
        type     = "message"
      )
    } else {
      message(paste0(table, " upsert completed (", n_rows, " rows)."))
    }
  }
  
  invisible(ok)
}

## Get primary key for each table
get_primary_key <- function(table) {
  keys <- list(
    best_plate_all = "best_plate_all_id",
    best_glance_all = "best_glance_all_id",
    best_tidy_all = "best_tidy_all_id",
    best_sample_se_all = "best_sample_se_all_id",
    best_standard_all = "best_standard_all_id",
    best_pred_all = "best_pred_all_id"
  )
  keys[[table]]
}

## Build SQL components for primary key based upsert
build_sql_components_pk <- function(conn, schema, table, cols, nk, pk) {
  schema_id <- as.character(DBI::dbQuoteIdentifier(conn, schema))
  table_id <- as.character(DBI::dbQuoteIdentifier(conn, table))

  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  pk_quoted <- as.character(DBI::dbQuoteIdentifier(conn, pk))

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  # Build WHERE clause for natural key matching
  # Use IS NOT DISTINCT FROM instead of = to handle NULL values correctly
  # (NULL = NULL evaluates to NULL/FALSE in SQL, but IS NOT DISTINCT FROM
  #  treats two NULLs as equal, which is the correct behavior for NK matching)
  nk_conditions <- vapply(nk, function(x) {
    col_quoted <- as.character(DBI::dbQuoteIdentifier(conn, x))
    paste0("t.", col_quoted, " IS NOT DISTINCT FROM tmp.", col_quoted)
  }, character(1), USE.NAMES = FALSE)

  nk_where_clause <- paste(nk_conditions, collapse = " AND ")

  # Build UPDATE SET clause for non-key columns
  update_cols <- setdiff(cols, c(nk, pk))
  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)
    set_clause <- paste(
      vapply(update_quoted, function(col) paste0(col, " = tmp.", col), character(1)),
      collapse = ", "
    )
  } else {
    set_clause <- NULL
  }

  # Columns for INSERT (excluding primary key - let it auto-generate)
  insert_cols <- setdiff(cols, pk)
  insert_cols_quoted <- vapply(insert_cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)
  insert_cols_list <- paste(insert_cols_quoted, collapse = ", ")

  list(
    schema_id = schema_id,
    table_id = table_id,
    pk = pk,
    pk_quoted = pk_quoted,
    cols = cols,
    cols_list = cols_list,
    insert_cols = insert_cols,
    insert_cols_list = insert_cols_list,
    nk = nk,
    nk_list = nk_list,
    nk_where_clause = nk_where_clause,
    set_clause = set_clause
  )
}


## ────────────────────────────────────────────────────────────────────
## PLATE SCOPE columns — the leading prefix of every UNIQUE constraint.
## Within a batch, ALL rows for each plate scope are regenerated,
## so we can safely DELETE by scope (fast index scan) rather than
## matching every row on the full 11-14 column NK (slow IS NOT DISTINCT FROM).
## ────────────────────────────────────────────────────────────────────
SCOPE_COLS <- c("project_id", "study_accession", "experiment_accession",
                "plateid", "plate", "nominal_sample_dilution")

## Execute single batch using SCOPED DELETE + bulk INSERT
## This replaces the old row-level IS NOT DISTINCT FROM join with a
## set-level DELETE by plate scope, which is orders of magnitude faster
## because it uses the leading columns of the UNIQUE constraint B-tree index.
upsert_batch_pk <- function(conn, df, table, sql_parts, use_copy, notify,
                            shiny_mode = TRUE) {
  
  tryCatch({
    DBI::dbWithTransaction(conn, {
      tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
      tmp_id   <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))
      temp_cols <- setdiff(sql_parts$cols, sql_parts$pk)
      df_temp <- df[, temp_cols, drop = FALSE]
      create_sql <- sprintf(
        "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
        tmp_id,
        paste(sprintf("%s %s",
                      vapply(temp_cols, function(x)
                        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
                      vapply(df_temp, pg_type_map, character(1))
        ), collapse = ", ")
      )
      DBI::dbExecute(conn, create_sql)

      # Load data into temp table using COPY (binary protocol)

      if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
        RPostgres::dbWriteTable(
          conn, tmp_name, df_temp,
          append = TRUE, row.names = FALSE, copy = TRUE
        )
      } else {
        DBI::dbWriteTable(conn, tmp_name, df_temp,
                          append = TRUE, row.names = FALSE)
      }

      # Let PostgreSQL know the temp table's size for better query plans
      DBI::dbExecute(conn, sprintf("ANALYZE %s", tmp_id))

      # ── Step 1: Scoped DELETE ──────────────────────────────────────
      # Delete ALL existing rows whose plate-scope matches any incoming
      # plate-scope. This is safe because a batch always regenerates
      # complete results for each plate scope.
      #
      # Uses the leading columns of the UNIQUE index → index scan,
      # not the old 11-14 column IS NOT DISTINCT FROM → seq scan.
      scope_available <- intersect(SCOPE_COLS, temp_cols)
      scope_quoted <- vapply(scope_available, function(x)
        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)

      delete_sql <- sprintf(
        "DELETE FROM %s.%s t
         WHERE EXISTS (
           SELECT 1 FROM (
             SELECT DISTINCT %s FROM %s
           ) scope
           WHERE %s
         )",
        sql_parts$schema_id, sql_parts$table_id,
        paste(scope_quoted, collapse = ", "),
        tmp_id,
        paste(sprintf("t.%s = scope.%s", scope_quoted, scope_quoted), collapse = " AND ")
      )
      n_deleted <- DBI::dbExecute(conn, delete_sql)
      message(sprintf("[upsert_batch_pk] %s: scoped DELETE removed %d existing rows", table, n_deleted))
      
      # delete_sql <- sprintf(
      #   "DELETE FROM %s.%s t USING %s tmp WHERE %s",
      #   sql_parts$schema_id, sql_parts$table_id,
      #   tmp_id, sql_parts$nk_where_clause
      # )
      # DBI::dbExecute(conn, delete_sql)

      # ── Step 2: Bulk INSERT ────────────────────────────────────────

      insert_sql <- sprintf(
        "INSERT INTO %s.%s (%s) SELECT %s FROM %s",
        sql_parts$schema_id, sql_parts$table_id,
        sql_parts$insert_cols_list, sql_parts$insert_cols_list, tmp_id
      )
      n_inserted <- DBI::dbExecute(conn, insert_sql)
      message(sprintf("[upsert_batch_pk] %s: INSERT added %d rows", table, n_inserted))
    })
    TRUE
  }, error = function(e) {
    msg <- paste0("Batch failed for ", table, ": ", conditionMessage(e))
    if (shiny_mode && !is.null(shiny::getDefaultReactiveDomain())) {
      showNotification(
        id = "error_batch", msg,
        duration = NULL, closeButton = TRUE, type = "error"
      )
    } else {
      message(msg)
    }
    FALSE
  })
}

## ────────────────────────────────────────────────────────────────────
## Feature sentinel — same pattern as wavelength.
## Use AFTER running migration_phase2_not_null_nk.sql.
## ────────────────────────────────────────────────────────────────────
FEAT_NONE <- "__none__"

#' Replace NA or empty feature values with the sentinel
normalize_feature <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", FEAT_NONE, as.character(x))
}

## ────────────────────────────────────────────────────────────────────
## Phase 2: ON CONFLICT upsert (requires NOT NULL NK columns)
## ────────────────────────────────────────────────────────────────────
## Activate by passing use_on_conflict = TRUE to upsert_best_curve
## AFTER running migration_phase2_not_null_nk.sql.
##
## This is the fastest possible upsert strategy:
## - Single SQL statement: INSERT ... ON CONFLICT ... DO UPDATE SET
## - PostgreSQL uses the B-tree index on the UNIQUE constraint directly
## - No DELETE, no two-pass, no IS NOT DISTINCT FROM
## ────────────────────────────────────────────────────────────────────
upsert_batch_on_conflict <- function(conn, df, table, sql_parts, use_copy, notify) {
  tryCatch({
    DBI::dbWithTransaction(conn, {
      tmp_name <- paste0("tmp_", substr(digest::digest(Sys.time()), 1, 8))
      tmp_id <- as.character(DBI::dbQuoteIdentifier(conn, tmp_name))

      temp_cols <- setdiff(sql_parts$cols, sql_parts$pk)
      df_temp <- df[, temp_cols, drop = FALSE]

      create_sql <- sprintf(
        "CREATE TEMP TABLE %s (%s) ON COMMIT DROP",
        tmp_id,
        paste(sprintf("%s %s",
                      vapply(temp_cols, function(x) as.character(DBI::dbQuoteIdentifier(conn, x)), character(1)),
                      vapply(df_temp, pg_type_map, character(1))
        ), collapse = ", ")
      )
      DBI::dbExecute(conn, create_sql)

      if (use_copy && requireNamespace("RPostgres", quietly = TRUE)) {
        RPostgres::dbWriteTable(
          conn, tmp_name, df_temp,
          append = TRUE, row.names = FALSE, copy = TRUE
        )
      } else {
        DBI::dbWriteTable(conn, tmp_name, df_temp, append = TRUE, row.names = FALSE)
      }

      DBI::dbExecute(conn, sprintf("ANALYZE %s", tmp_id))

      # Build ON CONFLICT upsert using the UNIQUE constraint name
      constraint_name <- paste0(table, "_nk")

      # Non-key, non-PK columns to update on conflict
      update_cols <- setdiff(temp_cols, sql_parts$nk)
      insert_cols_quoted <- vapply(temp_cols, function(x)
        as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)
      insert_cols_list <- paste(insert_cols_quoted, collapse = ", ")

      if (length(update_cols) > 0) {
        update_quoted <- vapply(update_cols, function(x)
          as.character(DBI::dbQuoteIdentifier(conn, x)), character(1), USE.NAMES = FALSE)
        set_clause <- paste(
          vapply(update_quoted, function(col) paste0(col, " = EXCLUDED.", col), character(1)),
          collapse = ", "
        )
        conflict_action <- paste("DO UPDATE SET", set_clause)
      } else {
        conflict_action <- "DO NOTHING"
      }

      upsert_sql <- sprintf(
        "INSERT INTO %s.%s (%s)
         SELECT %s FROM %s
         ON CONFLICT ON CONSTRAINT %s
         %s",
        sql_parts$schema_id, sql_parts$table_id,
        insert_cols_list,
        insert_cols_list,
        tmp_id,
        DBI::dbQuoteIdentifier(conn, constraint_name),
        conflict_action
      )
      n_upserted <- DBI::dbExecute(conn, upsert_sql)
      message(sprintf("[upsert_batch_on_conflict] %s: ON CONFLICT upserted %d rows", table, n_upserted))
    })
    TRUE
  }, error = function(e) {
    showNotification(
      id = "error_batch",
      paste0("Batch failed for ", table, " (ON CONFLICT): ", conditionMessage(e)),
      duration = NULL, closeButton = TRUE, type = "error"
    )
    FALSE
  })
}

## Map R types to PostgreSQL types
pg_type_map <- function(col) {
  switch(class(col)[1],
         "integer" = "INTEGER",
         "numeric" = "DOUBLE PRECISION",
         "character" = "TEXT",
         "logical" = "BOOLEAN",
         "Date" = "DATE",
         "POSIXct" = "TIMESTAMPTZ",
         "POSIXlt" = "TIMESTAMPTZ",
         "factor" = "TEXT",
         "TEXT"
  )
}

## Helper: Get natural keys for table

get_natural_keys <- function(table) {
  keys <- list(
    best_plate_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength"
    ),
    best_glance_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature"
    ),
    best_tidy_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "term"
    ),
    best_sample_se_all = c( "project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature",
      "patientid", "timeperiod", "sampleid", "dilution"
    ),
    best_standard_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "well"
    ), # dilution not included as it can be NA when geometric mean is used
    best_pred_all = c("project_id",
      "study_accession", "experiment_accession",
      "plateid", "plate", "nominal_sample_dilution", "source", "wavelength", "antigen", "feature", "x"
    )
  )

  keys[[table]]
}

## Helper: Build UPSERT SQL using glue_sql

build_upsert_sql_glue <- function(conn, schema, table, tmp_name, cols, nk) {

  ## Pre-quote identifiers to avoid glue_sql conflicts
  schema_id <- DBI::dbQuoteIdentifier(conn, schema)
  table_id <- DBI::dbQuoteIdentifier(conn, table)
  tmp_id <- DBI::dbQuoteIdentifier(conn, tmp_name)

  ## Quote column names
  cols_quoted <- vapply(cols, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  nk_quoted <- vapply(nk, function(x) {
    as.character(DBI::dbQuoteIdentifier(conn, x))
  }, character(1), USE.NAMES = FALSE)

  ## Build column list strings

  cols_list <- paste(cols_quoted, collapse = ", ")
  nk_list <- paste(nk_quoted, collapse = ", ")

  ## Build SET clause for non-key columns
  update_cols <- setdiff(cols, nk)

  if (length(update_cols) > 0) {
    update_quoted <- vapply(update_cols, function(x) {
      as.character(DBI::dbQuoteIdentifier(conn, x))
    }, character(1), USE.NAMES = FALSE)

    set_parts <- vapply(update_quoted, function(col) {
      paste0(col, " = EXCLUDED.", col)
    }, character(1), USE.NAMES = FALSE)

    set_clause <- paste(set_parts, collapse = ", ")
    conflict_action <- paste("DO UPDATE SET", set_clause)
  } else {
    conflict_action <- "DO NOTHING"
  }

  ## Build final SQL using glue_sql with DBI::SQL for pre-quoted parts
  glue::glue_sql(
    "INSERT INTO {DBI::SQL(schema_id)}.{DBI::SQL(table_id)} ({DBI::SQL(cols_list)})
     SELECT {DBI::SQL(cols_list)}
     FROM {DBI::SQL(tmp_id)}
     ON CONFLICT ({DBI::SQL(nk_list)})
     {DBI::SQL(conflict_action)}",
    .con = conn
  )
}

select_antigen_plate <- function(loaded_data,
                                 study_accession = study_accession,
                                 experiment_accession = experiment_accession,
                                 source = source,
                                 antigen = antigen,
                                 plate = plate,
                                 wavelength = WL_NONE,
                                 antigen_constraints = antigen_constraints) {
  print("select antigen plate in batch\n")
  print("plate in\n")
  print(plate)
  print(paste("wavelength in:", wavelength))
  print("standards structure\n")
  print(str(loaded_data$standards))
  print("antigens\n")
  print(unique(loaded_data$standards$antigen))
  print("source_nom\n")
  print(unique(loaded_data$standards$source_nom))
  print("plate_nom\n")
  print(unique(loaded_data$standards$plate_nom))
  print("plate\n")
  print(unique(loaded_data$standards$plate))
  
  # ── Filter standards ───────────────────────────────────────────────
  if ("source_nom" %in% names(loaded_data$standards)) {
    plate_standard <- loaded_data$standards[
      loaded_data$standards$source_nom == source &
        loaded_data$standards$antigen    == antigen &
        loaded_data$standards$plate_nom  == plate, ]
  } else {
    plate_standard <- loaded_data$standards[
      loaded_data$standards$source    == source &
        loaded_data$standards$antigen   == antigen &
        loaded_data$standards$plate_nom == plate, ]
  }
  
  # ── Filter by wavelength for standards ────────────────────────────
  if ("wavelength" %in% names(plate_standard) &&
      !is.null(wavelength) && wavelength != WL_NONE) {
    plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
    wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
    cat("wavelength filter:\n")
    print(wl_filter)
    if (any(wl_filter)) {
      plate_standard <- plate_standard[wl_filter, , drop = FALSE]
    } else {
      message(sprintf(
        "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
        wavelength, nrow(plate_standard),
        paste(unique(plate_standard$wavelength), collapse = ", ")
      ))
    }
  }
  
  # ── Guard against empty plate_standard data ───────────────────────
  if (is.null(plate_standard) || nrow(plate_standard) == 0) {
    warning(paste("No standard curve data found for:",
                  "source =", source,
                  ", antigen =", antigen,
                  ", plate =", plate))
    return(NULL)
  }
  
  # ── Filter blanks ─────────────────────────────────────────────────
  plate_blanks <- loaded_data$blanks[
    loaded_data$blanks$antigen   == antigen &
      loaded_data$blanks$plate_nom == plate, ]
  
  # ── Filter samples ────────────────────────────────────────────────
  plate_samples <- loaded_data$samples[
    loaded_data$samples$antigen   == antigen &
      loaded_data$samples$plate_nom == plate, ]
  
  # # ── Filter mcmc_samples — same pattern as blanks and samples ──────
  # plate_mcmc_samples <- if (!is.null(loaded_data$mcmc_samples) &&
  #                           nrow(loaded_data$mcmc_samples) > 0) {
  #   loaded_data$mcmc_samples[
  #     loaded_data$mcmc_samples$antigen   == antigen &
  #       loaded_data$mcmc_samples$plate_nom == plate, , drop = FALSE]
  # } else {
  #   data.frame()
  # }
  # 
  # # ── Filter mcmc_pred — dense prediction grid with MCMC pCoV ──────
  # plate_mcmc_pred <- if (!is.null(loaded_data$mcmc_pred) &&
  #                        nrow(loaded_data$mcmc_pred) > 0) {
  #   loaded_data$mcmc_pred[
  #     loaded_data$mcmc_pred$antigen   == antigen &
  #       loaded_data$mcmc_pred$plate_nom == plate, , drop = FALSE]
  # } else {
  #   data.frame()
  # }
  
  # ── Filter mcmc_samples — match by antigen, plate_nom, AND source_nom ──
  plate_mcmc_samples <- if (!is.null(loaded_data$mcmc_samples) &&
                            nrow(loaded_data$mcmc_samples) > 0) {
    mcmc_df <- loaded_data$mcmc_samples
    filter_mask <- mcmc_df$antigen == antigen & mcmc_df$plate_nom == plate
    # Also filter by source_nom if available (critical for ELISA multi-wavelength)
    if ("source_nom" %in% names(mcmc_df)) {
      filter_mask <- filter_mask & mcmc_df$source_nom == source
    }
    mcmc_df[filter_mask, , drop = FALSE]
  } else {
    data.frame()
  }
  
  # ── Filter mcmc_pred — match by antigen, plate_nom, AND source_nom ──
  plate_mcmc_pred <- if (!is.null(loaded_data$mcmc_pred) &&
                         nrow(loaded_data$mcmc_pred) > 0) {
    pred_df <- loaded_data$mcmc_pred
    filter_mask <- pred_df$antigen == antigen & pred_df$plate_nom == plate
    # Also filter by source_nom if available (critical for ELISA multi-wavelength)
    if ("source_nom" %in% names(pred_df)) {
      filter_mask <- filter_mask & pred_df$source_nom == source
    }
    pred_df[filter_mask, , drop = FALSE]
  } else {
    data.frame()
  }
  
  # ── Filter blanks, samples, mcmc_samples, mcmc_pred by wavelength ─
  if (!is.null(wavelength) && wavelength != WL_NONE) {
    if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
      wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
      if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
      wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
      if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_mcmc_samples) && nrow(plate_mcmc_samples) > 0) {
      wl_m <- plate_mcmc_samples$wavelength == normalize_wavelength(wavelength)
      if (any(wl_m)) plate_mcmc_samples <- plate_mcmc_samples[wl_m, , drop = FALSE]
    }
    if ("wavelength" %in% names(plate_mcmc_pred) && nrow(plate_mcmc_pred) > 0) {
      wl_p <- plate_mcmc_pred$wavelength == normalize_wavelength(wavelength)
      if (any(wl_p)) plate_mcmc_pred <- plate_mcmc_pred[wl_p, , drop = FALSE]
    }
  }
  
  # anything after - is removed (nominal sample dilutions)
  plate_c <- sub("-.*$", "", plate)
  
  # ── Resolve response column ───────────────────────────────────────
  response_col <- resolve_response_col(plate_standard)
  
  # ── Antigen settings ──────────────────────────────────────────────
  antigen_settings <- obtain_lower_constraint(
    dat                  = plate_standard,
    antigen              = antigen,
    study_accession      = study_accession,
    experiment_accession = experiment_accession,
    plate                = plate_c,
    plateid              = unique(plate_standard$plateid),
    plate_blanks         = plate_blanks,
    antigen_constraints  = antigen_constraints,
    response_col         = response_col
  )
  
  # ── Fixed lower asymptote ─────────────────────────────────────────
  fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
  fixed_a_result <- validate_fixed_lower_asymptote(
    fixed_a_result_raw = fixed_a_result,
    verbose            = TRUE
  )
  
  # ── Blank standard error ──────────────────────────────────────────
  std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
  
  # ── Sort mcmc_pred by x for smooth line drawing ───────────────────
  if (nrow(plate_mcmc_pred) > 0 && "x" %in% names(plate_mcmc_pred)) {
    plate_mcmc_pred <- plate_mcmc_pred[order(plate_mcmc_pred$x), , drop = FALSE]
  }
  
  # ── Return ────────────────────────────────────────────────────────
  return(list(
    plate_standard     = plate_standard,
    plate_blanks       = plate_blanks,
    plate_samples      = plate_samples,
    plate_mcmc_samples = plate_mcmc_samples,
    plate_mcmc_pred    = plate_mcmc_pred,
    antigen_settings   = antigen_settings,
    fixed_a_result     = fixed_a_result,
    std_error_blank    = std_error_blank
  ))
}
# select_antigen_plate <- function(loaded_data,
#                                  study_accession = study_accession,
#                                  experiment_accession = experiment_accession,
#                                  source = source,
#                                  antigen = antigen,
#                                  plate = plate,
#                                  wavelength = WL_NONE,
#                                  antigen_constraints = antigen_constraints) {
#   
#   print("select antigen plate in batch\n")
#   print("plate in\n")
#   print(plate)
#   print(paste("wavelength in:", wavelength))
#   print("standards structure\n")
#   print(str(loaded_data$standards))
#   print("antigens\n")
#   print(unique(loaded_data$standards$antigen))
#   print("source_nom\n")
#   print(unique(loaded_data$standards$source_nom))
#   print("plate_nom\n")
#   print(unique(loaded_data$standards$plate_nom))
#   print("plate\n")
#   print(unique(loaded_data$standards$plate))
#   
#   # ── Filter standards ───────────────────────────────────────────────
#   if ("source_nom" %in% names(loaded_data$standards)) {
#     plate_standard <- loaded_data$standards[
#       loaded_data$standards$source_nom == source &
#         loaded_data$standards$antigen    == antigen &
#         loaded_data$standards$plate_nom  == plate, ]
#   } else {
#     plate_standard <- loaded_data$standards[
#       loaded_data$standards$source    == source &
#         loaded_data$standards$antigen   == antigen &
#         loaded_data$standards$plate_nom == plate, ]
#   }
#   
#   # ── Filter by wavelength for standards ────────────────────────────
#   if ("wavelength" %in% names(plate_standard) &&
#       !is.null(wavelength) && wavelength != WL_NONE) {
#     plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
#     wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
#     if (any(wl_filter)) {
#       plate_standard <- plate_standard[wl_filter, , drop = FALSE]
#     } else {
#       message(sprintf(
#         "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
#         wavelength, nrow(plate_standard),
#         paste(unique(plate_standard$wavelength), collapse = ", ")
#       ))
#     }
#   }
#   
#   # ── Guard against empty plate_standard data ───────────────────────
#   if (is.null(plate_standard) || nrow(plate_standard) == 0) {
#     warning(paste("No standard curve data found for:",
#                   "source =", source,
#                   ", antigen =", antigen,
#                   ", plate =", plate))
#     return(NULL)
#   }
#   
#   # ── Filter blanks ─────────────────────────────────────────────────
#   plate_blanks <- loaded_data$blanks[
#     loaded_data$blanks$antigen   == antigen &
#       loaded_data$blanks$plate_nom == plate, ]
#   
#   # ── Filter samples ────────────────────────────────────────────────
#   plate_samples <- loaded_data$samples[
#     loaded_data$samples$antigen   == antigen &
#       loaded_data$samples$plate_nom == plate, ]
#   
#   # ── Filter mcmc_samples — same pattern as blanks and samples ──────
#   plate_mcmc_samples <- loaded_data$mcmc_samples[
#     loaded_data$mcmc_samples$antigen   == antigen &
#       loaded_data$mcmc_samples$plate_nom == plate, ]
#   
#   # ── Filter blanks, samples, mcmc_samples by wavelength ────────────
#   if (!is.null(wavelength) && wavelength != WL_NONE) {
#     if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
#       wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
#       wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_mcmc_samples) && nrow(plate_mcmc_samples) > 0) {
#       wl_m <- plate_mcmc_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_m)) plate_mcmc_samples <- plate_mcmc_samples[wl_m, , drop = FALSE]
#     }
#   }
#   
#   # anything after - is removed (nominal sample dilutions)
#   plate_c <- sub("-.*$", "", plate)
#   
#   # ── Resolve response column ───────────────────────────────────────
#   response_col <- resolve_response_col(plate_standard)
#   
#   # ── Antigen settings ──────────────────────────────────────────────
#   antigen_settings <- obtain_lower_constraint(
#     dat                  = plate_standard,
#     antigen              = antigen,
#     study_accession      = study_accession,
#     experiment_accession = experiment_accession,
#     plate                = plate_c,
#     plateid              = unique(plate_standard$plateid),
#     plate_blanks         = plate_blanks,
#     antigen_constraints  = antigen_constraints,
#     response_col         = response_col
#   )
#   
#   # ── Fixed lower asymptote ─────────────────────────────────────────
#   fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
#   fixed_a_result <- validate_fixed_lower_asymptote(
#     fixed_a_result_raw = fixed_a_result,
#     verbose            = TRUE
#   )
#   
#   # ── Blank standard error ──────────────────────────────────────────
#   std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
#   
#   # ── Return ────────────────────────────────────────────────────────
#   return(list(
#     plate_standard     = plate_standard,
#     plate_blanks       = plate_blanks,
#     plate_samples      = plate_samples,
#     plate_mcmc_samples = plate_mcmc_samples,
#     antigen_settings   = antigen_settings,
#     fixed_a_result     = fixed_a_result,
#     std_error_blank    = std_error_blank
#   ))
# }
# select_antigen_plate <- function(loaded_data,
#                                  study_accession = study_accession,
#                                  experiment_accession = experiment_accession,
#                                  source = source,
#                                  antigen = antigen,
#                                  plate = plate,
#                                  wavelength = WL_NONE,
#                                  antigen_constraints = antigen_constraints){
#   print("select antigen plate in batch\n")
#   print("plate in\n")
#   print(plate)
#   print(paste("wavelength in:", wavelength))
#   print("standards structure\n")
#   print(str(loaded_data$standards))
#   print("antigens\n")
#   print(unique(loaded_data$standards$antigen))
#   print("source_nom\n")
#   print(unique(loaded_data$standards$source_nom))
#   print("plate_nom\n")
#   print(unique(loaded_data$standards$plate_nom))
# 
#   print("plate\n")
#   print(unique(loaded_data$standards$plate))
# 
#   # Use source_nom for filtering if available, fall back to source
#   if ("source_nom" %in% names(loaded_data$standards)) {
#     plate_standard  <- loaded_data$standards[loaded_data$standards$source_nom == source &
#                                                loaded_data$standards$antigen == antigen &
#                                                loaded_data$standards$plate_nom == plate ,]
#   } else {
#     plate_standard  <- loaded_data$standards[loaded_data$standards$source == source &
#                                                loaded_data$standards$antigen == antigen &
#                                                loaded_data$standards$plate_nom == plate ,]
#   }
#   
#   # ── Filter by wavelength when data contains multiple wavelengths ──
#   if ("wavelength" %in% names(plate_standard) && 
#       !is.null(wavelength) && wavelength != WL_NONE) {
#     plate_standard$wavelength <- normalize_wavelength(plate_standard$wavelength)
#     wl_filter <- plate_standard$wavelength == normalize_wavelength(wavelength)
#     if (any(wl_filter)) {
#       plate_standard <- plate_standard[wl_filter, , drop = FALSE]
#     } else {
#       message(sprintf(
#         "[select_antigen_plate] WARNING: wavelength '%s' matched 0 rows; keeping all %d rows. Wavelengths in data: %s",
#         wavelength, nrow(plate_standard),
#         paste(unique(plate_standard$wavelength), collapse = ", ")
#       ))
#     }
#   }
#   # Guard against empty plate_standard data
#   if (is.null(plate_standard) || nrow(plate_standard) == 0) {
#     warning(paste("No standard curve data found for:",
#                   "source =", source,
#                   ", antigen =", antigen,
#                   ", plate =", plate))
#     return(NULL)
#   }
# 
#   plate_blanks <- loaded_data$blanks[loaded_data$blanks$antigen == antigen &
#                                        loaded_data$blanks$plate_nom == plate,]
# 
#   plate_samples <- loaded_data$samples[loaded_data$samples$antigen == antigen &
#                                          loaded_data$samples$plate_nom == plate,]
# 
#   # ── Filter blanks and samples by wavelength when relevant ──
#   if (!is.null(wavelength) && wavelength != WL_NONE) {
#     if ("wavelength" %in% names(plate_blanks) && nrow(plate_blanks) > 0) {
#       plate_blanks$wavelength <- normalize_wavelength(plate_blanks$wavelength)
#       wl_b <- plate_blanks$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_b)) plate_blanks <- plate_blanks[wl_b, , drop = FALSE]
#     }
#     if ("wavelength" %in% names(plate_samples) && nrow(plate_samples) > 0) {
#       plate_samples$wavelength <- normalize_wavelength(plate_samples$wavelength)
#       wl_s <- plate_samples$wavelength == normalize_wavelength(wavelength)
#       if (any(wl_s)) plate_samples <- plate_samples[wl_s, , drop = FALSE]
#     }
#   }
# 
#   # anything after - is removed (nominal sample dilutions)
#   plate_c <- sub("-.*$", "", plate)
# 
#   # Resolve response column for this data (mfi for bead array, absorbance for ELISA)
#   response_col <- resolve_response_col(plate_standard)
# 
#   antigen_settings <- obtain_lower_constraint(dat = plate_standard,
#                                               antigen = antigen,
#                                               study_accession = study_accession,
#                                               experiment_accession = experiment_accession,
#                                               plate = plate_c,
# 
#                                               plateid = unique(plate_standard$plateid),
# 
#                                               plate_blanks = plate_blanks,
#                                               antigen_constraints = antigen_constraints,
#                                               response_col = response_col)
# 
# 
#   fixed_a_result <- resolve_fixed_lower_asymptote(antigen_settings)
#   fixed_a_result <- validate_fixed_lower_asymptote(
#     fixed_a_result_raw = fixed_a_result,
#     verbose = TRUE
#   )
# 
#   std_error_blank <- get_blank_se(antigen_settings = antigen_settings)
# 
#   return (list(plate_standard=plate_standard,
#                plate_blanks=plate_blanks,
#                plate_samples=plate_samples,
#                antigen_settings=antigen_settings,
#                fixed_a_result = fixed_a_result,
#                std_error_blank = std_error_blank))
# }

#### Fetch saved results from std_curver
fetch_best_plate_all <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("
SELECT best_plate_all_id, project_id, study_accession, experiment_accession, feature, source, wavelength, plateid, plate, nominal_sample_dilution, assay_response_variable, assay_independent_variable
	FROM madi_results.best_plate_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
	AND experiment_accession = '{experiment_accession}';
")
  best_plate_all <- dbGetQuery(conn, query)
  return(best_plate_all)
}

fetch_best_tidy_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_tidy_all_id, project_id, study_accession, experiment_accession, term, lower, upper, estimate, std_error, statistic, 
  p_value, nominal_sample_dilution, antigen, feature, plateid, plate, source, wavelength
	FROM madi_results.best_tidy_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'")
  best_tidy_all <- dbGetQuery(conn, query)
  return(best_tidy_all)
}

fetch_best_pred_all <- function(study_accession, experiment_accession, project_id, conn) {

  query <- glue("SELECT best_pred_all_id, project_id, x, model, yhat, overall_se, predicted_concentration, se_x, pcov, study_accession, experiment_accession, 
  nominal_sample_dilution, plateid, plate,
  antigen, feature, source, wavelength, best_glance_all_id, raw_robust_concentration, final_robust_concentration, se_robust_concentration, pcov_robust_concentration
	FROM madi_results.best_pred_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_pred_all <- dbGetQuery(conn, query)
  return(best_pred_all)
}

fetch_best_standard_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_standard_all_id, project_id, study_accession, experiment_accession, feature, source, wavelength, plateid, plate, stype, nominal_sample_dilution
  , sampleid, well, dilution, antigen, feature, assay_response, assay_response_variable, assay_independent_variable
  , concentration, g, best_glance_all_id
	FROM madi_results.best_standard_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_standard_all <- dbGetQuery(conn, query)
  return(best_standard_all)
}

fetch_best_glance_all <- function(study_accession,experiment_accession, project_id, conn) {
  query <- glue("SELECT best_glance_all_id, project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution
  , antigen, feature, iter, status, crit, a, b, c, d, g, lloq, uloq, lloq_y, uloq_y, llod, ulod, inflect_x, inflect_y, std_error_blank
  , dydx_inflect, mindc, maxdc, minrdl, maxrdl, dfresidual, nobs, rsquare_fit, aic, bic, loglik, mse, cv, source, wavelength, bkg_method, 
  is_log_response, is_log_x, apply_prozone, formula, last_concentration_calc_method, lloq_fda2018_concentration, lloq_fda2018_response, uloq_fda2018_concentration,
  uloq_fda2018_response, blank_mean, blank_sd
	FROM madi_results.best_glance_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}';")
  best_glance_all <- dbGetQuery(conn, query)
  return(best_glance_all)
}

fetch_best_glance_mcmc <- function(study_accession, project_id, conn) {
  query <- glue("SELECT best_glance_all_id, project_id, study_accession, experiment_accession, plateid, plate,  
  nominal_sample_dilution, CONCAT(plate, '-',nominal_sample_dilution) as plate_nom,
  antigen, feature, source, wavelength, 
  iter, status, crit as model_name, a, b, c, d, g,  is_log_response,
  is_log_x, last_concentration_calc_method,
  CASE WHEN nobs > 1 THEN mse * dfresidual / (nobs - 1) ELSE NULL END AS resid_sample_variance
	FROM madi_results.best_glance_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
  AND nominal_sample_dilution is NOT NULL;")
  best_glance_all <- dbGetQuery(conn, query)
  
  best_glance_all$best_glance_all_id <- bit64::as.integer64(best_glance_all$best_glance_all_id)
  
  return(best_glance_all)
}

# fetch_best_pred_mcmc - fixed
fetch_best_pred_mcmc <- function(study_accession, project_id, best_glance_ids, conn) {
  ids_collapsed <- paste(best_glance_ids, collapse = ", ")
  query <- glue("
    SELECT best_pred_all_id, x as concentration,  1 as dilution, yhat as assay_response, best_glance_all_id,
    'pred_se' as mcmc_set
    FROM madi_results.best_pred_all
    WHERE project_id = {project_id}
      AND study_accession = '{study_accession}'
      AND best_glance_all_id IN ({ids_collapsed})
  ")
  dbGetQuery(conn, query)
}

# fetch_best_sample_se_mcmc - fixed
fetch_best_sample_se_mcmc <- function(study_accession, project_id, best_glance_ids, conn) {
  ids_collapsed <- paste(best_glance_ids, collapse = ", ")
  query <- glue("
    SELECT best_sample_se_all_id, assay_response_variable,
           dilution, assay_response, best_glance_all_id,
           'sample_se' as mcmc_set
    FROM madi_results.best_sample_se_all
    WHERE project_id = {project_id}
      AND study_accession = '{study_accession}'
      AND best_glance_all_id IN ({ids_collapsed})
  ")
  dbGetQuery(conn, query)
}

fetch_combined_mcmc <- function(study_accession, project_id, best_glance_ids, conn) {
  
  ids <- paste(best_glance_ids, collapse = ", ")
  
  query <- glue::glue("
    SELECT 
      best_pred_all_id  AS row_id,
      x                 AS concentration,
      1                 AS dilution,
      yhat              AS assay_response,
      best_glance_all_id,
      wavelength,
      feature,
      'pred_se'         AS mcmc_set
    FROM madi_results.best_pred_all
    WHERE project_id = {project_id}
      AND study_accession = '{study_accession}'
      AND best_glance_all_id IN ({ids})
    UNION ALL
    SELECT 
      best_sample_se_all_id AS row_id,
      NULL                  AS concentration,
      dilution,
      assay_response,
      best_glance_all_id,
      wavelength,
      feature,
      'sample_se'           AS mcmc_set
    FROM madi_results.best_sample_se_all
    WHERE project_id = {project_id}
      AND study_accession = '{study_accession}'
      AND best_glance_all_id IN ({ids})
  ")
  
  DBI::dbGetQuery(conn, query)
}
# fetch_combined_mcmc <- function(study_accession, project_id, best_glance_ids, conn) {
#   
#   ids <- paste(best_glance_ids, collapse = ", ")
#   
#   query <- glue::glue("
#     SELECT 
#       best_pred_all_id  AS row_id,
#       x                 AS concentration,
#       1                 AS dilution,
#       yhat              AS assay_response,
#       best_glance_all_id,
#       'pred_se'         AS mcmc_set
#     FROM madi_results.best_pred_all
#     WHERE project_id = {project_id}
#       AND study_accession = '{study_accession}'
#       AND best_glance_all_id IN ({ids})
# 
#     UNION ALL
# 
#     SELECT 
#       best_sample_se_all_id AS row_id,
#       NULL                  AS concentration,
#       dilution,
#       assay_response,
#       best_glance_all_id,
#       'sample_se'           AS mcmc_set
#     FROM madi_results.best_sample_se_all
#     WHERE project_id = {project_id}
#       AND study_accession = '{study_accession}'
#       AND best_glance_all_id IN ({ids})
#   ")
#   
#   DBI::dbGetQuery(conn, query)
# }


update_combined_mcmc_bulk <- function(pred_all_mcmc, sample_all_mcmc, best_glance_complete,  conn) {
  # -------------------------------------------------------------
  # 1️⃣  Write temporary staging tables
  # -------------------------------------------------------------
  dbExecute(conn, "CREATE TEMP TABLE tmp_pred (LIKE madi_results.best_pred_all INCLUDING ALL) ON COMMIT DROP;")
  dbExecute(conn, "CREATE TEMP TABLE tmp_samp (LIKE madi_results.best_sample_se_all INCLUDING ALL) ON COMMIT DROP;")
  dbExecute(conn, "CREATE TEMP TABLE tmp_glance (LIKE madi_results.best_glance_all INCLUDING ALL) ON COMMIT DROP;")
  
  
  # Only the columns we need – if the source tables have many extra columns,
  # we can select a subset before writing.
  dbWriteTable(conn, "tmp_pred", pred_all_mcmc, overwrite = TRUE, row.names = FALSE)
  dbWriteTable(conn, "tmp_samp", sample_all_mcmc, overwrite = TRUE, row.names = FALSE)
  dbWriteTable(conn, "tmp_glance", best_glance_complete, overwrite = TRUE, row.names = FALSE)
  
  # -------------------------------------------------------------
  # 2️⃣  Bulk UPDATE via JOIN
  # -------------------------------------------------------------
  sql_update_pred <- "
    UPDATE madi_results.best_pred_all AS tgt
    SET    raw_robust_concentration = src.raw_robust_concentration,
           se_robust_concentration   = src.se_robust_concentration,
           pcov_robust_concentration = src.pcov_robust_concentration
    FROM   tmp_pred AS src
    WHERE  tgt.best_pred_all_id = src.best_pred_all_id
  "
  
  sql_update_samp <- "
    UPDATE madi_results.best_sample_se_all AS tgt
    SET    raw_robust_concentration = src.raw_robust_concentration,
           se_robust_concentration   = src.se_robust_concentration,
           pcov_robust_concentration = src.pcov_robust_concentration,
           final_robust_concentration = src.final_robust_concentration
    FROM   tmp_samp AS src
    WHERE  tgt.best_sample_se_all_id = src.best_sample_se_all_id
  "
  
  sql_update_glance <- "
  UPDATE madi_results.best_glance_all AS tgt
  SET    last_concentration_calc_method = src.last_concentration_calc_method
  FROM tmp_glance as src
  WHERE  tgt.best_glance_all_id = src.best_glance_all_id
  "
  
  dbBegin(conn)
  dbExecute(conn, sql_update_pred)
  dbExecute(conn, sql_update_samp)
  dbExecute(conn, sql_update_glance)
  dbCommit(conn)   # one transaction covers both updates
  # Temp tables automatically drop at end of transaction because of ON COMMIT DROP
  invisible(TRUE)
}


fetch_best_sample_robust_concentrations <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT
    -- Sample identifiers
    bss.project_id,
    bss.study_accession,
    bss.experiment_accession,
    bss.patientid,
    bss.sampleid,
    bss.well,
    bss.stype,
    bss.agroup,
    bss.timeperiod,
    bss.antigen,
    bss.plateid,
    bss.plate,
    bss.nominal_sample_dilution,
    CONCAT(bss.plate, '-', bss.nominal_sample_dilution) AS plate_nom,
    -- Feature info
    bss.feature,
    bss.wavelength,
    bss.source,
    -- Robust concentration results
	bss.assay_response,
    bss.raw_robust_concentration,
    bss.final_robust_concentration,
    bss.se_robust_concentration,
    bss.pcov_robust_concentration,
    -- Key LOQ/LOD gating only
    bss.gate_class_loq,
    bss.gate_class_lod,
    -- Curve quality from glance
    bga.last_concentration_calc_method

FROM madi_results.best_sample_se_all bss
INNER JOIN madi_results.best_glance_all bga
    ON  bss.best_glance_all_id = bga.best_glance_all_id
    AND bss.study_accession    = bga.study_accession
    AND bss.plateid             = bga.plateid
    AND bss.antigen             = bga.antigen
    AND bss.feature             = bga.feature
	AND bss.source              = bga.source
    AND bss.wavelength          = bga.wavelength

WHERE bss.study_accession                = '{study_accession}'
  AND bss.experiment_accession               = '{experiment_accession}'
  AND bss.project_id                     = {project_id}
  AND bga.last_concentration_calc_method = 'mcmc_robust';")
  
  robust_sample_concentrations <- dbGetQuery(conn, query)
  return(robust_sample_concentrations)
}

fetch_best_pred_robust_concentrations <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT
    bpa.project_id,
    bpa.study_accession,
    bpa.experiment_accession,
    bpa.x,
    bpa.yhat,
    bpa.pcov,
    bpa.antigen,
    bpa.plateid,
    bpa.plate,
    bpa.nominal_sample_dilution,
    CONCAT(bpa.plate, '-', bpa.nominal_sample_dilution) AS plate_nom,
    bpa.feature,
    bpa.wavelength,
    bpa.source,
    -- MCMC robust results on the dense grid
    bpa.raw_robust_concentration,
    bpa.se_robust_concentration,
    bpa.pcov_robust_concentration,
    -- Curve quality from glance
    bga.last_concentration_calc_method
FROM madi_results.best_pred_all bpa
INNER JOIN madi_results.best_glance_all bga
    ON  bpa.best_glance_all_id = bga.best_glance_all_id
WHERE bpa.study_accession        = '{study_accession}'
  AND bpa.experiment_accession   = '{experiment_accession}'
  AND bpa.project_id             = {project_id}
  AND bga.last_concentration_calc_method = 'mcmc_robust';")
  robust_pred_concentrations <- dbGetQuery(conn, query)
  return(robust_pred_concentrations)
}
#' Fetch best_glance_all filtered by user's current study parameters
#' This ensures consistency with fetch_best_pred_all_summary and other summary functions
#' @param study_accession Study accession ID
#' @param experiment_accession Experiment accession ID
#' @param param_user Current user for parameter lookup
#' @param conn Database connection
#' @return Filtered best_glance_all dataframe matching user's current study configuration
fetch_best_glance_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
         AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      g.best_glance_all_id, g.project_id, g.study_accession, g.experiment_accession,
      g.plateid, g.plate, g.nominal_sample_dilution, g.antigen, g.feature, g.iter,
      g.status, g.crit, g.a, g.b, g.c, g.d, g.lloq, g.uloq, g.lloq_y,
      g.uloq_y, g.llod, g.ulod, g.inflect_x, g.inflect_y, g.std_error_blank,
      g.dydx_inflect, g.dfresidual, g.nobs, g.rsquare_fit, g.aic, g.bic,
      g.loglik, g.mse, g.cv, g.source, g.wavelength, g.bkg_method, g.is_log_response,
      g.is_log_x, g.apply_prozone, g.formula, g.g, g.last_concentration_calc_method,
      g.lloq_fda2018_concentration, g.lloq_fda2018_response, g.uloq_fda2018_concentration, g.uloq_fda2018_response, g.blank_mean, g.blank_sd
    FROM madi_results.best_glance_all g
    CROSS JOIN params
    WHERE g.project_id = {project_id}
      AND g.study_accession = {study_accession}
      AND g.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  result <- dbGetQuery(conn, query)
  #result <- apply_source_nom(result)
  return(result)
  
}

fetch_best_sample_se_all <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("

SELECT best_sample_se_all_id, raw_predicted_concentration, project_id, study_accession, experiment_accession, timeperiod, patientid, well, stype, sampleid,
agroup, pctaggbeads, samplingerrors, antigen, feature,
antibody_n, plateid, plate, nominal_sample_dilution, assay_response_variable, assay_independent_variable, dilution, overall_se, raw_assay_response, 
assay_response,
se_concentration, final_predicted_concentration, pcov, source, wavelength, gate_class_loq, gate_class_lod,
gate_class_pcov, best_glance_all_id, feature, norm_assay_response, raw_robust_concentration, final_robust_concentration, se_robust_concentration, pcov_robust_concentration
	FROM madi_results.best_sample_se_all
	WHERE project_id = {project_id}
	AND study_accession = '{study_accession}'
	AND experiment_accession = '{experiment_accession}';")
  best_sample_se_all <- dbGetQuery(conn, query)
  #best_sample_se_all <- apply_source_nom(result)

  return(best_sample_se_all)
}

## Adds the Bayesian concentration and SE to the dataframe
fetch_best_sampl_se_with_bayes <- function(study_accession, experiment_accession, project_id, conn) {
  query <- glue("SELECT
    -- Identifiers
    bse.best_sample_se_all_id,
    bse.project_id,
    bse.study_accession,
    bse.experiment_accession,
    bse.plateid,
    bse.plate,
    bse.patientid,
    bse.timeperiod,
    bse.well,
    bse.sampleid,
    bse.stype,
    bse.agroup,

    -- Assay target
    bse.antigen,
    bse.feature,
    bse.source,
    bse.wavelength,
    bse.antibody_n,

    -- Sample QC
    bse.pctaggbeads,
    bse.samplingerrors,
    bse.nominal_sample_dilution,
    bse.assay_response_variable,
    bse.assay_independent_variable,
    bse.dilution,
    bse.overall_se,

    -- Assay response
    bse.raw_assay_response,
    bse.assay_response,
    bse.norm_assay_response,

    -- Frequentist concentration estimates
    bse.raw_predicted_concentration     AS freq_raw_concentration,
    bse.final_predicted_concentration   AS freq_final_concentration,
    bse.se_concentration                AS freq_se_concentration,
    bse.pcov                            AS freq_pcov,

    -- Robust estimates
    bse.raw_robust_concentration,
    bse.final_robust_concentration,
    bse.se_robust_concentration,
    bse.pcov_robust_concentration,

    -- Frequentist gate classes
    bse.gate_class_loq,
    bse.gate_class_lod,
    bse.gate_class_pcov,
    
    -- Bayesian concentration estimates
    bs.raw_predicted_concentration      AS bayes_raw_concentration,
    bs.se_concentration                 AS bayes_se_concentration,
    bs.pcov                             AS bayes_pcov,
    bs.conc_lower                       AS bayes_conc_lower,
    bs.conc_upper                       AS bayes_conc_upper,
    bs.gate_class                       AS bayes_gate_class

FROM madi_results.best_sample_se_all bse
LEFT JOIN madi_results.bayes_samples bs
    ON  bse.project_id           = bs.project_id
    AND bse.study_accession      = bs.study_accession
    AND bse.experiment_accession = bs.experiment_accession
    AND bse.plateid               = bs.plateid
    AND bse.plate                 = bs.plate
    AND bse.patientid             = bs.patientid
    AND bse.sampleid              = bs.sampleid
    AND bse.well                  = bs.well
    AND bse.antigen               = bs.antigen
    AND bse.feature               = bs.feature
WHERE bse.project_id           = {project_id}
  AND bse.study_accession      = '{study_accession}'
  AND bse.experiment_accession = '{experiment_accession}'

ORDER BY best_sample_se_all_id")
  
  best_sample_qc_bayes <- dbGetQuery(conn, query)
  return(best_sample_qc_bayes)
}


#etch_concentration_calculation_status <- function(study_accession, experiment_accession, project_id, conn) {
#   query <- glue("
#   
#   
#   ")
# }

## Specific fetch queries for the summary of standard curves accounting for
# selected study_configuration based on glance_id
fetch_best_pred_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      p.best_pred_all_id, p.x, p.model, p.yhat, p.overall_se,
      p.predicted_concentration, p.se_x, p.pcov, p.project_id,
      p.study_accession, p.experiment_accession, p.nominal_sample_dilution,
      p.plateid, p.plate, p.antigen, p.feature, p.source , p.wavelength, p.best_glance_all_id,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone, g.last_concentration_calc_method
    FROM madi_results.best_pred_all p
    LEFT JOIN madi_results.best_glance_all g
      ON p.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE p.project_id = {project_id}
      AND p.study_accession = {study_accession}
      AND p.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_best_standard_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      s.best_standard_all_id, s.project_id, s.study_accession, s.experiment_accession,
      s.feature, s.source, s.wavelength, s.plateid, s.plate, s.stype, s.nominal_sample_dilution,
      s.sampleid, s.well, s.dilution, s.antigen, s.assay_response,
      s.assay_response_variable, s.assay_independent_variable,
      s.concentration, s.g, s.best_glance_all_id,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone
    FROM madi_results.best_standard_all s
    LEFT JOIN madi_results.best_glance_all g
      ON s.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE s.project_id = {project_id}
      AND s.study_accession = {study_accession}
      AND s.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  result <- dbGetQuery(conn, query)
 # result <- apply_source_nom(result)
  return(result)
  
}

fetch_best_sample_se_all_summary <- function(study_accession, experiment_accession, param_user, project_id, conn) {
  query <- glue_sql("
    WITH params AS (
      SELECT
        study_accession,
        BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
        MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
        BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
      FROM madi_results.xmap_study_config
      WHERE project_id = {project_id}
        AND study_accession = {study_accession}
        AND param_user = {param_user}
        AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
      GROUP BY study_accession
    )
    SELECT
      ss.best_sample_se_all_id, ss.raw_predicted_concentration,
      ss.study_accession, ss.experiment_accession, ss.timeperiod,
      ss.patientid, ss.well, ss.stype, ss.sampleid, ss.agroup,
      ss.pctaggbeads, ss.samplingerrors, ss.antigen, ss.antibody_n,
      ss.plateid, ss.plate, ss.nominal_sample_dilution,
      ss.assay_response_variable, ss.assay_independent_variable,
      ss.dilution, ss.overall_se, ss.assay_response, ss.se_concentration,
      ss.final_predicted_concentration, ss.pcov, ss.source, ss.wavelength, ss.gate_class_loq, ss.gate_class_lod,
      ss.gate_class_pcov, ss.best_glance_all_id, ss.feature, ss.norm_assay_response,
      g.is_log_response, g.is_log_x, g.bkg_method, g.apply_prozone
    FROM madi_results.best_sample_se_all ss
    LEFT JOIN madi_results.best_glance_all g
      ON ss.best_glance_all_id = g.best_glance_all_id
    CROSS JOIN params
    WHERE ss.project_id = {project_id}
      AND ss.study_accession = {study_accession}
      AND ss.experiment_accession = {experiment_accession}
      AND g.is_log_response = params.is_log_mfi_axis
      AND g.bkg_method = params.blank_option
      AND g.apply_prozone = params.apply_prozone",
                    .con = conn
  )
  dbGetQuery(conn, query)
}

fetch_current_sc_options_wide <- function(currentuser, study_accession, project_id, conn) {
  query <- glue_sql(
    "
SELECT
  study_accession,
  param_user,
  BOOL_OR(CASE WHEN param_name = 'is_log_mfi_axis' THEN param_boolean_value END) AS is_log_mfi_axis,
  MAX(CASE WHEN param_name = 'blank_option' THEN param_character_value END) AS blank_option,
  BOOL_OR(CASE WHEN param_name = 'applyProzone' THEN param_boolean_value END) AS apply_prozone
FROM madi_results.xmap_study_config
WHERE project_id = {project_id}
  AND study_accession = {study_accession}
  AND param_user = {currentuser}
  AND param_name IN ('is_log_mfi_axis', 'blank_option', 'applyProzone')
GROUP BY study_accession, param_user
",
    currentuser     = currentuser,
    study_accession = study_accession,
    .con = conn
  )

  print(query)
  dbGetQuery(conn, query)
}



attach_antigen_familes <- function(best_pred_all, antigen_families, default_family = "All Antigens") {
  # Handle case where antigen_families is NULL or empty
  if (is.null(antigen_families) || nrow(antigen_families) == 0) {
    # Create antigen_family column with default value for all antigens
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Ensure antigen_families has required columns
  required_cols <- c("study_accession", "antigen", "antigen_family")
  if (!all(required_cols %in% names(antigen_families))) {
    # If required columns are missing, use default family
    best_pred_all$antigen_family <- default_family
    return(best_pred_all)
  }

  # Perform the merge
  pred_with_antigen_familes <- merge(best_pred_all,
                                     antigen_families[, required_cols],
                                     by = c("study_accession", "antigen"),
                                     all.x = TRUE)

  # Replace NA/NULL antigen_family values with the default
  if ("antigen_family" %in% names(pred_with_antigen_familes)) {
    na_family <- is.na(pred_with_antigen_familes$antigen_family) |
      pred_with_antigen_familes$antigen_family == "" |
      is.null(pred_with_antigen_familes$antigen_family)
    pred_with_antigen_familes$antigen_family[na_family] <- default_family
  } else {
    # Column doesn't exist after merge, add it with default
    pred_with_antigen_familes$antigen_family <- default_family
  }

  return(pred_with_antigen_familes)
}




