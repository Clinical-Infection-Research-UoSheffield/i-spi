# curve_lookup_functions.R
# Generic curve_lookup upsert functions for all assay types.
# Called at the end of every upload workflow to register new standard curves.


#' Build a curve_lookup candidate data frame from standard data
#'
#' Extracts the natural key columns from a standards data frame that has
#' already been prepared for upload (i.e., column names are already in the
#' database schema format: study_accession, experiment_accession, etc.).
#'
#' Handles missing columns gracefully by substituting the '__none__' sentinel
#' value that the curve_lookup table uses as a default.
#'
#' @param standards_df Data frame of standard rows ready for DB insert.
#'   Must contain at minimum: study_accession, experiment_accession, antigen.
#' @param project_id  Integer project/workspace ID.
#'
#' @return Data frame with exactly the columns required for curve_lookup insert,
#'   deduplicated on the natural key.
#'
build_curve_lookup_candidates <- function(standards_df, project_id) {

  if (is.null(standards_df) || nrow(standards_df) == 0) {
    cat("  [curve_lookup] No standards data — returning empty candidate frame\n")
    return(empty_curve_lookup_df())
  }

  cat("\n  [curve_lookup] Building candidates from",
      nrow(standards_df), "standard rows...\n")

  # Helper: pull a column or fill with sentinel value.
  # Forces character type immediately to prevent combine errors downstream.
  pull_as_char <- function(df, col, sentinel = "__none__") {
    if (col %in% names(df)) {
      val <- df[[col]]
      # Convert factor → character first, then NA → sentinel
      val <- as.character(val)
      val[is.na(val) | trimws(val) == ""] <- sentinel
      val
    } else {
      rep(sentinel, nrow(df))
    }
  }

  candidates <- data.frame(
    # project_id is the one numeric column — coerce explicitly to integer
    project_id              = as.integer(project_id),

    # All remaining columns are character in the DB schema
    study_accession         = pull_as_char(standards_df, "study_accession"),
    experiment_accession    = pull_as_char(standards_df, "experiment_accession"),
    plateid                 = pull_as_char(standards_df, "plateid"),
    plate                   = pull_as_char(standards_df, "plate"),
    nominal_sample_dilution = pull_as_char(standards_df, "nominal_sample_dilution"),
    source                  = pull_as_char(standards_df, "source"),
    # ELISA carries wavelength natively; bead array will get "__none__"
    wavelength              = pull_as_char(standards_df, "wavelength"),
    antigen                 = pull_as_char(standards_df, "antigen"),
    feature                 = pull_as_char(standards_df, "feature"),

    stringsAsFactors = FALSE
  )

  # ── Deduplicate on the full natural key ───────────────────────────────────
  nk_cols    <- c("project_id", "study_accession", "experiment_accession",
                  "plateid", "plate", "nominal_sample_dilution",
                  "source", "wavelength", "antigen", "feature")
  candidates <- candidates[
    !duplicated(candidates[, nk_cols, drop = FALSE]), ,
    drop = FALSE
  ]
  rownames(candidates) <- NULL

  # ── Drop rows missing the three required non-defaulted DB columns ─────────
  candidates <- candidates[
    !is.na(candidates$study_accession)      &
      candidates$study_accession      != "" &
      !is.na(candidates$experiment_accession) &
      candidates$experiment_accession != "" &
      !is.na(candidates$antigen)            &
      candidates$antigen              != "" &
      candidates$antigen              != "__none__",
    ,
    drop = FALSE
  ]

  cat("  [curve_lookup]", nrow(candidates),
      "unique candidate combinations after dedup\n")

  candidates
}

#' Return an empty data frame with the curve_lookup candidate schema.
#' All types must match what build_curve_lookup_candidates() produces
#' so dplyr::anti_join() never hits a type mismatch.
#' Useful as a safe return value when there is nothing to insert.
#'
#' @return Zero-row data frame with correct columns and types.
#'

empty_curve_lookup_df <- function() {
  data.frame(
    project_id              = integer(0),
    study_accession         = character(0),
    experiment_accession    = character(0),
    plateid                 = character(0),
    plate                   = character(0),
    nominal_sample_dilution = character(0),
    source                  = character(0),
    wavelength              = character(0),
    antigen                 = character(0),
    feature                 = character(0),
    stringsAsFactors        = FALSE
  )
}


#' Upsert curve_lookup rows — insert new natural keys, skip existing ones
#'
#' Uses a server-side INSERT … ON CONFLICT DO NOTHING so the operation is
#' fully idempotent and safe to re-run at any time.
#'
#' The function works for ALL assay types (bead array, ELISA, flow cytometry,
#' etc.) because it operates on the already-normalised candidate frame produced
#' by \code{build_curve_lookup_candidates()}.
#'
#' @param conn      Active DBI/RPostgres connection.
#' @param candidates Data frame from \code{build_curve_lookup_candidates()}.
#'
#' @return Named list:
#'   \describe{
#'     \item{success}{Logical — TRUE even if 0 rows were inserted (not an error).}
#'     \item{rows_inserted}{Integer count of actually-new rows committed.}
#'     \item{rows_skipped}{Integer count of rows that already existed.}
#'     \item{message}{Human-readable summary string.}
#'   }
#'
upsert_curve_lookup <- function(conn, candidates) {

  result <- list(
    success       = TRUE,
    rows_inserted = 0L,
    rows_skipped  = 0L,
    message       = ""
  )

  if (is.null(candidates) || nrow(candidates) == 0) {
    result$message <- "curve_lookup: no candidates to insert"
    cat(" ", result$message, "\n")
    return(result)
  }

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  UPSERTING curve_lookup                                  ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("  Candidates:", nrow(candidates), "\n")

  nk_cols <- c("project_id", "study_accession", "experiment_accession",
               "plateid", "plate", "nominal_sample_dilution",
               "source", "wavelength", "antigen", "feature")

  # ── Step 1: find which candidates already exist ──────────────────────────
  existing <- tryCatch(
    fetch_existing_curve_lookup(conn, candidates),
    error = function(e) {
      cat("  [curve_lookup] WARNING: could not fetch existing rows:",
          e$message, "\n")
      cat("  [curve_lookup] Proceeding with full insert",
          "(ON CONFLICT will guard).\n")
      empty_curve_lookup_df()
    }
  )

  to_insert           <- dplyr::anti_join(candidates, existing, by = nk_cols)
  result$rows_skipped <- nrow(candidates) - nrow(to_insert)

  cat("  Already exist:", result$rows_skipped, "\n")
  cat("  To insert:    ", nrow(to_insert), "\n")

  if (nrow(to_insert) == 0) {
    result$message <- paste0(
      "curve_lookup: all ", nrow(candidates),
      " combinations already exist — nothing inserted"
    )
    cat(" ", result$message, "\n")
    return(result)
  }

  # ── Step 2: insert via VALUES rows — no temp table, no schema issue ───────
  # Build a parameterised INSERT … VALUES with ON CONFLICT DO NOTHING.
  # This avoids dbWriteTable(temporary=TRUE) which PostgreSQL rejects when
  # the target is schema-qualified (pg_temp cannot carry a schema prefix).
  tryCatch({

    # Ensure correct types before building the statement
    to_insert$project_id <- as.integer(to_insert$project_id)

    # Replace any remaining NAs with sentinel values
    char_cols <- c("plateid", "plate", "nominal_sample_dilution",
                   "source", "wavelength", "antigen", "feature")
    for (col in char_cols) {
      to_insert[[col]][is.na(to_insert[[col]])] <- "__none__"
    }

    # Build individual value tuples
    # Using DBI::dbQuoteLiteral() keeps the function safe against
    # injection and handles special characters in plate IDs.
    value_rows <- vapply(seq_len(nrow(to_insert)), function(i) {
      r <- to_insert[i, ]
      sprintf(
        "(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
        DBI::dbQuoteLiteral(conn, r$project_id),
        DBI::dbQuoteLiteral(conn, r$study_accession),
        DBI::dbQuoteLiteral(conn, r$experiment_accession),
        DBI::dbQuoteLiteral(conn, r$plateid),
        DBI::dbQuoteLiteral(conn, r$plate),
        DBI::dbQuoteLiteral(conn, r$nominal_sample_dilution),
        DBI::dbQuoteLiteral(conn, r$source),
        DBI::dbQuoteLiteral(conn, r$wavelength),
        DBI::dbQuoteLiteral(conn, r$antigen),
        DBI::dbQuoteLiteral(conn, r$feature)
      )
    }, character(1))

    # Split into batches of 500 rows to avoid overly large SQL statements
    batch_size    <- 500L
    n_rows        <- length(value_rows)
    batch_starts  <- seq(1, n_rows, by = batch_size)
    total_inserted <- 0L

    for (start in batch_starts) {
      end          <- min(start + batch_size - 1L, n_rows)
      batch_values <- value_rows[start:end]

      sql <- paste0(
        "INSERT INTO madi_results.curve_lookup (\n",
        "  project_id, study_accession, experiment_accession,\n",
        "  plateid, plate, nominal_sample_dilution,\n",
        "  source, wavelength, antigen, feature\n",
        ")\nVALUES\n",
        paste(batch_values, collapse = ",\n"),
        "\nON CONFLICT ON CONSTRAINT curve_lookup_nk DO NOTHING;"
      )

      rows_affected  <- DBI::dbExecute(conn, sql)
      total_inserted <- total_inserted + rows_affected

      cat("    → Batch", which(batch_starts == start), "of",
          length(batch_starts), ": inserted", rows_affected, "rows\n")
    }

    result$rows_inserted <- total_inserted
    result$message <- paste0(
      "curve_lookup: inserted ", result$rows_inserted,
      " new row(s), skipped ", result$rows_skipped, " existing"
    )
    cat("  ✓", result$message, "\n")

  }, error = function(e) {
    result$success  <<- FALSE
    result$message  <<- paste0("curve_lookup insert failed: ", e$message)
    cat("  ✗", result$message, "\n")
  })

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  result
}


#' Fetch curve_lookup rows that match any of the candidate natural keys
#'
#' Uses a single IN-style query scoped to the project / study / experiment
#' to avoid a full table scan.
#'
#' @param conn       Active DBI connection.
#' @param candidates Data frame from \code{build_curve_lookup_candidates()}.
#'
#' @return Data frame of matching rows (natural key columns only).
#'
fetch_existing_curve_lookup <- function(conn, candidates) {

  # Scope to the unique (project, study, experiment) combinations present
  # in the candidate set — avoids an unbounded query.
  scope <- unique(candidates[,
                             c("project_id", "study_accession", "experiment_accession"),
                             drop = FALSE
  ])

  # Build a VALUES literal for a small anti-join
  # For large candidate sets this is much faster than row-by-row checks.
  if (nrow(scope) == 0) return(empty_curve_lookup_df())

  # Pull all existing rows for the relevant (project, study, experiment) tuples
  rows <- lapply(seq_len(nrow(scope)), function(i) {
    q <- glue::glue_sql(
      "SELECT project_id, study_accession, experiment_accession,
              plateid, plate, nominal_sample_dilution,
              source, wavelength, antigen, feature
       FROM   madi_results.curve_lookup
       WHERE  project_id           = {scope$project_id[i]}
         AND  study_accession      = {scope$study_accession[i]}
         AND  experiment_accession = {scope$experiment_accession[i]}",
      .con = conn
    )
    DBI::dbGetQuery(conn, q)
  })

  existing <- do.call(rbind, rows)

  if (is.null(existing) || nrow(existing) == 0) {
    return(empty_curve_lookup_df())
  }

  existing
}


#' High-level helper: build candidates and upsert in one call
#'
#' This is the function that upload observers should call.  It accepts the
#' standards data frame (already in DB-schema column names) and handles
#' everything else.
#'
#' @param conn         Active DBI connection.
#' @param standards_df Standards data frame (post column-mapping).
#' @param project_id   Integer project/workspace ID.
#'
#' @return Result list from \code{upsert_curve_lookup()}.
#'
register_curve_lookup <- function(conn, standards_df, project_id) {
  candidates <- build_curve_lookup_candidates(standards_df, project_id)
  upsert_curve_lookup(conn, candidates)
}
