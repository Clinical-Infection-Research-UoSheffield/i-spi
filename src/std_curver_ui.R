# ============================================================================
# Bayesian Ensemble reactive state
# Holds the full lifecycle of the background stanassay run:
#   status        – "idle" | "running" | "ready" | "error"
#   plots         – named list of plotly objects keyed by plateid
#   cdan_profiles – named list of CDAN result lists keyed by plateid
#   assay         – the fitted StanAssay R6 object
#   error_msg     – character error message (when status == "error")
#   trigger_key   – a string that identifies the last run's inputs so we can
#                   skip re-running when the user only switches plates to view
# ============================================================================
bayes_state <- reactiveValues(
  status            = "idle",
  plots             = list(),
  cdan_profiles     = list(),
  lod_profiles      = list(),
  infl_profiles     = list(),
  d2_profiles       = list(),
  lrdl_profiles     = list(),
  uod_profiles      = list(),
  urdl_profiles     = list(),
  asymmetry         = NULL,
  assay             = NULL,
  error_msg         = NULL,
  trigger_key       = NULL,
  best_family       = NULL,   # global best (highest stacking weight)
  stacking_weights  = NULL,
  plate_best_family = NULL,   # named char vec: plate_id → best family
  plate_elpd        = NULL,   # matrix: plates × families
  loo_comparison    = NULL,   # loo::loo_compare() matrix
  pareto_k_summary  = NULL,   # data.frame: family, n_good, n_ok, n_bad, n_vbad, max_k
  sample_results    = NULL,   # data.frame from predict_samples (with gate_class, conc columns)
  ensemble_data     = NULL,   # data.frame from bayes_ensemble (DB mode: param CIs per family)
  data_source       = NULL    # "database" or "live_fit" — tracks where results came from
)

# ============================================================================
# Batch API helpers — submit and poll Bayesian jobs
# ============================================================================

submit_bayes_job <- function(project_id, study, experiment = NULL,
                              antigen = NULL, scope = "study",
                              source = NULL) {
  api_url <- Sys.getenv("BATCH_API_URL", "")
  api_key <- Sys.getenv("BATCH_API_KEY", "")

  if (!nzchar(api_url) || !nzchar(api_key)) {
    stop("BATCH_API_URL or BATCH_API_KEY not configured in .Renviron")
  }

  body <- list(
    project_id  = as.integer(project_id),
    study       = study,
    scope       = scope,
    script_type = "bayesian"
  )
  if (!is.null(experiment) && nzchar(experiment)) body$experiment <- experiment
  if (!is.null(antigen) && nzchar(antigen))       body$antigen    <- antigen
  if (!is.null(source) && nzchar(source))          body$source     <- source

  resp <- httr2::request(paste0(api_url, "jobs")) |>
    httr2::req_headers(`X-API-Key` = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop(sprintf("Batch API error (%d): %s", httr2::resp_status(resp), err_body))
  }

  httr2::resp_body_json(resp)
}


poll_bayes_job <- function(job_id) {
  api_url <- Sys.getenv("BATCH_API_URL", "")
  api_key <- Sys.getenv("BATCH_API_KEY", "")

  if (!nzchar(api_url) || !nzchar(api_key)) return(NULL)

  resp <- tryCatch({
    httr2::request(paste0(api_url, "jobs/", job_id)) |>
      httr2::req_headers(`X-API-Key` = api_key) |>
      httr2::req_timeout(10) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) { message("[poll_bayes] HTTP error: ", e$message); NULL })

  if (is.null(resp) || httr2::resp_status(resp) >= 400) return(NULL)
  httr2::resp_body_json(resp)
}


save_bayes_job_audit <- function(conn, project_id, study, experiment, antigen,
                                  scope, job_id) {
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO madi_results.bayes_job_audit
       (project_id, study_accession, experiment_accession, antigen, scope, job_id, status)
     VALUES (%s, '%s', %s, %s, '%s', '%s', 'queued')
     ON CONFLICT (job_id) DO NOTHING",
    project_id, study,
    if (is.null(experiment) || !nzchar(experiment)) "NULL" else paste0("'", experiment, "'"),
    if (is.null(antigen) || !nzchar(antigen)) "NULL" else paste0("'", antigen, "'"),
    scope, job_id
  ))
}


update_bayes_job_audit <- function(conn, job_id, api_status) {
  if (is.null(api_status)) return()
  status     <- api_status$status %||% "unknown"
  progress   <- api_status$progress %||% ""
  pct        <- api_status$percentage %||% 0
  eta        <- api_status$eta_display %||% ""
  err        <- api_status$error %||% ""
  completed  <- if (status %in% c("completed", "failed", "error")) "now()" else "NULL"

  DBI::dbExecute(conn, sprintf(
    "UPDATE madi_results.bayes_job_audit
     SET status = '%s', progress = '%s', percentage = %s,
         eta_display = '%s', error = '%s',
         completed_at = %s, updated_at = now()
     WHERE job_id = '%s'",
    status, progress, pct, eta, err, completed, job_id
  ))
}


get_latest_bayes_job <- function(conn, project_id, study, experiment = NULL,
                                  antigen = NULL, scope = "study") {
  where_parts <- sprintf(
    "project_id = %s AND study_accession = '%s' AND scope = '%s'",
    project_id, study, scope
  )
  if (!is.null(experiment) && nzchar(experiment)) {
    where_parts <- paste0(where_parts, sprintf(" AND experiment_accession = '%s'", experiment))
  }
  if (!is.null(antigen) && nzchar(antigen)) {
    where_parts <- paste0(where_parts, sprintf(" AND antigen = '%s'", antigen))
  }

  df <- tryCatch(
    DBI::dbGetQuery(conn, sprintf(
      "SELECT * FROM madi_results.bayes_job_audit WHERE %s ORDER BY created_at DESC LIMIT 1",
      where_parts)),
    error = function(e) data.frame()
  )

  if (nrow(df) == 0) return(NULL)
  as.list(df[1, ])
}


get_bayes_calc_status <- function(conn, project_id, study, experiment = NULL,
                                   antigen = NULL, scope = "study") {
  # Only use the audit table — no fallback to bayes_curves.
  # The audit table tracks jobs submitted from i-spi.
  # Pre-computed results from external batch runs are visible in
  # the Bayesian curve view (DB-first auto-fetch) but don't affect
  # the status grid here.

  job <- get_latest_bayes_job(conn, project_id, study, experiment, antigen, scope)

  if (is.null(job)) return(list(status = "not begun"))

  if (job$status %in% c("queued", "running")) {
    return(list(
      status      = "pending",
      progress    = job$progress %||% "",
      percentage  = job$percentage %||% 0,
      eta_display = job$eta_display %||% "",
      job_id      = job$job_id,
      timestamp   = job$created_at
    ))
  }
  if (job$status == "completed") {
    return(list(
      status    = "completed",
      timestamp = job$completed_at %||% job$updated_at,
      job_id    = job$job_id
    ))
  }
  if (job$status %in% c("failed", "error")) {
    return(list(
      status    = "failed",
      error     = job$error %||% "Unknown error",
      timestamp = job$completed_at %||% job$updated_at,
      job_id    = job$job_id
    ))
  }

  list(status = "not begun")
}


# ============================================================================
# DB-First Bayesian — fetch pre-computed results from madi_results.bayes_*
# ============================================================================

# Build a plotly object from DB-stored grids (mirrors plot_from_db.R)
build_bayes_plot_from_db <- function(curve_row, curve_grid, cdan_grid,
                                     standards_df = NULL, samples_df = NULL) {
  COL_STD  <- "#000000"; COL_FIT  <- "#0072B2"; COL_CI   <- "rgba(86,180,233,0.20)"
  COL_SAMP <- "#E69F00"; COL_LOQ  <- "#D55E00"; COL_RDL  <- "#604e97"
  COL_INFL <- "#009E73"; COL_CDAN <- "#1565C0"; COL_ASYM <- "#999999"

  fam <- switch(as.character(curve_row$curve_family),
                "4pl" = "4PL", "5pl" = "5PL", "gompertz" = "Gompertz",
                curve_row$curve_family)

  fit_df <- data.frame(
    x       = curve_grid$log10_conc,
    y       = log10(pmax(curve_grid$mfi_median, 1e-9)),
    y_lower = log10(pmax(curve_grid$mfi_lower_95, 1e-9)),
    y_upper = log10(pmax(curve_grid$mfi_upper_95, 1e-9))
  )

  y_all <- c(fit_df$y_lower, fit_df$y_upper)
  if (!is.null(standards_df) && nrow(standards_df) > 0) {
    y_all <- c(y_all, log10(pmax(standards_df$mfi, 1)))
  }
  y_lo <- min(y_all, na.rm = TRUE); y_hi <- max(y_all, na.rm = TRUE)

  p <- plotly::plot_ly()

  # Standards
  if (!is.null(standards_df) && nrow(standards_df) > 0) {
    p <- p |> plotly::add_markers(
      data = standards_df, x = ~log10(concentration), y = ~log10(mfi),
      name = "Standards", marker = list(color = COL_STD, size = 7),
      hovertemplate = "Conc: %{customdata:.4f}<br>MFI: %{y:.3f}<extra></extra>",
      customdata = standards_df$concentration)
  }

  # CI ribbon + fitted curve
  p <- p |>
    plotly::add_ribbons(data = fit_df, x = ~x, ymin = ~y_lower, ymax = ~y_upper,
      name = "95% CI", fillcolor = COL_CI, line = list(color = "transparent")) |>
    plotly::add_lines(data = fit_df, x = ~x, y = ~y,
      name = paste0(fam, " Fit"), line = list(color = COL_FIT, width = 2.5))

  # Samples
  if (!is.null(samples_df) && nrow(samples_df) > 0) {
    vs <- samples_df[!is.na(samples_df$raw_predicted_concentration) &
                     samples_df$raw_predicted_concentration > 0 &
                     !is.na(samples_df$mfi) & samples_df$mfi > 0, , drop = FALSE]
    if (nrow(vs) > 0) {
      vs$hover <- paste0(
        "<b>ID:</b> ", vs$sampleid,
        "<br><b>MFI:</b> ", round(vs$mfi, 1),
        "<br><b>Conc:</b> ", signif(vs$raw_predicted_concentration, 4),
        "<br><b>95%CI:</b> [", signif(vs$conc_lower, 3), ", ", signif(vs$conc_upper, 3), "]",
        "<br><b>pCoV:</b> ", ifelse(!is.na(vs$pcov), paste0(round(vs$pcov, 1), "%"), "N/A"),
        "<br><b>Gate:</b> ", vs$gate_class)
      p <- p |> plotly::add_markers(
        data = vs, x = ~log10(raw_predicted_concentration), y = ~log10(mfi),
        name = "Samples", text = ~hover, hoverinfo = "text",
        marker = list(color = COL_SAMP, size = 7, symbol = "diamond-open",
                      line = list(width = 2, color = COL_SAMP)))
    }
  }

  # Vertical limit lines
  add_vline <- function(p, val, nm, lg, col, dash, show) {
    if (!is.na(val) && is.finite(val) && val > 0)
      p |> plotly::add_segments(x = log10(val), xend = log10(val), y = y_lo, yend = y_hi,
        name = nm, legendgroup = lg, line = list(color = col, dash = dash, width = 2),
        showlegend = show, hoverinfo = "skip")
    else p
  }
  p <- p |>
    add_vline(curve_row$lloq %||% NA, "LLOQ / ULOQ", "loq", COL_LOQ, "dash", TRUE) |>
    add_vline(curve_row$uloq %||% NA, "LLOQ / ULOQ", "loq", COL_LOQ, "dash", FALSE) |>
    add_vline(curve_row$lrdl %||% NA, "LRDL / URDL", "rdl", COL_RDL, "dashdot", TRUE) |>
    add_vline(curve_row$urdl %||% NA, "LRDL / URDL", "rdl", COL_RDL, "dashdot", FALSE)

  # Inflection point
  ix <- curve_row$inflect_x
  if (!is.na(ix) && ix > 0) {
    iy <- tryCatch(approx(fit_df$x, fit_df$y, xout = log10(ix))$y, error = function(e) NA)
    if (!is.na(iy)) {
      p <- p |> plotly::add_markers(x = log10(ix), y = iy,
        name = "Inflection (max |dy/dx|)",
        marker = list(color = COL_INFL, size = 10, symbol = "diamond",
                      line = list(width = 1.5, color = "#000000")))
    }
  }

  # CDAN precision profile (secondary Y axis)
  if (!is.null(cdan_grid) && nrow(cdan_grid) > 0) {
    cg <- cdan_grid[!is.na(cdan_grid$smoothed_cv) & cdan_grid$smoothed_cv < 60, , drop = FALSE]
    if (nrow(cg) > 0) {
      p <- p |>
        plotly::add_lines(data = cg, x = ~log10_conc, y = ~smoothed_cv,
          name = "Bayesian CDAN Precision Profile", yaxis = "y2",
          line = list(color = COL_CDAN, width = 2.5),
          hovertemplate = "Log10 Conc: %{x:.2f}<br>CV%%: %{y:.1f}%%<extra>CDAN</extra>") |>
        plotly::add_lines(x = range(cg$log10_conc), y = c(20, 20),
          name = "pCoV Threshold: 20%", yaxis = "y2",
          line = list(color = "#e68fac", dash = "dash", width = 1.5), hoverinfo = "skip") |>
        plotly::add_lines(x = range(cg$log10_conc), y = c(15, 15),
          name = "pCoV Threshold: 15%", yaxis = "y2",
          line = list(color = "#4CAF50", dash = "dash", width = 1.5), hoverinfo = "skip")
    }
  }

  # Layout
  layout_args <- list(
    title = list(text = paste0(fam, " Fit \u2014 ", curve_row$plateid), font = list(size = 14)),
    xaxis = list(title = "Log\u2081\u2080 Concentration",
      gridcolor = "#E5E5E5", showline = TRUE, linecolor = "#CCCCCC"),
    yaxis = list(title = "Log\u2081\u2080 MFI",
      gridcolor = "#E5E5E5", showline = TRUE, linecolor = "#CCCCCC"),
    plot_bgcolor = "white", paper_bgcolor = "white", hovermode = "closest",
    legend = list(x = 1.05, y = 1, xanchor = "left",
      bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#CCCCCC", borderwidth = 1))
  if (!is.null(cdan_grid) && nrow(cdan_grid) > 0) {
    layout_args$yaxis2 <- list(
      overlaying = "y", side = "right",
      title = "Concentration Uncertainty (pCoV %)",
      range = c(0, 55), showgrid = FALSE, zeroline = FALSE,
      tickfont = list(color = COL_CDAN), titlefont = list(color = COL_CDAN))
    layout_args$margin <- list(r = 140)
  }
  do.call(plotly::layout, c(list(p), layout_args))
}


# Fetch all Bayesian results from DB for a given antigen+source combination.
# Returns NULL if no results exist. Otherwise returns a list that can be
# assigned directly into bayes_state fields.
fetch_bayes_from_db <- function(conn, project_id, study, experiment,
                                antigen, source_filter) {
  # 1. Master curves table
  curves <- tryCatch(
    DBI::dbGetQuery(conn, sprintf(
      "SELECT * FROM madi_results.bayes_curves
       WHERE project_id = %s AND study_accession = '%s'
         AND experiment_accession = '%s' AND antigen = '%s' AND source = '%s'",
      project_id, study, experiment, antigen, source_filter)),
    error = function(e) { message("[fetch_bayes_db] curves query error: ", e$message); NULL })

  if (is.null(curves) || nrow(curves) == 0) return(NULL)

  plate_ids <- unique(curves$plateid)
  message(sprintf("[fetch_bayes_db] Found %d plates for %s/%s", length(plate_ids), antigen, source_filter))

  # 2. Get concentration base_num from the first curve row
  nom <- as.numeric(curves$nominal_sample_dilution[1])
  sc_conc <- tryCatch(
    DBI::dbGetQuery(conn, sprintf(
      "SELECT DISTINCT standard_curve_concentration FROM madi_results.xmap_antigen_family
       WHERE study_accession = '%s' AND experiment_accession = '%s' AND antigen = '%s'
         AND standard_curve_concentration IS NOT NULL LIMIT 1",
      study, experiment, antigen)),
    error = function(e) NULL)
  if (!is.null(sc_conc) && nrow(sc_conc) > 0) {
    nom <- as.numeric(sc_conc$standard_curve_concentration[1])
  }

  # 3. Fetch grids, samples, ensemble, pareto_k
  curve_ids <- paste(curves$bayes_curves_id, collapse = ",")

  curve_grids <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT cg.*, bc.plateid, bc.source FROM madi_results.bayes_curve_grid cg
     JOIN madi_results.bayes_curves bc ON cg.bayes_curves_id = bc.bayes_curves_id
     WHERE cg.bayes_curves_id IN (%s) ORDER BY bc.plateid, cg.log10_conc", curve_ids)),
    error = function(e) { message("[fetch_bayes_db] curve_grid error: ", e$message); data.frame() })

  cdan_grids <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT cg.*, bc.plateid, bc.source FROM madi_results.bayes_cdan_grid cg
     JOIN madi_results.bayes_curves bc ON cg.bayes_curves_id = bc.bayes_curves_id
     WHERE cg.bayes_curves_id IN (%s) ORDER BY bc.plateid, cg.log10_conc", curve_ids)),
    error = function(e) { message("[fetch_bayes_db] cdan_grid error: ", e$message); data.frame() })

  samples_all <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT * FROM madi_results.bayes_samples
     WHERE bayes_curves_id IN (%s)", curve_ids)),
    error = function(e) { message("[fetch_bayes_db] samples error: ", e$message); data.frame() })

  ensemble <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT * FROM madi_results.bayes_ensemble
     WHERE project_id = %s AND study_accession = '%s'
       AND experiment_accession = '%s' AND antigen = '%s'",
    project_id, study, experiment, antigen)),
    error = function(e) { message("[fetch_bayes_db] ensemble error: ", e$message); data.frame() })

  pareto_k <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT * FROM madi_results.bayes_pareto_k
     WHERE project_id = %s AND study_accession = '%s'
       AND experiment_accession = '%s' AND antigen = '%s'",
    project_id, study, experiment, antigen)),
    error = function(e) { message("[fetch_bayes_db] pareto_k error: ", e$message); data.frame() })

  # 4. Fetch standards for plot (median-aggregate + prozone correct per plate)
  stds_all <- tryCatch(DBI::dbGetQuery(conn, sprintf(
    "SELECT plateid, dilution as dilution_factor, antibody_mfi as mfi
     FROM madi_results.xmap_standard
     WHERE study_accession = '%s' AND experiment_accession = '%s'
       AND antigen = '%s' AND source = '%s'
       AND antibody_mfi > 0 AND dilution > 0",
    study, experiment, antigen, source_filter)),
    error = function(e) { message("[fetch_bayes_db] standards error: ", e$message); data.frame() })
  if (nrow(stds_all) > 0) stds_all$concentration <- nom / stds_all$dilution_factor

  # Prozone correction helper (inlined from worker_batch.R)
  correct_prozone <- function(df, prop_diff = 0.1, dil_scale = 2) {
    if (nrow(df) == 0) return(df)
    max_mfi <- max(df$mfi, na.rm = TRUE)
    conc_at_max <- max(df$concentration[df$mfi == max_mfi])
    post_peak <- df$concentration > conc_at_max
    if (any(post_peak)) {
      df$mfi[post_peak] <- max_mfi +
        (max_mfi - df$mfi[post_peak]) * prop_diff /
        ((df$concentration[post_peak] - conc_at_max) * dil_scale)
    }
    df
  }

  # 5. Build per-plate structures
  plots          <- list()
  cdan_profiles  <- list()
  lod_profiles   <- list()
  lrdl_profiles  <- list()
  uod_profiles   <- list()
  urdl_profiles  <- list()
  infl_profiles  <- list()
  d2_profiles    <- list()

  for (i in seq_len(nrow(curves))) {
    row <- curves[i, ]
    pid <- row$plateid

    # Curve grid for this plate
    cg <- if (nrow(curve_grids) > 0) {
      curve_grids[curve_grids$plateid == pid, , drop = FALSE]
    } else { data.frame() }

    # CDAN grid for this plate
    cdg <- if (nrow(cdan_grids) > 0) {
      cdan_grids[cdan_grids$plateid == pid, , drop = FALSE]
    } else { data.frame() }

    # Standards for this plate (median-aggregated + prozone)
    stds_plate <- if (nrow(stds_all) > 0) {
      sp <- stds_all[stds_all$plateid == pid, , drop = FALSE]
      if (nrow(sp) > 0) {
        sp <- sp |>
          dplyr::group_by(concentration) |>
          dplyr::summarise(mfi = median(mfi, na.rm = TRUE), .groups = "drop") |>
          dplyr::filter(!is.na(mfi))
        correct_prozone(as.data.frame(sp))
      } else { data.frame() }
    } else { data.frame() }

    # Samples for this plate
    samp_plate <- if (nrow(samples_all) > 0) {
      samples_all[samples_all$plateid == pid, , drop = FALSE]
    } else { data.frame() }

    # Build plotly
    if (nrow(cg) > 0) {
      plots[[pid]] <- tryCatch(
        build_bayes_plot_from_db(row, cg, cdg, stds_plate, samp_plate),
        error = function(e) {
          message(sprintf("[fetch_bayes_db] plot build failed for %s: %s", pid, e$message))
          NULL
        })
    }

    # CDAN profile (mimic stanassay cdan_profile structure)
    cdan_profiles[[pid]] <- list(
      lloq_20  = row$lloq, uloq_20  = row$uloq,
      lloq_15  = row$lloq_15, uloq_15  = row$uloq_15,
      n_draws  = NA_integer_, n_grid = NA_integer_,
      method   = row$cdan_method %||% NA_character_,
      profile  = if (nrow(cdg) > 0) {
        data.frame(log10_conc = cdg$log10_conc, concentration = 10^cdg$log10_conc,
                   smoothed_cv = cdg$smoothed_cv, stringsAsFactors = FALSE)
      } else { data.frame(log10_conc = numeric(0), concentration = numeric(0), smoothed_cv = numeric(0)) }
    )

    lod_profiles[[pid]]  <- list(lod = row$lod, lod_log10 = if (!is.na(row$lod)) log10(row$lod) else NA,
                                  threshold_mfi_median = row$lod_y)
    lrdl_profiles[[pid]] <- list(lrdl = row$lrdl, lrdl_log10 = if (!is.na(row$lrdl)) log10(row$lrdl) else NA,
                                  method = "posterior_predictive_lrdl")
    uod_profiles[[pid]]  <- list(uod = row$uod, uod_log10 = if (!is.na(row$uod)) log10(row$uod) else NA)
    urdl_profiles[[pid]] <- list(urdl = row$urdl, urdl_log10 = if (!is.na(row$urdl)) log10(row$urdl) else NA,
                                  method = "posterior_predictive_urdl")
    infl_profiles[[pid]] <- list(x_median = row$inflect_x,
                                  x_lower = row$inflect_x_lower, x_upper = row$inflect_x_upper,
                                  family = row$curve_family)
    d2_profiles[[pid]]   <- list(lo2d_median = row$lo2d, uo2d_median = row$uo2d)
  }

  # 6. Global stacking weights (from first curve row — same for all plates)
  r1 <- curves[1, ]
  stacking_weights <- c(
    "4pl"      = as.numeric(r1$global_stacking_4pl),
    "5pl"      = as.numeric(r1$global_stacking_5pl),
    "gompertz" = as.numeric(r1$global_stacking_gompertz)
  )

  # Per-plate best family + ELPD matrix
  plate_best_family <- setNames(as.character(curves$plate_best_family), curves$plateid)
  plate_elpd <- matrix(NA_real_, nrow = length(plate_ids), ncol = 3,
    dimnames = list(plate_ids, c("4pl", "5pl", "gompertz")))
  for (i in seq_len(nrow(curves))) {
    pid <- curves$plateid[i]
    plate_elpd[pid, "4pl"]      <- curves$plate_elpd_4pl[i]
    plate_elpd[pid, "5pl"]      <- curves$plate_elpd_5pl[i]
    plate_elpd[pid, "gompertz"] <- curves$plate_elpd_gompertz[i]
  }

  # 7. Pareto k summary
  pk_summary <- NULL
  if (!is.null(pareto_k) && nrow(pareto_k) > 0) {
    pk_summary <- data.frame(
      family = pareto_k$family,
      n_good = pareto_k$n_good,
      n_ok   = pareto_k$n_ok,
      n_bad  = pareto_k$n_bad,
      n_vbad = pareto_k$n_vbad,
      max_k  = pareto_k$max_k,
      stringsAsFactors = FALSE
    )
  }

  # 8. LOO comparison from ensemble table (build a matrix similar to loo::loo_compare)
  loo_comp <- NULL
  if (!is.null(ensemble) && nrow(ensemble) > 0) {
    # Aggregate ELPD across plates per family
    fam_elpd <- ensemble |>
      dplyr::group_by(family) |>
      dplyr::summarise(
        elpd_loo = sum(plate_elpd, na.rm = TRUE),
        .groups = "drop") |>
      dplyr::arrange(dplyr::desc(elpd_loo))

    best_elpd <- fam_elpd$elpd_loo[1]
    loo_comp <- data.frame(
      elpd_diff = fam_elpd$elpd_loo - best_elpd,
      elpd_loo  = fam_elpd$elpd_loo,
      stringsAsFactors = FALSE
    )
    rownames(loo_comp) <- fam_elpd$family
  }

  # 9. Asymmetry (from bayes_curves new columns)
  asymmetry <- NULL
  if (!is.na(r1$prob_4pl)) {
    asymmetry <- list(
      prob_4pl     = r1$prob_4pl,
      g_prior_mode = r1$g_prior_mode %||% NA_character_,
      g_plate      = data.frame(
        plateid = curves$plateid,
        median  = curves$g_median,
        `2.5%`  = curves$g_q2p5,
        `97.5%` = curves$g_q97p5,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    )
  }

  # 10. Sample results (for download handlers)
  sample_results <- if (nrow(samples_all) > 0) samples_all else NULL

  # 11. Ensemble data (for param downloads + model comparison modal)
  ensemble_data <- if (nrow(ensemble) > 0) ensemble else NULL

  list(
    status            = "ready",
    plots             = plots,
    cdan_profiles     = cdan_profiles,
    lod_profiles      = lod_profiles,
    lrdl_profiles     = lrdl_profiles,
    uod_profiles      = uod_profiles,
    urdl_profiles     = urdl_profiles,
    infl_profiles     = infl_profiles,
    d2_profiles       = d2_profiles,
    stacking_weights  = stacking_weights,
    best_family       = as.character(r1$global_best_family),
    plate_best_family = plate_best_family,
    plate_elpd        = plate_elpd,
    loo_comparison    = loo_comp,
    pareto_k_summary  = pk_summary,
    asymmetry         = asymmetry,
    sample_results    = sample_results,
    ensemble_data     = ensemble_data,
    assay             = NULL,
    error_msg         = NULL,
    source            = "database"  # flag to distinguish DB vs live fit
  )
}


# Cache of the last-rendered frequentist plotly object + comparison toggle
freq_curve_plot_cache  <- reactiveVal(NULL)
comparison_visible     <- reactiveVal(FALSE)
sc_concentration_cache <- reactiveVal(NULL)

# ---- concentration_calc_df — session-scoped so batch observers can use it ----
concentration_calc_df <- reactive({
  req(
    input$sc_plate_select,
    input$readxMap_study_accession    != "Click here",
    input$readxMap_experiment_accession != "Click here"
  )
  concentrationUIRefresher()
  
  calc_df <- get_existing_concentration_calc(
    conn                 = conn,
    project_id           = userWorkSpaceID(),
    study_accession      = input$readxMap_study_accession,
    experiment_accession = input$readxMap_experiment_accession,
    plate_nom            = input$sc_plate_select
  )
  
  # Overlay in-memory "pending" states for MCMC
  pending <- mcmc_pending_scopes()
  if (length(pending) > 0) {
    for (entry in pending) {
      match_rows <- (
        calc_df$scope                     == entry[["scope"]] &
          calc_df$concentration_calc_method == entry[["method"]]
      )
      if (any(match_rows)) calc_df$job_status[match_rows] <- "pending"
    }
  }
  
  # Overlay in-memory "pending" states for interpolated
  interp_pending <- interp_pending_scopes()
  if (length(interp_pending) > 0) {
    for (entry in interp_pending) {
      match_rows <- (
        calc_df$scope                      == entry[["scope"]] &
          calc_df$concentration_calc_method == entry[["method"]]
      )
      if (any(match_rows)) calc_df$job_status[match_rows] <- "pending"
    }
  }
  
  # When mcmc_robust is active at a scope, show interpolated as completed
  # BUT NOT if interpolated is currently pending (running)
  mcmc_active_scopes <- unique(
    calc_df$scope[
      calc_df$concentration_calc_method == "mcmc_robust" &
        calc_df$job_status                != "not begun"
    ]
  )
  calc_df$job_status <- ifelse(
    calc_df$concentration_calc_method == "interpolated" &
      calc_df$scope %in% mcmc_active_scopes &
      calc_df$job_status != "pending",
    "completed",
    calc_df$job_status
  )
  
  calc_df
})

# ============================================================================
# Navigation Observer
# Handles UI setup when the user navigates to the Standard Curve tab.
# ============================================================================
observeEvent(
  list(
    input$readxMap_experiment_accession,
    input$readxMap_study_accession,
    input$qc_component,
    input$study_level_tabs,
    input$main_tabs
  ),
  {
    req(
      input$qc_component                  == "Standard Curve",
      input$readxMap_study_accession      != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs              == "Experiments",
      input$main_tabs                     == "view_files_tab",
      !is.null(userWorkSpaceID())
    )

    selected_study      <- input$readxMap_study_accession
    selected_experiment <- input$readxMap_experiment_accession
    
    # #  Validate experiment belongs to current study
    valid_exps <- reactive_df_study_exp()
    if (!is.null(valid_exps) && nrow(valid_exps) > 0) {
      study_exps <- valid_exps[
        valid_exps$study_accession == selected_study,
        "experiment_accession"
      ]
      if (!selected_experiment %in% study_exps) {
        cat("⚠ [std_curver_ui] Stale experiment", selected_experiment,
            "not in study", selected_study, "- skipping pull_data\n")
        return()
      }
    }

    stored_std <- stored_plates_data$stored_standard
    if (!is.null(stored_std) && nrow(stored_std) > 0) {
      if ("study_accession" %in% names(stored_std)) {
        stored_study <- unique(stored_std$study_accession)
        if (!selected_study %in% stored_study) {
          cat("⚠ [std_curver_ui] stored_standard belongs to", stored_study,
              "not current study", selected_study, "- skipping\n")
          return()
        }
      }
    }
    
    verbose                    <- FALSE
    model_names                <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
    param_group                <- "standard_curve_options"
    allowed_constraint_methods <- c(
      "default", "user_defined", "range_of_blanks", "geometric_mean_of_blanks"
    )
    
    loaded_data <- pull_data(
      study_accession      = selected_study,
      experiment_accession = selected_experiment,
      project_id           = userWorkSpaceID(),
      conn                 = conn
    )
    
    if (is.null(loaded_data)) {
      cat("⚠ [std_curver_ui] pull_data returned NULL - aborting\n")
      return()
    }
    
    #loaded_data_v <<- loaded_data

    response_var <- loaded_data$response_var
    indep_var    <- loaded_data$indep_var

    # Guard: if pull_data returned no plates (e.g. workspace ID mismatch or DB
    # issue), response_var will be character(0) / NA and downstream [[col]] calls
    # will crash.  Bail out silently so the user sees an empty UI rather than a
    # red error page.
    req(
      length(response_var) == 1L,
      !is.na(response_var),
      nzchar(response_var),
      nrow(loaded_data$standards) > 0L
    )

    se_antigen_table <- compute_antigen_se_table(
      standards_data = loaded_data$standards,
      response_col   = response_var,
      dilution_col   = "dilution",
      plate_col      = "plate",
      grouping_cols  = c("project_id", "study_accession", "experiment_accession", "source", "antigen"),
      #method         = "pooled_within",
      verbose        = TRUE
    )
    

    dil_series_se_table <- compute_dil_series_se(standards_data = loaded_data$standards, 
                                                response_col  = response_var,
                                                dilution_col  = "dilution",
                                                plate_col     = "plate_nom",
                                                grouping_cols = c("project_id",
                                                                  "study_accession",
                                                                  "experiment_accession",
                                                                  "source_nom",
                                                                  "antigen",
                                                                  "feature"),
                                                min_reps = 2,
                                                verbose  = FALSE) 
    

    study_params <- fetch_study_parameters(
      study_accession = selected_study,
      param_user      = currentuser(),
      param_group     = param_group,
      project_id      = userWorkSpaceID(),
      conn            = conn
    )
    # ------------------------------------------------------------------
    # Top-level UI shell
    # ------------------------------------------------------------------
    output$std_curver_ui <- renderUI({
      tagList(
        div(
          style = paste0(
            "background-color:#f0f8ff; border:1px solid #4a90e2;",
            "padding:10px; margin-bottom:15px; border-radius:5px;"
          ),
          tags$h4(
            "Current Standard Curve Context",
            style = "margin-top:0; color:#2c5aa0;"
          ),
          uiOutput("standard_curve_context")
        ),
        
        conditionalPanel(
          condition = "output.can_fit_standard_curve == true",
          
          # ── Row 1: Curve Method + Antigen + Source (always visible) ──
          fluidRow(
            column(
              4,
              div(
                class = "radio-card-group",
                radioButtons(
                  "sc_curve_method",
                  label = "Curve Method",
                  choiceNames = list(
                    
                    # Frequentist option
                    HTML("
          <div class='radio-card'>
            <div class='radio-title'>Frequentist regression</div>
            <div class='radio-desc'>
              Choose the Frequentist approach when you want a fast, transparent curve fit using nonlinear least squares (Levenberg–Marquardt)
              with automatic model selection via AIC. It requires no prior information from other plates, gives you direct control over asymptote constraints,
              and estimates concentrations by simple inversion of thefitted equation - making it the practical default for routine single-plate analyses 
              where computational speed and interpertability matter most.
            </div>
          </div>
        "),
                    
                    # Bayesian option
                    HTML("
          <div class='radio-card'>
            <div class='radio-title'>Bayesian regression</div>
            <div class='radio-desc'>
              Choose the Bayseian approach when you need robust estimates near the upper and lower asymptotes, where frequentist methods often struggle with instability.
              By borrowing strength from prior plate data via Hamiltonian Monte Carlo sampling, the Baysian framework naturally regularizes extreme regions of the curve and delivers concentration estimates 
              with full posterior uncertainty - making it especially valuable in multi-plate studies where predictive accuracy and honest error quantification outweigh computational cost.
            </div>
          </div>
        ")
                  ),
                  choiceValues = c("Frequentist", "Bayesian"),
                  selected = "Frequentist"
                
                )
              )
            ),
            # column(3, radioButtons(
            #   "sc_curve_method",
            #   label    = "Curve Method",
            #   choices  = c("Frequentist", "Bayesian"),
            #   selected = "Frequentist",
            #   inline   = TRUE
            # )),
            column(3, uiOutput("sc_antigen_selector")),
            column(3, uiOutput("sc_source_selector"))
          ),

          # ── Row 2: Plate selector (Frequentist only) ──
          conditionalPanel(
            condition = "input.sc_curve_method == 'Frequentist'",
            fluidRow(
              column(3, uiOutput("sc_plate_selector"))
            )
          ),

          fluidRow(
            column(3, actionButton("show_comparisions",
                                   "Show Model Comparisons")),
            column(3, downloadButton("download_best_fit_parameter_estimates",
                                     "Download Parameter Estimates for Selected Fit")),
            column(3, downloadButton("download_samples_above_ulod",
                                     "Download Samples above the Upper Limit of Detection")),
            column(3, downloadButton("download_samples_below_llod",
                                     "Download Samples below the Lower Limit of Detection"))
          ),
          
          # ----------------------------------------------------------------
          # Frequentist panel — visible when Curve Method == "Frequentist"
          # ----------------------------------------------------------------
          conditionalPanel(
            condition = "input.sc_curve_method == 'Frequentist'",

            fluidRow(
              column(3, uiOutput("is_display_log_response")),
              column(3, uiOutput("is_display_log_independent_variable"))
            ),

            shinycssloaders::withSpinner(
              plotlyOutput("standard_curve", width = "75vw", height = "800px"),
              type    = 6,
              color   = "#4a90e2",
              caption = "Fitting standard curve, please wait..."
            )
          ),

          # ----------------------------------------------------------------
          # Bayesian Ensemble Analysis Panel (stanassay)
          # Visible when Curve Method == "Bayesian"
          # ----------------------------------------------------------------
          conditionalPanel(
            condition = "input.sc_curve_method == 'Bayesian'",

            br(),
            div(
              style = paste0(
                "background-color:#f0fff4; border:1px solid #27ae60;",
                "border-radius:6px; padding:14px; margin-top:10px; margin-bottom:10px;"
              ),
              tags$h4(
                tags$span(
                  style = "color:#1a7a40;",
                  icon("flask"), " ",
                  textOutput("bayes_panel_title", inline = TRUE),
                  tags$small(
                    style = "font-size:0.75em; color:#555; margin-left:6px;",
                    "(stanassay — hierarchical Stan ensemble, all plates)"
                  )
                ),
                style = "margin-top:0; margin-bottom:10px;"
              ),

              tags$p(
                style = "color:#555; font-style:italic; margin-bottom:8px;",
                icon("info-circle"),
                " Bayesian ensemble fits all plates jointly. Use the dropdown below to view individual plate results."
              ),

              # Wavelength selector — visible for ELISA only, hidden for xMAP
              uiOutput("bayes_wavelength_ui"),

              fluidRow(
                column(
                  3,
                  selectInput(
                    "bayes_view_plate",
                    label = "View Results for Plate:",
                    choices = NULL
                  )
                ),
                column(
                  2,
                  numericInput(
                    "bayes_n_chains",
                    label = "Parallel Chains:",
                    value = 4L,
                    min   = 1L,
                    max   = 8L,
                    step  = 1L
                  )
                ),
                column(
                  2,
                  numericInput(
                    "bayes_n_iter",
                    label = "Iterations:",
                    value = 1000L,
                    min   = 500L,
                    max   = 4000L,
                    step  = 500L
                  )
                ),
                column(
                  2,
                  actionButton(
                    "btn_run_bayes",
                    "Calculate Bayes",
                    class = "btn-primary",
                    style = "margin-top: 25px;"
                  )
                ),
                column(
                  3,
                  align = "right",
                  uiOutput("bayes_run_status", style = "margin-top: 30px;")
                )
              ),

              shinycssloaders::withSpinner(
                plotlyOutput(
                  "bayes_standard_curve",
                  width  = "75vw",
                  height = "700px"
                ),
                type    = 6,
                color   = "#27ae60",
                caption = "Running Bayesian ensemble (Stan) in background — this takes ~30-60 s on first compile..."
              ),

              br()
            )
          ),

          br(),

          bsCollapsePanel(
            title = "Standard Curve QC Glossary",
            style = "success",
            tagList(
              tags$dl(
                style = "margin-bottom:0;",
                
                tags$dt(tags$strong("Standards")),
                tags$dd("Known concentration reference points used to construct the standard curve."),
                
                tags$dt(tags$strong("FDA + 2018")),
                tags$dd("Parameter QC Levels. Standard curve reference points with precision ≤ 20% unless more concentrated than the
                        ULOQ where ≤25% is used. In addition, between-plate accuracy is ±20% of the nominal concentration."),
                
                tags$dt(tags$strong("FDA - 2018")),
                tags$dd("Standard curve reference points with precision > 20% (> ULOQ: > 25%) and 
                        between-plate accuracy is > ±20% of the nominal concentration.
                        U.S. Food and Drug Administration. Bioanalytical Method Validation:
                        Guidance for Industry. Center for Drug Evaluation and Research (CDER) / Center for Veterinary Medicine (CVM). May 2018.
                        Available ", tags$a(
                          href   = "https://www.fda.gov/files/drugs/published/Bioanalytical-Method-Validation-Guidance-for-Industry.pdf",
                          "here",
                          target = "_blank")
                        ),
                
                tags$dt(tags$strong("Samples")),
                tags$dd(paste0(
                  "Unknown test samples interpolated against the standard curve to determine ",
                  "their concentrations based on measured assay response values."
                )),
                tags$dt(tags$strong("MCMC Samples")),
                tags$dd(paste0(
                  "Unknown test samples with a robust MCMC Bayes algorithm with the standard curve to determine ",
                  "their concentrations based on measured assay response values. MCMC sample estimates and their precision profile
                  are shown when the robust MCMC Bayes algorithm is completed for the samples for the specific standard curve."
                )),
                
                tags$dt(tags$strong("Fitted Curve")),
                tags$dd("The fitted sigmoidal curve to the standard data points."),
                
                tags$dt(tags$strong("95% CI (Confidence Interval)")),
                tags$dd("The 95% CI around the fitted standard curve."),
                
                tags$dt(tags$strong("Lower and Upper LODs (Limit of Detection)")),
                tags$dd(
                  paste0(
                    "Lower and upper LODs are defined as the upper 97.5% confidence bound of the ",
                    "lower asymptote and the lower 2.5% confidence bound of the upper asymptote, ",
                    "respectively "
                  ),
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  paste0(
                    " Limits of Detection correspond to the y-coordinate in the legend, ",
                    "as they are defined on the response axis."
                  )
                ),
                
                tags$dt(tags$strong("Minimum Detectable Concentration")),
                tags$dd(
                  "The smallest antibody concentration that produces a signal the assay can detect above background ",
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  paste0(
                    " This corresponds to the x-coordinate of the Lower Limit of Detection in the legend, ",
                    "as it is on the concentration axis."
                  )
                ),
                
                tags$dt(tags$strong("Lower and Upper RDL (Reliable Detection Limit)")),
                tags$dd(
                  "Lower RDL: The lowest concentration at which the assay consistently produces a signal above background with 95% confidence based on the fit of the standard curve ",
                  tags$a(
                    href   = "Development%20and%20validation%20of%20a%20robust%20multiplex%20serological%20assay.pdf",
                    "(Rajam et al.).",
                    target = "_blank"
                  ),
                  tags$br(),
                  " Upper RDL: Analogously, the highest concentration at which the assay consistently produces a signal below the upper asymptote (saturation) with 95% confidence, based on the fit of the standard curve."
                ),
                
                tags$dt(tags$strong("Lower and Upper LOQs (Limits of Quantification)")),
                tags$dd(
                  "Defines a region of assay response (MFI) and concentration where sample estimates have less measurement error. Limits of Quantification are derived from the local minimum and maximum of the second derivative of x given y of the standard curve ",
                  tags$a(
                    href   = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                    "(Daly et al.)",
                    target = "_blank"
                  ),
                  ", ",
                  tags$a(
                    href   = "LinearPortion_BendPoints.pdf",
                    "(Jeanne L Sebaugh and P. D. McCray)",
                    target = "_blank"
                  ),
                  ", ",
                  tags$a(
                    href   = "drLumi-An_open-source_package_to_manage_data_calibrate_and_conduct_quality_control_of_multiplex_bead-based_immunoassays_data_analysis.pdf",
                    "(Sanz et al.).",
                    target = "_blank"
                  )
                ),
                
                tags$dt(tags$strong("2nd Derivative of x given y")),
                tags$dd("The second derivative curve used to identify Limits of Quantification."),
                
                tags$dt(tags$strong("pCoV (predicted concentration Coefficient of Variation)")),
                tags$dd(
                  "A measure of the concentration estimation error corresponding to each sample measurement and is on the concentration uncertainty axis ",
                  tags$a(
                    href   = "Daly%20et.%20al%202005_BMCBioinformatics_Evaluating%20concentration%20estimation%20errors%20in%20ELISA%20microarray%20experiments1471-2105-6-17.pdf",
                    "(Daly et al.).",
                    target = "_blank"
                  )
                ),
                
                tags$dt(tags$strong("pCoV Threshold")),
                tags$dd("The acceptable cutoff for the predicted concentration coefficient of variation."),
                
                tags$dt(tags$strong("Inflection Point")),
                tags$dd(paste0(
                  "The point on the standard curve where the concavity transitions from concave up ",
                  "to concave down. It is the point where the assay is most sensitive to measurement ",
                  "errors in the measured response of the assay."
                ))
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.sc_curve_method == 'Frequentist'",
            div(class = "table-container", tableOutput("summary_statistics"))
          ),
          conditionalPanel(
            condition = "input.sc_curve_method == 'Bayesian'",
            div(class = "table-container", style = "overflow-x:auto;",
                tableOutput("bayes_summary_statistics"))
          ),
          uiOutput("concentrationMethodUI")
        )
      )
    })
    
    
    # ------------------------------------------------------------------
    # Prerequisites check (shared between context UI and conditional panel)
    # ------------------------------------------------------------------
    sc_prereqs <- reactive({
      standards_n         <- nrow(loaded_data$standards[
        loaded_data$standards$experiment_accession == selected_experiment, ])
      blanks_n            <- nrow(loaded_data$blanks[
        loaded_data$blanks$experiment_accession == selected_experiment, ])
      blank_option        <- study_params$blank_option
      constraints_methods <- unique(loaded_data$antigen_constraints$l_asy_constraint_method)
      invalid_constraints <- setdiff(constraints_methods, allowed_constraint_methods)
      
      list(
        standards_n         = standards_n,
        blanks_n            = blanks_n,
        blank_option        = blank_option,
        constraints_methods = constraints_methods,
        invalid_constraints = invalid_constraints,
        standards_missing   = standards_n == 0,
        blanks_missing      = blanks_n == 0,
        constraints_invalid = length(invalid_constraints) > 0,
        blank_required      = blank_option != "ignored",
        blank_blocking      = blanks_n == 0 && blank_option != "ignored"
      )
    })
    
    
    # ------------------------------------------------------------------
    # Context banner
    # ------------------------------------------------------------------
    output$standard_curve_context <- renderUI({
      p <- sc_prereqs()
      
      blank_labels <- c(
        ignored        = "Ignored",
        included       = "Included",
        subtracted     = "Subtracted 1 \u00d7 Geometric Mean",
        subtracted_3x  = "Subtracted 3 \u00d7 Geometric Mean",
        subtracted_5x  = "Subtracted 5 \u00d7 Geometric Mean",
        subtracted_10x = "Subtracted 10 \u00d7 Geometric Mean"
      )
      blank_label <- blank_labels[[p$blank_option]]
      
      reasons <- character(0)
      if (p$standards_missing)
        reasons <- c(reasons,
                     glue::glue("{p$standards_n} standards found in {selected_experiment}."))
      if (p$constraints_invalid)
        reasons <- c(reasons,
                     glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
      if (p$blank_blocking)
        reasons <- c(reasons, glue::glue(
          "Blank control is set to {blank_label} in the study settings but {p$blanks_n} blanks found."
        ))
      
      blank_note <- if (!p$blank_blocking) {
        glue::glue(
          " (ok since blank control is currently set to {blank_label} in study settings.)"
        )
      } else {
        ""
      }
      
      if (length(reasons) > 0) {
        return(HTML(glue::glue(
          "Standard curve fitting cannot proceed for {selected_experiment}.<br><br>",
          "Unmet requirements:<br>",
          "{paste(paste0('&nbsp;- ', reasons), collapse = '<br>')}<br><br>",
          "Details:<br>",
          "- Standards: {p$standards_n}<br>",
          "- Blanks: {p$blanks_n}{blank_note}<br>",
          "- Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
          "- Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}"
        )))
      }
      
      HTML(glue::glue(
        "{p$standards_n} standards found for {selected_experiment}.<br>",
        "{p$blanks_n} blanks found for {selected_experiment}.<br>",
        "Constraint method(s) found in {selected_experiment}: {paste(p$constraints_methods, collapse=', ')}<br>",
        "Constraint method(s) valid: {ifelse(p$constraints_invalid, 'no', 'yes')}<br>",
        "Current Blank Option selected (in study settings): {blank_label}<br><br>",
        "Standard curve fitting may proceed."
      ))
    })
    
    
    # ------------------------------------------------------------------
    # Gate for the conditional panel
    # ------------------------------------------------------------------
    can_fit_standard_curve <- reactive({
      p       <- sc_prereqs()
      reasons <- character(0)
      if (p$standards_missing)
        reasons <- c(reasons,
                     glue::glue("{p$standards_n} standards found in {selected_experiment}."))
      if (p$constraints_invalid)
        reasons <- c(reasons,
                     glue::glue("Invalid constraint methods: {paste(p$invalid_constraints, collapse=', ')}"))
      if (p$blank_blocking)
        reasons <- c(reasons,
                     glue::glue("Blanking is '{p$blank_option}' but {p$blanks_n} blanks found."))
      length(reasons) == 0
    })
    
    output$can_fit_standard_curve <- reactive({ can_fit_standard_curve() })
    outputOptions(output, "can_fit_standard_curve", suspendWhenHidden = FALSE)
    
    
    # ------------------------------------------------------------------
    # Selectors
    # ------------------------------------------------------------------
    output$sc_plate_selector <- renderUI({
      req(loaded_data$standards$study_accession,
          loaded_data$standards$experiment_accession,
          nrow(loaded_data$standards) > 0)
      
      updateSelectInput(session, "sc_plate_select", selected = NULL)  # Reset the plateSelection
      req(nrow(loaded_data$standards) > 0)

     unique_plates <- unique(loaded_data$standards$plate_nom)

      selectInput("sc_plate_select",
                  label = "Plate - Sample Dilution(s)",
                  choices = unique_plates)
    })

    output$sc_antigen_selector <- renderUI({
      req(loaded_data$standards$study_accession, loaded_data$standards$experiment_accession)
      updateSelectInput(session, "sc_antigen_select", selected = NULL)
      
      response_var <- loaded_data$response_var  # "absorbance" for ELISA, "mfi" for Luminex
      
      cat("debug in antigen selector:\n")
      print(response_var)
      
      print(selected_study)
      print(selected_experiment)
      print(input$sc_plate_select)
      
      print(head(loaded_data$standards))

      dat_antigen <- loaded_data$standards[loaded_data$standards$study_accession %in% selected_study &
                                          loaded_data$standards$experiment_accession %in% selected_experiment &
                                            loaded_data$standards$plate_nom %in% input$sc_plate_select, ]
      
      # Use the correct response variable column name
      dat_antigen <- dat_antigen[!is.na(dat_antigen[[response_var]]),]
      
      req(nrow(dat_antigen) > 0)
      
      sc_feature_select(dat_antigen$feature)
      
      selectInput("sc_antigen_select",
                  label = "Antigen",
                  choices = unique(dat_antigen$antigen))
    })

    output$sc_source_selector <- renderUI({
      req(loaded_data$standards, input$sc_plate_select, input$sc_antigen_select)
      
      cat("debug in source selector:\n")
      print(selected_study)
      print(selected_experiment)
      print(input$sc_plate_select)
      print(input$sc_antigen_select)
      
      print(head(loaded_data$standards))
      

      dat_source <- loaded_data$standards[
          loaded_data$standards$study_accession %in% selected_study &
          loaded_data$standards$experiment_accession %in% selected_experiment &
          loaded_data$standards$plate_nom %in% input$sc_plate_select &
          loaded_data$standards$antigen %in% input$sc_antigen_select, ]


      req(nrow(dat_source) > 0)

      # Use source_nom for the selector (includes wavelength for ELISA)
      source_choices <- if ("source_nom" %in% names(dat_source)) {
        unique(dat_source$source_nom)
      } else {
        unique(dat_source$source)
      }

      radioButtons(
        "sc_source_select",
        label = "Source",
        choices = source_choices,
        selected = source_choices[1]
      )
    })
    
    # output$sc_antigen_selector <- renderUI({
    #   req(loaded_data$standards$study_accession,
    #       loaded_data$standards$experiment_accession)
    #   
    #   updateSelectInput(session, "sc_antigen_select", selected = NULL)
    #   
    #   dat <- loaded_data$standards[
    #     loaded_data$standards$study_accession      %in% selected_study &
    #       loaded_data$standards$experiment_accession %in% selected_experiment &
    #       loaded_data$standards$plate_nom            %in% input$sc_plate_select, ]
    #   dat <- dat[!is.na(dat$mfi), ]
    #   req(nrow(dat) > 0)
    #   
    #   selectInput("sc_antigen_select",
    #               label   = "Antigen",
    #               choices = unique(dat$antigen))
    # })
    
    # output$sc_source_selector <- renderUI({
    #   req(loaded_data$standards, input$sc_plate_select, input$sc_antigen_select)
    #   
    #   dat <- loaded_data$standards[
    #     loaded_data$standards$study_accession      %in% selected_study &
    #       loaded_data$standards$experiment_accession %in% selected_experiment &
    #       loaded_data$standards$plate_nom            %in% input$sc_plate_select &
    #       loaded_data$standards$antigen              %in% input$sc_antigen_select, ]
    #   req(nrow(dat) > 0)
    #   
    #   radioButtons(
    #     "sc_source_select",
    #     label    = "Source",
    #     choices  = unique(dat$source),
    #     selected = unique(dat$source)[1]
    #   )
    # })
    # 
    
    # ------------------------------------------------------------------
    # Loading notification
    # ------------------------------------------------------------------
    sc_loading_id      <- reactiveVal(NULL)
    sc_best_fit_ready  <- reactiveVal(FALSE)
    
    observeEvent(
      list(input$sc_plate_select, input$sc_antigen_select, input$sc_source_select),
      ignoreInit = TRUE,
      {
        req(input$sc_plate_select, input$sc_antigen_select)
        
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
        sc_best_fit_ready(FALSE)
        
        id <- showNotification(
          ui = tagList(
            tags$b("Fitting standard curve..."),
            tags$br(),
            tags$span(
              style = "font-size:0.9em; color:#555;",
              paste0("Plate: ", input$sc_plate_select,
                     " | Antigen: ", input$sc_antigen_select)
            )
          ),
          duration    = NULL,
          closeButton = FALSE,
          type        = "message",
          id          = "sc_loading"
        )
        sc_loading_id(id)
      }
    )
    
    observeEvent(best_fit(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      shinyjs::delay(100, {
        if (!is.null(sc_loading_id())) {
          removeNotification(sc_loading_id())
          sc_loading_id(NULL)
        }
        sc_best_fit_ready(TRUE)
      })
    })
    
    
    # ------------------------------------------------------------------
    # Curve-fitting reactives
    # ------------------------------------------------------------------
    antigen_plate <- reactive({
      req(input$sc_source_select,
          input$sc_antigen_select,
          input$sc_plate_select,
          loaded_data,
          loaded_data$antigen_constraints,
          loaded_data$standards,
          nrow(loaded_data$standards) > 0)

      antigen_constraints <- loaded_data$antigen_constraints[
        loaded_data$antigen_constraints$antigen %in% input$sc_antigen_select, ,
        drop = FALSE]
      req(nrow(antigen_constraints) > 0)
      
      result <- select_antigen_plate(
        loaded_data          = loaded_data,
        study_accession      = selected_study,
        experiment_accession = selected_experiment,
        source               = input$sc_source_select,
        antigen              = input$sc_antigen_select,
        plate                = input$sc_plate_select,
        antigen_constraints  = antigen_constraints
      )
      req(result)
      req(result$plate_standard, nrow(result$plate_standard) > 0)
      result
    })
    
    prepped_data <- reactive({
      plate <- antigen_plate()
      req(plate)
      
      preprocess_robust_curves(
        data                 = plate$plate_standard,
        antigen_settings     = plate$antigen_settings,
        response_variable    = loaded_data$response_var,
        independent_variable = loaded_data$indep_var,
        is_log_response      = study_params$is_log_response,
        blank_data           = plate$plate_blanks,
        blank_option         = study_params$blank_option,
        is_log_independent   = study_params$is_log_independent,
        apply_prozone        = study_params$applyProzone,
        verbose              = verbose
      )
    })
    
    formulas <- reactive({
      plate <- antigen_plate()
      req(plate)
      
      select_model_formulas(
        fixed_constraint  = plate$fixed_a_result,
        response_variable = loaded_data$response_var,
        is_log_response   = study_params$is_log_response
      )
    })
    
    model_constraints <- reactive({
      plate <- antigen_plate()
      pdata <- prepped_data()
      f     <- formulas()
      req(plate, pdata, f)
      
      obtain_model_constraints(
        data                 = pdata$data,
        formulas             = f,
        independent_variable = loaded_data$indep_var,
        response_variable    = loaded_data$response_var,
        is_log_response      = TRUE,
        is_log_concentration = TRUE,
        antigen_settings     = plate$antigen_settings,
        max_response         = max(pdata$data[[loaded_data$response_var]], na.rm = TRUE),
        min_response         = min(pdata$data[[loaded_data$response_var]], na.rm = TRUE)
      )
    })
    
    start_lists <- reactive({
      mc <- model_constraints()
      req(mc)
      
      make_start_lists(
        model_constraints = mc,
        frac_generate     = 0.8,
        quants            = c(low = 0.2, mid = 0.5, high = 0.8)
      )
    })
    
    fit_robust_lm <- reactive({
      pdata <- prepped_data()
      f     <- formulas()
      mc    <- model_constraints()
      sl    <- start_lists()
      req(pdata, f, mc, sl)
      
      compute_robust_curves(
        prepped_data         = pdata$data,
        response_variable    = loaded_data$response_var,
        independent_variable = loaded_data$indep_var,
        formulas             = f,
        model_constraints    = mc,
        start_lists          = sl,
        verbose              = verbose
      )
    })
    
    fit_summary <- reactive({
      fit <- fit_robust_lm()
      req(fit)
      summarize_model_fits(fit, verbose = verbose)
    })
    
    fit_params <- reactive({
      fit <- fit_robust_lm()
      req(fit)
      
      summarize_model_parameters(
        models_fit_list = fit,
        level           = 0.95,
        model_names     = model_names
      )
    })
    
    plot_data <- reactive({
      pdata  <- prepped_data()
      fit    <- fit_robust_lm()
      plate  <- antigen_plate()
      params <- fit_params()
      req(pdata, fit, plate, params)
      
      get_plot_data(
        models_fit_list = fit,
        prepped_data    = pdata$data,
        fit_params      = params,
        fixed_a_result  = plate$fixed_a_result,
        model_names     = model_names,
        x_var           = loaded_data$indep_var,
        y_var           = loaded_data$response_var
      )
    })
    
    best_fit <- reactive({
      params     <- fit_params()
      fit        <- fit_robust_lm()
      summary    <- fit_summary()
      pdata      <- prepped_data()
      plate      <- antigen_plate()
      pdata_plot <- plot_data()
      mc         <- model_constraints()
      req(params, fit, summary, pdata, plate, pdata_plot, mc)
      
      current_se <- lookup_antigen_se(
        se_table             = se_antigen_table,
        study_accession      = selected_study,
        experiment_accession = selected_experiment,
        source = input$sc_source_select,
        antigen = input$sc_antigen_select,
        feature = sc_feature_select()
      )
  
      bf <- select_model_fit_AIC(
        fit_summary   = summary,
        fit_robust_lm = fit,
        fit_params    = params,
        plot_data     = pdata_plot,
        verbose       = verbose
      )
      # ── Ensure response column is valid ──────────────────────────────
      resolved <- ensure_response_column(
        df           = bf$best_data,
        response_var = response_var,
        coerce_numeric = TRUE,
        context      = "bf_reactive"
      )
      
      bf$best_data <- resolved$df
      
      # If the column was found under a different name, update response_var
      # for all downstream calls in this reactive
      if (resolved$ok && resolved$response_var != response_var) {
        message(sprintf(
          "[bf reactive] response_var changed from '%s' to '%s'",
          response_var, resolved$response_var
        ))
        response_var <- resolved$response_var
      }
      
      # ── Ensure response column is numeric in best_data ──────────────
      response_var <- loaded_data$response_var
      if (!is.null(bf$best_data) && 
          response_var %in% names(bf$best_data) &&
          !is.numeric(bf$best_data[[response_var]])) {
        message(sprintf(
          "[bf reactive] Coercing '%s' from %s to numeric in best_data",
          response_var, class(bf$best_data[[response_var]])[1]
        ))
        bf$best_data[[response_var]] <- suppressWarnings(
          as.numeric(bf$best_data[[response_var]])
        )
      }
      
      dil_series_df_filtered <- tryCatch({
        # Filter to the specific antigen + plate + source being fitted
        mask <- (
          dil_series_se_table$plate_nom  == input$sc_plate_select  &
            dil_series_se_table$source_nom == input$sc_source_select &
            dil_series_se_table$antigen    == input$sc_antigen_select
        )
        sub <- dil_series_se_table[mask, , drop = FALSE]
        if (nrow(sub) == 0L) {
          if (verbose) message("[best_fit] dil_series_se_table: no rows after antigen filter — skipping accuracy")
          NULL
        } else {
          sub
        }
      }, error = function(e) {
        message("[best_fit] dil_series_se_table filter error: ", e$message)
        NULL
      })
        
        dil_series_acc <- if (!is.null(dil_series_df_filtered)) {
          tryCatch(
            compute_dil_series_accuracy(
              best_fit                   = bf,
              dil_series_df              = dil_series_df_filtered,
              response_col               = response_var,
              independent_variable       = loaded_data$indep_var,
              dilution_col               = "dilution",
              fixed_a_result             = plate$fixed_a_result,
              is_log_response            = study_params$is_log_response,
              is_log_concentration       = study_params$is_log_independent,
              undiluted_sc_concentration = plate$antigen_settings$standard_curve_concentration,
              cv_threshold               = 20, #plate$antigen_settings$pcov_threshold, #15 # pCoV threshold 
              lloq_cv_threshold          = 25,  # if it is the lowest dilution factor/highest concentration use this. 
              accuracy_lo                = 80,
              accuracy_hi                = 120,
              verbose                    = verbose
            ),
            error = function(e) {
              message("[best_fit] compute_dil_series_accuracy error: ", e$message)
              dil_series_df_filtered   # return unmodified if accuracy fails
            }
          )
        } else {
          NULL
        }
        
      #dil_series_acc_v <<- dil_series_acc
      
      bf <- fit_qc_glance(
        best_fit             = bf,
        response_variable    = response_var,
        independent_variable = loaded_data$indep_var,
        fixed_a_result       = plate$fixed_a_result,
        antigen_settings     = plate$antigen_settings,
        antigen_fit_options  = pdata$antigen_fit_options,
        dil_series_se_plate_source = dil_series_acc,
        verbose              = verbose
      )
      

      
      bf <- tidy.nlsLM(
        best_fit            = bf,
        fixed_a_result      = plate$fixed_a_result,
        model_constraints   = mc,
        antigen_settings    = plate$antigen_settings,
        antigen_fit_options = pdata$antigen_fit_options,
        verbose             = verbose
      )
        
      bf <- predict_and_propagate_error(
        best_fit        = bf,
        response_var    = response_var,
        antigen_plate   = plate,
        study_params    = study_params,
        se_std_response = current_se,
        verbose         = verbose
      )
      
      gate_samples(
        best_fit          = bf,
        response_variable = loaded_data$response_var,
        pcov_threshold    = plate$antigen_settings$pcov_threshold,
        verbose           = verbose
      )
    })
    
    
    # ------------------------------------------------------------------
    # Toggle switches
    # ------------------------------------------------------------------
    output$is_display_log_response <- renderUI({
      input_switch("display_log_response",
                   "Display as Log Response",
                   value = TRUE)
    })
    
    output$is_display_log_independent_variable <- renderUI({
      input_switch("display_log_independent",
                   "Display independent variable as logged",
                   value = TRUE)
    })
    
    
    # ------------------------------------------------------------------
    # Standard curve plot
    # ------------------------------------------------------------------
    output$standard_curve <- renderPlotly({
      bf    <- best_fit()
      plate <- antigen_plate()
      req(bf, plate)

      p <- plot_standard_curve(
        best_fit                   = bf,
        is_display_log_response    = input$display_log_response,
        is_display_log_independent = input$display_log_independent,
        pcov_threshold             = plate$antigen_settings$pcov_threshold,
        independent_variable       = loaded_data$indep_var,
        response_variable          = loaded_data$response_var,
        mcmc_samples               = plate$plate_mcmc_samples,
        mcmc_pred                  = plate$plate_mcmc_pred
      )
      freq_curve_plot_cache(p)   # store for comparison overlay
      comparison_visible(FALSE)  # reset comparison when curve changes
      sc_concentration_cache(plate$antigen_settings$standard_curve_concentration)
      p
    })
    
    
    # ------------------------------------------------------------------
    # Model comparisons modal
    # ------------------------------------------------------------------
    output$model_comparisions <- renderPlot({
      pd <- plot_data()
      req(pd)
      
      plot_model_comparisons(
        plot_data                  = pd,
        model_names                = model_names,
        x_var                      = loaded_data$indep_var,
        y_var                      = loaded_data$response_var,
        is_display_log_response    = input$display_log_response,
        is_display_log_independent = input$display_log_independent,
        use_patchwork              = TRUE
      )
    })
    
    observeEvent(input$show_comparisions, {

      # ── Bayesian mode: LOO ensemble comparison ──
      if (identical(input$sc_curve_method, "Bayesian")) {
        if (bayes_state$status != "ready") {
          showNotification(
            "Please run 'Calculate Bayes' first before viewing model comparisons.",
            type = "warning"
          )
          return()
        }

        viewing_plate <- input$bayes_view_plate %||% "\u2014"
        pbf <- bayes_state$plate_best_family
        plate_best <- if (!is.null(pbf) && viewing_plate %in% names(pbf)) {
          fam <- pbf[[viewing_plate]]
          switch(fam, "4pl" = "4PL", "5pl" = "5PL", gompertz = "Gompertz", fam)
        } else { "\u2014" }

        showModal(modalDialog(
          title     = sprintf("Model Comparison \u2014 %s (Best: %s)", viewing_plate, plate_best),
          size      = "l",
          easyClose = TRUE,

          # Widen the modal to fit parameter tables
          tags$head(tags$style(HTML(
            ".modal-lg { max-width: 95vw; width: 95vw; }
             .modal-lg .modal-body { overflow-x: auto; }"
          ))),

          # ── Per-Plate section (primary) ─────────────────────────────
          tags$h5(
            icon("flask"), sprintf(" %s \u2014 Per-Family Comparison", viewing_plate),
            style = "margin-top:0; color:#1a7a40;"
          ),
          tags$p(class = "text-muted",
                 "ELPD and parameter estimates for each model family on this plate. ",
                 "Star marks the best-fitting family by plate-level ELPD."),
          div(style = "overflow-x:auto;",
              tableOutput("bayes_modal_plate_params_tbl")),

          hr(),

          # ── Global Ensemble section (secondary) ─────────────────────
          tags$h5(icon("globe"), " Global Ensemble (all plates)", style = "color:#2c5aa0;"),

          fluidRow(
            column(4,
              tags$h6("Stacking Weights"),
              tags$p(class = "text-muted", style = "font-size:0.85em;",
                     "Bayesian stacking (Yao et al. 2018)."),
              tableOutput("bayes_modal_stacking_tbl")
            ),
            column(4,
              tags$h6("LOO-CV Comparison"),
              tags$p(class = "text-muted", style = "font-size:0.85em;",
                     "PSIS-LOO (Vehtari et al.). Best model first."),
              div(style = "overflow-x:auto;", tableOutput("bayes_loo_comparison_tbl"))
            ),
            column(4,
              tags$h6("Pareto k Diagnostics"),
              tags$p(class = "text-muted", style = "font-size:0.85em;",
                     "Good (k\u22640.5), Ok, Bad, Very Bad."),
              tableOutput("bayes_pareto_k_tbl")
            )
          ),

          br(),
          tags$h6("Per-Plate ELPD Breakdown"),
          tags$p(class = "text-muted", style = "font-size:0.85em;",
                 "Pointwise ELPD aggregated per plate. Best model selected by highest ELPD."),
          div(class = "table-container", style = "overflow-x:auto;",
              tableOutput("bayes_modal_plate_elpd_tbl")),

          footer = modalButton("Close")
        ))
        return()
      }

      # ── Frequentist mode: existing plot-based comparison ──
      pd <- tryCatch(plot_data(), error = function(e) NULL)

      if (is.null(pd) || is.null(pd$dat)) {
        showNotification(
          "No plot data available. Please ensure standard curve data is loaded.",
          type = "warning"
        )
        return()
      }

      showModal(modalDialog(
        title     = paste("Model Comparisons for",
                          unique(pd$dat$antigen), "on", unique(pd$dat$plate)),
        size      = "l",
        plotOutput("model_comparisions"),
        downloadButton("download_model_comparisons", "Download Model Comparisons"),
        easyClose = TRUE,
        footer    = modalButton("Close")
      ))
    })
    
    output$download_model_comparisons <- downloadHandler(
      filename = function() {
        pd <- plot_data()
        paste0(
          "model_comparison_",
          unique(pd$dat$study_accession),
          unique(pd$dat$experiment_accession),
          unique(pd$dat$plate_nom),
          unique(pd$dat$antigen),
          ".pdf"
        )
      },
      content = function(file) {
        pd <- plot_data()
        req(pd)
        
        p <- plot_model_comparisons(
          plot_data                  = pd,
          model_names                = model_names,
          x_var                      = loaded_data$indep_var,
          y_var                      = loaded_data$response_var,
          is_display_log_response    = input$display_log_response,
          is_display_log_independent = input$display_log_independent,
          use_patchwork              = TRUE
        )
        
        ggsave(filename = file, plot = p, device = "pdf",
               width = 8, height = 10, units = "in")
      }
    )
    
    
    # ------------------------------------------------------------------
    # Summary statistics table
    # ------------------------------------------------------------------
    output$summary_statistics <- renderTable({
      bf <- best_fit()
      req(bf)
      bf$best_glance
    },
    caption           = "Summary Statistics",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    
    # ------------------------------------------------------------------
    # Downloads
    # ------------------------------------------------------------------
    output$download_best_fit_parameter_estimates <- downloadHandler(
      filename = function() {
        if (identical(input$sc_curve_method, "Bayesian")) {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_antigen_select,
                "bayesian_parameter_estimates.csv", sep = "_")
        } else {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_plate_select, "x",
                input$sc_antigen_select,
                "tidy_parameter_estimates.csv", sep = "_")
        }
      },
      content = function(file) {
        if (identical(input$sc_curve_method, "Bayesian")) {
          req(bayes_state$status == "ready")

          # DB mode: use bayes_ensemble table (has param CIs per family)
          if (identical(bayes_state$data_source, "database") && !is.null(bayes_state$ensemble_data)) {
            ens <- bayes_state$ensemble_data
            rows <- list()
            for (i in seq_len(nrow(ens))) {
              r <- ens[i, ]
              param_names <- c("a", "b", "c", "d")
              if (r$family %in% c("4pl", "5pl")) param_names <- c(param_names, "g")
              for (pnm in param_names) {
                lwr_col <- paste0(pnm, "_lower")
                upr_col <- paste0(pnm, "_upper")
                rows[[length(rows) + 1]] <- data.frame(
                  plate     = r$plateid,
                  family    = toupper(r$family),
                  parameter = pnm,
                  median    = as.numeric(r[[pnm]]),
                  q2.5      = as.numeric(r[[lwr_col]]),
                  q97.5     = as.numeric(r[[upr_col]]),
                  stringsAsFactors = FALSE, check.names = FALSE
                )
              }
            }
            df <- do.call(rbind, rows)
            rownames(df) <- NULL
            write.csv(df, file, row.names = FALSE)

          # Live fit mode: extract full posterior from Stan
          } else {
            req(bayes_state$assay)
            assay <- bayes_state$assay
            pbf   <- bayes_state$plate_best_family
            req(pbf)

            rows <- list()
            for (pid in names(pbf)) {
              fam     <- pbf[[pid]]
              fit_obj <- assay$ensemble$fits[[fam]]
              if (is.null(fit_obj)) next
              plate_map <- fit_obj$plate_map
              plate_idx <- which(as.character(plate_map$plateid) == pid)
              if (length(plate_idx) == 0L) next

              samples <- rstan::extract(fit_obj$fit)
              param_names <- c("a", "d", "b", "log_c")
              if (fam %in% c("4pl", "5pl")) param_names <- c(param_names, "g")

              for (pnm in param_names) {
                draws <- samples[[pnm]][, plate_idx]
                rows[[length(rows) + 1]] <- data.frame(
                  plate     = pid,
                  family    = toupper(fam),
                  parameter = pnm,
                  mean      = mean(draws),
                  median    = median(draws),
                  sd        = sd(draws),
                  q2.5      = quantile(draws, 0.025),
                  q97.5     = quantile(draws, 0.975),
                  stringsAsFactors = FALSE, check.names = FALSE
                )
              }
            }
            df <- do.call(rbind, rows)
            rownames(df) <- NULL
            write.csv(df, file, row.names = FALSE)
          }
        } else {
          bf <- best_fit()
          req(bf)
          write.csv(bf$best_tidy, file, row.names = FALSE)
        }
      }
    )

    output$download_samples_above_ulod <- downloadHandler(
      filename = function() {
        if (identical(input$sc_curve_method, "Bayesian")) {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_antigen_select,
                "bayesian_samples_above_uloq.csv", sep = "_")
        } else {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_plate_select, "x",
                input$sc_antigen_select,
                "samples_above_ulod.csv", sep = "_")
        }
      },
      content = function(file) {
        if (identical(input$sc_curve_method, "Bayesian")) {
          req(bayes_state$status == "ready")
          sr <- bayes_state$sample_results
          if (is.null(sr) || nrow(sr) == 0) {
            write.csv(data.frame(message = "No sample data available"), file, row.names = FALSE)
            return()
          }
          above <- sr[!is.na(sr$gate_class) & sr$gate_class == "Above ULOQ", , drop = FALSE]
          write.csv(above, file, row.names = FALSE)
        } else {
          bf <- best_fit()
          req(bf)
          se <- bf$sample_se
          drop_cols <- c("plate_id", "assay_response_variable",
                         "assay_independent_variable", "y_new", "overall_se")
          write.csv(
            se[se$gate_class_lod == "Too Concentrated",
               !(names(se) %in% drop_cols)],
            file, row.names = FALSE
          )
        }
      }
    )

    output$download_samples_below_llod <- downloadHandler(
      filename = function() {
        if (identical(input$sc_curve_method, "Bayesian")) {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_antigen_select,
                "bayesian_samples_below_lloq.csv", sep = "_")
        } else {
          paste(input$readxMap_study_accession,
                input$readxMap_experiment_accession,
                input$sc_plate_select, "x",
                input$sc_antigen_select,
                "samples_below_llod.csv", sep = "_")
        }
      },
      content = function(file) {
        if (identical(input$sc_curve_method, "Bayesian")) {
          req(bayes_state$status == "ready")
          sr <- bayes_state$sample_results
          if (is.null(sr) || nrow(sr) == 0) {
            write.csv(data.frame(message = "No sample data available"), file, row.names = FALSE)
            return()
          }
          below <- sr[!is.na(sr$gate_class) & sr$gate_class == "Below LLOQ", , drop = FALSE]
          write.csv(below, file, row.names = FALSE)
        } else {
          bf <- best_fit()
          req(bf)
          se <- bf$sample_se
          drop_cols <- c("plate_id", "assay_response_variable",
                         "assay_independent_variable", "y_new", "overall_se")
          write.csv(
            se[se$gate_class_lod == "Too Diluted",
               !(names(se) %in% drop_cols)],
            file, row.names = FALSE
          )
        }
      }
    )
    
    
    # ------------------------------------------------------------------
    # Concentration method UI (references session-scoped concentration_calc_df)
    # ------------------------------------------------------------------
    # output$concentrationMethodUI <- renderUI({
    #   df  <- concentration_calc_df()
    #   msg <- mcmc_progress_msg()
    #   
    #   if (nrow(df) == 0) {
    #     HTML("<p>No concentration methods have been calculated for this plate.</p>")
    #   } else {
    #     createStandardCurveConcentrationTypeUI(
    #       existing_concentration_calc = df,
    #       progress_msg                = msg
    #     )
    #   }
    # })
    output$concentrationMethodUI <- renderUI({
      df         <- concentration_calc_df()
      interp_msg <- interp_progress_msg()

      # Build Bayesian status for each scope
      bayes_sl <- tryCatch({
        proj <- userWorkSpaceID()
        stdy <- input$readxMap_study_accession
        expt <- input$readxMap_experiment_accession
        antg <- input$sc_antigen_select
        list(
          study      = get_bayes_calc_status(conn, proj, stdy, NULL, NULL, "study"),
          experiment = get_bayes_calc_status(conn, proj, stdy, expt, NULL, "experiment"),
          antigen    = get_bayes_calc_status(conn, proj, stdy, expt, antg, "antigen")
        )
      }, error = function(e) {
        message("[concentrationMethodUI] bayes status error: ", e$message)
        NULL
      })

      # Poll running jobs — invalidate every 10s if any Bayesian job is active
      if (!is.null(bayes_sl)) {
        active <- any(vapply(bayes_sl, function(x) {
          identical(x$status, "pending")
        }, logical(1)))
        if (active) {
          # Re-poll the API and update audit table for active jobs
          for (s in names(bayes_sl)) {
            st <- bayes_sl[[s]]
            if (identical(st$status, "pending") && !is.null(st$job_id)) {
              api_st <- tryCatch(poll_bayes_job(st$job_id), error = function(e) NULL)
              if (!is.null(api_st)) {
                tryCatch(update_bayes_job_audit(conn, st$job_id, api_st), error = function(e) NULL)
                # Update the status with fresh API data
                bayes_sl[[s]]$progress    <- api_st$progress %||% st$progress
                bayes_sl[[s]]$percentage  <- api_st$percentage %||% st$percentage
                bayes_sl[[s]]$eta_display <- api_st$eta_display %||% st$eta_display
                # Check if completed
                if (api_st$status %in% c("completed", "failed", "error")) {
                  bayes_sl[[s]]$status    <- api_st$status
                  bayes_sl[[s]]$timestamp <- api_st$completed_at %||% Sys.time()
                  bayes_sl[[s]]$error     <- api_st$error
                }
              }
            }
          }
          invalidateLater(10000, session)
        }
      }

      createStandardCurveConcentrationTypeUI(
        existing_concentration_calc = df,
        progress_msg                = NULL,
        interp_progress_msg         = interp_msg,
        bayes_status_list           = bayes_sl
      )
    })
    
  } # end if Standard Curve
)   # end navigation observer


# ============================================================================
# Scope selector
# Rendered once; selected_scope() restores the user's last choice so the
# radio buttons don't snap back to "study" when concentration_calc_df()
# invalidates mid-calculation.
# ============================================================================
output$calculation_scope_ui <- renderUI({
  tagList(
    tags$strong("Frequentist scope:", style = "margin-right:8px;"),
    radioButtons(
      inputId  = "save_scope",
      label    = NULL,
      choices  = c(
        "Current Plate"      = "plate",
        "Current Experiment" = "experiment",
        "All Experiments"    = "study"
      ),
      selected = selected_scope(),
      inline   = TRUE
    ),
    tags$strong("Bayesian scope:", style = "margin-right:8px; margin-top:6px;"),
    radioButtons(
      inputId  = "bayes_scope",
      label    = NULL,
      choices  = c(
        "Current Antigen"    = "antigen",
        "Current Experiment" = "experiment",
        "All Experiments"    = "study"
      ),
      selected = "antigen",
      inline   = TRUE
    )
  )
})

# Mirror every user change into the persistent reactiveVal
observeEvent(input$save_scope, ignoreInit = TRUE, {
  selected_scope(input$save_scope)
})


# ============================================================================
# Concentration buttons UI
# Uses selected_scope() (not input$save_scope) so the buttons remain stable
# while a long-running calculation causes concentration_calc_df() to re-run.
# ============================================================================
output$concentration_buttons_ui <- renderUI({
  df          <- concentration_calc_df()
  freq_scope  <- selected_scope()
  bayes_scope <- input$bayes_scope %||% "antigen"

  interp_status <- get_status(df, freq_scope, "interpolated")

  freq_scope_label <- switch(freq_scope,
    "study"      = "(All Experiments)",
    "experiment" = "(Current Experiment)",
    "plate"      = "(Current Plate)"
  )

  bayes_scope_label <- switch(bayes_scope,
    "study"      = "(All Experiments)",
    "experiment" = "(Current Experiment)",
    "antigen"    = "(Current Antigen)"
  )

  interp_btn <- make_method_btn(
    status       = interp_status,
    input_id     = paste0("run_batch_fit_", freq_scope),
    method_label = "Frequentist",
    scope_label  = freq_scope_label
  )

  bayes_btn <- actionButton(
    "run_bayes_batch",
    tagList(
      icon("flask"),
      tags$strong(" Calculate Bayesian"),
      " concentrations ",
      tags$br(),
      bayes_scope_label
    ),
    class = "btn",
    style = paste0(
      "padding:12px 30px; font-size:14px; line-height:1.5; ",
      "white-space:normal; margin:5px; border-radius:5px; ",
      "background-color:#27ae60; border-color:#1a7a40; color:white;"
    )
  )

  tagList(interp_btn, bayes_btn)
})


# ============================================================================
# Bayesian batch concentration observer — single button, reads scope from radio
# Submits a job to the batch API and saves audit trail.
# ============================================================================
observeEvent(input$run_bayes_batch, ignoreInit = TRUE, {

  req(
    input$readxMap_study_accession      != "Click here",
    input$readxMap_experiment_accession != "Click here"
  )

  s <- input$bayes_scope %||% "antigen"

  study_val  <- input$readxMap_study_accession
  exp_val    <- input$readxMap_experiment_accession
  ant_val    <- input$sc_antigen_select
  proj_id    <- userWorkSpaceID()

  scope_label <- switch(s,
    "study"      = "all experiments",
    "experiment" = paste0("experiment ", exp_val),
    "antigen"    = paste0("antigen ", ant_val)
  )

  # Validate required inputs per scope
  if (s %in% c("experiment", "antigen") && (is.null(exp_val) || !nzchar(exp_val))) {
    showNotification("Please select an experiment first.", type = "warning")
    return()
  }
  if (s == "antigen" && (is.null(ant_val) || !nzchar(ant_val))) {
    showNotification("Please select an antigen first.", type = "warning")
    return()
  }

  # Submit to batch API
  tryCatch({
    result <- submit_bayes_job(
      project_id = proj_id,
      study      = study_val,
      experiment = if (s %in% c("experiment", "antigen")) exp_val else NULL,
      antigen    = if (s == "antigen") ant_val else NULL,
      scope      = s
    )

    job_id <- result$job_id
    if (is.null(job_id)) stop("No job_id returned from API")

    # Save to audit table
    save_bayes_job_audit(
      conn       = conn,
      project_id = proj_id,
      study      = study_val,
      experiment = if (s %in% c("experiment", "antigen")) exp_val else NULL,
      antigen    = if (s == "antigen") ant_val else NULL,
      scope      = s,
      job_id     = job_id
    )

    showNotification(
      sprintf("Bayesian batch job submitted for %s (job: %s)",
              scope_label, substr(job_id, 1, 8)),
      type = "message", duration = 6
    )

    # Trigger UI refresh so polling starts
    concentrationUIRefresher(concentrationUIRefresher() + 1L)

  }, error = function(e) {
    showNotification(
      paste0("Failed to submit Bayesian batch job: ", e$message),
      type = "error", duration = 10
    )
  })
})


# ============================================================================
# Frequentist (Interpolated) concentration observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  observeEvent(input[[paste0("run_batch_fit_", s)]], ignoreInit = TRUE, {
    
    scope <- s
    
    if (is_batch_processing()) {
      showNotification("Batch processing is already running. Please wait.",
                       type = "warning", duration = 10, closeButton = TRUE)
      return()
    }
    
    req(
      input$qc_component                  == "Standard Curve",
      input$readxMap_study_accession      != "Click here",
      input$readxMap_experiment_accession != "Click here",
      input$study_level_tabs              == "Experiments",
      input$main_tabs                     == "view_files_tab"
    )
    
    if (is.null(input$readxMap_study_accession) ||
        input$readxMap_study_accession == "") {
      showNotification("Please select a study before running batch processing.",
                       type = "error", duration = 10, closeButton = TRUE)
      return()
    }
    
    scope_label <- switch(scope,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    
    # ── Rerun check: warn before overwriting existing interpolated results ──
    df        <- concentration_calc_df()
    is_rerun  <- get_status(df, scope, "interpolated") %in%
      c("completed", "partially completed")
    
    if (is_rerun) {
      showModal(modalDialog(
        title = tagList(
          tags$i(class = "fa fa-exclamation-triangle",
                 style = "color:#ffc107; margin-right:8px;"),
          "Confirm Interpolated Rerun"
        ),
        tagList(
          p(paste0(
            "Interpolated concentrations have already been calculated for ",
            scope_label, "."
          )),
          p(strong("Running again will overwrite all existing interpolated results for this scope.")),
          p("Are you sure you want to continue?")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            paste0("confirm_interp_rerun_", scope),
            label = tagList(
              tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
              "Yes, Rerun Interpolated"
            ),
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
      return()   # wait for confirmation observer below
    }
    
    # ── Not a rerun — proceed directly ──
    # .run_interpolated(scope, scope_label, session)
    .run_interpolated(
      scope       = scope,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      current_user = currentuser(),
      scope_label = scope_label,
      session     = session
    )
  })
})

# ============================================================================
# Interpolated rerun confirmation observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  observeEvent(input[[paste0("confirm_interp_rerun_", s)]], ignoreInit = TRUE, {
    removeModal()
    
    scope_label <- switch(s,
                          "study"      = "all experiments",
                          "experiment" = "current experiment",
                          "plate"      = "current plate"
    )
    
    .run_interpolated(
      scope       = s,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      current_user = currentuser(),
      scope_label = scope_label,
      session     = session
    )
  })
})

.run_interpolated <- function(scope, study, experiment, plate, proj,
                              current_user, scope_label, session) {
  is_batch_processing(TRUE)
  
  # ── Progress setup ──
  prog_file <- tempfile(pattern = "interp_progress_", fileext = ".txt")
  writeLines(paste0("Starting Interpolated...\nScope: ", scope_label), prog_file)
  interp_progress_file(prog_file)
  interp_progress_msg(paste0("Starting Interpolated...\nScope: ", scope_label))
  interp_pending_scopes(c(
    interp_pending_scopes(),
    list(c(scope = scope, method = "interpolated"))
  ))
  concentrationUIRefresher(concentrationUIRefresher() + 1)
  
  showNotification(
    id = "batch_sc_fit_notify",
    HTML(
      paste0(
        "<div class='big-notification'>",
        "Starting standard curves:<br>",
        "interpolated concentrations<br>",
        "for ", scope_label, "<span class='dots'></span>",
        "</div>"
      )
    ),
    duration = NULL
  )
  
  # ── ALL DATA LOADING ON MAIN THREAD ──
  headers <- fetch_db_header_experiments(
    study_accession = study, conn = conn
  )
  exp_list <- switch(scope,
                     "study"      = unique(headers$experiment_accession),
                     "experiment" = ,
                     "plate"      = experiment
  )
  
  loaded_data_list <- lapply(
    stats::setNames(exp_list, exp_list),
    function(exp) pull_data(
      study_accession      = study,
      experiment_accession = exp,
      project_id           = proj,
      conn                 = conn
    )
  )
  
  # ── Compute dil_series SE on FULL unfiltered standards (all plates) ──
  # Must happen BEFORE plate-scope filtering so we get proper cross-plate
  # replication (n >= min_reps) and valid SE estimates for FDA LOQ computation.
  all_standards_full <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
  response_var <- loaded_data_list[[exp_list[1]]]$response_var
  
  dil_series_se_table_batch <- compute_dil_series_se(
    standards_data = all_standards_full,
    response_col   = response_var,
    dilution_col   = "dilution",
    plate_col      = "plate_nom",
    grouping_cols  = c("project_id",
                       "study_accession",
                       "experiment_accession",
                       "source_nom",
                       "antigen",
                       "feature"),
    min_reps = 2,
    verbose  = FALSE
  )
  
  # ── NOW apply plate scope filter to loaded_data_list ──
  if (scope == "plate") {
    loaded_data_list <- lapply(loaded_data_list, function(x) {
      for (tbl in c("plates", "standards", "samples", "blanks")) {
        if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0)
          x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
      }
      x
    })
  }
  
  all_standards <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
  
  se_antigen_table_batch <- compute_antigen_se_table(
    standards_data = all_standards,
    response_col   = response_var,
    dilution_col   = "dilution",
    plate_col      = "plate",
    grouping_cols  = c("project_id", "study_accession", "experiment_accession",
                       "source_nom", "antigen", "feature"),
    verbose        = FALSE
  )
  
  study_params_batch <- fetch_study_parameters(
    study_accession = study,
    param_user      = current_user,
    param_group     = "standard_curve_options",
    project_id      = proj,
    conn            = conn
  )
  
  model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
  
  antigen_list_res       <- build_antigen_list(exp_list, loaded_data_list, study)
  antigen_plate_list_res <- build_antigen_plate_list(antigen_list_res, loaded_data_list)
  prepped_data_list_res  <- prep_plate_data_batch(
    antigen_plate_list_res, study_params_batch, verbose = FALSE
  )
  antigen_plate_list_res$antigen_plate_list_ids <-
    prepped_data_list_res$antigen_plate_name_list
  n_total <- length(prepped_data_list_res$antigen_plate_name_list)
  
  db_conn_args <- get_db_connection_args()
  
  # ── FUTURE: Only fitting + DB writes ──
  future_promise <- future::future({
    bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
    on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
    
    batch_fit_res <- fit_experiment_plate_batch(
      prepped_data_list_res   = prepped_data_list_res,
      antigen_plate_list_res  = antigen_plate_list_res,
      model_names             = model_names,
      study_params            = study_params_batch,
      se_antigen_table        = se_antigen_table_batch,
      dil_series_se_table     = dil_series_se_table_batch,
      prog_file               = prog_file,
      dil_series_response_col = response_var,
      verbose                 = FALSE
    )
    
    batch_outputs <- create_batch_fit_outputs(
      batch_fit_res, antigen_plate_list_res
    )
    batch_outputs <- process_batch_outputs(
      batch_outputs, response_var, proj
    )
    
    # ── Debug: check FDA LOQ columns ──
    tryCatch(writeLines(
      paste0("Interpolated: saving best_glance_all...\nScope: ", scope_label),
      prog_file), error = function(e) NULL)
    
    message("[debug] best_glance_all columns: ",
            paste(names(batch_outputs$best_glance_all), collapse = ", "))
    
    lloq_check_cols <- grep("lloq_fda2018|uloq_fda2018",
                            names(batch_outputs$best_glance_all), value = TRUE)
    message("[debug] lloq cols present in best_glance_all: ",
            if (length(lloq_check_cols) == 0) "NONE"
            else paste(lloq_check_cols, collapse = ", "))
    
    if (length(lloq_check_cols) > 0) {
      message("[debug] lloq values (first 3 rows):")
      print(batch_outputs$best_glance_all[
        1:min(3, nrow(batch_outputs$best_glance_all)),
        lloq_check_cols, drop = FALSE])
      message("[debug] lloq NA counts:")
      print(colSums(is.na(
        batch_outputs$best_glance_all[, lloq_check_cols, drop = FALSE]
      )))
    }
    

    # ── Save best_glance_all first (parent table) ──
    upsert_best_curve(
      conn = bg_conn, df = batch_outputs$best_glance_all,
      schema = "madi_results", table = "best_glance_all",
      notify = NULL, shiny_mode = FALSE
    )
    
    # ── Fetch glance lookup for FK joins ──
    study_to_save   <- unique(batch_outputs$best_glance_all$study_accession)
    project_to_save <- unique(batch_outputs$best_glance_all$project_id)
    exp_list_sql    <- paste0("'", paste(exp_list, collapse = "','"), "'")
    
    glance_lookup <- DBI::dbGetQuery(bg_conn, glue::glue(
      "SELECT best_glance_all_id, project_id, study_accession, experiment_accession,
              plateid, plate, nominal_sample_dilution, source, antigen, feature, wavelength
       FROM madi_results.best_glance_all
       WHERE project_id = {project_to_save}
         AND study_accession = '{study_to_save}'
         AND experiment_accession IN ({exp_list_sql});"
    ))
    glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
    
    keys <- c("project_id", "study_accession", "experiment_accession", "plateid",
              "plate", "nominal_sample_dilution", "source", "antigen", "feature", "wavelength")
    
    # ── Normalize wavelength in glance_lookup AND all child tables ──
    glance_lookup$wavelength <- normalize_wavelength(glance_lookup$wavelength)
    
    for (tbl_name in c("best_pred_all", "best_sample_se_all", "best_standard_all",
                       "best_plate_all", "best_tidy_all")) {
      if (!is.null(batch_outputs[[tbl_name]]) &&
          "wavelength" %in% names(batch_outputs[[tbl_name]])) {
        batch_outputs[[tbl_name]]$wavelength <-
          normalize_wavelength(batch_outputs[[tbl_name]]$wavelength)
      }
    }
    
    message(sprintf("[save] glance_lookup: %d rows", nrow(glance_lookup)))
    message(sprintf("[save] glance_lookup wavelengths: %s",
                    paste(unique(glance_lookup$wavelength), collapse = ", ")))
    
    # ── FK joins: attach best_glance_all_id to each child table ──
    if (!is.null(batch_outputs$best_pred_all) &&
        nrow(batch_outputs$best_pred_all) > 0) {
      n_before <- nrow(batch_outputs$best_pred_all)
      batch_outputs$best_pred_all <- dplyr::inner_join(
        batch_outputs$best_pred_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_pred_all)
      message(sprintf("[save] best_pred_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_pred_all rows — likely key mismatch.")
    }
    
    if (!is.null(batch_outputs$best_sample_se_all) &&
        nrow(batch_outputs$best_sample_se_all) > 0) {
      n_before <- nrow(batch_outputs$best_sample_se_all)
      batch_outputs$best_sample_se_all <- dplyr::inner_join(
        batch_outputs$best_sample_se_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_sample_se_all)
      message(sprintf("[save] best_sample_se_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_sample_se_all rows — likely key mismatch.")
    }
    
    if (!is.null(batch_outputs$best_standard_all) &&
        nrow(batch_outputs$best_standard_all) > 0) {
      n_before <- nrow(batch_outputs$best_standard_all)
      batch_outputs$best_standard_all <- dplyr::inner_join(
        batch_outputs$best_standard_all, glance_lookup, by = keys
      )
      n_after <- nrow(batch_outputs$best_standard_all)
      message(sprintf("[save] best_standard_all FK join: %d -> %d rows", n_before, n_after))
      if (n_after == 0 && n_before > 0)
        message("[save] WARNING: FK join dropped ALL best_standard_all rows — likely key mismatch.")
    }
    
    # ── Save all child tables ──
    for (pair in list(
      list(df = "best_plate_all",     table = "best_plate_all"),
      list(df = "best_tidy_all",      table = "best_tidy_all"),
      list(df = "best_pred_all",      table = "best_pred_all"),
      list(df = "best_sample_se_all", table = "best_sample_se_all"),
      list(df = "best_standard_all",  table = "best_standard_all")
    )) {
      tryCatch(writeLines(
        paste0("Interpolated: saving ", pair$table, "...\nScope: ", scope_label),
        prog_file), error = function(e) NULL)
      
      upsert_best_curve(
        conn = bg_conn, df = batch_outputs[[pair$df]],
        schema = "madi_results", table = pair$table,
        notify = NULL, shiny_mode = FALSE
      )
    }
    
    list(ok = TRUE, n_curves = n_total, scope_label = scope_label)
  }, seed = TRUE)
  
  removeNotification("batch_sc_fit_notify")
  
  progress_poller <- reactivePoll(
    intervalMillis = 2000, session = session,
    checkFunc = function() {
      pf <- interp_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(0)
      file.info(pf)$mtime
    },
    valueFunc = function() {
      pf <- interp_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(NULL)
      tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
    }
  )
  
  progress_observer <- observe({
    msg <- progress_poller()
    if (!is.null(msg) && nzchar(msg)) interp_progress_msg(msg)
  })
  
  .cleanup_interp <- function(label, type = "message", duration = 10) {
    progress_observer$destroy()
    pf <- interp_progress_file()
    if (!is.null(pf) && file.exists(pf)) file.remove(pf)
    interp_progress_file(NULL)
    interp_progress_msg(NULL)
    .remove_pending(interp_pending_scopes, scope, "interpolated")
    showNotification(label, type = type, duration = duration)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
  }
  
  promises::then(
    future_promise,
    onFulfilled = function(result) {
      .cleanup_interp(
        paste0("Interpolated completed for ", result$scope_label,
               " (", result$n_curves, " curves).")
      )
    },
    onRejected = function(err) {
      .cleanup_interp(
        paste0("Interpolated error: ", conditionMessage(err)),
        type = "error", duration = 15
      )
      message("Interpolated future rejected: ", conditionMessage(err))
    }
  )
  
  NULL
}
# 
# .run_interpolated <- function(scope, study, experiment, plate, proj,
#                               current_user, scope_label, session) {
#   is_batch_processing(TRUE)
#   
#   # ── Progress setup (same as before) ──
#   prog_file <- tempfile(pattern = "interp_progress_", fileext = ".txt")
#   writeLines(paste0("Starting Interpolated...\nScope: ", scope_label), prog_file)
#   interp_progress_file(prog_file)
#   interp_progress_msg(paste0("Starting Interpolated...\nScope: ", scope_label))
#   interp_pending_scopes(c(
#     interp_pending_scopes(),
#     list(c(scope = scope, method = "interpolated"))
#   ))
#   concentrationUIRefresher(concentrationUIRefresher() + 1)
#   
#   showNotification(
#     id = "batch_sc_fit_notify",
#     HTML(
#       paste0(
#         "<div class='big-notification'>",
#         "Starting standard curves:<br>",
#         "interpolated concentrations<br>",
#         "for ", scope_label, "<span class='dots'></span>",
#         "</div>"
#       )
#     ),
#     duration = NULL
#   )
#   
#   # ── ALL DATA LOADING ON MAIN THREAD (mirrors .launch_mcmc step 4) ──
#   headers <- fetch_db_header_experiments(
#     study_accession = study, conn = conn
#   )
#   exp_list <- switch(scope,
#                      "study"      = unique(headers$experiment_accession),
#                      "experiment" = ,
#                      "plate"      = experiment
#   )
#   
#   loaded_data_list <- lapply(
#     stats::setNames(exp_list, exp_list),
#     function(exp) pull_data(
#       study_accession      = study,
#       experiment_accession = exp,
#       project_id           = proj,
#       conn                 = conn
#     )
#   )
#   
#   # ── Compute dil_series SE on FULL unfiltered standards (all plates) ──
#   # Must happen BEFORE plate-scope filtering so we get proper cross-plate
#   # replication (n >= min_reps) and valid SE estimates for FDA LOQ computation.
#   all_standards_full <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
#   response_var <- loaded_data_list[[exp_list[1]]]$response_var
#   
#   dil_series_se_table_batch <- compute_dil_series_se(
#     standards_data = all_standards_full,
#     response_col   = response_var,
#     dilution_col   = "dilution",
#     plate_col      = "plate_nom",
#     grouping_cols  = c("project_id",
#                        "study_accession",
#                        "experiment_accession",
#                        "source_nom",
#                        "antigen",
#                        "feature"),
#     min_reps = 2,
#     verbose  = FALSE
#   )
#   
#   # ── NOW apply plate scope filter to loaded_data_list ──
#   if (scope == "plate") {
#     loaded_data_list <- lapply(loaded_data_list, function(x) {
#       for (tbl in c("plates", "standards", "samples", "blanks")) {
#         if (!is.null(x[[tbl]]) && nrow(x[[tbl]]) > 0)
#           x[[tbl]] <- x[[tbl]][x[[tbl]]$plate_nom == plate, , drop = FALSE]
#       }
#       x
#     })
#   }
#   
#   all_standards <- do.call(rbind, lapply(loaded_data_list, `[[`, "standards"))
#   
#   se_antigen_table_batch <- compute_antigen_se_table(
#     standards_data = all_standards,
#     response_col   = response_var,
#     dilution_col   = "dilution",
#     plate_col      = "plate",
#     grouping_cols  = c("project_id","study_accession", "experiment_accession",
#                        "source_nom", "antigen", "feature"),
#     verbose        = FALSE
#   )
#   
#   study_params_batch <- fetch_study_parameters(
#     study_accession = study,
#     param_user      = current_user,
#     param_group     = "standard_curve_options",
#     project_id      = proj,
#     conn            = conn
#   )
#   
#   model_names <- c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
#   
#   antigen_list_res       <- build_antigen_list(exp_list, loaded_data_list, study)
#   antigen_plate_list_res <- build_antigen_plate_list(antigen_list_res, loaded_data_list)
#   prepped_data_list_res  <- prep_plate_data_batch(
#     antigen_plate_list_res, study_params_batch, verbose = FALSE
#   )
#   antigen_plate_list_res$antigen_plate_list_ids <-
#     prepped_data_list_res$antigen_plate_name_list
#   n_total <- length(prepped_data_list_res$antigen_plate_name_list)
#   
#   db_conn_args <- get_db_connection_args()
#   
#   # ── FUTURE: Only fitting + DB writes ──
#   future_promise <- future::future({
#     bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
#     on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
#     
#     batch_fit_res <- fit_experiment_plate_batch(
#       prepped_data_list_res  = prepped_data_list_res,
#       antigen_plate_list_res = antigen_plate_list_res,
#       model_names            = model_names,
#       study_params           = study_params_batch,
#       se_antigen_table       = se_antigen_table_batch,
#       dil_series_se_table    = dil_series_se_table_batch,
#       prog_file              = prog_file,
#       dil_series_response_col = response_var,
#       verbose                = FALSE
#     )
#     
#     batch_outputs <- create_batch_fit_outputs(
#       batch_fit_res, antigen_plate_list_res
#     )
#     batch_outputs <- process_batch_outputs(
#       batch_outputs, response_var, proj
#     )
#     
#     tryCatch(writeLines(
#       paste0("Interpolated: saving best_glance_all...\nScope: ", scope_label),
#       prog_file), error = function(e) NULL)
#     
#     message("[debug] best_glance_all columns: ",
#             paste(names(batch_outputs$best_glance_all), collapse = ", "))
#     
#     lloq_check_cols <- grep("lloq_fda2018|uloq_fda2018", names(batch_outputs$best_glance_all), value = TRUE)
#     message("[debug] lloq cols present in best_glance_all: ",
#             if (length(lloq_check_cols) == 0) "NONE" else paste(lloq_check_cols, collapse = ", "))
#     
#     if (length(lloq_check_cols) > 0) {
#       message("[debug] lloq values (first 3 rows):")
#       print(batch_outputs$best_glance_all[1:min(3, nrow(batch_outputs$best_glance_all)),
#                                           lloq_check_cols, drop = FALSE])
#       message("[debug] lloq NA counts:")
#       print(colSums(is.na(batch_outputs$best_glance_all[, lloq_check_cols, drop = FALSE])))
#     }
#     
#     ## debug in future needs to save 
#     saveRDS(batch_outputs$best_glance_all, "best_glance_debug.rds")
#     
#     upsert_best_curve(
#       conn = bg_conn, df = batch_outputs$best_glance_all,
#       schema = "madi_results", table = "best_glance_all",
#       notify = NULL, shiny_mode = FALSE
#     )
#     
#     study_to_save   <- unique(batch_outputs$best_glance_all$study_accession)
#     project_to_save <- unique(batch_outputs$best_glance_all$project_id)
#     
#     exp_list_sql <- paste0("'", paste(exp_list, collapse = "','"), "'")
#     glance_lookup <- DBI::dbGetQuery(bg_conn, glue::glue(
#       "SELECT best_glance_all_id, project_id, study_accession, experiment_accession,
#               plateid, plate, nominal_sample_dilution, source, antigen, feature, wavelength
#        FROM madi_results.best_glance_all
#        WHERE project_id = {project_to_save}
#          AND study_accession = '{study_to_save}'
#          AND experiment_accession IN ({exp_list_sql});"
#     ))
#     glance_lookup$best_glance_all_id <- as.integer(glance_lookup$best_glance_all_id)
#     
#     keys <- c("project_id","study_accession", "experiment_accession", "plateid",
#               "plate", "nominal_sample_dilution", "source", "antigen", "feature", "wavelength")
#     
#     glance_lookup$wavelength <- normalize_wavelength(glance_lookup$wavelength)
#     
#     message(sprintf("[save] glance_lookup: %d rows, keys: %s",
#                     nrow(glance_lookup), paste(keys, collapse = ", ")))
#     message(sprintf("[save] glance_lookup wavelengths: %s",
#                     paste(unique(glance_lookup$wavelength), collapse = ", ")))
#     
#     if (!is.null(batch_outputs$best_pred_all) && nrow(batch_outputs$best_pred_all) > 0) {
#       n_before <- nrow(batch_outputs$best_pred_all)
#       batch_outputs$best_pred_all <- dplyr::inner_join(batch_outputs$best_pred_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_pred_all)
#       message(sprintf("[save] best_pred_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_pred_all rows — likely wavelength mismatch.")
#     }
#     
#     if (!is.null(batch_outputs$best_sample_se_all) && nrow(batch_outputs$best_sample_se_all) > 0) {
#       n_before <- nrow(batch_outputs$best_sample_se_all)
#       batch_outputs$best_sample_se_all <- dplyr::inner_join(batch_outputs$best_sample_se_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_sample_se_all)
#       message(sprintf("[save] best_sample_se_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_sample_se_all rows — likely wavelength mismatch.")
#     }
#     
#     if (!is.null(batch_outputs$best_standard_all) && nrow(batch_outputs$best_standard_all) > 0) {
#       n_before <- nrow(batch_outputs$best_standard_all)
#       batch_outputs$best_standard_all <- dplyr::inner_join(batch_outputs$best_standard_all, glance_lookup, by = keys)
#       n_after <- nrow(batch_outputs$best_standard_all)
#       message(sprintf("[save] best_standard_all FK join: %d -> %d rows", n_before, n_after))
#       if (n_after == 0 && n_before > 0)
#         message("[save] WARNING: FK join dropped ALL best_standard_all rows — likely wavelength mismatch.")
#     }
#     
#     for (tbl_name in c("best_pred_all", "best_sample_se_all", "best_standard_all")) {
#       batch_outputs[[tbl_name]] <- dplyr::inner_join(
#         batch_outputs[[tbl_name]], glance_lookup, by = keys
#       )
#     }
#     
#     for (pair in list(
#       list(df = "best_plate_all",     table = "best_plate_all"),
#       list(df = "best_tidy_all",      table = "best_tidy_all"),
#       list(df = "best_pred_all",      table = "best_pred_all"),
#       list(df = "best_sample_se_all", table = "best_sample_se_all"),
#       list(df = "best_standard_all",  table = "best_standard_all")
#     )) {
#       tryCatch(writeLines(
#         paste0("Interpolated: saving ", pair$table, "...\nScope: ", scope_label),
#         prog_file), error = function(e) NULL)
#       
#       upsert_best_curve(
#         conn = bg_conn, df = batch_outputs[[pair$df]],
#         schema = "madi_results", table = pair$table,
#         notify = NULL, shiny_mode = FALSE
#       )
#     }
#     
#     list(ok = TRUE, n_curves = n_total, scope_label = scope_label)
#   }, seed = TRUE)
#   
#   removeNotification("batch_sc_fit_notify")
#   
#   progress_poller <- reactivePoll(
#     intervalMillis = 2000, session = session,
#     checkFunc = function() {
#       pf <- interp_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(0)
#       file.info(pf)$mtime
#     },
#     valueFunc = function() {
#       pf <- interp_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(NULL)
#       tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
#     }
#   )
#   
#   progress_observer <- observe({
#     msg <- progress_poller()
#     if (!is.null(msg) && nzchar(msg)) interp_progress_msg(msg)
#   })
#   
#   .cleanup_interp <- function(label, type = "message", duration = 10) {
#     progress_observer$destroy()
#     pf <- interp_progress_file()
#     if (!is.null(pf) && file.exists(pf)) file.remove(pf)
#     interp_progress_file(NULL)
#     interp_progress_msg(NULL)
#     .remove_pending(interp_pending_scopes, scope, "interpolated")
#     showNotification(label, type = type, duration = duration)
#     concentrationUIRefresher(concentrationUIRefresher() + 1)
#     is_batch_processing(FALSE)
#   }
#   
#   promises::then(
#     future_promise,
#     onFulfilled = function(result) {
#       .cleanup_interp(
#         paste0("Interpolated completed for ", result$scope_label,
#                " (", result$n_curves, " curves).")
#       )
#     },
#     onRejected = function(err) {
#       .cleanup_interp(
#         paste0("Interpolated error: ", conditionMessage(err)),
#         type = "error", duration = 15
#       )
#       message("Interpolated future rejected: ", conditionMessage(err))
#     }
#   )
#   
#   NULL
# }

# ============================================================================
# .launch_mcmc — async helper (future + promises + progress polling)
# Called by both the initial MCMC observer and the rerun confirmation observer.
# ============================================================================
.launch_mcmc <- function(scope, study, experiment, plate, proj,
                         scope_label, session) {
  
  is_batch_processing(TRUE)
  
  # ── 1. Temp file for IPC progress messages ──────────────────────────────
  prog_file <- tempfile(pattern = "mcmc_progress_", fileext = ".txt")
  #writeLines("Starting MCMC Robust...", prog_file)
  mcmc_progress_file(prog_file)
  mcmc_progress_msg(paste0(
    "Running MCMC Robust\n",
    "Scope: ",      scope_label, "\n",
    "Study: ",      study,       "\n",
    "Experiment: ", experiment
  ))
  
  # ── 2. Inject "pending" into in-memory overlay so UI updates immediately ─
  current_pending <- mcmc_pending_scopes()
  mcmc_pending_scopes(c(
    current_pending,
    list(list(scope = scope, method = "mcmc_robust"))
  ))
  concentrationUIRefresher(concentrationUIRefresher() + 1)
  
  # ── 3. Persistent running notification ──────────────────────────────────
  showNotification(
    id  = "mcmc_calc_notify",
    div(class = "big-notification",
        paste0("Starting MCMC Robust for ", scope_label, "...")),
    duration = 10
  )
  
  # ── 4. Fetch data snapshots in the main session before handing off ───────
  best_glance_snapshot <- tryCatch(
    fetch_best_glance_mcmc(
      study_accession = study,
      project_id      = proj,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  if (is.null(best_glance_snapshot) || nrow(best_glance_snapshot) == 0) {
    showNotification("No fitted curves found.", type = "warning")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  best_glance_snapshot <- filter_glance_scope(
    best_glance_snapshot, scope, experiment, plate
  )
  
  if (nrow(best_glance_snapshot) == 0) {
    showNotification("No fitted curves found for this scope.", type = "warning")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  id_set  <- best_glance_snapshot$best_glance_all_id
  study <- study
  proj <- proj
  n_total <- length(id_set)
  
  message("IDs: ", paste(id_set, collapse = ", "))
  message("Study: ", study)
  message("Project: ", proj)
  
  
  combined_df_snapshot <- tryCatch(
    fetch_combined_mcmc(
      study_accession = study,
      project_id      = proj,
      best_glance_ids = id_set,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  if (is.null(combined_df_snapshot) || nrow(combined_df_snapshot) == 0) {
    showNotification("No prediction data found for MCMC.", type = "error")
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    mcmc_progress_msg(NULL)
    mcmc_progress_file(NULL)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
    return()
  }
  
  # ── NEW: Fetch tidy params snapshot for all curves in scope ─────────────
  tidy_params_snapshot <- tryCatch(
    fetch_tidy_params_bulk(
      study_accession = study,
      project_id      = proj,
      best_glance_snapshot = best_glance_snapshot,
      conn            = conn
    ),
    error = function(e) {
      message("Warning: could not fetch tidy params — error was: ", e$message)
      NULL
    }
  )
  
  
  # Snapshot DB connection args so the future can open its own connection
  db_conn_args <- get_db_connection_args()
  
  # ── 5. Launch future (runs in a separate process) ────────────────────────
  future_promise <- future::future({
    
    bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
    on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
    
    results     <- vector("list", length(id_set))
    best_glance <- best_glance_snapshot
    
    for (i in seq_along(id_set)) {
      
      id  <- id_set[i]
      row <- best_glance[best_glance$best_glance_all_id == id, ]
      
      # Write progress so the main session poller can read it
      progress_text <- paste0(
        "MCMC Robust: ", i, " / ", n_total,       "\n",
        "Study:      ", row$study_accession,       "\n",
        "Experiment: ", row$experiment_accession,  "\n",
        "Plate:      ", row$plate_nom,             "\n",
        "Antigen:    ", row$antigen,               "\n",
        "Model:      ", row$model_name
      )
      tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
      message(progress_text)
      
      curve_df <- combined_df_snapshot[
        combined_df_snapshot$best_glance_all_id == id, ]
      pred_df  <- curve_df[curve_df$mcmc_set == "pred_se", ]
      if (nrow(pred_df) == 0) next
      
      
      # ── NEW: Extract tidy params for this curve ────────────────────────
      if (is.null(tidy_row) || nrow(tidy_row) == 0) {
        message(paste0(
          "[MCMC] Warning: no tidy params found for ID ", id,
          " (", row$study_accession,
          " / ", row$experiment_accession,
          " / ", row$antigen, ")",
          " — running without parameter uncertainty."
        ))
        tidy_row <- NULL
      }
      
      # ── Warn if tidy params missing ────────────────────────────────────
      if (is.null(tidy_row) || nrow(tidy_row) == 0) {
        message(paste0(
          "[MCMC] Warning: no tidy params found for ID",
          id, row$study_accession, row$experiment_accession, row$antigen,
          "running without parameter uncertainty."
        ))
        tidy_row <- NULL
      }
      
      res <- tryCatch(
        run_jags_predicted_concentration(
          glance_row   = row,
          best_pred_df = pred_df,
          sample_df    = curve_df,
          response_col = "assay_response",
          tidy_df = tidy_row,
          verbose      = TRUE
        ),
        error = function(e) {
          message("JAGS error for ID ", id, ": ", e$message)
          NULL
        }
      )
      
      if (!is.null(res)) {
        if (!"mcmc_set" %in% names(res)) {
          res$mcmc_set <- curve_df$mcmc_set[match(res$row_id, curve_df$row_id)]
        }
        results[[i]] <- res
      }
    }
    
    # Combine all results
    results_df <- do.call(rbind, Filter(Negate(is.null), results))
    if (is.null(results_df) || nrow(results_df) == 0) stop("MCMC produced no results.")
    
    result_pred_all   <- results_df[results_df$mcmc_set == "pred_se",   ]
    result_sample_all <- results_df[results_df$mcmc_set == "sample_se", ]
    
    result_sample_all$final_robust_concentration <-
      result_sample_all$dilution * result_sample_all$raw_robust_concentration
    
    best_glance$last_concentration_calc_method[
      best_glance$best_glance_all_id %in% id_set
    ] <- "mcmc_robust"
    
    result_pred_all2   <- process_jag_result(result_pred_all,   df_name = "pred_se")
    result_sample_all2 <- process_jag_result(result_sample_all, df_name = "sample_se")
    
    update_combined_mcmc_bulk(
      pred_all_mcmc        = result_pred_all2,
      sample_all_mcmc      = result_sample_all2,
      best_glance_complete = best_glance,
      conn                 = bg_conn
    )
    
    list(ok = TRUE, n_curves = nrow(best_glance), scope_label = scope_label)
    
  }, seed = TRUE)
  
  # ── 6. Poll the progress file every 2 s while the future runs ───────────
  progress_poller <- reactivePoll(
    intervalMillis = 2000,
    session        = session,
    
    checkFunc = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(0)
      file.info(pf)$mtime
    },
    
    valueFunc = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(NULL)
      tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
    }
  )
  
  # Keep the poller alive by observing it
  progress_observer <- observe({
    msg <- progress_poller()
    if (!is.null(msg) && nzchar(msg)) mcmc_progress_msg(msg)
  })
  
  # ── 7. Handle promise resolution ────────────────────────────────────────
  promises::then(
    future_promise,
    
    onFulfilled = function(result) {
      progress_observer$destroy()
      
      pf <- mcmc_progress_file()
      if (!is.null(pf) && file.exists(pf)) file.remove(pf)
      mcmc_progress_file(NULL)
      mcmc_progress_msg(NULL)
      
      .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
      
      showNotification(
        paste0("MCMC Robust completed for ", result$scope_label, "."),
        type = "message", duration = 10
      )
      
      concentrationUIRefresher(concentrationUIRefresher() + 1)
      is_batch_processing(FALSE)
    },
    
    onRejected = function(err) {
      progress_observer$destroy()
      
      pf <- mcmc_progress_file()
      if (!is.null(pf) && file.exists(pf)) file.remove(pf)
      mcmc_progress_file(NULL)
      mcmc_progress_msg(NULL)
      
      .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
      
      showNotification(
        paste0("MCMC Robust error: ", conditionMessage(err)),
        type = "error", duration = 15
      )
      message("MCMC future rejected: ", conditionMessage(err))
      
      concentrationUIRefresher(concentrationUIRefresher() + 1)
      is_batch_processing(FALSE)
    }
  )
  
  NULL
}


# ============================================================================
# MCMC Robust observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  # ── Initial click ──
  observeEvent(input[[paste0("run_mcmc_calc_", s)]], ignoreInit = TRUE, {
    
    scope      <- s
    study      <- input$readxMap_study_accession
    experiment <- input$readxMap_experiment_accession
    plate      <- input$sc_plate_select
    proj       <- userWorkSpaceID()
    
    if (is_batch_processing()) {
      showNotification("Batch processing is already running.",
                       type = "warning", duration = 10)
      return()
    }
    
    req(input$qc_component == "Standard Curve",
        study != "Click here", experiment != "Click here")
    
    df <- concentration_calc_df()
    
    if (!(get_status(df, scope, "interpolated") %in%
          c("completed", "partially completed"))) {
      showNotification("Interpolated concentrations must be completed first.",
                       type = "error", duration = 10)
      return()
    }
    
    is_rerun <- get_status(df, scope, "mcmc_robust") %in%
      c("completed", "partially completed")
    
    scope_label <- c(
      study      = "all experiments",
      experiment = "current experiment",
      plate      = "current plate"
    )[scope]
    
    if (is_rerun) {
      # Show confirmation modal before overwriting
      showModal(modalDialog(
        title = tagList(
          tags$i(class = "fa fa-exclamation-triangle",
                 style = "color:#ffc107; margin-right:8px;"),
          "Confirm MCMC Rerun"
        ),
        tagList(
          p(paste0(
            "MCMC Robust concentrations have already been calculated for ",
            scope_label, "."
          )),
          p(strong("Running again will overwrite all existing MCMC results for this scope.")),
          p("Are you sure you want to continue?")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            paste0("confirm_mcmc_rerun_", scope),
            label = tagList(
              tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
              "Yes, Rerun MCMC"
            ),
            class = "btn-warning"
          )
        ),
        easyClose = TRUE
      ))
      return()   # wait for confirmation observer below
    }
    
    # Not a rerun — proceed immediately
    .launch_mcmc(
      scope       = scope,
      study       = study,
      experiment  = experiment,
      plate       = plate,
      proj        = proj,
      scope_label = scope_label,
      session     = session
    )
  })
})


# ============================================================================
# MCMC Rerun confirmation observers — one per scope
# ============================================================================
lapply(c("study", "experiment", "plate"), function(s) {
  
  observeEvent(input[[paste0("confirm_mcmc_rerun_", s)]], ignoreInit = TRUE, {
    removeModal()
    
    scope_label <- c(
      study      = "all experiments",
      experiment = "current experiment",
      plate      = "current plate"
    )[s]
    
    .launch_mcmc(
      scope       = s,
      study       = input$readxMap_study_accession,
      experiment  = input$readxMap_experiment_accession,
      plate       = input$sc_plate_select,
      proj        = userWorkSpaceID(),
      scope_label = scope_label,
      session     = session
    )
  })
})


# ============================================================================
# BAYESIAN 5PL — Background execution observer
#
# Triggered ONLY by an explicit "Calculate Bayes" button click.
# This avoids the race condition where asynchronous dropdown resolution
# causes input$sc_antigen_select etc. to be NULL/NA when the observer
# first fires, producing "missing value where TRUE/FALSE needed".
#
# Scope of the fit: ALL plates for the selected Antigen + Experiment.
# Stan's hierarchical model borrows strength across plates and must see
# them all at once. The user then uses bayes_view_plate to choose which
# plate's plot to display.
# ============================================================================
observeEvent(input$btn_run_bayes, ignoreInit = TRUE, {

  # ── Snapshot inputs immediately ──────────────────────────────────────────
  study_val   <- input$readxMap_study_accession
  exp_val     <- input$readxMap_experiment_accession
  ant_val     <- input$sc_antigen_select
  src_val     <- input$sc_source_select
  snap_n_chains          <- max(1L, as.integer(input$bayes_n_chains %||% 4L))
  snap_n_iter            <- max(500L, as.integer(input$bayes_n_iter %||% 1000L))
  snap_prozone_threshold <- 0.1  # kept for StanAssay$new() signature; apply_prozone = FALSE

  message(
    "[btn_run_bayes] fired — ",
    "study=", deparse(study_val), " | ",
    "exp=",   deparse(exp_val),   " | ",
    "ant=",   deparse(ant_val)
  )

  # NA-safe guards
  if (is.null(study_val)  || !nzchar(study_val)  || study_val  == "Click here") {
    showNotification("Please select a Study before running Bayesian analysis.", type = "warning")
    return()
  }
  if (is.null(exp_val) || !nzchar(exp_val) || exp_val == "Click here") {
    showNotification("Please select an Experiment before running Bayesian analysis.", type = "warning")
    return()
  }
  if (is.null(ant_val) || length(ant_val) == 0L || !nzchar(ant_val)) {
    showNotification("Please select an Antigen before running Bayesian analysis.", type = "warning")
    return()
  }

  # Guard: do not run while already running
  if (isTRUE(bayes_state$status == "running")) {
    showNotification(
      "Bayesian ensemble model is already running. Please wait for it to complete.",
      type = "warning", duration = 6
    )
    return()
  }

  # --- Check stanassay is available ---
  if (!exists("StanAssay")) {
    bayes_state$status    <- "error"
    bayes_state$error_msg <- paste0(
      "stanassay package is not loaded. ",
      "Ensure it is installed via: R CMD INSTALL /Users/hardik/Documents/work/stanassay"
    )
    return()
  }

  # --- Snapshot values needed in the body ---
  snap_study      <- study_val
  snap_experiment <- exp_val
  snap_antigen    <- ant_val
  snap_source     <- src_val %||% ""
  # Extract raw DB source from source_nom (strip "|wavelength_nm" suffix for ELISA)
  snap_source_raw <- sub("\\|.*$", "", snap_source)

  # Validate wavelength against DB — Shiny retains the last input$bayes_selected_wavelength
  # value even when the widget is not rendered (e.g. after switching from ELISA to xMAP).
  # Re-query to confirm this antigen actually has non-NULL wavelengths before applying filter.
  has_wavelength_data <- tryCatch({
    DBI::dbGetQuery(conn,
      "SELECT COUNT(*) AS n FROM madi_results.xmap_standard
       WHERE study_accession      = $1
         AND experiment_accession = $2
         AND antigen              = $3
         AND wavelength IS NOT NULL",
      params = list(snap_study, snap_experiment, snap_antigen)
    )$n[[1]] > 0L
  }, error = function(e) FALSE)

  snap_wavelength <- if (has_wavelength_data) input$bayes_selected_wavelength else NULL

  # Use wavelength presence to detect assay type (more reliable than parsing source string)
  snap_assay_type <- if (!is.null(snap_wavelength) && !is.na(snap_wavelength) &&
                         nzchar(as.character(snap_wavelength))) "elisa" else "xmap"

  # Detect if multiple sources exist for this antigen (e.g. MADI_P3_GAPS has
  # NIBSC06_140 and SD). Only apply source filter when >1 source is present.
  has_multiple_sources <- tryCatch({
    DBI::dbGetQuery(conn,
      "SELECT COUNT(DISTINCT source) AS n FROM madi_results.xmap_standard
       WHERE study_accession      = $1
         AND experiment_accession = $2
         AND antigen              = $3",
      params = list(snap_study, snap_experiment, snap_antigen)
    )$n[[1]] > 1L
  }, error = function(e) FALSE)

  snap_source_filter <- if (has_multiple_sources && nzchar(snap_source_raw)) snap_source_raw else NULL
  message(sprintf("[bayes] Source filter: %s", snap_source_filter %||% "(none — single source)"))

  # --- Reset state and mark running ---
  bayes_state$status        <- "running"
  bayes_state$plots         <- list()
  bayes_state$cdan_profiles <- list()
  bayes_state$lod_profiles  <- list()
  bayes_state$infl_profiles <- list()
  bayes_state$d2_profiles   <- list()
  bayes_state$lrdl_profiles <- list()
  bayes_state$uod_profiles  <- list()
  bayes_state$urdl_profiles <- list()
  bayes_state$assay         <- NULL
  bayes_state$error_msg     <- NULL
  bayes_state$trigger_key   <- paste(snap_study, snap_experiment, snap_antigen, sep = "|")

  # ── Synchronous execution — Stan runs its own internal C++ threads ────────
  tryCatch({

    # ── 1. Extract Standards ────────────────────────────────────────────────
    #
    # `xmap_standard` stores the raw plate_id (file-path derived key).
    # The human-readable `plateid` lives in `xmap_header` and is obtained
    # via an INNER JOIN on (study_accession, experiment_accession, plate_id).
    # This is exactly what pull_data() / fetch_db_standards() does.
    # Using `plate` directly from xmap_standard fails because that column
    # is often NULL, which cascades to N_plates = NA inside Stan.
    # For ELISA, filter by the user-selected wavelength so only one channel's
    # readings reach the Stan model (mixing 450 + 620 nm would average them).
    # For xMAP, no wavelength column filter is added.
    if (!is.null(snap_wavelength) && nzchar(as.character(snap_wavelength))) {
      stds_raw <- DBI::dbGetQuery(conn,
        "SELECT
           s.antigen,
           s.source,
           h.plateid,
           h.nominal_sample_dilution,
           s.antibody_mfi            AS mfi,
           s.dilution                AS dilution_factor
         FROM madi_results.xmap_standard s
         INNER JOIN madi_results.xmap_header h
           ON  h.study_accession      = s.study_accession
           AND h.experiment_accession = s.experiment_accession
           AND TRIM(h.plate_id)       = TRIM(s.plate_id)
         WHERE s.study_accession      = $1
           AND s.experiment_accession = $2
           AND s.antigen              = $3
           AND s.wavelength           = $4",
        params = list(snap_study, snap_experiment, snap_antigen, as.character(snap_wavelength))
      )
    } else {
      stds_raw <- DBI::dbGetQuery(conn,
        "SELECT
           s.antigen,
           s.source,
           h.plateid,
           h.nominal_sample_dilution,
           s.antibody_mfi            AS mfi,
           s.dilution                AS dilution_factor
         FROM madi_results.xmap_standard s
         INNER JOIN madi_results.xmap_header h
           ON  h.study_accession      = s.study_accession
           AND h.experiment_accession = s.experiment_accession
           AND TRIM(h.plate_id)       = TRIM(s.plate_id)
         WHERE s.study_accession      = $1
           AND s.experiment_accession = $2
           AND s.antigen              = $3",
        params = list(snap_study, snap_experiment, snap_antigen)
      )
    }

    # ── Source filter (post-query) ──────────────────────────────────────────
    # For multi-source studies (e.g. MADI_P3_GAPS with NIBSC06_140 vs SD),
    # keep only the user-selected source. Single-source studies skip this.
    if (!is.null(snap_source_filter) && "source" %in% names(stds_raw)) {
      stds_raw <- stds_raw[stds_raw$source == snap_source_filter, , drop = FALSE]
      message(sprintf("[bayes] Standards filtered to source='%s': %d rows remain",
                      snap_source_filter, nrow(stds_raw)))
    }

    if (nrow(stds_raw) == 0L) {
      stop(sprintf(
        "No standard data found for study='%s', experiment='%s', antigen='%s'.",
        snap_study, snap_experiment, snap_antigen
      ))
    }

    # Ensure plateid is a clean character vector — never NA, never a factor.
    # NA plateid → max(plate_idx) = NA → Stan error "does not support NA".
    stds <- stds_raw |>
      dplyr::mutate(
        plateid        = as.character(plateid),
        mfi            = as.numeric(mfi),
        dilution_factor = as.numeric(dilution_factor),
        nominal_sample_dilution = as.numeric(nominal_sample_dilution)
      ) |>
      dplyr::filter(
        !is.na(plateid), nzchar(plateid),
        mfi > 0, !is.na(dilution_factor), dilution_factor > 0
      ) |>
      dplyr::mutate(
        base_num      = sc_concentration_cache() %||% dplyr::coalesce(nominal_sample_dilution, 100000),
        concentration = base_num / dilution_factor
      ) |>
      dplyr::group_by(plateid, concentration) |>
      dplyr::summarise(mfi = median(mfi, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(!is.na(mfi))

    # Validate: all plateids must be non-NA after the pipeline
    if (any(is.na(stds$plateid))) {
      stop(sprintf(
        "plateid column still contains NA after pipeline for antigen='%s'. Plate count: %d",
        snap_antigen, dplyr::n_distinct(stds$plateid)
      ))
    }

    message(sprintf(
      "[bayes] Standards ready: %d rows, %d plates: %s",
      nrow(stds),
      dplyr::n_distinct(stds$plateid),
      paste(sort(unique(stds$plateid)), collapse = ", ")
    ))

    # Apply Scot's prozone correction per plate: adjusts post-peak MFI values
    # upward to a neutral asymptote rather than removing high-concentration
    # points, preserving all data for upper-asymptote estimation.
    stds <- stds |>
      dplyr::group_by(plateid) |>
      dplyr::group_modify(~ correct_prozone(
        stdframe             = .x,
        prop_diff            = 0.1,
        dil_scale            = 2,
        response_variable    = "mfi",
        independent_variable = "concentration"
      )) |>
      dplyr::ungroup()

    # ── 2. Extract Samples ──────────────────────────────────────────────────
    #
    # Same join pattern: use xmap_header to resolve plateid.
    samps_raw <- DBI::dbGetQuery(conn,
      "SELECT
         s.antigen,
         h.plateid,
         s.sampleid     AS sample_id,
         s.antibody_mfi AS mfi,
         s.agroup,
         s.dilution
       FROM madi_results.xmap_sample s
       INNER JOIN madi_results.xmap_header h
         ON  h.study_accession      = s.study_accession
         AND h.experiment_accession = s.experiment_accession
         AND TRIM(h.plate_id)       = TRIM(s.plate_id)
       WHERE s.study_accession      = $1
         AND s.experiment_accession = $2
         AND s.antigen              = $3",
      params = list(snap_study, snap_experiment, snap_antigen)
    )

    # NOTE: No source filter on samples — xmap_sample.source is NULL in
    # multi-source studies (e.g. MADI_P3_GAPS). Samples are source-agnostic;
    # they get interpolated against whichever source's standard curve is fitted.

    samps <- samps_raw |>
      dplyr::mutate(
        plateid = as.character(plateid),
        mfi     = as.numeric(mfi)
      ) |>
      dplyr::filter(!is.na(plateid), mfi > 0, !is.na(mfi))

    target_plates <- sort(unique(stds$plateid))

    # ── 1b. Extract Blanks ──────────────────────────────────────────────────
    # Query blank wells (stype = 'B') from xmap_buffer for the same
    # study/experiment/antigen. JOIN with xmap_header to resolve the
    # human-readable plateid (xmap_buffer.plateid is nullable).
    if (!is.null(snap_wavelength) && nzchar(as.character(snap_wavelength))) {
      blanks_raw <- DBI::dbGetQuery(conn,
        "SELECT
           b.antigen,
           h.plateid,
           b.antibody_mfi AS mfi
         FROM madi_results.xmap_buffer b
         INNER JOIN madi_results.xmap_header h
           ON  h.study_accession      = b.study_accession
           AND h.experiment_accession = b.experiment_accession
           AND TRIM(h.plate_id)       = TRIM(b.plate_id)
         WHERE b.study_accession      = $1
           AND b.experiment_accession = $2
           AND b.antigen              = $3
           AND UPPER(b.stype)         = 'B'
           AND b.antibody_mfi         > 0
           AND b.wavelength           = $4",
        params = list(snap_study, snap_experiment, snap_antigen, as.character(snap_wavelength))
      )
    } else {
      blanks_raw <- DBI::dbGetQuery(conn,
        "SELECT
           b.antigen,
           h.plateid,
           b.antibody_mfi AS mfi
         FROM madi_results.xmap_buffer b
         INNER JOIN madi_results.xmap_header h
           ON  h.study_accession      = b.study_accession
           AND h.experiment_accession = b.experiment_accession
           AND TRIM(h.plate_id)       = TRIM(b.plate_id)
         WHERE b.study_accession      = $1
           AND b.experiment_accession = $2
           AND b.antigen              = $3
           AND UPPER(b.stype)         = 'B'
           AND b.antibody_mfi         > 0",
        params = list(snap_study, snap_experiment, snap_antigen)
      )
    }

    blanks_df <- blanks_raw |>
      dplyr::mutate(
        plateid = as.character(plateid),
        mfi     = as.numeric(mfi)
      ) |>
      dplyr::filter(!is.na(plateid), nzchar(plateid), !is.na(mfi), mfi > 0)

    message(sprintf(
      "[bayes] Blanks ready: %d rows, %d plates",
      nrow(blanks_df),
      dplyr::n_distinct(blanks_df$plateid)
    ))

    # ── 3. Instantiate StanAssay ────────────────────────────────────────────
    # `error_model` was removed from the constructor in stanassay@58777fe;
    # the model type is now specified via exact_model() / eiv_model() in fit().
    # blank_data anchors the lower asymptote `a` to plate-specific blank signal
    # via a Student-T likelihood in Stan (Klauenberg et al. 2015).
    assay <- StanAssay$new(
      std_data           = stds,
      concentration_col  = "concentration",
      response_col       = "mfi",
      plate_col          = "plateid",
      assay_type         = snap_assay_type,
      blank_data         = if (nrow(blanks_df) > 0L) blanks_df else NULL,
      blank_response_col = "mfi",
      blank_plate_col    = "plateid",
      apply_prozone      = FALSE,
      prozone_threshold  = snap_prozone_threshold
    )

    # ── 4. Fit the hierarchical Stan model ──────────────────────────────────
    # Between-chain parallelism: each chain runs on its own core (cores = n_chains).
    # Within-chain parallelism: STAN_THREADS compiled in; each chain uses
    # threads_per_chain TBB threads for reduce_sum. Total = n_chains * tpc.
    n_logical <- max(2L, as.integer(parallel::detectCores(logical = FALSE) %||% 4L))
    n_chains  <- min(snap_n_chains, n_logical - 1L)  # leave 1 core for Shiny
    tpc       <- max(1L, floor((n_logical - 1L) / n_chains))  # remaining cores / chain

    suppressWarnings(
      assay$fit_ensemble(
        families          = c("4pl", "5pl", "gompertz"),
        error_model       = "exact",
        n_iter            = snap_n_iter,
        n_chains          = n_chains,
        cores             = n_chains,
        threads_per_chain = tpc,
        grainsize         = 1L
      )
    )

    # ── 4b. Retrieve ensemble summary ────────────────────────────────────────
    ens_summary <- assay$summarize_ensemble()

    # ── 5. Back-calculate sample concentrations ─────────────────────────────
    results_df <- NULL
    if (nrow(samps) > 0L) {
      results_df <- assay$predict_samples(samps) |>
        dplyr::mutate(
          sample_dilution     = as.numeric(dilution),
          sample_dilution     = ifelse(is.na(sample_dilution), 1, sample_dilution),
          conc_absolute_mean  = predicted_conc_mean  * sample_dilution,
          conc_absolute_lower = predicted_conc_lower * sample_dilution,
          conc_absolute_upper = predicted_conc_upper * sample_dilution
        )
    }

    # ── 6. Compute CDAN precision profiles ──────────────────────────────────
    cdan_list <- list()
    for (plt in target_plates) {
      tryCatch({
        cdan_list[[plt]] <- assay$compute_cdan(plt, n_grid = 100L, n_draws = 500L)
      }, error = function(e) {
        message(sprintf("[bayes] cdan(%s) failed: %s", plt, e$message))
      })
    }

    # ── 6b. Compute Bayesian LOD per plate (O'Malley 2003) ───────────────────
    # Requires blank data to have been supplied — gracefully returns NA if not.
    lod_list <- list()
    for (plt in target_plates) {
      tryCatch({
        lod_list[[plt]] <- assay$compute_lod(plt)
      }, error = function(e) {
        message(sprintf("[bayes] lod(%s) failed: %s", plt, e$message))
      })
    }

    # ── 6c. Compute inflection point (EC50) per plate ─────────────────────────
    infl_list <- list()
    for (plt in target_plates) {
      infl_list[[plt]] <- tryCatch(
        assay$compute_inflection_point(plt),
        error = function(e) {
          message(sprintf("[bayes] compute_inflection_point(%s) failed: %s", plt, e$message))
          NULL
        }
      )
    }

    # ── 6d. Compute exact shoulders (LO2D / UO2D) per plate ──────────────────
    # Uses closed-form roots of d²y/d(ln x)² — Scot's shoulder_positions() formula.
    d2_list <- list()
    for (plt in target_plates) {
      d2_list[[plt]] <- tryCatch(
        assay$compute_shoulders(plt),
        error = function(e) {
          message(sprintf("[bayes] compute_shoulders(%s) failed: %s", plt, e$message))
          NULL
        }
      )
    }

    # ── 6e. Compute LRDL per plate (posterior predictive) ────────────────────
    lrdl_list <- list()
    for (plt in target_plates) {
      lrdl_list[[plt]] <- tryCatch(
        assay$compute_lrdl(plt),
        error = function(e) { NULL }
      )
    }

    # ── 6e2. Compute UOD per plate ──────────────────────────────────────────
    uod_list <- list()
    for (plt in target_plates) {
      uod_list[[plt]] <- tryCatch(
        assay$compute_uod(plt),
        error = function(e) { NULL }
      )
    }

    # ── 6e3. Compute URDL per plate (posterior predictive) ──────────────────
    urdl_list <- list()
    for (plt in target_plates) {
      urdl_list[[plt]] <- tryCatch(
        assay$compute_urdl(plt),
        error = function(e) { NULL }
      )
    }

    # ── 6f. Summarize asymmetry (4PL vs 5PL) ─────────────────────────────────
    # Gompertz has no g parameter — skip asymmetry summary for that family.
    asym <- if (!is.null(assay$fit_obj$curve_family) &&
                assay$fit_obj$curve_family != "gompertz") {
      tryCatch(
        assay$summarize_asymmetry(),
        error = function(e) {
          message(sprintf("[bayes] summarize_asymmetry failed: %s", e$message))
          NULL
        }
      )
    } else {
      NULL
    }

    # ── 7. Generate per-plate plots (with LOD/LLOQ/ULOQ markers) ────────────
    # CDAN and LOD are computed first so limits can be passed into the plot.
    generated_plots <- list()
    for (plt in target_plates) {
      tryCatch({
        # LOD threshold MFI — NULL-safe (compute_lod may have failed for this plate)
        lod_mfi <- tryCatch(lod_list[[plt]]$threshold_mfi_median, error = function(e) NULL)

        generated_plots[[plt]] <- assay$plot(
          plt,
          sample_data         = results_df
        )
      }, error = function(e) {
        message(sprintf("[bayes] plot(%s) failed: %s", plt, e$message))
      })
    }

    # ── 8. Update reactive state on success ─────────────────────────────────
    bayes_state$assay             <- assay
    bayes_state$plots             <- generated_plots
    bayes_state$cdan_profiles     <- cdan_list
    bayes_state$lod_profiles      <- lod_list
    bayes_state$infl_profiles     <- infl_list
    bayes_state$d2_profiles       <- d2_list
    bayes_state$lrdl_profiles     <- lrdl_list
    bayes_state$uod_profiles      <- uod_list
    bayes_state$urdl_profiles     <- urdl_list
    bayes_state$asymmetry         <- asym
    bayes_state$best_family       <- ens_summary$best_family
    bayes_state$stacking_weights  <- ens_summary$stacking_weights
    bayes_state$plate_best_family <- ens_summary$plate_best_family
    bayes_state$plate_elpd        <- ens_summary$plate_elpd
    bayes_state$loo_comparison    <- ens_summary$loo_comparison
    bayes_state$pareto_k_summary  <- ens_summary$pareto_k_summary
    bayes_state$sample_results    <- results_df
    bayes_state$ensemble_data     <- NULL
    bayes_state$data_source       <- "live_fit"
    bayes_state$status            <- "ready"
    bayes_state$error_msg         <- NULL

    updateSelectInput(
      session,
      "bayes_view_plate",
      choices  = target_plates,
      selected = target_plates[1]
    )

    asym_note <- ""
    if (!is.null(asym)) {
      asym_note <- sprintf(" | P(4PL): %.0f%%", asym$prob_4pl * 100)
    }
    # Summarise per-plate model selection for the notification
    pbf <- bayes_state$plate_best_family
    if (!is.null(pbf) && length(pbf) > 0L) {
      tbl <- table(pbf)
      family_summary <- paste(
        sapply(names(tbl), function(f) {
          lbl <- switch(f, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gomp", f)
          sprintf("%s×%s", tbl[[f]], lbl)
        }),
        collapse = ", "
      )
      fit_note <- paste0("per-plate: ", family_summary)
    } else {
      fit_note <- switch(
        bayes_state$best_family %||% "5pl",
        "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz",
        bayes_state$best_family %||% "5PL"
      )
    }
    showNotification(
      paste0(
        "Bayesian ensemble fit complete for ", snap_antigen,
        " — ", fit_note,
        " (", length(generated_plots), " plates)", asym_note, "."
      ),
      type     = "message",
      duration = 8
    )

    # Prozone summary notification
    plog <- assay$prozone_log
    if (!is.null(plog) && nrow(plog) > 0L) {
      lines <- apply(plog, 1L, function(r) {
        sprintf("  %s: %s point(s) removed above %.3g",
                r[["plateid"]], r[["n_removed"]], as.numeric(r[["hook_concentration"]]))
      })
      showNotification(
        HTML(paste0(
          "<b>Hook-effect truncation applied (", nrow(plog), " plate(s)):</b><br>",
          paste(lines, collapse = "<br>")
        )),
        type     = "warning",
        duration = 12
      )
    }

  }, error = function(err) {
    bayes_state$status    <- "error"
    bayes_state$error_msg <- conditionMessage(err)
    message("[bayes] error: ", conditionMessage(err))
    showNotification(
      paste0("Bayesian ensemble error: ", conditionMessage(err)),
      type     = "error",
      duration = 15
    )
  })
})


# ============================================================================
# BAYESIAN 5PL — Wavelength selector (ELISA only)
# Queries available wavelengths for the current antigen/study/experiment.
# Renders a selectInput only when non-NA wavelengths exist (ELISA data).
# Returns NULL (invisible) for xMAP / Luminex data where wavelength is NULL.
# ============================================================================
output$bayes_wavelength_ui <- renderUI({
  study_val <- input$readxMap_study_accession
  exp_val   <- input$readxMap_experiment_accession
  ant_val   <- input$sc_antigen_select

  # Only query when all three selectors are populated
  if (is.null(study_val)  || !nzchar(study_val)  || study_val  == "Click here") return(NULL)
  if (is.null(exp_val)    || !nzchar(exp_val)    || exp_val    == "Click here") return(NULL)
  if (is.null(ant_val)    || !nzchar(ant_val))  return(NULL)

  available_waves <- tryCatch({
    res <- DBI::dbGetQuery(conn,
      "SELECT DISTINCT wavelength
       FROM madi_results.xmap_standard
       WHERE study_accession      = $1
         AND experiment_accession = $2
         AND antigen              = $3
         AND wavelength IS NOT NULL",
      params = list(study_val, exp_val, ant_val)
    )
    sort(unique(res$wavelength))
  }, error = function(e) {
    message("[bayes_wavelength_ui] DB query error: ", e$message)
    character(0)
  })

  # Hide the widget entirely for xMAP (no non-NA wavelengths)
  if (length(available_waves) == 0L) return(NULL)

  # ELISA: show the wavelength dropdown
  fluidRow(
    column(
      4,
      selectInput(
        "bayes_selected_wavelength",
        label   = "Select Wavelength (ELISA):",
        choices = available_waves
      )
    )
  )
})


# ============================================================================
# BAYESIAN ENSEMBLE — Panel title (shows per-plate best model for current view)
# ============================================================================
output$bayes_panel_title <- renderText({
  pbf  <- bayes_state$plate_best_family
  plt  <- input$bayes_view_plate
  if (!is.null(pbf) && !is.null(plt) && nzchar(plt) && plt %in% names(pbf)) {
    fam <- pbf[[plt]]
    lbl <- switch(fam, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", fam)
    paste0("Bayesian Ensemble — Best model for this plate: ", lbl)
  } else if (!is.null(bayes_state$best_family)) {
    lbl <- switch(bayes_state$best_family,
      "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz",
      bayes_state$best_family)
    paste0("Bayesian Ensemble — Global best: ", lbl)
  } else {
    "Bayesian Ensemble Analysis"
  }
})


# ============================================================================
# BAYESIAN ENSEMBLE — Stacking weights + per-plate model selection table
# ============================================================================
output$bayes_stacking_weights <- renderPrint({
  req(bayes_state$status == "ready", bayes_state$stacking_weights)

  sw  <- bayes_state$stacking_weights
  pbf <- bayes_state$plate_best_family
  plt <- input$bayes_view_plate

  # Global stacking weights
  cat("Ensemble \u2014 Global Stacking Weights\n")
  cat("=====================================\n")
  for (nm in names(sw)) {
    lbl    <- switch(nm, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", nm)
    global <- if (nm == bayes_state$best_family) " \u2605" else ""
    cat(sprintf("  %-10s  %.4f%s\n", lbl, sw[[nm]], global))
  }

  # Per-plate model selection
  if (!is.null(pbf) && length(pbf) > 0L) {
    cat("\nPer-Plate Best Model\n")
    cat("====================\n")
    for (pid in names(pbf)) {
      fam <- pbf[[pid]]
      lbl <- switch(fam, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", fam)
      viewing <- if (!is.null(plt) && pid == plt) " \u25c4 viewing" else ""

      # Include plate-level ELPD if available
      elpd_note <- ""
      pe <- bayes_state$plate_elpd
      if (!is.null(pe) && pid %in% rownames(pe) && fam %in% colnames(pe)) {
        elpd_note <- sprintf("  (ELPD %.1f)", pe[pid, fam])
      }
      cat(sprintf("  %-30s  %-10s%s%s\n", pid, lbl, elpd_note, viewing))
    }
  }
})


# ============================================================================
# BAYESIAN — Auto-fetch from DB when switching to Bayesian mode
# Fires when curve method changes to Bayesian or when antigen/source changes
# while already in Bayesian mode.
# ============================================================================
observeEvent(
  list(input$sc_curve_method, input$sc_antigen_select, input$sc_source_select),
  ignoreInit = TRUE,
  {
    req(identical(input$sc_curve_method, "Bayesian"))
    req(input$sc_antigen_select, input$sc_source_select)
    req(input$readxMap_study_accession != "Click here")
    req(input$readxMap_experiment_accession != "Click here")

    # Don't re-fetch if already ready (user might just be switching plates)
    # But DO re-fetch if antigen/source changed
    trigger_key <- paste(input$readxMap_study_accession,
                         input$readxMap_experiment_accession,
                         input$sc_antigen_select,
                         input$sc_source_select, sep = "|")
    if (identical(bayes_state$trigger_key, trigger_key) && bayes_state$status == "ready") {
      return()
    }

    message(sprintf("[bayes_db_fetch] Trying DB for %s / %s / %s",
                    input$sc_antigen_select, input$sc_source_select, trigger_key))

    db_result <- tryCatch(
      fetch_bayes_from_db(
        conn       = conn,
        project_id = userWorkSpaceID(),
        study      = input$readxMap_study_accession,
        experiment = input$readxMap_experiment_accession,
        antigen    = input$sc_antigen_select,
        source_filter = input$sc_source_select
      ),
      error = function(e) {
        message(sprintf("[bayes_db_fetch] Error: %s", e$message))
        NULL
      }
    )

    if (!is.null(db_result)) {
      message(sprintf("[bayes_db_fetch] Loaded %d plates from database", length(db_result$plots)))

      bayes_state$plots             <- db_result$plots
      bayes_state$cdan_profiles     <- db_result$cdan_profiles
      bayes_state$lod_profiles      <- db_result$lod_profiles
      bayes_state$lrdl_profiles     <- db_result$lrdl_profiles
      bayes_state$uod_profiles      <- db_result$uod_profiles
      bayes_state$urdl_profiles     <- db_result$urdl_profiles
      bayes_state$infl_profiles     <- db_result$infl_profiles
      bayes_state$d2_profiles       <- db_result$d2_profiles
      bayes_state$asymmetry         <- db_result$asymmetry
      bayes_state$assay             <- NULL
      bayes_state$best_family       <- db_result$best_family
      bayes_state$stacking_weights  <- db_result$stacking_weights
      bayes_state$plate_best_family <- db_result$plate_best_family
      bayes_state$plate_elpd        <- db_result$plate_elpd
      bayes_state$loo_comparison    <- db_result$loo_comparison
      bayes_state$pareto_k_summary  <- db_result$pareto_k_summary
      bayes_state$sample_results    <- db_result$sample_results
      bayes_state$ensemble_data     <- db_result$ensemble_data
      bayes_state$data_source       <- "database"
      bayes_state$trigger_key       <- trigger_key
      bayes_state$status            <- "ready"
      bayes_state$error_msg         <- NULL

      plate_ids <- names(db_result$plots)
      updateSelectInput(
        session, "bayes_view_plate",
        choices  = plate_ids,
        selected = plate_ids[1]
      )

      showNotification(
        sprintf("Bayesian results loaded from database (%d plates)", length(plate_ids)),
        type = "message", duration = 4
      )
    } else {
      message("[bayes_db_fetch] No pre-computed results in DB — user can Calculate Bayes manually")
      # Reset to idle only if not already running or ready from a live fit
      if (bayes_state$status != "running") {
        bayes_state$status      <- "idle"
        bayes_state$data_source <- NULL
        bayes_state$trigger_key <- trigger_key
      }
    }
  }
)


# ============================================================================
# BAYESIAN ENSEMBLE — Status indicator
# ============================================================================

output$bayes_run_status <- renderUI({
  status <- bayes_state$status

  if (status == "idle") {
    div(
      style = "padding:6px 0; color:#888; font-style:italic;",
      icon("clock"), " Awaiting antigen / plate selection to start Bayesian fit..."
    )
  } else if (status == "running") {
    div(
      style = "padding:6px 0; color:#e67e22; font-weight:bold;",
      icon("spinner", class = "fa-spin"),
      " Bayesian Stan model is running in background (30-60 s)..."
    )
  } else if (status == "ready") {
    src_label <- if (identical(bayes_state$data_source, "database")) {
      sprintf(" Bayesian results loaded from database \u2014 %d plate(s).", length(bayes_state$plots))
    } else {
      sprintf(" Bayesian fit ready \u2014 %d plate(s) fitted.", length(bayes_state$plots))
    }
    src_icon <- if (identical(bayes_state$data_source, "database")) icon("database") else icon("check-circle")
    div(
      style = "padding:6px 0; color:#27ae60; font-weight:bold;",
      src_icon, src_label
    )
  } else if (status == "error") {
    div(
      style = "padding:6px 0; color:#c0392b; font-weight:bold;",
      icon("exclamation-triangle"),
      " Bayesian fit failed: ",
      tags$small(bayes_state$error_msg %||% "unknown error")
    )
  }
})


# ============================================================================
# BAYESIAN ENSEMBLE — Plotly output with CDAN precision overlay
# (Logic mirrors sandbox app.R output$single_plot exactly)
# ============================================================================
output$bayes_standard_curve <- renderPlotly({
  req(bayes_state$status == "ready")
  req(input$bayes_view_plate)
  req(length(bayes_state$plots) > 0)

  p_plotly <- bayes_state$plots[[input$bayes_view_plate]]
  if (is.null(p_plotly)) return(plotly_empty())

  # DB-sourced plots already include CDAN overlay — skip the add_trace logic
  if (identical(bayes_state$data_source, "database")) return(p_plotly)

  # ── Overlay CDAN precision profile on secondary right-hand Y axis ──────
  prof_obj <- bayes_state$cdan_profiles[[input$bayes_view_plate]]
  if (!is.null(prof_obj)) {
    prof <- prof_obj$profile |> dplyr::filter(!is.na(smoothed_cv) & smoothed_cv < 60)

    if (nrow(prof) > 0L) {

      p_plotly <- p_plotly |>
        plotly::add_trace(
          x             = prof$log10_conc,
          y             = prof$smoothed_cv,
          type          = "scatter",
          mode          = "lines",
          name          = "Bayesian CDAN Precision Profile",
          yaxis         = "y2",
          line          = list(color = "#1565C0", width = 2.5),
          hovertemplate = "Log10 Conc: %{x:.2f}<br>%CV: %{y:.1f}%<extra>CDAN</extra>"
        ) |>
        plotly::add_trace(
          x          = c(min(prof$log10_conc), max(prof$log10_conc)),
          y          = c(20, 20),
          type       = "scatter",
          mode       = "lines",
          name       = "pCoV Threshold: 20%",
          yaxis      = "y2",
          line       = list(color = "#e68fac", dash = "dash", width = 1.5),
          showlegend = TRUE,
          hoverinfo  = "skip"
        ) |>
        plotly::add_trace(
          x          = c(min(prof$log10_conc), max(prof$log10_conc)),
          y          = c(15, 15),
          type       = "scatter",
          mode       = "lines",
          name       = "pCoV Threshold: 15%",
          yaxis      = "y2",
          line       = list(color = "#4CAF50", dash = "dash", width = 1.5),
          showlegend = TRUE,
          hoverinfo  = "skip"
        ) |>
        plotly::layout(
          yaxis2 = list(
            overlaying = "y",
            side       = "right",
            title      = "Concentration Uncertainty (pCoV %)",
            range      = c(0, 55),
            showgrid   = FALSE,
            zeroline   = FALSE,
            tickfont   = list(color = "#1565C0"),
            titlefont  = list(color = "#1565C0")
          ),
          yaxis3 = list(
            overlaying = "y",
            side       = "right",
            anchor     = "free",
            position   = 0.97,
            title      = "d\u00B2x/dy\u00B2 (normalized)",
            range      = c(-1.05, 1.05),
            showgrid   = FALSE,
            zeroline   = TRUE,
            zerolinecolor = "#CCCCCC",
            zerolinewidth = 1,
            tickfont   = list(color = "#56B4E9", size = 10),
            titlefont  = list(color = "#56B4E9", size = 11)
          ),
          margin = list(r = 140)
        )
    }
  }

  p_plotly
})


# ============================================================================
# BAYESIAN ENSEMBLE — CDAN precision text summary
# (Logic mirrors sandbox app.R output$precision_summary exactly)
# ============================================================================
output$bayes_precision_summary <- renderPrint({
  req(bayes_state$status == "ready")
  req(input$bayes_view_plate)
  req(length(bayes_state$cdan_profiles) > 0)

  prof_obj <- bayes_state$cdan_profiles[[input$bayes_view_plate]]
  if (is.null(prof_obj)) {
    cat("No precision profile available for this plate.\n")
    return(invisible(NULL))
  }

  cat(sprintf("CDAN Precision Profile Summary — %s\n", input$bayes_view_plate))
  cat(sprintf("Posterior draws used: %d | Grid points: %d\n\n",
              prof_obj$n_draws, prof_obj$n_grid))

  cat("Limits of Quantification (LOQ):\n")
  cat(sprintf("  At 20%% CV:  LLOQ = %s  |  ULOQ = %s\n",
              ifelse(is.na(prof_obj$lloq_20), "N/A (too noisy)",
                     sprintf("%.3f", prof_obj$lloq_20)),
              ifelse(is.na(prof_obj$uloq_20), "N/A",
                     sprintf("%.3f", prof_obj$uloq_20))))
  cat(sprintf("  At 15%% CV:  LLOQ = %s  |  ULOQ = %s\n",
              ifelse(is.na(prof_obj$lloq_15), "N/A (too noisy)",
                     sprintf("%.3f", prof_obj$lloq_15)),
              ifelse(is.na(prof_obj$uloq_15), "N/A",
                     sprintf("%.3f", prof_obj$uloq_15))))

  # Dynamic range
  if (!is.na(prof_obj$lloq_20) && !is.na(prof_obj$uloq_20)) {
    dyn_range <- prof_obj$uloq_20 / prof_obj$lloq_20
    cat(sprintf("\n  Dynamic Range (20%% CV): %.0f-fold (%.3f to %.3f)\n",
                dyn_range, prof_obj$lloq_20, prof_obj$uloq_20))
  }
  if (!is.na(prof_obj$lloq_15) && !is.na(prof_obj$uloq_15)) {
    dyn_range <- prof_obj$uloq_15 / prof_obj$lloq_15
    cat(sprintf("  Dynamic Range (15%% CV): %.0f-fold (%.3f to %.3f)\n",
                dyn_range, prof_obj$lloq_15, prof_obj$uloq_15))
  }

  # Best-precision sweet spot
  prof     <- prof_obj$profile
  best_idx <- which.min(prof$smoothed_cv)
  if (length(best_idx) > 0L && !is.na(prof$smoothed_cv[best_idx])) {
    cat(sprintf(
      "\n  Best precision (sweet spot): %.1f%% CV at concentration %.3f  [LOESS-smoothed]\n",
      prof$smoothed_cv[best_idx], prof$concentration[best_idx]
    ))
  }

  # LOD & LRDL (Bayesian, O'Malley 2003) — only shown when blank data was supplied
  lod_obj  <- bayes_state$lod_profiles[[input$bayes_view_plate]]
  lrdl_obj <- bayes_state$lrdl_profiles[[input$bayes_view_plate]]
  if (!is.null(lod_obj)) {
    if (!is.na(lod_obj$lod)) {
      cat(sprintf(
        "\nLimit of Detection (Bayesian, 3\u03c3 blank criterion):\n  LOD = %.4f  (log10: %.3f)\n",
        lod_obj$lod, lod_obj$lod_log10
      ))
      cat(sprintf(
        "  Blank threshold (median a + 3\u00d7\u03c3_blank): %.2f MFI\n",
        lod_obj$threshold_mfi_median
      ))
    } else {
      note <- lod_obj$note %||% "sigma_blank not available"
      cat(sprintf("\nLimit of Detection: N/A (%s)\n", note))
    }
  }
  if (!is.null(lrdl_obj) && !is.na(lrdl_obj$lrdl)) {
    cat(sprintf(
      "\nLower Reliable Detection Limit (LRDL):\n  LRDL = %.4f  (log10: %.3f)\n  Method: %s\n",
      lrdl_obj$lrdl, lrdl_obj$lrdl_log10, lrdl_obj$method
    ))
  }

  # UOD (Upper Limit of Detection)
  uod_obj <- bayes_state$uod_profiles[[input$bayes_view_plate]]
  if (!is.null(uod_obj) && !is.na(uod_obj$uod)) {
    cat(sprintf(
      "\nUpper Limit of Detection (UOD):\n  UOD = %.4f  (log10: %.3f)\n",
      uod_obj$uod, uod_obj$uod_log10
    ))
  }

  # URDL (Upper Reliable Detection Limit)
  urdl_obj <- bayes_state$urdl_profiles[[input$bayes_view_plate]]
  if (!is.null(urdl_obj) && !is.na(urdl_obj$urdl)) {
    cat(sprintf(
      "\nUpper Reliable Detection Limit (URDL):\n  URDL = %.4f  (log10: %.3f)\n  Method: %s\n",
      urdl_obj$urdl, urdl_obj$urdl_log10, urdl_obj$method
    ))
  }

  # Inflection point (max |dy/dx| — Scot's linear-x formula)
  infl_obj <- bayes_state$infl_profiles[[input$bayes_view_plate]]
  if (!is.null(infl_obj)) {
    cat(sprintf(
      "\nInflection Point (max |dy/dx|):\n  Conc = %.4f  [95%% CI: %.4f \u2013 %.4f]\n  Family: %s\n",
      infl_obj$x_median, infl_obj$x_lower, infl_obj$x_upper,
      toupper(infl_obj$family)
    ))
  }

  # LO2D / UO2D (exact shoulders — closed-form roots of d²y/d(ln x)²)
  d2_obj <- bayes_state$d2_profiles[[input$bayes_view_plate]]
  if (!is.null(d2_obj)) {
    cat(sprintf(
      "\nLO2D / UO2D (exact shoulders \u2014 closed-form roots of d\u00b2y/d(ln x)\u00b2):\n  LO2D = %s\n  UO2D = %s\n",
      ifelse(is.na(d2_obj$lo2d_median), "N/A", sprintf("%.4f", d2_obj$lo2d_median)),
      ifelse(is.na(d2_obj$uo2d_median), "N/A", sprintf("%.4f", d2_obj$uo2d_median))
    ))
  }

  # CDAN method
  if (!is.null(prof_obj$method)) {
    cat(sprintf("\nCDAN Method: %s\n", prof_obj$method))
  }

  # Asymmetry (4PL vs 5PL) summary
  asym <- bayes_state$asymmetry
  if (!is.null(asym)) {
    cat("\nAsymmetry Assessment (4PL vs 5PL):\n")
    cat(sprintf("  P(effectively 4PL): %.1f%%\n", asym$prob_4pl * 100))
    if (asym$prob_4pl > 0.8) {
      cat("  Interpretation: Data is effectively symmetric (4PL sufficient)\n")
    } else if (asym$prob_4pl < 0.3) {
      cat("  Interpretation: Genuine asymmetry detected (5PL features active)\n")
    } else {
      cat("  Interpretation: Ambiguous \u2014 uncertainty honestly represented in posterior\n")
    }
    cat(sprintf("  Prior mode: %s\n", asym$g_prior_mode))

    # Per-plate g for selected plate
    g_plate <- asym$g_plate
    if (!is.null(g_plate) && input$bayes_view_plate %in% g_plate$plateid) {
      row <- g_plate[g_plate$plateid == input$bayes_view_plate, ]
      cat(sprintf("  g (this plate): median = %.3f [%.3f, %.3f]\n",
                  row$median, row$`2.5%`, row$`97.5%`))
    }
  }
})


# ============================================================================
# BAYESIAN ENSEMBLE — Stacking Weights Table
# ============================================================================
output$bayes_stacking_weights_tbl <- renderTable({
  req(bayes_state$status == "ready", bayes_state$stacking_weights)

  sw  <- bayes_state$stacking_weights
  pbf <- bayes_state$plate_best_family
  plt <- input$bayes_view_plate

  # ── Global stacking weights table ──
  model_labels <- vapply(names(sw), function(nm) {
    switch(nm, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", nm)
  }, character(1))

  data.frame(
    Model   = model_labels,
    Weight  = sprintf("%.4f", unlist(sw)),
    Best    = ifelse(names(sw) == bayes_state$best_family, "\u2605", ""),
    stringsAsFactors = FALSE
  )
},
caption           = "Global Stacking Weights",
caption.placement = getOption("xtable.caption.placement", "top"),
striped           = TRUE,
hover             = TRUE,
bordered          = TRUE,
spacing           = "s")


# ============================================================================
# BAYESIAN ENSEMBLE — Plate Summary Table (horizontal, one row per plate)
# ============================================================================
output$bayes_precision_summary_tbl <- renderTable({
  req(bayes_state$status == "ready")
  req(length(bayes_state$cdan_profiles) > 0)

  fmt <- function(x, digits = 3) {
    if (is.null(x) || is.na(x)) NA_character_ else sprintf(paste0("%.", digits, "f"), x)
  }

  # Build one row per plate that has a CDAN profile
  plate_ids <- names(bayes_state$cdan_profiles)
  pbf <- bayes_state$plate_best_family
  viewing <- input$bayes_view_plate

  rows <- lapply(plate_ids, function(pid) {
    prof_obj <- bayes_state$cdan_profiles[[pid]]
    lod_obj  <- bayes_state$lod_profiles[[pid]]
    lrdl_obj <- bayes_state$lrdl_profiles[[pid]]
    uod_obj  <- bayes_state$uod_profiles[[pid]]
    urdl_obj <- bayes_state$urdl_profiles[[pid]]
    infl_obj <- bayes_state$infl_profiles[[pid]]
    d2_obj   <- bayes_state$d2_profiles[[pid]]

    # Best model for this plate
    best_lbl <- NA_character_
    if (!is.null(pbf) && pid %in% names(pbf)) {
      fam <- pbf[[pid]]
      best_lbl <- switch(fam, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", fam)
    }

    data.frame(
      Plate       = pid,
      Model       = best_lbl %||% NA_character_,
      LLOQ_20     = fmt(if (!is.null(prof_obj)) prof_obj$lloq_20 else NA),
      ULOQ_20     = fmt(if (!is.null(prof_obj)) prof_obj$uloq_20 else NA),
      LLOQ_15     = fmt(if (!is.null(prof_obj)) prof_obj$lloq_15 else NA),
      ULOQ_15     = fmt(if (!is.null(prof_obj)) prof_obj$uloq_15 else NA),
      LOD         = fmt(if (!is.null(lod_obj) && !is.na(lod_obj$lod)) lod_obj$lod else NA, 4),
      LRDL        = fmt(if (!is.null(lrdl_obj) && !is.na(lrdl_obj$lrdl)) lrdl_obj$lrdl else NA, 4),
      UOD         = fmt(if (!is.null(uod_obj) && !is.na(uod_obj$uod)) uod_obj$uod else NA, 4),
      URDL        = fmt(if (!is.null(urdl_obj) && !is.na(urdl_obj$urdl)) urdl_obj$urdl else NA, 4),
      Inflection  = fmt(if (!is.null(infl_obj)) infl_obj$x_median else NA, 4),
      LO2D        = fmt(if (!is.null(d2_obj)) d2_obj$lo2d_median else NA, 4),
      UO2D        = fmt(if (!is.null(d2_obj)) d2_obj$uo2d_median else NA, 4),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  })

  do.call(rbind, rows)
},
striped           = TRUE,
hover             = TRUE,
bordered          = TRUE,
spacing           = "s")


# ============================================================================
# BAYESIAN — Summary Statistics table (bottom section, richer than frequentist)
# Shows only the currently viewed plate with full Bayesian-specific metrics.
# ============================================================================
output$bayes_summary_statistics <- renderTable({
  req(bayes_state$status == "ready")
  req(input$bayes_view_plate)

  pid <- input$bayes_view_plate

  prof_obj  <- bayes_state$cdan_profiles[[pid]]
  lod_obj   <- bayes_state$lod_profiles[[pid]]
  lrdl_obj  <- bayes_state$lrdl_profiles[[pid]]
  uod_obj   <- bayes_state$uod_profiles[[pid]]
  urdl_obj  <- bayes_state$urdl_profiles[[pid]]
  infl_obj  <- bayes_state$infl_profiles[[pid]]
  d2_obj    <- bayes_state$d2_profiles[[pid]]

  fmt <- function(x, digits = 3) {
    if (is.null(x) || is.na(x)) NA_character_ else sprintf(paste0("%.", digits, "f"), x)
  }

  # Best model info
  pbf <- bayes_state$plate_best_family
  best_model <- NA_character_
  elpd_val   <- NA_character_
  if (!is.null(pbf) && pid %in% names(pbf)) {
    fam <- pbf[[pid]]
    best_model <- switch(fam, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", fam)
    pe <- bayes_state$plate_elpd
    if (!is.null(pe) && pid %in% rownames(pe) && fam %in% colnames(pe)) {
      elpd_val <- sprintf("%.1f", pe[pid, fam])
    }
  }

  sw <- bayes_state$stacking_weights
  best_weight <- NA_character_
  if (!is.null(sw) && !is.null(pbf) && pid %in% names(pbf)) {
    fam <- pbf[[pid]]
    if (fam %in% names(sw)) best_weight <- sprintf("%.4f", sw[[fam]])
  }

  # Dynamic range
  dr_20 <- NA_character_
  dr_15 <- NA_character_
  if (!is.null(prof_obj)) {
    if (!is.na(prof_obj$lloq_20) && !is.na(prof_obj$uloq_20)) {
      dr_20 <- sprintf("%.0f-fold", prof_obj$uloq_20 / prof_obj$lloq_20)
    }
    if (!is.na(prof_obj$lloq_15) && !is.na(prof_obj$uloq_15)) {
      dr_15 <- sprintf("%.0f-fold", prof_obj$uloq_15 / prof_obj$lloq_15)
    }
  }

  # Best precision sweet spot
  best_prec <- NA_character_
  if (!is.null(prof_obj)) {
    prof     <- prof_obj$profile
    best_idx <- which.min(prof$smoothed_cv)
    if (length(best_idx) > 0L && !is.na(prof$smoothed_cv[best_idx])) {
      best_prec <- sprintf("%.1f%% CV @ %.3f",
                           prof$smoothed_cv[best_idx],
                           prof$concentration[best_idx])
    }
  }

  # Inflection with CI
  infl_ci <- NA_character_
  if (!is.null(infl_obj)) {
    infl_ci <- sprintf("%.4f [%.4f, %.4f]",
                       infl_obj$x_median, infl_obj$x_lower, infl_obj$x_upper)
  }

  # Asymmetry
  asym_txt <- NA_character_
  asym <- bayes_state$asymmetry
  if (!is.null(asym)) {
    asym_txt <- if (asym$prob_4pl > 0.8) {
      sprintf("Symmetric (P(4PL)=%.0f%%)", asym$prob_4pl * 100)
    } else if (asym$prob_4pl < 0.3) {
      sprintf("Asymmetric (P(4PL)=%.0f%%)", asym$prob_4pl * 100)
    } else {
      sprintf("Ambiguous (P(4PL)=%.0f%%)", asym$prob_4pl * 100)
    }
  }

  data.frame(
    plate            = pid,
    best_model       = best_model %||% NA_character_,
    stacking_wt      = best_weight %||% NA_character_,
    elpd             = elpd_val %||% NA_character_,
    lloq_20          = fmt(if (!is.null(prof_obj)) prof_obj$lloq_20 else NA),
    uloq_20          = fmt(if (!is.null(prof_obj)) prof_obj$uloq_20 else NA),
    dynamic_range_20 = dr_20 %||% NA_character_,
    lloq_15          = fmt(if (!is.null(prof_obj)) prof_obj$lloq_15 else NA),
    uloq_15          = fmt(if (!is.null(prof_obj)) prof_obj$uloq_15 else NA),
    dynamic_range_15 = dr_15 %||% NA_character_,
    best_precision   = best_prec %||% NA_character_,
    lod              = fmt(if (!is.null(lod_obj) && !is.na(lod_obj$lod)) lod_obj$lod else NA, 4),
    lrdl             = fmt(if (!is.null(lrdl_obj) && !is.na(lrdl_obj$lrdl)) lrdl_obj$lrdl else NA, 4),
    uod              = fmt(if (!is.null(uod_obj) && !is.na(uod_obj$uod)) uod_obj$uod else NA, 4),
    urdl             = fmt(if (!is.null(urdl_obj) && !is.na(urdl_obj$urdl)) urdl_obj$urdl else NA, 4),
    inflection_95ci  = infl_ci %||% NA_character_,
    lo2d             = fmt(if (!is.null(d2_obj)) d2_obj$lo2d_median else NA, 4),
    uo2d             = fmt(if (!is.null(d2_obj)) d2_obj$uo2d_median else NA, 4),
    asymmetry        = asym_txt %||% NA_character_,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
},
caption           = "Summary Statistics",
caption.placement = getOption("xtable.caption.placement", "top"),
striped           = TRUE,
hover             = TRUE,
bordered          = TRUE,
spacing           = "s")


# ============================================================================
# BAYESIAN — Model Comparison Modal Tables
# ============================================================================

# LOO-CV Comparison table (loo::loo_compare output)
output$bayes_loo_comparison_tbl <- renderTable({
  req(bayes_state$status == "ready")
  lc <- bayes_state$loo_comparison
  req(lc)

  # loo_compare returns a matrix; convert to data.frame with Model column
  df <- as.data.frame(lc)
  df$Model <- rownames(df)

  # Pretty-label the model names
  df$Model <- vapply(df$Model, function(nm) {
    switch(nm, "4pl" = "4PL", "5pl" = "5PL", gompertz = "Gompertz", nm)
  }, character(1))

  # Reorder columns: Model first
  df <- df[, c("Model", setdiff(names(df), "Model")), drop = FALSE]
  rownames(df) <- NULL
  df
},
striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
digits = 2)

# Stacking Weights table (for modal)
output$bayes_modal_stacking_tbl <- renderTable({
  req(bayes_state$status == "ready", bayes_state$stacking_weights)
  sw <- bayes_state$stacking_weights

  model_labels <- vapply(names(sw), function(nm) {
    switch(nm, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", nm)
  }, character(1))

  data.frame(
    Model  = model_labels,
    Weight = sprintf("%.4f", unlist(sw)),
    Best   = ifelse(names(sw) == bayes_state$best_family, "\u2605", ""),
    stringsAsFactors = FALSE
  )
},
striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

# Pareto k diagnostics table
output$bayes_pareto_k_tbl <- renderTable({
  req(bayes_state$status == "ready")
  pk <- bayes_state$pareto_k_summary
  req(pk)

  # Pretty-label family column
  pk$family <- vapply(pk$family, function(nm) {
    switch(nm, "4pl" = "4PL", "5pl" = "5PL", gompertz = "Gompertz", nm)
  }, character(1))

  names(pk) <- c("Model", "Good (k\u22640.5)", "Ok (0.5-0.7)",
                  "Bad (0.7-1.0)", "Very Bad (k>1)", "Max k")
  pk
},
striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
digits = 3)

# Per-plate ELPD breakdown table
output$bayes_modal_plate_elpd_tbl <- renderTable({
  req(bayes_state$status == "ready")
  pe <- bayes_state$plate_elpd
  req(pe)
  pbf <- bayes_state$plate_best_family

  df <- as.data.frame(pe)

  # Round the numeric columns
  for (col in names(df)) {
    df[[col]] <- sprintf("%.1f", df[[col]])
  }

  # Pretty-label column names
  names(df) <- vapply(names(df), function(nm) {
    switch(nm, "4pl" = "4PL", "5pl" = "5PL", gompertz = "Gompertz", nm)
  }, character(1))

  df$Plate <- rownames(pe)
  df$Best  <- vapply(rownames(pe), function(pid) {
    fam <- pbf[[pid]]
    switch(fam, "5pl" = "5PL", "4pl" = "4PL", gompertz = "Gompertz", fam)
  }, character(1))

  # Reorder: Plate first, then families, then Best
  fam_cols <- setdiff(names(df), c("Plate", "Best"))
  df <- df[, c("Plate", fam_cols, "Best"), drop = FALSE]
  rownames(df) <- NULL
  df
},
striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")


# Selected plate parameter comparison (for modal)
output$bayes_modal_plate_params_tbl <- renderTable({
  req(bayes_state$status == "ready")
  req(input$bayes_view_plate)

  pid <- input$bayes_view_plate

  # DB mode: use ensemble_data
  if (!is.null(bayes_state$ensemble_data)) {
    ens <- bayes_state$ensemble_data
    plate_rows <- ens[ens$plateid == pid, , drop = FALSE]
    if (nrow(plate_rows) == 0) return(NULL)

    fmt <- function(x, d = 3) if (is.na(x)) "—" else sprintf(paste0("%.", d, "f"), x)

    rows <- lapply(seq_len(nrow(plate_rows)), function(i) {
      r <- plate_rows[i, ]
      fam <- switch(as.character(r$family), "4pl" = "4PL", "5pl" = "5PL",
                    gompertz = "Gompertz", r$family)
      best <- if (isTRUE(r$is_plate_best)) "\u2605" else ""
      data.frame(
        Family = fam,
        Best   = best,
        a      = sprintf("%s [%s, %s]", fmt(r$a), fmt(r$a_lower), fmt(r$a_upper)),
        b      = sprintf("%s [%s, %s]", fmt(r$b), fmt(r$b_lower), fmt(r$b_upper)),
        c      = sprintf("%s [%s, %s]", fmt(r$c, 4), fmt(r$c_lower, 4), fmt(r$c_upper, 4)),
        d      = sprintf("%s [%s, %s]", fmt(r$d), fmt(r$d_lower), fmt(r$d_upper)),
        g      = if (r$family %in% c("4pl", "5pl")) {
          sprintf("%s [%s, %s]", fmt(r$g), fmt(r$g_lower), fmt(r$g_upper))
        } else { "\u2014" },
        ELPD   = fmt(r$plate_elpd, 1),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    do.call(rbind, rows)

  # Live fit mode: extract from Stan
  } else if (!is.null(bayes_state$assay)) {
    assay <- bayes_state$assay
    families <- names(assay$ensemble$fits)

    rows <- lapply(families, function(fam) {
      fit_obj <- assay$ensemble$fits[[fam]]
      if (is.null(fit_obj)) return(NULL)
      plate_map <- fit_obj$plate_map
      plate_idx <- which(as.character(plate_map$plateid) == pid)
      if (length(plate_idx) == 0) return(NULL)

      samples <- rstan::extract(fit_obj$fit)
      fmt_draw <- function(nm) {
        draws <- samples[[nm]][, plate_idx]
        sprintf("%.3f [%.3f, %.3f]", median(draws), quantile(draws, 0.025), quantile(draws, 0.975))
      }

      fam_lbl <- switch(fam, "4pl" = "4PL", "5pl" = "5PL", gompertz = "Gompertz", fam)
      pbf <- bayes_state$plate_best_family
      best <- if (!is.null(pbf) && pid %in% names(pbf) && pbf[[pid]] == fam) "\u2605" else ""

      pe <- bayes_state$plate_elpd
      elpd_val <- if (!is.null(pe) && pid %in% rownames(pe) && fam %in% colnames(pe)) {
        sprintf("%.1f", pe[pid, fam])
      } else { "—" }

      data.frame(
        Family = fam_lbl, Best = best,
        a = fmt_draw("a"), b = fmt_draw("b"),
        c = { draws <- samples$log_c[, plate_idx]; sprintf("%.4f [%.4f, %.4f]",
              median(exp(draws)), quantile(exp(draws), 0.025), quantile(exp(draws), 0.975)) },
        d = fmt_draw("d"),
        g = if (fam %in% c("4pl", "5pl")) fmt_draw("g") else "\u2014",
        ELPD = elpd_val,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  } else {
    NULL
  }
},
striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")


# ============================================================================
# CURVE COMPARISON — toggle + overlay
# ============================================================================

# Show / hide the comparison panel
observeEvent(input$btn_compare_curves, {
  comparison_visible(!isTRUE(comparison_visible()))
})

# Comparison panel UI (rendered outside the big navigation observer so it is
# always registered, but it only has content once comparison_visible() is TRUE)
output$comparison_panel <- renderUI({
  req(isTRUE(comparison_visible()))

  div(
    style = paste0(
      "background-color:#fffde7; border:2px solid #f9a825;",
      "border-radius:6px; padding:14px; margin-top:10px; margin-bottom:10px;"
    ),
    tags$h4(
      tags$span(
        style = "color:#e65100;",
        icon("balance-scale"), " Frequentist vs Bayesian — Curve Overlay"
      ),
      style = "margin-top:0; margin-bottom:8px;"
    ),
    uiOutput("comparison_plate_warning"),
    shinycssloaders::withSpinner(
      plotlyOutput("comparison_curve", width = "75vw", height = "800px"),
      type    = 6,
      color   = "#f9a825",
      caption = "Building overlay..."
    )
  )
})

# Warn when the two plate selectors are out of sync
output$comparison_plate_warning <- renderUI({
  req(isTRUE(comparison_visible()))
  freq_plate  <- input$sc_plate_select
  bayes_plate <- input$bayes_view_plate
  if (!identical(freq_plate, bayes_plate)) {
    div(
      class = "alert alert-warning",
      style = "padding:6px 10px; margin-bottom:8px;",
      icon("exclamation-triangle"),
      sprintf(
        " Frequentist plate (%s) and Bayesian plate (%s) differ — the overlay may not be directly comparable.",
        freq_plate %||% "none", bayes_plate %||% "none"
      )
    )
  }
})

# The overlay plot itself
output$comparison_curve <- renderPlotly({
  req(isTRUE(comparison_visible()))
  p_freq  <- freq_curve_plot_cache()
  req(p_freq)
  req(bayes_state$status == "ready")
  req(input$bayes_view_plate)
  p_bayes <- bayes_state$plots[[input$bayes_view_plate]]
  req(p_bayes)

  make_comparison_overlay(p_freq, p_bayes)
})


# ============================================================================
# HELPER — merge two plotly objects into one overlay
#
# Strategy: use plotly_build() to extract the raw trace lists from both plots,
# prefix every trace name with its method label, assign legend groups, then
# graft all Bayesian traces onto the built frequentist widget.  The frequentist
# layout (axis titles, range, etc.) is used as the base because it carries the
# properly-named response variable and log-scale flags already baked in.
# ============================================================================
make_comparison_overlay <- function(p_freq, p_bayes,
                                    freq_label  = "Frequentist (Robust NLS)",
                                    bayes_label = "Bayesian (Stan Ensemble)") {

  pb_f <- plotly::plotly_build(p_freq)
  pb_b <- plotly::plotly_build(p_bayes)

  # ── Tag frequentist traces ───────────────────────────────────────────────
  pb_f$x$data <- lapply(pb_f$x$data, function(tr) {
    tr$legendgroup      <- freq_label
    tr$legendgrouptitle <- list(text = freq_label)
    if (!is.null(tr$name) && nzchar(tr$name)) {
      tr$name <- paste0(tr$name, " [Freq]")
    }
    tr
  })

  # ── Tag & append Bayesian traces ─────────────────────────────────────────
  bayes_traces_tagged <- lapply(pb_b$x$data, function(tr) {
    tr$legendgroup      <- bayes_label
    tr$legendgrouptitle <- list(text = bayes_label)
    if (!is.null(tr$name) && nzchar(tr$name)) {
      tr$name <- paste0(tr$name, " [Bayes]")
    }
    # Bayesian uses log10 concentration on x; keep as-is — axes are compatible
    tr
  })
  pb_f$x$data <- c(pb_f$x$data, bayes_traces_tagged)

  # ── Update title ─────────────────────────────────────────────────────────
  pb_f$x$layout$title <- list(
    text = paste0(freq_label, " vs ", bayes_label, " — Standard Curve Overlay"),
    font = list(size = 15)
  )

  # ── Enable legend grouping ────────────────────────────────────────────────
  pb_f$x$layout$legend <- modifyList(
    pb_f$x$layout$legend %||% list(),
    list(groupclick = "toggleitem", tracegroupgap = 10)
  )

  pb_f
}
