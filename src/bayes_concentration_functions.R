
# ── Fetch tidy params for multiple glance IDs in one query ──────────────────
fetch_tidy_params_bulk <- function(study_accession, project_id,
                                   best_glance_snapshot, conn) {
  
  key_clauses <- apply(best_glance_snapshot, 1, function(r) {
    paste0(
      "(t.project_id = ", r[["project_id"]],
      " AND t.study_accession = '", r[["study_accession"]], "'",
      " AND t.experiment_accession = '", r[["experiment_accession"]], "'",
      " AND t.plate = '", r[["plate"]], "'",
      " AND t.nominal_sample_dilution = '", r[["nominal_sample_dilution"]], "'",
      " AND t.source = '", r[["source"]], "'",
      " AND t.antigen = '", r[["antigen"]], "'",
      " AND t.feature = '", r[["feature"]], "'",
      " AND t.wavelength = '", r[["wavelength"]], "')"
    )
  })
  
  where_clause <- paste(key_clauses, collapse = "\n  OR ")
  
  query <- paste0(
    "SELECT
       t.best_tidy_all_id,
       t.project_id,
       t.study_accession,
       t.experiment_accession,
       t.term,
       t.estimate,
       t.std_error,
       t.lower,
       t.upper,
       t.plateid,
       t.plate,
       t.source,
       t.antigen,
       t.nominal_sample_dilution,
       t.feature,
       t.wavelength
     FROM madi_results.best_tidy_all t
     WHERE ", where_clause, ";"
  )
  
  message("[tidy_fetch] query:\n", query)
  
  result <- dbGetQuery(conn, query)
  message("[tidy_fetch] returned ", nrow(result), " rows for ",
          nrow(best_glance_snapshot), " glance rows")
  result
}

# fetch_tidy_params_bulk <- function(study_accession, project_id,
#                                    best_glance_ids, conn) {
#   ids_sql <- paste(as.integer(best_glance_ids), collapse = ",")
#   query <- glue::glue(
#     "SELECT
#        t.best_tidy_all_id,
#        t.best_glance_all_id,
#        t.project_id,
#        t.study_accession,
#        t.experiment_accession,
#        t.term,
#        t.estimate,
#        t.std_error,
#        t.lower,
#        t.upper
#      FROM madi_results.best_tidy_all t
#      WHERE t.project_id       = {project_id}
#        AND t.study_accession  = '{study_accession}'
#        AND t.best_glance_all_id IN ({ids_sql});"
#   )
#   result <- dbGetQuery(conn, query)
#   message(sprintf("[tidy_fetch] returned %d rows for %d IDs", 
#                   nrow(result), length(best_glance_ids)))
#   result
# }


# ── Draw parameter samples using std_error from best_tidy_all ───────────────
draw_param_samples <- function(tidy_df, n_draws, model_name) {
  
  params_needed <- switch(
    trimws(model_name),
    "Y5"     = c("a","b","c","d","g"),
    "Yd5"    = c("a","b","c","d","g"),
    "Y4"     = c("a","b","c","d"),
    "Yd4"    = c("a","b","c","d"),
    "Ygomp4" = c("a","b","c","d"),
    stop("Unknown model: ", model_name)
  )
  
  tidy_df <- tidy_df[tidy_df$term %in% params_needed, ]
  
  draws <- sapply(params_needed, function(p){
    row <- tidy_df[tidy_df$term == p, ]
    if(nrow(row) == 0) stop("Missing parameter in tidy_df: ", p)
    
    # Bounds from CI to respect model constraints
    lower <- if(!is.na(row$lower)) row$lower else -Inf
    upper <- if(!is.na(row$upper)) row$upper else  Inf
    
    # Draw from normal centred on estimate, clamp to [lower, upper]
    raw <- rnorm(n_draws, mean = row$estimate, sd = row$std_error)
    pmax(pmin(raw, upper), lower)
  })
  
  as.data.frame(draws)
}

# ============================================================
# Model string factory — curve params are DATA, only x is sampled
# Uses dunif prior on x instead of hierarchical dnorm
# ============================================================
#sigma ~ dunif(0, sigma_upper)
get_jags_calibration_model <- function(){
  
  "
model{

  for(i in 1:N){

    y[i] ~ dt(mu[i], tau, nu)

    mu[i] <- y_hat[i]

  }


  tau <- pow(sigma,-2)

  nu ~ dunif(2,30)

}
"

}
# ============================================================
# Forward function factory
# ============================================================
get_forward_function <- function(model_name) {
  switch(
    trimws(model_name),
    "Y5"     = function(x, p) p["d"] + (p["a"] - p["d"]) * (1 + exp((x - p["c"]) / p["b"]))^(-p["g"]),
    "Y4"     = function(x, p) p["d"] + (p["a"] - p["d"]) / (1 + exp((x - p["c"]) / p["b"])),
    "Yd5"    = function(x, p) p["a"] + (p["d"] - p["a"]) * (1 + p["g"] * exp(-p["b"] * (x - p["c"])))^(-1 / p["g"]),
    "Yd4"    = function(x, p) p["a"] + (p["d"] - p["a"]) / (1 + exp(-p["b"] * (x - p["c"]))),
    "Ygomp4" = function(x, p) p["a"] + (p["d"] - p["a"]) * exp(-exp(-p["b"] * (x - p["c"]))),
    stop("Unsupported model_name: ", model_name)
  )
}

# ============================================================
# Extract named param vector from a best_glance row
# Only includes g for 5PL models
# ============================================================
extract_params <- function(glance_row) {
  p <- c(a = glance_row$a,
         b = glance_row$b,
         c = glance_row$c,
         d = glance_row$d)
  
  if (trimws(glance_row$model_name) %in% c("Y5", "Yd5")) {
    p["g"] <- glance_row$g
  }
  
  return(p)
}



invert_response_fast <- function(y_obs, params, model_name, x_min, x_max){
  
  fwd <- get_forward_function(model_name)
  
  x_grid <- seq(x_min, x_max, length.out = 3000)
  
  y_grid <- fwd(x_grid, params)
  
  keep <- is.finite(y_grid)
  
  x_grid <- x_grid[keep]
  y_grid <- y_grid[keep]
  
  # ensure monotonic order
  if(y_grid[1] > y_grid[length(y_grid)]){
    y_grid <- rev(y_grid)
    x_grid <- rev(x_grid)
  }
  
  # remove duplicate y values (prevents approx warning)
  dup <- !duplicated(y_grid)
  y_grid <- y_grid[dup]
  x_grid <- x_grid[dup]
  
  approx(
    x = y_grid,
    y = x_grid,
    xout = y_obs,
    rule = 2
  )$y
}
# ============================================================
# Numerical inversion + adaptive prior variance
# ============================================================

# compute_x_init <- function(model_name, params, resid_var, y_obs,
#                            x_min, x_max, conc_range) {
#   fwd <- get_forward_function(model_name)
# 
#   
#   # Invert curve at each y
#   x_init <- vapply(y_obs, function(yi) {
#     tryCatch(
#       uniroot(function(x) fwd(x, params) - yi,
#               interval = c(x_min, x_max), tol = 1e-8)$root,
#       error = function(e) NA_real_
#     )
#   }, numeric(1))
# 
#   x_init[is.na(x_init)] <- (x_min + x_max) / 2
#   margin <- (x_max - x_min) * 0.001
#   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
# 
#   # Slope at each x_init
#   slope <- vapply(x_init, function(xi) {
#     h <- 1e-5
#     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
#   }, numeric(1))
# 
#   # Max slope across curve
#   x_grid <- seq(x_min, x_max, length.out = 500)
#   max_slope <- max(vapply(x_grid, function(xi) {
#     h <- 1e-5
#     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
#   }, numeric(1)), na.rm = TRUE)
# 
#   slope_ratio <- pmin(abs(slope) / max_slope, 1.0)
# 
#   # Delta-method variance
#   delta_var <- (resid_var / slope)^2
#   min_var <- (conc_range * 0.001)^2
#   max_var <- (conc_range * 2)^2
#   delta_var <- pmin(pmax(delta_var, min_var), max_var)
#   delta_var[is.na(delta_var)] <- max_var
# 
#   wide_var <- (conc_range * 1.0)^2
# 
#   # Sigmoidal blend
#   weight <- 1 / (1 + exp(-20 * (slope_ratio - 0.15)))
#   per_sample_variance <- weight * delta_var + (1 - weight) * wide_var
# 
#   list(
#     x_init              = x_init,
#     per_sample_variance = per_sample_variance,
#     slope_ratio         = slope_ratio,
#     weight_informative  = weight,
#     prior_type          = ifelse(weight > 0.5, "delta_method", "wide_prior")
#   )
# }
# compute_x_init <- function(model_name, params, resid_var, y_obs,
#                            x_min, x_max, conc_range,
#                            sc_x_min = NULL, sc_x_max = NULL) {
#   fwd <- get_forward_function(model_name)
#   
#   # Default SC bounds if not supplied
#   if (is.null(sc_x_min)) sc_x_min <- x_min + conc_range
#   if (is.null(sc_x_max)) sc_x_max <- x_max - conc_range
#   sc_mid <- (sc_x_min + sc_x_max) / 2
#   
#   # SC endpoint responses for directional fallback
#   y_at_sc_min  <- fwd(sc_x_min, params)
#   y_at_sc_max  <- fwd(sc_x_max, params)
#   y_lower_asym <- min(y_at_sc_min, y_at_sc_max)
#   y_upper_asym <- max(y_at_sc_min, y_at_sc_max)
#   x_for_low_y  <- if (y_at_sc_min < y_at_sc_max) sc_x_min else sc_x_max
#   x_for_high_y <- if (y_at_sc_min < y_at_sc_max) sc_x_max else sc_x_min
#   
#   # Invert curve at each y
#   x_init <- vapply(y_obs, function(yi) {
#     tryCatch(
#       uniroot(function(x) fwd(x, params) - yi,
#               interval = c(x_min, x_max), tol = 1e-8)$root,
#       error = function(e) NA_real_
#     )
#   }, numeric(1))
#   
#   # OLD — replaced:
#   # x_init[is.na(x_init)] <- (x_min + x_max) / 2
#   
#   # NEW — goes here, directly after the vapply block:
#   na_idx <- which(is.na(x_init))
#   for (i in na_idx) {
#     yi <- y_obs[i]
#     x_init[i] <- if (is.na(yi) || !is.finite(yi)) {
#       sc_mid
#     } else if (yi <= y_lower_asym) {
#       x_for_low_y - conc_range * 0.1
#     } else if (yi >= y_upper_asym) {
#       x_for_high_y + conc_range * 0.1
#     } else {
#       sc_mid
#     }
#   }
#   
#   # rest unchanged from your file [1]
#   margin <- (x_max - x_min) * 0.001
#   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
#   
#   # Slope at each x_init
#   slope <- vapply(x_init, function(xi) {
#     h <- 1e-5
#     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
#   }, numeric(1))
#   
#   # Max slope across curve
#   x_grid <- seq(x_min, x_max, length.out = 500)
#   max_slope <- max(vapply(x_grid, function(xi) {
#     h <- 1e-5
#     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
#   }, numeric(1)), na.rm = TRUE)
#   
#   slope_ratio <- pmin(abs(slope) / max_slope, 1.0)
#   
#   # Delta-method variance
#   delta_var <- (resid_var / slope)^2
#   min_var <- (conc_range * 0.001)^2
#   max_var <- (conc_range * 2)^2
#   delta_var <- pmin(pmax(delta_var, min_var), max_var)
#   delta_var[is.na(delta_var)] <- max_var
#   
#   wide_var <- (conc_range * 1.0)^2
#   
#   # Sigmoidal blend
#   weight <- 1 / (1 + exp(-20 * (slope_ratio - 0.15)))
#   per_sample_variance <- weight * delta_var + (1 - weight) * wide_var
#   
#   list(
#     x_init              = x_init,
#     per_sample_variance = per_sample_variance,
#     slope_ratio         = slope_ratio,
#     weight_informative  = weight,
#     prior_type          = ifelse(weight > 0.5, "delta_method", "wide_prior")
#   )
# }
# ============================================================
# Main function
#
#   glance_row   : single row from best_glance
#   best_pred_df : rows from best_pred for this glance_row
#                  (x column used for concentration bounds)
#   sample_df    : dataframe to predict on
#                  - pred grid:  best_pred rows, response_col = "yhat"
#                  - samples:    best_sample rows, response_col = "assay_response"
#   response_col : column name holding the response values
# ============================================================
run_jags_predicted_concentration <- function(
    glance_row,
    best_pred_df,
    sample_df,
    response_col,
    tidy_df         = NULL,
    adapt_steps     = 500,
    burn_in_steps   = 2000,
    num_saved_steps = 10000,
    thin_steps      = 2,
    n_chains        = 3,
    verbose         = TRUE
){
  # ── 1. Setup ─────────────────────────────────────────────────────────────
  model_name  <- trimws(glance_row$model_name)
  params      <- extract_params(glance_row)
  fwd         <- get_forward_function(model_name)
  y_obs       <- sample_df[[response_col]]
  
  conc_range  <- diff(range(best_pred_df$concentration))
  x_min       <- min(best_pred_df$concentration) - conc_range * 2
  x_max       <- max(best_pred_df$concentration) + conc_range * 2
  
  if (verbose) {
    cat("[JAGS] best_pred_df nrow:", nrow(best_pred_df), "\n")
    cat("[JAGS] best_pred_df colnames:", paste(names(best_pred_df), collapse = ", "), "\n")
    if (nrow(best_pred_df) > 0) {
      cat("[JAGS] best_pred_df head:\n")
      print(head(best_pred_df, 3))
    }
  }
  
  if (verbose) {
    cat(paste0(
      "\n[JAGS] model=", model_name,
      " | n_obs=", length(y_obs),
      " | x_min=", round(x_min, 3),
      " | x_max=", round(x_max, 3), "\n"
    ))
  }
  
  # ── 2. Point estimate: invert y_obs → x_est (log10 concentration) ────────
  x_est <- invert_response_fast(
    y_obs, params, model_name, x_min, x_max
  )
  y_hat <- fwd(x_est, params)
  
  if (verbose) {
    cat(paste0(
      "[JAGS] x_est range: [",
      round(min(x_est, na.rm = TRUE), 4), ", ",
      round(max(x_est, na.rm = TRUE), 4), "]\n"
    ))
  }
  
  # ── 3. JAGS: estimate sigma + nu from STANDARDS residuals ────────────────
  # Key fix: use pred_se rows (standards on the fitted curve) so that
  # y_standards - fwd(x_standards) are real NLS residuals, not near-zero
  # sample inversion residuals.
  resid_sigma <- sqrt(glance_row$resid_sample_variance)
  
  y_standards     <- best_pred_df[["yhat"]]
  x_standards     <- best_pred_df[["concentration"]]
  y_hat_standards <- fwd(x_standards, params)
  
  if (verbose) {
    cat(paste0(
      "[JAGS] resid_sigma from glance = ", round(resid_sigma, 4),
      " | n_standards = ", length(y_standards), "\n"
    ))
  }
  
  data_list <- list(
    y           = y_standards,
    y_hat       = y_hat_standards,
    N           = length(y_standards),
    sigma      = resid_sigma
    #sigma_upper = resid_sigma * 5
  )
  
  init_list <- lapply(seq_len(n_chains), function(i){
    list(
      #sigma = resid_sigma * runif(1, 0.8, 1.2),
      nu    = runif(1, 3, 15)
    )
  })
  
  jm <- rjags::jags.model(
    textConnection(get_jags_calibration_model()),
    data     = data_list,
    inits    = init_list,
    n.chains = n_chains,
    n.adapt  = adapt_steps,
    quiet    = !verbose
  )
  
  update(jm, burn_in_steps)
  
  samps <- rjags::coda.samples(
    jm,
    variable.names = c("nu"), 
    n.iter = num_saved_steps,
    thin   = thin_steps
  )
  
  chain   <- as.matrix(samps)
  n_draws <- nrow(chain)
  chain_nu <- chain[, "nu"] 
  
  if (verbose) {
    cat(paste0(
      "[JAGS] fixed sigma = ", round(resid_sigma, 4),
      " | posterior nu: mean = ", round(mean(chain_nu), 4),
      " | sd = ", round(sd(chain_nu), 4), "\n"
    ))
    # cat(paste0(
    #   "[JAGS] resid_sigma = ", round(resid_sigma, 4),
    #   
    #   # " | posterior sigma: mean = ", round(mean(chain[, "sigma"]), 4),
    #   # " | sd = ", round(sd(chain[, "sigma"]), 4), "\n"
    # ))
    # cat(paste0(
    #   "[JAGS] posterior nu: mean = ", round(mean(chain[, "nu"]), 4),
    #   " | sd = ", round(sd(chain[, "nu"]), 4), "\n"
    # ))
  }
  
  # ── 4. Parameter draws from best_tidy_all std_error (if available) ────────
  param_draws           <- NULL
  use_param_uncertainty <- FALSE
  
  if (!is.null(tidy_df) && nrow(tidy_df) > 0) {
    param_draws <- tryCatch(
      draw_param_samples(tidy_df, n_draws, model_name),
      error = function(e) {
        message(paste0(
          "[JAGS] draw_param_samples failed (", e$message,
          ") — falling back to fixed params."
        ))
        NULL
      }
    )
    use_param_uncertainty <- !is.null(param_draws)
  }
  
  if (verbose) {
    if (use_param_uncertainty) {
      cat("\n[JAGS] Parameter uncertainty: YES (from best_tidy_all std_error)\n")
      cat("[JAGS] Parameter draw summary:\n")
      print(sapply(param_draws, function(x)
        c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
      ))
    } else {
      cat("[JAGS] Parameter uncertainty: NO (fixed params only)\n")
    }
  }
  
  # ── 5. Predictive draws: propagate parameter + residual uncertainty ───────
  # ── 5. Predictive draws: propagate parameter + residual uncertainty ───────
  # ── 5. Predictive draws ───────────────────────────────────────────────────
  conc_draws <- matrix(NA, nrow = n_draws, ncol = length(x_est))
  n_obs      <- length(y_obs)
  
  # Standard curve bounds for slope lookup
  sc_x_min <- min(best_pred_df$concentration)
  sc_x_max <- max(best_pred_df$concentration)
  
  # Clamp x_est to SC range for slope computation ONLY
  # Samples outside the range get slope from the nearest SC endpoint,
  # which correctly gives them a large-but-finite x_se
  x_est_for_slope <- pmax(pmin(x_est, sc_x_max), sc_x_min)
  
  # Compute local slope at clamped position
  h           <- 1e-5
  local_slope <- vapply(x_est_for_slope, function(xi) {
    abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
  }, numeric(1))
  
  max_slope           <- max(local_slope, na.rm = TRUE)
  slope_floor         <- max_slope * 0.05
  local_slope_floored <- pmax(local_slope, slope_floor)
  
  # Delta-method SE in x-space, with absolute cap at 2x conc_range
  x_se <- pmin(resid_sigma / local_slope_floored, conc_range * 2)
  
  if (verbose) {
    cat("\n[JAGS] slope-scaled x_se summary (clamped):\n")
    print(summary(x_se))
    cat("[JAGS] n samples outside SC range:",
        sum(x_est < sc_x_min | x_est > sc_x_max), "\n")
  }
  
  # Draw bounds: allow 1x conc_range outside SC on each side
  draw_x_min <- sc_x_min - conc_range
  draw_x_max <- sc_x_max + conc_range
  
  for (i in seq_len(n_draws)) {
    p_i <- if (use_param_uncertainty) unlist(param_draws[i, ]) else params
    
    x_est_i <- if (use_param_uncertainty) {
      tryCatch(
        invert_response_fast(y_obs, p_i, model_name, x_min, x_max),
        error = function(e) x_est
      )
    } else {
      x_est
    }
    
    x_noise        <- rt(n_obs, df = pmax(chain_nu[i], 2.001)) * x_se
    conc_draws[i,] <- pmax(pmin(x_est_i + x_noise, draw_x_max), draw_x_min)
    
    # conc_draws[i,] <- x_est_i + x_noise
  }
  # conc_draws <- matrix(NA, nrow = n_draws, ncol = length(x_est))
  # n_obs      <- length(y_obs)
  # 
  # # Pre-compute t-distributed noise using posterior nu per draw
  # # rt gives heavier tails than rnorm — the robust part
  # t_noise_mat <- matrix(NA, nrow = n_draws, ncol = n_obs)
  # for (i in seq_len(n_draws)) {
  #   t_noise_mat[i, ] <- rt(n_obs, df = pmax(chain_nu[i], 2.001)) * resid_sigma
  # }
  # 
  # for (i in seq_len(n_draws)) {
  #   
  #   p_i <- if (use_param_uncertainty) {
  #     unlist(param_draws[i, ])
  #   } else {
  #     params
  #   }
  #   
  #   y_hat_i <- tryCatch(
  #     fwd(x_est, p_i),
  #     error = function(e) y_hat
  #   )
  #   
  #   # t-distributed noise with correct resid_sigma magnitude
  #   y_sim_i <- y_hat_i + t_noise_mat[i, ]
  #   
  #   # Always invert with fixed params for stable single U-shape
  #   conc_draws[i, ] <- tryCatch(
  #     invert_response_fast(y_sim_i, params, model_name, x_min, x_max),
  #     error = function(e) x_est
  #   )
  # }
  # conc_draws <- matrix(NA, nrow = n_draws, ncol = length(x_est))
  # n_obs      <- length(y_obs)
  # 
  # # Pre-compute all noise at once — vectorized, no inner loop
  # sigma_draws <- chain[, "sigma"]   # posterior sigma per draw
  # t_noise_mat <- matrix(
  #   rnorm(n_draws * n_obs, mean = 0, sd = rep(sigma_draws, each = n_obs)),
  #   nrow  = n_draws,
  #   ncol  = n_obs,
  #   byrow = TRUE
  # )
  # 
  # for (i in seq_len(n_draws)) {
  #   
  #   p_i <- if (use_param_uncertainty) {
  #     unlist(param_draws[i, ])
  #   } else {
  #     params
  #   }
  #   
  #   # Perturb y using fixed params for y_hat + noise
  #   # CRITICAL: always use fixed params for y_hat so parameter uncertainty
  #   # shifts y_hat slightly but inversion stays on one stable curve
  #   y_hat_i <- tryCatch(
  #     fwd(x_est, p_i),
  #     error = function(e) y_hat
  #   )
  #   
  #   y_sim_i <- y_hat_i + t_noise_mat[i, ]
  #   
  #   # ALWAYS invert with fixed params — this is what keeps the U-shape
  #   # single and smooth. Inverting with p_i creates two separate curves.
  #   conc_draws[i, ] <- tryCatch(
  #     invert_response_fast(y_sim_i, params, model_name, x_min, x_max),
  #     error = function(e) x_est
  #   )
  # }
  # for (i in seq_len(n_draws)) {
  #   
  #   p_i <- if (use_param_uncertainty) {
  #     unlist(param_draws[i, ])
  #   } else {
  #     params
  #   }
  #   
  #   # Forward predict at sample x_est using this draw's parameters
  #   y_hat_i <- tryCatch(
  #     fwd(x_est, p_i),
  #     error = function(e) y_hat
  #   )
  #   
  #   # Add residual noise from JAGS posterior sigma — now meaningful
  #   # because sigma was estimated from real standards residuals
  #   y_sim_i <- rnorm(n_obs, mean = y_hat_i, sd = chain[i, "sigma"])
  #   
  #   # ALWAYS invert using FIXED point-estimate params, not perturbed p_i
  #   # This keeps the inversion grid stable across draws, giving smooth pc
  #   conc_draws[i, ] <- tryCatch(
  #     invert_response_fast(y_sim_i, p_i, model_name, x_min, x_max),
  #     error = function(e) x_est
  #   )
  # }
  
  # ── 6. Summaries on log10-concentration scale ─────────────────────────────
  sample_df$raw_robust_concentration <- apply(conc_draws, 2, median)
  sample_df$se_robust_concentration  <- apply(conc_draws, 2, sd)
  
  # ── 7. CV matching propagate_error formula exactly ────────────────────────
  sample_df$pcov_robust_concentration <- apply(conc_draws, 2, function(x_log10){
    pmin(sd(x_log10) * log(10) * 100, 150)
  })
  
  # ── 8. Diagnostics ────────────────────────────────────────────────────────
  if (verbose) {
    cat("\n── Concentration summary (log10 scale) ───────────────────────────\n")
    cat("raw_robust_concentration (log10):\n")
    print(summary(sample_df$raw_robust_concentration))
    
    cat("\nse_robust_concentration (log10):\n")
    print(summary(sample_df$se_robust_concentration))
    
    cat("\npcov_robust_concentration (%):\n")
    print(summary(sample_df$pcov_robust_concentration))
    
    if ("pcov" %in% names(sample_df)) {
      cat("\nInterpolated pcov (%) for comparison:\n")
      print(summary(sample_df$pcov))
      cat(paste0(
        "\nMedian CV — MCMC robust: ",
        round(median(sample_df$pcov_robust_concentration, na.rm = TRUE), 2),
        "% | Interpolated: ",
        round(median(sample_df$pcov, na.rm = TRUE), 2), "%\n"
      ))
    }
    cat("──────────────────────────────────────────────────────────────────\n")
  }
  
  sample_df
}
# run_jags_predicted_concentration <- function(
#     glance_row,
#     best_pred_df,
#     sample_df,
#     response_col,
#     tidy_df         = NULL,        # from best_tidy_all; NULL = no parameter uncertainty
#     adapt_steps     = 500,
#     burn_in_steps   = 2000,
#     num_saved_steps = 10000,
#     thin_steps      = 2,
#     n_chains        = 3,
#     verbose         = TRUE
# ){
#   # ── 1. Setup ─────────────────────────────────────────────────────────────
#   model_name  <- trimws(glance_row$model_name)
#   params      <- extract_params(glance_row)
#   fwd         <- get_forward_function(model_name)
#   y_obs       <- sample_df[[response_col]]
#   
#   conc_range  <- diff(range(best_pred_df$concentration))
#   x_min       <- min(best_pred_df$concentration) - conc_range * 2
#   x_max       <- max(best_pred_df$concentration) + conc_range * 2
#   
#   if (verbose) {
#     cat(sprintf(
#       "\n[JAGS] model=%s | n_obs=%d | x_min=%.3f | x_max=%.3f\n",
#       model_name, length(y_obs), x_min, x_max
#     ))
#   }
#   
#   # ── 2. Point estimate: invert y_obs → x_est (log10 concentration) ────────
#   x_est <- invert_response_fast(
#     y_obs, params, model_name, x_min, x_max
#   )
#   y_hat <- fwd(x_est, params)
#   
#   if (verbose) {
#     cat(sprintf(
#       "[JAGS] x_est range: [%.4f, %.4f]\n",
#       min(x_est, na.rm = TRUE), max(x_est, na.rm = TRUE)
#     ))
#   }
#   
#   # ── 3. JAGS: estimate posterior of sigma (residual SD) and nu (df) ────────
#   resid_sigma <- sqrt(glance_row$resid_sample_variance)
#   
#   data_list <- list(
#     y           = y_obs,
#     y_hat       = y_hat,
#     N           = length(y_obs),
#     sigma_upper = resid_sigma * 5
#   )
#   
#   init_list <- lapply(seq_len(n_chains), function(i){
#     list(
#       sigma = resid_sigma * runif(1, 0.8, 1.2),
#       nu    = runif(1, 3, 15)
#     )
#   })
#   
#   jm <- rjags::jags.model(
#     textConnection(get_jags_calibration_model()),
#     data     = data_list,
#     inits    = init_list,
#     n.chains = n_chains,
#     n.adapt  = adapt_steps,
#     quiet    = !verbose
#   )
#   
#   update(jm, burn_in_steps)
#   
#   samps <- rjags::coda.samples(
#     jm,
#     variable.names = c("sigma", "nu"),
#     n.iter = num_saved_steps,
#     thin   = thin_steps
#   )
#   
#   chain   <- as.matrix(samps)
#   n_draws <- nrow(chain)
#   
#   if (verbose) {
#     cat(sprintf(
#       "[JAGS] resid_sigma = %.4f | posterior sigma: mean = %.4f | sd = %.4f\n",
#       resid_sigma,
#       mean(chain[, "sigma"]),
#       sd(chain[, "sigma"])
#     ))
#     cat(sprintf(
#       "[JAGS] posterior nu:    mean = %.4f | sd = %.4f\n",
#       mean(chain[, "nu"]),
#       sd(chain[, "nu"])
#     ))
#   }
#   
#   # ── 4. Parameter draws from best_tidy_all std_error (if available) ────────
#   # Initialise both as safe defaults — no <<- needed anywhere
#   param_draws           <- NULL
#   use_param_uncertainty <- FALSE
#   
#   if (!is.null(tidy_df) && nrow(tidy_df) > 0) {
#     param_draws <- tryCatch(
#       draw_param_samples(tidy_df, n_draws, model_name),
#       error = function(e) {
#         message(sprintf(
#           "[JAGS] draw_param_samples failed (%s) — falling back to fixed params.",
#           e$message
#         ))
#         NULL    # NULL return drives the fallback; no <<- needed
#       }
#     )
#     # TRUE only if draw actually succeeded
#     use_param_uncertainty <- !is.null(param_draws)
#   }
#   
#   if (verbose) {
#     if (use_param_uncertainty) {
#       cat("\n[JAGS] Parameter uncertainty: YES (from best_tidy_all std_error)\n")
#       cat("[JAGS] Parameter draw summary:\n")
#       print(sapply(param_draws, function(x)
#         c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
#       ))
#     } else {
#       cat("[JAGS] Parameter uncertainty: NO (fixed params only)\n")
#     }
#   }
#   
#   # ── 5. Predictive draws: propagate parameter + residual uncertainty ───────
#   conc_draws <- matrix(NA, nrow = n_draws, ncol = length(x_est))
#   n_obs      <- length(y_obs)
#   
#   for (i in seq_len(n_draws)) {
#     
#     # Parameter set: perturbed (with param uncertainty) or fixed (without)
#     p_i <- if (use_param_uncertainty) {
#       unlist(param_draws[i, ])
#     } else {
#       params
#     }
#     
#     # Forward predict y_hat using this draw's parameters
#     y_hat_i <- tryCatch(
#       fwd(x_est, p_i),
#       error = function(e) y_hat      # fallback to point-estimate y_hat
#     )
#     
#     # Add residual noise from JAGS posterior sigma draw
#     y_sim_i <- rnorm(n_obs, mean = y_hat_i, sd = chain[i, "sigma"])
#     
#     # Invert perturbed y_sim back to log10 concentration
#     conc_draws[i, ] <- tryCatch(
#       invert_response_fast(y_sim_i, p_i, model_name, x_min, x_max),
#       error = function(e) x_est      # fallback to point estimate
#     )
#   }
#   
#   # ── 6. Summaries on log10-concentration scale ─────────────────────────────
#   sample_df$raw_robust_concentration <- apply(conc_draws, 2, median)
#   sample_df$se_robust_concentration  <- apply(conc_draws, 2, sd)
#   
#   # ── 7. CV matching propagate_error formula exactly ────────────────────────
#   # propagate_error uses: CV = se_log10 * ln(10) * 100
#   # conc_draws columns are in log10 units so sd(col) = se_log10
#   # cap at 150 matching propagate_error cap
#   sample_df$pcov_robust_concentration <- apply(conc_draws, 2, function(x_log10){
#     pmin(sd(x_log10) * log(10) * 100, 150)
#   })
#   
#   # ── 8. Diagnostics ────────────────────────────────────────────────────────
#   if (verbose) {
#     cat("\n── Concentration summary (log10 scale) ───────────────────────────\n")
#     cat("raw_robust_concentration (log10):\n")
#     print(summary(sample_df$raw_robust_concentration))
#     
#     cat("\nse_robust_concentration (log10):\n")
#     print(summary(sample_df$se_robust_concentration))
#     
#     cat("\npcov_robust_concentration (%):\n")
#     print(summary(sample_df$pcov_robust_concentration))
#     
#     if ("pcov" %in% names(sample_df)) {
#       cat("\nInterpolated pcov (%) for comparison:\n")
#       print(summary(sample_df$pcov))
#       cat(sprintf(
#         "\nMedian CV — MCMC robust: %.2f%% | Interpolated: %.2f%%\n",
#         median(sample_df$pcov_robust_concentration, na.rm = TRUE),
#         median(sample_df$pcov,                      na.rm = TRUE)
#       ))
#     }
#     cat("──────────────────────────────────────────────────────────────────\n")
#   }
#   
#   sample_df
# }


## older

# run_jags_predicted_concentration <- function(
#     glance_row,
#     best_pred_df,
#     sample_df,
#     response_col,
#     adapt_steps = 500,
#     burn_in_steps = 2000,
#     num_saved_steps = 10000,
#     thin_steps = 2,
#     n_chains = 3,
#     verbose = TRUE
# ){
#   
#   model_name <- trimws(glance_row$model_name)
#   
#   params <- extract_params(glance_row)
#   
#   fwd <- get_forward_function(model_name)
#   
#   y_obs <- sample_df[[response_col]]
#   
#   conc_range <- diff(range(best_pred_df$concentration))
#   
#   x_min <- min(best_pred_df$concentration) - conc_range*2
#   x_max <- max(best_pred_df$concentration) + conc_range*2
#   
#   # Posterior inversion
#   x_est <- invert_response_fast(
#     y_obs,
#     params,
#     model_name,
#     x_min,
#     x_max
#   )
#   
#   y_hat <- fwd(x_est, params)
#   
#   resid_sigma <- sqrt(glance_row$resid_sample_variance)
#   
#   data_list <- list(
#     y = y_obs,
#     y_hat = y_hat,
#     N = length(y_obs),
#     sigma_upper = resid_sigma * 5
#   )
#   
#   model_string <- get_jags_calibration_model()
#   
#   init_list <- lapply(seq_len(n_chains), function(i){
#     
#     list(
#       sigma = resid_sigma * runif(1,0.8,1.2),
#       nu = runif(1,3,15)
#     )
#     
#   })
#   
#   jm <- rjags::jags.model(
#     textConnection(model_string),
#     data = data_list,
#     inits = init_list,
#     n.chains = n_chains,
#     n.adapt = adapt_steps,
#     quiet = !verbose
#   )
#   
#   update(jm, burn_in_steps)
#   
#   samps <- rjags::coda.samples(
#     jm,
#     variable.names = c("sigma","nu"),
#     n.iter = num_saved_steps,
#     thin = thin_steps
#   )
#   
#   chain <- as.matrix(samps)
#   
#   n_draws <- nrow(chain)
#   
#   conc_draws <- matrix(
#     NA,
#     nrow = n_draws,
#     ncol = length(x_est)
#   )
#   
#   for(i in seq_len(n_draws)){
#     
#     y_sim <- rnorm(
#       length(y_hat),
#       mean = y_hat,
#       sd = chain[i,"sigma"]
#     )
#     
#     conc_draws[i,] <- invert_response_fast(
#       y_sim,
#       params,
#       model_name,
#       x_min,
#       x_max
#     )
#     
#   }
#   
#   sample_df$raw_robust_concentration <- apply(conc_draws,2,median)
#   
#   sample_df$se_robust_concentration <- apply(conc_draws,2,sd)
#   
#   chain_original <- 10^conc_draws
#   
#   sample_df$pcov_robust_concentration <- apply(chain_original,2,function(x){
#     
#     pmin(sd(x)/abs(mean(x))*100,125)
#     
#   })
#   
#   sample_df
#   
# }

###### Shiny Side 
process_jag_result <- function(df, df_name = c("pred_se", "sample_se")) {
  
  df_name <- match.arg(df_name)
  
  if (df_name == "pred_se") {
    names(df)[names(df) == "row_id"] <- "best_pred_all_id"
    df <- df[, !names(df) %in% c("dilution", "best_glance_all_id", "concentration", "assay_response", "mcmc_set")]
    
    
  } else if (df_name == "sample_se") {
    names(df)[names(df) == "row_id"] <- "best_sample_se_all_id"
    df <- df[, !names(df) %in% c("concentration", "dilution", "assay_response", "best_glance_all_id", "mcmc_set")]
  }
  
  return(df)
}

filter_glance_scope <- function(df, scope, experiment, plate) {
  
  switch(
    scope,
    plate      = df[df$experiment_accession == experiment &
                      df$plate_nom == plate, ],
    
    experiment = df[df$experiment_accession == experiment, ],
    
    study      = df
  )
}

update_mcmc_progress <- function(i, total, row) {
  
  msg <- paste0(
    "MCMC Robust: ", i, " / ", total, "\n",
    "Study: ",      row$study_accession, "\n",
    "Experiment: ", row$experiment_accession, "\n",
    "Plate: ",      row$plate_nom, "\n",
    "Antigen: ",    row$antigen, "\n",
    "Model: ",      row$model_name
  )
  
  showNotification(
    id = "mcmc_calc_notify",
    div(class = "big-notification",
        style = "white-space: pre-line;",
        msg),
    duration = NULL
  )
}




# -----------------------------------------------------------------
# 1. Fetch the job-status table + fill missing combos
# -----------------------------------------------------------------
get_existing_concentration_calc <- function(conn,
                                            project_id,
                                            study_accession,
                                            experiment_accession,
                                            plate_nom) {

  query <- glue::glue(
    "SELECT * FROM madi_results.get_job_status2(
        {project_id},
        '{study_accession}',
        '{experiment_accession}',
        '{plate_nom}'
    );"
  )

  df <- dbGetQuery(conn, query)

  # Normalize label for UI
  df$job_status[df$job_status == "partial completion"] <-
    "partially completed"

  return(df)
}



# -----------------------------------------------------------------
# 2. Build a single status badge
# -----------------------------------------------------------------
# -----------------------------------------------------------------
createStatusBadge <- function(method,
                              existing_concentration_calc,
                              scope,
                              progress_msg = NULL,
                              bayes_status = NULL) {

  # ── Bayesian method: use bayes_status list instead of DB calc table ──
  if (method == "bayesian") {
    # bayes_status is a list with $status, $progress, $percentage, $eta_display,
    # $timestamp, $error, $job_id — from get_bayes_calc_status() or NULL
    if (is.null(bayes_status)) {
      type_status <- "not begun"
    } else {
      type_status <- bayes_status$status %||% "not begun"
    }

    bg_color <- switch(type_status,
      "pending"   = "#FFA500",
      "completed" = "#28a745",
      "failed"    = "#dc3545",
      "not begun" = "#dc3545",
      "partially completed" = "#6f42c1",
      "#999999"
    )

    # "via experiment/study job" label shown when status cascaded from a parent scope
    covered_by <- bayes_status$covered_by %||% scope
    via_label  <- if (!identical(covered_by, scope) && type_status == "completed") {
      tags$small(
        style = "font-weight:normal; opacity:0.80; display:block; margin-top:2px;",
        paste0("(covered by ", covered_by, " job)")
      )
    } else NULL

    # Experiment coverage shown on study badge: "3 / 10 experiments"
    coverage <- bayes_status$coverage
    cov_label <- if (!is.null(coverage) && !is.na(coverage$n_total) && coverage$n_total > 0L) {
      pct <- round(100 * coverage$n_done / coverage$n_total)
     col <- if (coverage$n_done == coverage$n_total) "#155724" else "#ffffff"#"#856404"
      tags$small(
        style = paste0("font-weight:normal; display:block; margin-top:2px; color:", col, ";"),
        sprintf("%d / %d experiments", coverage$n_done, coverage$n_total)
      )
    } else NULL

    # Coverage pills for source and wavelength — two independent groups.
    # get_antigen_source_coverage() returns list(sources=df|NULL, wavelengths=df|NULL).
    # Each group is only shown when that dimension has >1 distinct value.
    coverage_groups <- bayes_status$sources  # named list or NULL
    make_pill_group <- function(df, group_label) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)
      pills <- lapply(seq_len(nrow(df)), function(i) {
        ok  <- isTRUE(df$covered[i])
        col <- if (ok) "#28a745" else "#dc3545"
        ico <- if (ok) "fa-check" else "fa-times"
        tags$span(
          style = paste0(
            "display:inline-block; margin:2px 2px 0; padding:1px 5px; ",
            "border-radius:8px; font-size:10px; background:", col, "; color:white;"
          ),
          tags$i(class = paste("fa", ico)), " ", df$label[i]
        )
      })
      tags$div(
        style = "margin-top:3px; line-height:1.6;",
        tags$span(style = "font-size:10px; opacity:0.75; margin-right:3px;",
                  paste0(group_label, ":")),
        pills
      )
    }
    src_pills <- if (is.list(coverage_groups) && !is.data.frame(coverage_groups)) {
      tagList(
        make_pill_group(coverage_groups$sources,     "Source"),
        make_pill_group(coverage_groups$wavelengths, "Wavelength")
      )
    } else NULL

    badge_label <- switch(type_status,
      "pending" = {
        prog <- bayes_status$progress %||% ""
        eta  <- bayes_status$eta_display %||% ""
        lbl  <- if (nzchar(prog)) paste0("Running (", prog, ")") else "Running..."
        if (nzchar(eta)) lbl <- paste0(lbl, " \u2014 ", eta)
        tagList(tags$i(class = "fa fa-spinner fa-spin"), " ", lbl)
      },
      "completed" = {
        ts <- bayes_status$timestamp
        ts_label <- if (!is.null(ts) && !is.na(ts)) {
          format(as.POSIXct(ts, tz = "UTC"), "%b %d at %I:%M %p")
        } else { NULL }
        tagList(
          tags$i(class = "fa fa-check"), " Completed",
          if (!is.null(ts_label)) tags$br(),
          if (!is.null(ts_label)) tags$small(style = "font-weight:normal; opacity:0.85;",
                                              paste0("Last: ", ts_label)),
          via_label,
          cov_label
        )
      },
      # add partial completion status
      "partially completed" = {
        ts <- bayes_status$timestamp
        ts_label <- if (!is.null(ts) && !is.na(ts)) {
          format(as.POSIXct(ts, tz = "UTC"), "%b %d at %I:%M %p")
        } else { NULL }
        tagList(
          tags$i(class = "fa fa-layer-group"), " Partially Completed",
          if (!is.null(ts_label)) tags$br(),
          if (!is.null(ts_label)) tags$small(style = "font-weight:normal; opacity:0.85;",
                                             paste0("Last: ", ts_label)),
          cov_label
        )
      },
      "failed" = {
        ts <- bayes_status$timestamp
        ts_label <- if (!is.null(ts) && !is.na(ts)) {
          format(as.POSIXct(ts, tz = "UTC"), "%b %d at %I:%M %p")
        } else { NULL }
        err_msg <- bayes_status$error %||% ""
        tagList(
          tags$i(class = "fa fa-exclamation-triangle"), " Failed",
          if (!is.null(ts_label)) tags$br(),
          if (!is.null(ts_label)) tags$small(style = "font-weight:normal; opacity:0.85;",
                                              paste0("Last: ", ts_label)),
          if (nzchar(err_msg)) tags$br(),
          if (nzchar(err_msg)) tags$small(style = "font-weight:normal; opacity:0.85;",
                                           substr(err_msg, 1, 80))
        )
      },
      "not begun" = tagList(tags$i(class = "fa fa-times"), " Not Begun", cov_label),
      NULL
    )

    if (is.null(badge_label)) return(NULL)

    return(tagList(
      tags$span(
        class = "badge",
        style = paste0(
          "display:inline-block; position:relative; ",
          "padding:4px 10px; border-radius:10px; font-size:12px; ",
          "background-color:", bg_color, "; color:white; white-space:nowrap;"
        ),
        badge_label
      ),
      src_pills  # rendered outside the badge span so pills sit below it
    ))
  }

  # ── Frequentist method: existing logic from get_job_status2 DB function ──
  row <- existing_concentration_calc[
    existing_concentration_calc$concentration_calc_method == method &
      existing_concentration_calc$scope == scope,
  ]

  if (nrow(row) == 0) return(NULL)

  type_status <- row$job_status[1]
  
  incomplete <- if (
    "incomplete_items" %in% names(row) &&
    type_status == "partially completed"
  ) {
    row$incomplete_items[1]
  } else {
    NA
  }
  
  has_detail <- type_status == "partially completed" &&
    !is.na(incomplete) && nzchar(incomplete)
  
  bg_color <- switch(type_status,
                     "partially completed" = "#6f42c1",
                     "pending"             = "#FFA500",
                     "completed"           = "#28a745",
                     "not begun"           = "#dc3545",
                     NULL
  )
  if (is.null(bg_color)) return(NULL)
  
  # ── Badge label — NO title attr, NO data-toggle, NO cursor:help on badge ──
  badge_label <- switch(type_status,
                        
                        "partially completed" = tagList(
                          tags$i(class = "fa fa-layer-group"), " Partially Completed",
                          if (has_detail) tags$span(
                            # sc-tip-parent wraps both the ? icon and the popup span.
                            # The CSS rule .sc-tip-parent:hover .sc-tip-box makes the popup
                            # appear only while the mouse is over this span.
                            class = "sc-tip-parent",
                            style = "position:relative; display:inline-block; margin-left:5px; cursor:help;",
                            tags$i(class = "fa fa-question-circle", style = "font-size:11px;"),
                            tags$span(
                              class = "sc-tip-box",
                              style = paste0(
                                "display:none; ",
                                "position:absolute; bottom:130%; left:50%; ",
                                "transform:translateX(-50%); ",
                                "background:#333; color:#fff; ",
                                "padding:5px 9px; border-radius:4px; ",
                                "font-size:11px; white-space:pre-line; ",
                                "min-width:210px; text-align:left; ",
                                "pointer-events:none; z-index:9999; line-height:1.4;"
                              ),
                              paste("Incomplete:", incomplete)
                            )
                          )
                        ),
                        
                        "pending" = {
                          has_progress <- !is.null(progress_msg) && nzchar(progress_msg)
                          tagList(
                            if (has_progress) tags$style(HTML(
                              ".sc-tip-parent:hover .sc-tip-box { display:block !important; }"
                            )),
                            tags$span(
                              class = if (has_progress) "sc-tip-parent" else NULL,
                              style = "position:relative; display:inline-block;",
                              tags$i(class = "fa fa-spinner fa-spin"), " Running...",
                              if (has_progress) tags$span(
                                class = "sc-tip-box",
                                style = paste0(
                                  "display:none; ",
                                  "position:absolute; bottom:130%; left:50%; ",
                                  "transform:translateX(-50%); ",
                                  "background:#333; color:#fff; ",
                                  "padding:5px 9px; border-radius:4px; ",
                                  "font-size:11px; white-space:pre-line; ",
                                  "min-width:210px; text-align:left; ",
                                  "pointer-events:none; z-index:9999; line-height:1.4;"
                                ),
                                progress_msg
                              )
                            )
                          )
                        },
                        
                        "completed" = tagList(
                          tags$i(class = "fa fa-check"), " Completed"
                        ),
                        
                        "not begun" = tagList(
                          tags$i(class = "fa fa-times"), " Not Begun"
                        ),
                        
                        NULL
  )
  if (is.null(badge_label)) return(NULL)
  
  style_tag <- if (has_detail) {
    tags$style(HTML(
      ".sc-tip-parent:hover .sc-tip-box { display:block !important; }"
    ))
  } else NULL
  
  tagList(
    style_tag,
    tags$span(
      class = "badge",
      style = paste0(
        "display:inline-block; position:relative; ",
        "padding:4px 10px; border-radius:10px; font-size:12px; ",
        "background-color:", bg_color, "; color:white; white-space:nowrap;"
      ),
      badge_label
    )
  )
}
# createStatusBadge <- function(method, existing_concentration_calc, scope) {
#   
#   row <- existing_concentration_calc[
#     existing_concentration_calc$concentration_calc_method == method &
#       existing_concentration_calc$scope == scope,
#   ]
#   
#   if (nrow(row) == 0) return(NULL)
#   
#   # type_status <- row$job_status[1]
#   # incomplete  <<- row$incomplete_items[1]
#   
#   type_status <- row$job_status[1]
#   
#   incomplete <- if (
#     "incomplete_items" %in% names(row) &&
#     type_status == "partially completed"
#   ) {
#     row$incomplete_items[1]
#   } else {
#     NA
#   }
#   
#   print(incomplete)
#   
#   # Tooltip if incomplete items exist
#   # tooltip <- if (!is.na(incomplete) && incomplete != "") {
#   #   paste("Incomplete:", incomplete)
#   # } else {
#   #   NULL
#   # }
#   
#   tooltip <- if (
#     !is.na(incomplete) &&
#     nzchar(incomplete)
#   ) {
#     paste("Incomplete:", incomplete)
#   } else {
#     NULL
#   }
#   
#   status_style <- switch(type_status,
#                          "partially completed" = "background-color: #6f42c1; color: white;",
#                          "pending"             = "background-color: #FFA500; color: white;",
#                          "completed"           = "background-color: #28a745; color: white;",
#                          "not begun"           = "background-color: #dc3545; color: white;",
#                          NULL
#   )
#   
#   status_text <- switch(type_status,
#                         "partially completed" = tagList(
#                           tags$i(class = "fa fa-layer-group"),
#                           " Partially Completed"
#                         ),
#                         "pending" = tagList(
#                           tags$i(class = "fa fa-spinner fa-spin"),
#                           " Running..."
#                         ),
#                         "completed" = tagList(
#                           tags$i(class = "fa fa-check"),
#                           " Completed"
#                         ),
#                         "not begun" = tagList(
#                           tags$i(class = "fa fa-times"),
#                           " Not Begun"
#                         ),
#                         NULL
#   )
#   
#   if (!is.null(status_style) && !is.null(status_text)) {
#     span(
#       class = "badge",
#       title = tooltip,
#       style = paste0(
#         "padding: 4px 10px; border-radius: 10px; font-size: 12px; ",
#         status_style
#       ),
#       status_text
#     )
#    
#   } else {
#     NULL
#   }
# }

# -----------------------------------------------------------------
# 3. Get status for a specific scope + method
# -----------------------------------------------------------------
get_status <- function(existing_concentration_calc, scope, method) {
  
  sts <- existing_concentration_calc$job_status[
    existing_concentration_calc$concentration_calc_method == method &
      existing_concentration_calc$scope == scope
  ]
  if (length(sts) == 0) return("not begun")
  sts[1]
}

# -----------------------------------------------------------------
# 4. Build the status grid + conditional buttons
# -----------------------------------------------------------------
createStandardCurveConcentrationTypeUI <- function(existing_concentration_calc, progress_msg = NULL,
                                                   interp_progress_msg = NULL,
                                                   bayes_status_list = NULL) {
  concentrationUIRefresher()
  
  ## Method display labels
  method_labels <- c(
    "interpolated" = "Frequentist",
    "bayesian"     = "Bayesian"
  )

  ## Frequentist uses plate/experiment/study; Bayesian uses antigen/experiment/study
  freq_scopes <- c("study", "experiment", "plate")
  bayes_scopes <- c("study", "experiment", "antigen")

  scope_labels <- c(
    "study"      = "Study (All Experiments)",
    "experiment" = "Experiment (Current)",
    "plate"      = "Plate (Current)",
    "antigen"    = "Antigen (Current)"
  )

  scope_icons <- c(
    "study"      = "fa-flask",
    "experiment" = "fa-vial",
    "plate"      = "fa-th",
    "antigen"    = "fa-dna"
  )

  all_methods <- c("interpolated", "bayesian")
  # Display columns: Study, Experiment, Plate/Antigen
  all_scopes  <- c("study", "experiment", "plate")
  
  ## ── Build the status grid as an HTML table ──
  ## Column headers: Study | Experiment | Plate/Antigen
  header_cells <- list(
    tags$th(
      style = "text-align:center; padding:10px 15px; font-size:14px;",
      tags$i(class = "fa fa-flask", style = "margin-right:5px;"),
      "Study (All Experiments)"
    ),
    tags$th(
      style = "text-align:center; padding:10px 15px; font-size:14px;",
      tags$i(class = "fa fa-vial", style = "margin-right:5px;"),
      "Experiment (Current)"
    ),
    tags$th(
      style = "text-align:center; padding:10px 15px; font-size:14px;",
      tags$i(class = "fa fa-th", style = "margin-right:5px;"),
      "Plate / Antigen"
    )
  )

  body_rows <- lapply(all_methods, function(m) {
    if (m == "interpolated") {
      # Frequentist: study, experiment, plate
      scopes_for_method <- c("study", "experiment", "plate")
    } else {
      # Bayesian: study, experiment, antigen
      scopes_for_method <- c("study", "experiment", "antigen")
    }

    cells <- lapply(scopes_for_method, function(s) {
      # For Bayesian, pass the scope-specific status from bayes_status_list
      bayes_st <- if (m == "bayesian" && !is.null(bayes_status_list)) {
        bayes_status_list[[s]]
      } else { NULL }

      tags$td(
        style = "text-align:center; padding:10px 15px; vertical-align:middle;",
        createStatusBadge(
          method                     = m,
          existing_concentration_calc = existing_concentration_calc,
          scope                      = s,
          progress_msg               = switch(m,
                                              "interpolated" = interp_progress_msg,
                                              NULL),
          bayes_status               = bayes_st
        )
      )
    })
    tags$tr(
      tags$td(
        style = "padding:10px 15px; font-weight:bold; vertical-align:middle;",
        method_labels[m]
      ),
      cells
    )
  })
  
  status_grid <- tags$table(
    class = "table table-bordered",
    style = "width:100%; margin-top:10px; border-radius:8px;",
    tags$thead(
      tags$tr(
        tags$th(style = "padding:10px 15px;", "Method"),
        header_cells
      )
    ),
    tags$tbody(body_rows)
  )
  
  ## ── Scope selectors (separate for Frequentist and Bayesian) ──
  scope_selector <- uiOutput("calculation_scope_ui")

  ## ── Buttons section (rendered server-side for conditional logic) ──
  buttons_section <- uiOutput("concentration_buttons_ui")
  
  ## ── Assemble everything ──
  tagList(
    tags$head(tags$style(HTML("
      .conc-btn {
        padding: 12px 30px;
        font-size: 14px;
        line-height: 1.5;
        white-space: normal;
        margin: 5px;
        border-radius: 5px;
        color: white;
        border: none;
        cursor: pointer;
      }
      .conc-btn-green {
        background-color: #7DAF4C;
        border-color: #91CF60;
      }
      .conc-btn-green:hover {
        background-color: #6B9A3F;
      }
      .conc-btn-blue {
        background-color: #4A90D9;
        border-color: #5BA0E9;
      }
      .conc-btn-blue:hover {
        background-color: #3D7DC0;
      }
      .conc-btn-disabled {
        background-color: #cccccc;
        border-color: #bbbbbb;
        color: #666666;
        cursor: not-allowed;
      }
    "))),
    
    wellPanel(
      tags$h4(
        #tags$i(class = "fa fa-table", style = "margin-right:8px;"),
        "Calculation Status of Standard Curves by Concentration Prediction Method"
      ),
      tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
      status_grid,
      tags$hr(),
      scope_selector,
      buttons_section
    )
  )
}


# -----------------------------------------------------------------
# Remove a specific scope+method entry from the pending overlay.
# Called from both onFulfilled and onRejected so the badge always
# clears regardless of outcome.
# -----------------------------------------------------------------
.remove_pending <- function(pending_rv, scope, method) {
  updated <- Filter(
    function(entry) {
      !(entry[["scope"]] == scope && entry[["method"]] == method)
    },
    pending_rv()
  )
  pending_rv(updated)
}

# -----------------------------------------------------------------
# Internal helper — called by both the first-run and rerun observers.
# All arguments are plain R values (NOT reactives).
# -----------------------------------------------------------------
.launch_mcmc <- function(scope, study, experiment, plate, proj,
                         scope_label, session) {
  
  # ── 1. Set processing flag ──────────────────────────────────────
  is_batch_processing(TRUE)
  
  # ── 2. Inject "pending" into the in-memory overlay ─────────────
  current_pending <- mcmc_pending_scopes()
  mcmc_pending_scopes(c(
    current_pending,
    list(c(scope = scope, method = "mcmc_robust"))
  ))
  
  # ── 3. Trigger UI refresh so badge shows "pending" now ──────────
  concentrationUIRefresher(concentrationUIRefresher() + 1)
  
  # ── 4. Progress file for IPC ────────────────────────────────────
  prog_file <- tempfile(pattern = "mcmc_progress_", fileext = ".txt")
  writeLines("Starting MCMC Robust...", prog_file)
  mcmc_progress_file(prog_file)
  mcmc_progress_msg(paste0(
    "Running MCMC Robust\n",
    "Scope: ", scope_label, "\n",
    "Study: ", study, "\n",
    "Experiment: ", experiment
  ))
  
  # ── 5. Persistent running notification ──────────────────────────
  showNotification(
    id  = "mcmc_calc_notify",
    div(
      class = "big-notification",
      paste0("Starting MCMC Robust for ", scope_label, "...")
    ),
    duration = 10
  )
  
  # ── 6. Snapshot reactive data ───────────────────────────────────
  best_glance_snapshot <- tryCatch(
    fetch_best_glance_mcmc(
      study_accession = study,
      project_id      = proj,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  .mcmc_guard <- function(condition, msg, type = "warning") {
    if (condition) {
      showNotification(msg, type = type)
      removeNotification("mcmc_calc_notify")
      .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
      mcmc_progress_msg(NULL)
      mcmc_progress_file(NULL)
      concentrationUIRefresher(concentrationUIRefresher() + 1)
      is_batch_processing(FALSE)
      return(TRUE)
    }
    FALSE
  }
  
  if (.mcmc_guard(
    is.null(best_glance_snapshot) || nrow(best_glance_snapshot) == 0,
    "No fitted curves found."
  )) return()
  
  best_glance_snapshot <- filter_glance_scope(
    best_glance_snapshot, scope, experiment, plate
  )
  
  if (.mcmc_guard(
    nrow(best_glance_snapshot) == 0,
    "No fitted curves found for this scope."
  )) return()
  
  id_set  <- best_glance_snapshot$best_glance_all_id
  n_total <- length(id_set)
  
  combined_df_snapshot <- tryCatch(
    fetch_combined_mcmc(
      study_accession = study,
      project_id      = proj,
      best_glance_ids = id_set,
      conn            = conn
    ),
    error = function(e) NULL
  )
  
  if (.mcmc_guard(
    is.null(combined_df_snapshot) || nrow(combined_df_snapshot) == 0,
    "No prediction data found for MCMC.",
    type = "error"
  )) return()
  
  # ── Fetch tidy params snapshot for all curves in scope ───────────
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
  
  db_conn_args <- get_db_connection_args()
  
  # ── 7. Launch future ────────────────────────────────────────────
  future_promise <- future::future({
    
    bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
    on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
    
    results     <- vector("list", length(id_set))
    best_glance <- best_glance_snapshot
    
    for (i in seq_along(id_set)) {
      
      id  <- id_set[i]
      row <- best_glance[best_glance$best_glance_all_id == id, ]
      
      progress_text <- paste0(
        "MCMC Robust: ", i, " / ", n_total, "\n",
        "Study:      ", row$study_accession,     "\n",
        "Experiment: ", row$experiment_accession, "\n",
        "Plate:      ", row$plate_nom,            "\n",
        "Antigen:    ", row$antigen,              "\n",
        "Model:      ", row$model_name
      )
      tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
      
      curve_df <- combined_df_snapshot[
        combined_df_snapshot$best_glance_all_id == id, ]
      pred_df  <- curve_df[curve_df$mcmc_set == "pred_se", ]
      if (nrow(pred_df) == 0) next
      if ("wavelength" %in% names(pred_df) && !is.na(row$wavelength)) {
        pred_df <- pred_df[pred_df$wavelength == row$wavelength, ]
      }
      
      # ── Extract tidy params for this curve ────────────────────────
      tidy_row <- if (!is.null(tidy_params_snapshot) && nrow(tidy_params_snapshot) > 0) {
        tidy_params_snapshot[
          tidy_params_snapshot$experiment_accession    == row$experiment_accession    &
            tidy_params_snapshot$plate                   == row$plate                   &
            tidy_params_snapshot$source                  == row$source                  &
            tidy_params_snapshot$antigen                 == row$antigen                 &
            tidy_params_snapshot$nominal_sample_dilution == row$nominal_sample_dilution &
            tidy_params_snapshot$feature                 == row$feature                 &
            tidy_params_snapshot$wavelength              == row$wavelength,
        ]
      } else {
        NULL
      }
      # tidy_row <- if (!is.null(tidy_params_snapshot)) {
      #   tidy_params_snapshot[
      #     tidy_params_snapshot$best_glance_all_id == id, ]
      # } else {
      #   NULL
      # }
      
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
      res <- tryCatch(
        run_jags_predicted_concentration(
          glance_row   = row,
          best_pred_df = pred_df,
          sample_df    = curve_df,
          response_col = "assay_response",
          tidy_df      = tidy_row,
          verbose      = TRUE
        ),
        error = function(e) { message("JAGS error: ", e$message); NULL }
      )
      
      if (!is.null(res)) {
        if (!"mcmc_set" %in% names(res))
          res$mcmc_set <- curve_df$mcmc_set[match(res$row_id, curve_df$row_id)]
        results[[i]] <- res
      }
    }
    
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
  
  # ── 8. Poll progress file ────────────────────────────────────────
  progress_poller <- reactivePoll(
    intervalMillis = 2000,
    session        = session,
    checkFunc      = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(0)
      file.info(pf)$mtime
    },
    valueFunc      = function() {
      pf <- mcmc_progress_file()
      if (is.null(pf) || !file.exists(pf)) return(NULL)
      tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
    }
  )
  
  progress_observer <- observe({
    msg <- progress_poller()
    if (!is.null(msg) && nzchar(msg)) mcmc_progress_msg(msg)
  })
  
  # ── 9. Handle promise resolution ────────────────────────────────
  .cleanup_mcmc <- function(label, type = "message", duration = 10) {
    progress_observer$destroy()
    pf <- mcmc_progress_file()
    if (!is.null(pf) && file.exists(pf)) file.remove(pf)
    mcmc_progress_file(NULL)
    mcmc_progress_msg(NULL)
    .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
    showNotification(label, type = type, duration = duration)
    concentrationUIRefresher(concentrationUIRefresher() + 1)
    is_batch_processing(FALSE)
  }
  
  promises::then(
    future_promise,
    onFulfilled = function(result) {
      .cleanup_mcmc(
        paste0("MCMC Robust completed for ", result$scope_label, ".")
      )
    },
    onRejected = function(err) {
      .cleanup_mcmc(
        paste0("MCMC Robust error: ", conditionMessage(err)),
        type     = "error",
        duration = 15
      )
      message("MCMC future rejected: ", conditionMessage(err))
    }
  )
  
  NULL
}
# .launch_mcmc <- function(scope, study, experiment, plate, proj,
#                          scope_label, session) {
#   
#   # ── 1. Set processing flag ──────────────────────────────────────
#   is_batch_processing(TRUE)
#   
#   # ── 2. Inject "pending" into the in-memory overlay ─────────────
#   current_pending <- mcmc_pending_scopes()
#   mcmc_pending_scopes(c(
#     current_pending,
#     list(c(scope = scope, method = "mcmc_robust"))
#   ))
#   
#   # ── 3. Trigger UI refresh so badge shows "pending" now ──────────
#   concentrationUIRefresher(concentrationUIRefresher() + 1)
#   
#   # ── 4. Progress file for IPC ────────────────────────────────────
#   prog_file <- tempfile(pattern = "mcmc_progress_", fileext = ".txt")
#   writeLines("Starting MCMC Robust...", prog_file)
#   mcmc_progress_file(prog_file)
#   mcmc_progress_msg(paste0(
#     "Running MCMC Robust\n",
#     "Scope: ", scope_label, "\n",
#     "Study: ", study, "\n",
#     "Experiment: ", experiment
#   ))
#   
#   # ── 5. Persistent running notification ──────────────────────────
#   showNotification(
#     id  = "mcmc_calc_notify",
#     div(
#       class = "big-notification",
#       paste0("Starting MCMC Robust for ", scope_label, "...")
#     ),
#     duration = 10
#   )
#   
#   # ── 6. Snapshot reactive data ───────────────────────────────────
#   best_glance_snapshot <- tryCatch(
#     fetch_best_glance_mcmc(
#       study_accession = study,
#       project_id      = proj,
#       conn            = conn
#     ),
#     error = function(e) NULL
#   )
#   
#   .mcmc_guard <- function(condition, msg, type = "warning") {
#     if (condition) {
#       showNotification(msg, type = type)
#       removeNotification("mcmc_calc_notify")
#       .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
#       mcmc_progress_msg(NULL)
#       mcmc_progress_file(NULL)
#       concentrationUIRefresher(concentrationUIRefresher() + 1)
#       is_batch_processing(FALSE)
#       return(TRUE)
#     }
#     FALSE
#   }
#   
#   if (.mcmc_guard(
#     is.null(best_glance_snapshot) || nrow(best_glance_snapshot) == 0,
#     "No fitted curves found."
#   )) return()
#   
#   best_glance_snapshot <- filter_glance_scope(
#     best_glance_snapshot, scope, experiment, plate
#   )
#   
#   if (.mcmc_guard(
#     nrow(best_glance_snapshot) == 0,
#     "No fitted curves found for this scope."
#   )) return()
#   
#   id_set  <- best_glance_snapshot$best_glance_all_id
#   n_total <- length(id_set)
#   
#   combined_df_snapshot <- tryCatch(
#     fetch_combined_mcmc(
#       study_accession = study,
#       project_id      = proj,
#       best_glance_ids = id_set,
#       conn            = conn
#     ),
#     error = function(e) NULL
#   )
#   
#   if (.mcmc_guard(
#     is.null(combined_df_snapshot) || nrow(combined_df_snapshot) == 0,
#     "No prediction data found for MCMC.",
#     type = "error"
#   )) return()
#   
#   db_conn_args <- get_db_connection_args()
#   
#   # ── 7. Launch future ────────────────────────────────────────────
#   future_promise <- future::future({
#     
#     bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
#     on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
#     
#     results     <- vector("list", length(id_set))
#     best_glance <- best_glance_snapshot
#     
#     for (i in seq_along(id_set)) {
#       
#       id  <- id_set[i]
#       row <- best_glance[best_glance$best_glance_all_id == id, ]
#       
#       progress_text <- paste0(
#         "MCMC Robust: ", i, " / ", n_total, "\n",
#         "Study:      ", row$study_accession,     "\n",
#         "Experiment: ", row$experiment_accession, "\n",
#         "Plate:      ", row$plate_nom,            "\n",
#         "Antigen:    ", row$antigen,              "\n",
#         "Model:      ", row$model_name
#       )
#       tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
# 
#       curve_df <- combined_df_snapshot[
#         combined_df_snapshot$best_glance_all_id == id, ]
#       pred_df  <- curve_df[curve_df$mcmc_set == "pred_se", ]
#       if (nrow(pred_df) == 0) next
#       
#       res <- tryCatch(
#         run_jags_predicted_concentration(
#           glance_row   = row,
#           best_pred_df = pred_df,
#           sample_df    = curve_df,
#           response_col = "assay_response",
#           verbose      = TRUE
#         ),
#         error = function(e) { message("JAGS error: ", e$message); NULL }
#       )
#       
#       if (!is.null(res)) {
#         if (!"mcmc_set" %in% names(res))
#           res$mcmc_set <- curve_df$mcmc_set[match(res$row_id, curve_df$row_id)]
#         results[[i]] <- res
#       }
#     }
#     
#     results_df <- do.call(rbind, Filter(Negate(is.null), results))
#     if (is.null(results_df) || nrow(results_df) == 0) stop("MCMC produced no results.")
#     
#     result_pred_all   <- results_df[results_df$mcmc_set == "pred_se",   ]
#     result_sample_all <- results_df[results_df$mcmc_set == "sample_se", ]
#     result_sample_all$final_robust_concentration <-
#       result_sample_all$dilution * result_sample_all$raw_robust_concentration
#     
#     best_glance$last_concentration_calc_method[
#       best_glance$best_glance_all_id %in% id_set
#     ] <- "mcmc_robust"
#     
#     result_pred_all2   <- process_jag_result(result_pred_all,   df_name = "pred_se")
#     result_sample_all2 <- process_jag_result(result_sample_all, df_name = "sample_se")
#     
#     update_combined_mcmc_bulk(
#       pred_all_mcmc        = result_pred_all2,
#       sample_all_mcmc      = result_sample_all2,
#       best_glance_complete = best_glance,
#       conn                 = bg_conn
#     )
#     
#     list(ok = TRUE, n_curves = nrow(best_glance), scope_label = scope_label)
#     
#   }, seed = TRUE)
#   
#   # ── 8. Poll progress file ────────────────────────────────────────
#   progress_poller <- reactivePoll(
#     intervalMillis = 2000,
#     session        = session,
#     checkFunc      = function() {
#       pf <- mcmc_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(0)
#       file.info(pf)$mtime
#     },
#     valueFunc      = function() {
#       pf <- mcmc_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(NULL)
#       tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
#     }
#   )
#   
#   progress_observer <- observe({
#     msg <- progress_poller()
#     if (!is.null(msg) && nzchar(msg)) mcmc_progress_msg(msg)
#   })
#   
#   # ── 9. Handle promise resolution ────────────────────────────────
#   .cleanup_mcmc <- function(label, type = "message", duration = 10) {
#     progress_observer$destroy()
#     pf <- mcmc_progress_file()
#     if (!is.null(pf) && file.exists(pf)) file.remove(pf)
#     mcmc_progress_file(NULL)
#     mcmc_progress_msg(NULL)
#     .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
#     showNotification(label, type = type, duration = duration)
#     concentrationUIRefresher(concentrationUIRefresher() + 1)
#     is_batch_processing(FALSE)
#   }
#   
#   promises::then(
#     future_promise,
#     onFulfilled = function(result) {
#       .cleanup_mcmc(
#         paste0("MCMC Robust completed for ", result$scope_label, ".")
#       )
#     },
#     onRejected = function(err) {
#       .cleanup_mcmc(
#         paste0("MCMC Robust error: ", conditionMessage(err)),
#         type     = "error",
#         duration = 15
#       )
#       message("MCMC future rejected: ", conditionMessage(err))
#     }
#   )
#   
#   NULL
# }


## ── Helpers (defined once, outside the renderUI) ─────────────────────────────

make_spinner_btn <- function(label_text, scope_label) {
  tags$button(
    class    = "conc-btn conc-btn-disabled",
    disabled = "disabled",
    tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
    HTML(paste0(label_text, "<br>", scope_label))
  )
}

make_run_btn <- function(input_id, method_label, scope_label) {
  actionButton(
    inputId = input_id,
    label   = HTML(paste0(
      "Calculate <strong>", method_label, "</strong> concentrations<br>",
      scope_label
    ))
  )
}
# make_spinner_btn <- function(label_text, scope_label) {
#   tags$button(
#     class    = "conc-btn conc-btn-disabled",
#     disabled = "disabled",
#     tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
#     HTML(paste0(label_text, "<br>", scope_label))
#   )
# }
# 
# make_run_btn <- function(input_id, method_label, scope_label) {
#   actionButton(
#     inputId = input_id,
#     label   = HTML(paste0(
#       "Calculate <strong>", method_label, "</strong> concentrations<br>",
#       scope_label
#     ))
#   )
# }

make_rerun_btn <- function(input_id, method_label, scope_label) {
  actionButton(
    inputId = input_id,
    label   = HTML(paste0(
      tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
      " Rerun <strong>", method_label, "</strong> concentrations<br>",
      "<small style='color:#ffc107;'>",
      tags$i(class = "fa fa-exclamation-triangle"),
      " Will overwrite existing results</small><br>",
      scope_label
    )),
    style = paste0(
      "border: 2px solid #ffc107; ",
      "background-color: #fff8e1; ",
      "color: #333;"
    )
  )
}
# make_rerun_btn <- function(input_id, method_label, scope_label) {
#   actionButton(
#     inputId = input_id,
#     label   = HTML(paste0(
#       tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
#       " Rerun <strong>", method_label, "</strong> concentrations<br>",
#       "<small style='color:#ffc107;'>",
#       tags$i(class = "fa fa-exclamation-triangle"),
#       " Will overwrite existing results</small><br>",
#       scope_label
#     )),
#     style = paste0(
#       "border: 2px solid #ffc107; ",
#       "background-color: #fff8e1; ",
#       "color: #333;"
#     )
#   )
# }


make_method_btn <- function(status, input_id, method_label, scope_label) {
  if (is.null(status) || is.na(status) || status == "not begun") {
    make_run_btn(input_id, method_label, scope_label)
  } else if (status == "pending") {
    make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
  } else if (status == "completed" || status == "partially completed") {
    make_rerun_btn(input_id, method_label, scope_label)
  } else {
    # Fallback — treat unknown status as "not begun"
    make_run_btn(input_id, method_label, scope_label)
  }
}
# make_method_btn <- function(status, input_id, method_label, scope_label) {
#   if (status == "pending") {
#     make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
#   } else if (status == "completed") {
#     make_rerun_btn(input_id, method_label, scope_label)
#   } else {
#     make_run_btn(input_id, method_label, scope_label)
#   }
# }
# make_method_btn <- function(status, input_id, method_label, scope_label) {
#   if (is.null(status) || is.na(status)) {
#     ## Safety net — treat missing status as "not begun"
#     make_run_btn(input_id, method_label, scope_label)
#   } else if (status == "pending") {
#     make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
#   } else if (status == "completed") {
#     make_rerun_btn(input_id, method_label, scope_label)
#   } else {
#     ## "not begun" or anything else
#     make_run_btn(input_id, method_label, scope_label)
#   }
# }



# # ============================================================
# # Model string factory — curve params are DATA, only x is sampled
# # Uses dunif prior on x instead of hierarchical dnorm
# # ============================================================
# # get_jags_calibration_model <- function(model_name) {
# #   model_name <- trimws(as.character(model_name))
# #   model_string <- switch(
# #     model_name,
# #     
# #     # 5PL symmetric: d + (a - d) / (1 + exp((x - c)/b))^g
# #     "Y5" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- d + (a - d) * pow(1 + exp((x[i] - c) / b), -g)
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }
# #     ",
# #     
# #     # 4PL symmetric: d + (a - d) / (1 + exp((x - c)/b))
# #     "Y4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- d + (a - d) / (1 + exp((x[i] - c) / b))
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }
# #     ",
# #     
# #     # 5PL log-logistic: a + (d - a) * (1 + g * exp(-b*(x - c)))^(-1/g)
# #     "Yd5" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) * pow(1 + g * exp(-b * (x[i] - c)), -1/g)
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }
# #     ",
# #     
# #     # 4PL log-logistic: a + (d - a) / (1 + exp(-b*(x - c)))
# #     "Yd4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) / (1 + exp(-b * (x[i] - c)))
# #        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }
# #     ",
# #     
# #     # Gompertz: a + (d - a) * exp(-exp(-b*(x - c)))
# #     "Ygomp4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) * exp(-exp(-b * (x[i] - c)))
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }
# #     ",
# #     
# #     stop("Unsupported model_name: ", model_name)
# #   )
# #   return(model_string)
# # }
# # 
# # # ============================================================
# # # Build the forward function matching each model name
# # # ============================================================
# # get_forward_function <- function(model_name) {
# #   model_name <- trimws(as.character(model_name))
# #   switch(
# #     model_name,
# #     "Y5"    = function(x, p) p["d"] + (p["a"] - p["d"]) * (1 + exp((x - p["c"]) / p["b"]))^(-p["g"]),
# #     "Y4"    = function(x, p) p["d"] + (p["a"] - p["d"]) / (1 + exp((x - p["c"]) / p["b"])),
# #     "Yd5"   = function(x, p) p["a"] + (p["d"] - p["a"]) * (1 + p["g"] * exp(-p["b"] * (x - p["c"])))^(-1 / p["g"]),
# #     "Yd4"   = function(x, p) p["a"] + (p["d"] - p["a"]) / (1 + exp(-p["b"] * (x - p["c"]))),
# #     "Ygomp4"= function(x, p) p["a"] + (p["d"] - p["a"]) * exp(-exp(-p["b"] * (x - p["c"]))),
# #     stop("Unsupported model_name: ", model_name)
# #   )
# # }
# # 
# # # ============================================================
# # # Numerically invert the forward curve at observed y values
# # # to get good starting x values
# # # ============================================================
# # get_x_init <- function(model_name, fit, params, y_obs, x_min, x_max, conc_range) {
# #   fwd <- get_forward_function(model_name)
# #   
# #   # --- Numerical inversion (unchanged) ---
# #   x_init <- vapply(y_obs, function(yi) {
# #     result <- tryCatch(
# #       uniroot(
# #         function(x) fwd(x, params) - yi,
# #         interval = c(x_min, x_max),
# #         tol = 1e-8
# #       )$root,
# #       error = function(e) NA_real_
# #     )
# #     return(result)
# #   }, numeric(1))
# #   
# #   # Fall back to midpoint for any failures
# #   x_init[is.na(x_init)] <- (x_min + x_max) / 2
# #   
# #   # Clamp inside bounds with small margin
# #   margin <- (x_max - x_min) * 0.001
# #   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
# #   
# #   # --- Compute slope at each x_init ---
# #   slope <- vapply(x_init, function(xi) {
# #     h <- 1e-5
# #     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
# #   }, numeric(1))
# #   
# #   # --- Compute maximum slope across the curve (reference) ---
# #   x_grid <- seq(x_min, x_max, length.out = 500)
# #   slope_grid <- vapply(x_grid, function(xi) {
# #     h <- 1e-5
# #     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
# #   }, numeric(1))
# #   max_slope <- max(slope_grid, na.rm = TRUE)
# #   
# #   # --- Slope ratio: 0 = flat asymptote, 1 = steepest part ---
# #   slope_ratio <- abs(slope) / max_slope
# #   slope_ratio <- pmin(slope_ratio, 1.0)
# #   
# #   # --- Tiered variance based on slope ratio ---
# #   # Informative (mid-curve): delta-method variance
# #   resid_var <- var(resid(fit))
# #   delta_var <- (resid_var / slope)^2
# #   
# #   # Defensive bounds
# #   min_var <- (conc_range * 0.001)^2
# #   max_var <- (conc_range * 2)^2
# #   delta_var <- pmin(pmax(delta_var, min_var), max_var)
# #   delta_var[is.na(delta_var)] <- max_var
# #   
# #   # Weakly informative (asymptote): wide variance spanning the full range
# #   wide_var <- (conc_range * 1.0)^2
# #   
# #   # --- Smooth blending via slope_ratio ---
# #   # slope_ratio near 1 -> trust delta method (informative)
# #   # slope_ratio near 0 -> use wide prior (weakly informative)
# #   # Use a sigmoidal weight so transition is smooth
# #   slope_threshold <- 0.15   # below this, prior becomes wide
# #   slope_steepness <- 20     # controls transition sharpness
# #   weight_informative <- 1 / (1 + exp(-slope_steepness * (slope_ratio - slope_threshold)))
# #   
# #   per_sample_variance <- weight_informative * delta_var +
# #     (1 - weight_informative) * wide_var
# #   
# #   # --- Flag samples for which the prior is essentially uninformative ---
# #   prior_type <- ifelse(weight_informative > 0.5, "delta_method", "wide_prior")
# #   
# #   return(list(
# #     x_init = x_init,
# #     per_sample_variance = per_sample_variance,
# #     slope_ratio = slope_ratio,
# #     weight_informative = weight_informative,
# #     prior_type = prior_type
# #   ))
# # }
# # # ============================================================
# # # Main runner
# # # ============================================================
# # run_jags_predicted_concentration <- function(
#     #     model_name,
# #     fit,
# #     plate_standards,
# #     plate_samples,
# #     fixed_constraint = NULL,
# #     response_variable,
# #     adapt_steps    = 500,
# #     burn_in_steps  = 5000,
# #     num_saved_steps = 20000,
# #     thin_steps     = 2,
# #     n_chains       = 3,
# #     verbose        = TRUE
# # ) {
# #   model_name <- trimws(as.character(model_name))
# #   
# #   # ----------------------------------------------------------
# #   # 1. Extract NLS parameters
# #   # ----------------------------------------------------------
# #   params <- coef(fit)
# #   resid_sigma <- var(resid(fit)) #summary(fit)$sigma
# #   
# #   # Override 'a' if a fixed constraint is supplied
# #   if (!is.null(fixed_constraint)) {
# #     params["a"] <- fixed_constraint
# #   }
# #   
# #   if (verbose) {
# #     cat("Curve parameters (fixed as data):\n")
# #     print(params)
# #   }
# #   
# #   # ----------------------------------------------------------
# #   # 2. Concentration bounds — wide enough for both asymptotes
# #   # ----------------------------------------------------------
# #   conc_range <- max(plate_standards$concentration) - min(plate_standards$concentration)
# #   # x_min <- min(plate_standards$concentration) - conc_range * 1.0
# #   # x_max <- max(plate_standards$concentration) + conc_range * 1.0
# #   x_min <- min(plate_standards$concentration) - conc_range * 10.0
# #   x_max <- max(plate_standards$concentration) + conc_range * 10.0
# #   
# #   # ----------------------------------------------------------
# #   # 3. Compute good initial x values via numerical inversion
# #   # ----------------------------------------------------------
# #   y_obs <- plate_samples[[response_variable]]
# #   # x_init_list <- get_x_init(model_name, fit, params, y_obs, x_min, x_max, conc_range)
# #   # x_init <- x_init_list$x_init
# #   # per_sample_variance <- x_init_list$per_sample_variance
# #   # 
# #   x_init_list <- get_x_init(model_name, fit, params, y_obs, x_min, x_max, conc_range)
# #   x_init              <- x_init_list$x_init
# #   per_sample_variance <- x_init_list$per_sample_variance
# #   
# #   # For diagnostics
# #   if (verbose) {
# #     cat("Slope ratio range:", round(range(x_init_list$slope_ratio), 4), "\n")
# #     cat("Prior type counts:\n")
# #     print(table(x_init_list$prior_type))
# #     cat("Weight range:", round(range(x_init_list$weight_informative), 4), "\n")
# #     cat("Initial x range:", round(range(x_init), 4), "\n")
# #     cat("Bounds: [", round(x_min, 4), ",", round(x_max, 4), "]\n")
# #   }
# #   
# #   
# #   
# #   # ----------------------------------------------------------
# #   # 4. Build JAGS data list — curve params are DATA
# #   # ----------------------------------------------------------
# #   # Cap variance: floor to prevent Inf tau, ceiling to prevent zero tau
# #   max_var <- (conc_range * 10)^2
# #   min_var <- (conc_range * 0.001)^2
# #   
# #   per_sample_variance <- pmax(per_sample_variance, min_var)  # avoid division by zero
# #   per_sample_variance <- pmin(per_sample_variance, max_var)   # avoid near-zero tau
# #   per_sample_variance[is.na(per_sample_variance)] <- max_var  # NA -> wide prior
# #   tau_x <- 1 / per_sample_variance
# #   
# #   data_list <- list(
# #     y           = y_obs,
# #     N           = length(y_obs),
# #     a           = unname(params["a"]),
# #     b           = unname(params["b"]),
# #     c           = unname(params["c"]),
# #     d           = unname(params["d"]),
# #     x_min       = x_min,
# #     x_max       = x_max,
# #     sigma_upper = resid_sigma * 5,
# #     x_prior =  x_init,
# #     tau_x   = tau_x
# #   )
# #   
# #   
# #   # Add asymmetry parameter for 5PL models
# #   if (model_name %in% c("Y5", "Yd5")) {
# #     data_list$g <- unname(params["g"])
# #   }
# #   
# #   # ----------------------------------------------------------
# #   # 5. Get model string (no string manipulation needed)
# #   # ----------------------------------------------------------
# #   model_string <- get_jags_calibration_model(model_name)
# #   
# #   # ----------------------------------------------------------
# #   # 6. Generate initial values per chain
# #   # ----------------------------------------------------------
# #   init_list <- lapply(seq_len(n_chains), function(chain_id) {
# #     list(
# #       x     = x_init + rnorm(length(x_init), 0, 0.05),
# #       sigma = resid_sigma * runif(1, 0.8, 1.2),
# #       nu    = runif(1, 3, 15)
# #     )
# #   })
# #   
# #   # ----------------------------------------------------------
# #   # 7. MCMC
# #   # ----------------------------------------------------------
# #   n_iter <- ceiling((num_saved_steps * thin_steps) / n_chains)
# #   
# #   if (verbose) {
# #     cat("\nMCMC configuration:\n")
# #     cat("  Chains          :", n_chains, "\n")
# #     cat("  Adapt steps     :", adapt_steps, "\n")
# #     cat("  Burn-in steps   :", burn_in_steps, "\n")
# #     cat("  Iterations/chain:", n_iter, "\n")
# #     cat("  Thinning        :", thin_steps, "\n\n")
# #     cat("tau_x range:", range(tau_x), "\n")
# #     cat("Any NA in tau_x:", any(is.na(tau_x)), "\n")
# #     cat("Any Inf in tau_x:", any(is.infinite(tau_x)), "\n")
# #     cat("x_prior range:", range(x_init), "\n")
# #   }
# #   
# #   cat("Model String\n")
# #   print(textConnection(model_string))
# #   
# #   jm <- jags.model(
# #     textConnection(model_string),
# #     data     = data_list,
# #     inits    = init_list,
# #     n.chains = n_chains,
# #     n.adapt  = adapt_steps,
# #     quiet    = !verbose
# #   )
# #   
# #   if (verbose) cat("Burning in...\n")
# #   update(jm, burn_in_steps)
# #   
# #   if (verbose) cat("Sampling posterior concentrations...\n")
# #   samps <- coda.samples(
# #     jm,
# #     variable.names = "x",
# #     n.iter = n_iter,
# #     thin   = thin_steps
# #   )
# #   
# #   # ----------------------------------------------------------
# #   # 8. Summarise posteriors
# #   # ----------------------------------------------------------
# #   chain <- as.matrix(samps)
# #   
# #   plate_samples$se_robust_concentration     <- apply(chain, 2, sd)
# #   #plate_samples$predicted_concentration_lower  <- apply(chain, 2, quantile, 0.025)
# #   plate_samples$raw_robust_concentration <- apply(chain, 2, quantile, 0.50)
# #   #plate_samples$predicted_concentration_upper  <- apply(chain, 2, quantile, 0.975)
# #   
# #   # CV on log-scale: sd / |mean|
# #   chain_original <- 10^chain
# #   plate_samples$pcov_robust_concentration <- apply(chain_original, 2, function(x) {
# #     sd(x) / abs(mean(x)) * 100
# #   })
# #   plate_samples$pcov_robust_concentration <- ifelse(plate_samples$pcov_robust_concentration > 125,
# #                                            125,
# #                                            plate_samples$pcov_robust_concentration)
# #   
# #   if (verbose) {
# #     cat("\n--- Concentration range check ---\n")
# #     cat("Standard curve x range  :",
# #         round(range(plate_standards$concentration), 4), "\n")
# #     cat("Predicted Mean range  :",
# #         round(range(plate_samples$raw_robust_concentration), 4), "\n")
# #     cat("CV range                :",
# #         round(range(plate_samples$pcov_robust_concentration), 4), "\n")
# #   }
# #   
# #   return(plate_samples)
# # }
# # 
# # # ============================================================
# # # Wrapper for both pred_se and sample_se dataframes
# # # ============================================================
# # 
# # run_jags_predicted_concentration_wrapper <- function(best_fit_out, input_df = c("pred_se", "sample_se")) {
# #   
# #   input_df <- match.arg(input_df)
# #   
# #   if (input_df == "pred_se") {
# #     
# #     pred_df <- best_fit_out$pred_se
# #     
# #     names(pred_df)[names(pred_df) == "x"]    <- "concentration"
# #     names(pred_df)[names(pred_df) == "yhat"] <- "mfi"
# #     
# #     names(best_fit_out)[names(best_fit_out) == "model_name"] <- "best_model_name"
# #     
# #     plate_samples_bayes <- run_jags_predicted_concentration(
# #       model_name        = best_fit_out$best_model_name,
# #       fit               = best_fit_out$fit,
# #       plate_standards   = pred_df,
# #       plate_samples     = pred_df,
# #       fixed_constraint  = best_fit_out$fixed_a_result,
# #       response_variable = "mfi"
# #     )
# #     
# #   } else {  # sample_se
# #     
# #     plate_samples_bayes <- run_jags_predicted_concentration(
# #       model_name        = best_fit_out$best_fit$best_model_name,
# #       fit               = best_fit_out$best_fit$best_fit,
# #       plate_standards   = best_fit_out$best_fit$best_data,
# #       plate_samples     = best_fit_out$sample_se,
# #       fixed_constraint  = best_fit_out$fixed_a_result,
# #       response_variable = best_fit_out$response_var
# #     )
# #   }
# #   
# #   return(plate_samples_bayes)
# # }
# 
# 
# ### Updated 
# # ============================================================
# # Model string factory
# # ============================================================
# # get_jags_calibration_model <- function(model_name) {
# #   switch(
# #     trimws(model_name),
# #     "Y5" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- d + (a - d) * pow(1 + exp((x[i] - c) / b), -g)
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }",
# #     "Y4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- d + (a - d) / (1 + exp((x[i] - c) / b))
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }",
# #     "Yd5" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) * pow(1 + g * exp(-b * (x[i] - c)), -1/g)
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }",
# #     "Yd4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) / (1 + exp(-b * (x[i] - c)))
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }",
# #     "Ygomp4" = "
# #     model {
# #       for (i in 1:N) {
# #         y[i] ~ dt(mu[i], tau, nu)
# #         mu[i] <- a + (d - a) * exp(-exp(-b * (x[i] - c)))
# #         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
# #       }
# #       sigma ~ dunif(0, sigma_upper)
# #       tau   <- pow(sigma, -2)
# #       nu    ~ dunif(2, 30)
# #     }",
# #     stop("Unsupported model_name: ", model_name)
# #   )
# # }
# get_jags_calibration_model <- function(){
#   
#   "
# model{
# 
#   for(i in 1:N){
# 
#     y[i] ~ dt(mu[i], tau, nu)
# 
#     mu[i] <- y_hat[i]
# 
#   }
# 
#   sigma ~ dunif(0, sigma_upper)
# 
#   tau <- pow(sigma,-2)
# 
#   nu ~ dunif(2,30)
# 
# }
# "
# 
# }
# # ============================================================
# # Forward function factory
# # ============================================================
# get_forward_function <- function(model_name) {
#   switch(
#     trimws(model_name),
#     "Y5"     = function(x, p) p["d"] + (p["a"] - p["d"]) * (1 + exp((x - p["c"]) / p["b"]))^(-p["g"]),
#     "Y4"     = function(x, p) p["d"] + (p["a"] - p["d"]) / (1 + exp((x - p["c"]) / p["b"])),
#     "Yd5"    = function(x, p) p["a"] + (p["d"] - p["a"]) * (1 + p["g"] * exp(-p["b"] * (x - p["c"])))^(-1 / p["g"]),
#     "Yd4"    = function(x, p) p["a"] + (p["d"] - p["a"]) / (1 + exp(-p["b"] * (x - p["c"]))),
#     "Ygomp4" = function(x, p) p["a"] + (p["d"] - p["a"]) * exp(-exp(-p["b"] * (x - p["c"]))),
#     stop("Unsupported model_name: ", model_name)
#   )
# }
# 
# # ============================================================
# # Extract named param vector from a best_glance row
# # Only includes g for 5PL models
# # ============================================================
# extract_params <- function(glance_row) {
#   p <- c(a = glance_row$a,
#          b = glance_row$b,
#          c = glance_row$c,
#          d = glance_row$d)
#   
#   if (trimws(glance_row$model_name) %in% c("Y5", "Yd5")) {
#     p["g"] <- glance_row$g
#   }
#   
#   return(p)
# }
# 
# 
# 
# invert_response_fast <- function(y_obs, params, model_name, x_min, x_max){
#   
#   fwd <- get_forward_function(model_name)
#   
#   x_grid <- seq(x_min, x_max, length.out = 3000)
#   
#   y_grid <- fwd(x_grid, params)
#   
#   keep <- is.finite(y_grid)
#   
#   x_grid <- x_grid[keep]
#   y_grid <- y_grid[keep]
#   
#   # ensure monotonic order
#   if(y_grid[1] > y_grid[length(y_grid)]){
#     y_grid <- rev(y_grid)
#     x_grid <- rev(x_grid)
#   }
#   
#   # remove duplicate y values (prevents approx warning)
#   dup <- !duplicated(y_grid)
#   y_grid <- y_grid[dup]
#   x_grid <- x_grid[dup]
#   
#   approx(
#     x = y_grid,
#     y = x_grid,
#     xout = y_obs,
#     rule = 2
#   )$y
# }
# # ============================================================
# # Numerical inversion + adaptive prior variance
# # ============================================================
# 
# # compute_x_init <- function(model_name, params, resid_var, y_obs,
# #                            x_min, x_max, conc_range) {
# #   fwd <- get_forward_function(model_name)
# # 
# #   
# #   # Invert curve at each y
# #   x_init <- vapply(y_obs, function(yi) {
# #     tryCatch(
# #       uniroot(function(x) fwd(x, params) - yi,
# #               interval = c(x_min, x_max), tol = 1e-8)$root,
# #       error = function(e) NA_real_
# #     )
# #   }, numeric(1))
# # 
# #   x_init[is.na(x_init)] <- (x_min + x_max) / 2
# #   margin <- (x_max - x_min) * 0.001
# #   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
# # 
# #   # Slope at each x_init
# #   slope <- vapply(x_init, function(xi) {
# #     h <- 1e-5
# #     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
# #   }, numeric(1))
# # 
# #   # Max slope across curve
# #   x_grid <- seq(x_min, x_max, length.out = 500)
# #   max_slope <- max(vapply(x_grid, function(xi) {
# #     h <- 1e-5
# #     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
# #   }, numeric(1)), na.rm = TRUE)
# # 
# #   slope_ratio <- pmin(abs(slope) / max_slope, 1.0)
# # 
# #   # Delta-method variance
# #   delta_var <- (resid_var / slope)^2
# #   min_var <- (conc_range * 0.001)^2
# #   max_var <- (conc_range * 2)^2
# #   delta_var <- pmin(pmax(delta_var, min_var), max_var)
# #   delta_var[is.na(delta_var)] <- max_var
# # 
# #   wide_var <- (conc_range * 1.0)^2
# # 
# #   # Sigmoidal blend
# #   weight <- 1 / (1 + exp(-20 * (slope_ratio - 0.15)))
# #   per_sample_variance <- weight * delta_var + (1 - weight) * wide_var
# # 
# #   list(
# #     x_init              = x_init,
# #     per_sample_variance = per_sample_variance,
# #     slope_ratio         = slope_ratio,
# #     weight_informative  = weight,
# #     prior_type          = ifelse(weight > 0.5, "delta_method", "wide_prior")
# #   )
# # }
# # compute_x_init <- function(model_name, params, resid_var, y_obs,
# #                            x_min, x_max, conc_range,
# #                            sc_x_min = NULL, sc_x_max = NULL) {
# #   fwd <- get_forward_function(model_name)
# #   
# #   # Default SC bounds if not supplied
# #   if (is.null(sc_x_min)) sc_x_min <- x_min + conc_range
# #   if (is.null(sc_x_max)) sc_x_max <- x_max - conc_range
# #   sc_mid <- (sc_x_min + sc_x_max) / 2
# #   
# #   # SC endpoint responses for directional fallback
# #   y_at_sc_min  <- fwd(sc_x_min, params)
# #   y_at_sc_max  <- fwd(sc_x_max, params)
# #   y_lower_asym <- min(y_at_sc_min, y_at_sc_max)
# #   y_upper_asym <- max(y_at_sc_min, y_at_sc_max)
# #   x_for_low_y  <- if (y_at_sc_min < y_at_sc_max) sc_x_min else sc_x_max
# #   x_for_high_y <- if (y_at_sc_min < y_at_sc_max) sc_x_max else sc_x_min
# #   
# #   # Invert curve at each y
# #   x_init <- vapply(y_obs, function(yi) {
# #     tryCatch(
# #       uniroot(function(x) fwd(x, params) - yi,
# #               interval = c(x_min, x_max), tol = 1e-8)$root,
# #       error = function(e) NA_real_
# #     )
# #   }, numeric(1))
# #   
# #   # OLD — replaced:
# #   # x_init[is.na(x_init)] <- (x_min + x_max) / 2
# #   
# #   # NEW — goes here, directly after the vapply block:
# #   na_idx <- which(is.na(x_init))
# #   for (i in na_idx) {
# #     yi <- y_obs[i]
# #     x_init[i] <- if (is.na(yi) || !is.finite(yi)) {
# #       sc_mid
# #     } else if (yi <= y_lower_asym) {
# #       x_for_low_y - conc_range * 0.1
# #     } else if (yi >= y_upper_asym) {
# #       x_for_high_y + conc_range * 0.1
# #     } else {
# #       sc_mid
# #     }
# #   }
# #   
# #   # rest unchanged from your file [1]
# #   margin <- (x_max - x_min) * 0.001
# #   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
# #   
# #   # Slope at each x_init
# #   slope <- vapply(x_init, function(xi) {
# #     h <- 1e-5
# #     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
# #   }, numeric(1))
# #   
# #   # Max slope across curve
# #   x_grid <- seq(x_min, x_max, length.out = 500)
# #   max_slope <- max(vapply(x_grid, function(xi) {
# #     h <- 1e-5
# #     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
# #   }, numeric(1)), na.rm = TRUE)
# #   
# #   slope_ratio <- pmin(abs(slope) / max_slope, 1.0)
# #   
# #   # Delta-method variance
# #   delta_var <- (resid_var / slope)^2
# #   min_var <- (conc_range * 0.001)^2
# #   max_var <- (conc_range * 2)^2
# #   delta_var <- pmin(pmax(delta_var, min_var), max_var)
# #   delta_var[is.na(delta_var)] <- max_var
# #   
# #   wide_var <- (conc_range * 1.0)^2
# #   
# #   # Sigmoidal blend
# #   weight <- 1 / (1 + exp(-20 * (slope_ratio - 0.15)))
# #   per_sample_variance <- weight * delta_var + (1 - weight) * wide_var
# #   
# #   list(
# #     x_init              = x_init,
# #     per_sample_variance = per_sample_variance,
# #     slope_ratio         = slope_ratio,
# #     weight_informative  = weight,
# #     prior_type          = ifelse(weight > 0.5, "delta_method", "wide_prior")
# #   )
# # }
# # ============================================================
# # Main function
# #
# #   glance_row   : single row from best_glance
# #   best_pred_df : rows from best_pred for this glance_row
# #                  (x column used for concentration bounds)
# #   sample_df    : dataframe to predict on
# #                  - pred grid:  best_pred rows, response_col = "yhat"
# #                  - samples:    best_sample rows, response_col = "assay_response"
# #   response_col : column name holding the response values
# # ============================================================
# run_jags_predicted_concentration <- function(
#     glance_row,
#     best_pred_df,
#     sample_df,
#     response_col,
#     adapt_steps = 500,
#     burn_in_steps = 2000,
#     num_saved_steps = 10000,
#     thin_steps = 2,
#     n_chains = 3,
#     verbose = TRUE
# ){
#   
#   model_name <- trimws(glance_row$model_name)
#   
#   params <- extract_params(glance_row)
#   
#   fwd <- get_forward_function(model_name)
#   
#   y_obs <- sample_df[[response_col]]
#   
#   conc_range <- diff(range(best_pred_df$concentration))
#   
#   x_min <- min(best_pred_df$concentration) - conc_range*2
#   x_max <- max(best_pred_df$concentration) + conc_range*2
#   
#   # Posterior inversion
#   x_est <- invert_response_fast(
#     y_obs,
#     params,
#     model_name,
#     x_min,
#     x_max
#   )
#   
#   y_hat <- fwd(x_est, params)
#   
#   resid_sigma <- sqrt(glance_row$resid_sample_variance)
#   
#   data_list <- list(
#     y = y_obs,
#     y_hat = y_hat,
#     N = length(y_obs),
#     sigma_upper = resid_sigma * 5
#   )
#   
#   model_string <- get_jags_calibration_model()
#   
#   init_list <- lapply(seq_len(n_chains), function(i){
#     
#     list(
#       sigma = resid_sigma * runif(1,0.8,1.2),
#       nu = runif(1,3,15)
#     )
#     
#   })
#   
#   jm <- rjags::jags.model(
#     textConnection(model_string),
#     data = data_list,
#     inits = init_list,
#     n.chains = n_chains,
#     n.adapt = adapt_steps,
#     quiet = !verbose
#   )
#   
#   update(jm, burn_in_steps)
#   
#   samps <- rjags::coda.samples(
#     jm,
#     variable.names = c("sigma","nu"),
#     n.iter = num_saved_steps,
#     thin = thin_steps
#   )
#   
#   chain <- as.matrix(samps)
#   
#   n_draws <- nrow(chain)
#   
#   conc_draws <- matrix(
#     NA,
#     nrow = n_draws,
#     ncol = length(x_est)
#   )
#   
#   for(i in seq_len(n_draws)){
#     
#     y_sim <- rnorm(
#       length(y_hat),
#       mean = y_hat,
#       sd = chain[i,"sigma"]
#     )
#     
#     conc_draws[i,] <- invert_response_fast(
#       y_sim,
#       params,
#       model_name,
#       x_min,
#       x_max
#     )
#     
#   }
#   
#   sample_df$raw_robust_concentration <- apply(conc_draws,2,median)
#   
#   sample_df$se_robust_concentration <- apply(conc_draws,2,sd)
#   
#   chain_original <- 10^conc_draws
#   
#   sample_df$pcov_robust_concentration <- apply(chain_original,2,function(x){
#     
#     pmin(sd(x)/abs(mean(x))*100,125)
#     
#   })
#   
#   sample_df
#   
# }
# 
# # run_jags_predicted_concentration <- function(
#     #     glance_row,
# #     best_pred_df,
# #     sample_df,
# #     response_col,
# #     adapt_steps     = 500,
# #     burn_in_steps   = 5000,
# #     num_saved_steps = 20000,
# #     thin_steps      = 2,
# #     n_chains        = 3,
# #     verbose         = TRUE
# # ) {
# #   model_name <- trimws(as.character(glance_row$model_name))
# #   params     <- extract_params(glance_row)
# #   resid_var  <- glance_row$resid_sample_variance
# # 
# #   if (verbose) {
# #     cat("Model:", model_name, "\n")
# #     cat("Parameters:\n"); print(params)
# #     cat("Residual variance:", resid_var, "\n")
# #   }
# # 
# #   # --- Concentration bounds from best_pred$x ---
# #   sc_x_min   <- min(best_pred_df$concentration)    # store FIRST
# #   sc_x_max   <- max(best_pred_df$concentration)
# #   
# #   conc_range <- max(best_pred_df$concentration) - min(best_pred_df$concentration)
# #   x_min <- min(best_pred_df$concentration) - conc_range * 2
# #   x_max <- max(best_pred_df$concentration) + conc_range * 2
# # 
# #   # --- Initial x + adaptive prior ---
# #   y_obs <- sample_df[[response_col]]
# # 
# #   xinit <- compute_x_init(model_name, params, resid_var, y_obs,
# #                           x_min, x_max, conc_range)
# # 
# #   if (verbose) {
# #     cat("Slope ratio range:", round(range(xinit$slope_ratio), 4), "\n")
# #     cat("Prior type counts:\n"); print(table(xinit$prior_type))
# #     cat("Initial x range:", round(range(xinit$x_init), 4), "\n")
# #     cat("Bounds: [", round(x_min, 4), ",", round(x_max, 4), "]\n")
# #   }
# # 
# #   # --- Clamp variance ---
# #   max_var <- (conc_range * 10)^2
# #   min_var <- (conc_range * 0.001)^2
# #   psv <- pmin(pmax(xinit$per_sample_variance, min_var), max_var)
# #   psv[is.na(psv)] <- max_var
# #   tau_x <- 1 / psv
# # 
# #   # --- JAGS data ---
# #   resid_sigma <- sqrt(resid_var)
# # 
# #   data_list <- list(
# #     y           = y_obs,
# #     N           = length(y_obs),
# #     a           = unname(params["a"]),
# #     b           = unname(params["b"]),
# #     c           = unname(params["c"]),
# #     d           = unname(params["d"]),
# #     x_min       = x_min,
# #     x_max       = x_max,
# #     sigma_upper = resid_sigma * 5,
# #     x_prior     = xinit$x_init,
# #     tau_x       = tau_x
# #   )
# # 
# #   if (model_name %in% c("Y5", "Yd5")) {
# #     data_list$g <- unname(params["g"])
# #   }
# # 
# #   # --- MCMC ---
# #   model_string <- get_jags_calibration_model(model_name)
# # 
# #   init_list <- lapply(seq_len(n_chains), function(i) {
# #     list(
# #       x     = xinit$x_init + rnorm(length(xinit$x_init), 0, 0.05),
# #       sigma = resid_sigma * runif(1, 0.8, 1.2),
# #       nu    = runif(1, 3, 15)
# #     )
# #   })
# # 
# #   n_iter <- ceiling((num_saved_steps * thin_steps) / n_chains)
# # 
# #   if (verbose) {
# #     cat("MCMC: chains=", n_chains, " adapt=", adapt_steps,
# #         " burn=", burn_in_steps, " iter/chain=", n_iter,
# #         " thin=", thin_steps, "\n")
# #   }
# # 
# #   jm <- jags.model(
# #     textConnection(model_string),
# #     data     = data_list,
# #     inits    = init_list,
# #     n.chains = n_chains,
# #     n.adapt  = adapt_steps,
# #     quiet    = !verbose
# #   )
# #   update(jm, burn_in_steps)
# # 
# #   samps <- coda.samples(jm, variable.names = "x",
# #                         n.iter = n_iter, thin = thin_steps)
# # 
# #   # --- Summarise ---
# #   chain <- as.matrix(samps)
# # 
# #   sample_df$se_robust_concentration  <- apply(chain, 2, sd)
# #   sample_df$raw_robust_concentration <- apply(chain, 2, quantile, 0.50)
# # 
# #   chain_original <- 10^chain
# #   sample_df$pcov_robust_concentration <- apply(chain_original, 2, function(x) {
# #     pmin(sd(x) / abs(mean(x)) * 100, 125)
# #   })
# # 
# #   if (verbose) {
# #     cat("\nStandard curve x range:", round(range(best_pred_df$concentration), 4), "\n")
# #     cat("Predicted median range:", round(range(sample_df$raw_robust_concentration), 4), "\n")
# #     cat("CV range:              ", round(range(sample_df$pcov_robust_concentration), 4), "\n")
# #   }
# # 
# #   return(sample_df)
# # }
# 
# ###### Shiny Side 
# process_jag_result <- function(df, df_name = c("pred_se", "sample_se")) {
#   
#   df_name <- match.arg(df_name)
#   
#   if (df_name == "pred_se") {
#     names(df)[names(df) == "row_id"] <- "best_pred_all_id"
#     df <- df[, !names(df) %in% c("dilution", "best_glance_all_id", "concentration", "assay_response", "mcmc_set")]
#     
#     
#   } else if (df_name == "sample_se") {
#     names(df)[names(df) == "row_id"] <- "best_sample_se_all_id"
#     df <- df[, !names(df) %in% c("concentration", "dilution", "assay_response", "best_glance_all_id", "mcmc_set")]
#   }
#   
#   return(df)
# }
# 
# filter_glance_scope <- function(df, scope, experiment, plate) {
#   
#   switch(
#     scope,
#     plate      = df[df$experiment_accession == experiment &
#                       df$plate_nom == plate, ],
#     
#     experiment = df[df$experiment_accession == experiment, ],
#     
#     study      = df
#   )
# }
# 
# update_mcmc_progress <- function(i, total, row) {
#   
#   msg <- paste0(
#     "MCMC Robust: ", i, " / ", total, "\n",
#     "Study: ",      row$study_accession, "\n",
#     "Experiment: ", row$experiment_accession, "\n",
#     "Plate: ",      row$plate_nom, "\n",
#     "Antigen: ",    row$antigen, "\n",
#     "Model: ",      row$model_name
#   )
#   
#   showNotification(
#     id = "mcmc_calc_notify",
#     div(class = "big-notification",
#         style = "white-space: pre-line;",
#         msg),
#     duration = NULL
#   )
# }
# 
# 
# 
# 
# # -----------------------------------------------------------------
# # 1. Fetch the job-status table + fill missing combos
# # -----------------------------------------------------------------
# get_existing_concentration_calc <- function(conn,
#                                             project_id,
#                                             study_accession,
#                                             experiment_accession,
#                                             plate_nom) {
#   
#   query <- glue::glue(
#     "SELECT * FROM madi_results.get_job_status2(
#         {project_id},
#         '{study_accession}',
#         '{experiment_accession}',
#         '{plate_nom}'
#     );"
#   )
#   
#   df <- dbGetQuery(conn, query)
#   
#   # Normalize label for UI
#   df$job_status[df$job_status == "partial completion"] <- 
#     "partially completed"
#   
#   return(df)
# }
# # get_existing_concentration_calc <- function(conn,
# #                                             project_id,
# #                                             study_accession,
# #                                             experiment_accession,
# #                                             plate_nom,
# #                                             all_methods = c("interpolated", "mcmc_robust"),
# #                                             all_scopes  = c("study", "experiment", "plate")) {
# #   
# #   query <- glue::glue(
# #     "SELECT * FROM madi_results.get_job_status_v4({project_id},
# #                                                   '{study_accession}',
# #                                                   '{experiment_accession}',
# #                                                   '{plate_nom}');"
# #   )
# #   
# #   df <- dbGetQuery(conn, query)
# #   
# #   # Remove unwanted / NULL methods
# #   df <- df[!is.na(df$concentration_calc_method) &
# #              df$concentration_calc_method != "none", , drop = FALSE]
# #   
# #   # Map DB status to UI status
# #   df$job_status <- ifelse(
# #     df$job_status == "partial completion",
# #     "partially completed",
# #     df$job_status
# #   )
# #   # Build full grid to guarantee all scope/method combos exist
# #   full_grid <- expand.grid(
# #     scope                     = all_scopes,
# #     concentration_calc_method = all_methods,
# #     stringsAsFactors          = FALSE
# #   )
# #   
# #   df$key        <- paste(df$scope, df$concentration_calc_method, sep = "|")
# #   full_grid$key <- paste(full_grid$scope, full_grid$concentration_calc_method, sep = "|")
# #   
# #   missing <- full_grid[!full_grid$key %in% df$key, ]
# #   
# #   if (nrow(missing) > 0) {
# #     missing_rows <- data.frame(
# #       scope                     = missing$scope,
# #       concentration_calc_method = missing$concentration_calc_method,
# #       job_status                = "not begun",
# #       incomplete_items          = NA,
# #       stringsAsFactors          = FALSE
# #     )
# #     
# #     df <- rbind(df[, names(missing_rows)], missing_rows)
# #   }
# #   
# #   df$key <- NULL
# #   
# #   return(df)
# # }
# # get_existing_concentration_calc <- function(conn,
# #                                             project_id,
# #                                             study_accession,
# #                                             experiment_accession,
# #                                             plate_nom,
# #                                             all_methods = c("interpolated", "mcmc_robust"),
# #                                             all_scopes  = c("study", "experiment", "plate")) {
# #   
# #   query <- glue::glue(
# #     "SELECT * FROM madi_results.get_job_status({project_id},
# #                                                '{study_accession}',
# #                                                '{experiment_accession}',
# #                                                '{plate_nom}');"
# #   )
# #   
# #   print(query)
# #   df <- dbGetQuery(conn, query)
# #   print(df)
# #   print(project_id)
# #   print(experiment_accession)
# #   print(plate_nom)
# #   
# #   # Remove unwanted method
# #   #df <- df[df$concentration_calc_method != "none", , drop = FALSE]
# #   df <- df[!is.na(df$concentration_calc_method) &
# #          df$concentration_calc_method != "none", , drop = FALSE]
# #   
# #   # if (nrow(df) == 0) {
# #   #   # If nothing exists yet, return full grid with default status
# #   #   df <- expand.grid(
# #   #     scope                     = all_scopes,
# #   #     concentration_calc_method = all_methods,
# #   #     job_status = "not begun",
# #   #     stringsAsFactors          = FALSE
# #   #   )
# #   #   return(df)
# #   # }
# #   
# #   full_grid <- expand.grid(
# #     scope                     = all_scopes,
# #     concentration_calc_method = all_methods,
# #     stringsAsFactors          = FALSE
# #   )
# #   
# #   # Build key for comparison
# #   df$key        <- paste(df$scope, df$concentration_calc_method, sep = "|")
# #   full_grid$key <- paste(full_grid$scope, full_grid$concentration_calc_method, sep = "|")
# #   
# #   missing <- full_grid[!full_grid$key %in% df$key, ]
# #   
# #   if (nrow(missing) > 0) {
# #     missing_rows <- data.frame(
# #       scope                     = missing$scope,
# #       concentration_calc_method = missing$concentration_calc_method,
# #       job_status                = "not begun",
# #       stringsAsFactors          = FALSE
# #     )
# #     
# #     df <- rbind(df[, names(missing_rows)], missing_rows)
# #   }
# #   
# #   df$key <- NULL
# #   
# #   return(df)
# # }
# 
# 
# 
# # -----------------------------------------------------------------
# # 2. Build a single status badge
# # -----------------------------------------------------------------
# # -----------------------------------------------------------------
# createStatusBadge <- function(method,
#                               existing_concentration_calc,
#                               scope,
#                               progress_msg = NULL) {  # kept for API compat; no longer used
#   
#   row <- existing_concentration_calc[
#     existing_concentration_calc$concentration_calc_method == method &
#       existing_concentration_calc$scope == scope,
#   ]
#   
#   if (nrow(row) == 0) return(NULL)
#   
#   type_status <- row$job_status[1]
#   
#   incomplete <- if (
#     "incomplete_items" %in% names(row) &&
#     type_status == "partially completed"
#   ) {
#     row$incomplete_items[1]
#   } else {
#     NA
#   }
#   
#   has_detail <- type_status == "partially completed" &&
#     !is.na(incomplete) && nzchar(incomplete)
#   
#   bg_color <- switch(type_status,
#                      "partially completed" = "#6f42c1",
#                      "pending"             = "#FFA500",
#                      "completed"           = "#28a745",
#                      "not begun"           = "#dc3545",
#                      NULL
#   )
#   if (is.null(bg_color)) return(NULL)
#   
#   # ── Badge label — NO title attr, NO data-toggle, NO cursor:help on badge ──
#   badge_label <- switch(type_status,
#                         
#                         "partially completed" = tagList(
#                           tags$i(class = "fa fa-layer-group"), " Partially Completed",
#                           if (has_detail) tags$span(
#                             # sc-tip-parent wraps both the ? icon and the popup span.
#                             # The CSS rule .sc-tip-parent:hover .sc-tip-box makes the popup
#                             # appear only while the mouse is over this span.
#                             class = "sc-tip-parent",
#                             style = "position:relative; display:inline-block; margin-left:5px; cursor:help;",
#                             tags$i(class = "fa fa-question-circle", style = "font-size:11px;"),
#                             tags$span(
#                               class = "sc-tip-box",
#                               style = paste0(
#                                 "display:none; ",
#                                 "position:absolute; bottom:130%; left:50%; ",
#                                 "transform:translateX(-50%); ",
#                                 "background:#333; color:#fff; ",
#                                 "padding:5px 9px; border-radius:4px; ",
#                                 "font-size:11px; white-space:pre-line; ",
#                                 "min-width:210px; text-align:left; ",
#                                 "pointer-events:none; z-index:9999; line-height:1.4;"
#                               ),
#                               paste("Incomplete:", incomplete)
#                             )
#                           )
#                         ),
#                         
#                         "pending" = {
#                           has_progress <- !is.null(progress_msg) && nzchar(progress_msg)
#                           tagList(
#                             if (has_progress) tags$style(HTML(
#                               ".sc-tip-parent:hover .sc-tip-box { display:block !important; }"
#                             )),
#                             tags$span(
#                               class = if (has_progress) "sc-tip-parent" else NULL,
#                               style = "position:relative; display:inline-block;",
#                               tags$i(class = "fa fa-spinner fa-spin"), " Running...",
#                               if (has_progress) tags$span(
#                                 class = "sc-tip-box",
#                                 style = paste0(
#                                   "display:none; ",
#                                   "position:absolute; bottom:130%; left:50%; ",
#                                   "transform:translateX(-50%); ",
#                                   "background:#333; color:#fff; ",
#                                   "padding:5px 9px; border-radius:4px; ",
#                                   "font-size:11px; white-space:pre-line; ",
#                                   "min-width:210px; text-align:left; ",
#                                   "pointer-events:none; z-index:9999; line-height:1.4;"
#                                 ),
#                                 progress_msg
#                               )
#                             )
#                           )
#                         },
#                         
#                         "completed" = tagList(
#                           tags$i(class = "fa fa-check"), " Completed"
#                         ),
#                         
#                         "not begun" = tagList(
#                           tags$i(class = "fa fa-times"), " Not Begun"
#                         ),
#                         
#                         NULL
#   )
#   if (is.null(badge_label)) return(NULL)
#   
#   style_tag <- if (has_detail) {
#     tags$style(HTML(
#       ".sc-tip-parent:hover .sc-tip-box { display:block !important; }"
#     ))
#   } else NULL
#   
#   tagList(
#     style_tag,
#     tags$span(
#       class = "badge",
#       style = paste0(
#         "display:inline-block; position:relative; ",
#         "padding:4px 10px; border-radius:10px; font-size:12px; ",
#         "background-color:", bg_color, "; color:white; white-space:nowrap;"
#       ),
#       badge_label
#     )
#   )
# }
# # createStatusBadge <- function(method, existing_concentration_calc, scope) {
# #   
# #   row <- existing_concentration_calc[
# #     existing_concentration_calc$concentration_calc_method == method &
# #       existing_concentration_calc$scope == scope,
# #   ]
# #   
# #   if (nrow(row) == 0) return(NULL)
# #   
# #   # type_status <- row$job_status[1]
# #   # incomplete  <<- row$incomplete_items[1]
# #   
# #   type_status <- row$job_status[1]
# #   
# #   incomplete <- if (
# #     "incomplete_items" %in% names(row) &&
# #     type_status == "partially completed"
# #   ) {
# #     row$incomplete_items[1]
# #   } else {
# #     NA
# #   }
# #   
# #   print(incomplete)
# #   
# #   # Tooltip if incomplete items exist
# #   # tooltip <- if (!is.na(incomplete) && incomplete != "") {
# #   #   paste("Incomplete:", incomplete)
# #   # } else {
# #   #   NULL
# #   # }
# #   
# #   tooltip <- if (
# #     !is.na(incomplete) &&
# #     nzchar(incomplete)
# #   ) {
# #     paste("Incomplete:", incomplete)
# #   } else {
# #     NULL
# #   }
# #   
# #   status_style <- switch(type_status,
# #                          "partially completed" = "background-color: #6f42c1; color: white;",
# #                          "pending"             = "background-color: #FFA500; color: white;",
# #                          "completed"           = "background-color: #28a745; color: white;",
# #                          "not begun"           = "background-color: #dc3545; color: white;",
# #                          NULL
# #   )
# #   
# #   status_text <- switch(type_status,
# #                         "partially completed" = tagList(
# #                           tags$i(class = "fa fa-layer-group"),
# #                           " Partially Completed"
# #                         ),
# #                         "pending" = tagList(
# #                           tags$i(class = "fa fa-spinner fa-spin"),
# #                           " Running..."
# #                         ),
# #                         "completed" = tagList(
# #                           tags$i(class = "fa fa-check"),
# #                           " Completed"
# #                         ),
# #                         "not begun" = tagList(
# #                           tags$i(class = "fa fa-times"),
# #                           " Not Begun"
# #                         ),
# #                         NULL
# #   )
# #   
# #   if (!is.null(status_style) && !is.null(status_text)) {
# #     span(
# #       class = "badge",
# #       title = tooltip,
# #       style = paste0(
# #         "padding: 4px 10px; border-radius: 10px; font-size: 12px; ",
# #         status_style
# #       ),
# #       status_text
# #     )
# #    
# #   } else {
# #     NULL
# #   }
# # }
# 
# # -----------------------------------------------------------------
# # 3. Get status for a specific scope + method
# # -----------------------------------------------------------------
# get_status <- function(existing_concentration_calc, scope, method) {
#   
#   sts <- existing_concentration_calc$job_status[
#     existing_concentration_calc$concentration_calc_method == method &
#       existing_concentration_calc$scope == scope
#   ]
#   if (length(sts) == 0) return("not begun")
#   sts[1]
# }
# 
# # -----------------------------------------------------------------
# # 4. Build the status grid + conditional buttons
# # -----------------------------------------------------------------
# createStandardCurveConcentrationTypeUI <- function(existing_concentration_calc, progress_msg = NULL) {
#   concentrationUIRefresher()
#   
#   ## Method display labels
#   method_labels <- c(
#     "interpolated" = "Interpolated",
#     "mcmc_robust"  = "MCMC Robust"
#   )
#   
#   scope_labels <- c(
#     "study"      = "Study (All Experiments)",
#     "experiment" = "Experiment (Current)",
#     "plate"      = "Plate (Current)"
#   )
#   
#   scope_icons <- c(
#     "study"      = "fa-flask",
#     "experiment" = "fa-vial",
#     "plate"      = "fa-th"
#   )
#   
#   all_methods <- c("interpolated", "mcmc_robust")
#   all_scopes  <- c("study", "experiment", "plate")
#   
#   ## ── Build the status grid as an HTML table ──
#   header_cells <- lapply(all_scopes, function(s) {
#     tags$th(
#       style = "text-align:center; padding:10px 15px; font-size:14px;",
#       tags$i(class = paste("fa", scope_icons[s]), style = "margin-right:5px;"),
#       scope_labels[s]
#     )
#   })
#   
#   body_rows <- lapply(all_methods, function(m) {
#     cells <- lapply(all_scopes, function(s) {
#       tags$td(
#         style = "text-align:center; padding:10px 15px; vertical-align:middle;",
#         # createStatusBadge(m, existing_concentration_calc, s)
#         # ── Pass progress_msg only for the mcmc_robust method ──
#         createStatusBadge(
#           method                    = m,
#           existing_concentration_calc = existing_concentration_calc,
#           scope                     = s,
#           progress_msg              = if (m == "mcmc_robust") progress_msg else NULL
#         )
#         
#       )
#     })
#     tags$tr(
#       tags$td(
#         style = "padding:10px 15px; font-weight:bold; vertical-align:middle;",
#         method_labels[m]
#       ),
#       cells
#     )
#   })
#   
#   status_grid <- tags$table(
#     class = "table table-bordered",
#     style = "width:100%; margin-top:10px; border-radius:8px;",
#     tags$thead(
#       tags$tr(
#         tags$th(style = "padding:10px 15px;", "Method"),
#         header_cells
#       )
#     ),
#     tags$tbody(body_rows)
#   )
#   
#   ## ── Scope selector ──
#   scope_selector <- radioButtons(
#     inputId  = "save_scope",
#     label    = "Calculation scope:",
#     choices  = c(
#       "Current Plate"      = "plate",
#       "Current Experiment" = "experiment",
#       "All Experiments"    = "study"
#     ),
#     selected = "study",
#     inline   = TRUE
#   )
#   
#   ## ── Buttons section (rendered server-side for conditional logic) ──
#   buttons_section <- uiOutput("concentration_buttons_ui")
#   
#   ## ── Assemble everything ──
#   tagList(
#     tags$head(tags$style(HTML("
#       .conc-btn {
#         padding: 12px 30px;
#         font-size: 14px;
#         line-height: 1.5;
#         white-space: normal;
#         margin: 5px;
#         border-radius: 5px;
#         color: white;
#         border: none;
#         cursor: pointer;
#       }
#       .conc-btn-green {
#         background-color: #7DAF4C;
#         border-color: #91CF60;
#       }
#       .conc-btn-green:hover {
#         background-color: #6B9A3F;
#       }
#       .conc-btn-blue {
#         background-color: #4A90D9;
#         border-color: #5BA0E9;
#       }
#       .conc-btn-blue:hover {
#         background-color: #3D7DC0;
#       }
#       .conc-btn-disabled {
#         background-color: #cccccc;
#         border-color: #bbbbbb;
#         color: #666666;
#         cursor: not-allowed;
#       }
#     "))),
#     
#     wellPanel(
#       tags$h4(
#         #tags$i(class = "fa fa-table", style = "margin-right:8px;"),
#         "Calculation Status of Standard Curves by Concentration Prediction Method"
#       ),
#       tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
#       status_grid,
#       tags$hr(),
#       scope_selector,
#       buttons_section
#     )
#   )
# }
# 
# 
# # -----------------------------------------------------------------
# # Remove a specific scope+method entry from the pending overlay.
# # Called from both onFulfilled and onRejected so the badge always
# # clears regardless of outcome.
# # -----------------------------------------------------------------
# .remove_pending <- function(pending_rv, scope, method) {
#   updated <- Filter(
#     function(entry) {
#       !(entry[["scope"]] == scope && entry[["method"]] == method)
#     },
#     pending_rv()
#   )
#   pending_rv(updated)
# }
# 
# # -----------------------------------------------------------------
# # Internal helper — called by both the first-run and rerun observers.
# # All arguments are plain R values (NOT reactives).
# # -----------------------------------------------------------------
# .launch_mcmc <- function(scope, study, experiment, plate, proj,
#                          scope_label, session) {
#   
#   # ── 1. Set processing flag ──────────────────────────────────────
#   is_batch_processing(TRUE)
#   
#   # ── 2. Inject "pending" into the in-memory overlay ─────────────
#   current_pending <- mcmc_pending_scopes()
#   mcmc_pending_scopes(c(
#     current_pending,
#     list(c(scope = scope, method = "mcmc_robust"))
#   ))
#   
#   # ── 3. Trigger UI refresh so badge shows "pending" now ──────────
#   concentrationUIRefresher(concentrationUIRefresher() + 1)
#   
#   # ── 4. Progress file for IPC ────────────────────────────────────
#   prog_file <- tempfile(pattern = "mcmc_progress_", fileext = ".txt")
#   writeLines("Starting MCMC Robust...", prog_file)
#   mcmc_progress_file(prog_file)
#   mcmc_progress_msg(paste0(
#     "Running MCMC Robust\n",
#     "Scope: ", scope_label, "\n",
#     "Study: ", study, "\n",
#     "Experiment: ", experiment
#   ))
#   
#   # ── 5. Persistent running notification ──────────────────────────
#   showNotification(
#     id  = "mcmc_calc_notify",
#     div(
#       class = "big-notification",
#       paste0("Starting MCMC Robust for ", scope_label, "...")
#     ),
#     duration = 10
#   )
#   
#   # ── 6. Snapshot reactive data ───────────────────────────────────
#   best_glance_snapshot <- tryCatch(
#     fetch_best_glance_mcmc(
#       study_accession = study,
#       project_id      = proj,
#       conn            = conn
#     ),
#     error = function(e) NULL
#   )
#   
#   .mcmc_guard <- function(condition, msg, type = "warning") {
#     if (condition) {
#       showNotification(msg, type = type)
#       removeNotification("mcmc_calc_notify")
#       .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
#       mcmc_progress_msg(NULL)
#       mcmc_progress_file(NULL)
#       concentrationUIRefresher(concentrationUIRefresher() + 1)
#       is_batch_processing(FALSE)
#       return(TRUE)
#     }
#     FALSE
#   }
#   
#   if (.mcmc_guard(
#     is.null(best_glance_snapshot) || nrow(best_glance_snapshot) == 0,
#     "No fitted curves found."
#   )) return()
#   
#   best_glance_snapshot <- filter_glance_scope(
#     best_glance_snapshot, scope, experiment, plate
#   )
#   
#   if (.mcmc_guard(
#     nrow(best_glance_snapshot) == 0,
#     "No fitted curves found for this scope."
#   )) return()
#   
#   id_set  <- best_glance_snapshot$best_glance_all_id
#   n_total <- length(id_set)
#   
#   combined_df_snapshot <- tryCatch(
#     fetch_combined_mcmc(
#       study_accession = study,
#       project_id      = proj,
#       best_glance_ids = id_set,
#       conn            = conn
#     ),
#     error = function(e) NULL
#   )
#   
#   if (.mcmc_guard(
#     is.null(combined_df_snapshot) || nrow(combined_df_snapshot) == 0,
#     "No prediction data found for MCMC.",
#     type = "error"
#   )) return()
#   
#   db_conn_args <- get_db_connection_args()
#   
#   # ── 7. Launch future ────────────────────────────────────────────
#   future_promise <- future::future({
#     
#     bg_conn <- do.call(get_db_connection_from_args, db_conn_args)
#     on.exit(DBI::dbDisconnect(bg_conn), add = TRUE)
#     
#     results     <- vector("list", length(id_set))
#     best_glance <- best_glance_snapshot
#     
#     for (i in seq_along(id_set)) {
#       
#       id  <- id_set[i]
#       row <- best_glance[best_glance$best_glance_all_id == id, ]
#       
#       progress_text <- paste0(
#         "MCMC Robust: ", i, " / ", n_total, "\n",
#         "Study:      ", row$study_accession,     "\n",
#         "Experiment: ", row$experiment_accession, "\n",
#         "Plate:      ", row$plate_nom,            "\n",
#         "Antigen:    ", row$antigen,              "\n",
#         "Model:      ", row$model_name
#       )
#       tryCatch(writeLines(progress_text, prog_file), error = function(e) NULL)
#       message(progress_text)
#       
#       curve_df <- combined_df_snapshot[
#         combined_df_snapshot$best_glance_all_id == id, ]
#       pred_df  <- curve_df[curve_df$mcmc_set == "pred_se", ]
#       if (nrow(pred_df) == 0) next
#       
#       res <- tryCatch(
#         run_jags_predicted_concentration(
#           glance_row   = row,
#           best_pred_df = pred_df,
#           sample_df    = curve_df,
#           response_col = "assay_response",
#           verbose      = TRUE
#         ),
#         error = function(e) { message("JAGS error: ", e$message); NULL }
#       )
#       
#       if (!is.null(res)) {
#         if (!"mcmc_set" %in% names(res))
#           res$mcmc_set <- curve_df$mcmc_set[match(res$row_id, curve_df$row_id)]
#         results[[i]] <- res
#       }
#     }
#     
#     results_df <- do.call(rbind, Filter(Negate(is.null), results))
#     if (is.null(results_df) || nrow(results_df) == 0) stop("MCMC produced no results.")
#     
#     result_pred_all   <- results_df[results_df$mcmc_set == "pred_se",   ]
#     result_sample_all <- results_df[results_df$mcmc_set == "sample_se", ]
#     result_sample_all$final_robust_concentration <-
#       result_sample_all$dilution * result_sample_all$raw_robust_concentration
#     
#     best_glance$last_concentration_calc_method[
#       best_glance$best_glance_all_id %in% id_set
#     ] <- "mcmc_robust"
#     
#     result_pred_all2   <- process_jag_result(result_pred_all,   df_name = "pred_se")
#     result_sample_all2 <- process_jag_result(result_sample_all, df_name = "sample_se")
#     
#     update_combined_mcmc_bulk(
#       pred_all_mcmc        = result_pred_all2,
#       sample_all_mcmc      = result_sample_all2,
#       best_glance_complete = best_glance,
#       conn                 = bg_conn
#     )
#     
#     list(ok = TRUE, n_curves = nrow(best_glance), scope_label = scope_label)
#     
#   }, seed = TRUE)
#   
#   # ── 8. Poll progress file ────────────────────────────────────────
#   progress_poller <- reactivePoll(
#     intervalMillis = 2000,
#     session        = session,
#     checkFunc      = function() {
#       pf <- mcmc_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(0)
#       file.info(pf)$mtime
#     },
#     valueFunc      = function() {
#       pf <- mcmc_progress_file()
#       if (is.null(pf) || !file.exists(pf)) return(NULL)
#       tryCatch(paste(readLines(pf), collapse = "\n"), error = function(e) NULL)
#     }
#   )
#   
#   progress_observer <- observe({
#     msg <- progress_poller()
#     if (!is.null(msg) && nzchar(msg)) mcmc_progress_msg(msg)
#   })
#   
#   # ── 9. Handle promise resolution ────────────────────────────────
#   .cleanup_mcmc <- function(label, type = "message", duration = 10) {
#     progress_observer$destroy()
#     pf <- mcmc_progress_file()
#     if (!is.null(pf) && file.exists(pf)) file.remove(pf)
#     mcmc_progress_file(NULL)
#     mcmc_progress_msg(NULL)
#     .remove_pending(mcmc_pending_scopes, scope, "mcmc_robust")
#     showNotification(label, type = type, duration = duration)
#     concentrationUIRefresher(concentrationUIRefresher() + 1)
#     is_batch_processing(FALSE)
#   }
#   
#   promises::then(
#     future_promise,
#     onFulfilled = function(result) {
#       .cleanup_mcmc(
#         paste0("MCMC Robust completed for ", result$scope_label, ".")
#       )
#     },
#     onRejected = function(err) {
#       .cleanup_mcmc(
#         paste0("MCMC Robust error: ", conditionMessage(err)),
#         type     = "error",
#         duration = 15
#       )
#       message("MCMC future rejected: ", conditionMessage(err))
#     }
#   )
#   
#   NULL
# }
# 
# 
# ## ── Helpers (defined once, outside the renderUI) ─────────────────────────────
# 
# make_spinner_btn <- function(label_text, scope_label) {
#   tags$button(
#     class    = "conc-btn conc-btn-disabled",
#     disabled = "disabled",
#     tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
#     HTML(paste0(label_text, "<br>", scope_label))
#   )
# }
# 
# make_run_btn <- function(input_id, method_label, scope_label) {
#   actionButton(
#     inputId = input_id,
#     label   = HTML(paste0(
#       "Calculate <strong>", method_label, "</strong> concentrations<br>",
#       scope_label
#     ))
#   )
# }
# # make_spinner_btn <- function(label_text, scope_label) {
# #   tags$button(
# #     class    = "conc-btn conc-btn-disabled",
# #     disabled = "disabled",
# #     tags$i(class = "fa fa-spinner fa-spin", style = "margin-right:5px;"),
# #     HTML(paste0(label_text, "<br>", scope_label))
# #   )
# # }
# # 
# # make_run_btn <- function(input_id, method_label, scope_label) {
# #   actionButton(
# #     inputId = input_id,
# #     label   = HTML(paste0(
# #       "Calculate <strong>", method_label, "</strong> concentrations<br>",
# #       scope_label
# #     ))
# #   )
# # }
# 
# make_rerun_btn <- function(input_id, method_label, scope_label) {
#   actionButton(
#     inputId = input_id,
#     label   = HTML(paste0(
#       tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
#       " Rerun <strong>", method_label, "</strong> concentrations<br>",
#       "<small style='color:#ffc107;'>",
#       tags$i(class = "fa fa-exclamation-triangle"),
#       " Will overwrite existing results</small><br>",
#       scope_label
#     )),
#     style = paste0(
#       "border: 2px solid #ffc107; ",
#       "background-color: #fff8e1; ",
#       "color: #333;"
#     )
#   )
# }
# # make_rerun_btn <- function(input_id, method_label, scope_label) {
# #   actionButton(
# #     inputId = input_id,
# #     label   = HTML(paste0(
# #       tags$i(class = "fa fa-redo", style = "margin-right:5px;"),
# #       " Rerun <strong>", method_label, "</strong> concentrations<br>",
# #       "<small style='color:#ffc107;'>",
# #       tags$i(class = "fa fa-exclamation-triangle"),
# #       " Will overwrite existing results</small><br>",
# #       scope_label
# #     )),
# #     style = paste0(
# #       "border: 2px solid #ffc107; ",
# #       "background-color: #fff8e1; ",
# #       "color: #333;"
# #     )
# #   )
# # }
# 
# 
# make_method_btn <- function(status, input_id, method_label, scope_label) {
#   if (is.null(status) || is.na(status) || status == "not begun") {
#     make_run_btn(input_id, method_label, scope_label)
#   } else if (status == "pending") {
#     make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
#   } else if (status == "completed" || status == "partially completed") {
#     make_rerun_btn(input_id, method_label, scope_label)
#   } else {
#     # Fallback — treat unknown status as "not begun"
#     make_run_btn(input_id, method_label, scope_label)
#   }
# }
# # make_method_btn <- function(status, input_id, method_label, scope_label) {
# #   if (status == "pending") {
# #     make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
# #   } else if (status == "completed") {
# #     make_rerun_btn(input_id, method_label, scope_label)
# #   } else {
# #     make_run_btn(input_id, method_label, scope_label)
# #   }
# # }
# # make_method_btn <- function(status, input_id, method_label, scope_label) {
# #   if (is.null(status) || is.na(status)) {
# #     ## Safety net — treat missing status as "not begun"
# #     make_run_btn(input_id, method_label, scope_label)
# #   } else if (status == "pending") {
# #     make_spinner_btn(paste0("Calculating ", method_label, "..."), scope_label)
# #   } else if (status == "completed") {
# #     make_rerun_btn(input_id, method_label, scope_label)
# #   } else {
# #     ## "not begun" or anything else
# #     make_run_btn(input_id, method_label, scope_label)
# #   }
# # }
# 
# .remove_pending_interp <- function(interp_pending_scopes_rv, scope) {
#   current <- interp_pending_scopes_rv()
#   updated <- Filter(function(e) e[["scope"]] != scope, current)
#   interp_pending_scopes_rv(updated)
# }