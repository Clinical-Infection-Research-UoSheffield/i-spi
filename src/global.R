# Description: This file contains the global variables and libraries that are used in the app.

# Load the stanassay Bayesian ensemble package (R6-based; provides StanAssay class)
# The package provides hierarchical Bayesian standard curve fitting (4PL, 5PL, Gompertz
# ensemble with LOO-stacking model selection) via Stan MCMC.
#
# Installation:
#   - Docker/CI: installed from stanassay_0.0.0.9000.tar.gz bundled in this repo
#   - Local dev: R CMD INSTALL /path/to/stanassay
#
# NOTE: stanassay must be installed via R CMD INSTALL (not devtools::load_all())
# because it uses Stan + TBB within-chain threading compiled with -DSTAN_THREADS
# which is baked in at build time via src/Makevars.
tryCatch(
  library(stanassay),
  error = function(e) {
    warning(paste0(
      "[global.R] Could not load stanassay package: ", e$message,
      "\nBayesian ensemble features will be disabled at runtime.",
      "\nFor Docker builds, ensure stanassay tarball is in the repo and Dockerfile installs it."
    ))
  }
)

# Load necessary libraries
library(plotly);
library(shiny);
library(shinyjs);
library(shinyalert); library(shinydashboard);
library(shinyWidgets); library(shinyFiles);
library(shinybusy); library(shinyBS); library(readxl); library(openxlsx);
library(RPostgres); library(glue); library(DBI); library(DT); library(sqldf);
library(datamods); library(data.table); library(stringi); library(stringr);
library(tidyverse); library(tidyr); library(plyr); library(modelr); library(robustbase);
library(broom); library(rhandsontable); library(sendmailR);
library(reactable); library(gt); library(gtsummary); library(gtExtras); library(grid);
library(gridExtra); library(gtable); library(gtools); library(httr2); library(auth0);
library(moach); library(janitor); library(visNetwork); library(pdp); library(ggsci);
library(bslib)
library(aplpack)
library(weird)
library(ggplot2)
#library(gghdr)

## For study parameters
library(shinyFeedback)
library(later)
# For Study Overview
library(viridis)
library(htmltools)
library(ggrepel)
library(cowplot)



library(ks)     # For kernel density estimation
library(scales) # For color scaling
library(plotly) # For interactive plots
library(sp)

# For Standard Curve Fitting
library(socviz) # round_df function here
library(magrittr)
library(car) # for VIF
library(nlraa)
library(minpack.lm)
library(nls.multstart)
library(Deriv)
library(nlstools)
library(magrittr)
library(shinyWidgets)
library(formattable)
library(drda)
library(tmvtnorm)

## std-curver
library(patchwork)
library(rlang)
library(rjags)
library(bit64)
library(shinycssloaders)

# For standard curve Summary
library(fracture)

library(mgcv)
library(cgam)
library(extras)
library(Polychrome)
library(shinyjqui)
library(future)
library(promises)
library(progressr)

# For Subgroup detection
library(labelled)
library(flexmix)
library(factoextra)
library(cluster)

## For Subgroup Detection Summary
library(heatmaply)
library(dendextend)

## For Dilution analysis
library(data.tree)
library(DiagrammeR)
library(tidyr)
library(datamods)
library(digest)

## Dilution Linearity
library(strex)
library(purrr)

library(httr2)
library(jose)
library(openssl) # For rand_bytes
library(jsonlite)
library(urltools)

library(shiny.destroy)


# Enable progressr with shiny
handlers(global = TRUE)
handlers("shiny")

# Simple setup based on platform
if (Sys.info()["sysname"] == "Windows" || exists("RStudio.Version", envir = globalenv())) {
  plan(multisession)
  message("Using multisession plan (Windows/RStudio)")
} else {
  plan(multicore)
  message("Using multicore plan (Unix/Linux)")
}

# Set common options
options(future.globals.maxSize = 5000 * 1024^2)  # 5GB
options(future.rng.onMisuse = "ignore")

options(shiny.promise.backend = "future")
options(future.rng.onMisuse = "ignore")
options(shiny.maxRequestSize = 30*1024^2)
options(future.globals.maxSize = Inf)

# Print basic info
message("Cores available: ", parallel::detectCores())
message("Workers: ", future::nbrOfWorkers())

# Set options
options(shiny.maxRequestSize = 100 * 1024^2)
options(auth0_disable = FALSE)

# Define custom functions
rounddf <- function(x, digits = rep(2, ncol(x)), func = round, pad = FALSE) {
  DT <- FALSE
  if (class(x)[1] == "data.table") {
    x <- as.data.frame(x)
    if (requireNamespace("data.table", quietly = TRUE)) {
      DT <- TRUE
    }
  }

  if (length(digits) == 1) {
    digits <- rep(digits, ncol(x))
  } else if (length(digits) != ncol(x)) {
    digits <- c(digits, rep(digits[1], ncol(x) - length(digits)))
    warning("First value in digits repeated to match length.")
  }

  for (i in 1:ncol(x)) {
    if (class(x[, i, drop = TRUE])[1] == "numeric") {
      x[, i] <- func(x[, i], digits[i])
      if (pad && all(grepl("\\.", x[, i]))) {
        ff <- max(nchar(gsub(".+\\.", "", x[, i])))
        fmt <- paste0("%.0", ff, "f")
        x[, i] <- sprintf(fmt, x[, i])
      }
    }
  }

  if (DT) {
    x <- data.table::data.table(x)
  }

  return(x)
}

# Define color palettes
color_typ <- c("#DD4444", "orange", "#66BBBB", "#555599", "#C51B7D", "#91CF60")
focal_plate_color <- c("#66BBBB", "#555599")
color_groups <- c("#DD4444", "orange", "#66BBBB", "#555599")
color_times <- c("#DD4444", "orange", "#66BBBB", "#555599")
color_features <- c(
  "#E04C5C", "#7DAF4C", "#23AECE", "#FB894B", "#E7DA36", "#187A51",
  "#5EA4A2", "#3D3C4E", "#4D1836", "#C51B7D",
  "#E9A3C9", "#B35806", "#F1A340", "#FEE08B", "#D9EF8B",
  "#91CF60", "#C7EAE5", "#5AB4AC", "#01665E", "#E7D4E8",
  "#AF8DC3", "#762A83", "#FC0FC0", "#F9C7DE", "#f3a0c4"
)

names(color_features) <- c(
  "IgA2", "IgA1", "FcaR", "FcgR2A131", "FcgR3A158", "IgG",
  "ELISA_IgG", "IgG4", "IgG2", "IgA",
  "IgG3", "IgM", "FcgR1A", "MN", "FcgR2b",
  "IgG1", "HAI", "NAI", "FcgR3b", "ADCC",
  "ADCP", "ADCD"
)

# Define database connection function
get_db_connection <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("db"),
            host = Sys.getenv("db_host"),
            port = Sys.getenv("db_port"),
            user = Sys.getenv("db_userid_x"),
            password = Sys.getenv("db_pwd_x"),
            sslmode = 'allow',
            options = "-c search_path=madi_results"
  )
}

# Returns a plain list of connection parameters (no live connection object)
get_db_connection_args <- function() {
  list(
    host   = Sys.getenv("db_host"),
    port   = as.integer(Sys.getenv("db_port", "5432")),
    dbname = Sys.getenv("db"),
    user   = Sys.getenv("db_userid_x"),
    pass   = Sys.getenv("db_pwd_x")
  )
}

# Called inside the future to open a fresh connection
get_db_connection_from_args <- function(host, port, dbname, user, pass) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    port     = port,
    dbname   = dbname,
    user     = user,
    password = pass,
    sslmode = 'allow',
    options = "-c search_path=madi_results"
  )

}
# Function to get project name
getProjectName <- function(conn, current_user) {
  query <- glue::glue("SELECT project_name, workspace_id FROM madi_results.xmap_users pu WHERE pu.auth0_user = {dbQuoteLiteral(conn, current_user)}")
  result <- dbGetQuery(conn, query)
  if (nrow(result) > 0) {
    name <- result[1, "project_name"]
    id <- result[1, "workspace_id"]
  } else {
    name <- "unknown"
    id <- -1
  }
  return(list(name = name, id = id))
}

reloadReactive <- function(conn, userWorkSpaceID) {
  select_query <- "
    SELECT
      xmap_header.study_accession,
      xmap_header.experiment_accession,
      xmap_header.study_accession AS study_name,
      xmap_header.experiment_accession AS experiment_name,
      xmap_header.workspace_id,
      xmap_users.project_name
    FROM madi_results.xmap_header
    JOIN madi_results.xmap_users ON xmap_header.workspace_id = xmap_users.workspace_id
    WHERE xmap_header.workspace_id = $1;"

  query_result <- dbGetQuery(conn, select_query, params = list(userWorkSpaceID))
  query_result
}


