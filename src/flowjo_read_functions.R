

# ============================================================
# FUNCTION: parse_sample_id
# ============================================================
parse_sample_id <- function(df,
                            col = "Sample_ID",
                            delimiters = c("-", "_"),
                            new_cols = c("patientid", "timepoint", "plate_number_p")) {

  delim_pattern <- paste(delimiters, collapse = "|")
  split_list    <- strsplit(as.character(df[[col]]), delim_pattern)
  split_lengths <- sapply(split_list, length)

  # Diagnostic check
  if (any(split_lengths != length(new_cols))) {
    message("WARNING: The following Sample_IDs did not split into ",
            length(new_cols), " parts:")
    print(df[[col]][split_lengths != length(new_cols)])
    message("Split lengths found: ",
            paste(unique(split_lengths), collapse = ", "))
  }

  # Pad or truncate each split to expected number of columns
  split_list <- lapply(split_list, function(x) {
    length(x) <- length(new_cols)
    x
  })

  split_cols           <- as.data.frame(do.call(rbind, split_list),
                                        stringsAsFactors = FALSE)
  colnames(split_cols) <- new_cols

  # Classify stype
  classify_stype <- function(pid) {
    pid <- trimws(as.character(pid))
    if (is.na(pid))                                        return(NA)
    if (grepl("^\\d+$",         pid))                     return("X")
    if (grepl("^STD",           pid, ignore.case = TRUE)) return("S")
    if (grepl("^QC",            pid, ignore.case = TRUE)) return("C")
    if (grepl("^(Blank|empty)", pid, ignore.case = TRUE)) return("B")
    return(NA)
  }

  split_cols$stype <- sapply(split_cols[[new_cols[1]]], classify_stype)

  # For S, C, B: move parsed timepoint -> plate_number_p, set timepoint to NA
  non_experimental <- split_cols$stype %in% c("S", "C", "B")
  split_cols[[new_cols[3]]][non_experimental] <- split_cols[[new_cols[2]]][non_experimental]
  split_cols[[new_cols[2]]][non_experimental] <- NA

  cbind(df, split_cols)
}


# ============================================================
# FUNCTION: load_flowjo_file
# Reads a single Excel file and returns:
#   $flowjo_df  - wide format with all metadata
#   $dilutions  - dilutions reference table
# ============================================================
load_flowjo_file <- function(filepath) {

  # Capture file metadata
  file_meta <- tibble(
    source_file = basename(filepath),
    source_path = normalizePath(filepath),
    load_date   = Sys.Date()
  )

  # ---- Read Sheet1 (flow data) ------------------------------
  flowjo_raw <- read_excel(filepath, sheet = "Sheet1") %>%
    separate(Sample, c("well", "Sample_ID"), " ") %>%
    dplyr::rename(obs_number = Well,
              plate_number = Plate) %>%
    mutate(plate = paste("plate", plate_number, sep = "_"))

  # ---- Parse Sample_ID --------------------------------------
  flowjo_df <- parse_sample_id(
    flowjo_raw,
    col        = "Sample_ID",
    delimiters = c("-", "_"),
    new_cols   = c("patientid", "timepoint", "plate_number_p")
  )

  # ---- Add file metadata ------------------------------------
  flowjo_df <- flowjo_df %>%
    mutate(
      source_file = file_meta$source_file,
      source_path = file_meta$source_path,
      load_date   = file_meta$load_date
    )

  # ---- Read dilutions tab -----------------------------------
  dilutions <- read_excel(filepath, sheet = "dilutions") %>%
    mutate(source_file = file_meta$source_file)

  # ---- Validate dilutions columns ---------------------------
  required_cols <- c("stype", "patientid", "antigen", "antibody",
                     "source", "dilution_factor")
  missing_cols  <- setdiff(required_cols, names(dilutions))

  if (length(missing_cols) > 0) {
    stop("Dilutions tab is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  list(
    flowjo_df = flowjo_df,
    dilutions = dilutions
  )
}


# ============================================================
# FUNCTION: pivot_flowjo_long
# Pivots antibody columns to long format and joins dilutions
# ============================================================
pivot_flowjo_long <- function(flowjo_df, dilutions) {

  # Identify antibody columns from dilutions tab
  antibody_cols <- unique(dilutions$antibody)

  # Validate antibody columns exist in flowjo_df
  missing_ab <- setdiff(antibody_cols, names(flowjo_df))
  if (length(missing_ab) > 0) {
    warning("The following antibody columns from dilutions tab were not found ",
            "in flowjo_df and will be skipped: ",
            paste(missing_ab, collapse = ", "))
    antibody_cols <- intersect(antibody_cols, names(flowjo_df))
  }

  # ---- Pivot longer -----------------------------------------
  flowjo_long <- flowjo_df %>%
    pivot_longer(
      cols      = all_of(antibody_cols),
      names_to  = "antibody",
      values_to = "MFI"
    ) %>%
    # Join antigen from dilutions - one antigen per antibody
    left_join(
      dilutions %>%
        select(antibody, antigen) %>%
        distinct(),
      by = "antibody"
    )

  # ---- Build lookup tables ----------------------------------

  # X lookup: antigen + antibody only — no patientid
  x_dilutions <- dilutions %>%
    filter(stype == "X") %>%
    select(antigen, antibody, source, dilution_factor) %>%
    distinct()

  # S, B, C lookup: patientid + antigen + antibody
  sbc_dilutions <- dilutions %>%
    filter(stype %in% c("S", "B", "C")) %>%
    select(stype, patientid, antigen, antibody, source, dilution_factor) %>%
    distinct()

  # ---- Join dilutions ---------------------------------------
  flowjo_long <- flowjo_long %>%
    # Join X dilutions for all rows first
    left_join(
      x_dilutions,
      by     = c("antigen", "antibody"),
      suffix = c("", "_x")
    ) %>%
    # Join S/B/C dilutions
    left_join(
      sbc_dilutions,
      by     = c("stype", "patientid", "antigen", "antibody"),
      suffix = c("", "_sbc")
    ) %>%
    # Apply correct dilution based on stype
    mutate(
      source = case_when(
        stype == "X"               ~ source,
        stype %in% c("S", "B", "C") ~ source_sbc,
        TRUE                        ~ NA_character_
      ),
      dilution_factor = case_when(
        stype == "X"               ~ dilution_factor,
        stype %in% c("S", "B", "C") ~ dilution_factor_sbc,
        TRUE                        ~ NA_real_
      )
    ) %>%
    # Drop redundant columns from joins
    select(-ends_with("_x"), -ends_with("_sbc"))

  # ---- Diagnostic: report any unmatched dilutions -----------
  unmatched <- flowjo_long %>%
    filter(is.na(dilution_factor)) %>%
    distinct(stype, patientid, antigen, antibody)

  if (nrow(unmatched) > 0) {
    message("WARNING: ", nrow(unmatched),
            " stype/patientid/antigen/antibody combinations ",
            "had no matching dilution factor:")
    print(unmatched)
  }

  flowjo_long
}


