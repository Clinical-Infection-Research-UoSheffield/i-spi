

# Function to create download handler for each button
createDownloadHandler <- function(data_component, file_suffix) {
  downloadHandler(
    filename = function() {
      paste0(input$readxMap_study_accession, " ", input$readxMap_experiment_accession, " ", file_suffix, ".csv")
    },
    content = function(file) {
      # store_plates_data is the reactiveValue which stores all stored_plates
      write.csv(stored_plates_data[[data_component]], file)
    }
  )
}

observeEvent(input$stored_header_rows_selected, {
  print("in the observeEvent for header_rows_selected")
  selected_studyexpplate$study_accession <- input$readxMap_study_accession
  print(paste("selected study:", selected_studyexpplate$study_accession))
  selected_studyexpplate$experiment_accession <- input$readxMap_experiment_accession
  print(paste("selected experiment:", selected_studyexpplate$experiment_accession))
  selected_studyexpplate$plateid <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("plateid")]
  print(paste("selected plateid:", selected_studyexpplate$plateid))
  selected_studyexpplate$plate <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("plate")]
  print(paste("selected plate:", selected_studyexpplate$plate))
  selected_studyexpplate$nominal_sample_dilution <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("nominal_sample_dilution")]
  print(paste("selected nominal sample dilution", selected_studyexpplate$nominal_sample_dilution))
  selected_studyexpplate$wavelengths <- stored_plates_data$stored_header[input$stored_header_rows_selected, c("wavelengths")]
  print(paste("selected wavelengths", selected_studyexpplate$wavelengths))
  # header_row_selected <- stored_plates_data$stored_header[input$stored_header_rows_selected,]
  # print(header_row_selected)

  plateid <- stored_plates_data$stored_header[
    input$stored_header_rows_selected, "plateid"
  ]
  
  plate_id <- stored_plates_data$stored_header[
    input$stored_header_rows_selected, "plate_id"
  ]

  # --- Check if already split ---
  already_split_sql <- glue::glue("
    SELECT EXISTS (
      SELECT 1
      FROM madi_results.xmap_sample
      WHERE study_accession = '{input$readxMap_study_accession}'
        AND experiment_accession = '{input$readxMap_experiment_accession}'
        AND plateid = '{plateid}'
        AND nominal_sample_dilution IS NOT NULL
        AND nominal_sample_dilution NOT LIKE '%|%'
    ) AS already_split;
  ")
  already_split <- DBI::dbGetQuery(conn, already_split_sql)$already_split


  selected_nominal_dilutions <- strsplit(selected_studyexpplate$nominal_sample_dilution, "\\|")[[1]]
  if (!already_split && length(selected_nominal_dilutions) > 1) {
       split_by_nominal_dilution(TRUE)
  } else {
    split_by_nominal_dilution(FALSE)
  }
  
  ## wavelength check
  wl_parts <- strsplit(selected_studyexpplate$wavelengths, "\\|")[[1]]
  if (length(wl_parts) == 2) {
    delta_experiment <- paste0(input$readxMap_experiment_accession, "|D")
    
    already_subtracted_sql <- glue::glue("
      SELECT EXISTS (
        SELECT 1
        FROM madi_results.xmap_header
        WHERE study_accession     = '{input$readxMap_study_accession}'
          AND experiment_accession = '{delta_experiment}'
          AND plate_id              = '{plate_id}'
      ) AS already_subtracted;
    ")
    already_subtracted <- DBI::dbGetQuery(conn, already_subtracted_sql)$already_subtracted
    
    if (!already_subtracted) {
      show_wavelength_subtraction(TRUE)
    } else {
      show_wavelength_subtraction(FALSE)
    }
  } else {
    show_wavelength_subtraction(FALSE)
  }
  


  # output$selected_plate_text = renderText({
  #   paste0("Selected Plate: ", selected_studyexpplate$plateid)
  # })

  ## identify if the standard curve data is present and store
  # check_standard <- stored_plates_data$stored_standard[ ,"plateid"]
  # selected_studyexpplate$nrows_standard <- length(check_standard[check_standard == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_standard:", selected_studyexpplate$nrows_standard))
  #
  # ## identify if the 4 parameter standard curve is calculated and store
  # check_fits <- stored_plates_data$stored_fits[ ,"plateid"]
  # selected_studyexpplate$nrows_fits <- length(check_fits[check_fits == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_fits:", selected_studyexpplate$nrows_fits))
  #
  # ## identify if the buffer data is available and store
  # check_buffer <- stored_plates_data$stored_buffer[ ,"plateid"]
  # selected_studyexpplate$nrows_buffer <- length(check_buffer[check_buffer == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_buffer:", selected_studyexpplate$nrows_buffer))
  #
  # ## identify if the control data is available and store
  # check_control <- stored_plates_data$stored_control[ , c("plateid")]
  # selected_studyexpplate$nrows_control <- length(check_control[check_control == selected_studyexpplate$plateid])
  # print(paste(selected_studyexpplate$plateid," nrows_control:", selected_studyexpplate$nrows_control))
})

observeEvent(input$readxMap_study_accession, {

  if (input$readxMap_study_accession != "Click here") {
  initial_source <- obtain_initial_source(input$readxMap_study_accession)
  # std <<- stored_plates_data$stored_standard
  # initial_source <<- unique(stored_plates_data$stored_standard$source)[1]

  # Initialize study parameters for a user and study
  study_user_params_nrow <- nrow(fetch_study_configuration(study_accession = input$readxMap_study_accession
                                                           , user = currentuser()))
  if (study_user_params_nrow == 0) {
    intitialize_study_configurations(study_accession = input$readxMap_study_accession,
                                     user = currentuser(), initial_source = initial_source)

  }
  }
})

observeEvent(list(input$readxMap_experiment_accession, refresh_data_trigger()), {
  req(input$readxMap_experiment_accession != "Click here")

  
  # Validate experiment belongs to current study before doing anything
  current_study <- input$readxMap_study_accession
  current_exp   <- input$readxMap_experiment_accession
  
  if (is.null(current_study) || current_study == "Click here") return()
  if (is.null(current_exp)   || current_exp   == "Click here") return()
  
  valid_exps <- reactive_df_study_exp()
  if (!is.null(valid_exps)) {
    study_exps <- valid_exps[valid_exps$study_accession == current_study, "experiment_accession"]
    if (!current_exp %in% study_exps) {
      cat("⚠ Stale experiment selection:", current_exp, "not in study:", current_study, "- skipping\n")
      return()
    }
  }
  
  removeTab(inputId = "body_panel_id", target="previewxMap")
  removeTab(inputId = "body_panel_id", target="headerxMap")
  removeTab(inputId = "inLoadedData", target="Data")
  removeTab(inputId = "inLoadedData", target="Bead Count")
  removeTab(inputId = "inLoadedData", target="Standard Curve")
  removeTab(inputId = "inLoadedData", target="Dilution Analysis")
  removeTab(inputId = "inLoadedData", target="Plate Normalization")
  removeTab(inputId = "inLoadedData", target="Outliers")
  removeTab(inputId = "inLoadedData", target="Subgroup Detection")

  ## New DA Version
  average_au_table_reactive(NULL)
  final_average_au_table_rv(NULL)
  updated_classified_merged_rv(NULL)
  updated_margin_antigen_rv(NULL)
  # da_filters_rv(selected = list(
  #   n_pass_dilutions = NULL,
  #   status = NULL,
  #   timeperiod = NULL
  # ))
  da_filters_rv$selected <- list(
    n_pass_dilutions = NULL,
    status = NULL,
    timeperiod = NULL
  )
 # stored_plates_data(NULL)
  # storedlong_plates_data(NULL)
  # selected_studyexpplate(NULL)
  # p_data(NULL)
  # pa_data(NULL)
  # processing_status(FALSE)
  # job_status(NULL)
  #
  # n_plates_standard_curve(NULL)
  # mininum_dilution_count_boolean(NULL)


  num_plates_stored_std_curve <- count_n_std_curve_plates(input$readxMap_study_accession, input$readxMap_experiment_accession)
  n_plates_standard_curve(num_plates_stored_std_curve)


  n_dilutions_antigen_df <- count_n_dilutions_per_antigen(input$readxMap_study_accession, input$readxMap_experiment_accession)
  # check if all antigens have at minimum 5 points
  dilutions_per_antigen_boolean <- all(n_dilutions_antigen_df$num_dilutions >= 5)
  mininum_dilution_count_boolean(dilutions_per_antigen_boolean)
  # if (num_plates_stored_std_curve >= 2) {
  #   insertTab(inputId = "inLoadedData", target = "Standard Curve Summary")
  # }else {
  #   removeTab(inputId = "inLoadedData", target = "Standard Curve Summary")
  # }

  # req(input$readxMap_experiment_accession)



  print("in the observeEvent for readxMap_experiment_accession")
  selected_studyexpplate$study_accession <- input$readxMap_study_accession
  print(paste("selected study:", selected_studyexpplate$study_accession))
  selected_studyexpplate$experiment_accession <- input$readxMap_experiment_accession
  print(paste("selected experiment:", selected_studyexpplate$experiment_accession))
  print(length( selected_studyexpplate$experiment_accession))
  print(selected_studyexpplate$experiment_accession == "")

  req(selected_studyexpplate$experiment_accession,selected_studyexpplate$study_accession)
  
  display_exclude_cols <- c("plate_id", "xmap_standard_id", "antibody_name", 
                            "xmap_control_id", "xmap_buffer_id", "xmap_sample_id")
  

  m <- 0
  selected_studyexpplate$plateid <- NA
  print(input$readxMap_experiment_accession)
  if (input$readxMap_experiment_accession != "Click here") {
    show_modal_progress_circle(
      value = 0,
      text = "Loading header data",
      color = "#112446",
      stroke_width = 4,
      easing = "easeOut",
      duration = 1000,
      trail_color = "#eee",
      trail_width = 1,
      height = "200px",
      session = shiny::getDefaultReactiveDomain()
    )
    stored_header <- update_db(operation = "select",
                               schema = "madi_results",
                               table_name = "xmap_header",
                               select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )
    
    # columns to display 
    stored_header_display_cols <- c("project_id", "study_accession", "experiment_accession", 
                                    "plate", "nominal_sample_dilution", "plateid", "wavelengths",
                                    "acquisition_date", "reader_serial_number", "rp1_pmt_volts", 
                                    "rp1_target", "auth0_user", "plate_id", "Curve_Status", "curve_plate")
    

    # stored_header$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", stored_header$plate_id, fixed=TRUE)))
    # stored_header <- stored_header[ , c("experiment_accession", "plateid", "plate", "nominal_sample_dilution", "wavelengths", "acquisition_date", "reader_serial_number",
    #                                     "rp1_pmt_volts", "rp1_target", "auth0_user", "project_id", "study_accession", "plate_id")]
    stored_header$Curve_Status <- NA



    # print("### create the xMap standard_data object")
    stored_standard <- data.frame()
    stored_standard <- update_db(operation = "select",
                                 schema = "madi_results",
                                 table_name = "xmap_standard",
                                 select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )
    #stored_standard <- inner_join(stored_standard, stored_header[ , c("plate_id","plateid","plate")], by = "plate_id")

    if (length(stored_standard[ ,"experiment_accession"]) > 0) {
      # print(paste("stored_standard data:", length(stored_standard[ ,"experiment_accession"])))

      # stored_standard$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", stored_standard$plate_id, fixed=TRUE)))
      names(stored_standard)[names(stored_standard) == "antibody_n"] <- "n"
      names(stored_standard)[names(stored_standard) == "antibody_mfi"] <- "mfi"
      #stored_standard <- distinct(stored_standard[,!(names(stored_standard) %in% c("xmap_standard_id", "antibody_name", "plate_id"))])
      stored_standard <- distinct(stored_standard)
      
      # selected_studyexpplate$stored_standard <- stored_standard
      # storedlong_plates_data$stored_standard <- stored_standard
      stored_plates_data$stored_standard <- stored_standard

     # wide_standard <- pivot_wider(stored_standard, id_cols = c("plateid", "sampleid", "source", "feature", "dilution", "pctaggbeads", "samplingerrors", "study_accession", "experiment_accession"), names_from = antigen, values_from = c(mfi, n))
      # output$swide_standard = DT::renderDataTable(wide_standard, options = list(scrollX = TRUE))
      # stored_plates_data$stored_standardw = wide_standard
      
     # output$swide_standard = DT::renderDataTable(stored_standard, options = list(scrollX = TRUE))
      output$swide_standard <- DT::renderDataTable(
        stored_standard[, !(names(stored_standard) %in% display_exclude_cols)],
        options = list(scrollX = TRUE)
      )
      #stored_plates_data$stored_standardw = stored_standard



      cat("before update db stored fits\n")
      stored_fits <- update_db(operation = "select",
                               schema = "madi_results",
                               table_name = "best_glance_all",
                               select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
      )

      cat("stored fits:\n")
      print(stored_fits)



      if (is.null(stored_fits) || nrow(stored_fits) == 0) {
        nrows_fits <- 0
      } else {
        nrows_fits <- nrow(stored_fits)
        stored_fits$curve_plate <- 1
        stored_plates_data$stored_fits <- stored_fits
      }

      if (nrows_fits > 0) {
        # print("stored_fits data ")
        #standard_source <- unique(stored_fits$source)
       # standard_plates <- data.frame(plateid = unique(stored_fits[ , c("plateid")]),curve_plate = 1)
        # print(standard_plates)

        stored_header <- merge(stored_header, distinct(stored_fits[ , c("project_id", "study_accession", "experiment_accession", "plate", "nominal_sample_dilution","curve_plate")]), by = c("project_id", "study_accession", "experiment_accession", "plate", "nominal_sample_dilution"), all.x = TRUE)
       # stored_header <- merge(stored_header,standard_plates, by = "plateid", all.x = TRUE)
        # print(stored_header)
        stored_header$Curve_Status <- ifelse(stored_header$curve_plate==1,1,2)
        # Notify the browser that there are not standard curves to calculate
        # session$sendCustomMessage("disableButton", "btn_calc_select_standard_curves")
      } else {
        # print("no stored_fits data ")
        stored_header$Curve_Status <- 2
        # Notify the browser that there are standard curves to calculate
        # session$sendCustomMessage("enableButton", "btn_calc_select_standard_curves")
      }

      m <- m+1
      update_modal_progress(
        value = m / 5,
        text = "Loading Standard Curve data",
        session = shiny::getDefaultReactiveDomain()
      )


      if (nrows_fits > 0) {
        # print("stored_fit_tab data ")
        # m <- m+1
        # update_modal_progress(
        #   value = m / 8,
        #   text = "Loading Standard Curve parameters with std.errors",
        #   session = shiny::getDefaultReactiveDomain()
        # )
        # stored_fit_tab<- update_db(operation = "select",
        #                            schema = "madi_results",
        #                            table_name = "xmap_standard_fit_tab",
        #                            select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))        )
        # stored_fit_tab <- distinct(stored_fit_tab, study_accession, experiment_accession, antigen, plateid, term, source, .keep_all = TRUE)
        # # storedlong_plates_data$stored_fit_tab<- stored_fit_tab
        # stored_plates_data$stored_fit_tab<- stored_fit_tab

        # print("stored_preds data ")
        # m <- m+1
        # update_modal_progress(
        #   value = m / 8,
        #   text = "Calculating Standard Curve predictions",
        #   session = shiny::getDefaultReactiveDomain()
        # )
        # newdils <- data.frame(log_dilution = seq(from = -10, to = 0, by = 0.05))
        # stored_preds <- logist.predict(stored_fits,newdils)
        # # storedlong_plates_data$stored_preds<- stored_preds
        # stored_plates_data$stored_preds<- stored_preds

        # print("stored_fitstor data ")
        # m <- m+1
        # update_modal_progress(
        #   value = m / 8,
        #   text = "Loading Standard Curve corrected data",
        #   session = shiny::getDefaultReactiveDomain()
        # )
      #   stored_fitstor <- data.frame()
      #   fit_stor <- update_db(operation = "select",
      #                         schema = "madi_results",
      #                         table_name = "xmap_standard_stor",
      #                         select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))        )
      #   stored_fitstor <- distinct(fit_stor, study_accession, experiment_accession, antigen, plateid, mfi, log_dilution, source, .keep_all = TRUE)
      #   # storedlong_plates_data$stored_fitstor <- stored_fitstor
      #   stored_plates_data$stored_fitstor <- stored_fitstor
      # } else {
      #   # print("stored_fitstor data ")
      #   fit_stor <- update_db(operation = "select",
      #                         schema = "madi_results",
      #                         table_name = "xmap_standard",
      #                         select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
      #   )
      #   names(fit_stor)[names(fit_stor) == "antibody_mfi"] <- "MFI"
      #   names(fit_stor)[names(fit_stor) == "antibody_n"] <- "nbeads"
      #   fit_stor$dilution <- 1 / fit_stor$dilution
      #   fit_stor$log_dilution <- log10(fit_stor$dilution + 0.00001)
      #   fit_stor$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", fit_stor$plate_id, fixed=TRUE)))
      #   stored_fitstor <- distinct(fit_stor, study_accession, experiment_accession, antigen, plateid, MFI, log_dilution, source, .keep_all = TRUE)
      #   # storedlong_plates_data$stored_fitstor <- stored_fitstor
      #   stored_plates_data$stored_fitstor <- stored_fitstor
      }



    } else {
      # print("no stored_standard data ")
      stored_header$Curve_Status <- 3
      # Notify the browser that there are not standard curves to calculate
      session$sendCustomMessage("disableButton", "btn_calc_select_standard_curves")
    }

    stored_header$Curve_Status <- factor(stored_header$Curve_Status)
    print(paste(stored_header$plateid, stored_header$Curve_Status))

    # print("creating output stored_header ")
    # stored_header_react <- reactive(stored_header)
    stored_header_dt <- datatable(stored_header[, names(stored_header) %in% stored_header_display_cols],
                                  options = list(scrollX = TRUE),
                                  selection = 'single',
                                  escape = FALSE
    ) %>%
      formatStyle(
        'Curve_Status',
        target = 'row',
        backgroundColor = styleEqual(c(1, 2, 3),
                                     c("#D9EF8B", "#F9C7DE", "#C7EAE5"),
                                     default = NULL)
      )
   # output$stored_header = DT::renderDataTable(stored_header_dt, server = FALSE)
    output$stored_header <- DT::renderDataTable({
      req(input$dataCollapse == "Plates")  # Only run if "Header" panel is open
      stored_header_dt
    }, server = TRUE)


    # print("Updating the stored_plates_data reactive for downloading")
    stored_plates_data$stored_header <- stored_header
    # selected_studyexpplate$stored_header <- stored_header


    ### create the xMap control_data object
    m <- m+1
    update_modal_progress(
      value = m / 5,
      text = "Loading Control data",
      session = shiny::getDefaultReactiveDomain()
    )
    stored_control <- update_db(operation = "select",
                                schema = "madi_results",
                                table_name = "xmap_control",
                                select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )

    nrows_control <- nrow(stored_control)
    if (nrows_control > 0) {
     # stored_control <- inner_join(stored_control, stored_header[ , c("plate_id","plateid","plate")], by = "plate_id")
      # stored_control$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", stored_control$plate_id, fixed=TRUE)))

      names(stored_control)[names(stored_control) == "antibody_n"] <- "n"
      names(stored_control)[names(stored_control) == "antibody_mfi"] <- "mfi"
      #stored_control <- distinct(stored_control[,!(names(stored_control) %in% c("xmap_control_id", "antibody_name","plate_id"))])
      stored_control <- distinct(stored_control)
      
      stored_plates_data$stored_control <- stored_control
      # selected_studyexpplate$stored_control
      # storedlong_plates_data$stored_control = stored_control
      # wide_control <- pivot_wider(stored_control, names_from = antigen, values_from = c(mfi, n))
      # output$swide_control = DT::renderDataTable(wide_control, options = list(scrollX = TRUE))
      # stored_plates_data$stored_controlw <- wide_control
      #output$swide_control = DT::renderDataTable(stored_control, options = list(scrollX = TRUE))
      output$swide_control = DT::renderDataTable(
        stored_control[, !(names(stored_control) %in% display_exclude_cols)],
        options = list(scrollX = TRUE)
      )
      #stored_plates_data$stored_controlw <- stored_control

    }

    ### create the xMap buffer_data object
    m <- m+1
    update_modal_progress(
      value = m / 5,
      text = "Loading Buffer data",
      session = shiny::getDefaultReactiveDomain()
    )
    stored_buffer <- update_db(operation = "select",
                               schema = "madi_results",
                               table_name = "xmap_buffer",
                               select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )
    nrows_buffer <- nrow(stored_buffer)
    if (nrows_buffer > 0) {
    #  stored_buffer <- inner_join(stored_buffer, stored_header[ , c("plate_id","plateid","plate")], by = "plate_id")
      # stored_buffer$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", stored_buffer$plate_id, fixed=TRUE)))
      names(stored_buffer)[names(stored_buffer) == "antibody_n"] <- "n"
      names(stored_buffer)[names(stored_buffer) == "antibody_mfi"] <- "mfi"
      #stored_buffer <- distinct(stored_buffer[,!(names(stored_buffer) %in% c("xmap_buffer_id", "antibody_name","plate_id"))])
      stored_buffer <- distinct(stored_buffer)
      # print(paste("nrow previous stored buffer", nrow(stored_buffer)))
      stored_plates_data$stored_buffer <- stored_buffer
      # selected_studyexpplate$stored_buffer
      # storedlong_plates_data$stored_buffer = stored_buffer
      # wide_buffer <- pivot_wider(stored_buffer, id_cols = c("study_accession", "experiment_accession", "plateid", "well", "dilution", "pctaggbeads", "samplingerrors", "feature"), names_from = antigen, values_from = c(mfi, n))
      # output$swide_buffer = DT::renderDataTable(wide_buffer, options = list(scrollX = TRUE))
      # # Updating the stored_plates_data reactive for downloading
      # stored_plates_data$stored_bufferw <- wide_buffer
      #output$swide_buffer = DT::renderDataTable(stored_buffer, options = list(scrollX = TRUE))
      output$swide_buffer = DT::renderDataTable(
        stored_buffer[, !(names(stored_buffer) %in% display_exclude_cols)],
        options = list(scrollX = TRUE)
      )
      
      # Updating the stored_plates_data reactive for downloading
      #stored_plates_data$stored_bufferw <- stored_buffer
    }

    ### create the xMap sample_data object

    m <- m+1
    update_modal_progress(
      value = m / 5,
      text = "Loading Sample data",
      session = shiny::getDefaultReactiveDomain()
    )
    stored_sampley <- update_db(operation = "select",
                                schema = "madi_results",
                                table_name = "xmap_sample",
                                select_where = list("concat(study_accession,experiment_accession)" = paste0(input$readxMap_study_accession,input$readxMap_experiment_accession))
    )

    stored_sampley <- stored_sampley[, !(names(stored_sampley) %in% c("gate_class_dil", "reference_dilution", "gate_class", "antibody_au", "antibody_au_se", "id_imi",
                                                                      "norm_mfi", "in_linear_region", "gate_class_loq", "in_quantifiable_range", "gate_class_linear_region",
                                                                      "quality_score","predicted_concentration", "sample_dilution_factor"))]
    nrows_sample <- nrow(stored_sampley)
    if (nrows_sample > 0) {
      names(stored_sampley)[names(stored_sampley) == "antibody_n"] <- "n"
      names(stored_sampley)[names(stored_sampley) == "antibody_mfi"] <- "mfi"
      stored_samplex <- distinct(stored_sampley, study_accession, 
                                 experiment_accession, plate_id, antigen, wavelength,
                                 well, 
                                 .keep_all = TRUE)
      begin_cols <-  c("experiment_accession", "plate", "nominal_sample_dilution")
      stored_samplex <- stored_samplex[, c(
        begin_cols,
        setdiff(names(stored_samplex), begin_cols)
      )]

      stored_samplex_names <- names(stored_samplex)

      i <- match("plate_id", stored_samplex_names)
      j <- match("plateid", stored_samplex_names)

      stored_samplex_names[c(i, j)] <- stored_samplex_names[c(j, i)]

      stored_samplex <- stored_samplex[, stored_samplex_names]


      stored_plates_data$stored_sample <- stored_samplex

      # storedlong_plates_data$stored_sample = stored_samplex
      # wide_sample <- pivot_wider(stored_samplex[ , c("study_accession", "experiment_accession", "plateid", "timeperiod", "patientid", "well",
      #                                                "stype", "sampleid", "agroup","dilution", "pctaggbeads", "samplingerrors", "antigen", "mfi",
      #                                                "n", "feature", "gate_class_detection", "au", "au_se", "gate_class_linear_region", "gate_class_loq", "quality_score")],
      #                            names_from = antigen, values_from = c(mfi, n, gate_class_detection, au, au_se, gate_class_linear_region, gate_class_loq, quality_score))
      # output$swide_sample <- DT::renderDataTable(wide_sample, options = list(scrollX = TRUE))
      # stored_plates_data$stored_samplew <- wide_sample

      output$swide_sample <- DT::renderDataTable(stored_samplex, options = list(scrollX = TRUE))
      
      #stored_plates_data$stored_samplew <- stored_samplex
    }

    # Ssamples with QC metrics - was fetch_best_sample_se_all but now add Bayes results
    qc_best_samples_se <- fetch_best_sampl_se_with_bayes(study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession,
                                                   project_id = userWorkSpaceID(),
                                                   conn = conn)
    # stored_best_sample_se <- qc_best_samples_se
    stored_plates_data$stored_best_sample_se <- qc_best_samples_se


    output$stored_best_sample_se = DT::renderDataTable(qc_best_samples_se, options = list(scrollX = TRUE))


  }
  # Update the experiment in antigen family
  #message("before create antigen family rows")
  # fetch_antigen_family_table(study_accession = input$readxMap_study_accession, 
  #                            project_id = userWorkSpaceID(), experiment_accession = input$readxMap_experiment_accession,
  #                            default_family = "All Antigens")
  # 
  # create_antigen_family_rows(input$readxMap_study_accession, input$readxMap_experiment_accession,userWorkSpaceID())

  remove_modal_progress(session = getDefaultReactiveDomain())

  updateTabsetPanel(session, inputId = "inLoadedData", selected = "Data")
})

observe({
  m <- 0
  plate_selected <- !is.null(selected_studyexpplate$plateid) && grepl('[A-Za-z]', selected_studyexpplate$plateid)
  selected_plate_logic <- paste0("Selected Plate: ", selected_studyexpplate$plateid)
  selected_plate_text = renderText({
    selected_plate_logic
  })

  output$download_stored_header <- createDownloadHandler("stored_header", "stored headers")
  output$download_stored_standard <- createDownloadHandler("stored_standard", "stored standards")
  output$download_stored_control <- createDownloadHandler("stored_control", "stored controls")
  output$download_stored_buffer <- createDownloadHandler("stored_buffer", "stored buffers")
  output$download_stored_sample <- createDownloadHandler("stored_sample", "stored samples")
  output$download_stored_best_sample_se <- createDownloadHandler("stored_best_sample_se","stored best samples")


  output$stored_plates_ui <- renderUI({
    req(input$readxMap_experiment_accession)
    req(stored_plates_data)

    m <<- m + 1
    print(paste("plate_selected", plate_selected))
    update_modal_progress(
      value = m / 5,
      text = "Rendering wide tables",
      session = shiny::getDefaultReactiveDomain()
    )
    plate_data_valid <- validate_plate_data(stored_plates_data)

    # Custom CSS for green panels
    tagList(
      # Custom CSS for green panels
      tags$head(
        tags$style(HTML("
          .panel-primary {
            border-color: #28a745;
          }
          .panel-primary > .panel-heading {
            background-color: #28a745;
            border-color: #28a745;
            color: white;
          }
        "))
      ),

    tabsetPanel(id = "inLoadedData",
                tabPanel(
                  title = "Data",
                  br(),
                  bsCollapse(
                    id = "dataCollapse",
                    multiple = FALSE,
                    bsCollapsePanel(
                      title = "Plates",
                      DT::dataTableOutput("stored_header"),
                      downloadButton("download_stored_header"),
                      uiOutput("header_actions"),
                      style = "primary"
                    ),
                    bsCollapsePanel(
                      title = "Standards",
                      DT::dataTableOutput("swide_standard"),
                      downloadButton("download_stored_standard"),
                      style = "primary"
                    ),
                    bsCollapsePanel(
                      title = "Controls",
                      DT::dataTableOutput("swide_control"),
                      downloadButton("download_stored_control"),
                      style = "primary"
                    ),
                    bsCollapsePanel(
                      title = "Buffer",
                      DT::dataTableOutput("swide_buffer"),
                      downloadButton("download_stored_buffer"),
                      style = "primary"
                    ),
                    bsCollapsePanel(
                      title = "Sample",
                      DT::dataTableOutput("swide_sample"),
                      downloadButton("download_stored_sample"),
                      style = "primary" # set to open initially
                    ),
                    open = "Plates"
                  )
                ),
                tabPanel(
                  title = "Bead Count",
                  uiOutput("beadCountAnalysisUI")
                ),
                tabPanel(
                  title = "Standard Curve",
                  bsCollapse(
                    id = "StandardCurveCollapse",
                    multiple = TRUE,  # Also adding multiple = TRUE here for consistency
                    if (mininum_dilution_count_boolean()){
                      bsCollapsePanel(
                        title = "Standard Curve Fitting",
                        uiOutput("standardCurveUI"),
                        style = "primary"
                      )
                    },
                    bsCollapsePanel(
                      title = "Standard Curve Summary",
                      if (n_plates_standard_curve() >= 2) {
                        uiOutput("standardCurveSummaryUI")
                      } else {
                        output$invalidPlatesUI <- renderUI({
                          req(input$readxMap_study_accession, input$readxMap_experiment_accession)
                          HTML(paste("<span style='font-size:20px;'>There must be 2 or more plates in ",
                                     input$readxMap_experiment_accession, "for ",
                                     input$readxMap_study_accession," to access the standard curve summary<br></span>"))
                        })
                        uiOutput("invalidPlatesUI")
                      },
                      style = "primary"
                    )
                  )
                ),
                # tabPanel(
                #   title = "Dilution Analysis",
                #   uiOutput("dilutionalLinearity_UI")
                # ),
                tabPanel(
                  title = "Dilution Analysis",
                  conditionalPanel(
                    condition = "input.inLoadedData == 'Dilution Analysis'",
                    uiOutput("dilutionAnalysisUI"),
                    bsCollapse(
                      id = "main_dilution_linearity_collapse",
                    bsCollapsePanel(
                      title = "Dilutional Linearity",
                      uiOutput("dilutionalLinearityUI"),
                      style = "primary"
                    )
                    )
                  )
                ),
                if (plate_data_valid) {
                  x <- paste0("plateNorm", selected_studyexpplate$study_accession, selected_studyexpplate$experiment_accession)
                  plateNormalizationServer(
                    id = x,
                    stored_plates_data = stored_plates_data,
                    selected_studyexpplate = selected_studyexpplate
                  )
                  plateNormalizationUI(x)
                },
                tabPanel(
                  title = "Outliers",
                  uiOutput("outlierTab")
                ),
                tabPanel(
                  title = "Subgroup Detection",
                  bsCollapse(
                    id = "subgroupCollapse",
                    multiple = TRUE,  # This allows multiple panels to be open simultaneously
                    bsCollapsePanel(
                      title = "Subgroup Detection",
                      # if (length(reference_arm()) != 0) {
                      uiOutput("subgroupDetectionUI"),
                      # } else {
                      #   output$noSubgroupUI <- renderUI({
                      #     HTML(paste("<span style='font-size:20px;'> Please set a reference arm by navigating to Study Overview."))
                      #   })
                      #uiOutput("noSubgroupUI"),
                      #},
                      style = "primary"
                    ),
                    bsCollapsePanel(
                      title = "Subgroup Detection Summary",
                      uiOutput("subgroup_summary_UI"),
                      style = "primary"
                    )
                  ) # end bsCollapse
                ) # end TabPanel

    ) # end tabsetPanel
    )
  }) # end renderUI


  remove_modal_progress(session = getDefaultReactiveDomain())
})

check_nsample_plate <- function(df, plate_column){
  # Count the number of samples per plate
  plate_sample_counts <- table(df[[plate_column]])

  # all plates must have 2 samples
  min_n_per_plate <- 2
  all_samples_valid <- all(plate_sample_counts >= min_n_per_plate)
  # cat("Total rows in data frame:", nrow(df), "\n")
  # cat("Plate sample counts:", plate_sample_counts, "\n")

  return(all_samples_valid)
}

count_n_std_curve_plates <- function(study_accession, experiment_accession){
  num_plates_query <- paste0("SELECT plateid
  	FROM madi_results.xmap_standard_fits
  	WHERE study_accession = '",study_accession,"' and experiment_accession = '",experiment_accession ,"'
  	GROUP BY plateid;")


  # Run the query and fetch the result as a data frame
  plate_list <- dbGetQuery(conn, num_plates_query)
  cat("Previously stored plate list")
  print(plate_list)

  num_plates <- nrow(plate_list)
  # cat("num_plates")
  #print(num_plates)

  return(num_plates)

}

count_n_dilutions_per_antigen <- function(study_accession, experiment_accession) {

  n_dilutions_query <- paste0("SELECT antigen, COUNT(DISTINCT dilution) AS num_dilutions
  FROM madi_results.xmap_standard
 	WHERE study_accession = '",study_accession,"' and experiment_accession = '",experiment_accession ,"'
  	GROUP BY antigen;")
  # print(n_dilutions_query)

  n_dilution_per_antigen_df <- dbGetQuery(conn, n_dilutions_query)
  return(n_dilution_per_antigen_df)
  # record_count <<- table(n_dilution_per_antigen_df$antigen)
  #
  # # Find antigens with fewer than 5 records
  # if (any(record_count < 5)) {
  #   # Return FALSE if there are antigens with fewer than 5 records
  #   result <- FALSE
  # } else {
  #   # Return TRUE if all antigens have at least 5 records
  #   result <- TRUE
  # }
  # return(result)
}

validate_plate_data <- function(stored_plates_data){

  all_checks_pass <- T
  # Check for stored_sample
  if (!check_nsample_plate(stored_plates_data$stored_sample, "plate_id")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_buffer
  if (!check_nsample_plate(stored_plates_data$stored_buffer, "plateid")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_control
  if (!check_nsample_plate(stored_plates_data$stored_control, "plateid")) {
    all_checks_pass <- FALSE
  }

  # Check for stored_standard
  if (!check_nsample_plate(stored_plates_data$stored_standard, "plateid")) {
    all_checks_pass <- FALSE
  }
  return(all_checks_pass)
}


