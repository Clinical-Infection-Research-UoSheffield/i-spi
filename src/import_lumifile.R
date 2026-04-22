
# Extract plate number from text (filename, plateid, etc.)
# Looks for patterns like "plate_3", "plate 3", "plate.3", "plate3"
# This function is also defined in batch_layout_functions.R but is duplicated
# here to ensure availability in the upload_experiment_files observer.
extract_plate_number <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)

  # Try multiple patterns to extract plate number
  # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
  match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  if (length(match1) > 0 && nchar(match1) > 0) {
    num <- gsub("[^0-9]", "", match1)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
  match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  if (length(match2) > 0 && nchar(match2) > 0) {
    num <- gsub("[^0-9]", "", match2)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  return(NA_character_)
}

getData <- reactive({
  if(is.null(input$upload_to_shiny)) return(NULL)
})

all_completed <- reactive({
  # types present in data
  present_types <- unique_plate_types()

  # Start by requiring P to be completed
  if (!type_p_completed()) return(FALSE)

  # Map of type ->  reactive status
  status_map <- list(
    X = type_x_status,
    S = type_s_status,
    C = type_c_status,
    B = type_b_status
  )

  # For all types that are present (excluding P, which we already handled)
  # check that plate_exists == TRUE and (optionally) n_record > 0
  checks <- sapply(present_types[present_types != "P"], function(t) {
    if (!t %in% names(status_map)) return(TRUE)  # ignore unexpected types
    status <- status_map[[t]]()
    status$plate_exists && status$n_record > 0
  }, USE.NAMES = FALSE)

  # TRUE only if all present types are completed
  all(checks)
})

plate_layout_plots <- reactive({
  # Get layout sheets
  sheets <- layout_template_sheets()

  cat("\n=== plate_layout_plots REACTIVE ===\n")
  cat("  sheets is NULL:", is.null(sheets), "\n")
  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no sheets
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  cat("  Sheet names:", paste(names(sheets), collapse = ", "), "\n")

  # Check for required sheets
  plates_map <- sheets[["plates_map"]]
  plate_id_data <- sheets[["plate_id"]]

  cat("  plates_map is NULL:", is.null(plates_map), "\n")
  cat("  plate_id_data is NULL:", is.null(plate_id_data), "\n")

  if (is.null(plates_map) || is.null(plate_id_data)) {
    cat("  → Returning NULL (missing required sheets)\n")
    return(NULL)
  }

  cat("  plates_map rows:", nrow(plates_map), "\n")
  cat("  plate_id_data rows:", nrow(plate_id_data), "\n")

  # Return NULL if empty data
  if (nrow(plates_map) == 0 || nrow(plate_id_data) == 0) {
    cat("  → Returning NULL (empty data)\n")
    return(NULL)
  }

  # Check required columns exist in plates_map
  required_plates_map_cols <- c("study_name", "experiment_name", "plate_number", "well", "specimen_type")
  missing_cols <- setdiff(required_plates_map_cols, names(plates_map))
  if (length(missing_cols) > 0) {
    cat("  ⚠️ plates_map missing columns:", paste(missing_cols, collapse = ", "), "\n")
    cat("  → Returning NULL\n")
    return(NULL)
  }

  # Check required columns exist in plate_id_data
  required_plate_id_cols <- c("study_name", "experiment_name", "plate_number", "number_of_wells")
  missing_plate_id_cols <- setdiff(required_plate_id_cols, names(plate_id_data))
  if (length(missing_plate_id_cols) > 0) {
    cat("  ⚠️ plate_id_data missing columns:", paste(missing_plate_id_cols, collapse = ", "), "\n")
    cat("  → Returning NULL\n")
    return(NULL)
  }

  cat("  ✓ All required columns present\n")
  cat("  Calling plot_plate_layout()...\n")

  # Call the plotting function with error handling
  result <- tryCatch({
    plots <- plot_plate_layout(plates_map, plate_id_data)
    # add nominal sample dilution
    names(plots) <- vapply(names(plots), function(nm) {

      idx <- which(vapply(
        plate_id_data$plate_number,
        function(pn) grepl(pn, nm),
        logical(1)
      ))

      if (length(idx) == 1) {
        paste0(nm, "-", plate_id_data$nominal_sample_dilution[idx])
      } else {
        nm
      }

    }, character(1))


    cat("  ✓ plot_plate_layout() returned", length(plots), "plots\n")
    if (length(plots) > 0) {
      cat("  Plot names:", paste(names(plots), collapse = ", "), "\n")
    }
    plots
  }, error = function(e) {
    cat("  ✗ ERROR in plot_plate_layout():", conditionMessage(e), "\n")
    cat("  Stack trace:\n")
    print(sys.calls())
    return(NULL)
  })

  cat("=================================\n\n")
  return(result)
})

### Outputs
output$upload_path_text <- renderText({
  paste(stri_replace_all_charclass(Sys.getenv("upload_template_path"), "\\p{WHITE_SPACE}", ""))
})

output$fileUploaded <- reactive({
  return(!is.null(getData()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

# UI COMPONENT: Delete Study UI
output$delete_study_ui <- renderUI({
  tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "delete_study") {
    if (input$readxMap_study_accession != "Click here") {
      selected_study_accession <- input$readxMap_study_accession
      import_plate_data_title <- paste("Delete", selected_study_accession, "Plate Data", sep = " ")

      tagList(
        fluidPage(
          fluidRow(
            column(12,
                   h3(import_plate_data_title, style = "color: #d9534f; margin-bottom: 20px;"),
                   hr()
            )
          ),
          fluidRow(
            column(12,
                   h4("Tables with data for this study:", style = "margin-bottom: 15px;"),
                   div(
                     style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd;",
                     DT::dataTableOutput("delete_study_table")
                   )
            )
          ),
          fluidRow(
            column(12,
                   div(
                     style = "margin-top: 25px; padding: 15px; background-color: #fcf8e3; border: 1px solid #faebcc; border-radius: 5px;",
                     icon("exclamation-triangle", style = "color: #8a6d3b;"),
                     span(
                       "Warning: Deleting study data is permanent and cannot be undone.",
                       style = "color: #8a6d3b; font-weight: bold; margin-left: 10px;"
                     )
                   )
            )
          ),
          fluidRow(
            column(12,
                   div(
                     style = "margin-top: 20px; text-align: center;",
                     actionButton(
                       "delete_study_btn",
                       label = tagList(icon("trash"), "Delete Study Data"),
                       class = "btn-danger btn-lg",
                       style = "padding: 12px 30px; font-size: 16px;"
                     )
                   )
            )
          )
        )
      )
    } else {
      tagList(
        fluidPage(
          fluidRow(
            column(12,
                   div(
                     style = "text-align: center; padding: 50px; color: #777;",
                     icon("hand-pointer", style = "font-size: 48px; margin-bottom: 20px;"),
                     h4("Choose a study for deleting plate data")
                   )
            )
          )
        )
      )
    }
  }
})

# SERVER: Reactive to fetch row counts for selected study
delete_study_data <- reactive({
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  selected_study_accession <- input$readxMap_study_accession

  query <- glue_sql(
    "SELECT * FROM public.count_study_accession_rows({selected_study_accession}) WHERE row_count > 0;",
    selected_study_accession = selected_study_accession,
    .con = conn
  )

  tryCatch({
    result <- dbGetQuery(conn, query)
    result
  }, error = function(e) {
    shinyalert(
      title = "Error",
      text = paste("Error fetching data:", e$message),
      type = "error"
    )
    data.frame(
      schema_name = character(),
      table_name = character(),
      row_count = integer()
    )
  })
})

# SERVER: Render the DataTable
output$delete_study_table <- DT::renderDataTable({
  req(delete_study_data())

  data <- delete_study_data()

  if (nrow(data) == 0) {
    return(
      DT::datatable(
        data.frame(Message = "No data found for this study accession"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    )
  }

  # Add total row
  total_row <- data.frame(
    schema_name = "TOTAL",
    table_name = "",
    row_count = sum(data$row_count)
  )
  data_with_total <- rbind(data, total_row)

  DT::datatable(
    data_with_total,
    colnames = c("Schema", "Table", "Row Count"),
    rownames = FALSE,
    options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 2),
        list(className = 'dt-left', targets = c(0, 1))
      )
    ),
    class = "table table-striped table-bordered"
  ) %>%
    DT::formatStyle(
      columns = c("schema_name", "table_name", "row_count"),
      target = "row",
      backgroundColor = DT::styleEqual("TOTAL", "#f5f5f5"),
      fontWeight = DT::styleEqual("TOTAL", "bold")
    ) %>%
    DT::formatRound(columns = "row_count", digits = 0, mark = ",")
})

output$readxMapData <- renderUI({
 tabRefreshCounter()$import_tab
  if (input$main_tabs != "home_page" & input$main_tabs != "manage_project_tab" & input$study_tabs == "import_tab") {
  if (input$readxMap_study_accession != "Click here") {
    import_plate_data_title <- paste("Import", input$readxMap_study_accession, "Plate Data", sep = " ")
    tagList(
      fluidPage(
        tagList(
          h3(import_plate_data_title),
          bsCollapsePanel(
            "Instructions for Importing and Uploading batches of plate files.",
            p("This is where you can import plate data into I-SPI. Select an assay type above, then choose from the available file formats:"),
            tags$ul(
              tags$li(tags$strong("Bead Array: Raw File"), " — Upload batch .xlsx raw data files, generate a layout template, and upload."),
              tags$li(tags$strong("Bead Array: xPONENT"), " — Upload batch xPONENT .csv files, generate a layout template, and upload.")
            ),
            # Section A: Before uploading
            h4("A. Before Uploading Batch Plate Files"),
            p("Ensure you have completed the following:"),
            tags$ol(
              tags$li("Created or loaded a Project - Navigate to \"Create, Add, and Load Projects\" in the sidebar."),
              tags$li("Created or selected a Study - Type a new study name (up to 15 characters) or select an existing one."),
              tags$li("Selected the sidebar option to Import Plate Data."),
              tags$li("Created or selected an Experiment - Each experiment represents a specific assay run or feature (e.g., \"IgG_total\", \"FcgR2a\")."),
              tags$li("Selected an assay type and file format (e.g. Raw File or xPONENT).")
            ),

            # Section B: Upload Experiment Files
            h4("B. Upload Experiment Files"),
            tags$ol(
              tags$li("Click \"Select all experiment raw data files\"."),
              tags$li("Select all plate files (.xlsx) for this batch."),
              tags$li("Enter a Feature name (up to 15 characters) - this identifies the assay type (e.g., \"IgGtot\", \"FcgR2a\").")
            ),

            # Section C: Configure Plate Settings
            h4("C. Configure Plate Settings"),
            tags$ol(
              tags$li(
                strong("Number of Wells:"),
                p("Select the plate format (6, 12, 24, 48, 96, 384, or 1536 wells).")
              ),
              tags$li(
                strong("Description Delimiter:"),
                p("If your Description field contains data, select the character that separates elements:"),
                tags$ul(
                  tags$li(tags$code("_"), " (underscore) - most common"),
                  tags$li(tags$code("|"), " (pipe)"),
                  tags$li(tags$code(":"), " (colon)"),
                  tags$li(tags$code("-"), " (hyphen)")
                )
              ),
              tags$li(
                strong("Optional Elements:"),
                p("Check/uncheck to include group assignments:"),
                tags$ul(
                  tags$li("☑ SampleGroupA"),
                  tags$li("☑ SampleGroupB")
                )
              ),
              tags$li(
                strong("Element Order:"),
                p("Drag and drop to match your Description field structure:"),
                tags$ul(
                  tags$li("Default order: PatientID → TimePeriod → DilutionFactor"),
                  tags$li("With groups: PatientID → SampleGroupA → SampleGroupB → TimePeriod → DilutionFactor")
                )
              )
            ),

            # Section D: Generate Layout Template
            h4("D. Generate Layout Template"),
            tags$ol(
              tags$li("Click \"Generate a Layout file\"."),
              tags$li("Save the Excel file to your computer."),
              tags$li("The template contains pre-populated sheets based on your plate data.")
            ),

            # Section E: Review and Edit Layout Template
            h4("E. Review and Edit Layout Template"),
            p("Open the generated Excel file and verify/edit each sheet:"),

            tags$h5("Sheet: plate_id"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("experiment_name"), tags$td("Experiment identifier")),
                tags$tr(tags$td("number_of_wells"), tags$td("Plate format (96, 384, etc.)")),
                tags$tr(tags$td("plate_number"), tags$td("Internal plate identifier")),
                tags$tr(tags$td("plateid"), tags$td("Plate ID from instrument")),
                tags$tr(tags$td("plate_filename"), tags$td("Original file path"))
              )
            ),

            tags$h5("Sheet: subject_groups"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("subject_id"), tags$td("Unique patient/subject identifier")),
                tags$tr(tags$td("groupa"), tags$td("First categorical grouping")),
                tags$tr(tags$td("groupb"), tags$td("Second categorical grouping"))
              )
            ),

            tags$h5("Sheet: timepoint"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("timepoint_tissue_abbreviation"), tags$td("Short timepoint code")),
                tags$tr(tags$td("tissue_type"), tags$td("e.g., \"blood\"")),
                tags$tr(tags$td("tissue_subtype"), tags$td("e.g., \"serum\"")),
                tags$tr(tags$td("description"), tags$td("Full timepoint description")),
                tags$tr(tags$td("min_time_since_day_0"), tags$td("Minimum days from baseline")),
                tags$tr(tags$td("max_time_since_day_0"), tags$td("Maximum days from baseline"))
              )
            ),

            tags$h5("Sheet: antigen_list"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("antigen_label_on_plate"), tags$td("Column name from plate file")),
                tags$tr(tags$td("antigen_abbreviation"), tags$td("Short name for analysis")),
                tags$tr(tags$td("antigen_family"), tags$td("Grouping category")),
                tags$tr(tags$td("standard_curve_max_concentration"), tags$td("Upper limit for curve fitting"))
              )
            ),

            tags$h5("Sheet: plates_map"),
            tags$table(
              class = "table table-bordered table-sm",
              tags$thead(tags$tr(tags$th("Column"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("study_name"), tags$td("Study identifier")),
                tags$tr(tags$td("plate_number"), tags$td("Plate identifier")),
                tags$tr(tags$td("well"), tags$td("Well position")),
                tags$tr(tags$td("specimen_type"), tags$td("X, S, B, C, or empty")),
                tags$tr(tags$td("specimen_source"), tags$td("Source material identifier")),
                tags$tr(tags$td("specimen_dilution_factor"), tags$td("Numeric dilution")),
                tags$tr(tags$td("subject_id"), tags$td("Links to subject_groups")),
                tags$tr(tags$td("biosample_id_barcode"), tags$td("Sample barcode")),
                tags$tr(tags$td("timepoint_tissue_abbreviation"), tags$td("Links to timepoint sheet"))
              )
            ),

            # Section F: Upload Completed Layout
            h4("F. Upload Completed Layout"),
            tags$ol(
              tags$li("Click \"Upload a completed layout file\"."),
              tags$li("Select your edited layout template."),
              tags$li("Review the plate layout visualization."),
              tags$li(
                "Configure Blank and Empty Well Handling:",
                tags$ul(
                  tags$li("\"Skip Empty Wells\" - removes blank entries"),
                  tags$li("\"Use as Blank\" - treats as background controls")
                )
              )
            ),

            # Section G: Validate and Upload
            h4("G. Validate and Upload"),
            tags$ol(
              tags$li("Check the Batch Validated badge appears (green checkmark)."),
              tags$li("If validation fails, review error messages and correct issues."),
              tags$li("Click \"Upload Batch\" to store data in database."),
              tags$li("Verify Batch Uploaded badge appears.")
            ),

            style = "warning"
          )
        ),
        br()
        ,
        # ---- Name the Experiment ----

        fluidRow(
          column(5,
                 tagList(
                   div(
                     style = "width: 700px;",
                     tags$label(
                       `for` = "readxMap_experiment_accession_import",
                       style = "display: block; padding-left: 15px;",
                       "Select Experiment Name",
                       tags$br(),
                       tags$small(style = "font-weight: normal;",
                                  "To create a new experiment, type in this box (up to 15 characters)."
                       )
                     ),
                     # conditionalPanel(
                     #  condition = "input.readxMap_study_accession != 'Click here'",
                     selectizeInput("readxMap_experiment_accession_import",
                                    label = NULL,
                                    # "Choose Existing Experiment Name OR Create a New Experiment Name (by typing up to 15 characters)",
                                    # choices <- c("Click OR Create New" = "Click here"),
                                    choices <- experiment_choices_rv(),
                                    selected = "Click here",
                                    multiple = FALSE,
                                    options = list(create = TRUE,
                                                   onType = I("function(str) {
                                                if (str.length > 15) {
                                                  this.setTextboxValue(str.substring(0, 15));
                                                }
                                              }")), width = '700px'
                     )
                     # )
                   )
                 )
          )
        )
        ,
        # ---- ASSAY TYPE SELECTOR ----
        fluidRow(
          column(9,
                 shinyWidgets::radioGroupButtons(
                   inputId = "assay_type_selector",
                   label = "Select Assay Type",
                   choices = c("Bead Array", "ELISA", "Post-gating Flow Cytometry"),
                   selected = "Bead Array",
                   justified = TRUE,
                   checkIcon = list(
                     yes = icon("check", lib = "font-awesome")
                   )
                 )
          )
        ),
        # ---- ELISA IMPORT SECTION ----
        conditionalPanel(
          condition = "input.assay_type_selector == 'ELISA'",
          fluidRow(
            column(3,
                   wellPanel(
                     h4("ELISA File Upload"),
                     p("Upload ELISA Excel files containing 'results' and 'plate_map' sheets."),

                     # Step 1: Upload ELISA files
                     fileInput("upload_elisa_experiment_files",
                               label = "Select ELISA data file(s)",
                               accept = c(".xlsx", ".xls"),
                               multiple = TRUE),

                     conditionalPanel(
                       condition = "output.hasElisaData",
                       hr(),
                       h4("Layout Template Options"),

                       numericInput("elisa_n_wells_on_plate",
                                    "Number of wells per plate",
                                    value = 96, min = 96, max = 384, step = 288),

                       textInput("elisa_description_delimiter",
                                 "Description Delimiter",
                                 value = "_"),

                       # Optional elements
                       tags$div(
                         class = "element-controls",
                         tags$span(style = "font-weight: 600;", "Include Optional Elements:"),
                         checkboxGroupButtons(
                           inputId = "elisa_optional_elements",
                           label = NULL,
                           choices = c("SampleGroupA", "SampleGroupB"),
                           selected = c("SampleGroupA", "SampleGroupB"),
                           status = "outline-primary",
                           checkIcon = list(
                             yes = icon("check"),
                             no = icon("times")
                           )
                         )
                       ),

                       conditionalPanel(
                         condition = "output.hasElisaData",
                         uiOutput("elisa_order_input_ui"),
                         uiOutput("elisa_bcsorder_input_ui")
                       ),

                       hr(),
                       downloadButton("elisa_blank_layout_file", "Generate ELISA Layout Template",
                                      class = "btn-info btn-block"),

                       hr(),
                       fileInput("upload_elisa_layout_file",
                                 label = "Upload completed ELISA layout file",
                                 accept = c(".xlsx", ".xls"),
                                 multiple = FALSE)
                     )
                   )
            ),
            column(9,
                   # Description warnings
                   uiOutput("elisa_description_warning_ui"),

                   # File summary
                   conditionalPanel(
                     condition = "output.hasElisaData",
                     wellPanel(
                       h4("ELISA Data Summary"),
                       verbatimTextOutput("elisa_file_summary")
                     )
                   ),

                   # Validation status
                   uiOutput("elisa_validation_status"),

                   # View layout sheets (reuses batch view pattern)
                   conditionalPanel(
                     condition = "output.hasElisaLayoutSheets",
                     wellPanel(
                       h4("Layout File Contents"),
                       fluidRow(
                         column(2, actionButton("view_elisa_plate_id", "Plate ID", class = "btn-sm btn-default")),
                         column(2, actionButton("view_elisa_plates_map", "Plates Map", class = "btn-sm btn-default")),
                         column(2, actionButton("view_elisa_subject_groups", "Subjects", class = "btn-sm btn-default")),
                         column(2, actionButton("view_elisa_timepoint", "Timepoint", class = "btn-sm btn-default")),
                         column(2, actionButton("view_elisa_antigen_list", "Antigens", class = "btn-sm btn-default")),
                         column(2, actionButton("view_elisa_metadata", "ELISA Metadata", class = "btn-sm btn-default"))
                       )
                     )
                   )
            )
          )
        ),
        # ---- POST-GATING FLOW CYTOMETRY PLACEHOLDER ----
        conditionalPanel(
          condition = "input.assay_type_selector == 'Post-gating Flow Cytometry'",
          fluidRow(
            column(12,
                   div(
                     style = "text-align: center; padding: 80px 40px; color: #777; background-color: #f9f9fa; border: 2px dashed #ccc; border-radius: 10px; margin: 20px 0;",
                     icon("filter", style = "font-size: 64px; margin-bottom: 20px; color: #aaa;"),
                     h3("Post-gating Flow Cytometry Data Import", style = "color: #555;"),
                     p("Post-gating Flow Cytometry data import is currently under development.", style = "font-size: 16px;"),
                     p("This feature will be available in a future release.", style = "font-size: 14px; color: #999;")
                   )
            )
          )
        ),
        # ---- BEAD ARRAY CONTENT (existing workflow, refactored) ----
        conditionalPanel(
          condition = "input.assay_type_selector == 'Bead Array'",
        fluidRow(
          column(9,
                 conditionalPanel(
                   condition = "input.readxMap_experiment_accession_import != 'Click here' && input.readxMap_experiment_accession_import != ''",
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Import Context", style = "margin-top: 0; color: #2c5aa0;"),
                     textOutput("current_context_display")
                   )
                 )
          )
        ),
        fluidRow(
          column(9,
                 conditionalPanel(
                   condition = "input.readxMap_experiment_accession_import != 'Click here' && input.readxMap_experiment_accession_import != ''",
                   shinyWidgets::radioGroupButtons(
                     inputId = "xPonentFile",
                     label = "File Format",
                     choices = c("Raw File", "xPONENT"),
                     selected = "Raw File",
                     justified = TRUE,
                     checkIcon = list(
                       yes = icon("check", lib = "font-awesome")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.xPonentFile == 'xPONENT'",
                     # ---- xPONENT BATCH UPLOAD (NEW - mirrors Raw File workflow) ----
                     tags$div(
                       style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                       tags$div(
                         class = "element-controls",
                         fileInput("upload_xponent_experiment_files",
                                   label = "Select all xPONENT data files (.csv)",
                                   accept = c(".csv"),
                                   multiple = TRUE),
                         tags$span(style = "font-weight: 600; align-self: center;", "Feature:"),
                         textInput("xponent_feature_value", "e.g. Total_IgG; FcgR2a; multiple", "Up to 15 chars")
                       )
                     ),
                     conditionalPanel(
                       condition = "output.hasXponentExperimentPath",
                       tags$div(
                         style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         tags$div(
                           class = "element-controls",
                           tags$span(style = "font-weight: 600; align-self: center;", "Wells:"),
                           radioGroupButtons(
                             inputId = "xponent_n_wells_on_plate",
                             label = NULL,
                             choices = c("96" = 96, "6" = 6, "12" = 12, "24" = 24, "48" = 48, "384" = 384, "1536" = 1536),
                             selected = 96,
                             size = "sm",
                             status = "outline-primary"
                           )
                         ),
                         conditionalPanel(
                           condition = "output.xponentDescriptionHasContent",
                           tags$div(
                             class = "element-controls",
                             tags$span(style = "font-weight: 600; align-self: center;", "Description Delimiter:"),
                             radioGroupButtons(
                               inputId = "xponent_description_delimiter",
                               label = NULL,
                               choices = setNames(c("_", "|", ":", "-"), c("_", "|", ":", "-")),
                               selected = "_",
                               size = "sm",
                               status = "outline-primary"
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "output.xponentDescriptionHasSufficientElements",
                           tags$div(
                             class = "element-controls",
                             tags$span(style = "font-weight: 600; align-self: center;", "Include Optional Elements:"),
                             checkboxGroupButtons(
                               inputId = "xponent_optional_elements",
                               label = NULL,
                               choices = c("SampleGroupA", "SampleGroupB"),
                               selected = c("SampleGroupA", "SampleGroupB"),
                               status = "outline-primary",
                               checkIcon = list(
                                 yes = icon("check"),
                                 no = icon("times")
                               )
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "output.xponentDescriptionHasSufficientElements",
                           uiOutput("xponent_order_input_ui")
                         ),
                         conditionalPanel(
                           condition = "output.xponentDescriptionHasContent",
                           uiOutput("xponent_bcsorder_input_ui")
                         ),
                         downloadButton("xponent_blank_layout_file", "Generate a Layout file")
                       ),
                       fileInput("upload_xponent_layout_file",
                                 label = "Upload a completed layout file (only accepts xlsx, xls)",
                                 accept = c(".xlsx", ".xls"),
                                 multiple = FALSE)
                     )
                   ),
                   conditionalPanel(
                     condition = "input.xPonentFile == 'Raw File'",
                     tags$div(
                       style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                       tags$div(
                         class = "element-controls",
                         fileInput("upload_experiment_files",
                                   label = "Select all experiment raw data files",
                                   accept = c(".xlsx", ".xls"),
                                   multiple = TRUE),
                         tags$span(style = "font-weight: 600; align-self: center;", "Feature:"),
                         textInput("feature_value", "e.g. Total_IgG; FcgR2a; multiple", "Up to 15 chars")
                       )
                     ),
                     conditionalPanel(
                       condition = "output.hasExperimentPath",
                       tags$div(
                         style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         tags$div(
                           class = "element-controls",
                           # style = "display: flex; align-items: center; gap: 10px;",
                           tags$span(style = "font-weight: 600; align-self: center;", "Wells:"),
                           radioGroupButtons(
                             inputId = "n_wells_on_plate",
                             label = NULL,
                             choices = c("96" = 96, "6" = 6, "12" = 12, "24" = 24, "48" = 48, "384" = 384, "1536" = 1536),
                             selected = 96,
                             size = "sm",
                             status = "outline-primary"
                           )
                         ),
                         conditionalPanel(
                           condition = "output.descriptionHasContent",
                           tags$div(
                             class = "element-controls",
                             tags$span(style = "font-weight: 600; align-self: center;", "Description Delimiter:"),
                             radioGroupButtons(
                               inputId = "description_delimiter",
                               label = NULL,
                               choices = setNames(c("_", "|", ":", "-"), c("_", "|", ":", "-")),
                               selected = "_",
                               size = "sm",
                               status = "outline-primary"
                             )
                           )
                           ),
                          conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
                           # Checkboxes to include/exclude optional elements
                           tags$div(
                             class = "element-controls",
                             tags$span(style = "font-weight: 600; align-self: center;", "Include Optional Elements:"),
                             checkboxGroupButtons(
                               inputId = "optional_elements",
                               label = NULL,
                               choices = c("SampleGroupA", "SampleGroupB"),
                               selected = c("SampleGroupA", "SampleGroupB"),
                               status = "outline-primary",
                               checkIcon = list(
                                 yes = icon("check"),
                                 no = icon("times")
                               )
                             )
                           )
                           ),
                         conditionalPanel(
                           condition = "output.descriptionHasSufficientElements",
                           uiOutput("order_input_ui")
                         ),
                         conditionalPanel(
                           condition = "output.descriptionHasContent",
                           uiOutput("bcsorder_input_ui")
                         )
                         ,
                         downloadButton("blank_layout_file", "Generate a Layout file")
                       ),
                       fileInput("upload_layout_file"
                                 , label="Upload a completed layout file (only accepts xlsx, xls)"
                                 , accept=c(".xlsx",".xls")
                                 , multiple=FALSE)
                     )
                   ),
                     ),
                     conditionalPanel(
                       condition = "output.hasLayoutTemplateSheets",
                       uiOutput("description_warning_ui"),
                       uiOutput("view_layout_file_ui"),
                       uiOutput("plate_layout_selector"),
                       plotlyOutput("selected_plate_layout_plot"),
                      conditionalPanel(
                         condition = "output.hasExperimentPath",
                      tagList(
                        tags$p("If this batch of plates contains wells without samples use the word the 'Blank' in the description column of the spreadsheet.
            Then assign the two phrases below to indicate if the wells should be treated as blanks
            (e.g. containing PBS) or if the wells should be treated as empty."),

                        selectInput("batch_blank", "Blank and Empty Well Handling",
                                    choices = c("Skip Empty Wells" = "empty_well",
                                                "Use as Blank" = "use_as_blank"))
                      ),
                       uiOutput("batch_validation_status"),
                       tableOutput("batch_invalid_messages"),
                       uiOutput("upload_batch_data_button")
                      ) # end of hasExperimentPath condition
                     )
                     )
                 )
          ),
          column(9,
                 # Removed: old xPONENT single-file parse UI and RAW segment selector
                 # Both pathways now use the shared layout template workflow below
          )
        )
        ) # end conditionalPanel for Bead Array
    #   )
    # )
  } else {
    import_plate_data_title<- paste("Choose or create a study for Importing Plate Data")
  }
   }
})

output$description_warning_ui <- renderUI({
  # Explicit dependency
  status <- description_status()

  # Also depend on batch_plate_data to clear when data is cleared
  plate_data <- batch_plate_data()

  # Return NULL if no plate data or status not checked
  if (is.null(plate_data) || !status$checked) {
    return(NULL)
  }

  if (!status$has_content) {
    # Warning for completely blank Description
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Description Field is Blank",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p("The Description field in your plate data is empty. Default values will be used:"),
      tags$ul(
        tags$li("Subject ID: '1'"),
        tags$li("Sample Dilution Factor: 1"),
        tags$li("Timeperiod: 'T0'"),
        tags$li("Groups: 'Unknown'")
      ),
      tags$p(
        tags$strong("You will need to manually update the layout template with correct values before uploading."),
        style = "margin-bottom: 0; color: #856404;"
      )
    )
  } else if (!status$has_sufficient_elements) {
    # Warning for insufficient elements
    tags$div(
      style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 5px;",
      tags$h5(
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #856404;"),
        " Insufficient Description Elements",
        style = "margin-top: 0; color: #856404;"
      ),
      tags$p(sprintf(
        "The Description field has only %d element(s), but at least %d are required.",
        status$min_elements_found,
        status$required_elements
      )),
      tags$p("Missing fields will be filled with default values. Manual update may be required.")
    )
  } else {
    return(NULL)
  }
})

output$hasExperimentPath <- reactive({
  path_df <- input$upload_experiment_files   # fileInput returns a data frame
  xponent_df <- input$upload_xponent_experiment_files
  raw_has_files <- !is.null(path_df) && nrow(path_df) > 0
  xponent_has_files <- !is.null(xponent_df) && nrow(xponent_df) > 0
  raw_has_files || xponent_has_files
})

outputOptions(output, "hasExperimentPath", suspendWhenHidden = FALSE)

# Reactive for xPONENT experiment files (used in xPONENT-specific conditionalPanels)
output$hasXponentExperimentPath <- reactive({
  path_df <- input$upload_xponent_experiment_files
  !is.null(path_df) && nrow(path_df) > 0
})
outputOptions(output, "hasXponentExperimentPath", suspendWhenHidden = FALSE)

# Description status reactives for xPONENT (mirror the Raw File ones)
output$xponentDescriptionHasContent <- reactive({
  status <- description_status()
  # Only show when xPONENT is selected and files are uploaded
  if (is.null(input$upload_xponent_experiment_files)) return(FALSE)
  return(isTRUE(status$has_content))
})
outputOptions(output, "xponentDescriptionHasContent", suspendWhenHidden = FALSE)

output$xponentDescriptionHasSufficientElements <- reactive({
  status <- description_status()
  if (is.null(input$upload_xponent_experiment_files)) return(FALSE)
  return(isTRUE(status$has_sufficient_elements))
})
outputOptions(output, "xponentDescriptionHasSufficientElements", suspendWhenHidden = FALSE)

# hasLayoutTemplateSheets
# This reactive is used by the conditionalPanel to control visibility of
# view_layout_file_ui, plate_layout_selector, selected_plate_layout_plot,
# and description_warning_ui outputs after layout file is uploaded and processed.

output$hasLayoutTemplateSheets <- reactive({
  sheets <- layout_template_sheets()

  # Return TRUE only if sheets exist and contain required data
  has_sheets <- !is.null(sheets) && length(sheets) > 0

  if (has_sheets) {
    # Additional check: verify required sheets exist
    required <- c("plates_map", "plate_id")
    has_required <- all(required %in% names(sheets))

    cat("hasLayoutTemplateSheets: sheets=", length(sheets),
        ", has_required=", has_required, "\n")

    return(has_required)
  }

  return(FALSE)
})

# CRITICAL: Must not suspend when hidden to work with conditionalPanel
outputOptions(output, "hasLayoutTemplateSheets", suspendWhenHidden = FALSE)

output$blank_layout_file <- downloadHandler(
  filename = function() {
    paste0(input$readxMap_study_accession, "_", input$readxMap_experiment_accession_import, "_layout_template.xlsx")
  },
  content = function(file) {
    req(input$upload_experiment_files)
    req(bead_array_header_list())

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║         GENERATING LAYOUT TEMPLATE                      ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    # CRITICAL: Use batch_plate_data() instead of reprocessing files
    # This ensures we use the EXACT same data that was uploaded
    all_plates <- batch_plate_data()

    if (is.null(all_plates)) {
      cat("⚠️  ERROR: batch_plate_data() is NULL!\n")
      cat("   Falling back to processing uploaded files...\n")
      all_plates <- process_uploaded_files(input$upload_experiment_files)
    }

    cat("Using plate data:\n")
    cat("  → Rows:", nrow(all_plates), "\n")
    cat("  → Columns:", ncol(all_plates), "\n")
    cat("  → Source files:", paste(unique(all_plates$source_file), collapse=", "), "\n")

    # Extract antigens
    metadata_cols <- c("source_file", "Well", "Type", "Description",
                       "% Agg Beads", "Sampling Errors", "Acquisition Time")
    antigen_cols <- names(all_plates)[!(names(all_plates) %in% metadata_cols)]
    cat("  → Antigens (", length(antigen_cols), "):\n", sep="")
    for (ag in antigen_cols) {
      cat("      •", ag, "\n")
    }

    desc_status <- description_status()

    generate_layout_template(
      all_plates = all_plates,
      study_accession = input$readxMap_study_accession,
      experiment_accession = input$readxMap_experiment_accession_import,
      n_wells = input$n_wells_on_plate,
      header_list = bead_array_header_list(),
      output_file = file,
      # NEW: Pass description status for handling defaults
      description_status = desc_status,
      delimiter = if (desc_status$has_content) input$description_delimiter else "_",
      element_order = if (desc_status$has_sufficient_elements) input$XElementOrder else c("PatientID", "TimePeriod", "DilutionFactor"),
      bcs_element_order = if (desc_status$has_content) input$BCSElementOrder else c("Source", "DilutionFactor"),
      feature_value = input$feature_value
    )

    # Show notification if defaults were applied
    if (!desc_status$has_content || !desc_status$has_sufficient_elements) {
      showNotification(
        "Layout template generated with default values. Please review and update before uploading.",
        type = "warning",
        duration = 10
      )
    }

    cat("✓ Layout template generated!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
  })

output$plate_metadata_info <- renderUI({
  req(inFile()) # ensure new upload triggers updates
  req(type_p_completed())
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  req(input$table_plates)

  # Convert rhandsontable to data frame
  metadata_table <- hot_to_r(input$table_plates)

  # Extract plate_id value removing extension
  plate_id <- metadata_table[metadata_table$variable == "plate_id",]$value
  #plate_id <- sub("\\.[^.]*$", "", metadata_table[metadata_table$variable == "plate_id", ]$value)

  import_plate_id <- metadata_table[metadata_table$variable == "plate", ]$value
  import_plate_number <- as.integer(gsub("\\D", "", import_plate_id))
  updateTextInput(session = session, "read_import_plate_id",  value = plate_id)
  updateTextInput(session = session, "read_import_plate_number",  value = import_plate_number)

  fluidRow(
    column(
      12,
      div(
        style = "margin-bottom: 10px;",
        tags$span(style = "font-weight: bold;", "Study: "), input$readxMap_study_accession,
        " | ",
        tags$span(style = "font-weight: bold;", "Experiment: "), input$readxMap_experiment_accession_import,
        " | ",
        tags$span(style = "font-weight: bold;", "plate_id: "), input$read_import_plate_id,
        " | ",
        tags$span(style = "font-weight: bold;", "Plate Number: "), input$read_import_plate_number
      )
    )
  )
})

output$plate_validated_status <- renderUI({
  req(input$uploaded_sheet)# trigger refresh

  if (!is_valid_plate()) {
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      createValidateBadge(is_validated = is_valid_plate())
    )
  } else {
    createValidateBadge(is_validated = is_valid_plate())
  }
})

output$plate_validation_message_table <- renderTable({
  req(plate_validation_messages())
  messages_table <- data.frame(
    "Message Number" = seq_along(plate_validation_messages()),
    "Please correct the formatting errors in the file" = plate_validation_messages(),
    check.names = FALSE
  )
})

output$ui_optimization <- renderUI({
  all_completed()
  optimization_parsed_boolean()

  if (!all_completed()) {
    return(div(
      " All required plate types must be completed before proceeding to optimization."
    ))
  }
  if (all_completed() && !optimization_parsed_boolean()) {
    fluidRow(
      tagList(
        tags$p("This plate has more than two serum dilutions. To assess dilutional linearity in the QC workflow
               the plate must be treated as different at each dilution. Splitting this optimization plate will
               also create a new experiment that parses the mixed plates into optimization experiments."),
        radioButtons(
          inputId = "decide_split",
          label = "Do you want to Split this optimization plate by serum dilution into a plate per dilution?",
          choices = c("Yes", "No")
        )
      )
    )
  }  #else if (optimization_parsed_boolean()) {
  #   createOptimizedBadge(is_optimized = optimization_parsed_boolean())
  #
  # }
  # )
})

output$ui_split_button <- renderUI({
  if (!is.null(input$decide_split) && input$decide_split == "Yes" &&
      !isTRUE(optimization_parsed_boolean())) {
    actionButton("split_opt_plates", "Split Optimization Plate")
  }
})

output$plate_optimized_status <- renderUI({
  req(input$uploaded_sheet)# trigger refresh
  input$split_opt_plates
  optimization_parsed_boolean()  # dependency

  createOptimizedBadge(is_optimized = optimization_parsed_boolean())

})

# Display basic info about uploaded files
output$file_summary <- renderPrint({
})

output$plate_layout_selector <- renderUI({
  cat("\n=== plate_layout_selector renderUI ===\n")

  # Explicit dependency on layout_template_sheets
  sheets <- layout_template_sheets()

  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no data
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  # Check for required sheets
  if (is.null(sheets[["plates_map"]]) || is.null(sheets[["plate_id"]])) {
    cat("  → Returning NULL (missing plates_map or plate_id)\n")
    return(NULL)
  }

  # Get the plots
  plots <- plate_layout_plots()

  cat("  plots is NULL:", is.null(plots), "\n")
  cat("  plots length:", length(plots), "\n")

  # Return NULL if no plots
  if (is.null(plots) || length(plots) == 0) {
    cat("  → Returning NULL (no plots generated)\n")
    return(NULL)
  }

  cat("  ✓ Rendering radioGroupButtons with", length(plots), "choices\n")
  cat("  Choices:", paste(names(plots), collapse = ", "), "\n")
  cat("=====================================\n\n")

  shinyWidgets::radioGroupButtons(
    inputId = "select_plate_layout_plot",
    label = "Select Plate Layout:",
    choices = names(plots),
    selected = names(plots)[1],
    status = "success"
  )
})

output$selected_plate_layout_plot <- renderPlotly({
  cat("\n=== selected_plate_layout_plot renderPlotly ===\n")

  # Explicit dependencies
  sheets <- layout_template_sheets()
  cat("  sheets length:", length(sheets), "\n")

  # Return NULL if no layout sheets
  if (is.null(sheets) || length(sheets) == 0) {
    cat("  → Returning NULL (no sheets)\n")
    return(NULL)
  }

  # Get plots FIRST (before requiring input selection)
  plots <- plate_layout_plots()

  cat("  plots is NULL:", is.null(plots), "\n")
  cat("  plots length:", length(plots), "\n")

  # Return NULL if no plots
  if (is.null(plots) || length(plots) == 0) {
    cat("  → Returning NULL (no plots)\n")
    return(NULL)
  }

  # Check if selection input exists and has a value
  selection <- input$select_plate_layout_plot
  cat("  input$select_plate_layout_plot:", selection, "\n")

  # If no selection yet, default to first plot
  if (is.null(selection) || selection == "" || !selection %in% names(plots)) {
    selection <- names(plots)[1]
    cat("  → Using default selection:", selection, "\n")
  }

  cat("  ✓ Rendering plot for:", selection, "\n")
  cat("=============================================\n\n")

  plots[[selection]]
})

output$batch_validation_status <- renderUI({
  # Explicit dependency on batch state
  state <- batch_validation_state()

  # Also depend on layout sheets to trigger re-render when cleared
  sheets <- layout_template_sheets()

  # Return "not validated" badge if no state or no sheets
  if (is.null(state) || is.null(sheets) || length(sheets) == 0) {
    return(createValidateBatchBadge(FALSE))
  }

  createValidateBatchBadge(state$is_validated)
})

output$batch_invalid_messages <- renderTable({
  state <- batch_validation_state()

  # Also depend on layout sheets
  sheets <- layout_template_sheets()

  # Return NULL if no data or validation passed
  if (is.null(state) || is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  if (!state$is_validated && !is.null(state$metadata_result) && !is.null(state$bead_array_result)) {
    create_batch_invalid_message_table(state$metadata_result, state$bead_array_result)
  } else {
    NULL
  }
})

output$upload_batch_data_button <- renderUI({
  # Explicit dependencies for proper invalidation
  metadata <- batch_metadata()
  state <- batch_validation_state()
  sheets <- layout_template_sheets()

  # Return NULL if no metadata or sheets (cleared state)
  if (is.null(metadata) || is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  req(state)

  metadata_batch <- metadata
  batch_study_accession <- unique(metadata$study_name)
  batch_experiment_accession <- unique(metadata$experiment_name)

  # FIXED: Use plate_id for checking existing plates (consistent with database column)
  plates_to_upload <- unique(metadata$plate_id)

  # Debug logging
  cat("\n=== UI DUPLICATE CHECK DEBUG ===\n")
  cat("Checking for existing plates with plate_id values:\n")
  cat(paste(plates_to_upload, collapse = ", "), "\n")
  cat("================================\n")

  # FIXED: Use proper parameterized query with glue_sql
  # Do not pre-format the plate list - let glue_sql handle it
  query <- glue_sql("
    SELECT plate_id
    FROM madi_results.xmap_header
    WHERE study_accession = {batch_study_accession}
    AND experiment_accession IN ({batch_experiment_accession*})
    AND plate_id IN ({plates_to_upload*});
  ", .con = conn)
  existing_plates <- DBI::dbGetQuery(conn, query)

  plates_exist_in_db <- nrow(existing_plates) > 0

  cat("Plates already in DB:", plates_exist_in_db, "\n")
  if (plates_exist_in_db) {
    cat("Found existing:", paste(existing_plates$plate_id, collapse = ", "), "\n")
  }

  # Determine badge status - use state$is_uploaded OR database check
  show_uploaded_badge <- state$is_uploaded || plates_exist_in_db

  badge <- createUploadedBatchBadge(show_uploaded_badge)

  # Show button only if validated AND not yet uploaded/existing
  button <- if (state$is_validated && !plates_exist_in_db && !state$is_uploaded) {
    actionButton("upload_batch_button", "Upload Batch")
  } else {
    NULL
  }

  tagList(
    badge,
    br(),
    button
  )
})

output$view_layout_file_ui <- renderUI({
  # Explicit dependency on layout_template_sheets
  sheets <- layout_template_sheets()

  # Return NULL if sheets is NULL or empty
  if (is.null(sheets) || length(sheets) == 0) {
    return(NULL)
  }

  # Verify required sheets exist
  required_sheets <- c("plate_id", "subject_groups", "timepoint", "plates_map", "antigen_list")
  available_sheets <- names(sheets)

  if (!all(required_sheets %in% available_sheets)) {
    return(NULL)
  }

  fluidRow(
    column(2, actionButton("view_layout_plate_id_sheet", "View Layout Plate ID")),
    column(2, actionButton("view_layout_subject_group_sheet", "View Layout Subject Map")),
    column(2, actionButton("view_layout_timepoint_sheet", "View Layout Timepoint")),
    column(2, actionButton("view_layout_plates_map_sheet", "View Layout Plate Map")),
    column(2, actionButton("view_layout_antigen_list_sheet", "View Layout Antigen List"))
  )
})

output$current_context_display <- renderText({
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  paste0(
    "Study: ", input$readxMap_study_accession, " | ",
    "Experiment: ", input$readxMap_experiment_accession_import
  )
})

output$segment_selector <- renderUI({
  #req(inFile()$datapath != )

  req(is_valid_plate()) # Only show if it has a valid plate too

  req(plate_data())  # Ensure that there is data to work with
  # require sheet
  req(input$uploaded_sheet)

  type_p_completed()

  # Find unique types in the dataset
  plate_data() %>%
    #slice(8:n()) %>%
    janitor::clean_names() %>%
    mutate(description = gsub("[^A-Za-z0-9]+", "_", description) %>% trimws(whitespace = "_")) %>%
    mutate(type = str_remove_all(type, "[0-9]")) %>%
    pull(type) %>%
    unique() -> unique_types

  unique_types <- c("P", unique_types)
  # only handle the allowed types
  allowed_types <- c("P", "X", "S", "C", "B","E")
  unique_types <- unique_types[!is.na(unique_types) & unique_types %in% allowed_types]

  unique_plate_types(unique_types)

  if (!type_p_completed()) {
    unique_types <- "P"
  }

  # Create a tab for each unique type
  tabs <- lapply(unique_types, function(type) {
    tabPanel(
      title = paste("Type", type),
      uiOutput(outputId = paste0("ui_", type))  # This will be where we place the table and inputs
    )
  })

  #late_data_in <<- plate_data()

  # add optimization tab if detect multiple sample serum dilutions.
  tabs <- append(tabs, list(uiOutput("optimization_tab")))

  #  is_opt_experiment <- is_optimization_plate(plate_data())
  # if (is_opt_experiment && all_completed()) {
  if (optimization_ready()) {
    tabs <- append(
      tabs,
      list(
        tabPanel(
          title = "Optimization",
          #uiOutput("optimize_plate_info"),
          uiOutput("plate_optimized_status"),
          uiOutput(outputId = "ui_optimization"),
          uiOutput(outputId = "ui_split_button")
        )
      )
    )
  }


  # Make sure to return the tabsetPanel with all tabs included
  do.call(tabsetPanel, tabs)

})

### Observes
# Call on server start
onSessionStart()

# Diagnostic observer to track state changes
observe({
  cat("\n=== BATCH VALIDATION STATE DEBUG ===\n")
  cat("batch_metadata() is NULL:", is.null(batch_metadata()), "\n")
  cat("layout_template_sheets() length:", length(layout_template_sheets()), "\n")
  cat("batch_plate_data() is NULL:", is.null(batch_plate_data()), "\n")

  if (!is.null(batch_metadata())) {
    cat("batch_metadata rows:", nrow(batch_metadata()), "\n")
    cat("batch_metadata study:", unique(batch_metadata()$study_accession), "\n")
  }

  state <- batch_validation_state()
  cat("Validation state - is_validated:", state$is_validated, "\n")
  cat("Validation state - is_uploaded:", state$is_uploaded, "\n")
  cat("=========================================\n")
})

observeEvent(input$optional_elements, {
  # Base elements (always included)
  base_elements <- c("PatientID", "DilutionFactor", "TimePeriod")

  # Reactive to get all active elements
  active_elements <- reactive({
    c(base_elements, input$optional_elements)
  })

  # Render order input dynamically
  output$order_input_ui <- renderUI({
    orderInput(
      inputId = "XElementOrder",
      label = "Description Label: Sample Elements (drag and drop items to change order)",
      items = active_elements(),
      width = "100%",
      item_class = "primary"
    )
  })

  output$bcsorder_input_ui <- renderUI({
    orderInput(
      inputId = "BCSElementOrder",
      label = "Description Label: Blank, Standard or Control Elements (drag and drop items to change order)",
      items = c('Source', 'DilutionFactor'),
      width = "100%",
      item_class = 'info'
    )
  })


})

observeEvent(input$readxMap_study_accession, {
  print(paste("readxMap_study_accession clicked:", input$readxMap_study_accession))

  if (input$readxMap_study_accession != "Click here") {

    # RESET ALL BATCH REACTIVES
    reset_batch_reactives()

    # RESET ALL BATCH UI ELEMENTS
    reset_batch_ui(
      session = session,
      include_experiment_files = TRUE,
      include_layout_file = TRUE
    )

    # NOTE: Output invalidation is handled by clearing layout_template_sheets()
    # in reset_batch_reactives(). The outputs view_layout_file_ui,
    # plate_layout_selector, selected_plate_layout_plot, and description_warning_ui
    # already check for NULL/empty layout_template_sheets() and return NULL.
    # DO NOT re-assign outputs here as it breaks the reactive chain.

    # RESET EXPERIMENT DROPDOWN
    # Clear the experiment selection when study changes
    updateSelectizeInput(
      session = session,
      inputId = "readxMap_experiment_accession_import",
      selected = "Click here"
    )

    # Show notification to user
    showNotification(
      "The workspace is clear for working with a new Study.",
      type = "warning",
      duration = 5
    )

    # UPDATE EXPERIMENT CHOICES
    study_exp <- reactive_df_study_exp()
    filtered_exp <- study_exp[study_exp$study_accession == input$readxMap_study_accession, ]

    print(paste("\n filtered_exp rows:", nrow(filtered_exp)))

    if (nrow(filtered_exp) > 0) {
      expvector <- setNames(filtered_exp$experiment_accession, filtered_exp$experiment_name)
    } else {
      expvector <- character(0)
    }

    experiment_drop <- c("Click OR Create New" = "Click here", expvector)
    print(paste("\n experiment choices:", experiment_drop))

    experiment_choices_rv(experiment_drop)
  }
})

observeEvent(input$readxMap_experiment_accession_import, {
  req(input$readxMap_study_accession)

  print(paste("readxMap_experiment_accession_import changed to:",
              input$readxMap_experiment_accession_import))

  if (input$readxMap_experiment_accession_import != "Click here" &&
      input$readxMap_experiment_accession_import != "") {

    # ========================================
    # RESET ALL BATCH REACTIVES
    # ========================================
    reset_batch_reactives()

    # ========================================
    # RESET ALL BATCH UI ELEMENTS
    # ========================================
    reset_batch_ui(
      session = session,
      include_experiment_files = TRUE,
      include_layout_file = TRUE
    )

    # NOTE: Output invalidation is handled by clearing layout_template_sheets()
    # in reset_batch_reactives(). The outputs view_layout_file_ui,
    # plate_layout_selector, selected_plate_layout_plot, and description_warning_ui
    # already check for NULL/empty layout_template_sheets() and return NULL.
    # DO NOT re-assign outputs here as it breaks the reactive chain.

    # cat("  ✓ Cleared dynamic UI outputs\n")

    # Show notification to user
    showNotification(
      paste("Experiment changed to:", input$readxMap_experiment_accession_import,
            "- All batch data has been cleared. Please upload new files."),
      type = "warning",
      duration = 5
    )
  }
})

### read template and create the preview template tab
### Clicks browse to load a plate/batch
## ==========================================================================
## DEPRECATED: Single-file RAW upload observers (removed from UI in refactor)
## These observers supported the old single-file RAW upload workflow.
## They are retained as dead code for reference but will not be triggered
## since the RAW file format option was removed from the UI.
## ==========================================================================

observeEvent(input$upload_to_shiny,{

  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)
  availableSheets(NULL)
  # # reset reactive that hold the data
  inFile(NULL)
  plate_data(NULL)
  header_info(NULL)
  plate_validation_messages(NULL)
  is_valid_plate(NULL)
  #imported_h_study(NULL)
  #imported_h_experiment(NULL)
  #imported_h_plate_id(NULL)
  #imported_h_plate_number(NULL)
  type_p_completed(FALSE)
  # type_x_status(list(plate_exists = FALSE, n_record = 0))
  # type_s_status(list(plate_exists = FALSE, n_record = 0))
  # type_c_status(list(plate_exists = FALSE, n_record = 0))
  # type_b_status(list(plate_exists = FALSE, n_record = 0))
  transform_dat <- data.frame()
  # original_df_combined <- reactive({NULL})
  # list_of_dataframes <- reactive({NULL})
  updateSelectInput(session = session, "uploaded_sheet", NULL)

  print("file_uploaded")

  # Store the uploaded file
  inFile(
    input$upload_to_shiny
  )

  if (is.null(inFile())) {
    return(NULL)
  }

  sheets <- readxl::excel_sheets(inFile()$datapath)
  availableSheets(sheets)
  # Dynamically generate UI for sheet selection
  output$sheet_ui <- renderUI({
    sheets <- availableSheets()
    if (!is.null(sheets)) {
      fluidRow(
        selectInput("uploaded_sheet",
                    "Select Sheet",
                    choices = c("Select excel sheet" = "", sheets),  # Combine default with sheets
                    selected = "")
      )
    }

  })

  output$raw_ui <- renderUI({
    sheets <- availableSheets()
    if (!is.null(sheets)) {
      fluidRow(
        if (is.null(plate_validation_messages())) {
          tagList(
            tags$p("If this plate contains wells without samples use the word the 'Blank' in the description column of the spreadsheet.
          Then assign the two phrases below to indicate if the wells should be treated as blanks
          (e.g. containing PBS) or if the wells should be treated as empty."),

            selectInput("blank_keyword", "Blank and Empty Well Handling",
                        choices = c("Skip Empty Wells" = "empty_well",
                                    "Use as Blank" = "use_as_blank"))
          )
        },
        actionButton("view_raw_file", "View Raw File"),
        actionButton("view_raw_header", "View Plate Metadata"),
        tags$div(style = "display:none;",
                 textInput("read_import_plate_id", label = NULL, value = ""),
                 textInput("read_import_plate_number", label = NULL, value = "")
        ),
        uiOutput("plate_metadata_info"),
        uiOutput("plate_validated_status"),
        tableOutput("plate_validation_message_table")
      )
    } else {
      fluidRow(
        tagList(
          tags$p("Select a sheet containing raw bead array data."
          )
        )
      )
    }


  })


})

observe({
  cat("plate_id:", input$read_import_plate_id, "\n")
  cat("Plate Number:",input$read_import_plate_number, "\n" )
})

observeEvent(input$uploaded_sheet,{

  # Require to select sheet to read
  req(input$uploaded_sheet)

  if (input$uploaded_sheet != "") {

  plate_data(
    openxlsx::read.xlsx(inFile()$datapath, startRow = 8, sheet = input$uploaded_sheet)
  )

  header_info(
    openxlsx::read.xlsx(inFile()$datapath, rows = c(1:7), sheet = input$uploaded_sheet, colNames = F)
  )

  transform_dat <- plate_data()
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[,]", ".", x) }))
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[*]+", "", x) }))
  transform_dat <- data.frame(lapply(transform_dat, function(x) {  gsub("[.]+", ".", x) }))


  # remove trailing rows at the end of file that contain all NA or blank strings
  transform_dat <- transform_dat[!apply(
    transform_dat, 1,
    function(x) all(is.na(x) | trimws(x) == "" | trimws(x) == "NA")
  ), ]


  plate_data(transform_dat)

  # options(max.print = 1000000)
  # print(transform_dat)

  plate_data <- transform_dat
  meta_df <- parse_metadata_df(header_info())

  plate_validation_result <- plate_validation(plate_metadata = meta_df,
                                               plate_data = plate_data,
                                               blank_keyword = input$blank_keyword)

  # cat(plate_validation_result$is_valid)
  # cat(plate_validation_result$messages)
  plate_validation_messages(plate_validation_result$messages)
  is_valid_plate(plate_validation_result$is_valid)
  if (plate_validation_result$is_valid) {
     plate_data(plate_validation_result$updated_plate_data)
  }

  }


})

observeEvent(input$view_raw_file,{
  print("view raw file")
  showModal(
    modalDialog(
      title = "Raw File",
      rhandsontable::rHandsontableOutput("raw_file"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$raw_file <- rhandsontable::renderRHandsontable({
    rhandsontable(plate_data(), readOnly = TRUE)
  })

 # plate_data <<- plate_data()
  print(plate_data())
})

observeEvent(input$view_raw_header,{
  print("view raw header")
  showModal(
    modalDialog(
      title = "Plate Metadata",
      rhandsontable::rHandsontableOutput("raw_header_file"),
      size = "l",
      easyClose = TRUE
    )
  )
  output$raw_header_file <- rhandsontable::renderRHandsontable({
    rhandsontable(header_info(), readOnly = TRUE)
  })
 # platemetadata <<- header_info()
  print(header_info())
})

observeEvent(plate_data(), {
  print("New plate data loaded!")
  print(head(plate_data()))
  all_df <- create_list_of_dataframes(plate_data = plate_data(), study_accession = input$readxMap_study_accession,
                            experiment_accession = input$readxMap_experiment_accession_import)
  list_of_dataframes(all_df)
  combined_df <- create_original_df_combined(plate_data = plate_data(), list_of_dataframes = all_df)
  original_df_combined(combined_df)
})

observe({
  cat("type_x_status current value:\n")
  print(type_x_status())
})

observeEvent(all_completed(), {
    cat("all completed:")
    print(all_completed())

    if (all_completed()) {
      optimization_ready(TRUE)
    } else {
      optimization_ready(FALSE)
    }
  }, ignoreInit = TRUE)

observeEvent(input$split_opt_plates, {
  split_optimization_single_upload(study_accession = input$readxMap_study_accession, experiment_accession = input$readxMap_experiment_accession_import,
                                   plate_id = input$read_import_plate_id,
                                   plate_number = input$read_import_plate_number)

  # trigger refresh
  optimization_refresh(optimization_refresh() + 1)
})

observeEvent(input$savexMapButton, {

  # Standardize acquisition_date to ISO format for PostgreSQL
  if ("acquisition_date" %in% names(theads)) {
    theads$acquisition_date <- standardize_date_for_postgres(theads$acquisition_date)
  }

  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_header"), theads)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_standard"), standard_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_control"), control_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_buffer"), buffer_data)
  DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_sample"), sample_data)

  removeTab(inputId = "body_panel_id", target="previewxMap")
  removeTab(inputId = "body_panel_id", target="headerxMap")
  removeTab(inputId = "body_panel_id", target="readxMap")

  select_query <- glue::glue_sql("
    SELECT DISTINCT
      xmap_header.study_accession,
      xmap_header.experiment_accession,
      xmap_header.study_accession AS study_name,
      xmap_header.experiment_accession AS experiment_name,
      xmap_header.workspace_id,
      xmap_users.project_name
    FROM madi_results.xmap_header
    JOIN madi_results.xmap_users ON xmap_header.workspace_id = xmap_users.workspace_id
    WHERE xmap_header.workspace_id = {userWorkSpaceID()}
  ;", .con = conn)

  query_result <- dbGetQuery(conn, select_query)
  reactive_df_study_exp(query_result)
  print("reactive_df_study_exp:loaded")

  initial_source <- obtain_initial_source(input$readxMap_study_accession)

  # Initialize study parameters for a user and study
  study_user_params_nrow <- nrow(fetch_study_configuration(study_accession = input$readxMap_study_accession
                                                             , user = currentuser()))
  if (study_user_params_nrow == 0) {
    intitialize_study_configurations(study_accession = input$readxMap_study_accession,
                                     user = currentuser(), initial_source = initial_source)
  }
})

observeEvent(input$upload_experiment_files, {
  req(input$upload_experiment_files)
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)


  # LOGGING
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         NEW EXPERIMENT FILES UPLOAD                      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat("Study:", input$readxMap_study_accession, "\n")
  cat("Experiment:", input$readxMap_experiment_accession_import, "\n")
  cat("Files to upload:", nrow(input$upload_experiment_files), "\n\n")

  # RESET ALL BATCH REACTIVES
  if (!is.null(batch_plate_data())) {
    cat("⚠️  Clearing existing data...\n")
  }

  batch_plate_data(NULL)
  batch_metadata(NULL)
  bead_array_header_list(list())
  bead_array_plate_list(list())
  layout_template_sheets(list())
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))
  description_status(list(
    has_content = FALSE,
    has_sufficient_elements = FALSE,
    min_elements_found = 0,
    required_elements = 3,
    checked = FALSE,
    message = ""
  ))


  # SINGLE-PASS FILE PROCESSING

  tryCatch({
    processed <- process_experiment_files(input$upload_experiment_files)

    # Store results in reactives
    bead_array_header_list(processed$header_list)
    bead_array_plate_list(processed$plate_list)
    batch_plate_data(processed$combined_plates)

    # For debugging (remove in production)
    #all_plates_v <<- processed$combined_plates


    # CHECK DESCRIPTION FIELD

    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║         CHECKING DESCRIPTION FIELD                       ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")

    delimiter <- input$description_delimiter %||% "_"
    desc_status <- check_and_report_description(
      processed$combined_plates,
      delimiter,
      required_elements = 3
    )
    description_status(desc_status)

    # Show warnings if needed
    if (!desc_status$has_content) {
      showNotification(
        "Description field is blank. Default values will be used.",
        type = "warning", duration = 10
      )
    } else if (!desc_status$has_sufficient_elements) {
      showNotification(
        sprintf("Description has insufficient elements (%d found, %d required).",
                desc_status$min_elements_found, desc_status$required_elements),
        type = "warning", duration = 10
      )
    }


    # LOG ANTIGENS

    metadata_cols <- c("source_file", "Well", "Type", "Description",
                       "% Agg Beads", "Sampling Errors", "Acquisition Time", "plateid")
    antigen_cols <- setdiff(names(processed$combined_plates), metadata_cols)
    cat("\nAntigens detected (", length(antigen_cols), "):\n", sep = "")
    for (ag in antigen_cols) cat("  •", ag, "\n")


    # SUCCESS

    cat("\n✓ Upload complete!\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")

    showNotification(
      paste("Successfully uploaded", nrow(input$upload_experiment_files), "experiment file(s)"),
      type = "message", duration = 3
    )

  }, error = function(e) {
    cat("✗ Upload failed:", conditionMessage(e), "\n")
    showNotification(
      paste("Upload failed:", conditionMessage(e)),
      type = "error", duration = 10
    )
  })
})

observeEvent(input$description_delimiter, {
  req(batch_plate_data())

  all_plates <- batch_plate_data()
  delimiter <- input$description_delimiter

  # Re-check description elements with new delimiter
  desc_check <- check_description_elements(
    plate_data = all_plates,
    delimiter = delimiter,
    required_elements = 3
  )

  # Update the reactive
  description_status(list(
    has_content = desc_check$has_content,
    has_sufficient_elements = desc_check$has_sufficient_elements,
    min_elements_found = desc_check$min_elements_found,
    required_elements = desc_check$required_elements,
    checked = TRUE,
    message = desc_check$message
  ))
}, ignoreInit = TRUE)

# upload_layout_file observer (sourcing ALL data from layout file)
# ASSUMPTIONS:
# 1. The layout template file contains an assay_response_long sheet
# 2. The layout template file contains all plate identifiers in plate_id sheet
# 3. The layout template file contains nominal_sample_dilution in plate_id and plates_map
#
# If assay_response_long is missing, we fall back to creating it from batch_plate_data()
# but emit a warning that the layout file is incomplete.

observeEvent(input$upload_layout_file, {
  req(input$readxMap_study_accession)
  req(input$readxMap_experiment_accession_import)

  project_id <- userWorkSpaceID()
  study_accession <- input$readxMap_study_accession
  experiment_accession <- input$readxMap_experiment_accession_import
  workspace_id <- userWorkSpaceID()
  current_user <- currentuser()

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         UPLOADING LAYOUT FILE (REFACTORED v2)            ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")


  # RESET STATE

  inLayoutFile(NULL)
  avaliableLayoutSheets(NULL)
  layout_template_sheets(list())

  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))

  input_upload_layout_file <- input$upload_layout_file
  inLayoutFile(input_upload_layout_file)

  if (is.null(input_upload_layout_file)) {
    cat("⚠️  No layout file provided\n")
    return()
  }

  cat("Layout file:", input_upload_layout_file$name, "\n\n")


  # READ LAYOUT FILE

  cat("Reading layout file...\n")
  sheets <- readxl::excel_sheets(input_upload_layout_file$datapath)
  cat("  → Excel sheets found:", paste(sheets, collapse=", "), "\n")

  validation <- check_sheet_names(input_upload_layout_file$datapath, exact_match = FALSE)
  if (!validation$valid) {
    showNotification(validation$message, type = "error", duration = NULL)
    return()
  }

  all_sheets <- import_layout_file(input_upload_layout_file$datapath)

  if (!all_sheets$success) {
    showNotification(paste(all_sheets$messages, collapse = "\n"), type = "error", duration = NULL)
    return()
  }

  cat("  ✓ Layout sheets read successfully\n")
  cat("  → Imported sheet names:", paste(names(all_sheets$data), collapse=", "), "\n\n")


  # VALIDATE REQUIRED SHEETS

  required_sheets <- c("plates_map", "plate_id", "antigen_list")
  missing_required <- setdiff(required_sheets, names(all_sheets$data))

  if (length(missing_required) > 0) {
    cat("⚠️  ERROR: Missing required sheets:", paste(missing_required, collapse=", "), "\n")
    showNotification(
      paste("Missing required sheets:", paste(missing_required, collapse=", ")),
      type = "error",
      duration = 10
    )
    return()
  }


  # EXTRACT DATA FROM LAYOUT FILE (not from raw data)

  plates_map <- all_sheets$data[["plates_map"]]
  plate_id_sheet <- all_sheets$data[["plate_id"]]
  antigen_list <- all_sheets$data[["antigen_list"]]
  subject_map <- all_sheets$data[["subject_groups"]]
  timepoint_map <- all_sheets$data[["timepoint"]]

  # Check for assay_response_long (the key sheet for self-contained workflow)
  has_assay_response_long <- "assay_response_long" %in% names(all_sheets$data)

  cat("\n=== LAYOUT FILE CONTENTS ===\n")
  cat("  plate_id:\n")
  cat("    → Rows:", nrow(plate_id_sheet), "\n")
  cat("    → Columns:", paste(names(plate_id_sheet), collapse=", "), "\n")
  cat("  plates_map:\n")
  cat("    → Rows:", nrow(plates_map), "\n")
  cat("    → Columns:", paste(names(plates_map), collapse=", "), "\n")
  cat("  antigen_list:\n")
  cat("    → Rows:", nrow(antigen_list), "\n")
  cat("    → Columns:", paste(names(antigen_list), collapse=", "), "\n")
  cat("  assay_response_long:", if(has_assay_response_long) "PRESENT ✓" else "MISSING ⚠", "\n")
  cat("============================\n\n")


  # VALIDATE REQUIRED COLUMNS IN LAYOUT FILE

  cat("Validating required columns...\n")

  # plate_id required columns
  required_plate_id_cols <- c("study_name", "experiment_name", "plate_number",
                              "plateid", "plate_id", "number_of_wells")
  missing_plate_id_cols <- setdiff(required_plate_id_cols, names(plate_id_sheet))
  if (length(missing_plate_id_cols) > 0) {
    cat("  ⚠️  plate_id missing columns:", paste(missing_plate_id_cols, collapse=", "), "\n")
  } else {
    cat("  ✓ plate_id has all required columns\n")
  }

  # plates_map required columns
  required_plates_map_cols <- c("study_name", "experiment_name", "plate_number",
                                "well", "specimen_type", "plateid")
  missing_plates_map_cols <- setdiff(required_plates_map_cols, names(plates_map))
  if (length(missing_plates_map_cols) > 0) {
    cat("  ⚠️  plates_map missing columns:", paste(missing_plates_map_cols, collapse=", "), "\n")
  } else {
    cat("  ✓ plates_map has all required columns\n")
  }

  # antigen_list required columns
  required_antigen_cols <- c("antigen_label_on_plate", "antigen_abbreviation")
  missing_antigen_cols <- setdiff(required_antigen_cols, names(antigen_list))
  if (length(missing_antigen_cols) > 0) {
    cat("  ⚠️  antigen_list missing columns:", paste(missing_antigen_cols, collapse=", "), "\n")
  } else {
    cat("  ✓ antigen_list has all required columns\n")
  }


  # ADD PROJECT_ID IF NOT PRESENT

  if (!"project_id" %in% names(plates_map)) {
    plates_map$project_id <- project_id
    cat("  → Added project_id to plates_map\n")
  }

  if (!"project_id" %in% names(plate_id_sheet)) {
    plate_id_sheet$project_id <- project_id
    cat("  → Added project_id to plate_id\n")
  }


  # CHECK/COMPUTE NOMINAL_SAMPLE_DILUTION (from layout file data only)

  cat("\n  Checking nominal_sample_dilution...\n")

  if ("nominal_sample_dilution" %in% names(plates_map) &&
      "nominal_sample_dilution" %in% names(plate_id_sheet)) {
    cat("    ✓ nominal_sample_dilution already present in layout file\n")
  } else {
    cat("    → Computing nominal_sample_dilution from plates_map...\n")

    # Compute from plates_map specimen_dilution_factor
    sample_rows <- plates_map[
      substr(plates_map$specimen_type, 1, 1) == "X" &
        !is.na(plates_map$specimen_dilution_factor),
    ]

    if (nrow(sample_rows) > 0) {
      nsd_df <- aggregate(
        specimen_dilution_factor ~ project_id + study_name + experiment_name + plate_number,
        data = sample_rows,
        FUN = function(x) paste(sort(unique(x)), collapse = "|")
      )
      names(nsd_df)[names(nsd_df) == "specimen_dilution_factor"] <- "nominal_sample_dilution"
    } else {
      unique_plates <- unique(plates_map[, c("project_id", "study_name", "experiment_name", "plate_number"), drop = FALSE])
      nsd_df <- unique_plates
      nsd_df$nominal_sample_dilution <- "1"
    }

    # Remove existing if present
    plates_map <- plates_map[, names(plates_map) != "nominal_sample_dilution", drop = FALSE]
    plate_id_sheet <- plate_id_sheet[, names(plate_id_sheet) != "nominal_sample_dilution", drop = FALSE]

    # Join to both sheets
    join_cols <- c("project_id", "study_name", "experiment_name", "plate_number")
    join_cols <- intersect(join_cols, names(plates_map))

    plates_map <- merge(plates_map, nsd_df, by = join_cols, all.x = TRUE)
    plates_map$nominal_sample_dilution[is.na(plates_map$nominal_sample_dilution)] <- "1"

    join_cols_plate_id <- intersect(join_cols, names(plate_id_sheet))
    plate_id_sheet <- merge(plate_id_sheet, nsd_df, by = join_cols_plate_id, all.x = TRUE)
    plate_id_sheet$nominal_sample_dilution[is.na(plate_id_sheet$nominal_sample_dilution)] <- "1"

    cat("    ✓ nominal_sample_dilution computed and added\n")
  }


  # CHECK/CREATE ASSAY_RESPONSE_LONG

  if (has_assay_response_long) {
    assay_response_long <- all_sheets$data[["assay_response_long"]]
    cat("  ✓ assay_response_long loaded from layout file (", nrow(assay_response_long), " rows)\n", sep="")

    # Validate required columns
    required_arl_cols <- c("plateid", "well", "antigen", "assay_response", "assay_bead_count")
    missing_arl_cols <- setdiff(required_arl_cols, names(assay_response_long))
    if (length(missing_arl_cols) > 0) {
      cat("    ⚠️  assay_response_long missing columns:", paste(missing_arl_cols, collapse=", "), "\n")
    }

  } else {
    # FALLBACK: Create from batch_plate_data() if available
    cat("\n  ⚠️  assay_response_long sheet NOT in layout file\n")
    cat("      Attempting to create from batch_plate_data()...\n")

    if (is.null(batch_plate_data())) {
      cat("      ✗ batch_plate_data() is NULL - cannot create assay_response_long\n")
      cat("      Please regenerate the layout template to include assay_response_long sheet\n")
      showNotification(
        "Layout file is missing assay_response_long sheet and no raw data available. Please regenerate the layout template.",
        type = "error",
        duration = 10
      )
      return()
    }

    combined_plates <- batch_plate_data()
    cat("      → batch_plate_data() has", nrow(combined_plates), "rows\n")

    # Build plate identifier lookup from plate_id sheet
    plate_identifiers <- plate_id_sheet[, c("plate_number", "plateid", "plate_id"), drop = FALSE]
    plate_identifiers <- unique(plate_identifiers)

    # Add source_file mapping if available
    if ("source_file" %in% names(combined_plates)) {
      source_files <- unique(combined_plates$source_file)
      # Try to match source_file to plateid
      source_to_plate <- data.frame(
        source_file = source_files,
        plateid = sapply(source_files, clean_plate_id),
        stringsAsFactors = FALSE
      )
      plate_identifiers <- merge(plate_identifiers, source_to_plate, by = "plateid", all.x = TRUE)
    }

    # Identify MFI columns
    all_cols <- names(combined_plates)
    mfi_cols <- grep("\\([0-9]+\\)", all_cols, value = TRUE)

    if (length(mfi_cols) > 0) {
      meta_cols <- intersect(c("source_file", "Well"), all_cols)

      # Pivot to long format
      assay_response_long <- tidyr::pivot_longer(
        combined_plates[, c(meta_cols, mfi_cols), drop = FALSE],
        cols = tidyselect::all_of(mfi_cols),
        names_to = "antigen_label_on_plate",
        values_to = "mfi_bead_combined"
      )

      # Clean and extract
      assay_response_long$antigen_label_on_plate <- gsub("\\.", " ", assay_response_long$antigen_label_on_plate)
      assay_response_long$assay_response <- as.numeric(
        stringr::str_extract(assay_response_long$mfi_bead_combined, "^[0-9.]+")
      )
      assay_response_long$assay_bead_count <- as.numeric(
        stringr::str_extract(assay_response_long$mfi_bead_combined, "(?<=\\()[0-9]+(?=\\))")
      )
      assay_response_long$mfi_bead_combined <- NULL

      # Rename Well
      if ("Well" %in% names(assay_response_long)) {
        names(assay_response_long)[names(assay_response_long) == "Well"] <- "well"
      }

      # Join plate identifiers
      if ("source_file" %in% names(assay_response_long) && "source_file" %in% names(plate_identifiers)) {
        assay_response_long <- merge(
          assay_response_long,
          plate_identifiers[, c("source_file", "plateid", "plate_id", "plate_number")],
          by = "source_file",
          all.x = TRUE
        )
      }

      # Join antigen abbreviation
      antigen_lookup <- unique(antigen_list[, c("antigen_label_on_plate", "antigen_abbreviation")])
      assay_response_long <- merge(assay_response_long, antigen_lookup, by = "antigen_label_on_plate", all.x = TRUE)
      names(assay_response_long)[names(assay_response_long) == "antigen_abbreviation"] <- "antigen"

      # Add context
      assay_response_long$project_id <- project_id
      assay_response_long$study_name <- study_accession
      assay_response_long$experiment_name <- experiment_accession

      cat("      ✓ Created assay_response_long with", nrow(assay_response_long), "rows\n")

    } else {
      cat("      ✗ No MFI columns found in batch_plate_data()\n")
      assay_response_long <- NULL
    }
  }


  # ANTIGEN ALIGNMENT CHECK (using layout file data)

  cat("\n  Checking antigen alignment...\n")

  layout_antigens <- unique(antigen_list$antigen_label_on_plate)
  cat("    → Layout antigens:", length(layout_antigens), "\n")

  if (!is.null(assay_response_long)) {
    response_antigens <- unique(assay_response_long$antigen_label_on_plate)

    extra_in_layout <- setdiff(layout_antigens, response_antigens)
    missing_from_layout <- setdiff(response_antigens, layout_antigens)

    if (length(extra_in_layout) > 0) {
      cat("    ⚠️  Layout has antigens NOT in response data:\n")
      for (ag in head(extra_in_layout, 5)) cat("        ✗", ag, "\n")
      if (length(extra_in_layout) > 5) cat("        ... and", length(extra_in_layout) - 5, "more\n")
    }

    if (length(missing_from_layout) > 0) {
      cat("    ⚠️  Response data has antigens NOT in layout:\n")
      for (ag in head(missing_from_layout, 5)) cat("        ✗", ag, "\n")
      if (length(missing_from_layout) > 5) cat("        ... and", length(missing_from_layout) - 5, "more\n")
    }

    if (length(extra_in_layout) == 0 && length(missing_from_layout) == 0) {
      cat("    ✓ All antigens align\n")
    }
  }


  # STORE ENHANCED SHEETS

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  STORING LAYOUT SHEETS                                   ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  all_sheets$data[["plates_map"]] <- plates_map
  all_sheets$data[["plate_id"]] <- plate_id_sheet

  if (!is.null(assay_response_long)) {
    all_sheets$data[["assay_response_long"]] <- assay_response_long
  }

  layout_template_sheets(all_sheets$data)

  cat("  ✓ layout_template_sheets() updated\n")
  cat("  → Contains", length(all_sheets$data), "sheets:", paste(names(all_sheets$data), collapse=", "), "\n")


  # PREPARE BATCH_METADATA (from plate_id sheet)

  batch_metadata_data <- plate_id_sheet
  batch_metadata_data$workspace_id <- workspace_id
  batch_metadata_data$currentuser <- current_user

  if ("plate_filename" %in% names(batch_metadata_data)) {
    names(batch_metadata_data)[names(batch_metadata_data) == "plate_filename"] <- "file_name"
  }

  cat("\n=== batch_metadata_data ===\n")
  cat("  Columns:", paste(names(batch_metadata_data), collapse = ", "), "\n")
  cat("  Rows:", nrow(batch_metadata_data), "\n")


  # VALIDATION

  cat("\nValidating...\n")

  # Metadata validation
  validate_metadata_result <- validate_batch_plate_metadata(
    plate_metadata = batch_metadata_data,
    plate_id_data = plate_id_sheet
  )

  # Bead array validation - use assay_response_long if available
  if (!is.null(assay_response_long)) {
    # Create a validation function that works with assay_response_long
    bead_array_validation <- validate_assay_response_data(
      assay_response_long = assay_response_long,
      antigen_import_list = antigen_list,
      plates_map = plates_map
    )
  } else if (!is.null(batch_plate_data())) {
    # Fallback to old validation
    bead_array_validation <- validate_batch_bead_array_data(
      combined_plate_data = batch_plate_data(),
      antigen_import_list = antigen_list,
      blank_keyword = input$batch_blank
    )
  } else {
    # No data to validate
    bead_array_validation <- list(
      is_valid = FALSE,
      message = "No assay response data available for validation"
    )
  }

  # Update validation state
  if (bead_array_validation$is_valid && validate_metadata_result$is_valid) {
    batch_validation_state(list(
      is_validated = TRUE,
      is_uploaded = FALSE,
      validation_time = Sys.time(),
      upload_time = NULL,
      metadata_result = validate_metadata_result,
      bead_array_result = bead_array_validation
    ))
    batch_metadata(batch_metadata_data)

    cat("✓ VALIDATION PASSED!\n")
    showNotification(
      "Layout file validated successfully! Ready to upload to database.",
      type = "message",
      duration = 5
    )
  } else {
    batch_validation_state(list(
      is_validated = FALSE,
      is_uploaded = FALSE,
      validation_time = Sys.time(),
      upload_time = NULL,
      metadata_result = validate_metadata_result,
      bead_array_result = bead_array_validation
    ))

    cat("✗ VALIDATION FAILED\n")
    cat("  Metadata valid:", validate_metadata_result$is_valid, "\n")
    cat("  Bead array valid:", bead_array_validation$is_valid, "\n")

    showNotification(
      "Layout file validation failed. Please review error messages.",
      type = "error",
      duration = 10
    )
  }

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
})

# validate_assay_response_data
# This validates the assay_response_long sheet against the antigen_list
# and plates_map, without needing the raw batch_plate_data()

validate_assay_response_data <- function(assay_response_long,
                                         antigen_import_list,
                                         plates_map) {

  result <- list(
    is_valid = TRUE,
    messages = c(),
    warnings = c()
  )

  # Check 1: assay_response_long is not empty
  if (is.null(assay_response_long) || nrow(assay_response_long) == 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages, "assay_response_long is empty or NULL")
    return(result)
  }

  # Check 2: Required columns present
  required_cols <- c("plateid", "well", "antigen", "assay_response")
  missing_cols <- setdiff(required_cols, names(assay_response_long))
  if (length(missing_cols) > 0) {
    result$is_valid <- FALSE
    result$messages <- c(result$messages,
                         paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check 3: All antigens in assay_response have a match in antigen_list
  response_antigens <- unique(assay_response_long$antigen_label_on_plate)
  layout_antigens <- unique(antigen_import_list$antigen_label_on_plate)

  missing_antigens <- setdiff(response_antigens, layout_antigens)
  if (length(missing_antigens) > 0) {
    result$warnings <- c(result$warnings,
                         paste(length(missing_antigens), "antigens in response data not in antigen_list"))
  }

  # Check 4: All wells in assay_response have a match in plates_map
  response_wells <- unique(paste(assay_response_long$plateid, assay_response_long$well, sep = "|"))
  plates_map_wells <- unique(paste(plates_map$plateid, plates_map$well, sep = "|"))

  missing_wells <- setdiff(response_wells, plates_map_wells)
  if (length(missing_wells) > 0) {
    result$warnings <- c(result$warnings,
                         paste(length(missing_wells), "well positions in response data not in plates_map"))
  }

  # Check 5: No NA values in critical columns
  na_antigen <- sum(is.na(assay_response_long$antigen))
  na_response <- sum(is.na(assay_response_long$assay_response))

  if (na_antigen > 0) {
    result$warnings <- c(result$warnings,
                         paste(na_antigen, "rows have NA antigen (missing mapping)"))
  }

  if (na_response > 0) {
    result$warnings <- c(result$warnings,
                         paste(na_response, "rows have NA assay_response"))
  }

  # Log results
  cat("  Validation results:\n")
  cat("    → Valid:", result$is_valid, "\n")
  if (length(result$messages) > 0) {
    cat("    → Errors:", paste(result$messages, collapse = "; "), "\n")
  }
  if (length(result$warnings) > 0) {
    cat("    → Warnings:", paste(result$warnings, collapse = "; "), "\n")
  }

  result
}

observeEvent(layout_template_sheets(), {
  cat("changed layout sheets")

})

observeEvent(input$view_layout_plate_id_sheet,{
  print("view layout plate id sheet")

  showModal(
    modalDialog(
      title = "Plate ID sheet ",
      rhandsontable::rHandsontableOutput("layout_plate_id_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_plate_id_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["plate_id"]])
    rhandsontable(layout_template_sheets()[["plate_id"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_subject_group_sheet, {
  showModal(
    modalDialog(
      title = "Subject Map",
      rhandsontable::rHandsontableOutput("layout_subject_groups_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_subject_groups_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["subject_groups"]])
    rhandsontable(layout_template_sheets()[["subject_groups"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_timepoint_sheet, {
  showModal(
    modalDialog(
      title = "Timepoint Map",
      rhandsontable::rHandsontableOutput("layout_timepoint_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_timepoint_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["timepoint"]])
    rhandsontable(layout_template_sheets()[["timepoint"]], readOnly = TRUE)
  })
})

observeEvent(input$view_layout_plates_map_sheet,{
  print("view layout plate map sheet")

  showModal(
    modalDialog(
      title = "Plate map sheet ",
      rhandsontable::rHandsontableOutput("layout_plates_map_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_plates_map_sheet <- rhandsontable::renderRHandsontable({
   req(layout_template_sheets()[["plates_map"]])
    rhandsontable(layout_template_sheets()[["plates_map"]], readOnly = TRUE)
  })

})

observeEvent(input$view_layout_antigen_list_sheet,{
  print("view layout antigen list sheet")

  showModal(
    modalDialog(
      title = "Antigen list sheet ",
      rhandsontable::rHandsontableOutput("layout_antigen_list_sheet"),
      size = "l",
      easyClose = TRUE
    )
  )

  output$layout_antigen_list_sheet <- rhandsontable::renderRHandsontable({
    req(layout_template_sheets()[["antigen_list"]])
    rhandsontable(layout_template_sheets()[["antigen_list"]], readOnly = TRUE)
  })

})

## Clear old data from layout when navigating between file formats
observeEvent(input$xPonentFile, {
  cat("Switched to", input$xPonentFile, "tab — clearing layout data\n")

  # Clear all layout-related reactive values
  inLayoutFile(NULL)
  avaliableLayoutSheets(NULL)
  layout_template_sheets(list())

  # Reset validation state
  batch_validation_state(list(
    is_validated = FALSE,
    is_uploaded = FALSE,
    validation_time = NULL,
    upload_time = NULL,
    metadata_result = NULL,
    bead_array_result = NULL
  ))
})

observeEvent(input$upload_batch_button, {
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║         UPLOADING BATCH TO DATABASE                      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # ── VALIDATE STATE ───────────────────────────────────────────────────────
  validation_state <- batch_validation_state()
  if (!validation_state$is_validated) {
    showNotification(
      "Please upload and validate a layout file before uploading to the database.",
      type = "error", duration = 5
    )
    return(NULL)
  }
  if (validation_state$is_uploaded) {
    showNotification(
      "This batch has already been uploaded.",
      type = "warning", duration = 5
    )
    return(NULL)
  }

  # ── GET PRE-COMPUTED DATA FROM LAYOUT SHEETS ─────────────────────────────
  layout_sheets <- layout_template_sheets()
  if (is.null(layout_sheets[["assay_response_long"]])) {
    cat("⚠ assay_response_long sheet not found, cannot proceed\n")
    showNotification(
      "Error: Assay response data not prepared. Please re-upload layout file.",
      type = "error", duration = 10
    )
    return(NULL)
  }

  assay_response <- layout_sheets[["assay_response_long"]]
  plates_map     <- layout_sheets[["plates_map"]]
  plate_id_sheet <- layout_sheets[["plate_id"]]
  antigen_list   <- layout_sheets[["antigen_list"]]
  subject_map    <- layout_sheets[["subject_groups"]]
  timepoint_map  <- layout_sheets[["timepoint"]]

  # ── GET METADATA ──────────────────────────────────────────────────────────
  metadata_batch       <- batch_metadata()
  project_id           <- userWorkSpaceID()
  workspace_id         <- userWorkSpaceID()
  auth0_user           <- currentuser()
  study_accession      <- unique(plates_map$study_name)[1]
  experiment_accession <- unique(plates_map$experiment_name)[1]

  cat("  Study:", study_accession, "\n")
  cat("  Experiment:", experiment_accession, "\n")
  cat("  assay_response rows:", nrow(assay_response), "\n")
  cat("  plates_map rows:", nrow(plates_map), "\n")

  # ── DEBUGGING MERGE KEY ALIGNMENT ────────────────────────────────────────
  cat("\n=== DEBUGGING MERGE KEY ALIGNMENT ===\n")
  cat("  plates_map columns:", paste(names(plates_map), collapse = ", "), "\n")
  cat("  assay_response columns:", paste(names(assay_response), collapse = ", "), "\n")
  key_cols <- c("project_id", "study_name", "experiment_name", "plateid", "well")
  cat("\n  Checking key columns:\n")
  for (col in key_cols) {
    pm_has <- col %in% names(plates_map)
    ar_has <- col %in% names(assay_response)
    cat("    ", col, ": plates_map=", pm_has, ", assay_response=", ar_has, "\n", sep = "")
    if (pm_has && ar_has) {
      pm_vals <- unique(plates_map[[col]])
      ar_vals <- unique(assay_response[[col]])
      if (length(pm_vals) <= 5 && length(ar_vals) <= 5) {
        cat("      plates_map values: ",    paste(pm_vals, collapse = ", "), "\n", sep = "")
        cat("      assay_response values: ", paste(ar_vals, collapse = ", "), "\n", sep = "")
      } else {
        cat("      plates_map unique count: ",    length(pm_vals), "\n", sep = "")
        cat("      assay_response unique count: ", length(ar_vals), "\n", sep = "")
      }
      matches <- intersect(pm_vals, ar_vals)
      cat("      Matching values: ", length(matches), "/", length(pm_vals), "\n", sep = "")
    }
  }
  cat("===========================================\n\n")

  # ── CHECK FOR EXISTING PLATES ─────────────────────────────────────────────
  plate_ids <- unique(plate_id_sheet$plate_id)
  existing_plates <- check_existing_plates(
    conn             = conn,
    project_id       = project_id,
    study_accession  = study_accession,
    experiment_accession = experiment_accession,
    plateids         = plate_ids
  )
  if (nrow(existing_plates) > 0) {
    cat("⚠ Plates already exist:\n")
    print(existing_plates)
    showNotification(
      "These plates already exist for this study and experiment.",
      type = "warning", duration = 5
    )
    return(NULL)
  }

  # ── COLUMN MAPPING ────────────────────────────────────────────────────────
  col_mapping  <- create_column_mapping()
  natural_key  <- c("study_name", "experiment_name", "plateid", "well")

  # Helper: fill missing sampleid values that would cause NOT NULL violations
  fill_missing_sampleid <- function(df, specimen_type = c("X", "S", "C")) {
    specimen_type <- match.arg(specimen_type)
    if (!"sampleid" %in% names(df)) df$sampleid <- NA_character_
    needs_fill <- is.na(df$sampleid) | trimws(df$sampleid) == ""
    if (!any(needs_fill)) return(df)
    if (specimen_type == "X") {
      if ("patientid" %in% names(df))
        df$sampleid[needs_fill] <- df$patientid[needs_fill]
    } else {
      if ("dilution" %in% names(df))
        df$sampleid[needs_fill] <- as.character(df$dilution[needs_fill])
    }
    still_empty <- is.na(df$sampleid) | trimws(df$sampleid) == ""
    if (any(still_empty) && "well" %in% names(df))
      df$sampleid[still_empty] <- df$well[still_empty]
    return(df)
  }

  # ── PREPARE HEADER ────────────────────────────────────────────────────────
  cat("\n  Preparing header data...\n")
  header_data <- plate_id_sheet
  header_data$workspace_id                <- workspace_id
  header_data$auth0_user                  <- auth0_user
  header_data$assay_response_variable     <- "mfi"
  header_data$assay_independent_variable  <- "concentration"
  header_data <- apply_column_mapping(header_data, col_mapping)
  if ("plate_filename" %in% names(header_data))
    names(header_data)[names(header_data) == "plate_filename"] <- "file_name"
  header_cols <- c(
    "study_accession", "experiment_accession", "plate_id", "file_name",
    "acquisition_date", "reader_serial_number", "rp1_pmt_volts", "rp1_target",
    "auth0_user", "workspace_id", "plateid", "plate",
    "n_wells", "assay_response_variable", "assay_independent_variable",
    "nominal_sample_dilution", "project_id"
  )
  header_data <- header_data[, intersect(header_cols, names(header_data)), drop = FALSE]
  nk_cols     <- intersect(
    c("project_id", "study_accession", "experiment_accession",
      "plate_id", "nominal_sample_dilution"),
    names(header_data)
  )
  header_data <- header_data[!duplicated(header_data[, nk_cols, drop = FALSE]), ]
  cat("    → Header rows:", nrow(header_data), "\n")

  # ── PREPARE SAMPLES (X) ───────────────────────────────────────────────────
  cat("  Preparing sample data...\n")
  sample_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "X"), ]
  if (nrow(sample_map) > 0 && !is.null(subject_map)) {
    sample_map <- merge(
      sample_map, subject_map,
      by = c("study_name", "subject_id"), all.x = TRUE
    )
    sample_map$agroup <- ifelse(
      is.na(sample_map$groupb),
      sample_map$groupa,
      paste(sample_map$groupa, sample_map$groupb, sep = "_")
    )
  }
  if (nrow(sample_map) > 0) {
    assay_cols        <- intersect(
      c(natural_key, "antigen", "assay_response", "assay_bead_count"),
      names(assay_response)
    )
    samples_to_upload <- merge(
      sample_map,
      assay_response[, assay_cols, drop = FALSE],
      by = natural_key, all.x = TRUE
    )
    if (!"plate_id" %in% names(samples_to_upload) &&
        "plate_id" %in% names(plate_id_sheet)) {
      pid_lookup        <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
      samples_to_upload <- merge(samples_to_upload, pid_lookup,
                                 by = "plateid", all.x = TRUE)
    }
    samples_to_upload <- apply_column_mapping(samples_to_upload, col_mapping)
    samples_to_upload <- fill_missing_sampleid(samples_to_upload, "X")
    sample_cols       <- c(
      "project_id", "study_accession", "experiment_accession", "timeperiod",
      "patientid", "well", "stype", "sampleid", "agroup", "dilution",
      "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
      "feature", "plate", "nominal_sample_dilution", "plateid", "plate_id"
    )
    samples_to_upload <- samples_to_upload[,
                                           intersect(sample_cols, names(samples_to_upload)), drop = FALSE]
    cat("    → Sample rows:", nrow(samples_to_upload), "\n")
  } else {
    samples_to_upload <- NULL
    cat("    → No samples found\n")
  }

  # ── PREPARE STANDARDS (S) ─────────────────────────────────────────────────
  cat("  Preparing standard data...\n")
  standard_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "S"), ]
  if (nrow(standard_map) > 0) {
    assay_cols          <- intersect(
      c(natural_key, "antigen", "assay_response", "assay_bead_count"),
      names(assay_response)
    )
    standards_to_upload <- merge(
      standard_map,
      assay_response[, assay_cols, drop = FALSE],
      by = natural_key, all.x = TRUE
    )
    if (!"plate_id" %in% names(standards_to_upload) &&
        "plate_id" %in% names(plate_id_sheet)) {
      pid_lookup          <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
      standards_to_upload <- merge(standards_to_upload, pid_lookup,
                                   by = "plateid", all.x = TRUE)
    }
    standards_to_upload <- apply_column_mapping(standards_to_upload, col_mapping)
    standards_to_upload <- fill_missing_sampleid(standards_to_upload, "S")
    standard_cols       <- c(
      "project_id", "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature",
      "plateid", "nominal_sample_dilution", "plate"
    )
    standards_to_upload <- standards_to_upload[,
                                               intersect(standard_cols, names(standards_to_upload)), drop = FALSE]
    cat("    → Standard rows:", nrow(standards_to_upload), "\n")
  } else {
    standards_to_upload <- NULL
    cat("    → No standards found\n")
  }

  # ── PREPARE BLANKS (B) ────────────────────────────────────────────────────
  cat("  Preparing blank data...\n")
  blank_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "B"), ]
  if (nrow(blank_map) > 0) {
    assay_cols       <- intersect(
      c(natural_key, "antigen", "assay_response", "assay_bead_count"),
      names(assay_response)
    )
    blanks_to_upload <- merge(
      blank_map,
      assay_response[, assay_cols, drop = FALSE],
      by = natural_key, all.x = TRUE
    )
    if (!"plate_id" %in% names(blanks_to_upload) &&
        "plate_id" %in% names(plate_id_sheet)) {
      pid_lookup       <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
      blanks_to_upload <- merge(blanks_to_upload, pid_lookup,
                                by = "plateid", all.x = TRUE)
    }
    blanks_to_upload <- apply_column_mapping(blanks_to_upload, col_mapping)
    blank_cols       <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
      "plateid", "nominal_sample_dilution", "plate"
    )
    blanks_to_upload <- blanks_to_upload[,
                                         intersect(blank_cols, names(blanks_to_upload)), drop = FALSE]
    cat("    → Blank rows:", nrow(blanks_to_upload), "\n")
  } else {
    blanks_to_upload <- NULL
    cat("    → No blanks found\n")
  }

  # ── PREPARE CONTROLS (C) ──────────────────────────────────────────────────
  cat("  Preparing control data...\n")
  control_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "C"), ]
  if (nrow(control_map) > 0) {
    assay_cols         <- intersect(
      c(natural_key, "antigen", "assay_response", "assay_bead_count"),
      names(assay_response)
    )
    controls_to_upload <- merge(
      control_map,
      assay_response[, assay_cols, drop = FALSE],
      by = natural_key, all.x = TRUE
    )
    if (!"plate_id" %in% names(controls_to_upload) &&
        "plate_id" %in% names(plate_id_sheet)) {
      pid_lookup         <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
      controls_to_upload <- merge(controls_to_upload, pid_lookup,
                                  by = "plateid", all.x = TRUE)
    }
    controls_to_upload <- apply_column_mapping(controls_to_upload, col_mapping)
    controls_to_upload <- fill_missing_sampleid(controls_to_upload, "C")
    control_cols       <- c(
      "study_accession", "experiment_accession", "plate_id", "well",
      "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors",
      "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
      "plateid", "nominal_sample_dilution", "plate"
    )
    controls_to_upload <- controls_to_upload[,
                                             intersect(control_cols, names(controls_to_upload)), drop = FALSE]
    cat("    → Control rows:", nrow(controls_to_upload), "\n")
  } else {
    controls_to_upload <- NULL
    cat("    → No controls found\n")
  }

  # ── PREPARE ANTIGEN FAMILY ────────────────────────────────────────────────
  cat("  Preparing antigen family data...\n")
  antigen_cols_needed <- c(
    "project_id", "study_name", "experiment_name", "antigen_abbreviation",
    "antigen_family", "standard_curve_max_concentration", "antigen_name",
    "virus_bacterial_strain", "antigen_source", "catalog_number",
    "l_asy_min_constraint", "l_asy_max_constraint", "l_asy_constraint_method"
  )
  antigens_to_upload <- antigen_list[,
                                     intersect(antigen_cols_needed, names(antigen_list)), drop = FALSE]
  if ("standard_curve_max_concentration" %in% names(antigens_to_upload))
    names(antigens_to_upload)[
      names(antigens_to_upload) == "standard_curve_max_concentration"
    ] <- "standard_curve_concentration"
  antigens_to_upload <- apply_column_mapping(antigens_to_upload, col_mapping)
  cat("    → Antigen rows:", nrow(antigens_to_upload), "\n")

  # ── PREPARE PLANNED VISITS ────────────────────────────────────────────────
  cat("  Preparing planned visits data...\n")
  visits_to_upload <- timepoint_map
  if (!is.null(visits_to_upload)) {
    names(visits_to_upload)[
      names(visits_to_upload) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
    names(visits_to_upload)[names(visits_to_upload) == "tissue_type"]    <- "type"
    names(visits_to_upload)[names(visits_to_upload) == "tissue_subtype"] <- "subtype"
    names(visits_to_upload)[names(visits_to_upload) == "description"]    <- "end_rule"
    names(visits_to_upload)[
      names(visits_to_upload) == "min_time_since_day_0"] <- "min_start_day"
    names(visits_to_upload)[
      names(visits_to_upload) == "max_time_since_day_0"] <- "max_start_day"
    visits_to_upload <- apply_column_mapping(visits_to_upload, col_mapping)
    cat("    → Visit rows:", nrow(visits_to_upload), "\n")
  } else {
    cat("    → No visits found\n")
  }

  # ════════════════════════════════════════════════════════════════════════════
  # INSERT INTO DATABASE
  # ════════════════════════════════════════════════════════════════════════════
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║  INSERTING DATA INTO DATABASE                            ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  result <- list(
    success        = FALSE,
    already_exists = FALSE,
    counts         = list(header = 0, samples = 0, standards = 0,
                          blanks = 0, controls = 0, antigens = 0,
                          visits = 0, curves = 0),   # curves added
    errors         = list(),
    message        = ""
  )

  # ── Header ────────────────────────────────────────────────────────────────
  if (!is.null(header_data) && nrow(header_data) > 0) {
    cat("  Inserting header...\n")
    if ("acquisition_date" %in% names(header_data))
      header_data$acquisition_date <- standardize_date_for_postgres(
        header_data$acquisition_date)
    header_result <- insert_to_table(
      conn, "madi_results", "xmap_header", header_data, "header",
      required_cols = c("project_id", "study_accession", "plate_id")
    )
    result$counts$header <- header_result$rows_inserted
    if (!header_result$success) {
      result$errors$header <- header_result$message
      result$message <- "Failed to upload header"
      cat("    ✗ Header insert failed:", header_result$message, "\n")
    } else {
      cat("    ✓ Header inserted:", header_result$rows_inserted, "rows\n")
    }
  }

  # ── Samples ───────────────────────────────────────────────────────────────
  if (!is.null(samples_to_upload) && nrow(samples_to_upload) > 0 &&
      length(result$errors) == 0) {
    cat("  Inserting samples...\n")
    sample_result <- insert_to_table(
      conn, "madi_results", "xmap_sample", samples_to_upload, "sample",
      required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
    )
    result$counts$samples <- sample_result$rows_inserted
    if (!sample_result$success) {
      result$errors$samples <- sample_result$message
      cat("    ✗ Sample insert failed:", sample_result$message, "\n")
    } else {
      cat("    ✓ Samples inserted:", sample_result$rows_inserted, "rows\n")
    }
  }

  # ── Standards ─────────────────────────────────────────────────────────────
  if (!is.null(standards_to_upload) && nrow(standards_to_upload) > 0) {
    cat("  Inserting standards...\n")
    standard_result <- insert_to_table(
      conn, "madi_results", "xmap_standard", standards_to_upload, "standard",
      required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
    )
    result$counts$standards <- standard_result$rows_inserted
    if (!standard_result$success) {
      result$errors$standards <- standard_result$message
      cat("    ✗ Standard insert failed:", standard_result$message, "\n")
    } else {
      cat("    ✓ Standards inserted:", standard_result$rows_inserted, "rows\n")
    }

    # ── Register new curve combinations in curve_lookup ──────────────────
    # Placed immediately after standards are committed.
    # standards_to_upload is already fully column-mapped at this point:
    #   study_accession, experiment_accession, plateid, plate,
    #   nominal_sample_dilution, source, antigen, feature are all present.
    # Non-fatal: a curve_lookup failure never blocks or rolls back the upload.
    if (standard_result$success) {
      cat("  Registering curves in curve_lookup...\n")
      tryCatch({
        cl_result <- register_curve_lookup(
          conn         = conn,
          standards_df = standards_to_upload,
          project_id   = workspace_id
        )
        result$counts$curves <- cl_result$rows_inserted
        if (cl_result$success) {
          cat("    ✓ curve_lookup:", cl_result$message, "\n")
        } else {
          cat("    ⚠ curve_lookup warning:", cl_result$message, "\n")
          showNotification(
            paste("curve_lookup warning:", cl_result$message),
            type = "warning", duration = 8
          )
        }
      }, error = function(e_cl) {
        # Fully isolated — curve_lookup error must never surface as an
        # upload failure because the standard rows are already committed.
        cat("    ⚠ curve_lookup non-fatal error:", conditionMessage(e_cl), "\n")
        showNotification(
          paste("curve_lookup (non-fatal):", conditionMessage(e_cl)),
          type = "warning", duration = 8
        )
      })
    }
    # ── End curve_lookup registration ─────────────────────────────────────
  }

  # ── Blanks ────────────────────────────────────────────────────────────────
  if (!is.null(blanks_to_upload) && nrow(blanks_to_upload) > 0) {
    cat("  Inserting blanks...\n")
    blank_result <- insert_to_table(
      conn, "madi_results", "xmap_buffer", blanks_to_upload, "blank",
      required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
    )
    result$counts$blanks <- blank_result$rows_inserted
    if (!blank_result$success) {
      result$errors$blanks <- blank_result$message
      cat("    ✗ Blank insert failed:", blank_result$message, "\n")
    } else {
      cat("    ✓ Blanks inserted:", blank_result$rows_inserted, "rows\n")
    }
  }

  # ── Controls ──────────────────────────────────────────────────────────────
  if (!is.null(controls_to_upload) && nrow(controls_to_upload) > 0) {
    cat("  Inserting controls...\n")
    control_result <- insert_to_table(
      conn, "madi_results", "xmap_control", controls_to_upload, "control",
      required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
    )
    result$counts$controls <- control_result$rows_inserted
    if (!control_result$success) {
      result$errors$controls <- control_result$message
      cat("    ✗ Control insert failed:", control_result$message, "\n")
    } else {
      cat("    ✓ Controls inserted:", control_result$rows_inserted, "rows\n")
    }
  }

  # ── Antigens ──────────────────────────────────────────────────────────────
  if (!is.null(antigens_to_upload) && nrow(antigens_to_upload) > 0) {
    cat("  Inserting antigens...\n")
    existing_antigens <- get_existing_antigens(conn, study_accession, experiment_accession)
    result$counts$antigens <- insert_new_rows(
      conn, "madi_results", "xmap_antigen_family",
      new_data      = antigens_to_upload,
      existing_data = existing_antigens,
      join_keys     = c("study_accession", "experiment_accession", "antigen"),
      label         = "antigen family"
    )
    cat("    ✓ Antigens inserted:", result$counts$antigens, "rows\n")
  }

  # ── Planned visits ────────────────────────────────────────────────────────
  if (!is.null(visits_to_upload) && nrow(visits_to_upload) > 0) {
    cat("  Inserting visits...\n")
    existing_visits <- get_existing_visits(conn, study_accession)
    result$counts$visits <- insert_new_rows(
      conn, "madi_results", "xmap_planned_visit",
      new_data      = visits_to_upload,
      existing_data = existing_visits,
      join_keys     = c("study_accession", "timepoint_name"),
      label         = "planned visit"
    )
    cat("    ✓ Visits inserted:", result$counts$visits, "rows\n")
  }

  # ── FINALISE RESULT ───────────────────────────────────────────────────────
  # curve_lookup is explicitly excluded from the fatal error check —
  # data is already committed by the time curve_lookup runs.
  fatal_errors   <- result$errors[!names(result$errors) %in% "curve_lookup"]
  result$success <- length(fatal_errors) == 0
  result$message <- if (result$success) {
    "Batch uploaded successfully"
  } else {
    paste("Upload completed with errors:", paste(names(fatal_errors), collapse = ", "))
  }

  # ── Update validation state ───────────────────────────────────────────────
  if (result$success) {
    current_state <- batch_validation_state()
    batch_validation_state(list(
      is_validated    = current_state$is_validated,
      is_uploaded     = TRUE,
      validation_time = current_state$validation_time,
      upload_time     = Sys.time(),
      metadata_result = current_state$metadata_result,
      bead_array_result = current_state$bead_array_result
    ))
  }

  # ── Notifications ─────────────────────────────────────────────────────────
  if (result$success) {
    counts         <- result$counts
    detail_message <- sprintf(
      "Uploaded: %d headers, %d samples, %d standards, %d blanks, %d controls, %d antigens, %d visits, %d curves registered",
      counts$header, counts$samples, counts$standards,
      counts$blanks, counts$controls, counts$antigens,
      counts$visits, counts$curves
    )
    showNotification("Batch Uploaded Successfully", type = "message", duration = 5)
    showNotification(detail_message,               type = "message", duration = 10)
    cat("\n✓ UPLOAD COMPLETE\n")
    cat(detail_message, "\n")
  } else {
    showNotification(
      paste("Upload failed:", result$message),
      type = "error", duration = NULL
    )
    for (error_name in names(fatal_errors)) {
      showNotification(
        paste(error_name, "error:", result$errors[[error_name]]),
        type = "error", duration = NULL
      )
    }
    cat("\n✗ UPLOAD FAILED\n")
    cat("Errors:", paste(names(fatal_errors), collapse = ", "), "\n")
  }

  cat("╚══════════════════════════════════════════════════════════╝\n\n")
})

# observeEvent(input$upload_batch_button, {
#
#   cat("\n╔══════════════════════════════════════════════════════════╗\n")
#   cat("║         UPLOADING BATCH TO DATABASE                      ║\n")
#   cat("╚══════════════════════════════════════════════════════════╝\n")
#
#
#   # VALIDATE STATE
#
#   validation_state <- batch_validation_state()
#
#   if (!validation_state$is_validated) {
#     showNotification(
#       "Please upload and validate a layout file before uploading to the database.",
#       type = "error",
#       duration = 5
#     )
#     return(NULL)
#   }
#
#   if (validation_state$is_uploaded) {
#     showNotification(
#       "This batch has already been uploaded.",
#       type = "warning",
#       duration = 5
#     )
#     return(NULL)
#   }
#
#
#   # GET PRE-COMPUTED DATA FROM LAYOUT SHEETS
#
#   layout_sheets <- layout_template_sheets()
#
#   # Verify required sheets
#   if (is.null(layout_sheets[["assay_response_long"]])) {
#     cat("⚠ assay_response_long sheet not found, cannot proceed\n")
#     showNotification(
#       "Error: Assay response data not prepared. Please re-upload layout file.",
#       type = "error",
#       duration = 10
#     )
#     return(NULL)
#   }
#
#   assay_response <- layout_sheets[["assay_response_long"]]
#   plates_map <- layout_sheets[["plates_map"]]
#   plate_id_sheet <- layout_sheets[["plate_id"]]
#   antigen_list <- layout_sheets[["antigen_list"]]
#   subject_map <- layout_sheets[["subject_groups"]]
#   timepoint_map <- layout_sheets[["timepoint"]]
#
#   # Get metadata
#   metadata_batch <- batch_metadata()
#   project_id <- userWorkSpaceID()
#   workspace_id <- userWorkSpaceID()
#   auth0_user <- currentuser()
#
#   # Extract study/experiment from plates_map
#   study_accession <- unique(plates_map$study_name)[1]
#   experiment_accession <- unique(plates_map$experiment_name)[1]
#
#   cat("  Study:", study_accession, "\n")
#   cat("  Experiment:", experiment_accession, "\n")
#   cat("  assay_response rows:", nrow(assay_response), "\n")
#   cat("  plates_map rows:", nrow(plates_map), "\n")
#
#   cat("\n=== DEBUGGING MERGE KEY ALIGNMENT ===\n")
#   cat("  plates_map columns:", paste(names(plates_map), collapse = ", "), "\n")
#   cat("  assay_response columns:", paste(names(assay_response), collapse = ", "), "\n")
#   # Check each key column
#   key_cols <- c("project_id", "study_name", "experiment_name", "plateid", "well")
#   cat("\n  Checking key columns:\n")
#   for (col in key_cols) {
#     pm_has <- col %in% names(plates_map)
#     ar_has <- col %in% names(assay_response)
#     cat("    ", col, ": plates_map=", pm_has, ", assay_response=", ar_has, "\n", sep="")
#     if (pm_has && ar_has) {
#       pm_vals <- unique(plates_map[[col]])
#       ar_vals <- unique(assay_response[[col]])
#       # For small sets, show values
#       if (length(pm_vals) <= 5 && length(ar_vals) <= 5) {
#         cat("      plates_map values: ", paste(pm_vals, collapse=", "), "\n", sep="")
#         cat("      assay_response values: ", paste(ar_vals, collapse=", "), "\n", sep="")
#       } else {
#         cat("      plates_map unique count: ", length(pm_vals), "\n", sep="")
#         cat("      assay_response unique count: ", length(ar_vals), "\n", sep="")
#       }
#       matches <- intersect(pm_vals, ar_vals)
#       cat("      Matching values: ", length(matches), "/", length(pm_vals), "\n", sep="")
#     }
#   }
#   cat("===========================================\n\n")
#
#   # CHECK FOR EXISTING PLATES
#   plate_ids <- unique(plate_id_sheet$plate_id)
#
#   existing_plates <- check_existing_plates(
#     conn = conn,
#     project_id = project_id,
#     study_accession = study_accession,
#     experiment_accession = experiment_accession,
#     plateids = plate_ids
#   )
#
#   if (nrow(existing_plates) > 0) {
#     cat("⚠ Plates already exist:\n")
#     print(existing_plates)
#     showNotification(
#       "These plates already exist for this study and experiment.",
#       type = "warning",
#       duration = 5
#     )
#     return(NULL)
#   }
#
#   # PREPARE COLUMN MAPPING
#   col_mapping <- create_column_mapping()
#   # Natural key for joining plates_map to assay_response
#   natural_key <- c("study_name", "experiment_name", "plateid", "well")
#
#   # Helper: fill missing sampleid values that would cause NOT NULL violations.
#   # biosample_id_barcode is derived from Type (e.g. "X145" → "145") but xPONENT
#   # Types often lack numeric suffixes ("X", "S1", "B"), so the barcode may be
#
#   # empty after the Excel round-trip.
#   fill_missing_sampleid <- function(df, specimen_type = c("X", "S", "C")) {
#     specimen_type <- match.arg(specimen_type)
#     if (!"sampleid" %in% names(df)) df$sampleid <- NA_character_
#
#     needs_fill <- is.na(df$sampleid) | trimws(df$sampleid) == ""
#
#     if (!any(needs_fill)) return(df)
#
#     if (specimen_type == "X") {
#       # Samples: use patientid (subject_id) — preserves replicate semantics
#       if ("patientid" %in% names(df)) {
#         df$sampleid[needs_fill] <- df$patientid[needs_fill]
#       }
#     } else {
#       # Standards / Controls: use dilution factor as identifier
#       if ("dilution" %in% names(df)) {
#         df$sampleid[needs_fill] <- as.character(df$dilution[needs_fill])
#       }
#     }
#
#     # Final fallback: use well position (always present, always non-null)
#     still_empty <- is.na(df$sampleid) | trimws(df$sampleid) == ""
#     if (any(still_empty) && "well" %in% names(df)) {
#       df$sampleid[still_empty] <- df$well[still_empty]
#     }
#
#     return(df)
#   }
#
#
#   # PREPARE HEADER DATA
#
#   cat("\n  Preparing header data...\n")
#
#   header_data <- plate_id_sheet
#   header_data$workspace_id <- workspace_id
#   header_data$auth0_user <- auth0_user
#   header_data$assay_response_variable <- "mfi"
#   header_data$assay_independent_variable <- "concentration"
#
#   # Apply column mapping
#   header_data <- apply_column_mapping(header_data, col_mapping)
#
#   # Rename specific columns for database
#   if ("plate_filename" %in% names(header_data)) {
#     names(header_data)[names(header_data) == "plate_filename"] <- "file_name"
#   }
#
#   # Select required columns
#   header_cols <- c(
#     "study_accession", "experiment_accession", "plate_id", "file_name",
#     "acquisition_date", "reader_serial_number", "rp1_pmt_volts", "rp1_target",
#     "auth0_user", "workspace_id", "plateid", "plate",
#     "n_wells", "assay_response_variable", "assay_independent_variable",
#     "nominal_sample_dilution", "project_id"
#   )
#   available_header_cols <- intersect(header_cols, names(header_data))
#   header_data <- header_data[, available_header_cols, drop = FALSE]
#
#   # Deduplicate
#   nk_cols <- intersect(c("project_id", "study_accession", "experiment_accession", "plate_id", "nominal_sample_dilution"), names(header_data))
#   header_data <- header_data[!duplicated(header_data[, nk_cols, drop = FALSE]), ]
#
#   cat("    → Header rows:", nrow(header_data), "\n")
#
#
#   # PREPARE SAMPLE DATA (X)
#   cat("  Preparing sample data...\n")
#
#   sample_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "X"), ]
#
#   if (nrow(sample_map) > 0 && !is.null(subject_map)) {
#     # Join subject_groups for groupa/groupb
#     sample_map <- merge(
#       sample_map,
#       subject_map,
#       by = c("study_name", "subject_id"),
#       all.x = TRUE
#     )
#     sample_map$agroup <- ifelse(
#       is.na(sample_map$groupb),
#       sample_map$groupa,
#       paste(sample_map$groupa, sample_map$groupb, sep = "_")
#     )
#   }
#
#   if (nrow(sample_map) > 0) {
#     # Join with assay_response - FIXED: removed antigen_label_on_plate
#     assay_cols <- intersect(
#       c(natural_key, "antigen", "assay_response", "assay_bead_count"),
#       names(assay_response)
#     )
#
#     samples_to_upload <- merge(
#       sample_map,
#       assay_response[, assay_cols, drop = FALSE],
#       by = natural_key,
#       all.x = TRUE
#     )
#
#     if (!"plate_id" %in% names(samples_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#       cat("    → Joining plate_id from plate_id_sheet...\n")
#       plate_id_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#       samples_to_upload <- merge(
#         samples_to_upload,
#         plate_id_lookup,
#         by = "plateid",
#         all.x = TRUE
#       )
#     }
#
#     # Apply column mapping
#     samples_to_upload <- apply_column_mapping(samples_to_upload, col_mapping)
#
#     # Fill missing sampleid (xPONENT Types lack numeric suffixes)
#     samples_to_upload <- fill_missing_sampleid(samples_to_upload, "X")
#
#     # Select required columns
#     sample_cols <- c(
#       "project_id", "study_accession", "experiment_accession", "timeperiod",
#       "patientid", "well", "stype", "sampleid", "agroup", "dilution",
#       "pctaggbeads", "samplingerrors", "antigen", "antibody_mfi", "antibody_n",
#       "feature", "plate", "nominal_sample_dilution", "plateid","plate_id"
#     )
#     available_sample_cols <- intersect(sample_cols, names(samples_to_upload))
#     samples_to_upload <- samples_to_upload[, available_sample_cols, drop = FALSE]
#
#     cat("    → Sample rows:", nrow(samples_to_upload), "\n")
#   } else {
#     samples_to_upload <- NULL
#     cat("    → No samples found\n")
#   }
#
#   # PREPARE STANDARD DATA (S)
#   cat("  Preparing standard data...\n")
#   standard_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "S"), ]
#
#   if (nrow(standard_map) > 0) {
#     # Join with assay_response
#     assay_cols <- intersect(
#       c(natural_key, "antigen", "assay_response", "assay_bead_count"),
#       names(assay_response)
#     )
#
#     standards_to_upload <- merge(
#       standard_map,
#       assay_response[, assay_cols, drop = FALSE],
#       by = natural_key,
#       all.x = TRUE
#     )
#
#     # Ensure plate_id is present
#     if (!"plate_id" %in% names(standards_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#       cat("    → Joining plate_id from plate_id_sheet...\n")
#       plate_id_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#       standards_to_upload <- merge(
#         standards_to_upload,
#         plate_id_lookup,
#         by = "plateid",
#         all.x = TRUE
#       )
#     }
#
#     # Apply column mapping
#     standards_to_upload <- apply_column_mapping(standards_to_upload, col_mapping)
#
#     # Fill missing sampleid (xPONENT Types lack numeric suffixes)
#     standards_to_upload <- fill_missing_sampleid(standards_to_upload, "S")
#
#     # Select required columns
#     standard_cols <- c(
#       "project_id", "study_accession", "experiment_accession", "plate_id", "well",
#       "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors",
#       "antigen", "antibody_mfi", "antibody_n", "feature",
#       "plateid", "nominal_sample_dilution", "plate"
#     )
#     available_standard_cols <- intersect(standard_cols, names(standards_to_upload))
#     standards_to_upload <- standards_to_upload[, available_standard_cols, drop = FALSE]
#
#     cat("    → Standard rows:", nrow(standards_to_upload), "\n")
#   } else {
#     standards_to_upload <- NULL
#     cat("    → No standards found\n")
#   }
#
#   # PREPARE BLANK DATA (B)
#   cat("  Preparing blank data...\n")
#   blank_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "B"), ]
#
#   if (nrow(blank_map) > 0) {
#     # Join with assay_response - FIXED: removed antigen_label_on_plate
#     assay_cols <- intersect(
#       c(natural_key, "antigen", "assay_response", "assay_bead_count"),
#       names(assay_response)
#     )
#
#     blanks_to_upload <- merge(
#       blank_map,
#       assay_response[, assay_cols, drop = FALSE],
#       by = natural_key,
#       all.x = TRUE
#     )
#
#     # ADDED: Ensure plate_id is present
#     if (!"plate_id" %in% names(blanks_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#       cat("    → Joining plate_id from plate_id_sheet...\n")
#       plate_id_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#       blanks_to_upload <- merge(
#         blanks_to_upload,
#         plate_id_lookup,
#         by = "plateid",
#         all.x = TRUE
#       )
#     }
#
#     blanks_to_upload <- apply_column_mapping(blanks_to_upload, col_mapping)
#
#     blank_cols <- c(
#       "study_accession", "experiment_accession", "plate_id", "well",
#       "stype", "dilution", "pctaggbeads", "samplingerrors",
#       "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
#       "plateid", "nominal_sample_dilution", "plate"
#     )
#     available_blank_cols <- intersect(blank_cols, names(blanks_to_upload))
#     blanks_to_upload <- blanks_to_upload[, available_blank_cols, drop = FALSE]
#
#     cat("    → Blank rows:", nrow(blanks_to_upload), "\n")
#   } else {
#     blanks_to_upload <- NULL
#     cat("    → No blanks found\n")
#   }
#
#
#   # PREPARE CONTROL DATA (C)
#   cat("  Preparing control data...\n")
#   control_map <- plates_map[which(substr(plates_map$specimen_type, 1, 1) == "C"), ]
#
#   if (nrow(control_map) > 0) {
#     # Join with assay_response - FIXED: removed antigen_label_on_plate
#     assay_cols <- intersect(
#       c(natural_key, "antigen", "assay_response", "assay_bead_count"),
#       names(assay_response)
#     )
#
#     controls_to_upload <- merge(
#       control_map,
#       assay_response[, assay_cols, drop = FALSE],
#       by = natural_key,
#       all.x = TRUE
#     )
#
#     if (!"plate_id" %in% names(controls_to_upload) && "plate_id" %in% names(plate_id_sheet)) {
#       cat("    → Joining plate_id from plate_id_sheet...\n")
#       plate_id_lookup <- unique(plate_id_sheet[, c("plateid", "plate_id"), drop = FALSE])
#       controls_to_upload <- merge(
#         controls_to_upload,
#         plate_id_lookup,
#         by = "plateid",
#         all.x = TRUE
#       )
#     }
#
#     controls_to_upload <- apply_column_mapping(controls_to_upload, col_mapping)
#
#     # Fill missing sampleid (xPONENT Types lack numeric suffixes)
#     controls_to_upload <- fill_missing_sampleid(controls_to_upload, "C")
#
#     control_cols <- c(
#       "study_accession", "experiment_accession", "plate_id", "well",
#       "stype", "sampleid", "source", "dilution", "pctaggbeads", "samplingerrors",
#       "antigen", "antibody_mfi", "antibody_n", "feature", "project_id",
#       "plateid", "nominal_sample_dilution", "plate"
#     )
#     available_control_cols <- intersect(control_cols, names(controls_to_upload))
#     controls_to_upload <- controls_to_upload[, available_control_cols, drop = FALSE]
#
#     cat("    → Control rows:", nrow(controls_to_upload), "\n")
#   } else {
#     controls_to_upload <- NULL
#     cat("    → No controls found\n")
#   }
#
#
#   # PREPARE ANTIGEN FAMILY DATA
#
#   cat("  Preparing antigen family data...\n")
#
#   antigen_cols_needed <- c(
#     "project_id", "study_name", "experiment_name", "antigen_abbreviation", "antigen_family",
#     "standard_curve_max_concentration", "antigen_name", "virus_bacterial_strain",
#     "antigen_source", "catalog_number", "l_asy_min_constraint",
#     "l_asy_max_constraint", "l_asy_constraint_method"
#   )
#   available_antigen_cols <- intersect(antigen_cols_needed, names(antigen_list))
#   antigens_to_upload <- antigen_list[, available_antigen_cols, drop = FALSE]
#
#   if ("standard_curve_max_concentration" %in% names(antigens_to_upload)) {
#     names(antigens_to_upload)[names(antigens_to_upload) == "standard_curve_max_concentration"] <- "standard_curve_concentration"
#   }
#
#   antigens_to_upload <- apply_column_mapping(antigens_to_upload, col_mapping)
#   cat("    → Antigen rows:", nrow(antigens_to_upload), "\n")
#
#
#   # PREPARE PLANNED VISITS DATA
#
#   cat("  Preparing planned visits data...\n")
#
#   visits_to_upload <- timepoint_map
#
#   if (!is.null(visits_to_upload)) {
#     names(visits_to_upload)[names(visits_to_upload) == "timepoint_tissue_abbreviation"] <- "timepoint_name"
#     names(visits_to_upload)[names(visits_to_upload) == "tissue_type"] <- "type"
#     names(visits_to_upload)[names(visits_to_upload) == "tissue_subtype"] <- "subtype"
#     names(visits_to_upload)[names(visits_to_upload) == "description"] <- "end_rule"
#     names(visits_to_upload)[names(visits_to_upload) == "min_time_since_day_0"] <- "min_start_day"
#     names(visits_to_upload)[names(visits_to_upload) == "max_time_since_day_0"] <- "max_start_day"
#
#     visits_to_upload <- apply_column_mapping(visits_to_upload, col_mapping)
#     cat("    → Visit rows:", nrow(visits_to_upload), "\n")
#   } else {
#     cat("    → No visits found\n")
#   }
#
#
#   # UPLOAD TO DATABASE
#
#   cat("\n╔══════════════════════════════════════════════════════════╗\n")
#   cat("║  INSERTING DATA INTO DATABASE                            ║\n")
#   cat("╚══════════════════════════════════════════════════════════╝\n")
#
#   result <- list(
#     success = FALSE,
#     already_exists = FALSE,
#     counts = list(header = 0, samples = 0, standards = 0, blanks = 0, controls = 0, antigens = 0, visits = 0),
#     errors = list(),
#     message = ""
#   )
#
#   # Insert header
#   if (!is.null(header_data) && nrow(header_data) > 0) {
#     cat("  Inserting header...\n")
#     # Standardize acquisition_date to ISO format for PostgreSQL
#     if ("acquisition_date" %in% names(header_data)) {
#       header_data$acquisition_date <- standardize_date_for_postgres(header_data$acquisition_date)
#     }
#     header_result <- insert_to_table(
#       conn, "madi_results", "xmap_header", header_data, "header",
#       required_cols = c("project_id", "study_accession", "plate_id")
#     )
#     result$counts$header <- header_result$rows_inserted
#
#     if (!header_result$success) {
#       result$errors$header <- header_result$message
#       result$message <- "Failed to upload header"
#       cat("    ✗ Header insert failed:", header_result$message, "\n")
#     } else {
#       cat("    ✓ Header inserted:", header_result$rows_inserted, "rows\n")
#     }
#   }
#
#   # Insert samples
#   if (!is.null(samples_to_upload) && nrow(samples_to_upload) > 0 && length(result$errors) == 0) {
#     cat("  Inserting samples...\n")
#     sample_result <- insert_to_table(
#       conn, "madi_results", "xmap_sample", samples_to_upload, "sample",
#       required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
#     )
#     result$counts$samples <- sample_result$rows_inserted
#
#     # ── register curve combinations
#     if (standard_result$success) {
#       cl_result <- register_curve_lookup(
#         conn        = conn,
#         standards_df = standards_to_upload,   # already in DB column names
#         project_id  = userWorkSpaceID()
#       )
#       if (!cl_result$success) {
#         # Non-fatal — log but don't block the upload
#         showNotification(
#           paste("curve_lookup warning:", cl_result$message),
#           type = "warning", duration = 8
#         )
#       } else {
#         cat("  curve_lookup:", cl_result$message, "\n")
#       }
#     }
#     # ── END register curve combinations
#
#     if (!sample_result$success) {
#       result$errors$samples <- sample_result$message
#       cat("    ✗ Sample insert failed:", sample_result$message, "\n")
#     } else {
#       cat("    ✓ Samples inserted:", sample_result$rows_inserted, "rows\n")
#     }
#   }
#
#   # Insert standards
#   if (!is.null(standards_to_upload) && nrow(standards_to_upload) > 0) {
#     cat("  Inserting standards...\n")
#     standard_result <- insert_to_table(
#       conn, "madi_results", "xmap_standard", standards_to_upload, "standard",
#       required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
#     )
#     result$counts$standards <- standard_result$rows_inserted
#
#     if (!standard_result$success) {
#       result$errors$standards <- standard_result$message
#       cat("    ✗ Standard insert failed:", standard_result$message, "\n")
#     } else {
#       cat("    ✓ Standards inserted:", standard_result$rows_inserted, "rows\n")
#     }
#   }
#
#   # Insert blanks
#   if (!is.null(blanks_to_upload) && nrow(blanks_to_upload) > 0) {
#     cat("  Inserting blanks...\n")
#     blank_result <- insert_to_table(
#       conn, "madi_results", "xmap_buffer", blanks_to_upload, "blank",
#       required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
#     )
#     result$counts$blanks <- blank_result$rows_inserted
#
#     if (!blank_result$success) {
#       result$errors$blanks <- blank_result$message
#       cat("    ✗ Blank insert failed:", blank_result$message, "\n")
#     } else {
#       cat("    ✓ Blanks inserted:", blank_result$rows_inserted, "rows\n")
#     }
#   }
#
#   # Insert controls
#   if (!is.null(controls_to_upload) && nrow(controls_to_upload) > 0) {
#     cat("  Inserting controls...\n")
#     control_result <- insert_to_table(
#       conn, "madi_results", "xmap_control", controls_to_upload, "control",
#       required_cols = c("project_id", "study_accession", "plate_id", "well", "antigen")
#     )
#     result$counts$controls <- control_result$rows_inserted
#
#     if (!control_result$success) {
#       result$errors$controls <- control_result$message
#       cat("    ✗ Control insert failed:", control_result$message, "\n")
#     } else {
#       cat("    ✓ Controls inserted:", control_result$rows_inserted, "rows\n")
#     }
#   }
#
#   # Insert antigens (with deduplication)
#   if (!is.null(antigens_to_upload) && nrow(antigens_to_upload) > 0) {
#     cat("  Inserting antigens...\n")
#     existing_antigens <- get_existing_antigens(conn, study_accession, experiment_accession)
#
#     result$counts$antigens <- insert_new_rows(
#       conn, "madi_results", "xmap_antigen_family",
#       new_data = antigens_to_upload,
#       existing_data = existing_antigens,
#       join_keys = c("study_accession", "experiment_accession", "antigen"),
#       label = "antigen family"
#     )
#     cat("    ✓ Antigens inserted:", result$counts$antigens, "rows\n")
#   }
#
#   # Insert visits (with deduplication)
#   if (!is.null(visits_to_upload) && nrow(visits_to_upload) > 0) {
#     cat("  Inserting visits...\n")
#     existing_visits <- get_existing_visits(conn, study_accession)
#
#     result$counts$visits <- insert_new_rows(
#       conn, "madi_results", "xmap_planned_visit",
#       new_data = visits_to_upload,
#       existing_data = existing_visits,
#       join_keys = c("study_accession", "timepoint_name"),
#       label = "planned visit"
#     )
#     cat("    ✓ Visits inserted:", result$counts$visits, "rows\n")
#   }
#
#
#   # FINALIZE RESULT
#
#   result$success <- length(result$errors) == 0
#   result$message <- if (result$success) {
#     "Batch uploaded successfully"
#   } else {
#     paste("Upload completed with errors:", paste(names(result$errors), collapse = ", "))
#   }
#
#   # Update validation state
#   if (result$success) {
#     current_state <- batch_validation_state()
#     batch_validation_state(list(
#       is_validated = current_state$is_validated,
#       is_uploaded = TRUE,
#       validation_time = current_state$validation_time,
#       upload_time = Sys.time(),
#       metadata_result = current_state$metadata_result,
#       bead_array_result = current_state$bead_array_result
#     ))
#   }
#
#
#   # SHOW NOTIFICATIONS
#
#   if (result$success) {
#     counts <- result$counts
#     detail_message <- sprintf(
#       "Uploaded: %d headers, %d samples, %d standards, %d blanks, %d controls, %d antigens, %d visits",
#       counts$header, counts$samples, counts$standards,
#       counts$blanks, counts$controls, counts$antigens, counts$visits
#     )
#
#     showNotification("Batch Uploaded Successfully", type = "message", duration = 5)
#     showNotification(detail_message, type = "message", duration = 10)
#
#     cat("\n✓ UPLOAD COMPLETE\n")
#     cat(detail_message, "\n")
#   } else {
#     showNotification(
#       paste("Upload failed:", result$message),
#       type = "error",
#       duration = NULL
#     )
#
#     for (error_name in names(result$errors)) {
#       showNotification(
#         paste(error_name, "error:", result$errors[[error_name]]),
#         type = "error",
#         duration = NULL
#       )
#     }
#
#     cat("\n✗ UPLOAD FAILED\n")
#     cat("Errors:", paste(names(result$errors), collapse = ", "), "\n")
#   }
#
#   cat("╚══════════════════════════════════════════════════════════╝\n\n")
# })

# SERVER: Execute deletion when confirmation is received
observeEvent(input$delete_confirmation, {
  # input$delete_confirmation is TRUE when confirmed, FALSE when cancelled
  req(input$delete_confirmation == TRUE)
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  selected_study_accession <- input$readxMap_study_accession

  cat("\nDelete confirmation received for:", selected_study_accession, "\n")

  # Show busy indicator
  show_modal_spinner(
    spin = "fading-circle",
    color = "#3c8dbc",
    text = "Deleting study data... Please wait."
  )

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )

    cat("\nExecuting query:\n")
    print(query)

    result <- dbExecute(conn, query)

    cat("\ndbExecute result:", result, "\n")

    # Remove busy indicator
    remove_modal_spinner()

    # Show success message
    shinyalert(
      title = "Success!",
      text = paste0(
        "All data for '", selected_study_accession, "' has been successfully deleted."
      ),
      type = "success"
    )

    # Trigger refresh - get the list, modify it, set it back
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    cat("\nError during deletion:", e$message, "\n")

    # Remove busy indicator
    remove_modal_spinner()

    # Show error message
    shinyalert(
      title = "Deletion Failed",
      text = paste("An error occurred while deleting the data:\n\n", e$message),
      type = "error"
    )
  })
})

# SERVER: Confirmation Modal Dialog
observeEvent(input$delete_study_btn, {
  req(input$readxMap_study_accession)
  req(input$readxMap_study_accession != "Click here")

  data <- delete_study_data()

  if (nrow(data) == 0) {
    shinyalert(
      title = "No Data",
      text = "No data to delete for this study.",
      type = "warning"
    )
    return()
  }

  selected_study_accession <- input$readxMap_study_accession
  total_rows <- sum(data$row_count)
  table_count <- nrow(data)

  shinyalert(
    title = "Confirm Deletion",
    text = paste0(
      "You are about to permanently delete all data for:\n\n",
      selected_study_accession, "\n\n",
      "This will remove ", format(total_rows, big.mark = ","), " rows from ",
      table_count, " table", ifelse(table_count > 1, "s", ""), ".\n\n",
      "THIS ACTION CANNOT BE UNDONE!"
    ),
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonText = "Yes, Delete Permanently",
    cancelButtonText = "Cancel",
    confirmButtonCol = "#d9534f",
    inputId = "delete_confirmation"  # This creates input$delete_confirmation
  )
})

observeEvent(input$execute_delete_btn, {
  req(input$readxMap_study_accession)

  selected_study_accession <- input$readxMap_study_accession

  # Show busy indicator
  show_modal_spinner(
    spin = "fading-circle",
    color = "#3c8dbc",
    text = "Deleting study data... Please wait."
  )

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )
    cat("\n execute_delete_btn: query to delete study: \n")
    print(query)

    dbExecute(conn, query)

    # Remove busy indicator
    remove_modal_spinner()

    # Show success message
    shinyalert(
      title = "Success!",
      text = paste0(
        "All data for '", selected_study_accession, "' has been successfully deleted."
      ),
      type = "success"
    )

    # Trigger refresh of the tab/data
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    # Remove busy indicator
    remove_modal_spinner()

    # Show error message
    shinyalert(
      title = "Deletion Failed",
      text = paste("An error occurred while deleting the data:\n\n", e$message),
      type = "error"
    )
  })
})

# SERVER: Cancel Deletion - Close Modal
observeEvent(input$cancel_delete_btn, {
  removeModal()
  showNotification(
    "Deletion cancelled.",
    type = "message",
    duration = 2
  )
})

# SERVER: Confirm Deletion - Execute Delete Query
observeEvent(input$confirm_delete_btn, {
  req(input$readxMap_study_accession)

  selected_study_accession <- input$readxMap_study_accession

  # Show progress modal
  showModal(modalDialog(
    title = "Deleting Data...",
    size = "s",
    easyClose = FALSE,
    footer = NULL,
    div(
      style = "text-align: center; padding: 30px;",
      icon("spinner", class = "fa-spin", style = "font-size: 48px; color: #3c8dbc;"),
      p(
        style = "margin-top: 20px; font-size: 14px;",
        "Please wait while the data is being deleted..."
      )
    )
  ))

  # Execute the delete procedure
  tryCatch({
    query <- glue_sql(
      "CALL public.delete_study_accession_rows({selected_study_accession});",
      selected_study_accession = selected_study_accession,
      .con = conn
    )

    dbExecute(conn, query)

    # Close progress modal and show success
    removeModal()

    showModal(modalDialog(
      title = tagList(
        icon("check-circle", style = "color: #5cb85c;"),
        span("Deletion Complete", style = "color: #5cb85c;")
      ),
      size = "s",
      easyClose = TRUE,

      div(
        style = "text-align: center; padding: 20px;",
        icon("check-circle", style = "font-size: 48px; color: #5cb85c;"),
        p(
          style = "margin-top: 20px; font-size: 16px;",
          sprintf(
            "All data for '%s' has been successfully deleted.",
            selected_study_accession
          )
        )
      ),

      footer = tagList(
        actionButton(
          "close_success_modal",
          "Close",
          class = "btn-success",
          style = "padding: 10px 30px;"
        )
      )
    ))

    # Trigger refresh of the tab/data
    current_counters <- tabRefreshCounter()
    current_counters$import_tab <- current_counters$import_tab + 1
    tabRefreshCounter(current_counters)

  }, error = function(e) {
    removeModal()

    showModal(modalDialog(
      title = tagList(
        icon("times-circle", style = "color: #d9534f;"),
        span("Deletion Failed", style = "color: #d9534f;")
      ),
      size = "m",
      easyClose = TRUE,

      div(
        style = "text-align: center; padding: 20px;",
        icon("times-circle", style = "font-size: 48px; color: #d9534f;"),
        p(
          style = "margin-top: 20px; font-size: 14px;",
          "An error occurred while deleting the data:"
        ),
        div(
          style = "background-color: #f2dede; padding: 15px; border-radius: 5px; margin-top: 15px; text-align: left;",
          code(e$message)
        )
      ),

      footer = modalButton("Close")
    ))
  })
})

# SERVER: Close Success Modal
observeEvent(input$close_success_modal, {
  removeModal()
})

