#' Project Initialization UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert useShinyalert
mod_project_init_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    # --- Custom CSS for Project Init ---
    tags$style(HTML("
      /* Accordion Customization */
      .accordion-button:not(.collapsed) {
        background-color: rgba(0, 128, 128, 0.1);
        color: #008080;
        box-shadow: inset 0 -1px 0 rgba(0,0,0,.125);
      }
      .accordion-button:focus {
        border-color: #008080;
        box-shadow: 0 0 0 0.25rem rgba(0, 128, 128, 0.25);
      }
      .accordion-button::after {
        filter: invert(32%) sepia(16%) saturate(3065%) hue-rotate(136deg) brightness(96%) contrast(101%);
      }
      .btn-teal {
        background-color: #008080;
        color: white;
        border: none;
        transition: all 0.3s ease;
      }
      .btn-teal:hover {
        background-color: #006666;
        color: white;
        transform: translateY(-1px);
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .btn-teal:disabled {
        background-color: #a3d9d9;
        transform: none;
      }
      /* Simple Status Box */
      .status-panel {
        background-color: #f8f9fa;
        border-left: 5px solid #008080;
        padding: 15px;
        border-radius: 4px;
        margin-bottom: 20px;
      }
    ")),

    # Use page_fluid so the module controls its own spacing.
    bslib::page_fluid(
      class = "p-0",

      # Main layout: sidebar plus content.
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        sidebar = bslib::sidebar(
          title = "Project Settings",
          width = 400,
          bg = "#f8f9fa",

          bslib::accordion(
            open = "Data Upload",

            # --- Panel 1: Data Upload ---
            bslib::accordion_panel(
              title = "Data Upload",
              icon = bsicons::bs_icon("cloud-upload"),

              fileInput(ns('SampleInfo'), 'Sample Information (.csv)',
                        accept = '.csv', width = "100%"),

              div(class = "card p-2 bg-light border-0 mb-3",
                  tags$small(class="text-muted fst-italic mb-1",
                             bsicons::bs_icon("info-circle"), " Required CSV format:"),
                  img(src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo20240319180805.png",
                      width = "100%", style="border-radius: 4px; border: 1px solid #ddd;")
              ),

              # Column mapping
              tags$label("Column Mapping:", class="form-label fw-bold text-primary"),
              selectInput(ns("sample_id_raw"), "Sample ID", choices = NULL),
              selectInput(ns("injection.order_raw"), "Injection Order", choices = NULL),
              selectInput(ns("class_raw"), "Class", choices = NULL),
              selectInput(ns("group_raw"), "Group", choices = NULL),
              selectInput(ns("batch_raw"), "Batch", choices = NULL)
            ),

            # --- Panel 2: Optional LC-MS method context ---
            bslib::accordion_panel(
              title = "LC-MS Conditions",
              icon = bsicons::bs_icon("clipboard-data"),

              tags$small(
                class = "text-muted d-block mb-2",
                "Optional project-level method context. It will be reused by parameter advice, AI annotation review, and MetMiner Bot prompts."
              ),
              textAreaInput(
                ns("lcms_method_text"),
                "Paste LC-MS method text",
                value = "",
                rows = 8,
                placeholder = paste(
                  "Instrument: Orbitrap Exploris 240",
                  "Column: C18 ...",
                  "Mobile phase A: water + 0.1% formic acid",
                  "Mobile phase B: acetonitrile",
                  "Ion source: ESI; positive and negative; full scan DDA MS/MS",
                  "Scan range: m/z 70-1050; NCE 20/40/60",
                  sep = "\n"
                )
              ),
              actionButton(ns("autofill_lcms"), "Auto fill key items", icon = icon("wand-magic-sparkles"), class = "btn-outline-primary w-100 mb-2"),
              selectInput(ns("lcms_instrument"), "Instrument class",
                          choices = c("Auto detect" = "auto", "Orbitrap", "QTOF", "Other"), selected = "auto"),
              selectInput(ns("lcms_chromatography"), "Chromatography",
                          choices = c("Auto detect" = "auto", "C18 reversed phase", "HILIC", "Other"), selected = "auto"),
              textInput(ns("lcms_column"), "Column / stationary phase", value = ""),
              textInput(ns("lcms_mobile_a"), "Mobile phase A", value = ""),
              textInput(ns("lcms_mobile_b"), "Mobile phase B", value = ""),
              textInput(ns("lcms_ion_mode"), "Ion mode", value = "", placeholder = "positive; negative"),
              textInput(ns("lcms_scan_range"), "Scan range", value = "", placeholder = "m/z 70-1050"),
              textInput(ns("lcms_collision"), "Collision energy", value = "", placeholder = "NCE 20/40/60")
            ),

            # --- Panel 2: Resuming Task ---
            bslib::accordion_panel(
              title = "Resuming Task",
              icon = bsicons::bs_icon("arrow-repeat"),

              tags$label("Resume by Job ID:", class = "form-label fw-bold text-primary"),
              textInput(ns("resume_job_id"), NULL, value = "", placeholder = "MM20260504XXXXXXXX"),
              actionButton(ns("load_job_id"), "Load Previous Job", icon = icon("folder-open"),
                           class = "btn-outline-primary w-100 mb-3"),

              div(class="border-top my-3"),

              selectInput(ns("init_steps"), "Resume from Step:",
                          choices = c("None" = "none",
                                      "Remove Noise" = "Remove noisey feature",
                                      "Remove Outlier" = "Remove outlier",
                                      "Impute MV" = "impute missing value",
                                      "Normalization" = "Normalization",
                                      "Annotation" = "Annotation",
                                      "Filtering" = "Annotation filtering")),

              div(class="border-top my-3"),

              tags$small(class="text-muted d-block mb-2", "Or upload existing objects (.rda):"),
              fileInput(ns('saved_obj_pos'), 'Positive Mode Object', accept = '.rda'),
              fileInput(ns('saved_obj_neg'), 'Negative Mode Object', accept = '.rda')
            )
          )
        ),

        # --- Main Content Area ---
        # Keep the page compact without crowding the controls.
        div(class = "p-3",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h3("Project Initialization", class = "text-primary fw-bold m-0"),
              # Main initialization action.
              actionButton(ns('action_init'), 'Run Initialization',
                           icon = icon("play"), class = "btn-teal btn-lg shadow-sm")
            ),

            # Use the shared heading divider when available.
            if(exists("hr_head")) hr_head() else tags$hr(),

            # Project status display.
            uiOutput(ns("wd_status")),
            tags$div(class="mb-3",
                     tags$label("Job ID:", class="form-label text-muted"),
                     verbatimTextOutput(ns("generated_job_id"), placeholder = TRUE)
            ),
            tags$div(class="mb-3",
                     tags$label("Generated Working Directory:", class="form-label text-muted"),
                     verbatimTextOutput(ns("generated_wd_path"), placeholder = TRUE)
            ),

            bslib::layout_columns(
              col_widths = c(7, 5),
              bslib::card(
                height = "400px",
                bslib::card_header("Sample Information Summary", class = "bg-light"),
                DT::dataTableOutput(ns("tbl_sample_info"))
              ),
              bslib::card(
                height = "400px",
                bslib::card_header("LC-MS Conditions", class = "bg-light"),
                verbatimTextOutput(ns("lcms_summary"), placeholder = TRUE)
              )
            ),

            br(),
            h4("Loaded Objects Summary", class = "text-secondary"),

            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("Positive Mode Data", class = "bg-info-subtle text-info-emphasis"),
                verbatimTextOutput(ns("res_pos_mod")),
                style = "min-height: 300px;"
              ),
              bslib::card(
                bslib::card_header("Negative Mode Data", class = "bg-warning-subtle text-warning-emphasis"),
                verbatimTextOutput(ns("res_neg_mod")),
                style = "min-height: 300px;"
              )
            )
        )
      ),
      # --- Footer ---
      if(exists("metminer_footer")) metminer_footer() else tags$div("(c) MetMiner")
    )
  )
}

#' Project Initialization Server Module
#'
#' @param id Module id.
#' @param prj_init ReactiveValues to store project state.
#' @noRd
#' @importFrom shinyalert shinyalert
#' @importFrom dplyr rename mutate
#' @importFrom utils read.csv
mod_project_init_server <- function(id, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. State Management ---
    wd_generated <- reactiveVal(FALSE)
    initial_job_id <- generate_metminer_job_id()
    current_job_id <- reactiveVal(initial_job_id)

    # Generate a candidate MetMiner project directory.
    generate_random_path <- function(job_id = initial_job_id) {
      base_dir <- path.expand("~/metminer_results")
      timestamp <- format(Sys.time(), "%Y%m%d")
      folder_name <- paste0("proj_", timestamp, "_", job_id)
      file.path(base_dir, folder_name)
    }

    # Candidate path shown before the user confirms initialization.
    candidate_path <- reactiveVal(generate_random_path())

    # --- 2. UI Updates (Status) ---
    output$wd_status <- renderUI({
      if (wd_generated()) {
        div(
          class = "status-panel",
          div(
            style = "color: #008080; font-weight: bold; font-size: 1.1rem;",
            bsicons::bs_icon("check-circle-fill", class="me-2"),
            "Project Initialized Successfully"
          ),
          div(
            class = "text-muted mt-2",
            "The workspace has been created and files are ready for processing."
          )
        )
      } else {
        div(
          class = "alert alert-light border",
          bsicons::bs_icon("info-circle", class="me-2 text-info"),
          "Ready to initialize. Please upload sample info and click 'Run Initialization'."
        )
      }
    })

    output$generated_wd_path <- renderText({
      if (wd_generated()) {
        prj_init$wd
      } else {
        # Show the candidate path as a preview.
        paste(candidate_path(), "(Preview)")
      }
    })

    output$generated_job_id <- renderText({
      if (!is.null(prj_init$job_id) && nzchar(prj_init$job_id)) {
        prj_init$job_id
      } else {
        paste(current_job_id(), "(Preview)")
      }
    })

    current_lcms_items <- reactive({
      list(
        method_text = input$lcms_method_text %||% "",
        instrument = input$lcms_instrument %||% "auto",
        chromatography = input$lcms_chromatography %||% "auto",
        column = input$lcms_column %||% "",
        mobile_phase_a = input$lcms_mobile_a %||% "",
        mobile_phase_b = input$lcms_mobile_b %||% "",
        ion_source = "ESI",
        ion_mode = input$lcms_ion_mode %||% "",
        scan_range = input$lcms_scan_range %||% "",
        collision_energy = input$lcms_collision %||% ""
      )
    })

    output$lcms_summary <- renderText({
      if (!is.null(prj_init$lcms_conditions_text) && nzchar(trimws(prj_init$lcms_conditions_text))) {
        return(prj_init$lcms_conditions_text)
      }
      txt <- metminer_format_lcms_conditions(current_lcms_items())
      if (nzchar(trimws(gsub("Original method text:\\n", "", txt, fixed = TRUE)))) txt else "Optional. Add LC-MS method text or key items before initialization."
    })

    # --- 3. Sample Info Handling ---
    sample_info_raw <- reactive({
      req(input$SampleInfo)
      # Validate the uploaded file extension before reading.
      ext <- tools::file_ext(input$SampleInfo$name)
      if (tolower(ext) != "csv") {
        shinyalert::shinyalert("Error", "Invalid file type. Please upload a .csv file.", type = "error")
        return(NULL)
      }

      tryCatch({
        df <- read.csv(input$SampleInfo$datapath, header = TRUE, stringsAsFactors = FALSE)
        if(nrow(df) == 0 || ncol(df) == 0) stop("Empty CSV file")
        df
      }, error = function(e) {
        shinyalert::shinyalert("Error", paste("Failed to read CSV file:", e$message), type = "error")
        NULL
      })
    })

    # Update mapping inputs after the CSV is read successfully.
    observeEvent(sample_info_raw(), {
      df <- sample_info_raw()
      if (!is.null(df) && ncol(df) > 0) {
        cols <- colnames(df)
        # Safely pick defaults even if the CSV has fewer columns.
        get_col <- function(idx) if(idx <= length(cols)) cols[idx] else cols[1]

        updateSelectInput(session, "sample_id_raw", choices = cols, selected = get_col(1))
        updateSelectInput(session, "injection.order_raw", choices = cols, selected = get_col(2))
        updateSelectInput(session, "class_raw", choices = cols, selected = get_col(3))
        updateSelectInput(session, "group_raw", choices = cols, selected = get_col(4))
        updateSelectInput(session, "batch_raw", choices = cols, selected = get_col(5))
      }
    })

    # --- 4. Initialization Logic ---
    restore_job <- function(job_id) {
      full_path <- find_metminer_job(job_id)
      if (is.null(full_path)) {
        shinyalert::shinyalert("Job Not Found", paste("No project found for job id:", job_id), type = "error")
        return(FALSE)
      }

      prj_init$job_id <- trimws(job_id)
      prj_init$wd <- full_path
      prj_init$mass_dataset_dir <- file.path(prj_init$wd, "mass_dataset")
      prj_init$data_export_dir <- file.path(prj_init$wd, "data_export")

      sample_info_file <- file.path(prj_init$wd, "sample_info.rds")
      if (file.exists(sample_info_file)) {
        prj_init$sample_info <- readRDS(sample_info_file)
        output$tbl_sample_info <- DT::renderDataTable({
          DT::datatable(prj_init$sample_info, options = list(scrollX = TRUE, pageLength = 5))
        })
      }
      lcms_file <- file.path(prj_init$wd, "project_lcms_conditions.rds")
      if (file.exists(lcms_file)) {
        prj_init$lcms_conditions <- readRDS(lcms_file)
        prj_init$lcms_conditions_text <- metminer_format_lcms_conditions(prj_init$lcms_conditions)
      } else {
        project_info_file <- file.path(prj_init$wd, "project_info.rds")
        if (file.exists(project_info_file)) {
          info <- readRDS(project_info_file)
          if (!is.null(info$lcms_conditions)) {
            prj_init$lcms_conditions <- info$lcms_conditions
            prj_init$lcms_conditions_text <- metminer_format_lcms_conditions(prj_init$lcms_conditions)
          }
        }
      }

      loaded_objects <- load_metminer_saved_objects(prj_init$wd)
      if (!is.null(prj_init$sample_info) && length(loaded_objects) > 0) {
        loaded_objects <- lapply(names(loaded_objects), function(nm) {
          metminer_harmonize_sample_info(
            loaded_objects[[nm]],
            project_sample_info = prj_init$sample_info,
            mode = paste("restored", nm)
          )
        }) |>
          stats::setNames(names(loaded_objects))
      }
      prj_init$loaded_objects <- loaded_objects
      prj_init$object_positive.init <- latest_loaded_metminer_object(loaded_objects, "positive")
      prj_init$object_negative.init <- latest_loaded_metminer_object(loaded_objects, "negative")

      register_metminer_job(prj_init$job_id, prj_init$wd)
      wd_generated(TRUE)
      shinyjs::disable("action_init")

      loaded_names <- names(loaded_objects)
      shinyalert::shinyalert(
        "Job Loaded",
        paste0(
          "Workspace restored for job ", prj_init$job_id, ".\n",
          "Loaded objects: ",
          if (length(loaded_names) > 0) paste(loaded_names, collapse = ", ") else "none"
        ),
        type = "success"
      )
      TRUE
    }

    observeEvent(input$load_job_id, {
      job_id <- trimws(input$resume_job_id %||% "")
      if (!nzchar(job_id)) {
        shinyalert::shinyalert("Missing Job ID", "Please enter a job id.", type = "error")
        return()
      }
      restore_job(job_id)
    })

    observeEvent(input$autofill_lcms, {
      items <- metminer_extract_lcms_items(
        method_text = input$lcms_method_text %||% "",
        instrument_type = input$lcms_instrument %||% "auto",
        chromatography = input$lcms_chromatography %||% "auto"
      )
      updateSelectInput(session, "lcms_instrument", selected = items$instrument %||% "Other")
      updateSelectInput(session, "lcms_chromatography", selected = items$chromatography %||% "Other")
      updateTextInput(session, "lcms_column", value = items$column %||% "")
      updateTextInput(session, "lcms_mobile_a", value = items$mobile_phase_a %||% "")
      updateTextInput(session, "lcms_mobile_b", value = items$mobile_phase_b %||% "")
      updateTextInput(session, "lcms_ion_mode", value = items$ion_mode %||% "")
      updateTextInput(session, "lcms_scan_range", value = items$scan_range %||% "")
      updateTextInput(session, "lcms_collision", value = items$collision_energy %||% "")
      shinyalert::shinyalert("LC-MS Conditions", "Key method items were auto-filled. Please review and edit if needed.", type = "success")
    })

    observeEvent(prj_init$lcms_conditions, {
      items <- prj_init$lcms_conditions
      if (is.null(items) || length(items) == 0) {
        return()
      }
      updateTextAreaInput(session, "lcms_method_text", value = items$method_text %||% "")
      updateSelectInput(session, "lcms_instrument", selected = items$instrument %||% "Other")
      updateSelectInput(session, "lcms_chromatography", selected = items$chromatography %||% "Other")
      updateTextInput(session, "lcms_column", value = items$column %||% "")
      updateTextInput(session, "lcms_mobile_a", value = items$mobile_phase_a %||% "")
      updateTextInput(session, "lcms_mobile_b", value = items$mobile_phase_b %||% "")
      updateTextInput(session, "lcms_ion_mode", value = items$ion_mode %||% "")
      updateTextInput(session, "lcms_scan_range", value = items$scan_range %||% "")
      updateTextInput(session, "lcms_collision", value = items$collision_energy %||% "")
    }, ignoreInit = TRUE)

    observeEvent(input$action_init, {
      # Prevent repeated initialization in the same session.
      if (wd_generated()) {
        shinyalert::shinyalert("Already Initialized", "Please refresh the app to start a new project.", type = "warning")
        return()
      }

      if(is.null(sample_info_raw())){
        shinyalert::shinyalert("Missing Input", "Please upload sample information file first.", type = "error")
        return()
      }

      tryCatch({
        # A. Use the current candidate path.
        full_path <- candidate_path()
        job_id <- current_job_id()

        # B. Update state.
        wd_generated(TRUE)

        # C. Store project paths.
        prj_init$job_id <- job_id
        prj_init$wd <- full_path
        prj_init$mass_dataset_dir <- file.path(prj_init$wd, "mass_dataset")
        prj_init$data_export_dir <- file.path(prj_init$wd, "data_export")
        prj_init$lcms_conditions <- current_lcms_items()
        if (identical(prj_init$lcms_conditions$instrument, "auto") ||
            identical(prj_init$lcms_conditions$chromatography, "auto")) {
          parsed_lcms <- metminer_extract_lcms_items(
            method_text = prj_init$lcms_conditions$method_text,
            instrument_type = prj_init$lcms_conditions$instrument,
            chromatography = prj_init$lcms_conditions$chromatography
          )
          if (identical(prj_init$lcms_conditions$instrument, "auto")) prj_init$lcms_conditions$instrument <- parsed_lcms$instrument
          if (identical(prj_init$lcms_conditions$chromatography, "auto")) prj_init$lcms_conditions$chromatography <- parsed_lcms$chromatography
        }
        prj_init$lcms_conditions_text <- metminer_format_lcms_conditions(prj_init$lcms_conditions)

        # Create project directories.
        dir.create(prj_init$wd, showWarnings = FALSE, recursive = TRUE)
        dir.create(prj_init$mass_dataset_dir, showWarnings = FALSE, recursive = TRUE)
        dir.create(prj_init$data_export_dir, showWarnings = FALSE, recursive = TRUE)

        # D. Normalize sample information columns.
        prj_init$sample_info <- sample_info_raw() |>
          dplyr::rename(
            "sample_id" = !!input$sample_id_raw,
            "injection.order" = !!input$injection.order_raw,
            "class" = !!input$class_raw,
            "group" = !!input$group_raw,
            "batch" = !!input$batch_raw
          ) |>
          dplyr::mutate(batch = as.character(batch))

        saveRDS(prj_init$sample_info, file.path(prj_init$wd, "sample_info.rds"))
        saveRDS(prj_init$lcms_conditions, file.path(prj_init$wd, "project_lcms_conditions.rds"))
        saveRDS(
          list(
            job_id = prj_init$job_id,
            wd = prj_init$wd,
            mass_dataset_dir = prj_init$mass_dataset_dir,
            data_export_dir = prj_init$data_export_dir,
            lcms_conditions = prj_init$lcms_conditions,
            created_at = as.character(Sys.time())
          ),
          file.path(prj_init$wd, "project_info.rds")
        )
        register_metminer_job(prj_init$job_id, prj_init$wd)

        output$tbl_sample_info <- DT::renderDataTable({
          DT::datatable(prj_init$sample_info, options = list(scrollX = TRUE, pageLength = 5))
        })

        # E. Restore task state when resuming.
        prj_init$steps <- input$init_steps

        # E2. Load optional mass_dataset objects from manual upload.
        load_object <- function(file_input, label, slot_name) {
          if (!is.null(file_input)) {
            res <- validate_file(file_input$datapath, "Unknown", label)
            if (res$success) {
              prj_init[[slot_name]] <- metminer_harmonize_sample_info(
                res$object,
                project_sample_info = prj_init$sample_info,
                mode = label
              )
              return(TRUE)
            } else {
              shinyalert::shinyalert("Validation Failed", res$message, type = "error")
              return(FALSE)
            }
          }
          return(FALSE)
        }

        has_pos <- load_object(input$saved_obj_pos, "Positive Object", "object_positive.init")
        has_neg <- load_object(input$saved_obj_neg, "Negative Object", "object_negative.init")

        # F. Report success.
        shinyalert::shinyalert(
          title = "MetMiner Project Initialized!",
          text = paste0("Workspace created successfully.\nJob ID: ", prj_init$job_id),
          type = "success"
        )

        # Disable controls to prevent duplicate submission.
        shinyjs::disable("action_init")
        shinyjs::disable("SampleInfo")

      }, error = function(e) {
        shinyalert::shinyalert("Initialization Failed", paste("Error:", e$message), type = "error")
        # Roll back state if initialization fails.
        wd_generated(FALSE)
      })
    })

    # --- 5. Summary Outputs ---
    output$res_pos_mod <- check_massdata_info(reactive(prj_init$object_positive.init), "positive")
    output$res_neg_mod <- check_massdata_info(reactive(prj_init$object_negative.init), "negative")

  })
}
