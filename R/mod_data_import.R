#' Data Import Unified UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden show hide runjs
#' @importFrom magrittr %>%
mod_data_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # --- Custom CSS ---
    tags$style(HTML("
      .import-status-card { border-left: 5px solid #008080; background-color: #f8f9fa; }
      .param-section { border: 1px solid #e9ecef; padding: 15px; border-radius: 5px; margin-top: 10px; background-color: white; }
      .opt-highlight { border: 2px solid #ffc107; }
    ")),

    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        # --- Sidebar: Settings ---
        sidebar = bslib::sidebar(
          title = "Data Import Settings",
          width = 400,
          bg = "#f8f9fa",

          # 1. Source Selection
          selectInput(ns("data_source"), "Select Data Source",
                      choices = c(
                        "Raw MS Data (.zip)" = "raw",
                        "Peak Picking Table (.csv)" = "tbl",
                        "Mass Dataset (.rda)" = "mass"
                      ),
                      selected = "raw"
          ),
          tags$hr(),

          # --- A. Raw Data Inputs ---
          conditionalPanel(
            condition = sprintf("input['%s'] == 'raw'", ns("data_source")),
            fileInput(ns('ms1_zip'), 'Upload MS1 ZIP', accept = '.zip', placeholder = "No file selected"),
            actionButton(ns("process_ms1_zip"), "1. Pre-process ZIP", icon = icon("gears"), class = "btn-teal w-100 mb-2"),

            # --- Restored Parameters Panel ---
            div(id = ns("raw_params_panel"),
                tags$hr(),
                tags$label("Peak Picking Parameters", class="fw-bold"),
                bslib::accordion(
                  open = FALSE,
                  bslib::accordion_panel(
                    title = "Basic Parameters", icon = bsicons::bs_icon("sliders"),
                    textInput(ns('ppm'), 'ppm', value = 15),
                    textInput(ns('snthresh'), 'snthresh', value = 10),
                    textInput(ns('noise'), 'noise', value = 500),
                    selectInput(ns('threads'), 'threads', choices = c(1, 2, 4), selected = 1)
                  ),
                  bslib::accordion_panel(
                    title = "Advanced Parameters", icon = bsicons::bs_icon("sliders2"),
                    textInput(ns('p_min'), 'peakwidth min', value = 5),
                    textInput(ns('p_max'), 'peakwidth max', value = 30),
                    textInput(ns('pre_left'), 'prefilter peaks', value = 3),
                    textInput(ns('pre_right'), 'prefilter intensity', value = 500),
                    selectInput(ns('fitgauss'), 'fitgauss', choices = c("TRUE", "FALSE"), selected = "FALSE"),
                    textInput(ns('integrate'), 'integrate', value = 2),
                    textInput(ns('mzdiff'), 'mzdiff', value = 0.01),
                    textInput(ns('binSize'), 'binSize', value = 0.025),
                    textInput(ns('bw'), 'bw', value = 5),
                    textInput(ns('min_fraction'), 'min_fraction', value = 0.5),
                    selectInput(ns('fill_peaks'), 'fill_peaks', choices = c("TRUE", "FALSE"), selected = "FALSE"),
                    selectInput(ns('column'), 'column', choices = c("rp", "hilic"), selected = "rp")
                  )
                ),
                br()
            )
          ),

          # --- B. Peak Table Inputs ---
          conditionalPanel(
            condition = sprintf("input['%s'] == 'tbl'", ns("data_source")),
            fileInput(ns('expmat_tbl'), 'Peak Table (.csv)', accept = '.csv'),
            tags$label("Column Mapping:", class="fw-bold mt-2"),
            selectInput(ns("exp_vari_id"), "Variable ID Col", choices = NULL),
            selectInput(ns("exp_mz"), "m/z Col", choices = NULL),
            selectInput(ns("exp_rt"), "RT Col", choices = NULL),
            selectInput(ns("exp_ion"), "Polarity Col", choices = NULL),
            radioButtons(ns("rt_unit"), "RT Unit:", choices = c("Seconds" = "second", "Minutes" = "minute"), inline = TRUE),
            actionButton(ns("run_tbl_import"), "Import to MassDataset", icon = icon("file-import"), class = "btn-teal w-100 mt-3")
          ),

          # --- C. Mass Dataset Inputs ---
          conditionalPanel(
            condition = sprintf("input['%s'] == 'mass'", ns("data_source")),
            fileInput(ns('pos_obj_mass'), 'Positive Object (.rda)', accept = '.rda'),
            fileInput(ns('neg_obj_mass'), 'Negative Object (.rda)', accept = '.rda'),
            actionButton(ns("check_mass_obj"), "Load & Validate", icon = icon("check-double"), class = "btn-teal w-100")
          )
        ),

        # --- Main Content ---
        div(class = "p-3",

            # =========================================================
            # 1. Raw Data Workflow
            # =========================================================
            conditionalPanel(
              condition = sprintf("input['%s'] == 'raw'", ns("data_source")),

              # Row 1: Parameter Optimization (Conditional)
              div(id = ns("opt_ui_wrapper"), class = "mb-4",
                  tags$h3("Optimize Parameters", style = 'color: #0d6efd; font-size: 1.25rem; font-weight: bold; border-bottom: 2px solid #0d6efd; padding-bottom: 0.5rem; margin-bottom: 1rem;'),

                  bslib::navset_card_tab(
                    height = 600,
                    full_screen = TRUE,
                    title = "Parameter Optimization",
                    id = ns("opt_tabs"),

                    sidebar = bslib::sidebar(
                      width = 300,
                      bg = "#fcfcfc",
                      bslib::accordion(
                        open = FALSE,
                        bslib::accordion_panel(
                          title = "Step 1 Params",
                          icon = bsicons::bs_icon("1-circle"),
                          textInput(inputId = ns("opt_sd_1"), label = "massSDrange", value = 2),
                          textInput(inputId = ns("opt_smooth_1"), label = "smooth", value = 0),
                          textInput(inputId = ns("opt_cutoff_1"), label = "cutoff", value = 0.95),
                          selectInput(inputId = ns("opt_thread_1"), label = "thread", choices = c(1, 2, 4), selected = 1),
                          radioButtons(inputId = ns("opt_filenum_1"), label = "filenum", choices = c(3, 5, "all"), selected = 3)
                        ),
                        bslib::accordion_panel(
                          title = "Step 2 Params",
                          icon = bsicons::bs_icon("2-circle"),
                          textInput(inputId = ns("opt_sd_2"), label = "massSDrange", value = 2),
                          textInput(inputId = ns("opt_smooth_2"), label = "smooth", value = 0),
                          textInput(inputId = ns("opt_cutoff_2"), label = "cutoff", value = 0.95),
                          selectInput(inputId = ns("opt_thread_2"), label = "thread", choices = c(1, 2, 4), selected = 1),
                          textInput(inputId = ns("opt_ppm_2"), label = "ppmCut", value = 7, placeholder = "Auto from Step 1"),
                          radioButtons(inputId = ns("opt_filenum_2"), label = "filenum", choices = c(3, 5, "all"), selected = 3)
                        )
                      ),
                      tags$div(class="mt-2"),
                      actionButton(ns("run_opt_step1"), "Run Step 1: Estimate PPM", icon = icon("chart-line"), class = "btn-primary w-100"),
                      tags$div(class="mt-2"),
                      actionButton(ns("run_opt_step2"), "Run Step 2: Optimize Parameters", icon = icon("sliders"), class = "btn-outline-primary w-100"),
                      tags$div(class="mt-2"),
                      actionButton(ns("apply_opt_params"), "Apply Parameters", icon = icon("check"), class = "btn-success w-100")
                    ),

                    bslib::nav_panel(
                      title = "PPM Cutoff Plot",
                      bslib::card_body(
                        plotOutput(ns("ppmCut_plt"), height = "450px")
                      )
                    ),

                    bslib::nav_panel(
                      title = "Optimized Table",
                      bslib::card_body(
                        DT::dataTableOutput(ns("parameters_opt_tbl"))
                      )
                    )
                  )
              ),

              # Execution & Results Section (Always shown after ZIP process)
              div(id = ns("execution_ui_wrapper"),

                  # Button Area
                  div(class = "d-flex align-items-center mb-4 p-3 bg-light rounded border",
                      div(class="me-3",
                          actionButton(ns('action2'), '2. Start Peak Picking', icon = icon("play"), class = "btn-teal btn-lg", width = "320px")
                      ),
                      div(
                        tags$small(class="text-muted", "Click to generate MassDataset from Raw Data using settings from sidebar.")
                      )
                  ),

                  # Row 2: Data Preview (Fixed display)
                  tags$h3("Data Preview", style = 'color: #0d6efd; font-size: 1.25rem; font-weight: bold; border-bottom: 2px solid #0d6efd; padding-bottom: 0.5rem; margin-bottom: 1rem;'),
                  bslib::card(
                    min_height = "400px",
                    bslib::card_header(bsicons::bs_icon("table"), " Data Preview Table"),
                    bslib::layout_columns(
                      col_widths = c(12),
                      bslib::navset_card_tab(
                        bslib::nav_panel("Expression Data", DT::dataTableOutput(ns("preview_expmat"))),
                        bslib::nav_panel("Variable Info", DT::dataTableOutput(ns("preview_varinfo")))
                      )
                    )
                  ),

                  # Row 3: Status
                  tags$h3("Import Status", style = 'color: #0d6efd; font-size: 1.25rem; font-weight: bold; border-bottom: 2px solid #0d6efd; padding-bottom: 0.5rem; margin-bottom: 1rem; margin-top: 1.5rem;'),
                  bslib::layout_columns(
                    col_widths = c(6, 6),
                    bslib::card(
                      bslib::card_header("Positive Mode Data", class = "bg-info-subtle text-info-emphasis"),
                      verbatimTextOutput(ns("obj_mass_check.pos"), placeholder = TRUE),
                      style = "min-height: 300px;"
                    ),
                    bslib::card(
                      bslib::card_header("Negative Mode Data", class = "bg-warning-subtle text-warning-emphasis"),
                      verbatimTextOutput(ns("obj_mass_check.neg"), placeholder = TRUE),
                      style = "min-height: 300px;"
                    )
                  )
              )
            ),

            # =========================================================
            # 2. Table / MassDataset Status (Unchanged logic for other modes)
            # =========================================================
            conditionalPanel(
              condition = sprintf("input['%s'] != 'raw' && input['%s'] != ''", ns("data_source"), ns("data_source")),

              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  bslib::card_header("Positive Mode Data", class = "bg-info-subtle text-info-emphasis"),
                  verbatimTextOutput(ns("status_pos_summary"), placeholder = TRUE),
                  style = "min-height: 300px;"
                ),
                bslib::card(
                  bslib::card_header("Negative Mode Data", class = "bg-warning-subtle text-warning-emphasis"),
                  verbatimTextOutput(ns("status_neg_summary"), placeholder = TRUE),
                  style = "min-height: 300px;"
                )
              ),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("table"), " Data Preview"),
                bslib::layout_columns(
                  col_widths = c(12),
                  bslib::navset_card_tab(
                    bslib::nav_panel("Expression Data", DT::dataTableOutput(ns("preview_expmat_tbl"))),
                    bslib::nav_panel("Variable Info", DT::dataTableOutput(ns("preview_varinfo_tbl")))
                  )
                )
              )
            )
        )
      ),

      if(exists("metminer_footer")) metminer_footer() else tags$div()
    )
  )
}

#' Data Import Unified Server Module
#'
#' @param id Module id.
#' @param prj_init Project init reactive values.
#' @param global_data Global unified data store.
#' @param logger Function to log messages to the global console.
#' @noRd
#' @importFrom massprocesser process_data
#' @importFrom massdataset create_mass_dataset extract_expression_data extract_variable_info
#' @importFrom dplyr select mutate pull filter rename left_join everything all_of bind_rows
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom shinyalert shinyalert
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot ggtitle theme element_text element_rect annotate theme_void ggsave
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom utils head read.csv unzip write.csv
mod_data_import_server <- function(id, prj_init, global_data, logger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Log Helper
    log_msg <- function(msg, type = "info") {
      if(is.function(logger)) logger(msg, type)
    }

    # --- Internal State ---
    data_import <- reactiveValues(
      object_pos_raw = NULL,
      object_neg_raw = NULL
    )

    state <- reactiveValues(
      raw_ms1_dir = NULL,
      tbl_data = NULL,
      opt_results = list(pos = NULL, neg = NULL),
      combined_plot = NULL,
      final_params = NULL,
      opt_table = NULL,
      ppm_cut = NULL,
      opt_step1_done = FALSE,
      opt_step2_done = FALSE
    )

    shinyjs::hide("opt_ui_wrapper")
    shinyjs::hide("execution_ui_wrapper")

    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal   <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal  <- progress_handlers$close_progress_modal

    # ==========================================================================
    # 1. RAW DATA LOGIC
    # ==========================================================================

    # 1.1 Unzip and Check Structure
    observeEvent(input$process_ms1_zip, {
      req(prj_init$wd, input$ms1_zip)

      log_msg(paste("Processing ZIP:", input$ms1_zip$name), "info")
      shinyjs::runjs(sprintf("Swal.fire({title: 'Processing...', text: 'Extracting ZIP file...', icon: 'info', showConfirmButton: false});"))

      tryCatch({
        target_dir <- file.path(prj_init$wd, "MS1")
        if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
        dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

        unzip(input$ms1_zip$datapath, exdir = target_dir)

        all_dirs <- list.dirs(target_dir, recursive = TRUE, full.names = TRUE)
        all_dirs <- all_dirs[!grepl("__MACOSX", all_dirs)]

        has_data <- sapply(all_dirs, function(x) {
          sub_dirs <- basename(list.dirs(x, recursive = FALSE, full.names = TRUE))
          return("POS" %in% sub_dirs || "NEG" %in% sub_dirs)
        })

        valid_roots <- all_dirs[has_data]

        if (length(valid_roots) > 0) {
          state$raw_ms1_dir <- valid_roots[order(nchar(valid_roots))][1]
          log_msg(paste("Data found at:", state$raw_ms1_dir), "success")
        } else {
          stop("No 'POS' or 'NEG' folders found.")
        }

        check_ms1_structure(state$raw_ms1_dir)

        shinyjs::runjs("swal.close();")

        # ASK FOR OPTIMIZATION
        shinyalert::shinyalert(
          title = "Parameter Optimization",
          text = "Do you want to run parameter optimization before peak picking?",
          type = "info",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, optimize",
          cancelButtonText = "No, skip",
          inputId = ns("ask_opt"),
          callbackR = function(x) {
            if(x) {
              shinyjs::show("opt_ui_wrapper")
            } else {
              shinyjs::hide("opt_ui_wrapper")
            }
            # Always show the execution part (Button + Preview + Status)
            shinyjs::show("execution_ui_wrapper")
          }
        )

      }, error = function(e) {
        shinyjs::runjs("swal.close();")
        shinyalert::shinyalert("Error", e$message, type = "error")
        log_msg(paste("ZIP Error:", e$message), "error")
      })
    })

    get_polarity_dir <- function(root, polarity) {
      polarity_dir <- file.path(root, polarity)
      qc_dir <- file.path(polarity_dir, "QC")
      if(dir.exists(qc_dir)) qc_dir else file.path(polarity_dir, "Subject")
    }

    get_ppm_cut <- function(result) {
      if(is.null(result) || is.null(result$ppmCut)) return(NA_real_)
      ppm <- suppressWarnings(as.numeric(result$ppmCut))
      if(length(ppm) == 0 || !is.finite(ppm[1])) return(NA_real_)
      ppm[1]
    }

    combine_opt_tables <- function(tbl_pos, tbl_neg) {
      if(!is.null(tbl_pos) && !is.null(tbl_neg)) {
        dplyr::left_join(tbl_pos, tbl_neg, by = c("para", "desc"))
      } else if(!is.null(tbl_pos)) {
        tbl_pos
      } else if(!is.null(tbl_neg)) {
        tbl_neg
      } else {
        NULL
      }
    }

    # 1.2 Optimization Logic
    observeEvent(input$run_opt_step1, {
      req(state$raw_ms1_dir)
      struct <- check_ms1_structure(state$raw_ms1_dir)

      log_msg("Starting Parameter Optimization Step 1: PPM estimation...", "info")

      p1_sd <- as.numeric(input$opt_sd_1)
      p1_sm <- as.numeric(input$opt_smooth_1)
      p1_cutoff <- as.numeric(input$opt_cutoff_1)
      p1_th <- as.numeric(input$opt_thread_1)
      p1_fn <- input$opt_filenum_1

      show_progress_modal("Optimization Step 1", "Estimating ppmCut...", 0)

      tryCatch({
        state$opt_results <- list(pos = NULL, neg = NULL)
        state$opt_table <- NULL
        state$ppm_cut <- NULL
        state$opt_step1_done <- FALSE
        state$opt_step2_done <- FALSE

        if(struct$pos) {
          update_progress_modal(25, "Estimating POS ppmCut...")
          dir_pos <- get_polarity_dir(state$raw_ms1_dir, "POS")
          state$opt_results$pos <- paramounter_part1(directory = dir_pos, massSDrange = p1_sd, smooth = p1_sm, cutoff = p1_cutoff, filenum = p1_fn, thread = p1_th)
        }
        if(struct$neg) {
          update_progress_modal(65, "Estimating NEG ppmCut...")
          dir_neg <- get_polarity_dir(state$raw_ms1_dir, "NEG")
          state$opt_results$neg <- paramounter_part1(directory = dir_neg, massSDrange = p1_sd, smooth = p1_sm, cutoff = p1_cutoff, filenum = p1_fn, thread = p1_th)
        }

        ppm_candidates <- c(
          POS = get_ppm_cut(state$opt_results$pos),
          NEG = get_ppm_cut(state$opt_results$neg)
        )
        ppm_candidates <- ppm_candidates[is.finite(ppm_candidates)]

        if(length(ppm_candidates) > 0) {
          final_ppm <- max(ppm_candidates, na.rm = TRUE)
          state$ppm_cut <- final_ppm
          updateTextInput(session, "opt_ppm_2", value = round(final_ppm, 2))
        }

        state$opt_step1_done <- TRUE
        update_progress_modal(100, "Step 1 complete.")
        Sys.sleep(0.5)
        close_progress_modal()

        msg <- if(length(ppm_candidates) > 0) {
          paste0("Step 1 finished. Suggested ppmCut = ", round(state$ppm_cut, 2), ". Review the PPM plot, adjust ppmCut if needed, then run Step 2.")
        } else {
          "Step 1 finished, but no valid ppmCut was returned. Please inspect the plot/log and enter ppmCut manually before Step 2."
        }
        shinyalert::shinyalert("Step 1 Finished", msg, type = "success")

      }, error = function(e) {
        close_progress_modal()
        log_msg(paste("Optimization Step 1 Error:", e$message), "error")
        shinyalert::shinyalert("Error", paste("Step 1 failed:", e$message), type = "error")
      })
    })

    observeEvent(input$run_opt_step2, {
      req(state$raw_ms1_dir)
      struct <- check_ms1_structure(state$raw_ms1_dir)

      log_msg("Starting Parameter Optimization Step 2: final parameter search...", "info")

      p2_sd <- as.numeric(input$opt_sd_2)
      p2_sm <- as.numeric(input$opt_smooth_2)
      p2_cutoff <- as.numeric(input$opt_cutoff_2)
      p2_th <- as.numeric(input$opt_thread_2)
      p2_fn <- input$opt_filenum_2
      ppm_use <- suppressWarnings(as.numeric(input$opt_ppm_2))

      if(length(ppm_use) == 0 || !is.finite(ppm_use[1])) {
        shinyalert::shinyalert("Missing ppmCut", "Run Step 1 first, or enter a valid ppmCut manually before Step 2.", type = "warning")
        return(NULL)
      }
      ppm_use <- ppm_use[1]

      show_progress_modal("Optimization Step 2", "Optimizing peak-picking parameters...", 0)

      tryCatch({
        tbl_pos <- NULL
        tbl_neg <- NULL

        if(struct$pos) {
          update_progress_modal(35, "Optimizing POS parameters...")
          dir_pos <- get_polarity_dir(state$raw_ms1_dir, "POS")
          tbl_pos <- paramounter_part2(directory = dir_pos, massSDrange = p2_sd, smooth = p2_sm, cutoff = p2_cutoff, ppmCut = ppm_use, filenum = p2_fn, thread = p2_th)
          if(!is.null(tbl_pos)) tbl_pos <- tbl_pos %>% dplyr::rename(Positive = Value)
        }
        if(struct$neg) {
          update_progress_modal(70, "Optimizing NEG parameters...")
          dir_neg <- get_polarity_dir(state$raw_ms1_dir, "NEG")
          tbl_neg <- paramounter_part2(directory = dir_neg, massSDrange = p2_sd, smooth = p2_sm, cutoff = p2_cutoff, ppmCut = ppm_use, filenum = p2_fn, thread = p2_th)
          if(!is.null(tbl_neg)) tbl_neg <- tbl_neg %>% dplyr::rename(Negative = Value)
        }

        state$opt_table <- combine_opt_tables(tbl_pos, tbl_neg)
        if(is.null(state$opt_table)) {
          stop("No optimized parameter table was returned.")
        }
        state$opt_step2_done <- TRUE

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyalert::shinyalert("Step 2 Finished", "Optimized parameters are ready. Review the table and click 'Apply Parameters' to use them.", type = "success")

      }, error = function(e) {
        close_progress_modal()
        log_msg(paste("Optimization Step 2 Error:", e$message), "error")
        shinyalert::shinyalert("Error", paste("Step 2 failed:", e$message), type = "error")
      })
    })

    # 1.3 Apply Optimized Parameters to Sidebar
    observeEvent(input$apply_opt_params, {
      req(state$opt_table)
      df <- state$opt_table
      get_val <- function(param_name) {
        row <- df[df$para == param_name, ]
        if(nrow(row) == 0) return(NULL)
        # Prioritize Positive, then Negative for display in single inputs
        if("Positive" %in% names(row) && !is.na(row$Positive)) return(row$Positive)
        if("Negative" %in% names(row) && !is.na(row$Negative)) return(row$Negative)
        return(NULL)
      }

      updateTextInput(session, "ppm", value = get_val("ppm"))
      updateTextInput(session, "snthresh", value = get_val("snthresh"))
      updateTextInput(session, "noise", value = get_val("noise"))
      updateTextInput(session, "p_min", value = get_val("p_min"))
      updateTextInput(session, "p_max", value = get_val("p_max"))

      shinyalert::shinyalert("Applied", "Optimization parameters have been applied to the sidebar inputs.", type = "success")
    })

    # 1.4 Execution (Peak Picking)
    observeEvent(input$action2, {
      req(state$raw_ms1_dir, prj_init$mass_dataset_dir)
      struct <- check_ms1_structure(state$raw_ms1_dir)

      # Helper to get parameters from sidebar inputs (manual control)
      get_p <- function(param) {
        val <- input[[param]]
        if(is.null(val) || val == "") {
          # Fallback defaults if UI is somehow empty
          defaults <- list(ppm=15, snthresh=10, noise=500, threads=1, p_min=5, p_max=30, min_fraction=0.5, pre_left=3, pre_right=500)
          return(as.numeric(defaults[[param]]))
        }
        return(as.numeric(val))
      }

      show_progress_modal("Peak Picking", "Initializing...", 5)

      tryCatch({
        withCallingHandlers({
          if(struct$pos) {
            update_progress_modal(20, "Processing POS Mode...")
            massprocesser::process_data(
              path = file.path(state$raw_ms1_dir, "POS"),
              polarity = "positive",
              ppm = get_p("ppm"),
              snthresh = get_p("snthresh"),
              noise = get_p("noise"),
              threads = get_p("threads"),
              peakwidth = c(get_p("p_min"), get_p("p_max")),
              prefilter = c(get_p("pre_left"), get_p("pre_right")),
              min_fraction = get_p("min_fraction"),
              fill_peaks = FALSE
            )
            res_file <- file.path(state$raw_ms1_dir, "POS", "Result", "object")
            if(file.exists(res_file)) {
              load(res_file)
              data_import$object_pos_raw <- object
              global_data$object_pos_raw <- object # SYNC TO GLOBAL
              object_pos_raw <- data_import$object_pos_raw
              save(object_pos_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_pos_raw.rda"))
            }
          }

          if(struct$neg) {
            update_progress_modal(60, "Processing NEG Mode...")
            massprocesser::process_data(
              path = file.path(state$raw_ms1_dir, "NEG"),
              polarity = "negative",
              ppm = get_p("ppm"),
              snthresh = get_p("snthresh"),
              noise = get_p("noise"),
              threads = get_p("threads"),
              peakwidth = c(get_p("p_min"), get_p("p_max")),
              prefilter = c(get_p("pre_left"), get_p("pre_right")),
              min_fraction = get_p("min_fraction"),
              fill_peaks = FALSE
            )
            res_file <- file.path(state$raw_ms1_dir, "NEG", "Result", "object")
            if(file.exists(res_file)) {
              load(res_file)
              data_import$object_neg_raw <- object
              global_data$object_neg_raw <- object # SYNC TO GLOBAL
              object_neg_raw <- data_import$object_neg_raw
              save(object_neg_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_neg_raw.rda"))
            }
          }
        }, message = function(m) {
          msg_text <- trimws(m$message)
          if(nchar(msg_text) > 0) log_msg(paste("[Process]", msg_text), "info")
        })

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()

        # Display params used
        state$final_params <- data.frame(
          Parameter = c("ppm", "snthresh", "noise", "peakwidth_min", "peakwidth_max"),
          Value = c(get_p("ppm"), get_p("snthresh"), get_p("noise"), get_p("p_min"), get_p("p_max"))
        )

        shinyalert::shinyalert("Success", "Peak picking completed!", type = "success")

      }, error = function(e) {
        close_progress_modal()
        shinyalert::shinyalert("Error", paste("Peak picking failed:", e$message), type = "error")
        log_msg(paste("Peak Picking Error:", e$message), "error")
      })
    })

    # ==========================================================================
    # 2. TABLE IMPORT & DATASET LOADING (Same as current version)
    # ==========================================================================

    observeEvent(input$expmat_tbl, {
      req(input$expmat_tbl)
      df <- read.csv(input$expmat_tbl$datapath, header = TRUE, stringsAsFactors = FALSE)
      state$tbl_data <- df
      cols <- colnames(df)
      updateSelectInput(session, "exp_vari_id", choices = cols, selected = cols[1])
      updateSelectInput(session, "exp_mz", choices = cols, selected = cols[2])
      updateSelectInput(session, "exp_rt", choices = cols, selected = cols[3])
      updateSelectInput(session, "exp_ion", choices = cols, selected = cols[4])
    })

    observeEvent(input$run_tbl_import, {
      req(state$tbl_data, prj_init$sample_info)
      show_progress_modal("Importing", "Creating MassDataset...", 20)
      log_msg("Starting Table Import...", "info")

      tryCatch({
        df <- state$tbl_data

        update_progress_modal(40, "Processing Data Structure...")
        df_mapped <- df %>% dplyr::rename(
          variable_id = !!input$exp_vari_id, mz = !!input$exp_mz,
          rt = !!input$exp_rt, ion = !!input$exp_ion
        )
        if(input$rt_unit == "minute") df_mapped$rt <- df_mapped$rt * 60

        df_pos <- df_mapped %>% dplyr::filter(grepl("pos|\\+", ion, ignore.case = TRUE))
        df_neg <- df_mapped %>% dplyr::filter(grepl("neg|\\-", ion, ignore.case = TRUE))

        create_obj <- function(sub_df) {
          if(nrow(sub_df) == 0) return(NULL)
          exp_cols <- setdiff(colnames(sub_df), c("variable_id", "mz", "rt", "ion"))
          valid_samples <- intersect(exp_cols, prj_init$sample_info$sample_id)
          if(length(valid_samples) == 0) return(NULL)

          sample_info_filtered <- prj_init$sample_info %>% dplyr::filter(sample_id %in% valid_samples)

          expression_data <- sub_df %>%
            dplyr::select(variable_id, all_of(sample_info_filtered$sample_id)) %>%
            tibble::column_to_rownames("variable_id")
          variable_info <- sub_df %>% dplyr::select(variable_id, mz, rt)

          massdataset::create_mass_dataset(
            expression_data = expression_data,
            sample_info = sample_info_filtered,
            variable_info = variable_info
          )
        }

        update_progress_modal(60, "Creating Positive Object...")
        if(nrow(df_pos) > 0) {
          data_import$object_pos_raw <- create_obj(df_pos)
          if(!is.null(data_import$object_pos_raw)) {
            update_progress_modal(70, "Saving Positive Object...")
            object_pos_raw <- data_import$object_pos_raw
            global_data$object_pos_raw <- object_pos_raw # SYNC TO GLOBAL
            save(object_pos_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_pos_tbl.rda"))
            log_msg("POS Table Imported.", "success")
          }
        }

        update_progress_modal(80, "Creating Negative Object...")
        if(nrow(df_neg) > 0) {
          data_import$object_neg_raw <- create_obj(df_neg)
          if(!is.null(data_import$object_neg_raw)) {
            update_progress_modal(90, "Saving Negative Object...")
            object_neg_raw <- data_import$object_neg_raw
            global_data$object_neg_raw <- object_neg_raw # SYNC TO GLOBAL
            save(object_neg_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_neg_tbl.rda"))
            log_msg("NEG Table Imported.", "success")
          }
        }

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyalert::shinyalert("Success", "Table imported successfully!", type = "success")

      }, error = function(e) {
        close_progress_modal()
        shinyalert::shinyalert("Error", e$message, type = "error")
        log_msg(paste("Table Import Error:", e$message), "error")
      })
    })

    observeEvent(input$check_mass_obj, {
      req(prj_init$wd)
      log_msg("Loading Mass Dataset RDA...", "info")

      if(!is.null(input$pos_obj_mass)) {
        res <- validate_file(input$pos_obj_mass$datapath, "positive", "POS File")
        if(res$success) {
          data_import$object_pos_raw <- res$object
          global_data$object_pos_raw <- res$object # SYNC TO GLOBAL
          object_pos_raw <- data_import$object_pos_raw
          save(object_pos_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_pos_raw.rda"))
          log_msg("POS RDA Loaded.", "success")
        }
      }

      if(!is.null(input$neg_obj_mass)) {
        res <- validate_file(input$neg_obj_mass$datapath, "negative", "NEG File")
        if(res$success) {
          data_import$object_neg_raw <- res$object
          global_data$object_neg_raw <- res$object # SYNC TO GLOBAL
          object_neg_raw <- data_import$object_neg_raw
          save(object_neg_raw, file = file.path(prj_init$mass_dataset_dir, "01.object_neg_raw.rda"))
          log_msg("NEG RDA Loaded.", "success")
        }
      }
      shinyalert::shinyalert("Loaded", "Mass datasets loaded.", type = "success")
    })

    # Optimization Outputs
    get_combined_plot <- reactive({
      req(state$opt_results)
      plt_list <- list()
      if(!is.null(state$opt_results$pos)) plt_list$pos <- state$opt_results$pos$plot + ggtitle("POS PPM")
      if(!is.null(state$opt_results$neg)) plt_list$neg <- state$opt_results$neg$plot + ggtitle("NEG PPM")
      if(length(plt_list) > 0) patchwork::wrap_plots(plt_list, ncol = 1) else NULL
    })

    output$ppmCut_plt <- renderPlot({
      p <- get_combined_plot()
      if(is.null(p)) ggplot() + annotate("text", x=0.5, y=0.5, label="Run Optimization First") + theme_void()
      else p
    })

    output$download_opt_plot <- downloadHandler(
      filename = function() { paste0("ppm_cutoff_", format(Sys.time(), "%Y%m%d"), ".pdf") },
      content = function(file) {
        req(get_combined_plot())
        ggsave(file, plot = get_combined_plot(), width = 8, height = 10)
      }
    )

    output$parameters_opt_tbl <- DT::renderDataTable({
      req(state$opt_table)
      DT::datatable(state$opt_table, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_opt_table <- downloadHandler(
      filename = function() { paste0("opt_params_", format(Sys.time(), "%Y%m%d"), ".csv") },
      content = function(file) {
        req(state$opt_table)
        write.csv(state$opt_table, file, row.names = FALSE)
      }
    )

    # General Outputs
    output$obj_mass_check.pos <- check_massdata_info(reactive(data_import$object_pos_raw), "positive")
    output$obj_mass_check.neg <- check_massdata_info(reactive(data_import$object_neg_raw), "negative")
    output$status_pos_summary <- check_massdata_info(reactive(data_import$object_pos_raw), "positive")
    output$status_neg_summary <- check_massdata_info(reactive(data_import$object_neg_raw), "negative")
    output$storage_path_display <- renderText({ if(!is.null(prj_init$mass_dataset_dir)) prj_init$mass_dataset_dir else "Not set" })

    output$parameters_final <- DT::renderDataTable({
      req(state$final_params)
      DT::datatable(state$final_params, options = list(pageLength = 10, scrollX = TRUE))
    })

    # Separate Preview outputs for Raw mode to avoid ID collision if needed,
    # but here using shared reactive is fine if logic is clean.
    # To correspond to new UI IDs: 'preview_expmat' and 'preview_varinfo'
    preview_obj <- reactive({
      if(!is.null(data_import$object_pos_raw)) return(data_import$object_pos_raw)
      if(!is.null(data_import$object_neg_raw)) return(data_import$object_neg_raw)
      return(NULL)
    })

    output$preview_expmat <- DT::renderDataTable({
      req(preview_obj())
      exp <- massdataset::extract_expression_data(preview_obj())
      DT::datatable(head(exp, 100), options = list(scrollX = TRUE))
    })

    output$preview_varinfo <- DT::renderDataTable({
      req(preview_obj())
      var_info <- massdataset::extract_variable_info(preview_obj())
      DT::datatable(head(var_info, 100), options = list(scrollX = TRUE))
    })

    # For TBL mode inputs (using suffixes to avoid ID collision if both panels exist)
    output$preview_expmat_tbl <- DT::renderDataTable({
      req(preview_obj())
      exp <- massdataset::extract_expression_data(preview_obj())
      DT::datatable(head(exp, 100), options = list(scrollX = TRUE))
    })

    output$preview_varinfo_tbl <- DT::renderDataTable({
      req(preview_obj())
      var_info <- massdataset::extract_variable_info(preview_obj())
      DT::datatable(head(var_info, 100), options = list(scrollX = TRUE))
    })

    return(data_import)
  })
}
