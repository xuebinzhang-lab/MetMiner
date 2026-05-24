#' Remove Noise UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom shinyWidgets switchInput
mod_data_rm_noise_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        # --- Sidebar: Global Parameters ---
        sidebar = bslib::sidebar(
          title = "Noise Removal Settings",
          width = 350,
          bg = "#f8f9fa",

          # 1. Grouping Column
          selectInput(ns("noise_tag"), "Group Samples By:",
                      choices = NULL, # To be filled server-side
                      selected = "class"),
          helpText("Column used to group samples (e.g., 'class', 'group'). QC samples are identified by 'class=QC'."),

          tags$hr(),

          # 0. Blank-informed intensity masking
          tags$h6(class = "fw-bold", style = "color: #343a40;",
                  "0. Blank-Informed Intensity Masking"),
          checkboxInput(ns("rm_intensity"), "Mask low-confidence intensities", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("rm_intensity")),
            selectInput(ns("intensity_method"), "Method",
                        choices = c(
                          "Blank-informed (recommended)" = "blank",
                          "Distribution fallback (advanced, no blanks)" = "distribution"
                        ),
                        selected = "blank"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'blank'", ns("intensity_method")),
              numericInput(ns("blank_sd_mult"), "Blank SD Multiplier", value = 3, min = 1, max = 10, step = 0.5),
              helpText("Masks non-blank sample values below blank_mean + multiplier * blank_SD, calculated per feature.")
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'distribution'", ns("intensity_method")),
              numericInput(ns("pct_fallback"), "Percentile Fallback", value = 0.01, min = 0.001, max = 0.1, step = 0.005),
              helpText("Advanced fallback for datasets without blanks. Inspect results carefully because low-abundance metabolites can be masked.")
            ),
            helpText("Optional post-picking masking. This complements XCMS noise/prefilter/snthresh and lets the MV filter remove features with too many masked values.")
          ),

          tags$hr(),

          # 1. Blank Ratio Filter
          tags$h6(class = "fw-bold", style = "color: #343a40;",
                  "1. Blank Ratio Filter"),
          checkboxInput(ns("rm_blank_ratio"), "Apply Blank Ratio Filter", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("rm_blank_ratio")),
            selectInput(ns("blank_label"), "Blank Sample Label", choices = NULL),
            selectInput(ns("sample_label"), "Reference Sample Label", choices = NULL),
            numericInput(ns("blank_ratio"), "Ratio Threshold >", value = 3, min = 1, max = 20, step = 0.5),
            helpText("Features where mean(sample) / mean(blank) < threshold are removed as blank-dominated background.")
          ),

          tags$hr(),

          # 2. MV Thresholds
          tags$h6(class = "fw-bold", style = "color: #343a40;",
                  "2. MV Filter Parameters"),
          numericInput(ns("qc_na_freq"), "QC Missing Ratio Threshold <", value = 0.2, min = 0, max = 1, step = 0.05),
          numericInput(ns("s_na_freq"), "Group Missing Ratio Threshold <", value = 0.5, min = 0, max = 1, step = 0.05),

          tags$div(class="text-muted small mb-3",
                   "Note: Features are KEPT if (QC_MV <= Threshold) AND (At least one Group_MV <= Threshold)."),

          tags$hr(),

          # 3. RSD Setting
          tags$h6(class = "fw-bold", style = "color: #343a40;",
                  "3. RSD Filter (QC)"),
          checkboxInput(ns("rm_noise_rsd"), "Apply RSD Filter", value = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("rm_noise_rsd")),
            numericInput(ns("qc_rsd"), "QC RSD Threshold (%) <", value = 30, min = 0, max = 100, step = 5),
            helpText("Features with RSD in QC samples higher than this value will be removed.")
          ),

          tags$hr(),
          actionButton(ns("run_cleaning"), "Run Noise Removal", icon = icon("play"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        # --- Main Content ---
        div(class = "p-3",

            # 1. Summary Section (Updated to match project_init style)
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("clipboard-data"), " Cleaning Summary"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("Positive Mode Log", class = "bg-info-subtle text-info-emphasis"),
                # MassDataset Summary
                verbatimTextOutput(ns("res_pos_mod"), placeholder = TRUE),
                # Removed Features Detail
                tags$div(class="mt-3",
                         tags$label("Removed Noise Features:", class="form-label text-muted fw-bold"),
                         verbatimTextOutput(ns("noise_detail_pos"), placeholder = TRUE)
                ),
                style = "min-height: 250px;"
              ),
              bslib::card(
                bslib::card_header("Negative Mode Log", class = "bg-warning-subtle text-warning-emphasis"),
                # MassDataset Summary
                verbatimTextOutput(ns("res_neg_mod"), placeholder = TRUE),
                # Removed Features Detail
                tags$div(class="mt-3",
                         tags$label("Removed Noise Features:", class="form-label text-muted fw-bold"),
                         verbatimTextOutput(ns("noise_detail_neg"), placeholder = TRUE)
                ),
                style = "min-height: 250px;"
              )
            ),

            br(),

            # 2. Comparison Plots (Revised Structure with Full Screen)
            bslib::layout_column_wrap(
              width = 1/2,
              heights_equal = "row",

              # --- Positive Mode Card ---
              bslib::card(
                height = "650px",
                full_screen = TRUE, # Added full_screen
                bslib::card_header(bsicons::bs_icon("graph-up"), " Positive Mode: MV Distribution"),
                bslib::layout_sidebar(
                  fillable = TRUE,
                  sidebar = bslib::sidebar(
                    width = 250,
                    open = FALSE, # Collapsed by default
                    bg = "#f8f9fa",
                    title = "Plot Controls",

                    # 1. Interactive Switch
                    shinyWidgets::switchInput(inputId = ns("pos_interactive"), label = "Interactive", size = "small", inline = TRUE, labelWidth = "80px"),
                    tags$hr(),

                    # 2. Plot Aesthetics
                    selectInput(ns("pos_color"), "Color By", choices = NULL),
                    selectInput(ns("pos_order"), "Order By", choices = NULL),
                    checkboxInput(ns("pos_pct"), "Show Percentage", value = TRUE),
                    tags$hr(),

                    # 3. Download Params
                    numericInput(ns("pos_w"), "Width (in)", value = 10, min = 4),
                    numericInput(ns("pos_h"), "Height (in)", value = 8, min = 4),
                    downloadButton(ns("dl_plot_pos"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  # Main Plot Area
                  uiOutput(ns("ui_pos_plots"))
                )
              ),

              # --- Negative Mode Card ---
              bslib::card(
                height = "650px",
                full_screen = TRUE, # Added full_screen
                bslib::card_header(bsicons::bs_icon("graph-up"), " Negative Mode: MV Distribution"),
                bslib::layout_sidebar(
                  fillable = TRUE,
                  sidebar = bslib::sidebar(
                    width = 250,
                    open = FALSE,
                    bg = "#f8f9fa",
                    title = "Plot Controls",

                    # 1. Interactive Switch
                    shinyWidgets::switchInput(inputId = ns("neg_interactive"), label = "Interactive", size = "small", inline = TRUE, labelWidth = "80px"),
                    tags$hr(),

                    # 2. Plot Aesthetics
                    selectInput(ns("neg_color"), "Color By", choices = NULL),
                    selectInput(ns("neg_order"), "Order By", choices = NULL),
                    checkboxInput(ns("neg_pct"), "Show Percentage", value = TRUE),
                    tags$hr(),

                    # 3. Download Params
                    numericInput(ns("neg_w"), "Width (in)", value = 10, min = 4),
                    numericInput(ns("neg_h"), "Height (in)", value = 8, min = 4),
                    downloadButton(ns("dl_plot_neg"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  # Main Plot Area
                  uiOutput(ns("ui_neg_plots"))
                )
              )
            )
        )
      ),

      if(exists("metminer_footer")) metminer_footer() else tags$div()
    )
  )
}

#' Remove Noise Server Module
#'
#' @param id Module id.
#' @param global_data A reactiveValues object containing 'object_pos_raw' etc.
#' @param prj_init Project init reactive object (for resuming tasks)
#' @noRd
mod_data_rm_noise_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Helpers with Resume Check ---
    get_init_obj <- function(mode = c("positive", "negative")) {
      mode <- match.arg(mode)
      if(mode == "positive") {
        if(!is.null(global_data$object_pos_raw)) return(global_data$object_pos_raw)
        if(!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      } else {
        if(!is.null(global_data$object_neg_raw)) return(global_data$object_neg_raw)
        if(!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
        return(NULL)
      }
    }

    # Store removed noise count
    noise_info <- reactiveValues(
      pos_count = NULL,
      neg_count = NULL
    )

    observe({
      global_data$noise_filter_advisor_state <- list(
        available = TRUE,
        group_samples_by = input$noise_tag %||% NA_character_,
        qc_missing_ratio_threshold = input$qc_na_freq %||% NA_real_,
        group_missing_ratio_threshold = input$s_na_freq %||% NA_real_,
        use_qc_rsd_filter = isTRUE(input$rm_noise_rsd),
        qc_rsd_cutoff = input$qc_rsd %||% NA_real_,
        use_blank_ratio_filter = isTRUE(input$rm_blank_ratio),
        blank_label = input$blank_label %||% NA_character_,
        sample_label = input$sample_label %||% NA_character_,
        blank_ratio_cutoff = input$blank_ratio %||% NA_real_,
        use_blank_informed_intensity_masking = isTRUE(input$rm_intensity),
        intensity_method = input$intensity_method %||% NA_character_,
        blank_sd_multiplier = input$blank_sd_mult %||% NA_real_,
        percentile_fallback = input$pct_fallback %||% NA_real_,
        removed_feature_counts = list(
          positive = noise_info$pos_count %||% NA_integer_,
          negative = noise_info$neg_count %||% NA_integer_
        ),
        input_objects = list(
          positive_available = !is.null(get_init_obj("positive")),
          negative_available = !is.null(get_init_obj("negative"))
        ),
        output_objects = list(
          positive_available = !is.null(global_data$object_pos_clean),
          negative_available = !is.null(global_data$object_neg_clean)
        ),
        updated_at = as.character(Sys.time())
      )
    })

    # --- 2. Update UI (Choices) ---
    observe({
      obj <- get_init_obj("positive")
      if(!is.null(obj)) {
        info <- massdataset::extract_sample_info(obj)
        cols <- colnames(info)

        # Valid columns for Coloring (Discrete usually)
        color_cols <- setdiff(cols, c("sample_id", "raw_file_name"))
        # Valid columns for Ordering (Can be anything)
        order_cols <- cols

        # 1. Update Global Cleaning Group
        updateSelectInput(session, "noise_tag", choices = color_cols, selected = "class")

        # 1b. Update Blank Ratio Label Selectors
        class_values <- unique(info$class)
        updateSelectInput(session, "blank_label", choices = class_values,
                          selected = if("Blank" %in% class_values) "Blank" else class_values[1])
        updateSelectInput(session, "sample_label", choices = class_values,
                          selected = if("Subject" %in% class_values) "Subject" else class_values[1])

        # 2. Update Positive Plot Controls
        updateSelectInput(session, "pos_color", choices = color_cols, selected = "class")
        # Prefer injection.order, fallback to sample_id
        def_order <- if("injection.order" %in% order_cols) "injection.order" else "sample_id"
        updateSelectInput(session, "pos_order", choices = order_cols, selected = def_order)

        # 3. Update Negative Plot Controls
        updateSelectInput(session, "neg_color", choices = color_cols, selected = "class")
        updateSelectInput(session, "neg_order", choices = order_cols, selected = def_order)
      }
    })

    # --- 3. Cleaning Logic Pipeline ---
    do_cleaning_pipeline <- function(object, tag, qc_na_freq, s_na_freq, do_rsd, rsd_cut, polarity_name,
                                      do_blank = FALSE, blank_label = "Blank", sample_label = "Subject", ratio_cutoff = 3,
                                      do_intensity = FALSE, intensity_method = "blank", blank_sd_mult = 3, pct_fallback = 0.01) {
      if (is.null(object)) return(NULL)

      # Step 0: Optional blank-informed intensity masking.
      if (do_intensity) {
        object <- find_noise_intensity(
          object               = object,
          blank_label          = blank_label,
          method               = intensity_method,
          blank_sd_multiplier  = blank_sd_mult,
          percentile_fallback  = pct_fallback
        )
        if (is.null(object) || nrow(object) == 0) {
          showNotification(
            paste("Warning:", polarity_name, "mode: No features kept after intensity filter."),
            type = "warning"
          )
          return(NULL)
        }
      }

      # Step 1: Blank Ratio Filter
      if (do_blank) {
        object <- find_noise_blank(
          object        = object,
          blank_label   = blank_label,
          sample_label  = sample_label,
          ratio_cutoff  = ratio_cutoff
        )
        if (is.null(object) || nrow(object) == 0) {
          showNotification(
            paste("Warning:", polarity_name, "mode: No features kept after blank ratio filter."),
            type = "warning"
          )
          return(NULL)
        }
      }

      result <- find_noise_multiple(
        object      = object,
        tag         = tag,
        qc_na_freq  = qc_na_freq,
        S_na_freq   = s_na_freq,
        do_rsd      = do_rsd,
        rsd_cutoff  = rsd_cut
      )

      if (is.null(result)) {
        showNotification(
          paste("Warning:", polarity_name, "mode: No features kept after MV filter."),
          type = "warning"
        )
        return(NULL)
      }

      return(result)
    }

    # --- 4. Execution ---
    observeEvent(input$run_cleaning, {
      # CHECK FOR DATA EXISTENCE
      pos_raw <- get_init_obj("positive")
      neg_raw <- get_init_obj("negative")

      # FIX: Check if BOTH are null (allow single mode)
      if(is.null(pos_raw) && is.null(neg_raw)) {
        shinyalert::shinyalert(
          title = "Data Missing",
          text = "No mass_dataset objects found (Positive or Negative). Please ensure data is available from previous steps.",
          type = "error"
        )
        return()
      }

      shinyjs::disable("run_cleaning")
      progress <- shiny::Progress$new()
      progress$set(message = "Removing Noise...", value = 0)
      on.exit({
        progress$close()
        shinyjs::enable("run_cleaning")
      })

      tryCatch({
        tag <- input$noise_tag
        qc_cut <- input$qc_na_freq
        s_cut <- input$s_na_freq
        do_rsd <- input$rm_noise_rsd
        rsd_cut <- input$qc_rsd
        do_intensity <- input$rm_intensity
        intensity_method <- input$intensity_method
        blank_sd_mult <- input$blank_sd_mult
        pct_fallback <- input$pct_fallback
        do_blank <- input$rm_blank_ratio
        blank_label <- input$blank_label
        sample_label <- input$sample_label
        ratio_cutoff <- input$blank_ratio

        # Process each polarity mode
        polarities <- list(
          list(name = "positive", raw = pos_raw,
               global_key = "object_pos_clean", noise_key = "pos_count",
               save_var = "object_pos_mv",
               save_file = "02.object_pos_mv.rda"),
          list(name = "negative", raw = neg_raw,
               global_key = "object_neg_clean", noise_key = "neg_count",
               save_var = "object_neg_mv",
               save_file = "02.object_neg_mv.rda")
        )

        for (p in polarities) {
          if (!is.null(p$raw)) {
            progress$inc(if (p$name == "positive") 0.2 else 0.4,
                         detail = paste("Processing", p$name, "Mode..."))
            clean_obj <- do_cleaning_pipeline(p$raw, tag, qc_cut, s_cut, do_rsd, rsd_cut, p$name,
                                                  do_blank, blank_label, sample_label, ratio_cutoff,
                                                  do_intensity, intensity_method, blank_sd_mult, pct_fallback)
            global_data[[p$global_key]] <- clean_obj

            if (!is.null(clean_obj)) {
              noise_info[[p$noise_key]] <- nrow(p$raw) - nrow(clean_obj)
              assign(p$save_var, clean_obj)
              save(list = p$save_var,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file))
            }
          }
        }

        showNotification("Data cleaning completed successfully!", type = "success")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # --- 5. Plots & UI Logic ---

    # Internal plot generator with custom params
    generate_mv_plot <- function(obj, title_suffix, color_by, order_by, pct) {
      if(is.null(obj)) return(NULL)
      # Validate inputs
      if(is.null(color_by) || color_by == "") color_by <- "class"
      if(is.null(order_by) || order_by == "") order_by <- "sample_id"

      p <- massdataset::show_sample_missing_values(
        obj,
        color_by = color_by,
        order_by = order_by,
        percentage = pct
      )

      p <- p + ggplot2::ggtitle(title_suffix) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
      return(p)
    }

    # > Positive Mode UI
    output$ui_pos_plots <- renderUI({
      bslib::layout_columns(
        col_widths = c(6, 6),
        if(input$pos_interactive) plotly::plotlyOutput(ns("ply_pos_before")) else plotOutput(ns("plt_pos_before")),
        if(input$pos_interactive) plotly::plotlyOutput(ns("ply_pos_after")) else plotOutput(ns("plt_pos_after"))
      )
    })

    # > Negative Mode UI
    output$ui_neg_plots <- renderUI({
      bslib::layout_columns(
        col_widths = c(6, 6),
        if(input$neg_interactive) plotly::plotlyOutput(ns("ply_neg_before")) else plotOutput(ns("plt_neg_before")),
        if(input$neg_interactive) plotly::plotlyOutput(ns("ply_neg_after")) else plotOutput(ns("plt_neg_after"))
      )
    })

    # > Rendering Positive (Passing specific POS inputs)
    # Static
    output$plt_pos_before <- renderPlot({ req(get_init_obj("positive")); generate_mv_plot(get_init_obj("positive"), "Before (Raw)", input$pos_color, input$pos_order, input$pos_pct) })
    output$plt_pos_after <- renderPlot({ req(global_data$object_pos_clean); generate_mv_plot(global_data$object_pos_clean, "After (Clean)", input$pos_color, input$pos_order, input$pos_pct) })
    # Interactive
    output$ply_pos_before <- plotly::renderPlotly({ req(get_init_obj("positive")); plotly::ggplotly(generate_mv_plot(get_init_obj("positive"), "Before (Raw)", input$pos_color, input$pos_order, input$pos_pct)) })
    output$ply_pos_after <- plotly::renderPlotly({ req(global_data$object_pos_clean); plotly::ggplotly(generate_mv_plot(global_data$object_pos_clean, "After (Clean)", input$pos_color, input$pos_order, input$pos_pct)) })

    # > Rendering Negative (Passing specific NEG inputs)
    # Static
    output$plt_neg_before <- renderPlot({ req(get_init_obj("negative")); generate_mv_plot(get_init_obj("negative"), "Before (Raw)", input$neg_color, input$neg_order, input$neg_pct) })
    output$plt_neg_after <- renderPlot({ req(global_data$object_neg_clean); generate_mv_plot(global_data$object_neg_clean, "After (Clean)", input$neg_color, input$neg_order, input$neg_pct) })
    # Interactive
    output$ply_neg_before <- plotly::renderPlotly({ req(get_init_obj("negative")); plotly::ggplotly(generate_mv_plot(get_init_obj("negative"), "Before (Raw)", input$neg_color, input$neg_order, input$neg_pct)) })
    output$ply_neg_after <- plotly::renderPlotly({ req(global_data$object_neg_clean); plotly::ggplotly(generate_mv_plot(global_data$object_neg_clean, "After (Clean)", input$neg_color, input$neg_order, input$neg_pct)) })

    # --- 6. Result Summary ---
    output$res_pos_mod <- check_massdata_info(reactive(global_data$object_pos_clean), "positive")
    output$res_neg_mod <- check_massdata_info(reactive(global_data$object_neg_clean), "negative")

    # Removed Feature Counts
    output$noise_detail_pos <- renderText({
      if(is.null(noise_info$pos_count)) return("Not run yet.")
      paste(noise_info$pos_count, "features removed.")
    })

    output$noise_detail_neg <- renderText({
      if(is.null(noise_info$neg_count)) return("Not run yet.")
      paste(noise_info$neg_count, "features removed.")
    })

    # --- 7. Downloads ---
    # Positive Download
    output$dl_plot_pos <- downloadHandler(
      filename = function() { paste0("noise_removal_pos_", Sys.Date(), ".pdf") },
      content = function(file) {
        p1 <- generate_mv_plot(get_init_obj("positive"), "Before", input$pos_color, input$pos_order, input$pos_pct)
        p2 <- generate_mv_plot(global_data$object_pos_clean, "After", input$pos_color, input$pos_order, input$pos_pct)
        if(!is.null(p1) && !is.null(p2)) {
          p <- patchwork::wrap_plots(p1, p2, ncol = 2)
          ggplot2::ggsave(file, p, width = input$pos_w, height = input$pos_h)
        }
      }
    )

    # Negative Download
    output$dl_plot_neg <- downloadHandler(
      filename = function() { paste0("noise_removal_neg_", Sys.Date(), ".pdf") },
      content = function(file) {
        p1 <- generate_mv_plot(get_init_obj("negative"), "Before", input$neg_color, input$neg_order, input$neg_pct)
        p2 <- generate_mv_plot(global_data$object_neg_clean, "After", input$neg_color, input$neg_order, input$neg_pct)
        if(!is.null(p1) && !is.null(p2)) {
          p <- patchwork::wrap_plots(p1, p2, ncol = 2)
          ggplot2::ggsave(file, p, width = input$neg_w, height = input$neg_h)
        }
      }
    )
  })
}
