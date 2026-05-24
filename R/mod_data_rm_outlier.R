#' Outlier Detection UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom shinyWidgets switchInput prettyCheckboxGroup
mod_data_outlier_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        # --- Sidebar: Settings ---
        sidebar = bslib::sidebar(
          title = "Outlier Detection",
          width = 350,
          bg = "#f8f9fa",

          # 1. Method Selection
          radioButtons(ns("outlier_method"), "Detection Method:",
                       choices = c("TidyMass Recommended" = "By tidymass",
                                   "Manual Selection" = "By myself"),
                       selected = "By tidymass"),
          tags$hr(),

          # 2. Auto (TidyMass) Settings
          conditionalPanel(
            condition = sprintf("input['%s'] == 'By tidymass'", ns("outlier_method")),
            tags$h6(class="fw-bold text-primary", "Criteria Selection:"),
            # process_outliers uses all(judge == TRUE), so sample must fail ALL selected checks to be removed
            helpText("Samples meeting ALL selected criteria will be removed."),
            shinyWidgets::prettyCheckboxGroup(
              inputId = ns("auto_criteria"),
              label = "Select outlier definitions:",
              # Values updated to match masscleaner column names for regex matching
              choices = c(
                "NA Percentage" = "na",         # Matches "according_to_na"
                "SD Fold Change" = "pc_sd",     # Matches "pc_sd"
                "MAD Fold Change" = "pc_mad",   # Matches "pc_mad"
                "PCA Distance" = "distance"     # Matches "according_to_distance"
              ),
              selected = c("na", "distance"),
              icon = icon("check"),
              status = "primary"
            ),

            # Thresholds (Collapsible)
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                title = "Threshold Settings",
                icon = bsicons::bs_icon("sliders"),
                numericInput(ns("th_na"), "NA % Cutoff >", 0.5, 0, 1, 0.1),
                numericInput(ns("th_sd"), "SD Fold Change >", 6, 1, 20, 1),
                numericInput(ns("th_mad"), "MAD Fold Change >", 6, 1, 20, 1),
                numericInput(ns("th_dist"), "PCA Dist P-value <", 0.05, 0, 1, 0.01)
              )
            )
          ),

          # 3. Manual Settings
          conditionalPanel(
            condition = sprintf("input['%s'] == 'By myself'", ns("outlier_method")),
            tags$h6(class="fw-bold text-success", "Manual Removal:"),
            selectInput(ns("manual_pos"), "Pos Mode Outliers:", choices = NULL, multiple = TRUE),
            selectInput(ns("manual_neg"), "Neg Mode Outliers:", choices = NULL, multiple = TRUE)
          ),

          tags$hr(),
          actionButton(ns("run_outlier"), "Run Outlier Removal", icon = icon("trash-alt"), class = "btn-danger w-100 fw-bold shadow-sm")
        ),

        # --- Main Content ---
        div(class = "p-3",

            # Row 1: Summary (Status)
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("clipboard-data"), " Cleaning Summary"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("Positive Mode Status", class = "bg-info-subtle text-info-emphasis"),
                verbatimTextOutput(ns("status_pos"), placeholder = TRUE),
                # Removed Outliers Detail
                tags$div(class="mt-3",
                         tags$label("Removed Outliers (IDs):", class="form-label text-muted fw-bold"),
                         verbatimTextOutput(ns("outlier_detail_pos"), placeholder = TRUE)
                ),
                style = "min-height: 150px;"
              ),
              bslib::card(
                bslib::card_header("Negative Mode Status", class = "bg-warning-subtle text-warning-emphasis"),
                verbatimTextOutput(ns("status_neg"), placeholder = TRUE),
                # Removed Outliers Detail
                tags$div(class="mt-3",
                         tags$label("Removed Outliers (IDs):", class="form-label text-muted fw-bold"),
                         verbatimTextOutput(ns("outlier_detail_neg"), placeholder = TRUE)
                ),
                style = "min-height: 150px;"
              )
            ),

            br(),

            # Row 2: Plots (NA% & PCA)
            bslib::layout_column_wrap(
              width = 1/2,
              heights_equal = "row",

              # --- Positive Mode Card ---
              bslib::card(
                height = "750px",
                full_screen = TRUE,
                bslib::card_header(bsicons::bs_icon("graph-up"), " Positive Mode: Post-Cleaning Check"),
                bslib::layout_sidebar(
                  fillable = TRUE,
                  sidebar = bslib::sidebar(
                    width = 250,
                    open = FALSE,
                    bg = "#f8f9fa",
                    title = "Plot Controls",

                    # Aesthetics
                    selectInput(ns("pos_color"), "Color By", choices = NULL),
                    selectInput(ns("pos_label"), "Label By", choices = NULL),

                    # PCA Settings
                    tags$hr(),
                    tags$h6("PCA Settings", class="fw-bold"),
                    checkboxInput(ns("pos_scale"), "Scale Data (Z-score)", value = TRUE),
                    sliderInput(ns("pos_size"), "Point Size", min = 1, max = 10, value = 3, step = 0.5),
                    sliderInput(ns("pos_alpha"), "Point Alpha", min = 0.1, max = 1, value = 0.8, step = 0.1),
                    tags$hr(),

                    # Download
                    numericInput(ns("pos_w"), "Width (in)", 10, 4),
                    numericInput(ns("pos_h"), "Height (in)", 8, 4),
                    downloadButton(ns("dl_pos"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  bslib::navset_card_tab(
                    bslib::nav_panel("Sample NA %", plotOutput(ns("plt_pos_na"), height = "550px")),
                    bslib::nav_panel("PCA Plot", plotOutput(ns("plt_pos_pca"), height = "550px"))
                  )
                )
              ),

              # --- Negative Mode Card ---
              bslib::card(
                height = "750px",
                full_screen = TRUE,
                bslib::card_header(bsicons::bs_icon("graph-up"), " Negative Mode: Post-Cleaning Check"),
                bslib::layout_sidebar(
                  fillable = TRUE,
                  sidebar = bslib::sidebar(
                    width = 250,
                    open = FALSE,
                    bg = "#f8f9fa",
                    title = "Plot Controls",

                    # Aesthetics
                    selectInput(ns("neg_color"), "Color By", choices = NULL),
                    selectInput(ns("neg_label"), "Label By", choices = NULL),

                    # PCA Settings
                    tags$hr(),
                    tags$h6("PCA Settings", class="fw-bold"),
                    checkboxInput(ns("neg_scale"), "Scale Data (Z-score)", value = TRUE),
                    sliderInput(ns("neg_size"), "Point Size", min = 1, max = 10, value = 3, step = 0.5),
                    sliderInput(ns("neg_alpha"), "Point Alpha", min = 0.1, max = 1, value = 0.8, step = 0.1),
                    tags$hr(),

                    # Download
                    numericInput(ns("neg_w"), "Width (in)", 10, 4),
                    numericInput(ns("neg_h"), "Height (in)", 8, 4),
                    downloadButton(ns("dl_neg"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  bslib::navset_card_tab(
                    bslib::nav_panel("Sample NA %", plotOutput(ns("plt_neg_na"), height = "550px")),
                    bslib::nav_panel("PCA Plot", plotOutput(ns("plt_neg_pca"), height = "550px"))
                  )
                )
              )
            ),

            br(),

            # Row 3: Data Preview
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Data Preview (Cleaned)"),
            bslib::card(
              bslib::card_header("Cleaned Data (Variable Info)", class = "bg-light"),
              DT::dataTableOutput(ns("preview_data"))
            )
        )
      ),

      if(exists("metminer_footer")) metminer_footer() else tags$div()
    )
  )
}

#' Outlier Detection Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues. Expects `object_pos_clean` and `object_neg_clean`.
#' @param prj_init Project init reactive object (for resuming tasks)
#' @noRd
mod_data_outlier_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Helpers & Initialization
    get_input_obj <- function(mode) {
      if(mode == "positive") {
        if(!is.null(global_data$object_pos_clean)) return(global_data$object_pos_clean)
        if(!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      } else {
        if(!is.null(global_data$object_neg_clean)) return(global_data$object_neg_clean)
        if(!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
        return(NULL)
      }
    }

    # Store removed outlier info
    outlier_info <- reactiveValues(
      pos_removed = NULL,
      neg_removed = NULL
    )

    observe({
      global_data$outlier_advisor_state <- list(
        available = TRUE,
        outlier_method = input$outlier_method %||% "By tidymass",
        auto_criteria = input$auto_criteria %||% character(),
        thresholds = list(
          th_na = input$th_na %||% 0.5,
          th_sd = input$th_sd %||% 6,
          th_mad = input$th_mad %||% 6,
          th_dist = input$th_dist %||% 0.05
        ),
        manual_selection = list(
          positive = input$manual_pos %||% character(),
          negative = input$manual_neg %||% character()
        ),
        plot_controls = list(
          positive = list(
            color_by = input$pos_color %||% NA_character_,
            label_by = input$pos_label %||% NA_character_,
            scale = isTRUE(input$pos_scale)
          ),
          negative = list(
            color_by = input$neg_color %||% NA_character_,
            label_by = input$neg_label %||% NA_character_,
            scale = isTRUE(input$neg_scale)
          )
        ),
        removed = list(
          positive = outlier_info$pos_removed %||% character(),
          negative = outlier_info$neg_removed %||% character()
        ),
        input_objects = list(
          positive_available = !is.null(get_input_obj("positive")),
          negative_available = !is.null(get_input_obj("negative"))
        ),
        output_objects = list(
          positive_available = !is.null(global_data$object_pos_outlier),
          negative_available = !is.null(global_data$object_neg_outlier)
        ),
        updated_at = as.character(Sys.time())
      )
    })

    observe({
      # Update Manual Choices & Plot Choices
      for(mode in c("positive", "negative")) {
        obj <- get_input_obj(mode)
        if(!is.null(obj)) {
          s_info <- massdataset::extract_sample_info(obj)
          # Manual choices
          updateSelectInput(session, if(mode=="positive") "manual_pos" else "manual_neg",
                            choices = s_info$sample_id)

          # Plot choices
          cols <- colnames(s_info)
          col_choices <- setdiff(cols, c("sample_id", "raw_file_name"))
          updateSelectInput(session, if(mode=="positive") "pos_color" else "neg_color",
                            choices = col_choices, selected = "class")
          updateSelectInput(session, if(mode=="positive") "pos_label" else "neg_label",
                            choices = cols, selected = "sample_id")
        }
      }
    })

    # 3. Execution
    observeEvent(input$run_outlier, {
      # Check for data existence
      obj_pos <- get_input_obj("positive")
      obj_neg <- get_input_obj("negative")

      if(is.null(obj_pos) && is.null(obj_neg)) {
        shinyalert::shinyalert(
          title = "Data Missing",
          text = "No mass_dataset objects found (Positive or Negative). Please ensure data is available from previous steps.",
          type = "error"
        )
        return()
      }

      shinyjs::disable("run_outlier")
      on.exit(shinyjs::enable("run_outlier"))

      tryCatch({
        polarities <- list(
          list(name = "positive", obj = obj_pos,
               global_key = "object_pos_outlier", outlier_key = "pos_removed",
               manual_input = "manual_pos",
               save_var = "object_pos_outlier",
               save_file = "03.object_pos_outlier.rda"),
          list(name = "negative", obj = obj_neg,
               global_key = "object_neg_outlier", outlier_key = "neg_removed",
               manual_input = "manual_neg",
               save_var = "object_neg_outlier",
               save_file = "03.object_neg_outlier.rda")
        )

        for (p in polarities) {
          if (!is.null(p$obj)) {
            outlier_tbl <- calc_outlier_table(p$obj, input$th_na, input$th_sd, input$th_mad, input$th_dist)
            res <- process_outliers(
              object = p$obj,
              mv_method = input$outlier_method,
              by_witch = input$auto_criteria,
              outlier_samples = input[[p$manual_input]],
              outlier_table = outlier_tbl
            )

            if (!is.null(res$error)) stop(res$error)

            global_data[[p$global_key]] <- res$object
            outlier_info[[p$outlier_key]] <- res$outlier_ids

            if (!is.null(res$object)) {
              assign(p$save_var, res$object)
              save(list = p$save_var,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file))
            }

            showNotification(paste(p$name, ":", res$message),
                             type = if (is.null(res$outlier_ids)) "message" else "warning")
          }
        }

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # 4. Plots (Post-Cleaning)
    generate_plots <- function(obj, color_by, label_by, do_scale, pt_size, pt_alpha) {
      if(is.null(obj)) return(list(na=NULL, pca=NULL))

      # NA Plot
      p_na <- massdataset::show_sample_missing_values(obj, color_by = color_by, order_by = "sample_id", percentage = TRUE) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank())

      # PCA Plot
      # Logic: Log transform -> Scale (Optional) -> PCA
      p_pca <- tryCatch({
        temp <- obj
        # Log transform (log2(x+1))
        # Note: Depending on massdataset version, log() might work directly on object
        # or we might need to extract expression data.
        # Using the style provided in the request:
        temp <- log(temp + 1, 2)

        if(do_scale) {
          ed <- temp@expression_data
          # Scale features (columns of t(ed))
          scaled <- t(scale(t(ed)))
          temp@expression_data <- as.data.frame(scaled)
        }

        # Use massqc::massqc_pca as requested
        massqc::massqc_pca(temp, color_by = color_by, order_by = "sample_id",
                           point_alpha = pt_alpha, point_size = pt_size)

      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5, label=paste("PCA Error:\n", e$message), hjust=0.5) +
          ggplot2::theme_void() +
          ggplot2::theme(plot.background = ggplot2::element_rect(color="red", fill="white"))
      })

      list(na = p_na, pca = p_pca)
    }

    # Render Plots Positive
    output$plt_pos_na <- renderPlot({
      req(global_data$object_pos_outlier)
      generate_plots(global_data$object_pos_outlier, input$pos_color, input$pos_label,
                     input$pos_scale, input$pos_size, input$pos_alpha)$na
    })
    output$plt_pos_pca <- renderPlot({
      req(global_data$object_pos_outlier)
      generate_plots(global_data$object_pos_outlier, input$pos_color, input$pos_label,
                     input$pos_scale, input$pos_size, input$pos_alpha)$pca
    })

    # Render Plots Negative
    output$plt_neg_na <- renderPlot({
      req(global_data$object_neg_outlier)
      generate_plots(global_data$object_neg_outlier, input$neg_color, input$neg_label,
                     input$neg_scale, input$neg_size, input$neg_alpha)$na
    })
    output$plt_neg_pca <- renderPlot({
      req(global_data$object_neg_outlier)
      generate_plots(global_data$object_neg_outlier, input$neg_color, input$neg_label,
                     input$neg_scale, input$neg_size, input$neg_alpha)$pca
    })

    # 5. Status & Preview
    output$status_pos <- check_massdata_info(reactive(global_data$object_pos_outlier), "positive")
    output$status_neg <- check_massdata_info(reactive(global_data$object_neg_outlier), "negative")

    # Removed Outliers Text
    output$outlier_detail_pos <- renderText({
      if(is.null(outlier_info$pos_removed)) return("No outliers removed or not run yet.")
      if(length(outlier_info$pos_removed) == 0) return("None detected.")
      paste(outlier_info$pos_removed, collapse = ", ")
    })

    output$outlier_detail_neg <- renderText({
      if(is.null(outlier_info$neg_removed)) return("No outliers removed or not run yet.")
      if(length(outlier_info$neg_removed) == 0) return("None detected.")
      paste(outlier_info$neg_removed, collapse = ", ")
    })

    output$preview_data <- DT::renderDataTable({
      req(global_data$object_pos_outlier)
      DT::datatable(massdataset::extract_variable_info(global_data$object_pos_outlier),
                    options = list(scrollX = TRUE, pageLength = 5))
    })

    # 6. Downloads
    output$dl_pos <- downloadHandler(
      filename = "pos_outlier_check.pdf",
      content = function(file) {
        plts <- generate_plots(global_data$object_pos_outlier, input$pos_color, input$pos_label,
                               input$pos_scale, input$pos_size, input$pos_alpha)
        p <- patchwork::wrap_plots(plts$na, plts$pca, ncol = 1)
        ggplot2::ggsave(file, p, width = input$pos_w, height = input$pos_h)
      }
    )
    output$dl_neg <- downloadHandler(
      filename = "neg_outlier_check.pdf",
      content = function(file) {
        plts <- generate_plots(global_data$object_neg_outlier, input$neg_color, input$neg_label,
                               input$neg_scale, input$neg_size, input$neg_alpha)
        p <- patchwork::wrap_plots(plts$na, plts$pca, ncol = 1)
        ggplot2::ggsave(file, p, width = input$neg_w, height = input$neg_h)
      }
    )

  })
}
