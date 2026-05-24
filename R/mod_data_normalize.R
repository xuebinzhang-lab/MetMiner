#' Normalization & Integration UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom shinyWidgets switchInput prettyCheckbox
mod_data_norm_ui <- function(id) {
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
          title = "Norm & Integration",
          width = 350,
          bg = "#f8f9fa",

          # 1. Normalization Settings
          tags$h6(class="fw-bold text-primary", "1. Normalization"),
          selectInput(ns("norm_method"), "Method:",
                      choices = c(
                        "SVR (Support Vector Regression)" = "svr",
                        "PQN (Probabilistic Quotient)" = "pqn",
                        "Total Sum" = "total",
                        "Median" = "median",
                        "Mean" = "mean",
                        "LOESS" = "loess"
                      ),
                      selected = "svr"),

          # Dynamic Norm Params
          tags$div(class = "param-section",
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'svr'", ns("norm_method")),
                     checkboxInput(ns("svr_opt"), "Parameter Optimization", value = TRUE),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == true", ns("svr_opt")),
                       numericInput(ns("svr_begin"), "Begin", value = 0.5, step = 0.1),
                       numericInput(ns("svr_end"), "End", value = 1.0, step = 0.1),
                       numericInput(ns("svr_step"), "Step", value = 0.2, step = 0.1),
                       numericInput(ns("svr_multiple"), "Multiple", value = 1, step = 1)
                     )
                   ),
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'pqn'", ns("norm_method")),
                     selectInput(ns("pqn_ref"), "Reference:", choices = c("median", "mean"), selected = "median")
                   ),
                   checkboxInput(ns("keep_scale"), "Keep Scale", value = TRUE)
          ),

          tags$hr(),

          # 2. Integration Settings
          tags$h6(class="fw-bold text-success", "2. Integration (Batch)"),
          helpText("Applied automatically if > 1 batches detected."),
          selectInput(ns("integ_method"), "Method:",
                      choices = c(
                        "QC Mean" = "qc_mean",
                        "QC Median" = "qc_median",
                        "Subject Mean" = "subject_mean",
                        "Subject Median" = "subject_median"
                      ),
                      selected = "qc_mean"),

          tags$hr(),
          actionButton(ns("run_norm"), "Run Processing", icon = icon("play"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        # --- Main Content ---
        div(class = "p-3",

            # Row 1: Method Summary
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("info-circle"), " Method Summary"),
            div(class = "card bg-light border-start border-primary border-3 mb-4",
                div(class = "card-body py-2",
                    verbatimTextOutput(ns("method_summary"), placeholder = TRUE)
                )
            ),

            # Row 2: MassDataset Status
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Processing Status"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_header("Positive Mode Status", class = "bg-info-subtle text-info-emphasis"),
                verbatimTextOutput(ns("status_pos"), placeholder = TRUE),
                style = "min-height: 200px;"
              ),
              bslib::card(
                bslib::card_header("Negative Mode Status", class = "bg-warning-subtle text-warning-emphasis"),
                verbatimTextOutput(ns("status_neg"), placeholder = TRUE),
                style = "min-height: 200px;"
              )
            ),

            br(),

            # Row 3: PCA Comparison (Before vs After)
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("graph-up"), " PCA Comparison (Before vs After)"),
            bslib::layout_column_wrap(
              width = 1/2,
              heights_equal = "row",

              # Positive PCA Card
              bslib::card(
                height = "600px",
                full_screen = TRUE,
                bslib::card_header("Positive Mode PCA"),
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    open = FALSE, bg = "#f8f9fa", width = 250,
                    title = "Plot Controls",
                    checkboxInput(ns("pos_pca_scale"), "Log2 + Scale", value = TRUE),
                    selectInput(ns("pos_pca_color"), "Color By", choices = NULL),
                    sliderInput(ns("pos_pca_alpha"), "Point Alpha", 0.1, 1, 0.8),
                    sliderInput(ns("pos_pca_size"), "Point Size", 1, 10, 3),
                    tags$hr(),
                    numericInput(ns("pos_pca_w"), "Width (in)", 10),
                    numericInput(ns("pos_pca_h"), "Height (in)", 6),
                    downloadButton(ns("dl_pos_pca"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  plotOutput(ns("plt_pos_pca"), height = "100%")
                )
              ),

              # Negative PCA Card
              bslib::card(
                height = "600px",
                full_screen = TRUE,
                bslib::card_header("Negative Mode PCA"),
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    open = FALSE, bg = "#f8f9fa", width = 250,
                    title = "Plot Controls",
                    checkboxInput(ns("neg_pca_scale"), "Log2 + Scale", value = TRUE),
                    selectInput(ns("neg_pca_color"), "Color By", choices = NULL),
                    sliderInput(ns("neg_pca_alpha"), "Point Alpha", 0.1, 1, 0.8),
                    sliderInput(ns("neg_pca_size"), "Point Size", 1, 10, 3),
                    tags$hr(),
                    numericInput(ns("neg_pca_w"), "Width (in)", 10),
                    numericInput(ns("neg_pca_h"), "Height (in)", 6),
                    downloadButton(ns("dl_neg_pca"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  plotOutput(ns("plt_neg_pca"), height = "100%")
                )
              )
            ),

            br(),

            # Row 4: RSD Comparison (Before vs After)
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("bar-chart-line"), " QC RSD Comparison"),
            bslib::layout_column_wrap(
              width = 1/2,
              heights_equal = "row",

              # Positive RSD Card
              bslib::card(
                height = "600px",
                full_screen = TRUE,
                bslib::card_header("Positive Mode QC RSD"),
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    open = FALSE, bg = "#f8f9fa", width = 250,
                    title = "Plot Controls",
                    selectInput(ns("pos_rsd_color"), "Color By", choices = NULL),
                    numericInput(ns("pos_rsd_cutoff"), "RSD Cutoff (%)", value = 30),
                    checkboxInput(ns("pos_rsd_desc"), "Descending", value = FALSE),
                    tags$hr(),
                    numericInput(ns("pos_rsd_w"), "Width (in)", 10),
                    numericInput(ns("pos_rsd_h"), "Height (in)", 6),
                    downloadButton(ns("dl_pos_rsd"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  plotOutput(ns("plt_pos_rsd"), height = "100%")
                )
              ),

              # Negative RSD Card
              bslib::card(
                height = "600px",
                full_screen = TRUE,
                bslib::card_header("Negative Mode QC RSD"),
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    open = FALSE, bg = "#f8f9fa", width = 250,
                    title = "Plot Controls",
                    selectInput(ns("neg_rsd_color"), "Color By", choices = NULL),
                    numericInput(ns("neg_rsd_cutoff"), "RSD Cutoff (%)", value = 30),
                    checkboxInput(ns("neg_rsd_desc"), "Descending", value = FALSE),
                    tags$hr(),
                    numericInput(ns("neg_rsd_w"), "Width (in)", 10),
                    numericInput(ns("neg_rsd_h"), "Height (in)", 6),
                    downloadButton(ns("dl_neg_rsd"), "Download PDF", class = "btn-sm btn-outline-primary w-100")
                  ),
                  plotOutput(ns("plt_neg_rsd"), height = "100%")
                )
              )
            ),

            br(),

            # Row 5: Data Preview with Download
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Data Preview (Normalized)"),
            bslib::card(
              bslib::card_header("Expression Matrix Preview", class = "bg-light"),
              bslib::navset_card_tab(

                bslib::nav_panel(
                  title = "Positive Mode",
                  div(class = "d-flex justify-content-end mb-2",
                      downloadButton(ns("dl_pos_csv"), "Download CSV", class = "btn-sm btn-success")
                  ),
                  DT::dataTableOutput(ns("preview_pos"))
                ),

                bslib::nav_panel(
                  title = "Negative Mode",
                  div(class = "d-flex justify-content-end mb-2",
                      downloadButton(ns("dl_neg_csv"), "Download CSV", class = "btn-sm btn-success")
                  ),
                  DT::dataTableOutput(ns("preview_neg"))
                )
              )
            )
        )
      )
    )
  )
}

#' Normalization & Integration Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues. Expects `object_pos_impute` and `object_neg_impute`.
#' @param prj_init Project init reactive object (for resuming tasks).
#' @noRd
#' @importFrom utils head
mod_data_norm_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal   <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal  <- progress_handlers$close_progress_modal

    # --- 1. Input Helpers (Resuming Logic) ---
    get_input_obj <- function(mode) {
      if(mode == "positive") {
        if(!is.null(global_data$object_pos_impute)) return(global_data$object_pos_impute)
        if(!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      } else {
        if(!is.null(global_data$object_neg_impute)) return(global_data$object_neg_impute)
        if(!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
        return(NULL)
      }
    }

    observe({
      global_data$normalization_advisor_state <- list(
        available = TRUE,
        normalization_method = input$norm_method %||% NA_character_,
        integration_method = input$integ_method %||% NA_character_,
        keep_scale = isTRUE(input$keep_scale),
        svr = list(
          optimization = isTRUE(input$svr_opt),
          begin = input$svr_begin %||% NA_real_,
          end = input$svr_end %||% NA_real_,
          step = input$svr_step %||% NA_real_,
          multiple = input$svr_multiple %||% NA_real_
        ),
        pqn_reference = input$pqn_ref %||% NA_character_,
        plot_controls = list(
          positive = list(pca_color = input$pos_pca_color %||% NA_character_, rsd_color = input$pos_rsd_color %||% NA_character_),
          negative = list(pca_color = input$neg_pca_color %||% NA_character_, rsd_color = input$neg_rsd_color %||% NA_character_)
        ),
        input_objects = list(
          positive_available = !is.null(get_input_obj("positive")),
          negative_available = !is.null(get_input_obj("negative"))
        ),
        output_objects = list(
          positive_available = !is.null(global_data$object_pos_norm),
          negative_available = !is.null(global_data$object_neg_norm)
        ),
        updated_at = as.character(Sys.time())
      )
    })

    # Update UI choices based on input data
    observe({
      for(mode in c("positive", "negative")) {
        obj <- get_input_obj(mode)
        if(!is.null(obj)) {
          s_info <- massdataset::extract_sample_info(obj)
          cols <- colnames(s_info)
          col_choices <- setdiff(cols, c("sample_id", "raw_file_name"))

          # Update PCA and RSD Color choices
          prefix <- if(mode == "positive") "pos" else "neg"
          updateSelectInput(session, paste0(prefix, "_pca_color"), choices = col_choices, selected = "class")
          updateSelectInput(session, paste0(prefix, "_rsd_color"), choices = col_choices, selected = "class")
        }
      }
    })

    # --- 2. Method Summary ---
    output$method_summary <- renderText({
      n_m <- input$norm_method
      i_m <- input$integ_method

      base_txt <- paste0("Normalization: ", toupper(n_m))
      if(n_m == "svr") {
        base_txt <- paste0(base_txt, sprintf(" (Opt=%s, Range=%.1f-%.1f)",
                                             input$svr_opt, input$svr_begin, input$svr_end))
      } else if(n_m == "pqn") {
        base_txt <- paste0(base_txt, " (Ref=", input$pqn_ref, ")")
      }

      base_txt <- paste0(base_txt, "\nIntegration (Conditional): ", toupper(i_m),
                         "\nNote: Integration only runs if multiple batches are detected.")

      return(base_txt)
    })

    # --- 3. Processing Logic Pipeline ---
    process_norm_integ <- function(object, n_method, i_method, n_args) {
      if(is.null(object)) return(list(result=NULL, log=""))

      # Step A: Normalization
      args_norm <- c(list(object = object, method = n_method), n_args)
      args_norm$threads <- 1

      obj_norm <- do.call(masscleaner::normalize_data, args_norm)

      # Step B: Integration Check
      s_info <- massdataset::extract_sample_info(obj_norm)
      log_txt <- paste("Normalization:", n_method, "completed.")

      if("batch" %in% colnames(s_info) && length(unique(s_info$batch)) > 1) {
        # Run Integration
        obj_final <- masscleaner::integrate_data(obj_norm, method = i_method)
        log_txt <- paste(log_txt, "\nIntegration: Batches detected (>1). Method:", i_method, "applied.")
      } else {
        obj_final <- obj_norm
        log_txt <- paste(log_txt, "\nIntegration: Skipped (Single batch or no batch info).")
      }

      return(list(result = obj_final, log = log_txt))
    }

    # --- 4. Execution ---
    observeEvent(input$run_norm, {
      pos_in <- get_input_obj("positive")
      neg_in <- get_input_obj("negative")

      if(is.null(pos_in) && is.null(neg_in)) {
        shinyalert::shinyalert("Data Missing", "No input data found.", type = "error")
        return()
      }

      shinyjs::disable("run_norm")
      show_progress_modal("Processing", "Normalizing Data...", 0)

      tryCatch({
        # Prepare Norm Args
        n_args <- list(keep_scale = input$keep_scale)
        if(input$norm_method == "svr") {
          n_args$optimization <- input$svr_opt
          if(input$svr_opt) {
            n_args$begin <- input$svr_begin
            n_args$end <- input$svr_end
            n_args$step <- input$svr_step
            n_args$multiple <- input$svr_multiple
          }
        } else if(input$norm_method == "pqn") {
          n_args$pqn_reference <- input$pqn_ref
        }

        polarities <- list(
          list(name = "positive", obj = pos_in,
               global_key = "object_pos_norm",
               save_var = "object_pos_norm",
               save_file = "05.object_pos_norm.rda",
               progress_val = 30),
          list(name = "negative", obj = neg_in,
               global_key = "object_neg_norm",
               save_var = "object_neg_norm",
               save_file = "05.object_neg_norm.rda",
               progress_val = 60)
        )

        for (p in polarities) {
          if (!is.null(p$obj)) {
            update_progress_modal(p$progress_val, paste("Processing", p$name, "Mode..."))
            res <- process_norm_integ(p$obj, input$norm_method, input$integ_method, n_args)
            global_data[[p$global_key]] <- res$result

            if (!is.null(prj_init$mass_dataset_dir)) {
              assign(p$save_var, res$result)
              save(list = p$save_var,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file))
            }
          }
        }

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyjs::enable("run_norm")
        shinyalert::shinyalert("Processing Completed", "Normalization and Integration (if applicable) finished.", type = "success")

      }, error = function(e) {
        close_progress_modal()
        shinyjs::enable("run_norm")
        shinyalert::shinyalert("Error", paste("Processing failed:", e$message), type = "error")
      })
    })

    # 5. Status Output
    output$status_pos <- check_massdata_info(reactive(global_data$object_pos_norm), "positive")
    output$status_neg <- check_massdata_info(reactive(global_data$object_neg_norm), "negative")

    # --- 6. Comparison Plots (PCA & RSD) ---

    # Generic PCA Plot Generator
    make_pca_plot <- function(obj, color_by, do_scale, pt_alpha, pt_size) {
      tryCatch({
        temp <- obj
        temp <- log(temp + 1, 2)
        if(do_scale) {
          ed <- temp@expression_data
          scaled <- t(scale(t(ed)))
          temp@expression_data <- as.data.frame(scaled)
        }
        massqc::massqc_pca(temp, color_by = color_by, order_by = "sample_id",
                           point_alpha = pt_alpha, point_size = pt_size)
      }, error = function(e) ggplot2::ggplot() + ggplot2::theme_void())
    }

    # Compare PCA
    render_pca_compare <- function(obj_before, obj_after, color_by, do_scale, pt_alpha, pt_size) {
      if(is.null(obj_before) || is.null(obj_after)) return(NULL)
      p1 <- make_pca_plot(obj_before, color_by, do_scale, pt_alpha, pt_size) +
        ggplot2::ggtitle("Before") + ggplot2::theme(legend.position = "none")
      p2 <- make_pca_plot(obj_after, color_by, do_scale, pt_alpha, pt_size) +
        ggplot2::ggtitle("After")
      patchwork::wrap_plots(p1, p2, ncol = 2)
    }

    output$plt_pos_pca <- renderPlot({
      req(get_input_obj("positive"), global_data$object_pos_norm)
      render_pca_compare(get_input_obj("positive"), global_data$object_pos_norm,
                         input$pos_pca_color, input$pos_pca_scale, input$pos_pca_alpha, input$pos_pca_size)
    })

    output$plt_neg_pca <- renderPlot({
      req(get_input_obj("negative"), global_data$object_neg_norm)
      render_pca_compare(get_input_obj("negative"), global_data$object_neg_norm,
                         input$neg_pca_color, input$neg_pca_scale, input$neg_pca_alpha, input$neg_pca_size)
    })

    # Downloads for PCA
    output$dl_pos_pca <- downloadHandler(
      filename = "norm_pca_pos.pdf",
      content = function(file) {
        req(get_input_obj("positive"), global_data$object_pos_norm)
        p <- render_pca_compare(get_input_obj("positive"), global_data$object_pos_norm,
                                input$pos_pca_color, input$pos_pca_scale, input$pos_pca_alpha, input$pos_pca_size)
        ggplot2::ggsave(file, p, width = input$pos_pca_w, height = input$pos_pca_h)
      }
    )

    output$dl_neg_pca <- downloadHandler(
      filename = "norm_pca_neg.pdf",
      content = function(file) {
        req(get_input_obj("negative"), global_data$object_neg_norm)
        p <- render_pca_compare(get_input_obj("negative"), global_data$object_neg_norm,
                                input$neg_pca_color, input$neg_pca_scale, input$neg_pca_alpha, input$neg_pca_size)
        ggplot2::ggsave(file, p, width = input$neg_pca_w, height = input$neg_pca_h)
      }
    )

    # Generic RSD Plot Generator (QC Only usually?)
    # Using massqc_rsd_plot
    render_rsd_compare <- function(obj_before, obj_after, color_by, cutoff, desc) {
      if(is.null(obj_before) || is.null(obj_after)) return(NULL)

      p1 <- tryCatch({
        massqc::massqc_rsd_plot(obj_before, color_by = 'rsd', order_by = "variable_id") +
          ggplot2::ggtitle("Before") +
          ggplot2::geom_hline(yintercept = cutoff, linetype=2, color="red")
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5, label=paste("RSD Plot Error (Before):\n", e$message), hjust=0.5) +
          ggplot2::theme_void() +
          ggplot2::theme(plot.background = ggplot2::element_rect(color = "red", fill = "white", size = 2))
      })

      p2 <- tryCatch({
        massqc::massqc_rsd_plot(obj_after, color_by = 'rsd', order_by = "variable_id") +
          ggplot2::ggtitle("After") +
          ggplot2::geom_hline(yintercept = cutoff, linetype=2, color="red")
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5, label=paste("RSD Plot Error (After):\n", e$message), hjust=0.5) +
          ggplot2::theme_void() +
          ggplot2::theme(plot.background = ggplot2::element_rect(color = "red", fill = "white", size = 2))
      })

      patchwork::wrap_plots(p1, p2, ncol = 2)
    }

    output$plt_pos_rsd <- renderPlot({
      req(get_input_obj("positive"), global_data$object_pos_norm)
      render_rsd_compare(get_input_obj("positive"), global_data$object_pos_norm,
                         input$pos_rsd_color, input$pos_rsd_cutoff, input$pos_rsd_desc)
    })

    output$plt_neg_rsd <- renderPlot({
      req(get_input_obj("negative"), global_data$object_neg_norm)
      render_rsd_compare(get_input_obj("negative"), global_data$object_neg_norm,
                         input$neg_rsd_color, input$neg_rsd_cutoff, input$neg_rsd_desc)
    })

    # Downloads for RSD
    output$dl_pos_rsd <- downloadHandler(
      filename = "norm_rsd_pos.pdf",
      content = function(file) {
        req(get_input_obj("positive"), global_data$object_pos_norm)
        p <- render_rsd_compare(get_input_obj("positive"), global_data$object_pos_norm,
                                input$pos_rsd_color, input$pos_rsd_cutoff, input$pos_rsd_desc)
        ggplot2::ggsave(file, p, width = input$pos_rsd_w, height = input$pos_rsd_h)
      }
    )

    output$dl_neg_rsd <- downloadHandler(
      filename = "norm_rsd_neg.pdf",
      content = function(file) {
        req(get_input_obj("negative"), global_data$object_neg_norm)
        p <- render_rsd_compare(get_input_obj("negative"), global_data$object_neg_norm,
                                input$neg_rsd_color, input$neg_rsd_cutoff, input$neg_rsd_desc)
        ggplot2::ggsave(file, p, width = input$neg_rsd_w, height = input$neg_rsd_h)
      }
    )

    # --- 7. Data Preview & Download ---

    # Positive
    output$preview_pos <- DT::renderDataTable({
      req(global_data$object_pos_norm)
      DT::datatable(head(massdataset::extract_expression_data(global_data$object_pos_norm), 100),
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_pos_csv <- downloadHandler(
      filename = "normalized_data_pos.csv",
      content = function(file) {
        req(global_data$object_pos_norm)
        write.csv(massdataset::extract_expression_data(global_data$object_pos_norm), file)
      }
    )

    # Negative
    output$preview_neg <- DT::renderDataTable({
      req(global_data$object_neg_norm)
      DT::datatable(head(massdataset::extract_expression_data(global_data$object_neg_norm), 100),
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_neg_csv <- downloadHandler(
      filename = "normalized_data_neg.csv",
      content = function(file) {
        req(global_data$object_neg_norm)
        write.csv(massdataset::extract_expression_data(global_data$object_neg_norm), file)
      }
    )

  })
}
