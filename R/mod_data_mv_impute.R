#' Missing Value Imputation UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom shinyWidgets switchInput
mod_data_impute_ui <- function(id) {
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
          title = "MV Imputation",
          width = 350,
          bg = "#f8f9fa",

          # 1. Method Selection
          selectInput(ns("impute_method"), "Imputation Method:",
                      choices = c(
                        "KNN" = "knn",
                        "Random Forest (MissForest)" = "rf",
                        "BPCA" = "bpca",
                        "PPCA" = "ppca",
                        "SVD Impute" = "svdImpute",
                        "Mean" = "mean",
                        "Median" = "median",
                        "Zero" = "zero",
                        "Minimum" = "minimum"
                      ),
                      selected = "knn"),
          tags$hr(),

          # 2. Dynamic Parameters
          tags$div(class = "param-section",

                   # > KNN Parameters
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'knn'", ns("impute_method")),
                     tags$h6(class="fw-bold text-primary", "KNN Parameters"),
                     numericInput(ns("knn_k"), "k (Neighbors)", value = 10, min = 1, step = 1),
                     numericInput(ns("knn_rowmax"), "rowmax (Max MV per row)", value = 0.5, min = 0, max = 1, step = 0.1),
                     numericInput(ns("knn_colmax"), "colmax (Max MV per col)", value = 0.8, min = 0, max = 1, step = 0.1),
                     numericInput(ns("knn_maxp"), "maxp", value = 1500, min = 100, step = 100),
                     numericInput(ns("knn_seed"), "RNG Seed", value = 362436069)
                   ),

                   # > Random Forest Parameters
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'rf'", ns("impute_method")),
                     tags$h6(class="fw-bold text-success", "Random Forest Parameters"),
                     numericInput(ns("rf_maxiter"), "Max Iterations", value = 10, min = 1, step = 1),
                     numericInput(ns("rf_ntree"), "Number of Trees", value = 100, min = 10, step = 10),
                     selectInput(ns("rf_decreasing"), "Decreasing Order", choices = c("TRUE", "FALSE"), selected = "FALSE")
                   ),

                   # > BPCA / PPCA / SVD Parameters
                   # Note: nPcs is common for PCA-based methods
                   conditionalPanel(
                     condition = sprintf("['bpca', 'ppca', 'svdImpute'].includes(input['%s'])", ns("impute_method")),
                     tags$h6(class = "fw-bold", style = "color: #343a40;",
                             "PCA/SVD Parameters"),
                     numericInput(ns("pca_nPcs"), "Number of PCs (nPcs)", value = 2, min = 1, step = 1),

                     # BPCA specific
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'bpca'", ns("impute_method")),
                       numericInput(ns("bpca_maxSteps"), "Max Steps", value = 100, min = 10, step = 10),
                       numericInput(ns("bpca_threshold"), "Threshold", value = 1e-04, step = 1e-05)
                     )
                   ),

                   # > Simple Methods (Info only)
                   conditionalPanel(
                     condition = sprintf("['mean', 'median', 'zero', 'minimum'].includes(input['%s'])", ns("impute_method")),
                     helpText("No additional parameters required for this method.")
                   )
          ),

          tags$hr(),
          actionButton(ns("run_impute"), "Run Imputation", icon = icon("magic"), class = "btn-teal w-100 fw-bold shadow-sm")
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

            # Row 2: Status
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Imputation Status"),
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

            # Row 3: Data Preview
            tags$h5(class="text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Expression Data Preview (Imputed)"),
            bslib::card(
              height = "500px",
              bslib::card_header("Expression Matrix", class = "bg-light"),
              DT::dataTableOutput(ns("preview_data"))
            )
        )
      )
    )
  )
}

#' Missing Value Imputation Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues. Expects `object_pos_outlier` and `object_neg_outlier`.
#' @param prj_init Project init reactive object (for resuming tasks).
#' @noRd
#' @importFrom utils head
mod_data_impute_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Input Helpers (Resuming Logic)
    # Priority: Outlier Removed Object > Init Object (if resumed from Impute)
    get_input_obj <- function(mode) {
      if(mode == "positive") {
        if(!is.null(global_data$object_pos_outlier)) return(global_data$object_pos_outlier)
        if(!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      } else {
        if(!is.null(global_data$object_neg_outlier)) return(global_data$object_neg_outlier)
        if(!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
        return(NULL)
      }
    }

    observe({
      global_data$missing_value_advisor_state <- list(
        available = TRUE,
        impute_method = input$impute_method %||% "knn",
        parameters = list(
          knn = list(
            k = input$knn_k %||% 10,
            rowmax = input$knn_rowmax %||% 0.5,
            colmax = input$knn_colmax %||% 0.8,
            maxp = input$knn_maxp %||% 1500,
            rng_seed = input$knn_seed %||% 362436069
          ),
          rf = list(
            maxiter = input$rf_maxiter %||% 10,
            ntree = input$rf_ntree %||% 100,
            decreasing = input$rf_decreasing %||% "FALSE"
          ),
          pca = list(
            nPcs = input$pca_nPcs %||% 2,
            bpca_maxSteps = input$bpca_maxSteps %||% 100,
            bpca_threshold = input$bpca_threshold %||% 1e-04
          )
        ),
        input_objects = list(
          positive_available = !is.null(get_input_obj("positive")),
          negative_available = !is.null(get_input_obj("negative"))
        ),
        output_objects = list(
          positive_available = !is.null(global_data$object_pos_impute),
          negative_available = !is.null(global_data$object_neg_impute)
        ),
        updated_at = as.character(Sys.time())
      )
    })

    # 2. Method Summary Output
    output$method_summary <- renderText({
      m <- input$impute_method
      info <- paste("Selected Method:", toupper(m))

      params <- switch(m,
                       "knn" = sprintf("k=%s, rowmax=%s, colmax=%s, maxp=%s, seed=%s",
                                       input$knn_k, input$knn_rowmax, input$knn_colmax, input$knn_maxp, input$knn_seed),
                       "rf"  = sprintf("maxiter=%s, ntree=%s, decreasing=%s",
                                       input$rf_maxiter, input$rf_ntree, input$rf_decreasing),
                       "bpca" = sprintf("nPcs=%s, maxSteps=%s, threshold=%s",
                                        input$pca_nPcs, input$bpca_maxSteps, input$bpca_threshold),
                       "ppca" = sprintf("nPcs=%s", input$pca_nPcs),
                       "svdImpute" = sprintf("nPcs=%s", input$pca_nPcs),
                       "No additional parameters"
      )

      paste(info, "\nParameters:", params)
    })

    # 3. Execution Logic
    observeEvent(input$run_impute, {
      # Check existence
      pos_obj <- get_input_obj("positive")
      neg_obj <- get_input_obj("negative")

      if(is.null(pos_obj) && is.null(neg_obj)) {
        shinyalert::shinyalert(
          title = "Data Missing",
          text = "No mass_dataset objects found. Please ensure data is available from previous steps (Outlier Detection).",
          type = "error"
        )
        return()
      }

      shinyjs::disable("run_impute")
      progress <- shiny::Progress$new()
      progress$set(message = "Imputing Missing Values...", value = 0)
      on.exit({
        progress$close()
        shinyjs::enable("run_impute")
      })

      tryCatch({
        method <- input$impute_method

        # Prepare arguments list dynamically
        args_list <- list(method = method)

        if(method == "knn") {
          args_list <- c(args_list, list(k = input$knn_k, rowmax = input$knn_rowmax,
                                         colmax = input$knn_colmax, maxp = input$knn_maxp,
                                         rng.seed = input$knn_seed))
        } else if(method == "rf") {
          args_list <- c(args_list, list(maxiter = input$rf_maxiter, ntree = input$rf_ntree,
                                         decreasing = as.logical(input$rf_decreasing)))
        } else if(method == "bpca") {
          args_list <- c(args_list, list(nPcs = input$pca_nPcs, maxSteps = input$bpca_maxSteps,
                                         threshold = input$bpca_threshold))
        } else if(method %in% c("ppca", "svdImpute")) {
          args_list <- c(args_list, list(nPcs = input$pca_nPcs))
        }

        polarities <- list(
          list(name = "positive", obj = pos_obj,
               global_key = "object_pos_impute",
               save_var = "object_pos_impute",
               save_file = "04.object_pos_impute.rda"),
          list(name = "negative", obj = neg_obj,
               global_key = "object_neg_impute",
               save_var = "object_neg_impute",
               save_file = "04.object_neg_impute.rda")
        )

        for (p in polarities) {
          if (!is.null(p$obj)) {
            progress$inc(if (p$name == "positive") 0.3 else 0.4,
                         detail = paste("Processing", p$name, "Mode..."))
            args_mode <- c(list(object = p$obj), args_list)
            imputed <- do.call(masscleaner::impute_mv, args_mode)

            global_data[[p$global_key]] <- imputed

            if (!is.null(prj_init$mass_dataset_dir)) {
              assign(p$save_var, imputed)
              save(list = p$save_var,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file))
            }
          }
        }

        progress$inc(0.3, detail = "Done!")
        shinyalert::shinyalert("Imputation Completed", "Missing values have been imputed successfully.", type = "success")

      }, error = function(e) {
        shinyalert::shinyalert("Imputation Failed", paste("Error:", e$message), type = "error")
      })
    })

    # 4. Status Outputs
    output$status_pos <- check_massdata_info(reactive(global_data$object_pos_impute), "positive")
    output$status_neg <- check_massdata_info(reactive(global_data$object_neg_impute), "negative")

    # 5. Preview Output (Expression Data)
    preview_obj <- reactive({
      if(!is.null(global_data$object_pos_impute)) return(global_data$object_pos_impute)
      if(!is.null(global_data$object_neg_impute)) return(global_data$object_neg_impute)
      return(NULL)
    })

    output$preview_data <- DT::renderDataTable({
      req(preview_obj())
      # Showing Expression Data (head 100 rows)
      DT::datatable(head(massdataset::extract_expression_data(preview_obj()), 100),
                    options = list(scrollX = TRUE, pageLength = 10))
    })

  })
}
