#' Metabolite Annotation UI Module
#'
#' @param id Module id.
#' @noRd
mod_annotation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "Metabolite Annotation",
          width = 350,
          bg = "#f8f9fa",

          tags$h6(class = "fw-bold text-primary", "1. Databases"),
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("builtin_databases"),
            label = "Built-in massdbbuildin:",
            choices = metminer_builtin_annotation_databases(),
            selected = metminer_builtin_annotation_databases()[1],
            icon = icon("check"),
            status = "primary"
          ),
          textInput(ns("local_database_dir"), "Local database folder (.rda):", value = "",
                    placeholder = "/path/to/tidymass/database_folder"),
          tags$small(class = "text-muted d-block mb-2",
                     "Folder may contain custom metid databaseClass .rda files."),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "2. Matching"),
          selectInput(ns("column"), "LC Column:", choices = c("RP" = "rp", "HILIC" = "hilic"), selected = "rp"),
          numericInput(ns("ms1_ppm"), "MS1 Match (ppm):", value = 25, min = 0.1, step = 0.5),
          numericInput(ns("ms2_ppm"), "MS2 Match (ppm):", value = 30, min = 0.1, step = 0.5),
          numericInput(ns("rt_tol"), "RT Match (sec):", value = 30, min = 0, step = 1),
          numericInput(ns("candidate_num"), "Candidates per feature:", value = 3, min = 1, max = 20, step = 1),
          numericInput(ns("threads"), "Threads:", value = 1, min = 1, step = 1),

          tags$hr(),
          actionButton(ns("run_annotation"), "Run Annotation", icon = icon("tags"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        div(
          class = "p-3",
          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("info-circle"), " Method Summary"),
          div(class = "card bg-light border-start border-primary border-3 mb-4",
              div(class = "card-body py-2", verbatimTextOutput(ns("method_summary"), placeholder = TRUE))),

          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Annotation Status"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(bslib::card_header("Positive Mode Status", class = "bg-info-subtle text-info-emphasis"),
                        verbatimTextOutput(ns("status_pos"), placeholder = TRUE), style = "min-height: 190px;"),
            bslib::card(bslib::card_header("Negative Mode Status", class = "bg-warning-subtle text-warning-emphasis"),
                        verbatimTextOutput(ns("status_neg"), placeholder = TRUE), style = "min-height: 190px;")
          ),

          br(),
          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Annotation Table"),
          bslib::card(
            full_screen = TRUE,
            bslib::navset_tab(
              bslib::nav_panel("Positive Mode",
                               div(class = "d-flex justify-content-end mb-2",
                                   downloadButton(ns("download_pos_annotation"), "Download CSV", class = "btn-sm btn-success")),
                               DT::dataTableOutput(ns("table_pos"))),
              bslib::nav_panel("Negative Mode",
                               div(class = "d-flex justify-content-end mb-2",
                                   downloadButton(ns("download_neg_annotation"), "Download CSV", class = "btn-sm btn-success")),
                               DT::dataTableOutput(ns("table_neg")))
            )
          )
        )
      )
    )
  )
}

#' Metabolite Annotation Server Module
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues data store.
#' @param prj_init Project init reactiveValues.
#' @noRd
mod_annotation_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal <- progress_handlers$close_progress_modal

    get_input_obj <- function(mode) {
      if (mode == "positive") {
        if (!is.null(global_data$object_pos_network)) return(global_data$object_pos_network)
        if (!is.null(global_data$object_pos_norm)) return(global_data$object_pos_norm)
        if (!is.null(global_data$object_pos_impute)) return(global_data$object_pos_impute)
        if (!is.null(global_data$object_pos_outlier)) return(global_data$object_pos_outlier)
        if (!is.null(global_data$object_pos_clean)) return(global_data$object_pos_clean)
        if (!is.null(global_data$object_pos_raw)) return(global_data$object_pos_raw)
        if (!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      }

      if (!is.null(global_data$object_neg_network)) return(global_data$object_neg_network)
      if (!is.null(global_data$object_neg_norm)) return(global_data$object_neg_norm)
      if (!is.null(global_data$object_neg_impute)) return(global_data$object_neg_impute)
      if (!is.null(global_data$object_neg_outlier)) return(global_data$object_neg_outlier)
      if (!is.null(global_data$object_neg_clean)) return(global_data$object_neg_clean)
      if (!is.null(global_data$object_neg_raw)) return(global_data$object_neg_raw)
      if (!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
      NULL
    }

    annotation_status <- reactiveValues(
      pos = metminer_annotation_status(NULL),
      neg = metminer_annotation_status(NULL)
    )

    # ---- Run annotation for one polarity ----
    run_annotation_polarity <- function(mode, obj_in, databases, database_labels) {
      polarity <- if (mode == "positive") "positive" else "negative"
      updated <- metminer_annotate_mass_dataset(
        object = obj_in, databases = databases, polarity = polarity,
        ms1.match.ppm = input$ms1_ppm, ms2.match.ppm = input$ms2_ppm,
        rt.match.tol = input$rt_tol, column = input$column,
        candidate.num = input$candidate_num, threads = input$threads
      )
      status <- metminer_annotation_status(updated, database_labels)

      if (mode == "positive") {
        global_data$object_pos_annotated <- updated
        annotation_status$pos <- status
        if (!is.null(prj_init$mass_dataset_dir)) {
          object_pos_annotated <- updated
          save(object_pos_annotated, file = file.path(prj_init$mass_dataset_dir, "07.object_pos_annotated.rda"))
        }
      } else {
        global_data$object_neg_annotated <- updated
        annotation_status$neg <- status
        if (!is.null(prj_init$mass_dataset_dir)) {
          object_neg_annotated <- updated
          save(object_neg_annotated, file = file.path(prj_init$mass_dataset_dir, "07.object_neg_annotated.rda"))
        }
      }
    }

    # ---- Run annotation ----
    observeEvent(input$run_annotation, {
      pos_in <- get_input_obj("positive")
      neg_in <- get_input_obj("negative")

      if (is.null(pos_in) && is.null(neg_in)) {
        shinyalert::shinyalert("Data Missing", "No mass_dataset object found for annotation.", type = "error")
        return()
      }

      shinyjs::disable("run_annotation")
      show_progress_modal("Annotating", "Loading databases...", 0)

      tryCatch({
        databases <- metminer_collect_annotation_databases(
          builtin_ids = input$builtin_databases,
          local_dir = input$local_database_dir
        )
        database_labels <- vapply(databases, `[[`, character(1), "label")

        if (!is.null(pos_in)) {
          update_progress_modal(35, "Annotating positive mode...")
          run_annotation_polarity("positive", pos_in, databases, database_labels)
        }

        if (!is.null(neg_in)) {
          update_progress_modal(70, "Annotating negative mode...")
          run_annotation_polarity("negative", neg_in, databases, database_labels)
        }

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyjs::enable("run_annotation")
        shinyalert::shinyalert("Annotation Completed", "Metabolite annotation finished.", type = "success")
      }, error = function(e) {
        close_progress_modal()
        shinyjs::enable("run_annotation")
        shinyalert::shinyalert("Error", paste("Annotation failed:", e$message), type = "error")
      })
    })

    # ---- Outputs ----

    output$method_summary <- renderText({
      selected_db <- input$builtin_databases %||% character()
      local_dir <- input$local_database_dir %||% ""
      local_text <- if (has_text(local_dir)) as.character(local_dir)[1] else "None"
      paste0(
        "Built-in databases: ", if (length(selected_db) > 0) paste(selected_db, collapse = ", ") else "None", "\n",
        "Local database folder: ", local_text, "\n",
        "Column: ", toupper(input$column), "\n",
        "MS1 ppm: ", input$ms1_ppm,
        " | MS2 ppm: ", input$ms2_ppm,
        " | RT tolerance: ", input$rt_tol, " sec\n",
        "Candidates per feature: ", input$candidate_num
      )
    })

    output$status_pos <- renderText({
      status <- annotation_status$pos
      if (!is.null(global_data$object_pos_annotated)) {
        status <- metminer_annotation_status(global_data$object_pos_annotated, status$databases)
      }
      metminer_format_annotation_status(status, "positive")
    })

    output$status_neg <- renderText({
      status <- annotation_status$neg
      if (!is.null(global_data$object_neg_annotated)) {
        status <- metminer_annotation_status(global_data$object_neg_annotated, status$databases)
      }
      metminer_format_annotation_status(status, "negative")
    })

    output$table_pos <- DT::renderDataTable({
      req(global_data$object_pos_annotated)
      DT::datatable(metminer_safe_extract_annotation_table(global_data$object_pos_annotated),
                    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$table_neg <- DT::renderDataTable({
      req(global_data$object_neg_annotated)
      DT::datatable(metminer_safe_extract_annotation_table(global_data$object_neg_annotated),
                    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$download_pos_annotation <- downloadHandler(
      filename = "positive_annotation_table.csv",
      content = function(file) {
        req(global_data$object_pos_annotated)
        utils::write.csv(metminer_safe_extract_annotation_table(global_data$object_pos_annotated), file, row.names = FALSE)
      }
    )

    output$download_neg_annotation <- downloadHandler(
      filename = "negative_annotation_table.csv",
      content = function(file) {
        req(global_data$object_neg_annotated)
        utils::write.csv(metminer_safe_extract_annotation_table(global_data$object_neg_annotated), file, row.names = FALSE)
      }
    )
  })
}
