#' Metabolite Annotation UI Module
#'
#' @param id Module id.
#' @noRd
mod_annotation_ui <- function(id) {
  ns <- NS(id)
  plantcyc_choices <- metminer_builtin_plantcyc_annotation_databases()
  zma_idx <- grep("Zea mays", names(plantcyc_choices))
  default_plantcyc <- if (length(zma_idx) > 0) {
    plantcyc_choices[zma_idx[1]]
  } else if (length(plantcyc_choices) > 0) {
    plantcyc_choices[1]
  } else {
    character()
  }
  public_choices <- metminer_public_annotation_databases()
  plantcyc_choice_html <- metminer_annotation_database_label_html(names(plantcyc_choices))

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

          tags$h6(class = "fw-bold text-primary", "1. Layered Databases"),
          textInput(
            ns("kegg_database_dir"),
            "Layer 1 KEGG DB folder (.rda):",
            value = "",
            placeholder = "Temp/kegg_zma_database"
          ),
          shinyWidgets::pickerInput(
            inputId = ns("plantcyc_database"),
            label = "Layer 1 PlantCyc DB:",
            choices = stats::setNames(unname(plantcyc_choices), names(plantcyc_choices)),
            selected = unname(default_plantcyc),
            multiple = FALSE,
            choicesOpt = list(content = plantcyc_choice_html),
            options = shinyWidgets::pickerOptions(
              `live-search` = TRUE,
              size = 8,
              title = "Choose one plant species"
            )
          ),
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("public_databases"),
            label = "Layer 2 public MS2 DB:",
            choices = public_choices,
            selected = character(),
            icon = icon("check"),
            status = "success"
          ),
          textInput(ns("custom_database_dir"), "Customized DB folder (.rda):", value = "",
                    placeholder = "/path/to/tidymass/database_folder"),
          tags$small(class = "text-muted d-block mb-2",
                     "Four database inputs are independent: KEGG DB folder, PlantCyc DB, public MS2 DB, and customized DB. Customized DB is optional and can be empty."),

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

    observe({
      global_data$annotation_advisor_state <- list(
        available = TRUE,
        plantcyc_database = input$plantcyc_database %||% character(),
        public_databases = input$public_databases %||% character(),
        kegg_database_dir = input$kegg_database_dir %||% NA_character_,
        custom_database_dir = input$custom_database_dir %||% NA_character_,
        column = input$column %||% NA_character_,
        ms1_ppm = input$ms1_ppm %||% NA_real_,
        ms2_ppm = input$ms2_ppm %||% NA_real_,
        rt_tolerance_sec = input$rt_tol %||% NA_real_,
        candidate_num = input$candidate_num %||% NA_integer_,
        threads = input$threads %||% NA_integer_,
        input_objects = list(
          positive_available = !is.null(get_input_obj("positive")),
          negative_available = !is.null(get_input_obj("negative"))
        ),
        output_objects = list(
          positive_available = !is.null(global_data$object_pos_annotated),
          negative_available = !is.null(global_data$object_neg_annotated)
        ),
        status = list(positive = annotation_status$pos, negative = annotation_status$neg),
        updated_at = as.character(Sys.time())
      )
    })

    # ---- Run annotation for one polarity ----
    run_annotation_polarity <- function(mode, obj_in, databases, database_labels) {
      polarity <- if (mode == "positive") "positive" else "negative"
      prepared <- metminer_prepare_annotation_input(obj_in, mode = polarity)
      if (length(prepared$removed_qc_ids) > 0) {
        update_progress_modal(
          if (mode == "positive") 38 else 73,
          paste0(tools::toTitleCase(mode), " mode: ", prepared$message)
        )
      }
      updated <- metminer_annotate_mass_dataset(
        object = prepared$object, databases = databases, polarity = polarity,
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
        selected_builtin <- c(input$plantcyc_database %||% character(), input$public_databases %||% character())
        has_local_dir <- any(has_text(c(
          input$kegg_database_dir %||% NA_character_,
          input$custom_database_dir %||% NA_character_
        )))
        if (length(selected_builtin) == 0 && !has_local_dir) {
          stop("Please select at least one annotation database: KEGG folder, PlantCyc DB, public MS2 DB, or customized DB.", call. = FALSE)
        }

        databases <- metminer_collect_annotation_databases(
          builtin_ids = selected_builtin,
          kegg_dir = input$kegg_database_dir,
          custom_dir = input$custom_database_dir
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
        msg <- conditionMessage(e)
        parent <- e$parent
        parent_messages <- character()
        while (!is.null(parent)) {
          parent_msg <- conditionMessage(parent)
          if (has_text(parent_msg)) {
            parent_messages <- c(parent_messages, parent_msg)
          }
          parent <- parent$parent
        }
        if (length(parent_messages) > 0) {
          msg <- paste(c(msg, parent_messages), collapse = " | ")
        }
        if (!has_text(msg)) msg <- "Unknown annotation error. Please check database selection and input objects."
        shinyalert::shinyalert("Error", paste("Annotation failed:", msg), type = "error")
      })
    })

    # ---- Outputs ----

    output$method_summary <- renderText({
      selected_plantcyc <- input$plantcyc_database %||% character()
      selected_public <- input$public_databases %||% character()
      selected_plantcyc_labels <- metminer_annotation_database_labels(selected_plantcyc)
      selected_public_labels <- metminer_annotation_database_labels(selected_public)
      kegg_dir <- input$kegg_database_dir %||% ""
      kegg_text <- if (has_text(kegg_dir)) as.character(kegg_dir)[1] else "None"
      custom_dir <- input$custom_database_dir %||% ""
      custom_text <- if (has_text(custom_dir)) as.character(custom_dir)[1] else "None"
      paste0(
        "Layer 1 KEGG DB folder: ", kegg_text, "\n",
        "Layer 1 PlantCyc DB: ", if (length(selected_plantcyc_labels) > 0) paste(selected_plantcyc_labels, collapse = ", ") else "None", "\n",
        "Layer 1 policy: KEGG/PlantCyc candidates require strict core-adduct support during filtering\n",
        "Layer 2 public MS2 DB: ", if (length(selected_public_labels) > 0) paste(selected_public_labels, collapse = ", ") else "None", "\n",
        "Customized DB folder: ", custom_text, "\n",
        "Annotation sample policy: QC samples are removed from mass_dataset before annotation.\n",
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
      DT::datatable(metminer_format_annotation_table_for_display(global_data$object_pos_annotated, mode = "positive"),
                    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$table_neg <- DT::renderDataTable({
      req(global_data$object_neg_annotated)
      DT::datatable(metminer_format_annotation_table_for_display(global_data$object_neg_annotated, mode = "negative"),
                    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$download_pos_annotation <- downloadHandler(
      filename = "positive_annotation_table.csv",
      content = function(file) {
        req(global_data$object_pos_annotated)
        utils::write.csv(metminer_format_annotation_table_for_display(global_data$object_pos_annotated, mode = "positive"), file, row.names = FALSE)
      }
    )

    output$download_neg_annotation <- downloadHandler(
      filename = "negative_annotation_table.csv",
      content = function(file) {
        req(global_data$object_neg_annotated)
        utils::write.csv(metminer_format_annotation_table_for_display(global_data$object_neg_annotated, mode = "negative"), file, row.names = FALSE)
      }
    )
  })
}
