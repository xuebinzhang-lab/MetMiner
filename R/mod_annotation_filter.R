#' Annotation filtering and redundancy removal UI
#'
#' @param id Module id.
#' @noRd
mod_annotation_filter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "Annotation Filter",
          width = 350,
          bg = "#f8f9fa",
          tags$h6(class = "fw-bold text-primary", "Rules"),
          numericInput(ns("high_conf_level"), "High confidence Level <=", value = 2, min = 1, max = 3, step = 1),
          numericInput(ns("cross_rt_tol"), "Positive/Negative RT Tolerance (sec):", value = 10, min = 0, step = 1),
          checkboxInput(ns("use_network"), "Use feature-network validation", value = TRUE),
          tags$hr(),
          actionButton(ns("run_filter"), "Build Non-redundant Table", icon = icon("filter"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        div(
          class = "p-3",
          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Filter Status"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(bslib::card_header("Summary", class = "bg-success-subtle text-success-emphasis"), verbatimTextOutput(ns("status"))),
            bslib::card(bslib::card_header("Rule Notes", class = "bg-light"), verbatimTextOutput(ns("notes")))
          ),
          br(),
          bslib::card(
            full_screen = TRUE,
            bslib::navset_tab(
              bslib::nav_panel(
                "Final Non-redundant Table",
                div(class = "d-flex justify-content-end mb-2",
                    downloadButton(ns("download_final"), "Download CSV", class = "btn-sm btn-success")),
                DT::dataTableOutput(ns("tbl_final"))
              ),
              bslib::nav_panel(
                "Redundancy Audit",
                div(class = "d-flex justify-content-end mb-2",
                    downloadButton(ns("download_audit"), "Download CSV", class = "btn-sm btn-outline-success")),
                DT::dataTableOutput(ns("tbl_audit"))
              )
            )
          )
        )
      )
    )
  )
}

#' Annotation filtering and redundancy removal server
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues data store.
#' @param prj_init Project init reactiveValues.
#' @noRd
mod_annotation_filter_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filter_result <- reactiveVal(NULL)

    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal <- progress_handlers$close_progress_modal

    observeEvent(input$run_filter, {
      pos <- global_data$object_pos_annotated
      neg <- global_data$object_neg_annotated
      if (is.null(pos) && is.null(neg)) {
        shinyalert::shinyalert("Data Missing", "Run metabolite annotation first.", type = "error")
        return()
      }

      shinyjs::disable("run_filter")
      show_progress_modal("Filtering Annotations", "Validating with feature network...", 10)

      tryCatch({
        # Run network validation on a copy — do NOT mutate global_data annotation objects
        if (isTRUE(input$use_network)) {
          update_progress_modal(25, "Network-validating positive mode...")
          if (!is.null(pos)) pos <- metminer_validate_annotations_with_feature_network(pos)
          update_progress_modal(50, "Network-validating negative mode...")
          if (!is.null(neg)) neg <- metminer_validate_annotations_with_feature_network(neg)
        }

        update_progress_modal(70, "Building non-redundant tables...")
        res <- metminer_filter_redundant_annotations(
          positive_object = pos,
          negative_object = neg,
          rt_tolerance = input$cross_rt_tol,
          min_high_conf_level = input$high_conf_level,
          use_network_validation = input$use_network
        )
        filter_result(res)
        global_data$annotation_filter_result <- res

        if (!is.null(prj_init$mass_dataset_dir)) {
          annotation_filter_result <- res
          save(annotation_filter_result, file = file.path(prj_init$mass_dataset_dir, "08.annotation_filter_result.rda"))
        }

        update_progress_modal(100, "Done!")
        Sys.sleep(0.3)
        close_progress_modal()
        shinyjs::enable("run_filter")
        shinyalert::shinyalert("Filtering Completed", "Non-redundant annotation table was generated.", type = "success")
      }, error = function(e) {
        close_progress_modal()
        shinyjs::enable("run_filter")
        shinyalert::shinyalert("Error", paste("Filtering failed:", e$message), type = "error")
      })
    })

    output$status <- renderText({
      res <- filter_result()
      if (is.null(res)) return("No filtered table yet.")
      final <- res$final_table
      audit <- res$redundancy_table
      paste0(
        "Final records: ", nrow(final), "\n",
        "Audit records: ", nrow(audit), "\n",
        "Network records: ", sum(final$record_type == "sub_network", na.rm = TRUE), "\n",
        "Merged-compound records: ", sum(final$record_type == "merged_compound", na.rm = TRUE), "\n",
        "Single-feature records: ", sum(final$record_type == "single_feature", na.rm = TRUE), "\n",
        "Recurrent ion flagged: ", sum(audit$recurrent_status != "none", na.rm = TRUE), "\n",
        "Recurrent ISF removed: ", sum(!audit$keep & grepl("recurrent", audit$drop_reason %||% ""), na.rm = TRUE), "\n",
        "Cross-polarity removed: ", sum(!audit$keep & grepl("cross_polarity", audit$redundancy_reason), na.rm = TRUE)
      )
    })

    output$notes <- renderText({
      paste0(
        "High confidence: Level <= ", input$high_conf_level, ".\n",
        "Sub-network records use the feature-network parent and pseudo metabolite ID.\n",
        "Merged-compound records consolidate features sharing compound annotations when the relationship is RT-local or chemically explainable.\n",
        "Same compound, same m/z, cross-RT features are audited with recurrent-ion context; resolved recurrent ISF rows are removed, unresolved recurrent ions are retained for review.\n",
        "Positive/negative duplicate metabolites are matched by compound key and RT window; higher mean area is retained.\n",
        "Level3/unknown standalone features keep the best-scoring candidate."
      )
    })

    output$tbl_final <- DT::renderDataTable({
      req(filter_result())
      DT::datatable(filter_result()$final_table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$tbl_audit <- DT::renderDataTable({
      req(filter_result())
      DT::datatable(filter_result()$redundancy_table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$download_final <- downloadHandler(
      filename = "annotation_nonredundant_table.csv",
      content = function(file) {
        req(filter_result())
        utils::write.csv(filter_result()$final_table, file, row.names = FALSE)
      }
    )

    output$download_audit <- downloadHandler(
      filename = "annotation_redundancy_audit.csv",
      content = function(file) {
        req(filter_result())
        utils::write.csv(filter_result()$redundancy_table, file, row.names = FALSE)
      }
    )
  })
}
