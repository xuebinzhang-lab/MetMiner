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
          tags$h6(class = "fw-bold text-primary", "Candidate ranking"),
          numericInput(ns("top_n"), "Top annotations per feature", value = 3, min = 1, max = 10, step = 1),
          numericInput(ns("high_conf_level"), "High confidence Level <=", value = 2, min = 1, max = 3, step = 1),
          tags$hr(class = "my-3"),
          tags$h6(class = "fw-bold text-primary", "Network and polarity checks"),
          numericInput(ns("cross_rt_tol"), "Positive/Negative RT Tolerance (sec):", value = 10, min = 0, step = 1),
          checkboxInput(ns("use_network"), "Use feature-network validation", value = TRUE),
          checkboxInput(ns("drop_recurrent_background"), "Exclude suspected recurrent/background ions", value = FALSE),
          tags$hr(class = "my-3"),
          tags$h6(class = "fw-bold text-primary", "Optional inputs"),
          fileInput(
            ns("advice_file"),
            "LC-MS parameter advice JSON/TSV",
            accept = c(".json", ".tsv", ".txt", ".csv")
          ),
          fileInput(
            ns("id_mapping_file"),
            "Compound ID mapping TSV/CSV/XLSX",
            accept = c(".tsv", ".txt", ".csv", ".xlsx")
          ),
          tags$hr(),
          actionButton(ns("run_filter"), "Build Review Tables", icon = icon("filter"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        div(
          class = "p-3",
          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Annotation Filtering Review"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(bslib::card_header("Summary", class = "bg-success-subtle text-success-emphasis"), verbatimTextOutput(ns("status"))),
            bslib::card(bslib::card_header("Filtering Logic", class = "bg-light"), uiOutput(ns("notes")))
          ),
          br(),
          bslib::card(
            full_screen = TRUE,
            bslib::navset_tab(
              bslib::nav_panel(
                "Expand Review",
                div(
                  class = "d-flex justify-content-end gap-2 mb-2",
                  downloadButton(ns("download_expand"), "Download CSV", class = "btn-sm btn-outline-success"),
                  downloadButton(ns("download_workbook"), "Download Excel", class = "btn-sm btn-success")
                ),
                DT::dataTableOutput(ns("tbl_expand"))
              ),
              bslib::nav_panel(
                "Collapse Review",
                div(class = "d-flex justify-content-end mb-2",
                    downloadButton(ns("download_collapse"), "Download CSV", class = "btn-sm btn-success")),
                DT::dataTableOutput(ns("tbl_collapse"))
              ),
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

    observe({
      global_data$annotation_filter_advisor_state <- list(
        available = TRUE,
        use_network_validation = isTRUE(input$use_network),
        cross_rt_tolerance = input$cross_rt_tol %||% NA_real_,
        min_high_conf_level = input$high_conf_level %||% NA_integer_,
        drop_recurrent_background = isTRUE(input$drop_recurrent_background),
        top_n = input$top_n %||% NA_integer_,
        advice_file_name = input$advice_file$name %||% NA_character_,
        id_mapping_file_name = input$id_mapping_file$name %||% NA_character_,
        input_objects = list(
          positive_available = !is.null(global_data$object_pos_annotated),
          negative_available = !is.null(global_data$object_neg_annotated)
        ),
        result_available = !is.null(global_data$annotation_filter_result),
        updated_at = as.character(Sys.time())
      )
    })

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
        adduct_advice <- metminer_read_parameter_adduct_advice(input$advice_file$datapath %||% NULL)
        id_mapping <- metminer_read_compound_id_mapping(input$id_mapping_file$datapath %||% NULL)

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
          use_network_validation = input$use_network,
          drop_suspected_recurrent_background = input$drop_recurrent_background,
          adduct_advice = adduct_advice
        )

        update_progress_modal(85, "Building expand/collapse review tables...")
        review <- metminer_build_annotation_review_tables(
          positive_object = pos,
          negative_object = neg,
          top_n = input$top_n,
          adduct_advice = adduct_advice,
          id_mapping = id_mapping
        )
        res$expand_table <- review$expand_table
        res$collapse_table <- review$collapse_table
        res$adduct_advice <- review$adduct_advice
        res$review_top_n <- input$top_n
        res$id_mapping <- id_mapping

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
        shinyalert::shinyalert("Filtering Completed", "Expand and collapse review tables were generated.", type = "success")
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
      expand <- res$expand_table %||% data.frame()
      collapse <- res$collapse_table %||% data.frame()
      paste0(
        "Expand review rows: ", nrow(expand), "\n",
        "Collapse review rows: ", nrow(collapse), "\n",
        "Final records: ", nrow(final), "\n",
        "Audit records: ", nrow(audit), "\n",
        "Layer 1 genome/reaction records: ", sum(final$annotation_layer == "genome_reaction", na.rm = TRUE), "\n",
        "Layer 2 public MS2 records: ", sum(final$annotation_layer == "public_ms2", na.rm = TRUE), "\n",
        "Optional local/custom spectral records: ", sum(final$annotation_layer == "local_spectral_optional", na.rm = TRUE), "\n",
        "Network records: ", sum(final$record_type == "sub_network", na.rm = TRUE), "\n",
        "Merged-compound records: ", sum(final$record_type == "merged_compound", na.rm = TRUE), "\n",
        "Single-feature records: ", sum(final$record_type == "single_feature", na.rm = TRUE), "\n",
        "Recurrent ion flagged: ", sum(audit$recurrent_status != "none", na.rm = TRUE), "\n",
        "Signal-quality flagged: ", sum(audit$signal_quality_flag %in% c("recurrent_low_mz_background_candidate", "unresolved_recurrent_background_candidate", "unresolved_recurrent_review"), na.rm = TRUE), "\n",
        "Signal-quality removed: ", sum(!audit$keep & audit$drop_reason == "suspected_recurrent_background_ion", na.rm = TRUE), "\n",
        "Recurrent ISF removed: ", sum(!audit$keep & grepl("recurrent", audit$drop_reason %||% ""), na.rm = TRUE), "\n",
        "Cross-polarity removed: ", sum(!audit$keep & grepl("cross_polarity", audit$redundancy_reason), na.rm = TRUE)
      )
    })

    output$notes <- renderUI({
      advice <- if (!is.null(filter_result())) {
        filter_result()$adduct_advice
      } else {
        tryCatch(metminer_read_parameter_adduct_advice(input$advice_file$datapath %||% NULL), error = function(e) metminer_default_adduct_advice())
      }
      id_mapping_rows <- if (!is.null(filter_result())) {
        nrow(filter_result()$id_mapping %||% data.frame())
      } else {
        nrow(tryCatch(metminer_read_compound_id_mapping(input$id_mapping_file$datapath %||% NULL), error = function(e) data.frame()))
      }

      tags$div(
        class = "small lh-sm",
        tags$ol(
          class = "mb-2 ps-3",
          tags$li(tags$b("Candidate ranking: "), "for each feature, keep the top ", input$top_n,
                  " annotations ranked by evidence layer, Level, Total.score, then LC-MS adduct priority."),
          tags$li(tags$b("Layer 1: "), "KEGG/PlantCyc genome/reaction candidates are retained only when they pass the strict core-adduct rule."),
          tags$li(tags$b("Layer 2: "), "public MS2 libraries are default spectral evidence; local/custom standard libraries are optional enhancement inputs, not required."),
          tags$li(tags$b("Adduct priority: "), "core adducts define strict Layer 1 acceptance; optional adducts may still support Layer 2 spectral candidates."),
          tags$li(tags$b("Expand review: "), "keeps candidate annotations together with feature-network roles, so isotope/adduct/ISF relationships can be checked manually."),
          tags$li(tags$b("Collapse review: "), "keeps one representative feature per sub-network, including unannotated parent-like features when network evidence supports them."),
          tags$li(tags$b("Recurrent/background ion flag: "), "unresolved recurrent ions, especially low-m/z ions that repeatedly appear across RT without a resolved parent, are marked as suspected background/interference candidates."),
          tags$li(tags$b("Traceability: "), "the legacy non-redundant table and redundancy audit table are retained for comparison and export.")
        ),
        tags$div(class = "border-top pt-2 mt-2"),
        tags$div(tags$b("High-confidence cutoff: "), "Level <= ", input$high_conf_level),
        tags$div(tags$b("ID mapping rows: "), id_mapping_rows),
        tags$div(tags$b("Positive core: "), paste(advice$positive_core %||% character(), collapse = ", ")),
        tags$div(tags$b("Positive optional: "), paste(advice$positive_optional %||% character(), collapse = ", ")),
        tags$div(tags$b("Negative core: "), paste(advice$negative_core %||% character(), collapse = ", ")),
        tags$div(tags$b("Negative optional: "), paste(advice$negative_optional %||% character(), collapse = ", "))
      )
    })

    output$tbl_expand <- DT::renderDataTable({
      req(filter_result())
      DT::datatable(filter_result()$expand_table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$tbl_collapse <- DT::renderDataTable({
      req(filter_result())
      DT::datatable(filter_result()$collapse_table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
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

    output$download_expand <- downloadHandler(
      filename = "annotation_filtering_expand.csv",
      content = function(file) {
        req(filter_result())
        utils::write.csv(filter_result()$expand_table, file, row.names = FALSE)
      }
    )

    output$download_collapse <- downloadHandler(
      filename = "annotation_filtering_collapse.csv",
      content = function(file) {
        req(filter_result())
        utils::write.csv(filter_result()$collapse_table, file, row.names = FALSE)
      }
    )

    output$download_workbook <- downloadHandler(
      filename = "annotation_filtering_review_tables.xlsx",
      content = function(file) {
        req(filter_result())
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Package 'writexl' is required to export Excel workbooks.", call. = FALSE)
        }
        writexl::write_xlsx(
          list(
            Expand = filter_result()$expand_table,
            Collapse = filter_result()$collapse_table,
            Final_nonredundant = filter_result()$final_table,
            Redundancy_audit = filter_result()$redundancy_table
          ),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
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
