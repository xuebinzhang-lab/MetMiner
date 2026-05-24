#' Pathway enrichment UI
#'
#' @param id Module id.
#' @noRd
mod_data_enrich_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      fillable = FALSE,
      padding = 0,
      sidebar = bslib::sidebar(
        title = "Pathway Enrichment",
        width = 390,
        bg = "#f8f9fa",
        tags$h6(class = "fw-bold text-primary", "1. Database"),
        radioButtons(
          ns("database_type"),
          "Annotation/enrichment database",
          choices = c("KEGG" = "kegg", "PlantCyc" = "plantcyc"),
          selected = "kegg",
          inline = TRUE
        ),
        fileInput(ns("pathway_db"), "Pathway database .rda", accept = c(".rda", ".RData")),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "2. Query metabolites"),
        radioButtons(
          ns("query_source"),
          "Query source",
          choices = c(
            "Differential metabolites" = "differential",
            "Annotation filtering result" = "annotation_filter",
            "Upload ID list" = "upload"
          ),
          selected = "differential"
        ),
        selectInput(
          ns("annotation_table"),
          "Annotation table",
          choices = c("Collapse review" = "collapse", "Expand review" = "expand", "Final non-redundant" = "final"),
          selected = "collapse"
        ),
        fileInput(ns("query_file"), "Query ID file (optional)", accept = c(".tsv", ".txt", ".csv")),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "3. Statistics"),
        selectInput(ns("method"), "Test method", choices = c("Hypergeometric" = "hypergeometric", "Fisher exact" = "fisher"), selected = "hypergeometric"),
        selectInput(ns("p_adjust"), "P adjustment", choices = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"), selected = "BH"),
        numericInput(ns("min_pathway_size"), "Minimum pathway size", value = 2, min = 1, max = 100, step = 1),
        tags$hr(),
        actionButton(ns("run_enrich"), "Run Enrichment", icon = icon("chart-simple"), class = "btn-teal w-100 fw-bold")
      ),
      div(
        class = "p-3",
        h3("KEGG / PlantCyc Pathway Enrichment", class = "text-primary fw-bold"),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("Query IDs", textOutput(ns("value_query"), inline = TRUE), showcase = bsicons::bs_icon("search")),
          bslib::value_box("Pathways", textOutput(ns("value_pathways"), inline = TRUE), showcase = bsicons::bs_icon("diagram-3")),
          bslib::value_box("Mapped pathways", textOutput(ns("value_mapped"), inline = TRUE), showcase = bsicons::bs_icon("check2-circle")),
          bslib::value_box("FDR < 0.05", textOutput(ns("value_sig"), inline = TRUE), showcase = bsicons::bs_icon("stars"))
        ),
        br(),
        bslib::card(
          bslib::card_header("Status"),
          verbatimTextOutput(ns("status"))
        ),
        br(),
        bslib::card(
          full_screen = TRUE,
          bslib::navset_tab(
            bslib::nav_panel(
              "Plots",
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(bslib::card_header("Bubble Plot"), plotly::plotlyOutput(ns("bubble_plot"), height = "420px")),
                bslib::card(bslib::card_header("Bar Plot"), plotly::plotlyOutput(ns("bar_plot"), height = "420px"))
              ),
              br(),
              bslib::card(
                bslib::card_header(textOutput(ns("selected_pathway_title"), inline = TRUE)),
                DT::dataTableOutput(ns("tbl_pathway_features"))
              )
            ),
            bslib::nav_panel(
              "Result Table",
              div(class = "d-flex justify-content-end mb-2", downloadButton(ns("download_result"), "Download CSV", class = "btn-sm btn-success")),
              DT::dataTableOutput(ns("tbl_result"))
            )
          )
        )
      )
    )
  )
}

#' Pathway enrichment server
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues.
#' @param prj_init Project state.
#' @noRd
mod_data_enrich_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      result = NULL,
      result_table = data.frame(),
      query_ids = character(),
      query_table = data.frame(),
      pathway_db = NULL,
      selected_pathway = NULL,
      status = "No enrichment result yet."
    )

    current_id_column <- reactive({
      if (identical(input$database_type, "plantcyc")) "PlantCyc.ID" else "KEGG.ID"
    })

    observe({
      global_data$enrichment_advisor_state <- list(
        available = TRUE,
        database_type = input$database_type %||% NA_character_,
        id_column = if (identical(input$database_type, "plantcyc")) "PlantCyc.ID" else "KEGG.ID",
        query_source = input$query_source %||% NA_character_,
        annotation_table = input$annotation_table %||% NA_character_,
        query_file_name = input$query_file$name %||% NA_character_,
        pathway_database_file = input$pathway_db$name %||% NA_character_,
        test_method = input$method %||% NA_character_,
        p_adjust_method = input$p_adjust %||% NA_character_,
        min_pathway_size = input$min_pathway_size %||% NA_integer_,
        query_ids = length(state$query_ids %||% character()),
        result_rows = nrow(state$result_table %||% data.frame()),
        mapped_pathways = if (nrow(state$result_table %||% data.frame()) > 0 && "mapped_number" %in% colnames(state$result_table)) {
          sum(state$result_table$mapped_number > 0, na.rm = TRUE)
        } else NA_integer_,
        fdr_0_05 = if (nrow(state$result_table %||% data.frame()) > 0 && "p_value_adjust" %in% colnames(state$result_table)) {
          sum(state$result_table$p_value_adjust < 0.05, na.rm = TRUE)
        } else NA_integer_,
        selected_pathway = state$selected_pathway %||% NA_character_,
        status = state$status,
        updated_at = as.character(Sys.time())
      )
    })

    make_enrichment_query <- function(id_column) {
      if (identical(input$query_source, "upload")) {
        req(input$query_file)
        ids <- metminer_read_query_ids(input$query_file$datapath, id_column = id_column)
        list(ids = ids, table = data.frame(query_id = ids, stringsAsFactors = FALSE))
      } else if (identical(input$query_source, "differential")) {
        metminer_query_ids_from_differential_result(
          differential_result = global_data$differential_result,
          annotation_filter_result = global_data$annotation_filter_result,
          annotated_objects = list(
            positive = global_data$object_pos_annotated,
            negative = global_data$object_neg_annotated
          ),
          id_column = id_column
        )
      } else {
        ids <- metminer_query_ids_from_annotation_filter(
          filter_result = global_data$annotation_filter_result,
          id_column = id_column,
          table = input$annotation_table
        )
        list(ids = ids, table = data.frame(query_id = ids, stringsAsFactors = FALSE))
      }
    }

    observeEvent(input$run_enrich, {
      req(input$pathway_db)
      tryCatch({
        pathway_db <- metminer_load_pathway_database(input$pathway_db$datapath)
        id_column <- current_id_column()
        query <- make_enrichment_query(id_column)

        query_ids <- query$ids
        query_table <- query$table

        if (length(query_ids) == 0) {
          diag <- query$diagnostics %||% list()
          detail <- if (identical(input$query_source, "differential")) {
            paste0(
              " Differential rows: ", diag$total_rows %||% "unknown",
              "; Up/Down rows: ", diag$changed_rows %||% "unknown",
              "; annotation lookup rows: ", diag$lookup_rows %||% "unknown",
              "; ", id_column, " rows: ", diag$id_rows %||% 0, "."
            )
          } else {
            ""
          }
          stop(paste0("No query IDs were found for ", id_column, ".", detail,
                      " Check annotation filtering result, ID mapping, or uploaded query file."), call. = FALSE)
        }

        enrich <- if (identical(input$database_type, "plantcyc")) {
          metminer_enrich_plantcyc(
            query_id = query_ids,
            pathway_database = pathway_db,
            method = input$method,
            p_adjust_method = input$p_adjust,
            min_pathway_size = input$min_pathway_size
          )
        } else {
          metminer_enrich_kegg(
            query_id = query_ids,
            pathway_database = pathway_db,
            method = input$method,
            p_adjust_method = input$p_adjust,
            min_pathway_size = input$min_pathway_size
          )
        }

        result_table <- metminer_extract_enrich_result_table(enrich)
        if (nrow(result_table) == 0) {
          state$status <- paste0("No query IDs overlap the ", id_column, " universe in the pathway database.")
        } else {
          state$status <- paste0(
            "Database type: ", toupper(input$database_type), "\n",
            "ID column: ", id_column, "\n",
            "Input query IDs: ", length(query_ids), "\n",
            "Pathways tested: ", nrow(result_table), "\n",
            "Mapped pathways: ", sum(result_table$mapped_number > 0, na.rm = TRUE), "\n",
            "FDR < 0.05: ", sum(result_table$p_value_adjust < 0.05, na.rm = TRUE)
          )
        }

        state$result <- enrich
        state$result_table <- result_table
        state$query_ids <- query_ids
        state$query_table <- query_table
        state$pathway_db <- pathway_db
        state$selected_pathway <- if (nrow(result_table) > 0) result_table$pathway_id[1] else NULL
        global_data$enrichment_result <- enrich

        if (!is.null(prj_init$mass_dataset_dir)) {
          enrichment_result <- enrich
          save(enrichment_result, file = file.path(prj_init$mass_dataset_dir, "09.enrichment_result.rda"))
        }

        shinyalert::shinyalert("Enrichment Completed", "Pathway enrichment result was generated.", type = "success")
      }, error = function(e) {
        state$status <- paste("Enrichment failed:", e$message)
        shinyalert::shinyalert("Error", state$status, type = "error")
      })
    })

    output$status <- renderText(state$status)
    output$value_query <- renderText(length(state$query_ids))
    output$value_pathways <- renderText({
      if (is.null(state$pathway_db)) return("0")
      length(state$pathway_db@pathway_id)
    })
    output$value_mapped <- renderText({
      if (nrow(state$result_table) == 0) return("0")
      sum(state$result_table$mapped_number > 0, na.rm = TRUE)
    })
    output$value_sig <- renderText({
      if (nrow(state$result_table) == 0) return("0")
      sum(state$result_table$p_value_adjust < 0.05, na.rm = TRUE)
    })

    output$tbl_result <- DT::renderDataTable({
      req(nrow(state$result_table) > 0)
      DT::datatable(state$result_table, rownames = FALSE, selection = "single", options = list(scrollX = TRUE, pageLength = 15))
    })

    observeEvent(input$tbl_result_rows_selected, {
      idx <- input$tbl_result_rows_selected
      if (length(idx) == 1 && nrow(state$result_table) >= idx) {
        state$selected_pathway <- state$result_table$pathway_id[idx]
      }
    })

    observeEvent(plotly::event_data("plotly_click", source = "enrichment_bubble"), {
      click <- plotly::event_data("plotly_click", source = "enrichment_bubble", priority = "event")
      if (!is.null(click$key) && has_text(click$key)) {
        state$selected_pathway <- as.character(click$key[1])
      }
    }, ignoreInit = TRUE)

    output$bubble_plot <- plotly::renderPlotly({
      req(nrow(state$result_table) > 0)
      metminer_plot_enrichment_bubble(state$result, state$result_table)
    })

    output$bar_plot <- plotly::renderPlotly({
      req(nrow(state$result_table) > 0)
      metminer_plot_enrichment_bar(state$result, state$result_table)
    })

    selected_pathway_features <- reactive({
      req(state$selected_pathway)
      metminer_enrichment_pathway_feature_table(
        result_table = state$result_table,
        query_table = state$query_table,
        pathway_id = state$selected_pathway,
        id_column = current_id_column()
      )
    })

    output$selected_pathway_title <- renderText({
      pid <- state$selected_pathway
      if (is.null(pid)) return("Differential metabolites in selected pathway")
      hit <- state$result_table[state$result_table$pathway_id == pid, , drop = FALSE]
      if (nrow(hit) == 0) return(paste("Selected pathway:", pid))
      paste0("Differential metabolites in ", hit$pathway_name[1], " (", pid, ")")
    })

    output$tbl_pathway_features <- DT::renderDataTable({
      tbl <- selected_pathway_features()
      DT::datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$download_result <- downloadHandler(
      filename = function() paste0("metminer_", input$database_type, "_enrichment_", Sys.Date(), ".csv"),
      content = function(file) {
        req(nrow(state$result_table) > 0)
        utils::write.csv(state$result_table, file, row.names = FALSE)
      }
    )
  })
}
