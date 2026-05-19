#' Compound ID Mapping Toolkit UI
#'
#' @param id Module id.
#' @noRd
mod_id_mapping_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      fillable = FALSE,
      padding = 0,
      sidebar = bslib::sidebar(
        title = "Compound ID Mapping",
        width = 390,
        bg = "#f8f9fa",
        tags$h6(class = "fw-bold text-primary", "Input databases"),
        fileInput(
          ns("plantcyc_file"),
          "PlantCyc database or spectra_info",
          accept = c(".rda", ".RData", ".tsv", ".txt", ".csv")
        ),
        fileInput(
          ns("kegg_file"),
          "KEGG database or spectra_info",
          accept = c(".rda", ".RData", ".tsv", ".txt", ".csv")
        ),
        numericInput(ns("mass_ppm"), "Mass tolerance for fallback matching (ppm)", value = 10, min = 1, max = 50, step = 1),
        tags$hr(),
        actionButton(ns("build_mapping"), "Build ID Mapping", icon = icon("link"), class = "btn-teal w-100 fw-bold"),
        tags$hr(),
        downloadButton(ns("download_mapping"), "Download Mapping TSV", class = "btn-outline-primary w-100 mb-2"),
        downloadButton(ns("download_workbook"), "Download Workbook", class = "btn-outline-primary w-100 mb-2"),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "Updated databases"),
        downloadButton(ns("download_plantcyc_rda"), "Download PlantCyc .rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_kegg_rda"), "Download KEGG .rda", class = "btn-outline-success w-100"),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "Merge four databases"),
        fileInput(ns("merge_plantcyc_ms1"), "PlantCyc MS1 database", accept = c(".rda", ".RData")),
        fileInput(ns("merge_kegg_ms1"), "KEGG MS1 database", accept = c(".rda", ".RData")),
        fileInput(ns("merge_plantcyc_ms2"), "PlantCyc MS2 database", accept = c(".rda", ".RData")),
        fileInput(ns("merge_kegg_ms2"), "KEGG MS2 database", accept = c(".rda", ".RData")),
        textInput(ns("merge_prefix"), "New Lab.ID prefix", value = "MMDB"),
        actionButton(ns("build_merged_db"), "Build Merged Databases", icon = icon("object-group"), class = "btn-success w-100 fw-bold"),
        br(), br(),
        downloadButton(ns("download_merged_ms1"), "Download merged_zma_ms1.rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_merged_ms2"), "Download merged_zma_ms2.rda", class = "btn-outline-success w-100")
      ),
      div(
        class = "p-3",
        h3("PlantCyc / KEGG Compound ID Mapping", class = "text-primary fw-bold"),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("PlantCyc records", textOutput(ns("value_plant"), inline = TRUE), showcase = bsicons::bs_icon("database")),
          bslib::value_box("KEGG records", textOutput(ns("value_kegg"), inline = TRUE), showcase = bsicons::bs_icon("database-fill")),
          bslib::value_box("Mapped pairs", textOutput(ns("value_pairs"), inline = TRUE), showcase = bsicons::bs_icon("link-45deg")),
          bslib::value_box("High confidence", textOutput(ns("value_high"), inline = TRUE), showcase = bsicons::bs_icon("check-circle"))
        ),
        br(),
        bslib::card(
          bslib::card_header("Mapping Summary"),
          verbatimTextOutput(ns("status"))
        ),
        br(),
        bslib::card(
          full_screen = TRUE,
          bslib::navset_tab(
            bslib::nav_panel("ID Mapping", DT::dataTableOutput(ns("tbl_mapping"))),
            bslib::nav_panel("PlantCyc Index", DT::dataTableOutput(ns("tbl_plant"))),
            bslib::nav_panel("KEGG Index", DT::dataTableOutput(ns("tbl_kegg")))
          )
        )
      )
    )
  )
}

#' Compound ID Mapping Toolkit Server
#'
#' @param id Module id.
#' @noRd
mod_id_mapping_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(result = NULL, updated = NULL, merged = NULL, status = "Upload PlantCyc and KEGG databases or spectra_info tables.")

    observeEvent(input$build_mapping, {
      req(input$plantcyc_file, input$kegg_file)
      shinyjs::disable("build_mapping")
      on.exit(shinyjs::enable("build_mapping"), add = TRUE)

      tryCatch({
        result <- metminer_idmap_load_and_build(
          plantcyc_file = input$plantcyc_file$datapath,
          kegg_file = input$kegg_file$datapath,
          mass_ppm = input$mass_ppm
        )
        updated <- NULL
        if (tolower(tools::file_ext(input$plantcyc_file$name)) %in% c("rda", "rdata") &&
            tolower(tools::file_ext(input$kegg_file$name)) %in% c("rda", "rdata") &&
            nrow(result$mapping) > 0) {
          updated <- metminer_update_database_id_mapping(
            plantcyc_file = input$plantcyc_file$datapath,
            kegg_file = input$kegg_file$datapath,
            mapping = result$mapping
          )
        }
        state$result <- result
        state$updated <- updated
        state$status <- paste0(
          "PlantCyc index records: ", nrow(result$plantcyc_index), "\n",
          "KEGG index records: ", nrow(result$kegg_index), "\n",
          "Mapped pairs: ", nrow(result$mapping), "\n",
          "High-confidence pairs: ", sum(result$mapping$confidence == "high", na.rm = TRUE), "\n",
          "Input orientation: ", result$input_message %||% "not detected", "\n",
          "Match types: ",
          if (nrow(result$mapping) == 0) "none" else paste(names(table(result$mapping$match_type)), table(result$mapping$match_type), sep = "=", collapse = "; "),
          "\nUpdated database files: ",
          if (is.null(updated)) "not generated (requires two RDA/RData inputs with mapped pairs)" else paste0(
            "PlantCyc rows updated=", updated$plantcyc_updated,
            "; KEGG rows updated=", updated$kegg_updated
          )
        )
        shinyalert::shinyalert("ID Mapping Completed", "Mapping table and updated database objects were generated when RDA inputs were provided.", type = "success")
      }, error = function(e) {
        state$status <- paste("Mapping failed:", e$message)
        shinyalert::shinyalert("Error", state$status, type = "error")
      })
    })

    observeEvent(input$build_merged_db, {
      req(input$merge_plantcyc_ms1, input$merge_kegg_ms1, input$merge_plantcyc_ms2, input$merge_kegg_ms2)
      shinyjs::disable("build_merged_db")
      on.exit(shinyjs::enable("build_merged_db"), add = TRUE)

      tryCatch({
        merged <- metminer_build_merged_ms_databases(
          plantcyc_ms1_file = input$merge_plantcyc_ms1$datapath,
          kegg_ms1_file = input$merge_kegg_ms1$datapath,
          plantcyc_ms2_file = input$merge_plantcyc_ms2$datapath,
          kegg_ms2_file = input$merge_kegg_ms2$datapath,
          mass_ppm = input$mass_ppm,
          lab_prefix = input$merge_prefix %||% "MMDB"
        )
        state$merged <- merged
        state$status <- paste0(
          "Merged database build completed.\n",
          "MS1 mapping pairs: ", nrow(merged$ms1_mapping$mapping), "\n",
          "MS1 merged records: ", nrow(merged$ms1$spectra_info), "\n",
          "MS1 mapped groups: ", merged$ms1$mapped_groups, "; PlantCyc-only: ", merged$ms1$plantcyc_only, "; KEGG-only: ", merged$ms1$kegg_only, "\n",
          "MS2 mapping pairs: ", nrow(merged$ms2_mapping$mapping), "\n",
          "MS2 merged records: ", nrow(merged$ms2$spectra_info), "\n",
          "MS2 mapped groups: ", merged$ms2$mapped_groups, "; PlantCyc-only: ", merged$ms2$plantcyc_only, "; KEGG-only: ", merged$ms2$kegg_only
        )
        shinyalert::shinyalert("Merged Databases Completed", "merged_zma_ms1.rda and merged_zma_ms2.rda are ready for download.", type = "success")
      }, error = function(e) {
        state$status <- paste("Merged database build failed:", e$message)
        shinyalert::shinyalert("Error", state$status, type = "error")
      })
    })

    output$value_plant <- renderText({
      if (is.null(state$result)) return("0")
      nrow(state$result$plantcyc_index)
    })
    output$value_kegg <- renderText({
      if (is.null(state$result)) return("0")
      nrow(state$result$kegg_index)
    })
    output$value_pairs <- renderText({
      if (is.null(state$result)) return("0")
      nrow(state$result$mapping)
    })
    output$value_high <- renderText({
      if (is.null(state$result)) return("0")
      sum(state$result$mapping$confidence == "high", na.rm = TRUE)
    })

    output$status <- renderText(state$status)

    output$tbl_mapping <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$mapping, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_plant <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$plantcyc_index, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_kegg <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$kegg_index, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$download_mapping <- downloadHandler(
      filename = function() paste0("plantcyc_kegg_id_mapping_", Sys.Date(), ".tsv"),
      content = function(file) {
        req(state$result)
        utils::write.table(state$result$mapping, file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
      }
    )

    output$download_workbook <- downloadHandler(
      filename = function() paste0("plantcyc_kegg_id_mapping_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(state$result)
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Package 'writexl' is required to export Excel workbooks.", call. = FALSE)
        }
        writexl::write_xlsx(
          list(
            ID_mapping = state$result$mapping,
            PlantCyc_index = state$result$plantcyc_index,
            KEGG_index = state$result$kegg_index
          ),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    output$download_plantcyc_rda <- downloadHandler(
      filename = function() {
        base <- tools::file_path_sans_ext(input$plantcyc_file$name %||% "plantcyc_database")
        paste0(base, "_idmapped.rda")
      },
      content = function(file) {
        req(state$updated)
        metminer_save_idmapped_database(state$updated$plantcyc, file)
      }
    )

    output$download_kegg_rda <- downloadHandler(
      filename = function() {
        base <- tools::file_path_sans_ext(input$kegg_file$name %||% "kegg_database")
        paste0(base, "_idmapped.rda")
      },
      content = function(file) {
        req(state$updated)
        metminer_save_idmapped_database(state$updated$kegg, file)
      }
    )

    output$download_merged_ms1 <- downloadHandler(
      filename = function() "merged_zma_ms1.rda",
      content = function(file) {
        req(state$merged)
        metminer_save_merged_database(state$merged$ms1, file)
      }
    )

    output$download_merged_ms2 <- downloadHandler(
      filename = function() "merged_zma_ms2.rda",
      content = function(file) {
        req(state$merged)
        metminer_save_merged_database(state$merged$ms2, file)
      }
    )
  })
}
