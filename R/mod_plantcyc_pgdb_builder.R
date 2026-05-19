#' PlantCyc local PGDB builder UI
#'
#' @param id Module id.
#' @noRd
mod_plantcyc_pgdb_builder_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      fillable = FALSE,
      padding = 0,
      sidebar = bslib::sidebar(
        title = "PlantCyc Local PGDB",
        width = 410,
        bg = "#f8f9fa",
        tags$h6(class = "fw-bold text-primary", "Licensed PGDB input"),
        tags$p(class = "text-muted small",
               "MetMiner does not redistribute PMN/PlantCyc data. Apply for a PMN license, download a species PGDB archive, then upload it here or provide a local extracted folder path."),
        fileInput(ns("pgdb_archive"), "PGDB archive (.tar, .tar.gz, .tgz, .zip)", accept = c(".tar", ".gz", ".tgz", ".zip")),
        textInput(ns("pgdb_dir"), "Or extracted PGDB folder path", value = "", placeholder = "/path/to/speciescyc/version"),
        textInput(ns("output_prefix"), "Output prefix", value = "plantcyc_zma", placeholder = "plantcyc_zma"),
        textInput(ns("organism"), "Organism label", value = "Zea mays (maize)"),
        textInput(ns("output_dir"), "Output directory", value = file.path("Temp", "plantcyc_pgdb_builder")),
        actionButton(ns("build"), "Build Local Databases", icon = icon("database"), class = "btn-teal w-100 fw-bold"),
        tags$hr(),
        downloadButton(ns("download_local"), "Download local object .rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_ms1"), "Download MS1 database .rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_ms2"), "Download MS2 database .rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_pathway"), "Download pathway database .rda", class = "btn-outline-success w-100 mb-2"),
        downloadButton(ns("download_bundle"), "Download TSV bundle", class = "btn-outline-primary w-100")
      ),
      div(
        class = "p-3",
        h3("PlantCyc Local PGDB Builder", class = "text-primary fw-bold"),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("Compounds", textOutput(ns("value_compounds"), inline = TRUE), showcase = bsicons::bs_icon("database")),
          bslib::value_box("MS1 records", textOutput(ns("value_ms1"), inline = TRUE), showcase = bsicons::bs_icon("file-earmark-medical")),
          bslib::value_box("Pathways", textOutput(ns("value_pathways"), inline = TRUE), showcase = bsicons::bs_icon("diagram-3")),
          bslib::value_box("Reactions", textOutput(ns("value_reactions"), inline = TRUE), showcase = bsicons::bs_icon("shuffle"))
        ),
        br(),
        bslib::card(bslib::card_header("Build Status"), verbatimTextOutput(ns("status"))),
        br(),
        bslib::card(
          full_screen = TRUE,
          bslib::navset_tab(
            bslib::nav_panel("Summary", DT::dataTableOutput(ns("tbl_summary"))),
            bslib::nav_panel("Compounds", DT::dataTableOutput(ns("tbl_compounds"))),
            bslib::nav_panel("Removed Compounds", DT::dataTableOutput(ns("tbl_removed_compounds"))),
            bslib::nav_panel("CoA Fragment Rules", DT::dataTableOutput(ns("tbl_coa_rules"))),
            bslib::nav_panel("Pathway-Compound", DT::dataTableOutput(ns("tbl_pathway_compound"))),
            bslib::nav_panel("Reactions", DT::dataTableOutput(ns("tbl_reactions")))
          )
        )
      )
    )
  )
}

#' PlantCyc local PGDB builder server
#'
#' @param id Module id.
#' @noRd
mod_plantcyc_pgdb_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal <- progress_handlers$close_progress_modal

    state <- reactiveValues(result = NULL, status = "Waiting for a licensed PMN/PlantCyc PGDB archive or folder.")

    observeEvent(input$build, {
      shinyjs::disable("build")
      show_progress_modal(
        title = "Building PlantCyc Local Databases",
        message = "Preparing licensed PGDB input...",
        value = 5
      )

      shinyjs::delay(100, {
        on.exit({
          close_progress_modal()
          shinyjs::enable("build")
        }, add = TRUE)
        tryCatch({
          pgdb_data_dir <- NULL
          if (!is.null(input$pgdb_archive) && file.exists(input$pgdb_archive$datapath)) {
            state$status <- "Unpacking uploaded PGDB archive..."
            update_progress_modal(10, "Unpacking uploaded PGDB archive...")
            pgdb_data_dir <- metminer_pgdb_unpack_archive(input$pgdb_archive$datapath)
          } else if (has_text(input$pgdb_dir) && dir.exists(input$pgdb_dir)) {
            update_progress_modal(12, "Locating PGDB data directory...")
            pgdb_data_dir <- metminer_pgdb_locate_data_dir(input$pgdb_dir)
          } else {
            stop("Please upload a PGDB archive or provide a valid extracted PGDB folder path.", call. = FALSE)
          }

          output_dir <- input$output_dir %||% file.path("Temp", "plantcyc_pgdb_builder")
          update_progress_modal(25, "Parsing compounds, reactions, and pathways...")
          result <- metminer_pgdb_build_outputs(
            pgdb_dir = dirname(pgdb_data_dir),
            output_dir = output_dir,
            output_prefix = input$output_prefix %||% "plantcyc_local",
            organism = input$organism %||% "PlantCyc local PGDB"
          )

          update_progress_modal(90, "Refreshing tables and summary...")
          state$result <- result
          state$status <- paste0(
            "Build completed.\n",
            "PGDB data dir: ", pgdb_data_dir, "\n",
            "Output dir: ", normalizePath(output_dir, mustWork = FALSE), "\n",
          "Compounds: ", nrow(result$local_db$compounds), "\n",
          "MS1 records: ", nrow(result$ms1_db@spectra.info), "\n",
          "MS2 records: ", nrow(result$ms2_db@spectra.info), "\n",
          "Removed from MS1 by LC-MS filters: ", nrow(result$local_db$removed_compounds), "\n",
            "MS2-eligible compounds: ", nrow(result$local_db$ms2_eligible_compounds), "\n",
            "Pathways: ", nrow(result$local_db$pathways), "\n",
            "Reactions: ", nrow(result$local_db$reactions), "\n",
            "Pathway-compound links: ", nrow(result$local_db$pathway_compounds)
          )
          update_progress_modal(100, "Build completed.")
          close_progress_modal()
          showNotification(
            "PlantCyc PGDB build completed. Local object, MS1 database, and pathway database were generated.",
            type = "message",
            duration = 6
          )
        }, error = function(e) {
          state$status <- paste("Build failed:", e$message)
          close_progress_modal()
          showNotification(state$status, type = "error", duration = 10)
        })
      })
    })

    output$status <- renderText(state$status)
    output$value_compounds <- renderText(if (is.null(state$result)) "0" else nrow(state$result$local_db$compounds))
    output$value_ms1 <- renderText(if (is.null(state$result)) "0" else nrow(state$result$ms1_db@spectra.info))
    output$value_pathways <- renderText(if (is.null(state$result)) "0" else nrow(state$result$local_db$pathways))
    output$value_reactions <- renderText(if (is.null(state$result)) "0" else nrow(state$result$local_db$reactions))

    output$tbl_summary <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$tbl_compounds <- DT::renderDataTable({
      req(state$result)
      keep <- intersect(c("PlantCyc.ID", "KEGG.ID", "PubChem.ID", "ChEBI.ID", "Compound.name", "Formula", "mz", "INCHIKEY.ID", "Types"), colnames(state$result$local_db$compounds))
      DT::datatable(state$result$local_db$compounds[, keep, drop = FALSE], rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_removed_compounds <- DT::renderDataTable({
      req(state$result)
      keep <- intersect(
        c("PlantCyc.ID", "KEGG.ID", "Compound.name", "Formula", "mz", "filter_reason", "ms2_filter_reason", "Synonyms"),
        colnames(state$result$local_db$removed_compounds)
      )
      DT::datatable(state$result$local_db$removed_compounds[, keep, drop = FALSE], rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_coa_rules <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$local_db$coa_fragment_rules, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_pathway_compound <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$local_db$pathway_compounds, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_reactions <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$local_db$reactions, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$download_local <- downloadHandler(
      filename = function() paste0(state$result$output_prefix %||% "plantcyc_local", "_local_object.rda"),
      content = function(file) {
        req(state$result)
        file.copy(state$result$files$local_object, file, overwrite = TRUE)
      }
    )
    output$download_ms1 <- downloadHandler(
      filename = function() paste0(state$result$output_prefix %||% "plantcyc_local", "_ms1.rda"),
      content = function(file) {
        req(state$result)
        file.copy(state$result$files$ms1, file, overwrite = TRUE)
      }
    )
    output$download_ms2 <- downloadHandler(
      filename = function() paste0(state$result$output_prefix %||% "plantcyc_local", "_ms2.rda"),
      content = function(file) {
        req(state$result)
        file.copy(state$result$files$ms2, file, overwrite = TRUE)
      }
    )
    output$download_pathway <- downloadHandler(
      filename = function() paste0(state$result$output_prefix %||% "plantcyc_local", "_pathway.rda"),
      content = function(file) {
        req(state$result)
        file.copy(state$result$files$pathway, file, overwrite = TRUE)
      }
    )
    output$download_bundle <- downloadHandler(
      filename = function() paste0(state$result$output_prefix %||% "plantcyc_local", "_tables.zip"),
      content = function(file) {
        req(state$result)
        old <- setwd(state$result$output_dir)
        on.exit(setwd(old), add = TRUE)
        table_files <- state$result$files[setdiff(names(state$result$files), c("local_object", "ms1", "pathway"))]
        utils::zip(file, files = basename(unlist(table_files, use.names = FALSE)))
      },
      contentType = "application/zip"
    )
  })
}
