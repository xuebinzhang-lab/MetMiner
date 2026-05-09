#' KEGG Organism Database Construction UI Module
#'
#' @param id Module id.
#' @noRd
mod_kegg_database_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "KEGG Database",
          width = 390,
          bg = "#f8f9fa",

          tags$h6(class = "fw-bold text-primary", "1. Organism"),
          actionButton(
            ns("refresh_organisms"),
            "Refresh KEGG plant list",
            icon = icon("rotate"),
            class = "btn-outline-primary w-100 mb-2"
          ),
          selectizeInput(
            ns("organism_code"),
            "Green plant organism",
            choices = c("zma - Zea mays (maize)" = "zma"),
            selected = "zma",
            options = list(placeholder = "Search KEGG code or Latin name")
          ),
          textInput(ns("organism_name"), "Organism name", value = "Zea mays"),
          tags$small(
            class = "text-muted d-block mb-3",
            "The list is fetched from KEGG organism entries and filtered to Eukaryotes;Plants."
          ),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "2. LC-MS filters"),
          numericInput(ns("min_mw"), "Minimum MW", value = 70, min = 0, step = 1),
          numericInput(ns("max_mw"), "Maximum MW", value = 1500, min = 100, step = 10),

          tags$hr(),
          tags$h6(class = "fw-bold text-primary", "3. KEGG access"),
          numericInput(ns("sleep_sec"), "Sleep between KEGG requests (sec)", value = 0.35, min = 0.1, step = 0.05),
          tags$small(
            class = "text-muted d-block mb-3",
            "Requests are cached locally. The first run for a species may take several minutes while compound metadata are downloaded."
          ),

          tags$hr(),
          tags$h6(class = "fw-bold text-primary", "4. Output"),
          textInput(
            ns("output_dir"),
            "Output folder",
            value = file.path("Temp", "kegg_zma_database"),
            placeholder = "Temp/kegg_zma_database"
          ),
          actionButton(
            ns("run"),
            "Build KEGG Databases",
            icon = icon("database"),
            class = "btn-teal w-100 fw-bold shadow-sm"
          )
        ),

        div(
          class = "p-3",
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            h3("KEGG Organism Metabolite Database Construction", class = "text-primary fw-bold m-0"),
            div(
              downloadButton(ns("download_bundle"), "Download Bundle", class = "btn-success"),
              downloadButton(ns("download_ms1"), "MS1 .rda", class = "btn-outline-primary ms-2"),
              downloadButton(ns("download_ms2"), "MS2 .rda", class = "btn-outline-primary ms-2"),
              downloadButton(ns("download_pathway"), "Pathway .rda", class = "btn-outline-primary ms-2")
            )
          ),

          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3, 3),
            bslib::value_box(
              title = "Compounds",
              value = textOutput(ns("value_compounds"), inline = TRUE),
              showcase = bsicons::bs_icon("boxes")
            ),
            bslib::value_box(
              title = "Pathways",
              value = textOutput(ns("value_pathways"), inline = TRUE),
              showcase = bsicons::bs_icon("diagram-3")
            ),
            bslib::value_box(
              title = "MS2 Compounds",
              value = textOutput(ns("value_ms2"), inline = TRUE),
              showcase = bsicons::bs_icon("soundwave")
            ),
            bslib::value_box(
              title = "Supported Reactions",
              value = textOutput(ns("value_reactions"), inline = TRUE),
              showcase = bsicons::bs_icon("activity")
            ),
            bslib::value_box(
              title = "Links",
              value = textOutput(ns("value_links"), inline = TRUE),
              showcase = bsicons::bs_icon("share")
            )
          ),

          br(),
          bslib::card(
            bslib::card_header("Workflow Status", class = "bg-light"),
            verbatimTextOutput(ns("status"), placeholder = TRUE)
          ),

          br(),
          bslib::card(
            full_screen = TRUE,
            bslib::navset_tab(
              bslib::nav_panel("Summary", DT::dataTableOutput(ns("tbl_summary"))),
              bslib::nav_panel("Compounds", DT::dataTableOutput(ns("tbl_compounds"))),
              bslib::nav_panel("MS2 Compounds", DT::dataTableOutput(ns("tbl_ms2_info"))),
              bslib::nav_panel("MS2 Match Log", DT::dataTableOutput(ns("tbl_ms2_match_log"))),
              bslib::nav_panel("Pathway Map", DT::dataTableOutput(ns("tbl_pathway_map"))),
              bslib::nav_panel("Reaction Evidence", DT::dataTableOutput(ns("tbl_reactions"))),
              bslib::nav_panel("Removed Compounds", DT::dataTableOutput(ns("tbl_removed")))
            )
          )
        )
      )
    )
  )
}

#' KEGG Organism Database Construction Server Module
#'
#' @param id Module id.
#' @noRd
mod_kegg_database_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal <- progress_handlers$close_progress_modal

    state <- reactiveValues(
      organisms = data.frame(),
      result = NULL,
      status = "Waiting for a KEGG organism selection."
    )

    load_organisms <- function(force = FALSE) {
      tryCatch({
        organisms <- metminer_kegg_green_plant_organisms(force = force)
        state$organisms <- organisms
        choices <- stats::setNames(organisms$organism_code, organisms$display_name)
        updateSelectizeInput(session, "organism_code", choices = choices, selected = input$organism_code %||% "zma", server = TRUE)
        state$status <- paste0("Loaded ", nrow(organisms), " KEGG green plant organisms.")
      }, error = function(e) {
        state$status <- paste("Failed to load KEGG organism list:", e$message)
        showNotification(e$message, type = "error", duration = 8)
      })
    }

    observeEvent(TRUE, load_organisms(force = FALSE), once = TRUE)
    observeEvent(input$refresh_organisms, load_organisms(force = TRUE))

    observeEvent(input$organism_code, {
      organisms <- state$organisms
      hit <- organisms[organisms$organism_code == input$organism_code, , drop = FALSE]
      if (nrow(hit) > 0) {
        updateTextInput(session, "organism_name", value = hit$organism_name[1])
        updateTextInput(session, "output_dir", value = file.path("Temp", paste0("kegg_", input$organism_code, "_database")))
      }
    }, ignoreInit = TRUE)

    output$status <- renderText(state$status)

    summary_value <- function(metric) {
      if (is.null(state$result)) return("0")
      value <- state$result$summary$value[state$result$summary$metric == metric]
      if (length(value) == 0) "0" else as.character(value[1])
    }
    output$value_compounds <- renderText(summary_value("clean_compounds"))
    output$value_pathways <- renderText(summary_value("pathways"))
    output$value_ms2 <- renderText(summary_value("ms2_compounds"))
    output$value_reactions <- renderText(summary_value("supported_pathway_reactions"))
    output$value_links <- renderText(summary_value("pathway_compound_links"))

    output$tbl_summary <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$summary, options = list(scrollX = TRUE, pageLength = 20))
    })
    output$tbl_compounds <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$clean_compounds, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_ms2_info <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$ms2_database@spectra.info, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_ms2_match_log <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$ms2_match_log, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_pathway_map <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$pathway_compound_map, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_reactions <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$pathway_reaction_map, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_removed <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$removed_compounds, options = list(scrollX = TRUE, pageLength = 10))
    })
    observeEvent(input$run, {
      req(input$organism_code, input$organism_name)
      output_dir <- trimws(input$output_dir %||% "")
      if (!nzchar(output_dir)) {
        output_dir <- file.path("Temp", paste0("kegg_", input$organism_code, "_database"))
      }
      if (!grepl("^/", output_dir)) {
        output_dir <- file.path(getwd(), output_dir)
      }

      show_progress_modal("KEGG Database", "Preparing KEGG organism database construction...", 5)
      shinyjs::disable("run")
      on.exit({
        shinyjs::enable("run")
        close_progress_modal()
      }, add = TRUE)

      tryCatch({
        update_progress_modal(15, "Fetching KEGG gene, KO, EC, reaction, and pathway links...")
        result <- metminer_build_kegg_organism_database(
          organism_code = input$organism_code,
          organism_name = input$organism_name,
          output_dir = output_dir,
          min_mw = input$min_mw,
          max_mw = input$max_mw,
          sleep_sec = input$sleep_sec,
          version = as.character(Sys.Date())
        )
        update_progress_modal(100, "Done.")
        state$result <- result
        state$status <- paste0(
          "Completed.\n",
          "Output folder: ", output_dir, "\n",
          "Organism: ", input$organism_name, " (", input$organism_code, ")\n",
          "Compounds: ", nrow(result$clean_compounds), "\n",
          "MS2 compounds: ", nrow(result$ms2_database@spectra.info), "\n",
          "Pathways: ", length(result$pathway_database@pathway_id), "\n",
          "Supported reactions: ", length(unique(result$pathway_reaction_map$reaction_id)), "\n",
          "Ready for download."
        )
        showNotification("KEGG databases constructed successfully.", type = "message")
      }, error = function(e) {
        state$status <- paste("Failed:", e$message)
        showNotification(e$message, type = "error", duration = 8)
      })
    })

    output$download_ms1 <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_ms1.rda"),
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, paste0("kegg_", input$organism_code, "_ms1.rda")), file, overwrite = TRUE)
      }
    )
    output$download_pathway <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_pathway.rda"),
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, paste0("kegg_", input$organism_code, "_pathway.rda")), file, overwrite = TRUE)
      }
    )
    output$download_ms2 <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_ms2.rda"),
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, paste0("kegg_", input$organism_code, "_ms2.rda")), file, overwrite = TRUE)
      }
    )
    output$download_bundle <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_database_", Sys.Date(), ".zip"),
      content = function(file) {
        req(state$result)
        bundle_dir <- tempfile("kegg_bundle_")
        dir.create(bundle_dir)
        files <- list.files(state$result$output_dir, pattern = "\\.(rda|tsv)$", full.names = TRUE)
        file.copy(files, bundle_dir, overwrite = TRUE)
        old_wd <- setwd(bundle_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = basename(files))
      },
      contentType = "application/zip"
    )
  })
}
