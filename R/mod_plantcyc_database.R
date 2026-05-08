#' PlantCyc Metabolite Database Construction UI Module
#'
#' @param id Module id.
#' @noRd
mod_plantcyc_database_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .plantcyc-help-modal {
        max-height: 70vh;
        overflow-y: auto;
        padding-right: 0.5rem;
      }
      .plantcyc-help-modal h1,
      .plantcyc-help-modal h2 {
        color: #008080;
        margin-top: 1rem;
      }
      .plantcyc-help-modal code {
        color: #0f5132;
      }
      .plantcyc-help-modal img {
        display: block;
        max-width: 100%;
        height: auto;
        margin: 0.75rem 0 1.25rem 0;
        border: 1px solid #dee2e6;
      }
    ")),
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "PlantCyc Database",
          width = 390,
          bg = "#f8f9fa",

          actionButton(
            ns("show_help_sidebar"),
            "How to get PlantCyc SmartTables",
            icon = icon("circle-question"),
            class = "btn-outline-primary w-100 mb-3"
          ),

          tags$h6(class = "fw-bold text-primary", "1. SmartTable exports"),
          fileInput(
            ns("compound_file"),
            "Compound SmartTable (.txt/.tsv)",
            accept = c(".txt", ".tsv")
          ),
          fileInput(
            ns("pathway_file"),
            "Pathway SmartTable (.txt/.tsv)",
            accept = c(".txt", ".tsv")
          ),
          tags$small(
            class = "text-muted d-block mb-3",
            "Use PlantCyc SmartTable exports for compounds and all pathways."
          ),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "2. LC-MS filters"),
          numericInput(ns("min_mw"), "Minimum MW", value = 70, min = 0, step = 1),
          numericInput(ns("max_mw"), "Maximum MW", value = 1500, min = 100, step = 10),
          numericInput(ns("mass_ppm"), "MS2 match validation ppm", value = 10, min = 1, step = 1),
          numericInput(ns("mass_da"), "MS2 match validation Da", value = 0.01, min = 0.001, step = 0.001),

          tags$hr(),
          tags$h6(class = "fw-bold text-primary", "3. Optional classification"),
          checkboxInput(ns("use_classyfire"), "Add ClassyFire classification via Fiehn CFB", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("use_classyfire")),
            numericInput(ns("classyfire_sleep"), "Sleep between requests (sec)", value = 2, min = 1, step = 0.5),
            numericInput(ns("classyfire_retries"), "Max retries", value = 3, min = 1, max = 5, step = 1),
            tags$small(
              class = "text-muted d-block mb-2",
              "ClassyFire lookup first reuses the local PlantCyc cache, then queries only missing InChIKeys with throttling."
            )
          ),

          tags$hr(),
          tags$h6(class = "fw-bold text-primary", "4. Output"),
          textInput(
            ns("output_dir"),
            "Output folder",
            value = file.path("Temp", "plantcyc_database"),
            placeholder = "Temp/plantcyc_database"
          ),
          actionButton(
            ns("run"),
            "Build PlantCyc Databases",
            icon = icon("database"),
            class = "btn-teal w-100 fw-bold shadow-sm"
          )
        ),

        div(
          class = "p-3",
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            div(
              class = "d-flex align-items-center gap-2",
              h3("PlantCyc Metabolite Database Construction", class = "text-primary fw-bold m-0"),
              actionButton(
                ns("show_help_header"),
                NULL,
                icon = icon("circle-question"),
                class = "btn btn-outline-primary btn-sm",
                title = "How to use this toolkit"
              )
            ),
            div(
              downloadButton(ns("download_bundle"), "Download Bundle", class = "btn-success"),
              downloadButton(ns("download_ms1"), "MS1 .rda", class = "btn-outline-primary ms-2"),
              downloadButton(ns("download_ms2"), "MS2 .rda", class = "btn-outline-primary ms-2"),
              downloadButton(ns("download_pathway"), "Pathway .rda", class = "btn-outline-primary ms-2")
            )
          ),

          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            bslib::value_box(
              title = "MS1 Compounds",
              value = textOutput(ns("value_ms1"), inline = TRUE),
              showcase = bsicons::bs_icon("boxes")
            ),
            bslib::value_box(
              title = "MS2 Compounds",
              value = textOutput(ns("value_ms2"), inline = TRUE),
              showcase = bsicons::bs_icon("soundwave")
            ),
            bslib::value_box(
              title = "MS2 Spectra",
              value = textOutput(ns("value_spectra"), inline = TRUE),
              showcase = bsicons::bs_icon("activity")
            ),
            bslib::value_box(
              title = "Pathways",
              value = textOutput(ns("value_pathways"), inline = TRUE),
              showcase = bsicons::bs_icon("diagram-3")
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
              bslib::nav_panel("MS2 Match Log", DT::dataTableOutput(ns("tbl_match_log"))),
              bslib::nav_panel("MS2 Compounds", DT::dataTableOutput(ns("tbl_ms2_info"))),
              bslib::nav_panel("Pathway Map", DT::dataTableOutput(ns("tbl_pathway_map"))),
              bslib::nav_panel("ClassyFire", DT::dataTableOutput(ns("tbl_classyfire"))),
              bslib::nav_panel("Removed Compounds", DT::dataTableOutput(ns("tbl_removed")))
            )
          )
        )
      )
    )
  )
}

#' PlantCyc Metabolite Database Construction Server Module
#'
#' @param id Module id.
#' @noRd
mod_plantcyc_database_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal <- progress_handlers$close_progress_modal

    state <- reactiveValues(
      result = NULL,
      status = "Waiting for PlantCyc SmartTable uploads.",
      output_dir = NULL
    )

    show_help <- function() {
      help_file <- app_sys("app/plantcyc_database_how_to_use.md")
      help_text <- if (file.exists(help_file)) {
        paste(readLines(help_file, warn = FALSE), collapse = "\n")
      } else {
        "Help document is not available."
      }

      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center gap-2",
          icon("circle-question"),
          "PlantCyc SmartTable Guide"
        ),
        div(
          class = "plantcyc-help-modal",
          HTML(suppressWarnings(markdown::markdownToHTML(text = help_text, fragment.only = TRUE)))
        ),
        footer = tagList(
          tags$a(
            class = "btn btn-outline-primary",
            href = "https://plantcyc.org/list-of-pgdbs-2/",
            target = "_blank",
            "Open PlantCyc PGDB List"
          ),
          modalButton("Close")
        ),
        easyClose = TRUE,
        size = "l"
      ))
    }

    observeEvent(input$show_help_sidebar, show_help())
    observeEvent(input$show_help_header, show_help())

    observeEvent(input$use_classyfire, {
      if (!isTRUE(input$use_classyfire)) return()
      shinyalert::shinyalert(
        title = "Online ClassyFire Lookup",
        text = paste(
          "This option requires an internet connection and may take a long time for large PlantCyc SmartTables.",
          "MetMiner will reuse the local PlantCyc classification cache first and only query missing InChIKeys from Fiehn CFB.",
          "Requests are throttled with sleep time to avoid stressing the public server."
        ),
        type = "warning",
        closeOnClickOutside = TRUE
      )
    }, ignoreInit = TRUE)

    output$value_ms1 <- renderText({
      if (is.null(state$result)) return("0")
      value <- state$result$summary$value[state$result$summary$metric == "ms1_compounds"]
      if (length(value) == 0) "0" else as.character(value[1])
    })

    output$value_ms2 <- renderText({
      if (is.null(state$result)) return("0")
      value <- state$result$summary$value[state$result$summary$metric == "ms2_compounds"]
      if (length(value) == 0) "0" else as.character(value[1])
    })

    output$value_spectra <- renderText({
      if (is.null(state$result)) return("0")
      pos <- state$result$summary$value[state$result$summary$metric == "ms2_positive_spectra"]
      neg <- state$result$summary$value[state$result$summary$metric == "ms2_negative_spectra"]
      as.character(sum(c(pos, neg), na.rm = TRUE))
    })

    output$value_pathways <- renderText({
      if (is.null(state$result)) return("0")
      value <- state$result$summary$value[state$result$summary$metric == "pathway_count"]
      if (length(value) == 0) "0" else as.character(value[1])
    })

    output$status <- renderText({
      state$status
    })

    output$tbl_summary <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$summary, options = list(scrollX = TRUE, pageLength = 20))
    })

    output$tbl_match_log <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$ms2_match_log, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_ms2_info <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$ms2_database@spectra.info, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_pathway_map <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$clean_result$pathway_compound_map, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_classyfire <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$classyfire_classification, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_removed <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$clean_result$removed_compounds, options = list(scrollX = TRUE, pageLength = 10))
    })

    observeEvent(input$run, {
      req(input$compound_file, input$pathway_file)
      output_dir <- trimws(input$output_dir %||% "")
      if (!nzchar(output_dir)) {
        output_dir <- file.path(tempdir(), "plantcyc_database")
      }
      if (!grepl("^/", output_dir)) {
        output_dir <- file.path(getwd(), output_dir)
      }
      state$output_dir <- output_dir

      show_progress_modal("PlantCyc Database", "Checking uploaded SmartTables...", 5)
      shinyjs::disable("run")
      on.exit({
        shinyjs::enable("run")
        close_progress_modal()
      }, add = TRUE)

      tryCatch({
        update_progress_modal(15, "Cleaning PlantCyc compound and pathway tables...")
        clean_result <- metminer_clean_plantcyc_smarttables(
          compound_file = input$compound_file$datapath,
          pathway_file = input$pathway_file$datapath,
          output_dir = file.path(output_dir, "cleaned"),
          min_mw = input$min_mw,
          max_mw = input$max_mw
        )

        classyfire_classification <- data.frame()
        classyfire_local_cache_file <- metminer_plantcyc_classyfire_cache_file()
        classyfire_local_cache_hits <- 0
        classyfire_cfb_queries <- 0
        if (isTRUE(input$use_classyfire)) {
          update_progress_modal(25, "Checking local PlantCyc ClassyFire cache...")
          classyfire_result <- metminer_add_classyfire_to_compounds(
            clean_compounds = clean_result$clean_compounds,
            cache_dir = file.path(output_dir, "classyfire_cache"),
            sleep_sec = input$classyfire_sleep,
            max_retries = input$classyfire_retries,
            progress = function(i, total, inchikey) {
              if (i == 1 || i == total || i %% 10 == 0) {
                update_progress_modal(
                  min(45, 25 + round(20 * i / total)),
                  paste0("ClassyFire classification: ", i, "/", total, " (", inchikey, ")")
                )
              }
            }
          )
          clean_result$clean_compounds <- classyfire_result$compounds
          ms2_classyfire_result <- metminer_add_classyfire_to_compounds(
            clean_compounds = clean_result$ms2_eligible_compounds,
            cache_dir = file.path(output_dir, "classyfire_cache"),
            sleep_sec = input$classyfire_sleep,
            max_retries = input$classyfire_retries,
            progress = function(i, total, inchikey) {
              if (i == 1 || i == total || i %% 10 == 0) {
                update_progress_modal(
                  min(45, 35 + round(10 * i / total)),
                  paste0("ClassyFire MS2-eligible classification: ", i, "/", total, " (", inchikey, ")")
                )
              }
            },
            local_cache_file = classyfire_result$local_cache_file
          )
          clean_result$ms2_eligible_compounds <- ms2_classyfire_result$compounds
          classyfire_result$classification <- unique(rbind(
            classyfire_result$classification,
            ms2_classyfire_result$classification
          ))
          classyfire_classification <- classyfire_result$classification
          classyfire_local_cache_file <- classyfire_result$local_cache_file
          classyfire_local_cache_hits <- classyfire_result$local_cache_hits + ms2_classyfire_result$local_cache_hits
          classyfire_cfb_queries <- classyfire_result$cfb_queries + ms2_classyfire_result$cfb_queries
        }

        update_progress_modal(45, "Constructing PlantCyc MS1 databaseClass...")
        ms1_db <- metminer_build_plantcyc_ms1_database(
          clean_result$clean_compounds,
          version = as.character(Sys.Date())
        )

        update_progress_modal(58, "Extracting matching MS2 spectra from massdbbuildin...")
        ms2_result <- metminer_build_plantcyc_ms2_database(
          clean_compounds = clean_result$ms2_eligible_compounds,
          version = as.character(Sys.Date()),
          mass_tolerance_ppm = input$mass_ppm,
          mass_tolerance_da = input$mass_da
        )
        ms2_db <- ms2_result$database

        update_progress_modal(74, "Constructing PlantCyc pathway database...")
        pathway_result <- metminer_build_plantcyc_pathway_database(
          pathway_compound_map = clean_result$pathway_compound_map,
          clean_pathways = clean_result$clean_pathways,
          version = as.character(Sys.Date())
        )
        pathway_db <- pathway_result$database

        update_progress_modal(82, "Saving RDA databases and QC tables...")
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        plantcyc_ftataricum_ms1 <- ms1_db
        plantcyc_ftataricum_ms2 <- ms2_db
        plantcyc_ftataricum_pathway <- pathway_db
        save(plantcyc_ftataricum_ms1, file = file.path(output_dir, "plantcyc_ftataricum_ms1.rda"))
        save(plantcyc_ftataricum_ms2, file = file.path(output_dir, "plantcyc_ftataricum_ms2.rda"))
        save(plantcyc_ftataricum_pathway, file = file.path(output_dir, "plantcyc_ftataricum_pathway.rda"))
        utils::write.table(ms1_db@spectra.info, file.path(output_dir, "plantcyc_ms1_spectra_info.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(clean_result$ms2_eligible_compounds, file.path(output_dir, "plantcyc_ms2_eligible_compounds.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(metminer_plantcyc_coa_fragment_rules(), file.path(output_dir, "plantcyc_coa_fragment_rules.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(ms2_db@spectra.info, file.path(output_dir, "plantcyc_ms2_spectra_info.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(ms2_result$match_log, file.path(output_dir, "plantcyc_ms2_match_log.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(ms2_result$unmatched_compounds, file.path(output_dir, "plantcyc_ms2_unmatched_compounds.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        utils::write.table(pathway_result$reaction_table, file.path(output_dir, "plantcyc_pathway_reaction_table.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        if (nrow(classyfire_classification) > 0) {
          utils::write.table(classyfire_classification, file.path(output_dir, "plantcyc_classyfire_classification.tsv"),
                             sep = "\t", quote = FALSE, row.names = FALSE, na = "")
          utils::write.table(clean_result$clean_compounds, file.path(output_dir, "plantcyc_clean_compounds_with_classyfire.tsv"),
                             sep = "\t", quote = FALSE, row.names = FALSE, na = "")
          file.copy(classyfire_local_cache_file, file.path(output_dir, "plantcyc_classyfire_local_cache_snapshot.tsv"), overwrite = TRUE)
        }

        positive_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.positive, length, integer(1)))
        negative_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.negative, length, integer(1)))
        summary <- data.frame(
          metric = c(
            "ms1_compounds", "ms2_compounds", "ms2_positive_compounds", "ms2_negative_compounds",
            "ms2_positive_spectra", "ms2_negative_spectra", "ms2_match_rows", "ms2_unmatched_compounds",
            "ms2_eligible_compounds", "coa_retained_for_ms2",
            "pathway_count", "pathway_compound_links", "pathway_reaction_links",
            "classyfire_completed", "classyfire_not_found", "classyfire_failed",
            "classyfire_local_cache_hits", "classyfire_cfb_queries"
          ),
          value = c(
            nrow(ms1_db@spectra.info), nrow(ms2_db@spectra.info),
            length(ms2_db@spectra.data$Spectra.positive), length(ms2_db@spectra.data$Spectra.negative),
            positive_spectra, negative_spectra, nrow(ms2_result$match_log), nrow(ms2_result$unmatched_compounds),
            nrow(clean_result$ms2_eligible_compounds),
            sum(grepl("coa_related", clean_result$removed_compounds$filter_reason, fixed = TRUE) &
                  clean_result$removed_compounds$ms2_eligible, na.rm = TRUE),
            length(pathway_db@pathway_id), nrow(clean_result$pathway_compound_map), nrow(pathway_result$reaction_table),
            sum(classyfire_classification$classyfire_status == "completed", na.rm = TRUE),
            sum(classyfire_classification$classyfire_status %in% c("not_found", "no_data"), na.rm = TRUE),
            sum(classyfire_classification$classyfire_status == "failed", na.rm = TRUE),
            classyfire_local_cache_hits,
            classyfire_cfb_queries
          ),
          stringsAsFactors = FALSE
        )
        if (nrow(ms2_result$match_log) > 0) {
          source_table <- table(ms2_result$match_log$source_database)
          type_table <- table(ms2_result$match_log$match_type)
          summary <- rbind(
            summary,
            data.frame(metric = paste0("match_source_", names(source_table)), value = as.integer(source_table)),
            data.frame(metric = paste0("match_type_", names(type_table)), value = as.integer(type_table))
          )
        }
        utils::write.table(summary, file.path(output_dir, "plantcyc_database_summary.tsv"),
                           sep = "\t", quote = FALSE, row.names = FALSE, na = "")

        result <- list(
          ms1_database = ms1_db,
          ms2_database = ms2_db,
          pathway_database = pathway_db,
          clean_result = clean_result,
          ms2_match_log = ms2_result$match_log,
          ms2_unmatched_compounds = ms2_result$unmatched_compounds,
          pathway_reaction_table = pathway_result$reaction_table,
          coa_fragment_rules = metminer_plantcyc_coa_fragment_rules(),
          classyfire_classification = classyfire_classification,
          summary = summary,
          output_dir = output_dir
        )
        state$result <- result
        state$status <- paste0(
          "Completed.\n",
          "Output folder: ", output_dir, "\n",
          "MS1 compounds: ", nrow(ms1_db@spectra.info), "\n",
          "MS2 compounds: ", nrow(ms2_db@spectra.info), "\n",
          "MS2 spectra: ", positive_spectra + negative_spectra, "\n",
          "MS2-eligible compounds: ", nrow(clean_result$ms2_eligible_compounds), "\n",
          "CoA compounds retained for MS2 only: ",
          sum(grepl("coa_related", clean_result$removed_compounds$filter_reason, fixed = TRUE) &
                clean_result$removed_compounds$ms2_eligible, na.rm = TRUE), "\n",
          "Pathways: ", length(pathway_db@pathway_id), "\n",
          "ClassyFire local cache hits: ", classyfire_local_cache_hits, "\n",
          "ClassyFire CFB queries: ", classyfire_cfb_queries
        )
        update_progress_modal(100, "Done.")
        showNotification("PlantCyc databases constructed successfully.", type = "message")
      }, error = function(e) {
        state$status <- paste("Failed:", e$message)
        showNotification(e$message, type = "error", duration = 8)
      })
    })

    output$download_ms1 <- downloadHandler(
      filename = function() "plantcyc_ftataricum_ms1.rda",
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, "plantcyc_ftataricum_ms1.rda"), file, overwrite = TRUE)
      }
    )

    output$download_ms2 <- downloadHandler(
      filename = function() "plantcyc_ftataricum_ms2.rda",
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, "plantcyc_ftataricum_ms2.rda"), file, overwrite = TRUE)
      }
    )

    output$download_pathway <- downloadHandler(
      filename = function() "plantcyc_ftataricum_pathway.rda",
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, "plantcyc_ftataricum_pathway.rda"), file, overwrite = TRUE)
      }
    )

    output$download_bundle <- downloadHandler(
      filename = function() paste0("plantcyc_ftataricum_database_", Sys.Date(), ".zip"),
      content = function(file) {
        req(state$result)
        bundle_dir <- tempfile("plantcyc_bundle_")
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
