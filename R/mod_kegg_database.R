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

          radioButtons(
            ns("kegg_mode"),
            NULL,
            choices = c("Database builder" = "database", "AI review curation" = "review"),
            selected = "database",
            inline = TRUE
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'database'", ns("kegg_mode")),
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
            tags$h6(class = "fw-bold text-primary", "3. Pathway review prompt"),
            numericInput(ns("review_min_coverage"), "Review if reaction coverage below", value = 0.10, min = 0, max = 1, step = 0.01),
            numericInput(ns("review_min_reactions"), "Review if supported reactions below", value = 5, min = 1, step = 1),
            numericInput(ns("review_min_specific_compounds"), "Review if pathway-specific compounds below", value = 3, min = 1, step = 1),
            numericInput(ns("review_hub_cutoff"), "Hub compound frequency cutoff", value = 0.10, min = 0.01, max = 1, step = 0.01),
            numericInput(ns("review_prompt_max"), "Max pathways in prompt", value = 25, min = 1, step = 1),

            tags$hr(),
            tags$h6(class = "fw-bold text-primary", "4. KEGG access"),
            numericInput(ns("sleep_sec"), "Sleep between KEGG requests (sec)", value = 0.35, min = 0.1, step = 0.05),
            tags$small(
              class = "text-muted d-block mb-3",
              "Requests are cached locally. The first run for a species may take several minutes while compound metadata are downloaded."
            ),

            tags$hr(),
            tags$h6(class = "fw-bold text-primary", "5. Output"),
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

          conditionalPanel(
            condition = sprintf("input['%s'] === 'review'", ns("kegg_mode")),
            tags$h6(class = "fw-bold text-primary", "AI review curation"),
            tags$small(
              class = "text-muted d-block mb-3",
              "Upload one or more model review JSON files. Only pathways present in the JSON files are shown for manual curation."
            ),
            fileInput(
              ns("ai_review_json"),
              "Upload AI review JSON files",
              multiple = TRUE,
              accept = c(".json", ".jsonl", "application/json")
            ),
            actionButton(
              ns("load_ai_review"),
              "Load AI Review",
              icon = icon("file-import"),
              class = "btn-outline-primary w-100 mb-2"
            ),
            actionButton(
              ns("save_curated_pathway"),
              "Generate Curated Pathway DB",
              icon = icon("check"),
              class = "btn-success w-100 fw-bold"
            )
          )
        ),

        div(
          class = "p-3",
          conditionalPanel(
            condition = sprintf("input['%s'] === 'database'", ns("kegg_mode")),
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h3("KEGG Organism Metabolite Database Construction", class = "text-primary fw-bold m-0"),
              div(
                downloadButton(ns("download_bundle"), "Download Bundle", class = "btn-success"),
                downloadButton(ns("download_ms1"), "MS1 .rda", class = "btn-outline-primary ms-2"),
                downloadButton(ns("download_ms2"), "MS2 .rda", class = "btn-outline-primary ms-2"),
                downloadButton(ns("download_pathway"), "Pathway .rda", class = "btn-outline-primary ms-2"),
                downloadButton(ns("download_prompt"), "Review Prompt", class = "btn-outline-primary ms-2")
              )
            ),

            bslib::layout_columns(
              col_widths = c(2, 2, 2, 2, 2, 2),
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
              ),
              bslib::value_box(
                title = "Review",
                value = textOutput(ns("value_review"), inline = TRUE),
                showcase = bsicons::bs_icon("clipboard-check")
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
                bslib::nav_panel("Pathway QC", DT::dataTableOutput(ns("tbl_pathway_qc"))),
                bslib::nav_panel("Review Prompt", verbatimTextOutput(ns("txt_review_prompt"), placeholder = TRUE)),
                bslib::nav_panel("Pathway Map", DT::dataTableOutput(ns("tbl_pathway_map"))),
                bslib::nav_panel("Reaction Evidence", DT::dataTableOutput(ns("tbl_reactions"))),
                bslib::nav_panel("Removed Compounds", DT::dataTableOutput(ns("tbl_removed")))
              )
            )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'review'", ns("kegg_mode")),
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h3("KEGG Pathway AI Review Curation", class = "text-primary fw-bold m-0"),
              div(
                downloadButton(ns("download_curated_pathway"), "Curated Pathway .rda", class = "btn-outline-success")
              )
            ),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),
              bslib::value_box("Review files", textOutput(ns("value_ai_files"), inline = TRUE), showcase = bsicons::bs_icon("files")),
              bslib::value_box("Reviewed pathways", textOutput(ns("value_ai_pathways"), inline = TRUE), showcase = bsicons::bs_icon("diagram-3")),
              bslib::value_box("Default keep", textOutput(ns("value_ai_keep"), inline = TRUE), showcase = bsicons::bs_icon("check-circle")),
              bslib::value_box("Default remove", textOutput(ns("value_ai_remove"), inline = TRUE), showcase = bsicons::bs_icon("x-circle"))
            ),
            br(),
            bslib::card(
              bslib::card_header("Curation Status", class = "bg-light"),
              verbatimTextOutput(ns("review_status"), placeholder = TRUE)
            ),
            br(),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Pathway Curation"),
              tags$p(
                class = "text-muted mt-2",
                "Click AI review result to inspect model decisions. Use Keep or Remove in the popup to update the pathway status."
              ),
              uiOutput(ns("tbl_ai_curation"))
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
      ai_review = NULL,
      curation = NULL,
      curated_pathway = NULL,
      active_pathway = NULL,
      ai_review_files = 0L,
      review_status_text = "No AI review JSON loaded.",
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
    output$value_review <- renderText(summary_value("pathways_flagged_for_review"))
    output$value_ai_files <- renderText(as.character(state$ai_review_files %||% 0L))
    output$value_ai_pathways <- renderText({
      if (is.null(state$curation)) return("0")
      as.character(nrow(state$curation))
    })
    output$value_ai_keep <- renderText({
      if (is.null(state$curation)) return("0")
      as.character(sum(state$curation$final_status == "keep", na.rm = TRUE))
    })
    output$value_ai_remove <- renderText({
      if (is.null(state$curation)) return("0")
      as.character(sum(state$curation$final_status == "remove", na.rm = TRUE))
    })
    output$review_status <- renderText({
      state$review_status_text
    })
    lapply(
      c("value_ai_files", "value_ai_pathways", "value_ai_keep", "value_ai_remove", "review_status"),
      function(id) outputOptions(output, id, suspendWhenHidden = FALSE)
    )
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
    output$tbl_pathway_qc <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$pathway_qc, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$tbl_ai_curation <- renderUI({
      req(state$curation)
      table <- state$curation
      if (nrow(table) == 0) {
        return(tags$p(class = "text-muted", "No reviewed pathway was found in the uploaded JSON files."))
      }
      rows <- lapply(seq_len(nrow(table)), function(i) {
        status_class <- switch(
          as.character(table$final_status[i]),
          keep = "text-success fw-bold",
          remove = "text-danger fw-bold",
          review = "text-warning fw-bold",
          "fw-bold"
        )
        tags$tr(
          tags$td(i),
          tags$td(table$pathway_id[i]),
          tags$td(table$pathway_name[i]),
          tags$td(
            tags$a(
              href = "#",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                ns("open_ai_pathway"),
                table$pathway_id[i]
              ),
              "Click to open"
            )
          ),
          tags$td(class = status_class, table$final_status[i])
        )
      })
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-hover align-middle",
          tags$thead(
            tags$tr(
              tags$th("#"),
              tags$th("Pathway ID"),
              tags$th("Pathway Name"),
              tags$th("AI review result"),
              tags$th("Status")
            )
          ),
          tags$tbody(rows)
        )
      )
    })
    outputOptions(output, "tbl_ai_curation", suspendWhenHidden = FALSE)
    output$txt_review_prompt <- renderText({
      req(state$result)
      state$result$pathway_review_prompt
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
          review_min_reaction_coverage = input$review_min_coverage,
          review_min_supported_reactions = input$review_min_reactions,
          review_min_pathway_specific_compounds = input$review_min_specific_compounds,
          review_hub_compound_frequency_cutoff = input$review_hub_cutoff,
          review_prompt_max_pathways = input$review_prompt_max,
          version = as.character(Sys.Date())
        )
        update_progress_modal(100, "Done.")
        state$result <- result
        state$ai_review <- NULL
        state$curation <- NULL
        state$curated_pathway <- NULL
        state$active_pathway <- NULL
        state$ai_review_files <- 0L
        state$review_status_text <- "No AI review JSON loaded."
        state$status <- paste0(
          "Completed.\n",
          "Output folder: ", output_dir, "\n",
          "Organism: ", input$organism_name, " (", input$organism_code, ")\n",
          "Compounds: ", nrow(result$clean_compounds), "\n",
          "MS2 compounds: ", nrow(result$ms2_database@spectra.info), "\n",
          "Pathways: ", length(result$pathway_database@pathway_id), "\n",
          "Pathways flagged for review: ", sum(result$pathway_qc$review_flag, na.rm = TRUE), "\n",
          "Supported reactions: ", length(unique(result$pathway_reaction_map$reaction_id)), "\n",
          "Ready for download."
        )
        showNotification("KEGG databases constructed successfully.", type = "message")
      }, error = function(e) {
        state$status <- paste("Failed:", e$message)
        showNotification(e$message, type = "error", duration = 8)
      })
    })

    observeEvent(input$load_ai_review, {
      req(input$ai_review_json)
      tryCatch({
        review_list <- lapply(seq_len(nrow(input$ai_review_json)), function(i) {
          review <- metminer_kegg_parse_ai_review_json(input$ai_review_json$datapath[i])
          review$model <- tools::file_path_sans_ext(basename(input$ai_review_json$name[i]))
          review
        })
        ai_review <- do.call(rbind, review_list)
        pathway_qc <- if (!is.null(state$result) && "pathway_qc" %in% names(state$result)) state$result$pathway_qc else NULL
        curation <- metminer_kegg_prepare_multi_review_curation(pathway_qc, ai_review)
        curation$final_status <- curation$consensus_decision
        state$ai_review <- ai_review
        state$curation <- curation
        state$curated_pathway <- NULL
        state$ai_review_files <- nrow(input$ai_review_json)
        state$review_status_text <- paste0(
          "AI review files: ", state$ai_review_files, "\n",
          "Model decisions: ", nrow(ai_review), "\n",
          "Reviewed pathways: ", nrow(curation), "\n",
          "Keep: ", sum(curation$final_status == "keep", na.rm = TRUE), "\n",
          "Remove: ", sum(curation$final_status == "remove", na.rm = TRUE), "\n",
          "Review/undecided: ", sum(curation$final_status == "review", na.rm = TRUE)
        )
        output_dir <- if (!is.null(state$result) && !is.null(state$result$output_dir)) {
          state$result$output_dir
        } else {
          trimws(input$output_dir %||% "")
        }
        if (!nzchar(output_dir)) {
          output_dir <- file.path("Temp", paste0("kegg_", input$organism_code %||% "organism", "_database"))
        }
        if (!grepl("^/", output_dir)) output_dir <- file.path(getwd(), output_dir)
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        if (requireNamespace("jsonlite", quietly = TRUE)) {
          jsonlite::write_json(
            ai_review,
            file.path(output_dir, paste0("kegg_", input$organism_code %||% "organism", "_ai_review_merged.json")),
            pretty = TRUE,
            na = "null"
          )
          ai_review_export <- ai_review
          for (col in colnames(ai_review_export)) {
            if (is.list(ai_review_export[[col]])) {
              ai_review_export[[col]] <- vapply(
                ai_review_export[[col]],
                function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null"),
                character(1)
              )
            }
          }
          utils::write.table(
            ai_review_export,
            file.path(output_dir, paste0("kegg_", input$organism_code %||% "organism", "_ai_review_merged.tsv")),
            sep = "\t", quote = FALSE, row.names = FALSE, na = ""
          )
          curation_json <- curation[, setdiff(colnames(curation), "ai_detail_html"), drop = FALSE]
          jsonlite::write_json(
            curation_json,
            file.path(output_dir, paste0("kegg_", input$organism_code %||% "organism", "_ai_review_merged_summary.json")),
            pretty = TRUE,
            na = "null"
          )
        }
        state$status <- paste0(
          state$status,
          "\nAI review loaded: ", nrow(input$ai_review_json), " file(s), ",
          nrow(ai_review), " pathway decisions.\n",
          "Default retained pathways after model voting: ", sum(curation$final_status == "keep", na.rm = TRUE), "."
        )
        showNotification("AI review JSON files loaded. Click pathway rows to curate.", type = "message")
      }, error = function(e) {
        state$review_status_text <- paste("Failed to load AI review JSON:", e$message)
        showNotification(e$message, type = "error", duration = 8)
      })
    })

    observeEvent(input$open_ai_pathway, {
      req(state$curation, state$ai_review)
      pathway_id <- input$open_ai_pathway
      state$active_pathway <- pathway_id
      row <- state$curation[state$curation$pathway_id == pathway_id, , drop = FALSE]
      req(nrow(row) == 1)
      ai_rows <- state$ai_review[state$ai_review$pathway_id == pathway_id, , drop = FALSE]
      showModal(modalDialog(
        title = paste0(pathway_id, " - ", row$pathway_name[1]),
        size = "xl",
        easyClose = TRUE,
        div(
          class = "mb-3",
          tags$p(
            tags$b("Model voting: "),
            paste0("keep=", row$keep_n[1], ", remove=", row$remove_n[1], ", review=", row$review_n[1],
                   "; consensus=", row$consensus_decision[1], "; current status=", row$final_status[1])
          ),
          tags$p(
            tags$b("Pathway evidence: "),
            paste0("reaction coverage=", ifelse(is.na(row$reaction_coverage[1]), "NA", signif(row$reaction_coverage[1], 3)),
                   "; supported reactions=", row$supported_reactions[1],
                   "; pathway-specific compounds=", row$pathway_specific_compounds[1])
          )
        ),
        HTML(metminer_kegg_ai_detail_html(ai_rows)),
        footer = tagList(
          actionButton(ns("review_keep"), "Keep", icon = icon("check"), class = "btn-success"),
          actionButton(ns("review_remove"), "Remove", icon = icon("xmark"), class = "btn-danger"),
          modalButton("Close")
        )
      ))
    })

    observeEvent(input$review_keep, {
      req(state$curation, state$active_pathway)
      curation <- state$curation
      curation$final_status[curation$pathway_id == state$active_pathway] <- "keep"
      state$curation <- curation
      state$review_status_text <- paste0(
        "AI review files: ", state$ai_review_files, "\n",
        "Model decisions: ", if (is.null(state$ai_review)) 0 else nrow(state$ai_review), "\n",
        "Reviewed pathways: ", nrow(curation), "\n",
        "Keep: ", sum(curation$final_status == "keep", na.rm = TRUE), "\n",
        "Remove: ", sum(curation$final_status == "remove", na.rm = TRUE), "\n",
        "Review/undecided: ", sum(curation$final_status == "review", na.rm = TRUE)
      )
      removeModal()
    })

    observeEvent(input$review_remove, {
      req(state$curation, state$active_pathway)
      curation <- state$curation
      curation$final_status[curation$pathway_id == state$active_pathway] <- "remove"
      state$curation <- curation
      state$review_status_text <- paste0(
        "AI review files: ", state$ai_review_files, "\n",
        "Model decisions: ", if (is.null(state$ai_review)) 0 else nrow(state$ai_review), "\n",
        "Reviewed pathways: ", nrow(curation), "\n",
        "Keep: ", sum(curation$final_status == "keep", na.rm = TRUE), "\n",
        "Remove: ", sum(curation$final_status == "remove", na.rm = TRUE), "\n",
        "Review/undecided: ", sum(curation$final_status == "review", na.rm = TRUE)
      )
      removeModal()
    })

    observeEvent(input$save_curated_pathway, {
      req(state$result, state$curation)
      retained_ids <- state$curation$pathway_id[state$curation$final_status == "keep"]
      if (length(retained_ids) == 0) {
        showNotification("Please select at least one pathway to retain.", type = "error", duration = 8)
        return()
      }
      tryCatch({
        curated <- metminer_kegg_save_curated_pathway_database(
          result = state$result,
          retained_pathway_ids = retained_ids,
          organism_code = input$organism_code,
          organism_name = input$organism_name,
          output_dir = state$result$output_dir,
          version = as.character(Sys.Date())
        )
        state$curated_pathway <- curated
        curation_out <- state$curation
        curation_out$retained_by_user <- curation_out$final_status == "keep"
        utils::write.table(
          curation_out,
          file.path(state$result$output_dir, paste0("kegg_", input$organism_code, "_ai_review_curation.tsv")),
          sep = "\t", quote = FALSE, row.names = FALSE, na = ""
        )
        state$status <- paste0(
          state$status,
          "\nCurated pathway database generated: ", curated$file_name,
          "\nRetained pathways: ", length(retained_ids), "."
        )
        showNotification("Curated KEGG pathway database generated.", type = "message")
      }, error = function(e) {
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
    output$download_curated_pathway <- downloadHandler(
      filename = function() {
        req(state$curated_pathway)
        state$curated_pathway$file_name
      },
      content = function(file) {
        req(state$result, state$curated_pathway)
        file.copy(file.path(state$result$output_dir, state$curated_pathway$file_name), file, overwrite = TRUE)
      }
    )
    output$download_ms2 <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_ms2.rda"),
      content = function(file) {
        req(state$result)
        file.copy(file.path(state$result$output_dir, paste0("kegg_", input$organism_code, "_ms2.rda")), file, overwrite = TRUE)
      }
    )
    output$download_prompt <- downloadHandler(
      filename = function() paste0("kegg_", input$organism_code %||% "organism", "_pathway_review_prompt.md"),
      content = function(file) {
        req(state$result)
        writeLines(state$result$pathway_review_prompt, file, useBytes = TRUE)
      },
      contentType = "text/markdown"
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
