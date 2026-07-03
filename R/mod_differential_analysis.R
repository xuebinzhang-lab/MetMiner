#' Differential abundance metabolite analysis UI
#'
#' @param id Module id.
#' @noRd
mod_differential_analysis_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      fillable = FALSE,
      padding = 0,
      sidebar = bslib::sidebar(
        title = "Differential Analysis",
        width = 390,
        bg = "#f8f9fa",
        tags$h6(class = "fw-bold text-primary", "1. Data and groups"),
        radioButtons(
          ns("mode"),
          "Ion mode",
          choices = c("Positive + Negative" = "merged", "Positive" = "positive", "Negative" = "negative"),
          selected = "merged",
          inline = TRUE
        ),
        uiOutput(ns("group_controls")),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "2. DAM statistics"),
        selectInput(ns("mean_median"), "Summary statistic", choices = c("Mean" = "mean", "Median" = "median"), selected = "mean"),
        selectInput(ns("test_method"), "Group test", choices = c("t test" = "t.test", "Wilcoxon" = "wilcox.test"), selected = "t.test"),
        selectInput(ns("p_adjust"), "P adjustment", choices = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"), selected = "BH"),
        numericInput(ns("fc_cutoff"), "Fold-change cutoff", value = 1.5, min = 1, step = 0.1),
        numericInput(ns("p_cutoff"), "P/FDR cutoff", value = 0.05, min = 0, max = 1, step = 0.01),
        checkboxInput(ns("use_fdr"), "Use FDR for significance", value = TRUE),
        tags$hr(),
        checkboxInput(ns("interactive_volcano"), "Interactive volcano", value = TRUE),
        checkboxInput(ns("volcano_annotated_only"), "Show annotated points only in volcano", value = FALSE),
        checkboxInput(ns("run_opls"), "Run lightweight OPLS-DA when available", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("run_opls")),
          selectInput(ns("opls_scale"), "OPLS-DA scaling", choices = c("Pareto" = "pareto", "Standard" = "standard", "Center" = "center", "None" = "none"), selected = "pareto")
        ),
        actionButton(ns("run_dam"), "Run DAMs", icon = icon("chart-line"), class = "btn-teal w-100 fw-bold")
      ),
      div(
        class = "p-3",
        h3("Differential Abundance Metabolites", class = "text-primary fw-bold"),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("Features", textOutput(ns("value_features"), inline = TRUE), showcase = bsicons::bs_icon("grid")),
          bslib::value_box("Up", textOutput(ns("value_up"), inline = TRUE), showcase = bsicons::bs_icon("arrow-up-circle")),
          bslib::value_box("Down", textOutput(ns("value_down"), inline = TRUE), showcase = bsicons::bs_icon("arrow-down-circle")),
          bslib::value_box("OPLS-DA", textOutput(ns("value_opls"), inline = TRUE), showcase = bsicons::bs_icon("diagram-2"))
        ),
        br(),
        bslib::card(bslib::card_header("Status"), verbatimTextOutput(ns("status"))),
        br(),
        bslib::card(
          full_screen = TRUE,
          bslib::navset_tab(
            bslib::nav_panel(
              "DAM Table",
              div(class = "d-flex justify-content-end mb-2", downloadButton(ns("download_dam"), "Download CSV", class = "btn-sm btn-success")),
              DT::dataTableOutput(ns("tbl_dam"))
            ),
            bslib::nav_panel(
              "Volcano",
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(full_screen = TRUE, bslib::card_header("Volcano"), uiOutput(ns("volcano_ui"))),
                bslib::card(bslib::card_header("Group Difference"), plotOutput(ns("boxplot"), height = "320px"))
              ),
              br(),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                bslib::card(bslib::card_header("Selected Feature"), DT::dataTableOutput(ns("tbl_feature_annotation"))),
                bslib::card(bslib::card_header("EIC"), plotly::plotlyOutput(ns("eic_plot"), height = "360px")),
                bslib::card(bslib::card_header("MS2 Spectrum"), plotly::plotlyOutput(ns("ms2_plot"), height = "360px"))
              )
            ),
            bslib::nav_panel(
              "OPLS-DA",
              bslib::card(bslib::card_header("Lightweight OPLS-DA Status"), verbatimTextOutput(ns("opls_status"))),
              br(),
              plotly::plotlyOutput(ns("opls_plot"), height = "520px"),
              br(),
              DT::dataTableOutput(ns("tbl_opls_summary"))
            )
          )
        )
      )
    )
  )
}

#' Differential abundance metabolite analysis server
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues.
#' @param prj_init Project state.
#' @noRd
mod_differential_analysis_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveValues(result = NULL, status = "No differential analysis result yet.", selected_feature = NULL, object = NULL, opls = NULL, opls_status = "Lightweight OPLS-DA has not been evaluated.")

    current_object <- reactive({
      metminer_analysis_object(global_data, input$mode %||% "positive")
    })

    current_polarity_objects <- reactive({
      list(
        positive = metminer_analysis_object(global_data, "positive"),
        negative = metminer_analysis_object(global_data, "negative")
      )
    })

    observe({
      cmp <- tryCatch(metminer_parse_comparison(input$comparison_pair %||% ""), error = function(e) list(control_group = NA_character_, case_group = NA_character_))
      res <- state$result$result %||% data.frame()
      global_data$differential_advisor_state <- list(
        available = TRUE,
        mode = input$mode %||% "positive",
        group_column = input$group_column %||% NA_character_,
        comparison_pair = input$comparison_pair %||% NA_character_,
        control_group = cmp$control_group %||% NA_character_,
        case_group = cmp$case_group %||% NA_character_,
        mean_median = input$mean_median %||% NA_character_,
        test_method = input$test_method %||% NA_character_,
        p_adjust_method = input$p_adjust %||% NA_character_,
        fc_cutoff = input$fc_cutoff %||% NA_real_,
        p_cutoff = input$p_cutoff %||% NA_real_,
        use_fdr = isTRUE(input$use_fdr),
        run_opls = isTRUE(input$run_opls),
        opls_scale = isTRUE(input$opls_scale),
        volcano_annotated_only = isTRUE(input$volcano_annotated_only),
        selected_feature = state$selected_feature %||% NA_character_,
        result_rows = nrow(res),
        up = if ("change" %in% colnames(res)) sum(res$change == "Up", na.rm = TRUE) else NA_integer_,
        down = if ("change" %in% colnames(res)) sum(res$change == "Down", na.rm = TRUE) else NA_integer_,
        status = state$status,
        opls_status = state$opls_status,
        updated_at = as.character(Sys.time())
      )
    })

    output$group_controls <- renderUI({
      obj <- current_object()
      cols <- metminer_sample_group_columns(obj)
      default_col <- if ("group" %in% cols) "group" else cols[1]
      if (length(cols) == 0 || is.na(default_col)) {
        return(tags$div(class = "text-muted small", "No grouping column detected in sample_info."))
      }
      tagList(
        selectInput(ns("group_column"), "Group column", choices = cols, selected = default_col),
        uiOutput(ns("comparison_controls"))
      )
    })

    output$comparison_controls <- renderUI({
      obj <- current_object()
      req(obj, input$group_column)
      groups <- metminer_group_values(obj, input$group_column)
      choices <- metminer_comparison_choices(groups)
      if (length(choices) == 0) {
        return(tags$div(class = "text-muted small", "At least two groups are required for differential analysis."))
      }
      tagList(
        tags$h6(class = "fw-bold text-primary mt-3", "Comparison"),
        selectInput(ns("comparison_pair"), "Comparison (case vs control)", choices = choices, selected = choices[1]),
        div(class = "text-muted small", "Fold change is calculated as case/control for the selected comparison.")
      )
    })

    selected_comparison <- reactive({
      req(input$comparison_pair)
      metminer_parse_comparison(input$comparison_pair)
    })

    observeEvent(input$run_dam, {
      obj <- current_object()
      cmp <- selected_comparison()
      req(obj, input$group_column, cmp$control_group, cmp$case_group)
      if (identical(cmp$control_group, cmp$case_group)) {
        shinyalert::shinyalert("Invalid comparison", "Control and case group must be different.", type = "error")
        return()
      }
      tryCatch({
        res <- metminer_run_dam(
          object = obj,
          group_column = input$group_column,
          control_group = cmp$control_group,
          case_group = cmp$case_group,
          mode = input$mode,
          annotation_filter_result = global_data$annotation_filter_result,
          mean_median = input$mean_median,
          test_method = input$test_method,
          p_adjust_method = input$p_adjust,
          fc_cutoff = input$fc_cutoff,
          p_cutoff = input$p_cutoff,
          use_fdr = input$use_fdr
        )
        state$result <- res
        state$object <- res$object
        state$selected_feature <- if (nrow(res$result) > 0) res$result$variable_id[1] else NULL
        opls <- metminer_opls_ready(obj, input$group_column, cmp$control_group, cmp$case_group)
        state$opls <- NULL
        state$opls_status <- opls$message
        if (isTRUE(input$run_opls) && isTRUE(opls$ready)) {
          state$object <- metminer_mutate_oplsda(
            object = state$object,
            group_column = input$group_column,
            control_group = cmp$control_group,
            case_group = cmp$case_group,
            scale = input$opls_scale
          )
          state$opls <- metminer_extract_oplsda_result(state$object)
          state$result$result <- metminer_extract_dam_table(state$object, global_data$annotation_filter_result, input$mode, state$result$p_column)
          state$opls_status <- paste0(opls$message, "\nLightweight OPLS-DA model fitted with one predictive and one orthogonal component.")
        } else if (isTRUE(input$run_opls) && !isTRUE(opls$ready)) {
          state$opls_status <- paste0(opls$message, "\nLightweight OPLS-DA was not fitted.")
        }
        state$status <- paste0(
          "Comparison: ", cmp$case_group, " vs ", cmp$control_group, "\n",
          "Mode: ", if (identical(input$mode, "merged")) "positive + negative" else input$mode, "\n",
          "Control samples: ", length(res$control_ids), "\n",
          "Case samples: ", length(res$case_ids), "\n",
          "Features tested: ", nrow(res$result), "\n",
          "Up: ", sum(res$result$change == "Up", na.rm = TRUE), "\n",
          "Down: ", sum(res$result$change == "Down", na.rm = TRUE), "\n",
          opls$message
        )
        global_data$differential_result <- res
        global_data$differential_object <- state$object
        if (!is.null(prj_init$mass_dataset_dir)) {
          differential_result <- res
          differential_object <- state$object
          save(differential_result, file = file.path(prj_init$mass_dataset_dir, "10.differential_result.rda"))
          save(differential_object, file = file.path(prj_init$mass_dataset_dir, "10.object_differential.rda"))
        }
        shinyalert::shinyalert("DAM Analysis Completed", "Differential abundance table and volcano plot were generated.", type = "success")
      }, error = function(e) {
        state$status <- paste("Differential analysis failed:", e$message)
        shinyalert::shinyalert("Error", state$status, type = "error")
      })
    })

    observeEvent({
      req(isTRUE(input$interactive_volcano), state$result)
      suppressWarnings(plotly::event_data("plotly_click", source = "dam_volcano", priority = "event"))
    }, {
      click <- suppressWarnings(plotly::event_data("plotly_click", source = "dam_volcano", priority = "event"))
      if (!is.null(click$key) && has_text(click$key)) {
        state$selected_feature <- as.character(click$key[1])
      }
    }, ignoreInit = TRUE)

    output$status <- renderText(state$status)
    output$opls_status <- renderText(state$opls_status)
    output$value_features <- renderText(if (is.null(state$result)) "0" else nrow(state$result$result))
    output$value_up <- renderText(if (is.null(state$result)) "0" else sum(state$result$result$change == "Up", na.rm = TRUE))
    output$value_down <- renderText(if (is.null(state$result)) "0" else sum(state$result$result$change == "Down", na.rm = TRUE))
    output$value_opls <- renderText({
      obj <- current_object()
      if (is.null(obj) || is.null(input$group_column) || is.null(input$comparison_pair)) return("NA")
      cmp <- selected_comparison()
      if (isTRUE(metminer_opls_ready(obj, input$group_column, cmp$control_group, cmp$case_group)$ready)) "Ready" else "Blocked"
    })

    output$tbl_dam <- DT::renderDataTable({
      req(state$result)
      DT::datatable(state$result$result, rownames = FALSE, selection = "single", options = list(scrollX = TRUE, pageLength = 15))
    })

    observeEvent(input$tbl_dam_rows_selected, {
      req(state$result)
      idx <- input$tbl_dam_rows_selected
      if (length(idx) == 1) state$selected_feature <- state$result$result$variable_id[idx]
    })

    output$volcano_ui <- renderUI({
      if (isTRUE(input$interactive_volcano)) {
        plotly::plotlyOutput(ns("volcano_plotly"), height = "320px")
      } else {
        plotOutput(ns("volcano_static"), height = "320px")
      }
    })

    output$volcano_plotly <- plotly::renderPlotly({
      req(state$result)
      metminer_plot_volcano(
        state$result$result,
        input$fc_cutoff,
        input$p_cutoff,
        state$result$p_column,
        interactive = TRUE,
        annotated_only = isTRUE(input$volcano_annotated_only)
      )
    })
    output$volcano_static <- renderPlot({
      req(state$result)
      metminer_plot_volcano(
        state$result$result,
        input$fc_cutoff,
        input$p_cutoff,
        state$result$p_column,
        interactive = FALSE,
        annotated_only = isTRUE(input$volcano_annotated_only)
      )
    })

    output$opls_plot <- plotly::renderPlotly({
      req(state$opls)
      metminer_plot_opls_score(state$opls)
    })

    output$tbl_opls_summary <- DT::renderDataTable({
      req(state$opls)
      DT::datatable(as.data.frame(state$opls$summary), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    selected_row <- reactive({
      req(state$result, state$selected_feature)
      x <- state$result$result
      x[x$variable_id == state$selected_feature, , drop = FALSE]
    })

    output$tbl_feature_annotation <- DT::renderDataTable({
      row <- selected_row()
      keep <- intersect(c("variable_id", "Compound.name", "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "Database", "Level", "fc", "log2_fc", "p_value", "p_value_adjust", "change", "opls_vip_like", "opls_loading_predictive", "opls_loading_orthogonal"), colnames(row))
      DT::datatable(row[, keep, drop = FALSE], rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$boxplot <- renderPlot({
      req(state$object, state$result, state$selected_feature)
      row <- selected_row()
      metminer_plot_feature_boxplot(
        state$object,
        state$selected_feature,
        input$group_column,
        selected_comparison()$control_group,
        selected_comparison()$case_group,
        p_value = row$p_value[1],
        p_adjust = row$p_value_adjust[1]
      )
    })

    output$eic_plot <- plotly::renderPlotly({
      req(state$object, state$selected_feature)
      objs <- current_polarity_objects()
      eic <- if (identical(input$mode, "merged")) {
        make_feature_eic_data(
          wd = prj_init$wd,
          positive_object = objs$positive,
          negative_object = objs$negative,
          feature_id = state$selected_feature,
          max_traces = 8
        )
      } else {
        make_feature_eic_data(
          wd = prj_init$wd,
          object = state$object,
          feature_id = state$selected_feature,
          mode = input$mode,
          max_traces = 8
        )
      }
      plot_feature_eic(eic)
    })

    output$ms2_plot <- plotly::renderPlotly({
      req(state$object, state$selected_feature)
      objs <- current_polarity_objects()
      dat <- if (identical(input$mode, "merged")) {
        make_ms2_spectrum_data(
          positive_object = objs$positive,
          negative_object = objs$negative,
          feature_id = state$selected_feature,
          mz_tol = 0.02,
          ms2_mz_tol_ppm = 5,
          ms2_rt_tol = 10,
          top_n = 12
        )
      } else {
        make_ms2_spectrum_data(
          object = state$object,
          feature_id = state$selected_feature,
          mz_tol = 0.02,
          ms2_mz_tol_ppm = 5,
          ms2_rt_tol = 10,
          top_n = 12
        )
      }
      plot_ms2_spectrum(dat)
    })

    output$download_dam <- downloadHandler(
      filename = function() paste0("metminer_dam_", input$mode, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(state$result)
        utils::write.csv(state$result$result, file, row.names = FALSE)
      }
    )
  })
}
