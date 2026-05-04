#' Feature-network assisted annotation UI
#'
#' @param id Module id.
#' @noRd
mod_feature_annotation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "Network Annotation",
          width = 350,
          bg = "#f8f9fa",

          tags$h6(class = "fw-bold text-primary", "1. Data"),
          selectInput(ns("mode"), "Ion Mode:", choices = c("Positive" = "positive", "Negative" = "negative"), selected = "positive"),
          actionButton(ns("refresh_validation"), "Refresh Network Validation", icon = icon("rotate"), class = "btn-teal w-100 fw-bold shadow-sm"),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "2. EIC"),
          numericInput(ns("eic_rt_window"), "RT Window (sec):", value = 15, min = 1, step = 1),
          numericInput(ns("eic_mz_window"), "m/z Window:", value = 0.01, min = 0.0001, step = 0.001),
          numericInput(ns("eic_max_traces"), "Max Traces:", value = 20, min = 1, step = 1),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "3. MS2"),
          numericInput(ns("ms2_mz_tol_ppm"), "MS1-MS2 Match m/z (ppm):", value = 5, min = 0.1, step = 0.5),
          numericInput(ns("ms2_rt_tol"), "MS1-MS2 Match RT (sec):", value = 10, min = 0.1, step = 1),
          numericInput(ns("ms2_fragment_mz_tol"), "Fragment m/z Tolerance:", value = 0.02, min = 0.001, step = 0.001),
          numericInput(ns("ms2_top_n"), "Annotate Top N Peaks:", value = 12, min = 3, step = 1)
        ),

        div(
          class = "p-3",
          bslib::navset_tab(
            bslib::nav_panel(
              "Analysis",
              tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Validation Status"),
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(bslib::card_header("Positive Mode", class = "bg-info-subtle text-info-emphasis"), verbatimTextOutput(ns("status_pos"))),
                bslib::card(bslib::card_header("Negative Mode", class = "bg-warning-subtle text-warning-emphasis"), verbatimTextOutput(ns("status_neg")))
              ),
              br(),
              bslib::card(
                full_screen = TRUE,
                bslib::navset_tab(
                  bslib::nav_panel("Compound Hypothesis", DT::dataTableOutput(ns("tbl_hypothesis"))),
                  bslib::nav_panel("Feature Roles", DT::dataTableOutput(ns("tbl_roles"))),
                  bslib::nav_panel("Feature Selection", DT::dataTableOutput(ns("tbl_selection"))),
                  bslib::nav_panel("TopN Candidates", DT::dataTableOutput(ns("tbl_candidates"))),
                  bslib::nav_panel("Edge Consistency", DT::dataTableOutput(ns("tbl_edges")))
                )
              )
            ),

            bslib::nav_panel(
              "Visualize",
              bslib::layout_columns(
                col_widths = c(6, 6, 6, 6),
                bslib::card(
                  height = "560px",
                  full_screen = TRUE,
                  bslib::card_header("1. Sub-network Ontology"),
                  selectInput(ns("sub_network"), "Sub-network:", choices = NULL),
                  tags$small(class = "text-muted d-block mb-2", "Select a node in the DAG to update spectra and chromatogram."),
                  visNetwork::visNetworkOutput(ns("ontology_plot"), height = "430px")
                ),
                bslib::card(
                  height = "560px",
                  full_screen = TRUE,
                  bslib::card_header("2. Chromatogram"),
                  selectizeInput(ns("eic_samples"), "Samples:", choices = NULL, selected = NULL, multiple = TRUE,
                                 options = list(plugins = list("remove_button"), placeholder = "Top traces or selected samples")),
                  plotly::plotlyOutput(ns("eic_plot"), height = "420px")
                ),
                bslib::card(
                  height = "560px",
                  full_screen = TRUE,
                  bslib::card_header("3. MS1 Feature Spectrum"),
                  verbatimTextOutput(ns("selected_feature_text"), placeholder = TRUE),
                  plotly::plotlyOutput(ns("ms1_plot"), height = "470px")
                ),
                bslib::card(
                  height = "560px",
                  full_screen = TRUE,
                  bslib::card_header("4. MS2 Spectrum"),
                  plotly::plotlyOutput(ns("ms2_plot"), height = "500px")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Feature-network assisted annotation server
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues data store.
#' @param prj_init Project init reactiveValues.
#' @noRd
mod_feature_annotation_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {

    selected_feature <- reactiveVal(NULL)

    get_object <- function(mode = input$mode) {
      if (identical(mode, "positive")) {
        if (!is.null(global_data$object_pos_annotated)) return(global_data$object_pos_annotated)
        if (!is.null(global_data$object_pos_network)) return(global_data$object_pos_network)
        return(NULL)
      }
      if (!is.null(global_data$object_neg_annotated)) return(global_data$object_neg_annotated)
      if (!is.null(global_data$object_neg_network)) return(global_data$object_neg_network)
      NULL
    }

    update_validated_object <- function(mode) {
      obj <- get_object(mode)
      if (is.null(obj)) return(NULL)
      obj <- metminer_validate_annotations_with_feature_network(obj)
      if (identical(mode, "positive")) {
        global_data$object_pos_annotated <- obj
        if (!is.null(prj_init$mass_dataset_dir)) {
          object_pos_annotated <- obj
          save(object_pos_annotated, file = file.path(prj_init$mass_dataset_dir, "07.object_pos_annotated.rda"))
        }
      } else {
        global_data$object_neg_annotated <- obj
        if (!is.null(prj_init$mass_dataset_dir)) {
          object_neg_annotated <- obj
          save(object_neg_annotated, file = file.path(prj_init$mass_dataset_dir, "07.object_neg_annotated.rda"))
        }
      }
      obj
    }

    observeEvent(input$refresh_validation, {
      update_validated_object(input$mode)
      shinyalert::shinyalert("Validation Updated", "Feature-network assisted annotation tables were refreshed.", type = "success")
    })

    validation <- reactive({
      obj <- get_object()
      req(obj)
      val <- metminer_extract_annotation_validation(obj)
      if (is.null(val$feature_role_interpretation) || nrow(val$feature_role_interpretation) == 0) {
        obj <- update_validated_object(input$mode)
        val <- metminer_extract_annotation_validation(obj)
      }
      val
    })

    observe({
      obj <- get_object()
      if (is.null(obj)) {
        updateSelectInput(session, "sub_network", choices = character())
        return()
      }
      choices <- metminer_annotation_ontology_subnetwork_choices(obj)
      updateSelectInput(session, "sub_network", choices = choices, selected = if (length(choices) > 0) unname(choices[1]) else character())
    })

    observeEvent(input$sub_network, {
      roles <- validation()$feature_role_interpretation
      roles <- roles[roles$sub_network == suppressWarnings(as.integer(input$sub_network)), , drop = FALSE]
      parent <- roles$feature_id[roles$network_role == "putative_parent"][1]
      selected_feature(parent %||% roles$feature_id[1])
    }, ignoreInit = FALSE)

    observeEvent(input$ontology_plot_selected, {
      if (has_text(input$ontology_plot_selected)) {
        selected_feature(as.character(input$ontology_plot_selected)[1])
      }
    }, ignoreInit = TRUE)

    output$status_pos <- renderText(metminer_annotation_validation_status(global_data$object_pos_annotated, global_data$object_pos_network, "positive"))
    output$status_neg <- renderText(metminer_annotation_validation_status(global_data$object_neg_annotated, global_data$object_neg_network, "negative"))

    render_validation_table <- function(table_name) {
      table <- validation()[[table_name]]
      validate(need(!is.null(table) && nrow(table) > 0, "No validation table available. Run annotation and feature network first."))
      DT::datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    }

    output$tbl_hypothesis <- DT::renderDataTable(render_validation_table("subnetwork_hypothesis"))
    output$tbl_roles <- DT::renderDataTable(render_validation_table("feature_role_interpretation"))
    output$tbl_selection <- DT::renderDataTable(render_validation_table("feature_selection"))
    output$tbl_candidates <- DT::renderDataTable(render_validation_table("candidate_validation"))
    output$tbl_edges <- DT::renderDataTable(render_validation_table("edge_validation"))

    output$ontology_plot <- visNetwork::renderVisNetwork({
      obj <- get_object()
      req(obj, input$sub_network)
      graph <- metminer_build_annotation_ontology_graph(obj, input$sub_network)
      validate(need(nrow(graph$nodes) > 0, "No ontology DAG available for the selected sub-network."))

      visNetwork::visNetwork(graph$nodes, graph$edges, width = "100%", height = "430px") |>
        visNetwork::visGroups(groupname = "putative_parent", color = list(background = "#0f766e", border = "#064e3b"), font = list(color = "white")) |>
        visNetwork::visGroups(groupname = "isotope", color = list(background = "#dbeafe", border = "#2563eb")) |>
        visNetwork::visGroups(groupname = "adduct", color = list(background = "#fef3c7", border = "#d97706")) |>
        visNetwork::visGroups(groupname = "isf", color = list(background = "#fee2e2", border = "#dc2626")) |>
        visNetwork::visGroups(groupname = "cross_polarity", color = list(background = "#ede9fe", border = "#7c3aed")) |>
        visNetwork::visGroups(groupname = "neighbor", color = list(background = "#f3f4f6", border = "#6b7280")) |>
        visNetwork::visEdges(smooth = list(type = "cubicBezier", forceDirection = "horizontal"), font = list(size = 14, align = "middle")) |>
        visNetwork::visNodes(font = list(size = 16, face = "Inter"), margin = 10) |>
        visNetwork::visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 230, nodeSpacing = 150) |>
        visNetwork::visInteraction(hover = TRUE, tooltipDelay = 80, navigationButtons = TRUE, dragNodes = TRUE) |>
        visNetwork::visEvents(selectNode = sprintf("function(nodes) { Shiny.setInputValue('%s', nodes.nodes[0], {priority: 'event'}); }", session$ns("ontology_plot_selected"))) |>
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE)
    })

    ms1_data <- reactive({
      obj <- get_object()
      req(obj, input$sub_network)
      net <- extract_feature_network(obj)
      make_ms1_window_data(object = obj, network = net, selected_id = selected_feature() %||% "", sub_network = input$sub_network)
    })

    output$ms1_plot <- plotly::renderPlotly({
      plot_ms1_window(ms1_data())
    })

    output$selected_feature_text <- renderText({
      feature <- selected_feature()
      if (!has_text(feature)) return("Select a node in the ontology DAG.")
      roles <- validation()$feature_role_interpretation
      role <- roles[roles$feature_id == feature, , drop = FALSE]
      if (nrow(role) == 0) return(paste("Selected:", feature))
      paste0("Selected: ", feature, "\nRole: ", role$network_role[1], "\nAnnotation: ", role$selected_compound[1])
    })

    eic_data_raw <- reactive({
      obj <- get_object()
      feature <- selected_feature()
      req(obj, feature)
      make_feature_eic_data(
        wd = prj_init$wd,
        object = obj,
        feature_id = feature,
        mode = input$mode,
        expand_rt = input$eic_rt_window,
        expand_mz = input$eic_mz_window,
        max_traces = input$eic_max_traces
      )
    })

    observe({
      dat <- eic_data_raw()
      if (!isTRUE(dat$available)) {
        updateSelectizeInput(session, "eic_samples", choices = character(), selected = character(), server = TRUE)
        return()
      }
      choices <- metminer_eic_sample_choices(dat, get_object())
      updateSelectizeInput(session, "eic_samples", choices = choices, selected = character(), server = TRUE)
    })

    eic_data <- reactive({
      dat <- eic_data_raw()
      selected <- input$eic_samples
      if (isTRUE(dat$available) && length(selected) > 0) {
        dat$data <- dat$data[dat$data$sample %in% selected, , drop = FALSE]
      }
      dat
    })

    output$eic_plot <- plotly::renderPlotly({
      plot_feature_eic(eic_data())
    })

    output$ms2_plot <- plotly::renderPlotly({
      obj <- get_object()
      feature <- selected_feature()
      req(obj, feature)
      dat <- make_ms2_spectrum_data(
        object = obj,
        feature_id = feature,
        mz_tol = input$ms2_fragment_mz_tol,
        ms2_mz_tol_ppm = input$ms2_mz_tol_ppm,
        ms2_rt_tol = input$ms2_rt_tol,
        top_n = input$ms2_top_n
      )
      plot_ms2_spectrum(dat)
    })
  })
}
