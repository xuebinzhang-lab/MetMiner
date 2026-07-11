#' ClassyFire classification UI
#'
#' @param id Module id.
#' @noRd
mod_classification_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = "ClassyFire Classification",
          width = 350,
          bg = "#f8f9fa",
          tags$h6(class = "fw-bold text-primary", "Classification level"),
          selectInput(
            ns("dimension"),
            "Pie plot level",
            choices = c("Super class" = "Super_class", "Class" = "Class", "Sub class" = "Sub_class"),
            selected = "Super_class"
          ),
          numericInput(ns("top_n"), "Top slices", value = 12, min = 3, max = 30, step = 1),
          checkboxInput(ns("include_unclassified"), "Include unclassified", value = TRUE),
          shinyWidgets::switchInput(
            inputId = ns("interactive"),
            label = "Interactive pie plot",
            value = TRUE,
            size = "small",
            inline = TRUE,
            onStatus = "success",
            offStatus = "secondary"
          ),
          tags$hr(),
          downloadButton(ns("download_records"), "Download metabolite table", class = "btn-success w-100 mb-2"),
          downloadButton(ns("download_summary"), "Download summary table", class = "btn-outline-success w-100 mb-2"),
          downloadButton(ns("download_plot"), "Download static plot", class = "btn-outline-primary w-100")
        ),
        div(
          class = "p-3",
          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("pie-chart"), " ClassyFire Summary"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            bslib::value_box("Final metabolites", textOutput(ns("value_total"), inline = TRUE), showcase = bsicons::bs_icon("collection")),
            bslib::value_box("Classified", textOutput(ns("value_classified"), inline = TRUE), showcase = bsicons::bs_icon("tags")),
            bslib::value_box("Unclassified", textOutput(ns("value_unclassified"), inline = TRUE), showcase = bsicons::bs_icon("question-circle"))
          ),
          br(),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Pie Plot"),
            uiOutput(ns("plot_ui"))
          ),
          br(),
          bslib::card(
            full_screen = TRUE,
            bslib::navset_tab(
              bslib::nav_panel("Summary", DT::dataTableOutput(ns("tbl_summary"))),
              bslib::nav_panel("Metabolites", DT::dataTableOutput(ns("tbl_records")))
            )
          )
        )
      )
    )
  )
}

#' ClassyFire classification server
#'
#' @param id Module id.
#' @param global_data Shared reactiveValues data store.
#' @noRd
mod_classification_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    records <- reactive({
      metminer_classification_records(
        filter_result = global_data$annotation_filter_result,
        positive_object = global_data$object_pos_annotated,
        negative_object = global_data$object_neg_annotated
      )
    })

    summary_table <- reactive({
      metminer_classification_summary(records())
    })

    plot_data <- reactive({
      metminer_classification_plot_data(
        summary_table(),
        dimension = input$dimension %||% "Super_class",
        top_n = input$top_n %||% 12,
        include_unclassified = isTRUE(input$include_unclassified)
      )
    })

    plot_title <- reactive({
      label <- c(Super_class = "Super Class", Class = "Class", Sub_class = "Sub Class")[[input$dimension %||% "Super_class"]]
      paste0("ClassyFire ", label, " Distribution")
    })

    output$value_total <- renderText(nrow(records()))
    output$value_classified <- renderText({
      x <- records()
      if (nrow(x) == 0) return("0")
      sum(x$Super_class != "Unclassified" | x$Class != "Unclassified" | x$Sub_class != "Unclassified", na.rm = TRUE)
    })
    output$value_unclassified <- renderText({
      x <- records()
      if (nrow(x) == 0) return("0")
      sum(x$Super_class == "Unclassified" & x$Class == "Unclassified" & x$Sub_class == "Unclassified", na.rm = TRUE)
    })

    output$plot_ui <- renderUI({
      if (isTRUE(input$interactive)) {
        plotly::plotlyOutput(ns("pie_interactive"), height = "520px")
      } else {
        plotOutput(ns("pie_static"), height = "520px")
      }
    })

    output$pie_interactive <- plotly::renderPlotly({
      validate(need(nrow(records()) > 0, "Run annotation filtering first."))
      metminer_classification_interactive_pie(plot_data(), title = plot_title())
    })

    output$pie_static <- renderPlot({
      validate(need(nrow(records()) > 0, "Run annotation filtering first."))
      metminer_classification_static_pie(plot_data(), title = plot_title())
    })

    output$tbl_summary <- DT::renderDataTable({
      DT::datatable(summary_table(), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$tbl_records <- DT::renderDataTable({
      DT::datatable(records(), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download_records <- downloadHandler(
      filename = "classyfire_final_metabolite_classification.csv",
      content = function(file) {
        utils::write.csv(records(), file, row.names = FALSE)
      }
    )

    output$download_summary <- downloadHandler(
      filename = "classyfire_classification_summary.csv",
      content = function(file) {
        utils::write.csv(summary_table(), file, row.names = FALSE)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() paste0("classyfire_", tolower(input$dimension %||% "super_class"), "_pie.png"),
      content = function(file) {
        p <- metminer_classification_static_pie(plot_data(), title = plot_title())
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 300)
      },
      contentType = "image/png"
    )
  })
}
