#' LC-MS Parameter Advisor UI Module
#'
#' @param id Module id.
#' @noRd
mod_parameter_advisor_ui <- function(id) {
  ns <- NS(id)

  default_method <- paste(
    "Thermo Fisher Vanquish Flex UHPLC system coupled with a Quadrupole-Orbitrap Exploris 240 mass spectrometer.",
    "UHPLC column Thermo Scientific Hypersil Gold Vanquish (100 x 2.1 mm, 1.9 um).",
    "Mobile phase A was H2O with 0.1% formic acid and phase B was acetonitrile.",
    "LC-ESI-HRMS was acquired in positive and negative full scan data-dependent MS/MS mode.",
    "Scan range m/z 70-1050; resolution 70000 for MS1 and 17500 for MS2; stepped NCE 20, 40, 60.",
    sep = "\n"
  )

  bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      fillable = FALSE,
      padding = 0,
      sidebar = bslib::sidebar(
        title = "LC-MS Parameter Advisor",
        width = 420,
        bg = "#f8f9fa",
        tags$h6(class = "fw-bold text-primary", "1. Method text"),
        textAreaInput(
          ns("method_text"),
          "Paste LC-MS method",
          value = default_method,
          rows = 13,
          resize = "vertical"
        ),
        tags$hr(),
        tags$h6(class = "fw-bold text-primary", "2. Method context"),
        selectInput(
          ns("instrument_type"),
          "Instrument class",
          choices = c("Auto detect" = "auto", "Orbitrap", "QTOF", "Other"),
          selected = "auto"
        ),
        selectInput(
          ns("chromatography"),
          "Chromatography",
          choices = c("Auto detect" = "auto", "C18 reversed phase", "HILIC", "Other"),
          selected = "auto"
        ),
        actionButton(
          ns("run_advisor"),
          "Generate Advice",
          icon = icon("wand-magic-sparkles"),
          class = "btn-teal w-100 fw-bold"
        ),
        tags$hr(),
        downloadButton(ns("download_json"), "Download JSON", class = "btn-outline-primary w-100 mb-2"),
        downloadButton(ns("download_tsv"), "Download TSV", class = "btn-outline-primary w-100")
      ),
      div(
        class = "p-3",
        h3("AI-assisted LC-MS Parameter Advisor", class = "text-primary fw-bold"),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box("Instrument", textOutput(ns("value_instrument"), inline = TRUE), showcase = bsicons::bs_icon("cpu")),
          bslib::value_box("Chromatography", textOutput(ns("value_chrom"), inline = TRUE), showcase = bsicons::bs_icon("columns-gap")),
          bslib::value_box("Core adducts", textOutput(ns("value_core"), inline = TRUE), showcase = bsicons::bs_icon("check-circle")),
          bslib::value_box("Optional adducts", textOutput(ns("value_optional"), inline = TRUE), showcase = bsicons::bs_icon("sliders"))
        ),
        br(),
        bslib::card(
          bslib::card_header("Parameter Advice"),
          DT::dataTableOutput(ns("tbl_advice"))
        ),
        br(),
        bslib::card(
          bslib::card_header("Notes"),
          tags$ul(
            tags$li("This advisor contextualizes annotation parameters from the LC-MS method; it does not confirm metabolite identities."),
            tags$li("Dehydration ions such as [M+H-H2O]+ and [M-H2O-H]- are common but should be treated as secondary evidence."),
            tags$li("After species-specific database filtering, MS1/adduct-only candidates can be retained for prioritization but should not be treated as structural confirmation.")
          )
        )
      )
    )
  )
}

#' LC-MS Parameter Advisor Server Module
#'
#' @param id Module id.
#' @noRd
mod_parameter_advisor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(advice = NULL)

    build_advice <- function() {
      advice <- metminer_lcms_parameter_advice(
        method_text = input$method_text %||% "",
        instrument_type = input$instrument_type %||% "auto",
        chromatography = input$chromatography %||% "auto"
      )
      state$advice <- advice
    }

    observeEvent(TRUE, build_advice(), once = TRUE)
    observeEvent(input$run_advisor, build_advice())

    context_value <- function(field, fallback = "NA") {
      if (is.null(state$advice)) return(fallback)
      ctx <- attr(state$advice, "context")
      ctx[[field]] %||% fallback
    }

    output$value_instrument <- renderText(context_value("instrument"))
    output$value_chrom <- renderText(context_value("chromatography"))
    output$value_core <- renderText({
      if (is.null(state$advice)) return("0")
      sum(state$advice$priority == "core" & state$advice$section == "Adduct search")
    })
    output$value_optional <- renderText({
      if (is.null(state$advice)) return("0")
      sum(state$advice$priority == "optional" & state$advice$section == "Adduct search")
    })

    output$tbl_advice <- DT::renderDataTable({
      req(state$advice)
      DT::datatable(
        state$advice,
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 12)
      )
    })

    output$download_tsv <- downloadHandler(
      filename = function() paste0("metminer_lcms_parameter_advice_", Sys.Date(), ".tsv"),
      content = function(file) {
        req(state$advice)
        utils::write.table(state$advice, file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
      }
    )

    output$download_json <- downloadHandler(
      filename = function() paste0("metminer_lcms_parameter_advice_", Sys.Date(), ".json"),
      content = function(file) {
        req(state$advice)
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          stop("Package 'jsonlite' is required to export JSON.", call. = FALSE)
        }
        jsonlite::write_json(
          list(
            context = attr(state$advice, "context"),
            advice = state$advice
          ),
          file,
          pretty = TRUE,
          na = "null"
        )
      },
      contentType = "application/json"
    )
  })
}
