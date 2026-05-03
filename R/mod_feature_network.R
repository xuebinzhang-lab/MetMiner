#' Feature Network UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
mod_feature_network_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        sidebar = bslib::sidebar(
          title = "Feature Network",
          width = 350,
          open = TRUE,
          bg = "#f8f9fa",

          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Relationship Detection",
              icon = bsicons::bs_icon("diagram-3"),
              shinyWidgets::prettyCheckboxGroup(
                inputId = ns("detect_types"),
                label = "Relationship Types:",
                choices = c(
                  "Natural Isotopes" = "isotope",
                  "Adducts" = "adduct",
                  "In-source Fragments" = "isf"
                ),
                selected = c("isotope", "adduct", "isf"),
                icon = icon("check"),
                status = "primary"
              ),
              selectInput(
                ns("ion_mode"),
                "Ion Mode:",
                choices = c("Auto" = "auto", "Positive" = "positive", "Negative" = "negative"),
                selected = "auto"
              ),
              numericInput(ns("ppm"), "Mass Tolerance (ppm)", value = 10, min = 1, step = 1),
              numericInput(ns("rt_tolerance"), "RT Window (sec)", value = 1, min = 0.1, step = 0.1),
              sliderInput(ns("cor_cutoff"), "Abundance Correlation", min = 0, max = 1, value = 0.7, step = 0.05),
              numericInput(ns("max_charge"), "Max Isotope Charge", value = 2, min = 1, max = 4, step = 1),
              numericInput(ns("max_nl_charge"), "Max Neutral Loss Charge", value = 1, min = 1, max = 3, step = 1)
            ),

            bslib::accordion_panel(
              title = "MS2 Evidence",
              icon = bsicons::bs_icon("link-45deg"),
              checkboxInput(ns("use_ms2"), "Use audited MS2 evidence for ISF", value = TRUE),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("use_ms2")),
                fileInput(ns("ms2_zip"), "MS2 Spectra ZIP (.mgf)", accept = ".zip", placeholder = "No file selected"),
                selectInput(ns("ms2_column"), "LC Column", choices = c("RP" = "rp", "HILIC" = "hilic"), selected = "rp"),
                numericInput(ns("ms2_match_mz_tol"), "MS1-MS2 Match m/z (ppm)", value = 15, min = 0.1, step = 0.5),
                numericInput(ns("ms2_match_rt_tol"), "MS1-MS2 Match RT (sec)", value = 30, min = 0.1, step = 1),
                actionButton(ns("attach_ms2_zip"), "Attach MS2 to Normalized Data", icon = icon("link"), class = "btn-outline-primary w-100 mb-2"),
                tags$hr(),
                numericInput(ns("ms2_mz_tol_ppm"), "Strict MS2 Audit m/z (ppm)", value = 5, min = 0.1, step = 0.5),
                numericInput(ns("ms2_rt_tol"), "Strict MS2 Audit RT (sec)", value = 10, min = 0.1, step = 1),
                numericInput(ns("ms2_fragment_mz_tol"), "MS2 Fragment m/z Tolerance", value = 0.02, min = 0.001, step = 0.001)
              )
            ),

            bslib::accordion_panel(
              title = "Build",
              icon = bsicons::bs_icon("play-circle"),
              tags$small(class = "text-muted d-block mb-2",
                         "Build runs on normalized data. If MS2 ZIP is provided and MS2 evidence is enabled, spectra are attached before network detection."),
              actionButton(ns("run_network"), "Build Feature Network", icon = icon("project-diagram"), class = "btn-teal w-100 fw-bold shadow-sm")
            )
          )
        ),

        div(
          class = "p-3",

          bslib::navset_tab(
            bslib::nav_panel(
              "Analysis",
              tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("info-circle"), " Method Summary"),
              div(
                class = "card bg-light border-start border-primary border-3 mb-4",
                div(class = "card-body py-2", verbatimTextOutput(ns("method_summary"), placeholder = TRUE))
              ),

              tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Network Status"),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                bslib::card(
                  bslib::card_header("Positive Mode Status", class = "bg-info-subtle text-info-emphasis"),
                  verbatimTextOutput(ns("status_pos"), placeholder = TRUE),
                  style = "min-height: 170px;"
                ),
                bslib::card(
                  bslib::card_header("Negative Mode Status", class = "bg-warning-subtle text-warning-emphasis"),
                  verbatimTextOutput(ns("status_neg"), placeholder = TRUE),
                  style = "min-height: 170px;"
                ),
                bslib::card(
                  bslib::card_header("Final Merged Status", class = "bg-success-subtle text-success-emphasis"),
                  verbatimTextOutput(ns("status_merged"), placeholder = TRUE),
                  style = "min-height: 170px;"
                )
              ),

              br(),

              tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Empirical Compound Quantification"),
              bslib::card(
                bslib::card_header("Network Tables", class = "bg-light"),
                bslib::navset_card_tab(
                  bslib::nav_panel("Edges", DT::dataTableOutput(ns("tbl_edges"))),
                  bslib::nav_panel("Pseudo Area", DT::dataTableOutput(ns("tbl_pseudo_area"))),
                  bslib::nav_panel("Compound Info", DT::dataTableOutput(ns("tbl_compound_info"))),
                  bslib::nav_panel("Feature Mapping", DT::dataTableOutput(ns("tbl_feature_mapping")))
                )
              )
            ),

            bslib::nav_panel(
              "Visualization",
              tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("diagram-3"), " Feature Relationship Network"),
              div(
                class = "d-flex align-items-center gap-3 mb-3 p-3 bg-white rounded shadow-sm border flex-wrap",
                div(
                  style = "min-width: 260px;",
                  selectInput(
                    ns("network_scope"),
                    "Network View",
                    choices = c("Single ion mode" = "single", "Final merged polarity network" = "merged"),
                    selected = "single"
                  )
                ),
                div(
                  style = "min-width: 220px;",
                  selectInput(ns("view_mode"), "Ion Mode", choices = c("Positive" = "positive", "Negative" = "negative"), selected = "positive")
                ),
                div(
                  class = "align-self-end mb-3",
                  actionButton(ns("render_network"), "Show Network", icon = icon("eye"), class = "btn-primary")
                )
              ),
              bslib::layout_columns(
                col_widths = c(7, 5),
                bslib::card(
                  height = "860px",
                  full_screen = TRUE,
                  bslib::card_header(textOutput(ns("network_title"), inline = TRUE), class = "bg-light"),
                  bslib::layout_sidebar(
                    sidebar = bslib::sidebar(
                      title = "Network Controls",
                      open = FALSE,
                      bg = "#f8f9fa",
                      selectInput(ns("sub_network"), "Sub-network", choices = c("All networks" = "all")),
                      checkboxInput(ns("interactive_plot"), "Interactive network", value = TRUE),
                      sliderInput(ns("min_confidence"), "Min Edge Confidence", min = 0, max = 1, value = 0, step = 0.05),
                      numericInput(ns("max_render_edges"), "Max Rendered Edges", value = 500, min = 50, step = 50),
                      numericInput(ns("max_subnetworks"), "Max Sub-network Choices", value = 50, min = 5, step = 5),
                      tags$hr(),
                      tags$h6(class = "fw-bold text-success", "Cross-polarity Merge"),
                      numericInput(ns("cross_ppm"), "m/z Tolerance (ppm)", value = 10, min = 1, step = 1),
                      numericInput(ns("cross_rt_tolerance"), "RT Window (sec)", value = 5, min = 0.1, step = 0.5),
                      sliderInput(ns("cross_cor_cutoff"), "Correlation", min = 0, max = 1, value = 0.7, step = 0.05),
                      tags$hr(),
                      numericInput(ns("ms2_top_n_annotate"), "MS2 Annotate Top N Peaks", value = 12, min = 3, step = 1)
                    ),
                    uiOutput(ns("network_ui"))
                  )
                ),
                div(
                  bslib::card(
                    height = "300px",
                    full_screen = TRUE,
                    bslib::card_header("Sub-network MS1 Spectrum", class = "bg-light"),
                    tags$div(class = "px-2 pb-2 text-muted small", textOutput(ns("selected_node_text"))),
                    plotly::plotlyOutput(ns("ms1_window_plot"), height = "235px")
                  ),
                  bslib::card(
                    height = "300px",
                    full_screen = TRUE,
                    bslib::card_header("Assigned MS2 Spectrum", class = "bg-light"),
                    tags$div(class = "px-2 pb-2 text-muted small", textOutput(ns("selected_ms2_text"))),
                    plotly::plotlyOutput(ns("ms2_spectrum_plot"), height = "235px")
                  ),
                  bslib::card(
                    height = "300px",
                    full_screen = TRUE,
                    bslib::card_header("Raw XCMS Chromatogram", class = "bg-light"),
                    tags$div(class = "px-2 pb-2 text-muted small", textOutput(ns("selected_eic_text"))),
                    plotly::plotlyOutput(ns("feature_eic_plot"), height = "235px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Feature Network Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues. Expects `object_pos_norm` and
#'   `object_neg_norm`.
#' @param prj_init Project init reactive object.
#' @noRd
mod_feature_network_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    progress_handlers <- create_progress_handlers(ns)
    show_progress_modal   <- progress_handlers$show_progress_modal
    update_progress_modal <- progress_handlers$update_progress_modal
    close_progress_modal  <- progress_handlers$close_progress_modal

    get_input_obj <- function(mode) {
      if (identical(mode, "positive")) {
        if (!is.null(global_data$object_pos_network)) return(global_data$object_pos_network)
        if (!is.null(global_data$object_pos_norm)) return(global_data$object_pos_norm)
        if (!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      }

      if (!is.null(global_data$object_neg_network)) return(global_data$object_neg_network)
      if (!is.null(global_data$object_neg_norm)) return(global_data$object_neg_norm)
      if (!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
      NULL
    }

    get_pseudo <- function(mode) {
      if (identical(mode, "positive")) {
        return(global_data$pseudo_area_pos)
      }
      global_data$pseudo_area_neg
    }

    find_polarity_root <- function(target_dir) {
      all_dirs <- list.dirs(target_dir, recursive = TRUE, full.names = TRUE)
      all_dirs <- all_dirs[!grepl("__MACOSX", all_dirs)]
      has_data <- sapply(all_dirs, function(x) {
        sub_dirs <- basename(list.dirs(x, recursive = FALSE, full.names = TRUE))
        "POS" %in% sub_dirs || "NEG" %in% sub_dirs
      })
      valid_roots <- all_dirs[has_data]
      if (length(valid_roots) == 0) {
        return(NULL)
      }
      valid_roots[order(nchar(valid_roots))][1]
    }

    check_ms2_structure <- function(path) {
      if (is.null(path) || !dir.exists(path)) {
        stop("MS2 folder does not exist.", call. = FALSE)
      }
      pos_dir <- file.path(path, "POS")
      neg_dir <- file.path(path, "NEG")
      pos_files <- if (dir.exists(pos_dir)) {
        list.files(pos_dir, pattern = "\\.mgf$", recursive = TRUE,
                   full.names = TRUE, ignore.case = TRUE)
      } else {
        character()
      }
      neg_files <- if (dir.exists(neg_dir)) {
        list.files(neg_dir, pattern = "\\.mgf$", recursive = TRUE,
                   full.names = TRUE, ignore.case = TRUE)
      } else {
        character()
      }
      if (length(pos_files) == 0 && length(neg_files) == 0) {
        stop("No .mgf files found under POS or NEG folders.", call. = FALSE)
      }
      list(pos = length(pos_files) > 0, neg = length(neg_files) > 0,
           pos_count = length(pos_files), neg_count = length(neg_files))
    }

    process_ms2_zip <- function() {
      req(prj_init$wd, input$ms2_zip)
      target_dir <- file.path(prj_init$wd, "MS2")
      if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
      dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(input$ms2_zip$datapath, exdir = target_dir)
      ms2_root <- find_polarity_root(target_dir)
      if (is.null(ms2_root)) {
        stop("No POS or NEG folders found in MS2 ZIP.", call. = FALSE)
      }
      check_ms2_structure(ms2_root)
      ms2_root
    }

    attach_ms2_to_normalized_objects <- function(ms2_root) {
      struct <- check_ms2_structure(ms2_root)
      attached <- character()

      if (struct$pos && !is.null(global_data$object_pos_norm)) {
        update_progress_modal(35, "Matching POS MS2 spectra to normalized data...")
        global_data$object_pos_norm <- massdataset::mutate_ms2(
          object = global_data$object_pos_norm,
          column = input$ms2_column,
          polarity = "positive",
          ms1.ms2.match.mz.tol = input$ms2_match_mz_tol,
          ms1.ms2.match.rt.tol = input$ms2_match_rt_tol,
          path = file.path(ms2_root, "POS")
        )
        object_pos_norm <- global_data$object_pos_norm
        if (!is.null(prj_init$mass_dataset_dir)) {
          save(object_pos_norm, file = file.path(prj_init$mass_dataset_dir, "05.object_pos_norm.rda"))
        }
        attached <- c(attached, paste0("POS=", struct$pos_count, " mgf"))
      }

      if (struct$neg && !is.null(global_data$object_neg_norm)) {
        update_progress_modal(70, "Matching NEG MS2 spectra to normalized data...")
        global_data$object_neg_norm <- massdataset::mutate_ms2(
          object = global_data$object_neg_norm,
          column = input$ms2_column,
          polarity = "negative",
          ms1.ms2.match.mz.tol = input$ms2_match_mz_tol,
          ms1.ms2.match.rt.tol = input$ms2_match_rt_tol,
          path = file.path(ms2_root, "NEG")
        )
        object_neg_norm <- global_data$object_neg_norm
        if (!is.null(prj_init$mass_dataset_dir)) {
          save(object_neg_norm, file = file.path(prj_init$mass_dataset_dir, "05.object_neg_norm.rda"))
        }
        attached <- c(attached, paste0("NEG=", struct$neg_count, " mgf"))
      }

      if (length(attached) == 0) {
        stop("MS2 spectra were found, but no normalized positive or negative objects are available.", call. = FALSE)
      }
      attached
    }

    attach_uploaded_ms2 <- function() {
      ms2_root <- process_ms2_zip()
      attach_ms2_to_normalized_objects(ms2_root)
    }

    build_merged_network <- function() {
      if (!is.null(global_data$object_pos_network) && !is.null(global_data$object_neg_network)) {
        merged <- merge_polarity_feature_networks(
          positive_object = global_data$object_pos_network,
          negative_object = global_data$object_neg_network,
          ppm = input$cross_ppm,
          rt_tolerance = input$cross_rt_tolerance,
          cor_cutoff = input$cross_cor_cutoff
        )
        global_data$merged_feature_network <- merged
        return(merged)
      }
      empty_feature_network()
    }

    get_merged_network <- function() {
      if (!is.null(global_data$merged_feature_network)) {
        return(global_data$merged_feature_network)
      }
      empty_feature_network()
    }

    selected_object <- reactive({
      if (identical(input$network_scope, "merged")) {
        return(NULL)
      }
      get_input_obj(input$view_mode)
    })

    selected_network <- reactive({
      if (identical(input$network_scope, "merged")) {
        net <- get_merged_network()
        return(net[net$confidence >= input$min_confidence, , drop = FALSE])
      }
      obj <- selected_object()
      if (is.null(obj)) return(empty_feature_network())
      net <- extract_feature_network(obj)
      net[net$confidence >= input$min_confidence, , drop = FALSE]
    })

    output$method_summary <- renderText({
      paste0(
        "Detect: ", paste(input$detect_types, collapse = ", "),
        "\nIon mode: ", input$ion_mode,
        "\nMass tolerance: ", input$ppm, " ppm",
        "\nRT window: ", input$rt_tolerance, " sec",
        "\nCorrelation cutoff: ", input$cor_cutoff,
        "\nNeutral loss charge: z <= ", input$max_nl_charge,
        "\nMS2 audit: ", if (isTRUE(input$use_ms2)) {
          paste0(input$ms2_mz_tol_ppm, " ppm, ", input$ms2_rt_tol, " sec")
        } else {
          "disabled"
        },
        "\nCross-polarity merge: ", input$cross_ppm, " ppm, ",
        input$cross_rt_tolerance, " sec, cor >= ", input$cross_cor_cutoff
      )
    })

    observeEvent(input$attach_ms2_zip, {
      req(input$ms2_zip, prj_init$wd)
      if (!isTRUE(input$use_ms2)) {
        shinyalert::shinyalert(
          "MS2 Evidence Disabled",
          "Enable audited MS2 evidence before attaching MS2 spectra.",
          type = "warning"
        )
        return()
      }
      if (is.null(global_data$object_pos_norm) && is.null(global_data$object_neg_norm)) {
        shinyalert::shinyalert(
          "Normalized Data Missing",
          "Please run Normalization before attaching MS2 spectra.",
          type = "error"
        )
        return()
      }

      show_progress_modal("MS2 Spectra", "Extracting MS2 ZIP...", 10)
      tryCatch({
        attached <- attach_uploaded_ms2()
        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyalert::shinyalert(
          "MS2 Attached",
          paste("MS2 spectra matched to normalized data:", paste(attached, collapse = ", ")),
          type = "success"
        )
      }, error = function(e) {
        close_progress_modal()
        shinyalert::shinyalert("MS2 Attachment Failed", paste("Error:", e$message), type = "error")
      })
    })

    observe({
      choices <- c()
      if (!is.null(global_data$object_pos_norm) || !is.null(global_data$object_pos_network)) {
        choices <- c(choices, "Positive" = "positive")
      }
      if (!is.null(global_data$object_neg_norm) || !is.null(global_data$object_neg_network)) {
        choices <- c(choices, "Negative" = "negative")
      }
      if (length(choices) > 0) {
        updateSelectInput(session, "view_mode", choices = choices, selected = choices[[1]])
      }
    })

    observeEvent(list(input$network_scope, input$view_mode, input$sub_network,
                      input$interactive_plot, input$min_confidence,
                      input$max_render_edges), {
      rendered_network(NULL)
      render_state("idle")
    }, ignoreInit = TRUE)

    observe({
      net <- selected_network()
      choices <- c("All networks" = "all")
      if (nrow(net) > 0 && requireNamespace("igraph", quietly = TRUE)) {
        comp <- get_network_component_membership(net)
        involved_ids <- unique(c(net$from, net$to))
        comp_sizes <- sort(table(comp[involved_ids]), decreasing = TRUE)
        comp_sizes <- utils::head(comp_sizes, input$max_subnetworks)
        comp_choices <- stats::setNames(
          names(comp_sizes),
          paste0("Sub-network ", names(comp_sizes), " (", as.integer(comp_sizes), " features)")
        )
        choices <- c(choices, comp_choices)
      }
      updateSelectInput(session, "sub_network", choices = choices, selected = "all")
    })

    build_one_mode <- function(object, mode, progress_value, progress_label) {
      if (is.null(object)) {
        return(NULL)
      }

      update_progress_modal(progress_value, paste("Detecting relationships:", progress_label))
      ion_mode <- input$ion_mode
      if (identical(ion_mode, "auto")) {
        ion_mode <- mode
      }

      obj_net <- detect_feature_relationships(
        object = object,
        mode = ion_mode,
        detect = input$detect_types,
        ppm = input$ppm,
        rt_tolerance = input$rt_tolerance,
        cor_cutoff = input$cor_cutoff,
        max_charge = input$max_charge,
        max_neutral_loss_charge = input$max_nl_charge,
        use_ms2 = isTRUE(input$use_ms2),
        ms2_mz_tol_ppm = input$ms2_mz_tol_ppm,
        ms2_rt_tol = input$ms2_rt_tol,
        ms2_fragment_mz_tol = input$ms2_fragment_mz_tol,
        store = TRUE
      )

      update_progress_modal(progress_value + 12, paste("Collapsing subnetworks:", progress_label))
      pseudo <- collapse_to_pseudo_area(obj_net)

      list(object = obj_net, pseudo = pseudo)
    }

    observeEvent(input$run_network, {
      pos_in <- global_data$object_pos_norm
      neg_in <- global_data$object_neg_norm

      if (is.null(pos_in) && is.null(neg_in)) {
        shinyalert::shinyalert(
          "Data Missing",
          "No normalized mass_dataset objects found. Please run Normalization first.",
          type = "error"
        )
        return()
      }

      shinyjs::disable("run_network")
      show_progress_modal("Building Feature Network", "Preparing feature data...", 0)

      tryCatch({
        if (isTRUE(input$use_ms2) && !is.null(input$ms2_zip)) {
          update_progress_modal(8, "Attaching uploaded MS2 spectra to normalized data...")
          attached <- attach_uploaded_ms2()
          update_progress_modal(15, paste("MS2 attached:", paste(attached, collapse = ", ")))
          pos_in <- global_data$object_pos_norm
          neg_in <- global_data$object_neg_norm
        }

        polarities <- list(
          list(name = "positive", obj = pos_in,
               progress_value = 20, progress_label = "Positive Mode",
               global_key = "object_pos_network", pseudo_key = "pseudo_area_pos",
               save_var_net = "object_pos_network",
               save_file_net = "06.object_pos_feature_network.rda",
               save_var_pseudo = "pseudo_area_pos",
               save_file_pseudo = "06.pseudo_area_pos.rda"),
          list(name = "negative", obj = neg_in,
               progress_value = 55, progress_label = "Negative Mode",
               global_key = "object_neg_network", pseudo_key = "pseudo_area_neg",
               save_var_net = "object_neg_network",
               save_file_net = "06.object_neg_feature_network.rda",
               save_var_pseudo = "pseudo_area_neg",
               save_file_pseudo = "06.pseudo_area_neg.rda")
        )

        for (p in polarities) {
          if (!is.null(p$obj)) {
            res <- build_one_mode(p$obj, p$name, p$progress_value, p$progress_label)
            global_data[[p$global_key]] <- res$object
            global_data[[p$pseudo_key]] <- res$pseudo

            if (!is.null(prj_init$mass_dataset_dir)) {
              assign(p$save_var_net, res$object)
              save(list = p$save_var_net,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file_net))
              assign(p$save_var_pseudo, res$pseudo)
              save(list = p$save_var_pseudo,
                   file = file.path(prj_init$mass_dataset_dir, p$save_file_pseudo))
            }
          }
        }

        if (!is.null(global_data$object_pos_network) && !is.null(global_data$object_neg_network)) {
          update_progress_modal(88, "Merging positive and negative mode networks...")
          merged_feature_network <- build_merged_network()
          global_data$merged_feature_network <- merged_feature_network

          if (!is.null(prj_init$mass_dataset_dir)) {
            save(merged_feature_network,
                 file = file.path(prj_init$mass_dataset_dir, "06.merged_feature_network.rda"))
          }
        }

        rendered_network(NULL)
        render_state("idle")
        update_progress_modal(98, "Saving results...")
        update_progress_modal(100, "Done. Use Show Network to render the graph.")
        close_progress_modal()
        shinyjs::enable("run_network")
        shinyalert::shinyalert("Feature Network Completed", "Feature relationships and empirical compounds are ready. Click Show Network when you want to render the graph.", type = "success")
      }, error = function(e) {
        close_progress_modal()
        shinyjs::enable("run_network")
        shinyalert::shinyalert("Feature Network Failed", paste("Error:", e$message), type = "error")
      })
    })

    summarize_network <- function(mode) {
      obj <- get_input_obj(mode)
      pseudo <- get_pseudo(mode)
      if (is.null(obj)) {
        return("No normalized object available.")
      }

      net <- extract_feature_network(obj)
      type_counts <- if (nrow(net) > 0) {
        paste(names(table(net$type)), as.integer(table(net$type)), sep = "=", collapse = ", ")
      } else {
        "none"
      }

      compound_count <- if (!is.null(pseudo)) nrow(pseudo$expression_data) else 0
      paste0(
        "Features: ", nrow(massdataset::extract_variable_info(obj)),
        "\nEdges: ", nrow(net),
        "\nEdge types: ", type_counts,
        "\nEmpirical compounds: ", compound_count
      )
    }

    output$status_pos <- renderText(summarize_network("positive"))
    output$status_neg <- renderText(summarize_network("negative"))

    output$status_merged <- renderText({
      net <- get_merged_network()
      if (nrow(net) == 0) {
        return("No merged network available.\nBuild both positive and negative networks first.")
      }

      type_counts <- paste(names(table(net$type)), as.integer(table(net$type)), sep = "=", collapse = ", ")
      cross_edges <- sum(net$type %in% c("Cross-polarity", "Cross-polarity ISF"))
      paste0(
        "Edges: ", nrow(net),
        "\nCross-polarity edges: ", cross_edges,
        "\nEdge types: ", type_counts
      )
    })

    rendered_network <- reactiveVal(NULL)
    render_state <- reactiveVal("idle")

    build_network_display_data <- function() {
      net <- selected_network()
      if (nrow(net) == 0) {
        return(NULL)
      }
      if (identical(input$network_scope, "merged")) {
        return(make_merged_network_display_data(
          positive_object = global_data$object_pos_network,
          negative_object = global_data$object_neg_network,
          network = net,
          sub_network = input$sub_network,
          max_edges = input$max_render_edges
        ))
      }

      obj <- selected_object()
      if (is.null(obj)) {
        return(NULL)
      }
      make_network_display_data(object = obj, network = net,
                                sub_network = input$sub_network,
                                max_edges = input$max_render_edges)
    }

    observeEvent(input$render_network, {
      render_state("rendering")
      rendered_network(NULL)

      session$onFlushed(function() {
        tryCatch({
          display <- isolate(build_network_display_data())
          rendered_network(display)
          render_state(if (is.null(display)) "empty" else "ready")
        }, error = function(e) {
          rendered_network(NULL)
          render_state("error")
          shinyalert::shinyalert("Network Rendering Failed", paste("Error:", e$message), type = "error")
        })
      }, once = TRUE)
    })

    output$network_title <- renderText({
      mode_label <- if (identical(input$network_scope, "merged")) {
        "Final Merged Polarity Network"
      } else if (identical(input$view_mode, "positive")) {
        "Positive Mode"
      } else {
        "Negative Mode"
      }
      sub_label <- if (identical(input$sub_network, "all")) "All networks" else paste("Sub-network", input$sub_network)
      display <- rendered_network()
      render_label <- if (!is.null(display) && isTRUE(display$truncated)) {
        paste0(" - showing top ", nrow(display$edges), " edges by confidence")
      } else {
        ""
      }
      paste0(mode_label, " - ", sub_label, render_label)
    })

    output$network_ui <- renderUI({
      state <- render_state()
      if (identical(state, "rendering")) {
        return(div(
          class = "d-flex flex-column align-items-center justify-content-center h-100 text-muted",
          tags$div(class = "spinner-border text-primary mb-3", role = "status"),
          tags$strong("Rendering network, please wait..."),
          tags$small("Large interactive graphs can take a while to prepare and stabilize.")
        ))
      }
      if (identical(state, "empty")) {
        return(div(
          class = "d-flex flex-column align-items-center justify-content-center h-100 text-muted",
          bsicons::bs_icon("exclamation-circle"),
          tags$strong(class = "mt-3", "No network edges available"),
          tags$small("Try another ion mode, sub-network, or lower the confidence cutoff.")
        ))
      }
      if (identical(state, "error")) {
        return(div(
          class = "d-flex flex-column align-items-center justify-content-center h-100 text-danger",
          bsicons::bs_icon("x-circle"),
          tags$strong(class = "mt-3", "Network rendering failed"),
          tags$small("Check the error dialog and adjust the selected network view.")
        ))
      }
      if (is.null(rendered_network())) {
        return(div(
          class = "d-flex flex-column align-items-center justify-content-center h-100 text-muted",
          bsicons::bs_icon("diagram-3"),
          tags$strong(class = "mt-3", "Network graph is not rendered yet"),
          tags$small("Click Show Network above to render the selected view.")
        ))
      }
      if (isTRUE(input$interactive_plot)) {
        visNetwork::visNetworkOutput(ns("network_vis"), height = "790px")
      } else {
        plotOutput(ns("network_static"), height = "790px")
      }
    })

    output$network_vis <- visNetwork::renderVisNetwork({
      display <- rendered_network()
      validate(need(!is.null(display), "No network edges available. Run detection first or lower the confidence cutoff."))
      visNetwork::visNetwork(display$nodes, display$edges, height = "790px") |>
        visNetwork::visNodes(shape = "dot", size = 18, font = list(size = 18)) |>
        visNetwork::visGroups(groupname = "Parent ion", color = "#008080") |>
        visNetwork::visGroups(groupname = "ISF", color = "#d95f02") |>
        visNetwork::visGroups(groupname = "Natural isotope", color = "#1b9e77") |>
        visNetwork::visGroups(groupname = "Adduct", color = "#7570b3") |>
        visNetwork::visGroups(groupname = "Positive feature", color = "#2c7fb8") |>
        visNetwork::visGroups(groupname = "Negative feature", color = "#7a5195") |>
        visNetwork::visGroups(groupname = "Cross-polarity feature", color = "#4d4d4d") |>
        visNetwork::visGroups(groupname = "Feature", color = "#7f8c8d") |>
        visNetwork::visEdges(arrows = "to", smooth = FALSE, font = list(align = "middle", size = 12)) |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, hover = TRUE, tooltipDelay = 80) |>
        visNetwork::visPhysics(enabled = TRUE, stabilization = TRUE) |>
        visNetwork::visLegend(useGroups = TRUE, position = "right") |>
        visNetwork::visEvents(
          selectNode = sprintf(
            "function(nodes) { Shiny.setInputValue('%s', nodes.nodes[0] || null, {priority: 'event'}); }",
            ns("node_selected")
          ),
          deselectNode = sprintf(
            "function(nodes) { Shiny.setInputValue('%s', null, {priority: 'event'}); }",
            ns("node_selected")
          )
        )
    })

    output$network_static <- renderPlot({
      display <- rendered_network()
      validate(need(!is.null(display), "No network edges available. Run detection first or lower the confidence cutoff."))
      g <- igraph::graph_from_data_frame(display$edges[, c("from", "to")], directed = TRUE, vertices = display$nodes)
      node_colors <- c(
        "Parent ion" = "#008080",
        "ISF" = "#d95f02",
        "Natural isotope" = "#1b9e77",
        "Adduct" = "#7570b3",
        "Positive feature" = "#2c7fb8",
        "Negative feature" = "#7a5195",
        "Cross-polarity feature" = "#4d4d4d",
        "Feature" = "#7f8c8d"
      )
      plot(
        g,
        vertex.color = node_colors[display$nodes$group],
        vertex.label = display$nodes$id,
        vertex.label.cex = 0.8,
        vertex.size = 18,
        edge.arrow.size = 0.4,
        edge.label = display$edges$label,
        edge.label.cex = 0.7,
        layout = igraph::layout_with_fr(g)
      )
    })

    output$tbl_edges <- DT::renderDataTable({
      net <- selected_network()
      DT::datatable(net, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_pseudo_area <- DT::renderDataTable({
      validate(need(!identical(input$network_scope, "merged"),
                    "Pseudo-area tables are polarity-specific. Switch to a single ion mode view."))
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      tbl <- pseudo$expression_data
      tbl <- cbind(compound_id = rownames(tbl), tbl)
      DT::datatable(tbl, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_compound_info <- DT::renderDataTable({
      validate(need(!identical(input$network_scope, "merged"),
                    "Compound info tables are polarity-specific. Switch to a single ion mode view."))
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      DT::datatable(pseudo$compound_info, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_feature_mapping <- DT::renderDataTable({
      validate(need(!identical(input$network_scope, "merged"),
                    "Feature mapping tables are polarity-specific. Switch to a single ion mode view."))
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      DT::datatable(pseudo$feature_mapping, options = list(scrollX = TRUE, pageLength = 10))
    })

    selected_node_id <- reactive({
      node_id <- input$node_selected
      if (is.null(node_id) || !nzchar(node_id)) {
        return(NULL)
      }
      node_id
    })

    selected_ms2_feature <- reactiveVal(NULL)

    observeEvent(input$node_selected, {
      node_id <- input$node_selected
      selected_ms2_feature(if (is.null(node_id) || !nzchar(node_id)) NULL else node_id)
    }, ignoreInit = TRUE)

    observeEvent(plotly::event_data("plotly_click", source = "subnetwork_ms1"), {
      click <- plotly::event_data("plotly_click", source = "subnetwork_ms1")
      if (!is.null(click$key) && nzchar(click$key)) {
        selected_ms2_feature(as.character(click$key))
      }
    }, ignoreInit = TRUE)

    ms1_window_data <- reactive({
      node_id <- selected_node_id()
      req(node_id)
      net <- selected_network()
      validate(need(nrow(net) > 0, "No network edges available."))

      if (identical(input$network_scope, "merged")) {
        return(make_ms1_window_data(
          positive_object = global_data$object_pos_network,
          negative_object = global_data$object_neg_network,
          network = net,
          selected_id = node_id,
          sub_network = input$sub_network
        ))
      }

      obj <- selected_object()
      validate(need(!is.null(obj), "No selected ion-mode object available."))
      make_ms1_window_data(
        object = obj,
        network = net,
        selected_id = node_id,
        sub_network = input$sub_network
      )
    })

    output$selected_node_text <- renderText({
      node_id <- selected_node_id()
      if (is.null(node_id)) {
        return("Click a network node to inspect features within its retention-time window.")
      }
      dat <- ms1_window_data()
      sel <- dat$features[dat$features$variable_id == node_id, , drop = FALSE]
      if (nrow(sel) == 0) {
        return(paste("Selected node:", node_id))
      }
      paste0(
        "Selected: ", node_id,
        " | m/z ", sprintf("%.5f", sel$mz[1]),
        " | RT ", sprintf("%.2f", sel$rt[1]), " sec",
        " | sub-network features: ", nrow(dat$features)
      )
    })

    output$ms1_window_plot <- plotly::renderPlotly({
      node_id <- selected_node_id()
      validate(need(!is.null(node_id), "Click a node in the interactive network."))

      dat <- ms1_window_data()
      plot_ms1_window(dat)
    })

    selected_ms1_peak_id <- reactive({
      peak_id <- selected_ms2_feature()
      if (is.null(peak_id) || !nzchar(peak_id)) selected_node_id() else peak_id
    })

    ms2_spectrum_data <- reactive({
      feature_id <- selected_ms1_peak_id()
      req(feature_id)

      if (identical(input$network_scope, "merged")) {
        return(make_ms2_spectrum_data(
          positive_object = global_data$object_pos_network,
          negative_object = global_data$object_neg_network,
          feature_id = feature_id,
          mz_tol = input$ms2_fragment_mz_tol,
        ms2_mz_tol_ppm = input$ms2_mz_tol_ppm,
        ms2_rt_tol = input$ms2_rt_tol,
        top_n = input$ms2_top_n_annotate
      ))
      }

      obj <- selected_object()
      validate(need(!is.null(obj), "No selected ion-mode object available."))
      make_ms2_spectrum_data(
        object = obj,
        feature_id = feature_id,
        mz_tol = input$ms2_fragment_mz_tol,
        ms2_mz_tol_ppm = input$ms2_mz_tol_ppm,
        ms2_rt_tol = input$ms2_rt_tol,
        top_n = input$ms2_top_n_annotate
      )
    })

    output$selected_ms2_text <- renderText({
      feature_id <- selected_ms1_peak_id()
      if (is.null(feature_id)) {
        return("Click an MS1 peak to inspect its assigned MS2 spectrum.")
      }
      dat <- ms2_spectrum_data()
      if (nrow(dat$peaks) == 0) {
        return(paste("MS2:", feature_id, "- no assigned MS2 spectrum available."))
      }
      paste0(
        "MS2: ", feature_id,
        " | precursor m/z ", sprintf("%.5f", dat$precursor_mz),
        " | top peaks checked: ", sum(dat$peaks$annotate),
        " | diagnostic fragments: ", sum(nzchar(dat$peaks$fragment_annotation)),
        " | NL pairs: ", nrow(dat$nl_arrows)
      )
    })

    output$ms2_spectrum_plot <- plotly::renderPlotly({
      feature_id <- selected_ms1_peak_id()
      validate(need(!is.null(feature_id), "Click an MS1 peak above."))
      plot_ms2_spectrum(ms2_spectrum_data())
    })

    feature_eic_data <- reactive({
      feature_id <- selected_ms1_peak_id()
      req(feature_id)

      if (identical(input$network_scope, "merged")) {
        return(make_feature_eic_data(
          wd = prj_init$wd,
          positive_object = global_data$object_pos_network,
          negative_object = global_data$object_neg_network,
          feature_id = feature_id
        ))
      }

      obj <- selected_object()
      validate(need(!is.null(obj), "No selected ion-mode object available."))
      make_feature_eic_data(
        wd = prj_init$wd,
        object = obj,
        feature_id = feature_id,
        mode = input$view_mode
      )
    })

    output$selected_eic_text <- renderText({
      feature_id <- selected_ms1_peak_id()
      if (is.null(feature_id)) {
        return("Click an MS1 peak to inspect its raw chromatogram when XCMS xdata is available.")
      }
      dat <- feature_eic_data()
      if (!isTRUE(dat$available)) {
        return(paste("Chromatogram:", feature_id, "-", dat$message))
      }
      paste0(
        "Chromatogram: ", feature_id,
        " | traces: ", length(unique(dat$data$sample)),
        " | xdata: ", basename(dat$xdata_path)
      )
    })

    output$feature_eic_plot <- plotly::renderPlotly({
      feature_id <- selected_ms1_peak_id()
      validate(need(!is.null(feature_id), "Click an MS1 peak above."))
      plot_feature_eic(feature_eic_data())
    })
  })
}

make_network_display_data <- function(object, network, sub_network = "all", max_edges = Inf) {
  if (nrow(network) == 0) {
    return(NULL)
  }

  membership <- get_network_component_membership(network)

  if (!identical(sub_network, "all")) {
    selected <- names(membership)[membership == as.integer(sub_network)]
    network <- network[network$from %in% selected & network$to %in% selected, , drop = FALSE]
  }

  if (nrow(network) == 0) {
    return(NULL)
  }

  truncated <- FALSE
  if (identical(sub_network, "all") && is.finite(max_edges) && nrow(network) > max_edges) {
    network <- network[order(network$confidence, decreasing = TRUE), , drop = FALSE]
    network <- utils::head(network, max_edges)
    truncated <- TRUE
  }

  variable_info <- massdataset::extract_variable_info(object)
  feature_ids <- unique(c(network$from, network$to))
  variable_info <- variable_info[match(feature_ids, variable_info$variable_id), , drop = FALSE]

  node_type <- rep("Feature", length(feature_ids))
  names(node_type) <- feature_ids
  node_type[unique(network$to[network$type == "Adduct"])] <- "Adduct"
  node_type[unique(network$to[network$type == "Isotope"])] <- "Natural isotope"
  node_type[unique(network$to[network$type == "ISF"])] <- "ISF"
  node_type[unique(network$from[network$type == "ISF"])] <- "Parent ion"

  nodes <- data.frame(
    id = feature_ids,
    label = feature_ids,
    group = unname(node_type[feature_ids]),
    title = sprintf(
      "ID: %s<br>Type: %s<br>m/z: %.5f<br>RT: %.2f",
      feature_ids,
      unname(node_type[feature_ids]),
      variable_info$mz,
      variable_info$rt
    ),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = network$from,
    to = network$to,
    label = network$annotation,
    title = sprintf(
      "Type: %s<br>Annotation: %s<br>Evidence: %s<br>Confidence: %.3f<br>Correlation: %.3f<br>m/z error: %.3f ppm<br>RT diff: %.2f sec",
      network$type,
      network$annotation,
      ifelse(is.na(network$evidence_level) | !nzchar(network$evidence_level),
             "not scored", network$evidence_level),
      network$confidence,
      network$abundance_cor,
      network$mz_error_ppm,
      network$rt_diff
    ),
    arrows = "to",
    stringsAsFactors = FALSE
  )

  list(nodes = nodes, edges = edges, truncated = truncated)
}

make_merged_network_display_data <- function(positive_object,
                                             negative_object,
                                             network,
                                             sub_network = "all",
                                             max_edges = Inf) {
  if (nrow(network) == 0) {
    return(NULL)
  }

  membership <- get_network_component_membership(network)
  if (!identical(sub_network, "all")) {
    selected <- names(membership)[membership == as.integer(sub_network)]
    network <- network[network$from %in% selected & network$to %in% selected, , drop = FALSE]
  }
  if (nrow(network) == 0) {
    return(NULL)
  }

  truncated <- FALSE
  if (identical(sub_network, "all") && is.finite(max_edges) && nrow(network) > max_edges) {
    network <- network[order(network$confidence, decreasing = TRUE), , drop = FALSE]
    network <- utils::head(network, max_edges)
    truncated <- TRUE
  }

  variable_info <- merged_variable_info(positive_object, negative_object)
  feature_ids <- unique(c(network$from, network$to))
  variable_info <- variable_info[match(feature_ids, variable_info$variable_id), , drop = FALSE]
  if (nrow(variable_info) != length(feature_ids) || any(is.na(variable_info$variable_id))) {
    variable_info <- data.frame(
      variable_id = feature_ids,
      mz = NA_real_,
      rt = NA_real_,
      polarity = ifelse(grepl("^pos::", feature_ids), "positive",
                        ifelse(grepl("^neg::", feature_ids), "negative", NA_character_)),
      stringsAsFactors = FALSE
    )
  } else {
    variable_info$variable_id <- feature_ids
    if (!"polarity" %in% colnames(variable_info)) {
      variable_info$polarity <- ifelse(grepl("^pos::", feature_ids), "positive",
                                       ifelse(grepl("^neg::", feature_ids), "negative", NA_character_))
    }
  }

  node_type <- ifelse(grepl("^pos::", feature_ids), "Positive feature",
                      ifelse(grepl("^neg::", feature_ids), "Negative feature", "Cross-polarity feature"))
  names(node_type) <- feature_ids
  node_type[unique(network$to[network$type %in% c("ISF", "Cross-polarity ISF")])] <- "ISF"
  node_type[unique(network$from[network$type %in% c("ISF", "Cross-polarity ISF")])] <- "Parent ion"
  node_type[unique(network$to[network$type == "Adduct"])] <- "Adduct"
  node_type[unique(network$to[network$type == "Isotope"])] <- "Natural isotope"

  nodes <- data.frame(
    id = feature_ids,
    label = feature_ids,
    group = unname(node_type[feature_ids]),
    title = sprintf(
      "ID: %s<br>Type: %s<br>m/z: %.5f<br>RT: %.2f<br>Mode: %s",
      feature_ids,
      unname(node_type[feature_ids]),
      variable_info$mz,
      variable_info$rt,
      variable_info$polarity
    ),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = network$from,
    to = network$to,
    label = network$annotation,
    title = sprintf(
      "Type: %s<br>Annotation: %s<br>Evidence: %s<br>Confidence: %.3f<br>Correlation: %.3f<br>m/z error: %.3f ppm<br>RT diff: %.2f sec",
      network$type,
      network$annotation,
      ifelse(is.na(network$evidence_level) | !nzchar(network$evidence_level),
             "not scored", network$evidence_level),
      network$confidence,
      network$abundance_cor,
      network$mz_error_ppm,
      network$rt_diff
    ),
    arrows = "to",
    stringsAsFactors = FALSE
  )

  list(nodes = nodes, edges = edges, truncated = truncated)
}

merged_variable_info <- function(positive_object, negative_object) {
  out <- list()
  if (!is.null(positive_object)) {
    pos_info <- massdataset::extract_variable_info(positive_object)
    pos_info$variable_id <- paste("pos", pos_info$variable_id, sep = "::")
    pos_info$polarity <- "positive"
    out$pos <- pos_info
  }
  if (!is.null(negative_object)) {
    neg_info <- massdataset::extract_variable_info(negative_object)
    neg_info$variable_id <- paste("neg", neg_info$variable_id, sep = "::")
    neg_info$polarity <- "negative"
    out$neg <- neg_info
  }
  if (length(out) == 0) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(), polarity = character()))
  }
  info <- dplyr::bind_rows(out)
  for (col in c("mz", "rt")) {
    if (!col %in% colnames(info)) {
      info[[col]] <- NA_real_
    }
  }
  info
}

get_network_component_membership <- function(network) {
  feature_ids <- unique(c(network$from, network$to))
  graph <- igraph::graph_from_data_frame(network[, c("from", "to"), drop = FALSE],
                                         directed = FALSE,
                                         vertices = data.frame(name = feature_ids))
  igraph::components(graph)$membership
}

make_ms1_window_data <- function(object = NULL,
                                 positive_object = NULL,
                                 negative_object = NULL,
                                 network,
                                 selected_id,
                                 sub_network = "all") {
  feature_table <- if (!is.null(object)) {
    feature_spectrum_table(object)
  } else {
    dplyr::bind_rows(
      feature_spectrum_table(positive_object, prefix = "pos"),
      feature_spectrum_table(negative_object, prefix = "neg")
    )
  }

  if (nrow(feature_table) == 0) {
    return(list(features = feature_table, arrows = data.frame(), selected_id = selected_id))
  }

  subnet_ids <- get_selected_subnetwork_ids(network, selected_id, sub_network)
  if (length(subnet_ids) == 0) {
    return(list(features = feature_table[0, , drop = FALSE], arrows = data.frame(), selected_id = selected_id))
  }

  features <- feature_table[feature_table$variable_id %in% subnet_ids, , drop = FALSE]
  features <- features[order(features$rt, features$mz), , drop = FALSE]
  features$is_selected <- features$variable_id == selected_id
  features$hover <- sprintf(
    "Feature: %s<br>m/z: %.5f<br>RT: %.2f sec<br>Mean intensity: %.3g",
    features$variable_id,
    features$mz,
    features$rt,
    features$mean_intensity
  )

  isf_edges <- network[
    network$type %in% c("ISF", "Cross-polarity ISF") &
      network$from %in% subnet_ids & network$to %in% subnet_ids,
    ,
    drop = FALSE
  ]
  isf_edges <- isf_edges[
    isf_edges$from %in% features$variable_id & isf_edges$to %in% features$variable_id,
    ,
    drop = FALSE
  ]

  arrows <- if (nrow(isf_edges) > 0) {
    from_idx <- match(isf_edges$from, features$variable_id)
    to_idx <- match(isf_edges$to, features$variable_id)
    y <- features$mean_intensity[from_idx]
    yend <- features$mean_intensity[to_idx]
    y[!is.finite(y)] <- 0
    yend[!is.finite(yend)] <- 0
    y_base <- pmax(y, yend, na.rm = TRUE)
    y_base[!is.finite(y_base)] <- 0
    y_pad <- max(features$mean_intensity, na.rm = TRUE) * 0.08
    if (!is.finite(y_pad) || y_pad <= 0) y_pad <- 1
    data.frame(
      from = isf_edges$from,
      to = isf_edges$to,
      x = features$mz[from_idx],
      y = y,
      xend = features$mz[to_idx],
      yend = yend,
      y_arrow = y_base + y_pad,
      label = isf_annotation_label(features$mz[to_idx], isf_edges$annotation),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = character(), to = character(), x = numeric(), y = numeric(),
      xend = numeric(), yend = numeric(), y_arrow = numeric(), label = character()
    )
  }

  list(features = features, arrows = arrows, selected_id = selected_id)
}

get_selected_subnetwork_ids <- function(network, selected_id, sub_network = "all") {
  if (nrow(network) == 0) {
    return(character())
  }

  membership <- get_network_component_membership(network)
  if (!identical(sub_network, "all")) {
    return(names(membership)[membership == as.integer(sub_network)])
  }

  if (!selected_id %in% names(membership)) {
    return(character())
  }
  names(membership)[membership == membership[[selected_id]]]
}

feature_spectrum_table <- function(object, prefix = NULL) {
  if (is.null(object)) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(),
                      mean_intensity = numeric(), polarity = character()))
  }

  variable_info <- massdataset::extract_variable_info(object)
  expression_data <- massdataset::extract_expression_data(object)
  if (!all(c("variable_id", "mz", "rt") %in% colnames(variable_info))) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(),
                      mean_intensity = numeric(), polarity = character()))
  }

  variable_info$variable_id <- as.character(variable_info$variable_id)
  row_ids <- intersect(variable_info$variable_id, rownames(expression_data))
  variable_info <- variable_info[match(row_ids, variable_info$variable_id), , drop = FALSE]
  expression_data <- expression_data[row_ids, , drop = FALSE]
  expression_data <- suppressWarnings(as.data.frame(
    lapply(expression_data, function(x) as.numeric(as.character(x))),
    check.names = FALSE
  ))

  mean_intensity <- rowMeans(as.matrix(expression_data), na.rm = TRUE)
  mean_intensity[!is.finite(mean_intensity)] <- NA_real_

  out <- data.frame(
    variable_id = variable_info$variable_id,
    mz = suppressWarnings(as.numeric(variable_info$mz)),
    rt = suppressWarnings(as.numeric(variable_info$rt)),
    mean_intensity = mean_intensity,
    polarity = if (is.null(prefix)) NA_character_ else prefix,
    stringsAsFactors = FALSE
  )
  if (!is.null(prefix)) {
    out$variable_id <- paste(prefix, out$variable_id, sep = "::")
  }
  out
}

plot_ms1_window <- function(ms1_data) {
  features <- ms1_data$features
  validate(need(nrow(features) > 0, "No features found in the selected sub-network."))

  marker_color <- ifelse(features$is_selected, "#d95f02", "#2c7fb8")
  marker_size <- ifelse(features$is_selected, 12, 8)
  y_for_plot <- features$mean_intensity
  y_for_plot[!is.finite(y_for_plot)] <- 0

  stick_shapes <- lapply(seq_len(nrow(features)), function(i) {
    list(
      type = "line",
      x0 = features$mz[i],
      x1 = features$mz[i],
      y0 = 0,
      y1 = y_for_plot[i],
      xref = "x",
      yref = "y",
      line = list(color = marker_color[i], width = if (features$is_selected[i]) 2.5 else 1.4)
    )
  })

  p <- plotly::plot_ly(
    data = features,
    x = ~mz,
    y = y_for_plot,
    key = ~variable_id,
    source = "subnetwork_ms1",
    type = "scatter",
    mode = "markers",
    text = ~hover,
    hoverinfo = "text",
    marker = list(color = marker_color, size = marker_size, opacity = 0.82)
  )

  if (nrow(ms1_data$arrows) > 0) {
    for (i in seq_len(nrow(ms1_data$arrows))) {
      arrow <- ms1_data$arrows[i, ]
      p <- plotly::add_trace(
        p,
        x = c(arrow$x, arrow$xend),
        y = c(arrow$y_arrow, arrow$y_arrow),
        type = "scatter",
        mode = "lines",
        line = list(color = "#d95f02", width = 1.5, dash = "dot"),
        text = arrow$label,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    annotations <- lapply(seq_len(nrow(ms1_data$arrows)), function(i) {
      arrow <- ms1_data$arrows[i, ]
      list(
        x = arrow$xend,
        y = arrow$y_arrow,
        ax = arrow$x,
        ay = arrow$y_arrow,
        xref = "x",
        yref = "y",
        axref = "x",
        ayref = "y",
        text = "",
        showarrow = TRUE,
        arrowhead = 3,
        arrowsize = 1,
        arrowwidth = 1.5,
        arrowcolor = "#d95f02"
      )
    })
  } else {
    annotations <- list()
  }

  plotly::layout(
    p,
    shapes = stick_shapes,
    annotations = annotations,
    xaxis = list(title = "m/z", zeroline = FALSE),
    yaxis = list(title = "Mean feature intensity", rangemode = "tozero", zeroline = TRUE),
    margin = list(l = 65, r = 25, t = 20, b = 55),
    showlegend = FALSE
  ) |>
    plotly::event_register("plotly_click")
}

isf_annotation_label <- function(fragment_mz, annotation) {
  annotation <- ifelse(is.na(annotation) | !nzchar(annotation), "ISF", annotation)
  label <- ifelse(grepl("NL|loss|Loss|neutral", annotation),
                  annotation,
                  paste0("NL: ", annotation))
  sprintf("m/z=%.5f, %s", fragment_mz, label)
}

make_ms2_spectrum_data <- function(object = NULL,
                                   positive_object = NULL,
                                   negative_object = NULL,
                                   feature_id,
                                   mz_tol = 0.02,
                                   ms2_mz_tol_ppm = 5,
                                   ms2_rt_tol = 10,
                                   top_n = 12) {
  if (!is.null(object)) {
    return(extract_feature_ms2_data(
      object = object,
      feature_id = feature_id,
      display_id = feature_id,
      mz_tol = mz_tol,
      ms2_mz_tol_ppm = ms2_mz_tol_ppm,
      ms2_rt_tol = ms2_rt_tol,
      top_n = top_n
    ))
  }

  if (grepl("^pos::", feature_id)) {
    return(extract_feature_ms2_data(
      object = positive_object,
      feature_id = sub("^pos::", "", feature_id),
      display_id = feature_id,
      mz_tol = mz_tol,
      ms2_mz_tol_ppm = ms2_mz_tol_ppm,
      ms2_rt_tol = ms2_rt_tol,
      top_n = top_n
    ))
  }

  if (grepl("^neg::", feature_id)) {
    return(extract_feature_ms2_data(
      object = negative_object,
      feature_id = sub("^neg::", "", feature_id),
      display_id = feature_id,
      mz_tol = mz_tol,
      ms2_mz_tol_ppm = ms2_mz_tol_ppm,
      ms2_rt_tol = ms2_rt_tol,
      top_n = top_n
    ))
  }

  empty_ms2_plot_data(feature_id)
}

extract_feature_ms2_data <- function(object,
                                     feature_id,
                                     display_id,
                                     mz_tol,
                                     ms2_mz_tol_ppm,
                                     ms2_rt_tol,
                                     top_n) {
  if (is.null(object)) {
    return(empty_ms2_plot_data(display_id))
  }

  variable_info <- massdataset::extract_variable_info(object)
  if (!all(c("variable_id", "mz", "rt") %in% colnames(variable_info))) {
    return(empty_ms2_plot_data(display_id))
  }
  variable_info$variable_id <- as.character(variable_info$variable_id)
  selected <- variable_info[variable_info$variable_id == feature_id, , drop = FALSE]
  if (nrow(selected) == 0) {
    return(empty_ms2_plot_data(display_id))
  }

  ms2_index <- prepare_ms2_feature_index(
    object = object,
    variable_info = variable_info,
    mz_tol_ppm = ms2_mz_tol_ppm,
    rt_tol_sec = ms2_rt_tol
  )
  if (nrow(ms2_index$meta) == 0 || !feature_id %in% names(ms2_index$spectra)) {
    return(empty_ms2_plot_data(display_id, selected$mz[1], selected$rt[1]))
  }

  peaks <- as.data.frame(as_ms2_peak_matrix(ms2_index$spectra[[feature_id]]))
  if (nrow(peaks) == 0) {
    return(empty_ms2_plot_data(display_id, selected$mz[1], selected$rt[1]))
  }
  colnames(peaks) <- c("mz", "intensity")
  peaks$relative_intensity <- 100 * peaks$intensity / max(peaks$intensity, na.rm = TRUE)
  top_n <- max(1, min(as.integer(top_n), nrow(peaks)))
  top_idx <- order(peaks$relative_intensity, decreasing = TRUE)[seq_len(top_n)]
  peaks$annotate <- seq_len(nrow(peaks)) %in% top_idx
  peaks$fragment_annotation <- ""
  peaks$fragment_annotation[top_idx] <- annotate_ms2_fragment_peaks(
    peaks$mz,
    annotate_idx = top_idx,
    mz_tol = mz_tol
  )
  nl_arrows <- annotate_ms2_neutral_loss_pairs(
    peaks = peaks,
    mz_tol = mz_tol,
    top_idx = top_idx
  )
  peaks$hover <- sprintf(
    "m/z: %.5f<br>Intensity: %.3g<br>Relative: %.1f%%<br>%s",
    peaks$mz,
    peaks$intensity,
    peaks$relative_intensity,
    ifelse(nzchar(peaks$fragment_annotation),
           paste0("Diagnostic fragment: ", peaks$fragment_annotation),
           "Diagnostic fragment: -")
  )

  meta <- ms2_index$meta[feature_id, , drop = FALSE]
  list(
    feature_id = display_id,
    precursor_mz = as.numeric(selected$mz[1]),
    precursor_rt = as.numeric(selected$rt[1]),
    ms2_meta = meta,
    peaks = peaks,
    nl_arrows = nl_arrows
  )
}

empty_ms2_plot_data <- function(feature_id, precursor_mz = NA_real_, precursor_rt = NA_real_) {
  list(
    feature_id = feature_id,
    precursor_mz = precursor_mz,
    precursor_rt = precursor_rt,
    ms2_meta = data.frame(),
    peaks = data.frame(
      mz = numeric(),
      intensity = numeric(),
      relative_intensity = numeric(),
      annotate = logical(),
      fragment_annotation = character(),
      hover = character(),
      stringsAsFactors = FALSE
    ),
    nl_arrows = data.frame(
      x = numeric(), xend = numeric(), y = numeric(), label = character(),
      stringsAsFactors = FALSE
    )
  )
}

annotate_ms2_fragment_peaks <- function(mz, annotate_idx, mz_tol = 0.02) {
  fragment_table <- default_plant_fragment_ion_table()

  out <- rep("", length(mz))
  out[annotate_idx] <- vapply(mz[annotate_idx], function(x) {
    labels <- character()

    fragment_hit <- which(abs(fragment_table$fragment_mz - x) <= mz_tol)
    if (length(fragment_hit) > 0) {
      labels <- c(labels, paste0("Frag: ", fragment_table$assignment[fragment_hit[1]]))
    }

    paste(unique(labels), collapse = "; ")
  }, character(1))
  out
}

annotate_ms2_neutral_loss_pairs <- function(peaks, mz_tol = 0.02, top_idx = seq_len(nrow(peaks))) {
  neutral_loss_table <- default_neutral_loss_table()
  if (nrow(peaks) < 2 || length(top_idx) < 2) {
    return(data.frame(x = numeric(), xend = numeric(), y = numeric(), label = character()))
  }

  top_idx <- top_idx[order(peaks$mz[top_idx])]
  arrows <- list()
  arrow_idx <- 1L
  y_pad <- 8

  for (i in seq_along(top_idx)) {
    for (j in seq_along(top_idx)) {
      if (j <= i) next
      low <- top_idx[i]
      high <- top_idx[j]
      delta <- peaks$mz[high] - peaks$mz[low]
      hit <- which(abs(neutral_loss_table$mass - delta) <= mz_tol)
      if (length(hit) == 0) next

      y <- max(peaks$relative_intensity[c(low, high)], na.rm = TRUE) + y_pad
      y <- min(y, 105)
      arrows[[arrow_idx]] <- data.frame(
        x = peaks$mz[high],
        xend = peaks$mz[low],
        y = y,
        label = sprintf("NL: %s (%.5f)", neutral_loss_table$annotation[hit[1]], delta),
        stringsAsFactors = FALSE
      )
      arrow_idx <- arrow_idx + 1L
    }
  }

  if (length(arrows) == 0) {
    return(data.frame(x = numeric(), xend = numeric(), y = numeric(), label = character()))
  }

  do.call(rbind, arrows)
}

plot_ms2_spectrum <- function(ms2_data) {
  peaks <- ms2_data$peaks
  validate(need(nrow(peaks) > 0, "No assigned MS2 spectrum available for the selected peak."))

  shapes <- lapply(seq_len(nrow(peaks)), function(i) {
    list(
      type = "line",
      x0 = peaks$mz[i],
      x1 = peaks$mz[i],
      y0 = 0,
      y1 = peaks$relative_intensity[i],
      xref = "x",
      yref = "y",
      line = list(
        color = if (peaks$annotate[i] && nzchar(peaks$fragment_annotation[i])) "#d95f02" else "#4d4d4d",
        width = if (peaks$annotate[i]) 2 else 1
      )
    )
  })

  nl_shapes <- lapply(seq_len(nrow(ms2_data$nl_arrows)), function(i) {
    arrow <- ms2_data$nl_arrows[i, ]
    list(
      type = "line",
      x0 = arrow$x,
      x1 = arrow$xend,
      y0 = arrow$y,
      y1 = arrow$y,
      xref = "x",
      yref = "y",
      line = list(color = "#d95f02", width = 1.5, dash = "dot")
    )
  })

  nl_annotations <- lapply(seq_len(nrow(ms2_data$nl_arrows)), function(i) {
    arrow <- ms2_data$nl_arrows[i, ]
    list(
      x = arrow$xend,
      y = arrow$y,
      ax = arrow$x,
      ay = arrow$y,
      xref = "x",
      yref = "y",
      axref = "x",
      ayref = "y",
      text = "",
      showarrow = TRUE,
      arrowhead = 3,
      arrowsize = 1,
      arrowwidth = 1.5,
      arrowcolor = "#d95f02"
    )
  })

  plotly::plot_ly(
    data = peaks,
    x = ~mz,
    y = ~relative_intensity,
    type = "scatter",
    mode = "markers",
    text = ~hover,
    hoverinfo = "text",
    marker = list(size = 5, color = ifelse(peaks$annotate, "#d95f02", "#4d4d4d"))
  ) |>
    plotly::add_trace(
      data = ms2_data$nl_arrows,
      x = ~((x + xend) / 2),
      y = ~y,
      type = "scatter",
      mode = "markers",
      text = ~label,
      hoverinfo = "text",
      marker = list(size = 8, color = "rgba(217,95,2,0.001)"),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      shapes = c(shapes, nl_shapes),
      annotations = nl_annotations,
      xaxis = list(title = "m/z", zeroline = FALSE),
      yaxis = list(title = "Relative intensity (%)", range = c(0, 108), zeroline = TRUE),
      margin = list(l = 65, r = 25, t = 20, b = 55),
      showlegend = FALSE
    )
}

make_feature_eic_data <- function(wd,
                                  object = NULL,
                                  positive_object = NULL,
                                  negative_object = NULL,
                                  feature_id,
                                  mode = NULL,
                                  expand_rt = 15,
                                  expand_mz = 0.01,
                                  max_traces = 8) {
  if (!is.null(object)) {
    return(extract_feature_eic_data(
      wd = wd,
      object = object,
      feature_id = feature_id,
      display_id = feature_id,
      mode = mode,
      expand_rt = expand_rt,
      expand_mz = expand_mz,
      max_traces = max_traces
    ))
  }

  if (grepl("^pos::", feature_id)) {
    return(extract_feature_eic_data(
      wd = wd,
      object = positive_object,
      feature_id = sub("^pos::", "", feature_id),
      display_id = feature_id,
      mode = "positive",
      expand_rt = expand_rt,
      expand_mz = expand_mz,
      max_traces = max_traces
    ))
  }

  if (grepl("^neg::", feature_id)) {
    return(extract_feature_eic_data(
      wd = wd,
      object = negative_object,
      feature_id = sub("^neg::", "", feature_id),
      display_id = feature_id,
      mode = "negative",
      expand_rt = expand_rt,
      expand_mz = expand_mz,
      max_traces = max_traces
    ))
  }

  empty_eic_plot_data(feature_id, "Unable to determine ion mode for selected feature.")
}

extract_feature_eic_data <- function(wd,
                                     object,
                                     feature_id,
                                     display_id,
                                     mode,
                                     expand_rt,
                                     expand_mz,
                                     max_traces) {
  if (is.null(wd) || !dir.exists(wd)) {
    return(empty_eic_plot_data(display_id, "Project MS1 directory is not available."))
  }
  if (is.null(object)) {
    return(empty_eic_plot_data(display_id, "No mass_dataset object is available."))
  }

  xdata_path <- find_xcms_xdata_path(wd, mode)
  if (is.null(xdata_path)) {
    return(empty_eic_plot_data(display_id, "No massprocesser xdata file was detected."))
  }

  variable_info <- massdataset::extract_variable_info(object)
  variable_info$variable_id <- as.character(variable_info$variable_id)
  selected <- variable_info[variable_info$variable_id == feature_id, , drop = FALSE]
  if (nrow(selected) == 0 || !all(c("mz", "rt") %in% colnames(selected))) {
    return(empty_eic_plot_data(display_id, "Selected feature is missing m/z or RT metadata."))
  }

  env <- new.env(parent = emptyenv())
  load(xdata_path, envir = env)
  xdata_name <- intersect(c("xdata3", "xdata2", "xdata"), ls(env))[1]
  if (is.na(xdata_name)) {
    return(empty_eic_plot_data(display_id, "Detected xdata file does not contain an XCMS object."))
  }
  xdata <- env[[xdata_name]]

  mz <- as.numeric(selected$mz[1])
  rt <- as.numeric(selected$rt[1])
  if (!is.finite(mz) || !is.finite(rt)) {
    return(empty_eic_plot_data(display_id, "Selected feature has invalid m/z or RT."))
  }

  chrom <- tryCatch(
    xcms::chromatogram(
      object = xdata,
      mz = c(mz - expand_mz, mz + expand_mz),
      rt = c(rt - expand_rt, rt + expand_rt),
      aggregationFun = "max",
      missing = 0,
      include = "any"
    ),
    error = function(e) e
  )
  if (inherits(chrom, "error")) {
    return(empty_eic_plot_data(display_id, paste("Could not extract chromatogram:", chrom$message)))
  }

  eic_data <- xchromatograms_to_plot_data(chrom, max_traces = max_traces)
  if (nrow(eic_data) == 0) {
    return(empty_eic_plot_data(display_id, "No chromatographic signal was extracted for this feature."))
  }

  list(
    available = TRUE,
    feature_id = display_id,
    mz = mz,
    rt = rt,
    xdata_path = xdata_path,
    data = eic_data,
    message = "OK"
  )
}

find_xcms_xdata_path <- function(wd, mode) {
  mode_dir <- if (identical(mode, "negative")) "NEG" else "POS"
  ms1_root <- file.path(wd, "MS1")
  if (!dir.exists(ms1_root)) {
    return(NULL)
  }

  candidates <- list.files(
    ms1_root,
    pattern = "^(xdata3|xdata2|xdata)$",
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  candidates <- candidates[
    grepl(paste0(.Platform$file.sep, mode_dir, .Platform$file.sep), candidates) &
      grepl(paste0(.Platform$file.sep, "Result", .Platform$file.sep, "intermediate_data", .Platform$file.sep), candidates)
  ]
  if (length(candidates) == 0) {
    return(NULL)
  }

  priority <- match(basename(candidates), c("xdata3", "xdata2", "xdata"))
  candidates[order(priority, nchar(candidates))][1]
}

xchromatograms_to_plot_data <- function(chrom, max_traces = 8) {
  dims <- dim(chrom)
  if (length(dims) != 2 || dims[1] < 1 || dims[2] < 1) {
    return(data.frame(rt = numeric(), intensity = numeric(), sample = character()))
  }

  traces <- list()
  sample_names <- colnames(chrom)
  if (is.null(sample_names)) {
    sample_names <- paste0("sample_", seq_len(dims[2]))
  }

  trace_summaries <- numeric(dims[2])
  for (j in seq_len(dims[2])) {
    chr <- chrom@.Data[[1, j]]
    ints <- MSnbase::intensity(chr)
    trace_summaries[j] <- if (length(ints) > 0) max(ints, na.rm = TRUE) else NA_real_
  }
  keep_cols <- order(trace_summaries, decreasing = TRUE, na.last = NA)
  keep_cols <- utils::head(keep_cols, max_traces)

  for (j in keep_cols) {
    chr <- chrom@.Data[[1, j]]
    rts <- MSnbase::rtime(chr)
    ints <- MSnbase::intensity(chr)
    if (length(rts) == 0 || length(ints) == 0) {
      next
    }
    traces[[length(traces) + 1L]] <- data.frame(
      rt = rts,
      intensity = ints,
      sample = sample_names[j],
      stringsAsFactors = FALSE
    )
  }

  if (length(traces) == 0) {
    return(data.frame(rt = numeric(), intensity = numeric(), sample = character()))
  }
  do.call(rbind, traces)
}

empty_eic_plot_data <- function(feature_id, message) {
  list(
    available = FALSE,
    feature_id = feature_id,
    mz = NA_real_,
    rt = NA_real_,
    xdata_path = NA_character_,
    data = data.frame(rt = numeric(), intensity = numeric(), sample = character()),
    message = message
  )
}

plot_feature_eic <- function(eic_data) {
  validate(need(isTRUE(eic_data$available), eic_data$message))
  validate(need(nrow(eic_data$data) > 0, "No chromatogram points available."))

  plotly::plot_ly(
    data = eic_data$data,
    x = ~rt,
    y = ~intensity,
    color = ~sample,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    text = ~sprintf("Sample: %s<br>RT: %.2f sec<br>Intensity: %.3g", sample, rt, intensity)
  ) |>
    plotly::layout(
      xaxis = list(title = "Retention time (sec)", zeroline = FALSE),
      yaxis = list(title = "Raw EIC intensity", rangemode = "tozero", zeroline = TRUE),
      margin = list(l = 65, r = 20, t = 10, b = 45),
      legend = list(orientation = "h", x = 0, y = -0.25)
    )
}
