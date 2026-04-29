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
            )
          ),
          bslib::card(
            height = "720px",
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
                sliderInput(ns("cross_cor_cutoff"), "Correlation", min = 0, max = 1, value = 0.7, step = 0.05)
              ),
              uiOutput(ns("network_ui"))
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

    get_merged_network <- function() {
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
          merged_feature_network <- merge_polarity_feature_networks(
            positive_object = global_data$object_pos_network,
            negative_object = global_data$object_neg_network,
            ppm = input$cross_ppm,
            rt_tolerance = input$cross_rt_tolerance,
            cor_cutoff = input$cross_cor_cutoff
          )
          global_data$merged_feature_network <- merged_feature_network

          if (!is.null(prj_init$mass_dataset_dir)) {
            save(merged_feature_network,
                 file = file.path(prj_init$mass_dataset_dir, "06.merged_feature_network.rda"))
          }
        }

        update_progress_modal(98, "Saving results and preparing network display...")
        update_progress_modal(100, "Done. Rendering the network panel may take a moment for large graphs.")
        close_progress_modal()
        shinyjs::enable("run_network")
        shinyalert::shinyalert("Feature Network Completed", "Feature relationships and empirical compounds are ready.", type = "success")
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

    network_data <- reactive({
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
      display <- network_data()
      render_label <- if (!is.null(display) && isTRUE(display$truncated)) {
        paste0(" - showing top ", nrow(display$edges), " edges by confidence")
      } else {
        ""
      }
      paste0(mode_label, " - ", sub_label, render_label)
    })

    output$network_ui <- renderUI({
      if (isTRUE(input$interactive_plot)) {
        visNetwork::visNetworkOutput(ns("network_vis"), height = "650px")
      } else {
        plotOutput(ns("network_static"), height = "650px")
      }
    })

    output$network_vis <- visNetwork::renderVisNetwork({
      display <- network_data()
      validate(need(!is.null(display), "No network edges available. Run detection first or lower the confidence cutoff."))
      visNetwork::visNetwork(display$nodes, display$edges, height = "650px") |>
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
        visNetwork::visLegend(useGroups = TRUE, position = "right")
    })

    output$network_static <- renderPlot({
      display <- network_data()
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
