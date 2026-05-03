#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom golem add_resource_path bundle_resources favicon
#' @importFrom bsicons bs_icon
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Enable the bslib theme.
    bslib::page_navbar(
      # --- 1. Title area ---
      # Keep the brand and nav items aligned on the same baseline.
      title = tags$span(
        "MetMiner",
        # Add the version as a subscript.
        tags$sub("version 2.0.0",
                 style = "font-size: 0.75rem; color: #6c757d; font-weight: normal; margin-left: 5px; letter-spacing: 0;"),
        class = "fw-bold",
        style = "font-size: 1.6rem; color: #008080; letter-spacing: -0.5px; vertical-align: middle;"
      ),

      # --- 2. Theme ---
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "zephyr",
        primary = "#008080",
        "navbar-bg" = "#ffffff",
        base_font = font_google("Inter"),
        heading_font = font_google("Plus Jakarta Sans"),
        # Give navbar content a little more breathing room.
        "navbar-padding-y" = "0.8rem"
      ),

      # Keep navbar items vertically aligned.
      header = tags$style(HTML("
        .navbar-nav .nav-link {
            font-weight: 500;
            font-size: 1.05rem;
            display: flex;
            align-items: center;
            height: 100%;
        }
        /* Active nav item underline. */
        .navbar-nav .nav-link.active {
            color: #008080 !important;
            border-bottom: 3px solid #008080;
            margin-bottom: -3px;
        }
      ")),

      # --- 3. Navigation ---
      bslib::nav_panel("Home", mod_homepage_ui("home_1")),

      bslib::nav_panel(
        title = "Project Init",
        # The module owns its internal page width.
        mod_project_init_ui("project_init_1")
      ),

      bslib::nav_panel(
        title = "Data import",
        # The module owns its internal page width.
        mod_data_import_ui("data_import_1")
      ),

      bslib::nav_menu(
        title = "Cleaning",
        bslib::nav_panel("Overview", mod_data_overview_ui('data_overview_1')),
        bslib::nav_panel("Remove Noise", mod_data_rm_noise_ui('data_rm_noise_1')),
        bslib::nav_panel("Outlier Detection", mod_data_outlier_ui('data_outlier_1')),
        bslib::nav_panel("Imputation", mod_data_impute_ui('data_imputation_1')),
        bslib::nav_panel("Normalization", mod_data_norm_ui('data_norm_1')),
        bslib::nav_panel("Feature Network", mod_feature_network_ui('feature_network_1'))
      ),

      bslib::nav_menu(
        title = "Analysis",
        bslib::nav_panel("Annotation", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Filter Annotations", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Merge Data", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Classification", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Differential Analysis", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Enrichment", div(class="p-4", "Placeholder"))
      ),

      # --- 4. GitHub link ---
      bslib::nav_spacer(),

      bslib::nav_item(
        tags$a(
          # Use an outline button style for a quieter navbar action.
          class = "btn btn-outline-secondary d-flex align-items-center gap-2",
          style = "padding: 0.375rem 0.75rem; border-color: #dee2e6;",
          href = "https://github.com/ShawnWx2019/MetMiner",
          target = "_blank",
          # Keep icon and text aligned.
          bsicons::bs_icon("github", size = "1.1rem"),
          span("GitHub", style = "font-size: 0.95rem;")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources favicon
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MetMiner"
    )
  )
}
