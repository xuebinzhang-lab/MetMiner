#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # --- 1. Global Data Containers ---

  # Project Initialization State (Paths, Resume info)
  prj_init <- reactiveValues(
    job_id = NULL,
    wd = NULL,
    sample_info = NULL,
    object_positive.init = NULL, # From resuming
    object_negative.init = NULL, # From resuming
    loaded_objects = list(),
    mass_dataset_dir = NULL,
    data_export_dir = NULL
  )

  # Unified Data Store (The "Ledger" for all modules)
  global_data <- reactiveValues(
    # Raw Data (Imported or Resumed)
    object_pos_raw = NULL,
    object_neg_raw = NULL,

    # Cleaned Data (After Noise Removal)
    object_pos_clean = NULL,
    object_neg_clean = NULL

    # Future steps: object_pos_imputed, object_pos_norm, etc.
  )


  # --- 2. Sync Logic: Project Init -> Global Data ---
  # If user resumes a task and loads objects in prj_init, sync them to global_data
  observe({
    req(prj_init$wd)
    if (length(prj_init$loaded_objects) > 0) {
      for (nm in names(prj_init$loaded_objects)) {
        global_data[[nm]] <- prj_init$loaded_objects[[nm]]
      }
    }
    # If raw objects were loaded during init (Resuming), put them into raw slots
    if(!is.null(prj_init$object_positive.init) && is.null(global_data$object_pos_raw)) global_data$object_pos_raw <- prj_init$object_positive.init
    if(!is.null(prj_init$object_negative.init) && is.null(global_data$object_neg_raw)) global_data$object_neg_raw <- prj_init$object_negative.init
  })

  # --- 3. Modules Calling ---

  # -> Homepage
  mod_homepage_server("home_1")

  # -> Project Initialization
  mod_project_init_server("project_init_1", prj_init = prj_init)

  # -> Unified Data Import
  # Now accepts global_data to write directly into it
  mod_data_import_server("data_import_1", prj_init = prj_init, global_data = global_data)

  # --- 4. Analysis Pipeline ---

  downloads <- reactiveValues(data = NULL)

  # -> Data Overview & QC
  # Now reads from global_data
  mod_data_overview_server(
    id = "data_overview_1",
    global_data = global_data, # Unified source
    downloads = downloads
  )

  # -> Data Cleaning: Noise Removal
  # Reads raw from global_data -> Writes clean to global_data
  mod_data_rm_noise_server("data_rm_noise_1", global_data = global_data, prj_init = prj_init)
  # outlier detection
  mod_data_outlier_server("data_outlier_1", global_data = global_data, prj_init = prj_init)
  # mv imputation
  mod_data_impute_server("data_imputation_1", global_data = global_data, prj_init = prj_init)
  # data nrom
  mod_data_norm_server("data_norm_1",global_data = global_data, prj_init = prj_init)
  # feature relationship network
  mod_feature_network_server("feature_network_1", global_data = global_data, prj_init = prj_init)
  # metabolite annotation
  mod_annotation_server("annotation_1", global_data = global_data, prj_init = prj_init)
  # feature-network assisted annotation validation and visualization
  mod_feature_annotation_server("feature_annotation_1", global_data = global_data, prj_init = prj_init)
  # annotation filtering and redundancy removal
  mod_annotation_filter_server("annotation_filter_1", global_data = global_data, prj_init = prj_init)
}
