#' Summarize mass_dataset Object Information
#'
#' Generates a formatted report summarizing key metadata.
#'
#' @param object A mass_dataset object
#' @param mode Ionization mode
#' @param show_processing Logical
#' @param show_qc Logical
#' @param color Logical
#' @export
summarize_massdataset <- function(object, mode = c("positive", "negative"),
                                  show_processing = TRUE, show_qc = TRUE, color = TRUE) {
  if (missing(object)) return("No object provided")
  if (!inherits(object, "mass_dataset")) return("Input must be a mass_dataset S4 object")
  mode <- match.arg(mode)

  # Colors (ANSI codes, mostly for console, but readable in verbatimTextOutput)
  col_title <- ""; col_reset <- ""; col_green <- ""; col_yellow <- ""

  output <- character(0)
  output <- c(output, sprintf("-- massdataset Object Summary (%s Mode) --", tools::toTitleCase(mode)), "")

  # Core metadata
  output <- c(output, "Core Components:",
              sprintf("|- Expression Data: %s x %s variables", nrow(object@expression_data), ncol(object@expression_data)),
              sprintf("|- Sample Info: %s samples x %s metadata", nrow(object@sample_info), ncol(object@sample_info)),
              sprintf("|- Variable Info: %s features x %s annotation", nrow(object@variable_info), ncol(object@variable_info)),
              sprintf("`- MS2 Spectra: %s", ifelse(length(object@ms2_data) > 0, "Available", "Not available")), "")

  # Processing history
  if (show_processing && length(object@process_info) > 0) {
    output <- c(output, "Processing History:", sprintf("|- Total steps: %d", length(object@process_info)))
    last_step_name <- names(object@process_info)[length(object@process_info)]
    output <- c(output, sprintf("`- Last step: %s", last_step_name), "")
  }

  # QC Check
  if (show_qc) {
    qc_count <- sum(grepl("QC", object@sample_info$sample_id, ignore.case = TRUE))
    output <- c(output, ifelse(qc_count > 0, sprintf("OK Contains %d QC samples", qc_count), "WARNING No QC samples detected"))
  }

  paste(output, collapse = "\n")
}

#' Generate Formatted Data Summary Report for Shiny
#'
#' @param object A `mass_dataset` object or a reactive returning one.
#' @param mode Ionization mode.
#' @export
check_massdata_info <- function(object, mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  shiny::renderPrint({
    obj <- if (shiny::is.reactive(object)) object() else object
    if (is.null(obj)) {
      cat(sprintf("ERROR No %s ion mode data detected", mode))
      return(invisible(NULL))
    }
    cat(summarize_massdataset(obj, mode = mode))
  })
}

#' Validate mass_dataset file
#'
#' @param path Path to an `.rda` file containing a `mass_dataset` object.
#' @param expected_polarity Expected polarity label.
#' @param object_label Human-readable label used in error messages.
#' @export
validate_file <- function(path, expected_polarity, object_label) {
  tryCatch({
    if (is.null(path)) return(list(success = FALSE, message = "Path is null"))
    if (!grepl("\\.rda$", path, ignore.case = TRUE)) return(list(success = FALSE, message = paste("Wrong file format:", object_label)))

    env <- new.env()
    obj_name <- load(path, envir = env)
    obj <- get(obj_name, envir = env)

    if (!inherits(obj, "mass_dataset")) return(list(success = FALSE, message = paste("Wrong object class:", object_label)))

    # Polarity validation is intentionally permissive here. In practice this
    # can be made stricter by inspecting variable_info or file metadata.
    list(success = TRUE, message = NULL, object = obj)
  }, error = function(e) {
    list(success = FALSE, message = paste("Error loading:", object_label, "-", e$message))
  })
}

#' Generate a short MetMiner job id
#'
#' @noRd
generate_metminer_job_id <- function() {
  rand <- paste0(sample(c(LETTERS, 0:9), 8, replace = TRUE), collapse = "")
  paste0("MM", format(Sys.time(), "%Y%m%d"), rand)
}

#' Path to the local MetMiner job index
#'
#' @noRd
metminer_job_index_file <- function(base_dir = path.expand("~/metminer_results")) {
  file.path(base_dir, ".metminer_job_index.rds")
}

#' Read the local MetMiner job index
#'
#' @noRd
read_metminer_job_index <- function(base_dir = path.expand("~/metminer_results")) {
  index_file <- metminer_job_index_file(base_dir)
  if (!file.exists(index_file)) {
    return(data.frame(
      job_id = character(),
      wd = character(),
      created_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    ))
  }

  idx <- tryCatch(readRDS(index_file), error = function(e) NULL)
  if (is.null(idx) || !is.data.frame(idx)) {
    return(data.frame(
      job_id = character(),
      wd = character(),
      created_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    ))
  }
  idx
}

#' Register or update a MetMiner job
#'
#' @noRd
register_metminer_job <- function(job_id, wd, base_dir = path.expand("~/metminer_results")) {
  dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
  idx <- read_metminer_job_index(base_dir)
  now <- as.character(Sys.time())

  row <- data.frame(
    job_id = job_id,
    wd = wd,
    created_at = now,
    updated_at = now,
    stringsAsFactors = FALSE
  )

  if (job_id %in% idx$job_id) {
    idx[idx$job_id == job_id, c("wd", "updated_at")] <- row[, c("wd", "updated_at")]
  } else {
    idx <- rbind(idx, row)
  }

  saveRDS(idx, metminer_job_index_file(base_dir))
  invisible(idx)
}

#' Find a MetMiner project path by job id
#'
#' @noRd
find_metminer_job <- function(job_id, base_dir = path.expand("~/metminer_results")) {
  job_id <- trimws(job_id %||% "")
  if (!nzchar(job_id)) {
    return(NULL)
  }

  idx <- read_metminer_job_index(base_dir)
  hit <- idx[idx$job_id == job_id, , drop = FALSE]
  if (nrow(hit) > 0 && dir.exists(hit$wd[1])) {
    return(hit$wd[1])
  }

  matches <- list.files(base_dir, pattern = paste0("_", job_id, "$"), full.names = TRUE)
  matches <- matches[dir.exists(matches)]
  if (length(matches) > 0) {
    register_metminer_job(job_id, matches[1], base_dir)
    return(matches[1])
  }

  NULL
}

#' Load saved mass_dataset objects from a MetMiner project folder
#'
#' @noRd
load_metminer_saved_objects <- function(wd) {
  mass_dataset_dir <- file.path(wd, "mass_dataset")
  if (!dir.exists(mass_dataset_dir)) {
    return(list())
  }

  object_files <- c(
    object_pos_raw = "01.object_pos_raw.rda",
    object_neg_raw = "01.object_neg_raw.rda",
    object_pos_tbl = "01.object_pos_tbl.rda",
    object_neg_tbl = "01.object_neg_tbl.rda",
    object_pos_clean = "02.object_pos_mv.rda",
    object_neg_clean = "02.object_neg_mv.rda",
    object_pos_outlier = "03.object_pos_outlier.rda",
    object_neg_outlier = "03.object_neg_outlier.rda",
    object_pos_impute = "04.object_pos_impute.rda",
    object_neg_impute = "04.object_neg_impute.rda",
    object_pos_norm = "05.object_pos_norm.rda",
    object_neg_norm = "05.object_neg_norm.rda",
    object_pos_network = "06.object_pos_feature_network.rda",
    object_neg_network = "06.object_neg_feature_network.rda",
    object_pos_annotated = "07.object_pos_annotated.rda",
    object_neg_annotated = "07.object_neg_annotated.rda",
    annotation_filter_result = "08.annotation_filter_result.rda"
  )

  loaded <- list()
  for (slot in names(object_files)) {
    path <- file.path(mass_dataset_dir, object_files[[slot]])
    if (!file.exists(path)) {
      next
    }

    env <- new.env(parent = emptyenv())
    obj_names <- tryCatch(load(path, envir = env), error = function(e) character())
    if (length(obj_names) == 0) {
      next
    }

    obj <- get(obj_names[1], envir = env)
    if (identical(slot, "annotation_filter_result") && is.list(obj)) {
      loaded[[slot]] <- obj
    } else if (inherits(obj, "mass_dataset")) {
      if (slot == "object_pos_tbl") slot <- "object_pos_raw"
      if (slot == "object_neg_tbl") slot <- "object_neg_raw"
      loaded[[slot]] <- obj
    }
  }

  loaded
}

#' Pick the most advanced loaded object for one polarity
#'
#' @noRd
latest_loaded_metminer_object <- function(loaded_objects, polarity = c("positive", "negative")) {
  polarity <- match.arg(polarity)
  prefix <- if (polarity == "positive") "object_pos_" else "object_neg_"
  order <- c("annotated", "network", "norm", "impute", "outlier", "clean", "raw")
  keys <- paste0(prefix, order)
  for (key in keys) {
    if (!is.null(loaded_objects[[key]])) {
      return(loaded_objects[[key]])
    }
  }
  NULL
}
