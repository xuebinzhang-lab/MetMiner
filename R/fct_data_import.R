#' Validate sample files match sample info
#'
#' @param mode "POS" or "NEG"
#' @param QC_num.p List of QC files (POS)
#' @param S_num.p List of Subject files (POS)
#' @param QC_num.n List of QC files (NEG)
#' @param S_num.n List of Subject files (NEG)
#' @param sample_info_temp Sample info dataframe
#' @importFrom utils head
#' @export
validate_sample_files <- function(mode = "POS", QC_num.p, S_num.p, QC_num.n, S_num.n, sample_info_temp) {
  qc_files <- if (mode == "POS") QC_num.p else QC_num.n
  subj_files <- if (mode == "POS") S_num.p else S_num.n
  all_files <- c(qc_files, subj_files)

  # Remove file extensions for comparison
  clean_files <- gsub("\\.mzXML$", "", all_files, ignore.case = TRUE)

  # Check 1: Row count
  if (nrow(sample_info_temp) != length(all_files)) {
    shinyalert::shinyalert("Error",
                           paste0("Sample info count (", nrow(sample_info_temp),
                                  ") doesn't match ", mode, " files count (", length(all_files), ")"),
                           type = "error")
    return(FALSE)
  }

  # Check 2: ID consistency
  mismatch_ids <- setdiff(sample_info_temp$sample_id, clean_files)
  if (length(mismatch_ids) > 0) {
    shinyalert::shinyalert("Error",
                           HTML(paste("Sample ID mismatch in", mode, "mode:<br>",
                                      paste(head(mismatch_ids, 5), collapse = "<br>"),
                                      if(length(mismatch_ids)>5) "...and more" else "")),
                           type = "error", html = TRUE)
    return(FALSE)
  }
  return(TRUE)
}

#' Helper to check directory structure for Raw Import
#'
#' @param path Directory containing raw MS1 data folders.
#' @export
check_ms1_structure <- function(path) {
  dir_pos <- dir.exists(file.path(path, "POS"))
  dir_neg <- dir.exists(file.path(path, "NEG"))
  list(pos = dir_pos, neg = dir_neg)
}

#' Harmonize a mass_dataset sample_info table with project sample metadata
#'
#' Raw data imported by massprocesser can create a default sample_info from
#' file/folder names. This helper overlays the user-provided project
#' sample_info so downstream PCA/QC modules can use group, batch, and other
#' metadata columns consistently across raw, table, and RDA import paths.
#'
#' @param object A mass_dataset object.
#' @param project_sample_info Project-level sample_info data.frame.
#' @param mode Optional mode label used in messages.
#' @param logger Optional logger function.
#' @return Updated mass_dataset object.
#' @export
metminer_harmonize_sample_info <- function(object,
                                           project_sample_info = NULL,
                                           mode = NULL,
                                           logger = NULL) {
  if (!inherits(object, "mass_dataset") || is.null(project_sample_info)) {
    return(object)
  }

  obj_info <- tryCatch(
    as.data.frame(massdataset::extract_sample_info(object), stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  project_info <- as.data.frame(project_sample_info, stringsAsFactors = FALSE)

  if (nrow(obj_info) == 0 ||
      nrow(project_info) == 0 ||
      !"sample_id" %in% colnames(obj_info) ||
      !"sample_id" %in% colnames(project_info)) {
    return(object)
  }

  normalize_key <- function(x) {
    x <- trimws(as.character(x))
    x <- basename(x)
    x <- sub("\\.(mzML|mzXML|cdf|raw|wiff|mzData)$", "", x, ignore.case = TRUE)
    tolower(x)
  }

  project_keys <- normalize_key(project_info$sample_id)
  match_idx <- match(normalize_key(obj_info$sample_id), project_keys)

  if ("raw_file_name" %in% colnames(obj_info)) {
    need <- is.na(match_idx)
    if (any(need)) {
      match_idx[need] <- match(normalize_key(obj_info$raw_file_name[need]), project_keys)
    }
  }

  if ("raw_file_name" %in% colnames(project_info)) {
    project_raw_keys <- normalize_key(project_info$raw_file_name)
    need <- is.na(match_idx)
    if (any(need)) {
      match_idx[need] <- match(normalize_key(obj_info$sample_id[need]), project_raw_keys)
    }
    if ("raw_file_name" %in% colnames(obj_info)) {
      need <- is.na(match_idx)
      if (any(need)) {
        match_idx[need] <- match(normalize_key(obj_info$raw_file_name[need]), project_raw_keys)
      }
    }
  }

  matched <- !is.na(match_idx)
  if (!any(matched)) {
    if (is.function(logger)) {
      logger(
        paste0(
          "No sample_info rows matched imported ",
          mode %||% "mass_dataset",
          " samples. Keeping object sample_info unchanged."
        ),
        "warning"
      )
    }
    return(object)
  }

  metadata_cols <- setdiff(colnames(project_info), "sample_id")
  for (col in metadata_cols) {
    values <- rep(NA, nrow(obj_info))
    values[matched] <- project_info[[col]][match_idx[matched]]
    if (col %in% colnames(obj_info)) {
      keep_existing <- is.na(values) | values == ""
      obj_info[[col]][!keep_existing] <- values[!keep_existing]
    } else {
      obj_info[[col]] <- values
    }
  }

  if (!"project_sample_id" %in% colnames(obj_info)) {
    obj_info$project_sample_id <- NA_character_
  }
  obj_info$project_sample_id[matched] <- as.character(project_info$sample_id[match_idx[matched]])

  object@sample_info <- obj_info
  object <- massdataset::update_sample_info(object)
  object@other_files$metminer_sample_info_match <- list(
    matched_samples = sum(matched),
    total_samples = nrow(obj_info),
    unmatched_sample_id = as.character(obj_info$sample_id[!matched]),
    updated_at = as.character(Sys.time())
  )

  if (is.function(logger)) {
    type <- if (all(matched)) "success" else "warning"
    logger(
      paste0(
        "Applied project sample_info to ",
        sum(matched), "/", nrow(obj_info), " ",
        mode %||% "mass_dataset",
        " samples. Metadata columns: ",
        paste(metadata_cols, collapse = ", ")
      ),
      type
    )
  }

  object
}
