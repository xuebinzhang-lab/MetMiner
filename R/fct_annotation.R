# ---- Annotation database helpers ----

#' Built-in PlantCyc annotation database metadata
#'
#' @noRd
metminer_builtin_plantcyc_annotation_databases <- function() {
  manifest <- tryCatch({
    env <- new.env(parent = emptyenv())
    if (file.exists(file.path("data", "plantcyc_pgdb_manifest.rda"))) {
      load(file.path("data", "plantcyc_pgdb_manifest.rda"), envir = env)
    } else {
      utils::data(list = "plantcyc_pgdb_manifest", package = "MetMiner", envir = env)
    }
    env$plantcyc_pgdb_manifest
  }, error = function(e) data.frame())

  if (nrow(manifest) == 0 || !"ms1_file" %in% colnames(manifest)) {
    return(character())
  }

  ids <- tools::file_path_sans_ext(basename(manifest$ms1_file))
  labels <- paste0(manifest$organism, " (", manifest$version, ")")
  stats::setNames(ids, labels)
}

#' Expand species-level PlantCyc choices to paired MS1 and MS2 database ids
#'
#' @noRd
metminer_expand_builtin_annotation_ids <- function(database_ids) {
  database_ids <- as.character(database_ids %||% character())
  if (length(database_ids) == 0) {
    return(character())
  }
  plantcyc_choices <- unname(metminer_builtin_plantcyc_annotation_databases())
  out <- character()
  for (database_id in database_ids) {
    out <- c(out, database_id)
    if (database_id %in% plantcyc_choices && grepl("_ms1$", database_id)) {
      ms2_id <- sub("_ms1$", "_ms2", database_id)
      ms2_file <- file.path("data", paste0(ms2_id, ".rda"))
      pkg_ms2_file <- system.file("data", paste0(ms2_id, ".rda"), package = "MetMiner")
      if (file.exists(ms2_file) || has_text(pkg_ms2_file)) {
        out <- c(out, ms2_id)
      }
    }
  }
  unique(out)
}

#' Built-in public metid database metadata
#'
#' @noRd
metminer_public_annotation_databases <- function() {
  choices <- c(
    "HMDB MS2" = "hmdb_ms2",
    "MassBank MS2" = "massbank_ms2",
    "MoNA MS2" = "mona_ms2"
  )

  if (!requireNamespace("massdbbuildin", quietly = TRUE)) {
    return(character())
  }

  available <- tryCatch({
    utils::data(package = "massdbbuildin")$results[, "Item"]
  }, error = function(e) character())

  if (length(available) == 0) {
    return(choices)
  }
  choices[choices %in% available]
}

#' Built-in metid database metadata
#'
#' @noRd
metminer_builtin_annotation_databases <- function() {
  c(
    metminer_builtin_plantcyc_annotation_databases(),
    metminer_public_annotation_databases()
  )
}

#' Format annotation database ids as labels
#'
#' @noRd
metminer_annotation_database_labels <- function(database_ids) {
  database_ids <- database_ids %||% character()
  if (length(database_ids) == 0) {
    return(character())
  }
  choices <- metminer_builtin_annotation_databases()
  plantcyc_choices <- metminer_builtin_plantcyc_annotation_databases()
  ms2_ids <- sub("_ms1$", "_ms2", unname(plantcyc_choices))
  choices <- c(choices, stats::setNames(ms2_ids, paste0(names(plantcyc_choices), " MS2")))
  labels <- names(choices)[match(database_ids, unname(choices))]
  labels[!has_text(labels)] <- database_ids[!has_text(labels)]
  labels
}

#' Format database labels with italicized Latin binomials for HTML controls
#'
#' @noRd
metminer_annotation_database_label_html <- function(labels) {
  labels <- as.character(labels %||% character())
  vapply(labels, function(label) {
    escaped <- htmltools::htmlEscape(label)
    sub(
      "^([A-Z][a-z]+\\s+[a-z][a-zA-Z._-]*)(.*)$",
      "<em>\\1</em>\\2",
      escaped,
      perl = TRUE
    )
  }, character(1), USE.NAMES = FALSE)
}

#' Load a databaseClass object from massdbbuildin
#'
#' @noRd
metminer_load_builtin_database <- function(database_id) {
  plantcyc_choices <- metminer_builtin_plantcyc_annotation_databases()
  plantcyc_ms2_ids <- sub("_ms1$", "_ms2", unname(plantcyc_choices))
  if (database_id %in% c(unname(plantcyc_choices), plantcyc_ms2_ids)) {
    env <- new.env(parent = emptyenv())
    file <- file.path("data", paste0(database_id, ".rda"))
    object_names <- if (file.exists(file)) {
      load(file, envir = env)
    } else {
      utils::data(list = database_id, package = "MetMiner", envir = env)
    }
    if (!database_id %in% object_names && exists(database_id, envir = env, inherits = FALSE)) {
      object_names <- unique(c(object_names, database_id))
    }
    db <- NULL
    for (object_name in object_names) {
      obj <- get(object_name, envir = env)
      if (methods::is(obj, "databaseClass")) {
        db <- obj
        break
      }
    }
    if (is.null(db)) {
      stop("Built-in PlantCyc database is not a metid databaseClass object: ", database_id, call. = FALSE)
    }
    label <- names(plantcyc_choices)[match(sub("_ms2$", "_ms1", database_id), unname(plantcyc_choices))]
    if (!has_text(label)) label <- metminer_database_label(db, database_id)
    label <- paste(label, if (grepl("_ms2$", database_id)) "MS2" else "MS1")
    return(list(id = database_id, label = label, database = db))
  }

  if (!requireNamespace("massdbbuildin", quietly = TRUE)) {
    stop("Package 'massdbbuildin' is required for built-in databases.", call. = FALSE)
  }

  env <- new.env(parent = emptyenv())
  utils::data(list = database_id, package = "massdbbuildin", envir = env)
  db <- get(database_id, envir = env)

  if (!methods::is(db, "databaseClass")) {
    stop("Built-in database is not a metid databaseClass object: ", database_id, call. = FALSE)
  }

  list(id = database_id, label = metminer_database_label(db, database_id), database = db)
}

#' Load databaseClass objects from a local folder of .rda files
#'
#' @noRd
metminer_load_local_databases <- function(directory) {
  if (is.null(directory) || length(directory) == 0 || !has_text(directory)) {
    return(list())
  }
  directory <- as.character(directory)[1]
  if (!dir.exists(directory)) {
    stop("Local database folder does not exist: ", directory, call. = FALSE)
  }

  files <- list.files(directory, pattern = "\\.rda$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(list())
  }

  databases <- list()
  for (file in files) {
    env <- new.env(parent = emptyenv())
    object_names <- tryCatch(load(file, envir = env), error = function(e) {
      warning("Failed to load database file: ", basename(file), " - ", e$message, call. = FALSE)
      character()
    })

    for (object_name in object_names) {
      obj <- get(object_name, envir = env)
      if (methods::is(obj, "databaseClass")) {
        id <- paste0(tools::file_path_sans_ext(basename(file)), "::", object_name)
        databases[[id]] <- list(
          id = id,
          label = metminer_database_label(obj, id),
          database = obj
        )
      }
    }
  }

  unname(databases)
}

#' Build a readable database label
#'
#' @noRd
metminer_database_label <- function(database, fallback = "database") {
  info <- database@database.info
  source <- info$Source %||% fallback
  version <- info$Version %||% ""
  label <- paste(c(source, version), collapse = "_")
  label <- gsub("_$", "", label)
  if (!has_text(label)) fallback else as.character(label)
}

#' Collect annotation databases from built-in ids, KEGG output, and custom folders
#'
#' @noRd
metminer_collect_annotation_databases <- function(builtin_ids = character(),
                                                  kegg_dir = NULL,
                                                  custom_dir = NULL,
                                                  local_dir = NULL) {
  builtin_ids <- metminer_expand_builtin_annotation_ids(builtin_ids)
  dbs <- list()

  for (database_id in builtin_ids) {
    db <- tryCatch(
      metminer_load_builtin_database(database_id),
      error = function(e) {
        warning("Skipped annotation database '", database_id, "': ", e$message, call. = FALSE)
        NULL
      }
    )
    if (!is.null(db)) {
      dbs[[length(dbs) + 1L]] <- db
    }
  }

  for (directory in list(kegg_dir, custom_dir, local_dir)) {
    loaded <- tryCatch(
      metminer_load_local_databases(directory),
      error = function(e) {
        warning("Skipped local annotation database folder: ", e$message, call. = FALSE)
        list()
      }
    )
    dbs <- c(dbs, loaded)
  }

  if (length(dbs) == 0) {
    stop("No annotation database selected or loaded.", call. = FALSE)
  }
  unname(dbs)
}

# ---- Annotation execution ----

metminer_annotation_qc_sample_ids <- function(sample_info) {
  sample_info <- as.data.frame(sample_info %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(sample_info) == 0 || !"sample_id" %in% colnames(sample_info)) {
    return(character())
  }

  normalize_label <- function(x) {
    x <- toupper(trimws(as.character(x %||% NA_character_)))
    gsub("[^A-Z0-9]+", "", x, perl = TRUE)
  }

  qc_ids <- character()
  class_cols <- intersect(c("class", "sample_class", "sample_type", "type"), colnames(sample_info))
  for (col in class_cols) {
    label <- normalize_label(sample_info[[col]])
    is_qc <- label %in% c("QC", "QUALITYCONTROL", "QUALITYCONTROLSAMPLE")
    qc_ids <- c(qc_ids, as.character(sample_info$sample_id[is_qc]))
  }

  if (length(qc_ids) == 0) {
    sid <- as.character(sample_info$sample_id)
    qc_ids <- sid[grepl("(^|[^A-Za-z0-9])QC([^A-Za-z0-9]|$)|^QC[0-9_-]*$", sid, ignore.case = TRUE, perl = TRUE)]
  }

  unique(qc_ids[has_text(qc_ids)])
}

metminer_set_annotation_sample_filter <- function(object, filter_info) {
  tryCatch({
    object@other_files$annotation_sample_filter <- filter_info
    object
  }, error = function(e) object)
}

metminer_get_annotation_sample_filter <- function(object) {
  tryCatch(object@other_files$annotation_sample_filter, error = function(e) NULL)
}

metminer_prepare_annotation_input <- function(object, mode = NULL) {
  empty <- list(
    object = object,
    removed_qc_ids = character(),
    samples_before = NA_integer_,
    samples_after = NA_integer_,
    message = "No QC samples removed before annotation."
  )
  if (is.null(object) || !inherits(object, "mass_dataset")) {
    return(empty)
  }

  sample_info <- tryCatch(
    as.data.frame(massdataset::extract_sample_info(object), stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  if (nrow(sample_info) == 0 || !"sample_id" %in% colnames(sample_info)) {
    return(empty)
  }

  qc_ids <- intersect(metminer_annotation_qc_sample_ids(sample_info), as.character(sample_info$sample_id))
  samples_before <- nrow(sample_info)
  if (length(qc_ids) == 0) {
    filter_info <- list(
      removed_qc_ids = character(),
      removed_qc_count = 0L,
      samples_before = samples_before,
      samples_after = samples_before,
      mode = mode %||% NA_character_,
      applied = FALSE
    )
    object <- metminer_set_annotation_sample_filter(object, filter_info)
    return(modifyList(empty, list(
      object = object,
      samples_before = samples_before,
      samples_after = samples_before
    )))
  }

  keep_ids <- setdiff(as.character(sample_info$sample_id), qc_ids)
  if (length(keep_ids) == 0) {
    stop("All samples are QC samples; annotation requires at least one non-QC biological sample.", call. = FALSE)
  }

  filtered <- tryCatch({
    active <- massdataset::activate_mass_dataset(object, what = "sample_info")
    dplyr::filter(active, !sample_id %in% qc_ids)
  }, error = function(e) {
    stop("Failed to remove QC samples before annotation: ", conditionMessage(e), call. = FALSE)
  })

  samples_after <- tryCatch(nrow(massdataset::extract_sample_info(filtered)), error = function(e) length(keep_ids))
  filter_info <- list(
    removed_qc_ids = qc_ids,
    removed_qc_count = length(qc_ids),
    samples_before = samples_before,
    samples_after = samples_after,
    mode = mode %||% NA_character_,
    applied = TRUE
  )
  filtered <- metminer_set_annotation_sample_filter(filtered, filter_info)

  list(
    object = filtered,
    removed_qc_ids = qc_ids,
    samples_before = samples_before,
    samples_after = samples_after,
    message = paste0(
      "Removed ", length(qc_ids), " QC sample",
      if (length(qc_ids) == 1) "" else "s",
      " before annotation (", samples_before, " -> ", samples_after, ")."
    )
  )
}

metminer_annotation_database_type <- function(database = NULL, database_id = NA_character_, label = NA_character_) {
  text <- paste(
    as.character(database_id %||% NA_character_),
    as.character(label %||% NA_character_),
    tryCatch(as.character(database@database.info$Source %||% NA_character_), error = function(e) NA_character_),
    tryCatch(as.character(database@database.info$Version %||% NA_character_), error = function(e) NA_character_),
    collapse = " "
  )
  text <- tolower(text)
  dplyr::case_when(
    grepl("plantcyc|pmn|biocyc|pgdb|_cyc", text, perl = TRUE) ~ "plantcyc",
    grepl("kegg", text, perl = TRUE) ~ "kegg",
    grepl("hmdb|massbank|mona|gnps|respect|rist|nist|mzcloud|massive", text, perl = TRUE) ~ "public_ms2",
    TRUE ~ "custom"
  )
}

metminer_empty_annotation_table <- function() {
  cols <- c(
    "variable_id", "ms2_spectrum_id", "Compound.name", "CAS.ID", "HMDB.ID",
    "KEGG.ID", "PlantCyc.ID", "BIOCYC.ID", "Lab.ID", "Adduct",
    "Kingdom", "Super_class", "Class", "Sub_class", "direct_parent",
    "molecular_framework", "classyfire_status", "classyfire_source",
    "mz.error", "mz.match.score", "RT.error", "RT.match.score", "CE", "SS",
    "Total.score", "Database", "Database.ID", "Database.Label", "Database.Source.Type",
    "Level", "Annotation.Layer", "Annotation.Layer.detail", "annotation_layer",
    "evidence_scope", "core_adduct_match", "strict_genome_adduct_pass",
    "metminer_confidence_level", "database_rank", "annotation_rank"
  )
  char_cols <- c(
    "variable_id", "ms2_spectrum_id", "Compound.name", "CAS.ID", "HMDB.ID",
    "KEGG.ID", "PlantCyc.ID", "BIOCYC.ID", "Lab.ID", "Adduct",
    "Kingdom", "Super_class", "Class", "Sub_class", "direct_parent",
    "molecular_framework", "classyfire_status", "classyfire_source",
    "CE", "Database", "Database.ID", "Database.Label", "Database.Source.Type",
    "Annotation.Layer", "Annotation.Layer.detail", "annotation_layer",
    "evidence_scope", "core_adduct_match", "strict_genome_adduct_pass",
    "metminer_confidence_level"
  )
  numeric_cols <- c("mz.error", "mz.match.score", "RT.error", "RT.match.score", "SS", "Total.score")
  integer_cols <- c("Level", "database_rank", "annotation_rank")
  out <- c(
    stats::setNames(rep(list(character()), length(char_cols)), char_cols),
    stats::setNames(rep(list(numeric()), length(numeric_cols)), numeric_cols),
    stats::setNames(rep(list(integer()), length(integer_cols)), integer_cols)
  )
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out[, cols, drop = FALSE]
}

metminer_coerce_annotation_table_types <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0) {
    return(metminer_empty_annotation_table())
  }
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  char_cols <- c(
    "variable_id", "ms2_spectrum_id", "Compound.name", "CAS.ID", "HMDB.ID",
    "KEGG.ID", "PlantCyc.ID", "BIOCYC.ID", "Lab.ID", "Adduct",
    "Kingdom", "Super_class", "Class", "Sub_class", "direct_parent",
    "molecular_framework", "classyfire_status", "classyfire_source", "CE",
    "Database", "Database.ID", "Database.Label", "Database.Source.Type",
    "Annotation.Layer", "Annotation.Layer.detail", "annotation_layer",
    "evidence_scope", "core_adduct_match", "strict_genome_adduct_pass",
    "metminer_confidence_level"
  )
  numeric_cols <- c("mz.error", "mz.match.score", "RT.error", "RT.match.score", "SS", "Total.score")
  integer_cols <- c("Level", "database_rank", "annotation_rank")
  for (col in intersect(char_cols, colnames(tab))) {
    tab[[col]] <- as.character(tab[[col]])
  }
  for (col in intersect(numeric_cols, colnames(tab))) {
    tab[[col]] <- suppressWarnings(as.numeric(tab[[col]]))
  }
  for (col in intersect(integer_cols, colnames(tab))) {
    tab[[col]] <- suppressWarnings(as.integer(tab[[col]]))
  }
  tab
}

metminer_annotation_classification_cols <- function() {
  c(
    "Kingdom", "Super_class", "Class", "Sub_class", "direct_parent",
    "molecular_framework", "classyfire_status", "classyfire_source"
  )
}

metminer_add_database_classification_columns <- function(tab, database) {
  tab <- as.data.frame(tab %||% data.frame(), stringsAsFactors = FALSE)
  cols <- metminer_annotation_classification_cols()
  for (col in cols) {
    if (!col %in% colnames(tab)) tab[[col]] <- NA_character_
  }
  if (nrow(tab) == 0 || is.null(database)) {
    return(tab)
  }

  info <- tryCatch(as.data.frame(database@spectra.info, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(info) == 0 || !"Lab.ID" %in% colnames(info)) {
    return(tab)
  }
  present_cols <- intersect(cols, colnames(info))
  if (length(present_cols) == 0) {
    return(tab)
  }

  info$Lab.ID <- as.character(info$Lab.ID)
  info <- info[has_text(info$Lab.ID), unique(c("Lab.ID", present_cols)), drop = FALSE]
  info <- info[!duplicated(info$Lab.ID), , drop = FALSE]
  hit <- match(as.character(tab$Lab.ID %||% NA_character_), info$Lab.ID)
  for (col in present_cols) {
    value <- as.character(tab[[col]] %||% NA_character_)
    missing <- !has_text(value) & !is.na(hit)
    value[missing] <- as.character(info[[col]][hit[missing]])
    value[!has_text(value)] <- NA_character_
    tab[[col]] <- value
  }
  tab
}

metminer_order_annotation_table <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0) {
    return(metminer_empty_annotation_table())
  }
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(tab)) {
    return(metminer_empty_annotation_table())
  }
  tab$Level <- suppressWarnings(as.integer(tab$Level))
  tab$Total.score <- suppressWarnings(as.numeric(tab$Total.score))
  if (!"annotation_layer" %in% colnames(tab)) tab$annotation_layer <- NA_character_
  if (!"database_rank" %in% colnames(tab)) tab$database_rank <- NA_integer_
  if (!"annotation_rank" %in% colnames(tab)) tab$annotation_rank <- NA_integer_
  tab <- tab[order(
    tab$variable_id,
    metminer_annotation_layer_priority(tab$annotation_layer),
    tab$Level,
    -tab$Total.score,
    tab$database_rank,
    tab$annotation_rank,
    na.last = TRUE
  ), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

metminer_finalize_annotation_table <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0) {
    return(metminer_empty_annotation_table())
  }
  tab <- metminer_order_annotation_table(tab)
  tab <- dplyr::distinct(tab, .keep_all = TRUE)
  required <- colnames(metminer_empty_annotation_table())
  tab <- metminer_ensure_columns(tab, required)
  tab <- metminer_coerce_annotation_table_types(tab)
  extra <- setdiff(colnames(tab), c(required, "ms2_files_id"))
  tab <- tab[, c(required, extra), drop = FALSE]
  rownames(tab) <- NULL
  tab
}

metminer_normalize_annotation_result <- function(tab, db, database_rank = NA_integer_,
                                                 polarity = c("positive", "negative")) {
  polarity <- match.arg(polarity)
  if (is.null(tab) || nrow(tab) == 0) {
    return(metminer_empty_annotation_table())
  }

  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  tab$ms2_files_id <- NULL
  tab <- metminer_standardize_review_annotation_cols(tab)

  database_id <- db$id %||% NA_character_
  database_label <- db$label %||% metminer_database_label(db$database, database_id)
  database_type <- metminer_annotation_database_type(db$database, database_id, database_label)

  tab$Database.ID <- as.character(database_id)
  tab$Database.Label <- as.character(database_label)
  tab$Database.Source.Type <- database_type
  if (!"Database" %in% colnames(tab) || all(!has_text(tab$Database))) {
    tab$Database <- database_label
  }
  tab <- metminer_add_database_classification_columns(tab, db$database)
  tab <- metminer_add_annotation_layer_columns(tab, polarity)
  tab <- metminer_fill_same_compound_annotation_ids(tab)
  tab <- metminer_add_annotation_layer_columns(tab, polarity)
  tab$Annotation.Layer <- metminer_annotation_layer_short_label(tab$annotation_layer)
  tab$Annotation.Layer.detail <- tab$evidence_scope
  tab$database_rank <- suppressWarnings(as.integer(database_rank))

  tab <- metminer_order_annotation_table(tab)
  tab$annotation_rank <- ave(seq_len(nrow(tab)), tab$variable_id, FUN = seq_along)
  metminer_finalize_annotation_table(tab)
}

#' Normalize metID annotation process records to a list
#'
#' @noRd
metminer_annotation_process_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (is.list(x)) {
    return(x)
  }
  list(x)
}

#' Annotate one mass_dataset with multiple metid databases
#'
#' @noRd
metminer_annotate_mass_dataset <- function(object, databases, polarity,
                                           ms1.match.ppm = 25,
                                           ms2.match.ppm = 30,
                                           rt.match.tol = 30,
                                           column = c("rp", "hilic"),
                                           candidate.num = 3,
                                           threads = 1) {
  if (!requireNamespace("metid", quietly = TRUE)) {
    stop("Package 'metid' is required for metabolite annotation.", call. = FALSE)
  }
  suppressPackageStartupMessages(
    require("metid", character.only = TRUE, quietly = TRUE)
  )
  if (is.null(object)) {
    return(NULL)
  }

  column <- match.arg(column)
  out <- object
  base_object <- object
  base_object@annotation_table <- base_object@annotation_table[0, , drop = FALSE]
  original_process_info <- object@process_info
  original_annotation_process <- metminer_annotation_process_list(
    original_process_info$annotate_metabolites_mass_dataset
  )

  annotation_tables <- list()
  annotation_process <- list()
  for (db_i in seq_along(databases)) {
    db <- databases[[db_i]]
    db_label <- db$label %||% db$id %||% paste0("database_", db_i)
    annotated <- tryCatch(
      metid::annotate_metabolites_mass_dataset(
        object = base_object,
        ms1.match.ppm = ms1.match.ppm,
        ms2.match.ppm = ms2.match.ppm,
        rt.match.tol = rt.match.tol,
        polarity = polarity,
        column = column,
        candidate.num = candidate.num,
        database = db$database,
        threads = threads
      ),
      error = function(e) {
        stop("Annotation failed while matching database '", db_label, "': ", conditionMessage(e), call. = FALSE)
      }
    )
    tab <- metminer_safe_extract_annotation_table(annotated)
    if (nrow(tab) > 0) {
      tab <- metminer_normalize_annotation_result(
        tab = tab,
        db = db,
        database_rank = db_i,
        polarity = polarity
      )
      annotation_tables[[length(annotation_tables) + 1L]] <- tab
    }

    db_process <- metminer_annotation_process_list(
      annotated@process_info$annotate_metabolites_mass_dataset
    )
    if (length(db_process) > length(original_annotation_process)) {
      annotation_process <- c(annotation_process, db_process[(length(original_annotation_process) + 1L):length(db_process)])
    }
  }

  if (length(annotation_tables) > 0) {
    combined <- dplyr::bind_rows(annotation_tables)
    combined <- metminer_finalize_annotation_table(combined)
    out@annotation_table <- combined
  } else {
    out@annotation_table <- metminer_empty_annotation_table()
  }

  if (length(annotation_process) > 0) {
    process_info <- original_process_info
    if (length(original_annotation_process) == 0) {
      process_info$annotate_metabolites_mass_dataset <- annotation_process
    } else {
      process_info$annotate_metabolites_mass_dataset <- c(original_annotation_process, annotation_process)
    }
    out@process_info <- process_info
  }

  out
}

# ---- Safe extractors ----

#' Safely extract annotation table from a mass_dataset object
#'
#' @noRd
metminer_safe_extract_annotation_table <- function(object) {
  tryCatch(
    massdataset::extract_annotation_table(object),
    error = function(e) data.frame()
  )
}

metminer_annotation_layer_short_label <- function(layer) {
  layer <- as.character(layer %||% NA_character_)
  dplyr::case_when(
    layer == "genome_reaction" ~ "Layer1",
    layer %in% c("public_ms2", "local_spectral_optional", "other_spectral") ~ "Layer2",
    TRUE ~ NA_character_
  )
}

metminer_format_annotation_table_for_display <- function(object, mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  tab <- metminer_safe_extract_annotation_table(object)
  tab <- as.data.frame(tab %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(tab) == 0) return(tab)

  tab <- metminer_add_annotation_layer_columns(tab, mode = mode)
  tab$Annotation.Layer <- metminer_annotation_layer_short_label(tab$annotation_layer)
  tab$Annotation.Layer.detail <- tab$evidence_scope

  remove_keys <- tolower(gsub("[^a-z0-9]+", "_", colnames(tab)))
  internal_keys <- c(
    "ms2_files_id",
    "annotation_layer",
    "evidence_scope",
    "core_adduct_match",
    "strict_genome_adduct_pass",
    "metminer_confidence_level"
  )
  tab <- tab[, !remove_keys %in% internal_keys, drop = FALSE]

  layer_cols <- c("Annotation.Layer", "Annotation.Layer.detail")
  base_cols <- setdiff(colnames(tab), layer_cols)
  insert_after <- match("Level", base_cols)
  if (is.na(insert_after)) insert_after <- match("Database", base_cols)
  if (is.na(insert_after)) {
    tab <- tab[, c(layer_cols, base_cols), drop = FALSE]
  } else {
    tab <- tab[, c(base_cols[seq_len(insert_after)], layer_cols, base_cols[-seq_len(insert_after)]), drop = FALSE]
  }
  tab
}

#' Safely extract variable info from a mass_dataset object
#'
#' @noRd
metminer_safe_extract_variable_info <- function(object) {
  tryCatch(
    massdataset::extract_variable_info(object),
    error = function(e) {
      if (!is.null(object@variable_info)) {
        return(as.data.frame(object@variable_info, stringsAsFactors = FALSE))
      }
      data.frame()
    }
  )
}

#' Safely extract expression data from a mass_dataset object
#'
#' @noRd
metminer_safe_extract_expression_data <- function(object) {
  tryCatch(
    massdataset::extract_expression_data(object),
    error = function(e) {
      if (!is.null(object@expression_data)) {
        return(object@expression_data)
      }
      NULL
    }
  )
}

# ---- Annotation status reporting ----

#' Summarize annotation levels for a mass_dataset object
#'
#' @noRd
metminer_annotation_status <- function(object, databases = character()) {
  if (is.null(object)) {
    return(list(
      total_features = 0, level1 = 0, level2 = 0, level3 = 0,
      annotated = 0, unannotated = 0,
      databases = databases, table = data.frame(), sample_filter = NULL
    ))
  }

  variable_info <- massdataset::extract_variable_info(object)
  total_features <- nrow(variable_info)
  annotation_table <- metminer_safe_extract_annotation_table(object)
  sample_filter <- metminer_get_annotation_sample_filter(object)

  if (is.null(annotation_table) || nrow(annotation_table) == 0 || !"Level" %in% colnames(annotation_table)) {
    return(list(
      total_features = total_features, level1 = 0, level2 = 0, level3 = 0,
      annotated = 0, unannotated = total_features,
      databases = databases, table = annotation_table %||% data.frame(),
      sample_filter = sample_filter
    ))
  }

  levels <- annotation_table[, c("variable_id", "Level")]
  levels <- levels[!is.na(levels$variable_id) & !is.na(levels$Level), , drop = FALSE]
  levels$Level <- as.integer(levels$Level)
  if (nrow(levels) == 0) {
    return(list(
      total_features = total_features, level1 = 0, level2 = 0, level3 = 0,
      annotated = 0, unannotated = total_features,
      databases = databases, table = annotation_table,
      sample_filter = sample_filter
    ))
  }

  best_levels <- stats::aggregate(Level ~ variable_id, data = levels, FUN = min)
  best_levels <- best_levels[best_levels$Level %in% 1:3, , drop = FALSE]

  level_counts <- table(factor(best_levels$Level, levels = 1:3))
  annotated <- nrow(best_levels)

  list(
    total_features = total_features,
    level1 = unname(level_counts[["1"]]),
    level2 = unname(level_counts[["2"]]),
    level3 = unname(level_counts[["3"]]),
    annotated = annotated,
    unannotated = max(total_features - annotated, 0),
    databases = databases,
    table = annotation_table,
    sample_filter = sample_filter
  )
}

#' Format annotation status for Shiny
#'
#' @noRd
metminer_format_annotation_status <- function(status, mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  database_text <- if (length(status$databases) > 0) {
    paste(status$databases, collapse = ", ")
  } else {
    "None"
  }
  sample_filter_text <- ""
  if (!is.null(status$sample_filter) && isTRUE(status$sample_filter$applied)) {
    sample_filter_text <- paste0(
      "\nAnnotation sample filter: removed ",
      status$sample_filter$removed_qc_count %||% length(status$sample_filter$removed_qc_ids %||% character()),
      " QC sample(s) before annotation (",
      status$sample_filter$samples_before %||% NA_integer_,
      " -> ",
      status$sample_filter$samples_after %||% NA_integer_,
      ")."
    )
  }

  paste0(
    "-- Metabolite Annotation (", tools::toTitleCase(mode), " Mode) --\n\n",
    "Databases: ", database_text, "\n",
    "Total features: ", status$total_features, "\n",
    "Level 1 features: ", status$level1, "\n",
    "Level 2 features: ", status$level2, "\n",
    "Level 3 features: ", status$level3, "\n",
    "Annotated features (Level 1-3): ", status$annotated, "\n",
    "Unannotated features: ", status$unannotated,
    sample_filter_text
  )
}

#' Validation status summary for the feature-annotation module
#'
#' @noRd
metminer_annotation_validation_status <- function(annotated_object, network_object, mode) {
  obj <- annotated_object %||% network_object
  if (is.null(obj)) {
    return(sprintf("No %s mode network/annotation object available.", mode))
  }
  network <- extract_feature_network(obj)
  validation <- metminer_extract_annotation_validation(obj)
  roles <- validation$feature_role_interpretation
  paste0(
    "Features in network: ", length(unique(c(network$from, network$to))), "\n",
    "Network edges: ", nrow(network), "\n",
    "Validated sub-networks: ", if (!is.null(roles)) length(unique(roles$sub_network)) else 0, "\n",
    "Annotated object: ", if (!is.null(annotated_object)) "yes" else "no"
  )
}

# ---- Annotation validation storage ----

#' Store annotation validation results on a mass_dataset object
#'
#' @noRd
metminer_set_annotation_validation <- function(object, annotation_validation) {
  object@other_files$annotation_validation <- annotation_validation
  object
}

#' Extract annotation validation results from a mass_dataset object
#'
#' @noRd
metminer_extract_annotation_validation <- function(object) {
  validation <- object@other_files$annotation_validation
  if (is.null(validation)) {
    return(list(
      candidate_validation = data.frame(),
      feature_selection = data.frame(),
      edge_validation = data.frame(),
      subnetwork_summary = data.frame(),
      feature_role_interpretation = data.frame(),
      subnetwork_hypothesis = data.frame()
    ))
  }
  validation
}

# ---- Ontology DAG ----

#' Build a graph-friendly ontology DAG for one annotation sub-network
#'
#' @noRd
metminer_build_annotation_ontology_graph <- function(object, sub_network = NULL) {
  validation <- metminer_extract_annotation_validation(object)
  roles <- validation$feature_role_interpretation
  network <- extract_feature_network(object)

  if (is.null(roles) || nrow(roles) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }

  if (is.null(sub_network) || !has_text(sub_network) || identical(as.character(sub_network), "all")) {
    choices <- metminer_annotation_ontology_subnetwork_choices(object)
    sub_network <- if (length(choices) > 0) unname(choices[1]) else roles$sub_network[1]
  }
  sub_network <- suppressWarnings(as.integer(sub_network))
  roles <- roles[roles$sub_network == sub_network, , drop = FALSE]
  if (nrow(roles) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }

  feature_ids <- roles$feature_id
  parent_id <- roles$parent_feature_id[1]
  subnet_network <- network[network$from %in% feature_ids & network$to %in% feature_ids, , drop = FALSE]

  nodes <- data.frame(
    id = roles$feature_id,
    label = ontology_node_label(roles),
    title = ontology_node_title(roles),
    group = ontology_node_group(roles$network_role),
    level = ifelse(roles$feature_id == parent_id, 0, 1),
    shape = ifelse(roles$feature_id == parent_id, "box", "ellipse"),
    stringsAsFactors = FALSE
  )

  edges <- ontology_edges_from_roles(roles, subnet_network)
  list(nodes = nodes, edges = edges)
}

#' Sub-network choices for ontology DAG display
#'
#' @noRd
metminer_annotation_ontology_subnetwork_choices <- function(object, preferred_min_features = 3) {
  validation <- metminer_extract_annotation_validation(object)
  hypothesis <- validation$subnetwork_hypothesis

  if (is.null(hypothesis) || nrow(hypothesis) == 0) {
    return(character())
  }

  hypothesis$has_annotation <- has_text(hypothesis$putative_real_compound)
  hypothesis$display_priority <- dplyr::case_when(
    hypothesis$feature_count >= preferred_min_features & hypothesis$has_annotation ~ 1,
    hypothesis$feature_count >= 2 & hypothesis$has_annotation ~ 2,
    hypothesis$feature_count >= 2 ~ 3,
    TRUE ~ 4
  )

  hypothesis <- hypothesis[order(
    hypothesis$display_priority,
    -hypothesis$feature_count,
    hypothesis$annotation_conflicts,
    hypothesis$sub_network,
    na.last = TRUE
  ), , drop = FALSE]

  labels <- vapply(seq_len(nrow(hypothesis)), function(i) {
    sid <- hypothesis$sub_network[i]
    compound <- hypothesis$putative_real_compound[i]
    compound <- if (has_text(compound)) as.character(compound) else "unannotated"
    relation_hint <- paste0(
      "iso=", hypothesis$isotope_features[i],
      ", add=", hypothesis$adduct_features[i],
      ", isf=", hypothesis$isf_features[i]
    )
    sprintf("Sub-network %s | n=%s | %s | %s", sid, hypothesis$feature_count[i], compound, relation_hint)
  }, character(1))

  choices <- as.character(hypothesis$sub_network)
  names(choices) <- labels
  choices
}

# ---- Ontology DAG internal helpers ----

ontology_node_label <- function(roles) {
  compound <- coerce_text(roles$selected_compound, "unannotated")
  paste0(roles$feature_id, "\n", compound)
}

ontology_node_title <- function(roles) {
  selected_compound <- coerce_text(roles$selected_compound, "unannotated")
  selected_adduct <- coerce_text(roles$selected_adduct, "NA")
  paste0(
    "<b>", roles$feature_id, "</b>",
    "<br>m/z: ", roles$mz,
    "<br>RT: ", roles$rt,
    "<br>Role: ", roles$network_role,
    "<br>Annotation: ", selected_compound,
    "<br>Adduct: ", selected_adduct,
    "<br>Level: ", ifelse(is.na(roles$metid_level), "NA", roles$metid_level),
    "<br>Candidate rank: ", ifelse(is.na(roles$metid_rank), "NA", roles$metid_rank),
    "<br>Total score: ", ifelse(is.na(roles$metid_total_score), "NA", roles$metid_total_score),
    "<br>Network score: ", ifelse(is.na(roles$network_final_score), "NA", roles$network_final_score),
    "<br>Interpretation: ", roles$annotation_interpretation
  )
}

ontology_node_group <- function(role) {
  dplyr::case_when(
    role == "putative_parent" ~ "putative_parent",
    grepl("isotope", role) ~ "isotope",
    grepl("adduct", role) ~ "adduct",
    grepl("isf|fragment", role, ignore.case = TRUE) ~ "isf",
    grepl("cross_polarity", role) ~ "cross_polarity",
    TRUE ~ "neighbor"
  )
}

ontology_edges_from_roles <- function(roles, subnet_network) {
  rows <- lapply(seq_len(nrow(roles)), function(i) {
    role <- roles$network_role[i]
    feature_id <- roles$feature_id[i]
    parent_id <- roles$parent_feature_id[i]
    if (identical(feature_id, parent_id)) {
      return(NULL)
    }

    edge <- subnet_network[
      (subnet_network$from == parent_id & subnet_network$to == feature_id) |
        (subnet_network$from == feature_id & subnet_network$to == parent_id),
      , drop = FALSE
    ]
    edge <- if (nrow(edge) > 0) edge[order(edge_role_priority(edge$type)), , drop = FALSE][1, ] else NULL

    label <- ontology_edge_label(role, edge)
    data.frame(
      from = feature_id,
      to = parent_id,
      label = label,
      arrows = "to",
      title = if (!is.null(edge)) paste(edge$type, edge$annotation, roles$edge_evidence[i], sep = "<br>") else roles$edge_evidence[i],
      dashes = role %in% c("network_neighbor", "possible_parent_of_selected_parent", "possible_cross_polarity_parent"),
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(from = character(), to = character(), label = character(), arrows = character(), title = character()))
  }
  do.call(rbind, rows)
}

ontology_edge_label <- function(role, edge) {
  annotation <- if (!is.null(edge) && has_text(edge$annotation)) paste0(" (", as.character(edge$annotation), ")") else ""
  if (role == "isotope_of_parent") return(paste0("is isotope of", annotation))
  if (role == "adduct_of_parent") return(paste0("is adduct of", annotation))
  if (role == "isf_fragment_of_parent") return(paste0("is ISF of", annotation))
  if (role == "cross_polarity_counterpart") return(paste0("is cross-polarity form of", annotation))
  if (role == "cross_polarity_isf_fragment") return(paste0("is cross-polarity ISF of", annotation))
  if (role == "parent_isotope_or_reverse_isotope_edge") return(paste0("has isotope relation with", annotation))
  if (role == "possible_parent_of_selected_parent") return(paste0("possible reverse ISF", annotation))
  "is connected to"
}

# ---- Feature-network assisted annotation validation ----

#' Validate topN annotation candidates with the stored feature network
#'
#' @noRd
metminer_validate_annotations_with_feature_network <- function(object) {
  if (is.null(object)) {
    return(NULL)
  }

  # Enhance the network with annotation-informed edges before validation
  object <- metminer_enhance_network_with_annotations(object)

  annotation_table <- metminer_safe_extract_annotation_table(object)
  network <- extract_feature_network(object)
  variable_info <- metminer_safe_extract_variable_info(object)
  expression_data <- metminer_safe_extract_expression_data(object)

  validation <- metminer_build_annotation_network_validation(annotation_table, network, variable_info, expression_data)
  metminer_set_annotation_validation(object, validation)
}

#' Enhance the feature network with edges derived from shared compound annotations
#'
#' For features that share a compound_key (same annotated metabolite by metid)
#' but are NOT yet connected in the existing network, this function checks
#' whether their mass difference matches a known neutral-loss, adduct pair, or
#' isotope pattern. If it does, an edge is added with `evidence_level =
#' "Annotation"` so downstream tools (ontology DAG, sub-network grouping,
#' redundancy removal) can treat them as related.
#'
#' @noRd
metminer_enhance_network_with_annotations <- function(object,
                                                      mz_ppm = 10,
                                                      rt_tolerance = 10,
                                                      ms2_mz_tol_ppm = 5,
                                                      ms2_rt_tol = 10,
                                                      ms2_fragment_mz_tol = 0.02,
                                                      ms2_similarity_cutoff = 0.7,
                                                      ms2_matched_ratio_cutoff = 0.6,
                                                      min_ms2_matched_peaks = 3) {
  annotation_table <- metminer_safe_extract_annotation_table(object)
  if (nrow(annotation_table) == 0) return(object)

  network <- extract_feature_network(object)
  variable_info <- metminer_safe_extract_variable_info(object)
  if (nrow(variable_info) == 0) return(object)

  # Get best candidate per feature (one compound_key per feature)
  best <- metminer_prepare_annotation_candidates(annotation_table)
  if (nrow(best) == 0) return(object)
  best <- best[has_text(best$compound_key), , drop = FALSE]
  if (nrow(best) < 2) return(object)

  # Build mz/rt lookup from variable_info
  vi <- as.data.frame(variable_info, stringsAsFactors = FALSE)
  vi$variable_id <- as.character(vi$variable_id)
  mz_lookup <- stats::setNames(vi$mz, vi$variable_id)
  rt_lookup <- stats::setNames(vi$rt, vi$variable_id)
  expression_data <- metminer_safe_extract_expression_data(object)
  mean_area_lookup <- stats::setNames(rep(NA_real_, nrow(vi)), vi$variable_id)
  if (!is.null(expression_data)) {
    expr <- as.matrix(expression_data)
    mean_area <- rowMeans(expr, na.rm = TRUE)
    mean_area_lookup[names(mean_area)] <- as.numeric(mean_area)
  }

  # Determine which features are already connected in the network
  network <- metminer_prune_cross_rt_annotation_edges(network, rt_tolerance = rt_tolerance)
  existing_pairs <- if (nrow(network) > 0) {
    unique(paste(pmin(network$from, network$to), pmax(network$from, network$to), sep = "::"))
  } else character()

  # Load NL table
  nl_table <- default_plant_neutral_loss_table()
  nl_masses <- nl_table$mass[nl_table$use_in_isf & is.finite(nl_table$mass)]
  nl_labels <- nl_table$annotation[nl_table$use_in_isf & is.finite(nl_table$mass)]

  ms2_index <- metminer_annotation_ms2_index(
    object = object,
    variable_info = vi,
    mz_tol_ppm = ms2_mz_tol_ppm,
    rt_tol_sec = ms2_rt_tol
  )

  # Group features by compound_key
  by_key <- split(seq_len(nrow(best)), best$compound_key)
  new_edges <- list()
  edge_idx <- 1L

  for (idx in by_key) {
    if (length(idx) < 2) next
    members <- best[idx, , drop = FALSE]
    mids <- members$variable_id

    for (i in seq_along(mids)) {
      for (j in seq_along(mids)) {
        if (j <= i) next
        fi <- mids[i]
        fj <- mids[j]

        # Skip if already connected
        pair_key <- paste(pmin(fi, fj), pmax(fi, fj), sep = "::")
        if (pair_key %in% existing_pairs) next

        mzi <- mz_lookup[fi]
        mzj <- mz_lookup[fj]
        rti <- rt_lookup[fi]
        rtj <- rt_lookup[fj]

        if (!is.finite(mzi) || !is.finite(mzj)) next
        delta <- abs(mzi - mzj)
        rt_diff <- if (is.finite(rti) && is.finite(rtj)) abs(rti - rtj) else NA_real_
        if (!is.finite(rt_diff) || rt_diff > rt_tolerance) {
          next
        }

        if (delta == 0) next
        rel <- classify_mass_difference(delta, mzi, mzj, nl_masses, nl_labels, mz_ppm)
        if (is.null(rel)) next

        # Determine edge type and direction (parent = higher m/z for ISF)
        parent <- if (mzi >= mzj) fi else fj
        child  <- if (mzi >= mzj) fj else fi
        edge_type <- if (grepl("^NL:", rel)) "ISF"
                     else if (grepl("^isotope:", rel)) "Isotope"
                     else if (grepl("^adduct:", rel)) "Adduct"
                     else "ISF"

        new_edges[[edge_idx]] <- data.frame(
          from = parent,
          to = child,
          type = edge_type,
          annotation = rel,
          confidence = 0.3,
          mz_error_ppm = 0,
          rt_diff = rt_diff,
          abundance_cor = NA_real_,
          qc_ratio_rsd = NA_real_,
          evidence_level = "Annotation",
          evidence = "same_compound_annotation",
          stringsAsFactors = FALSE
        )
        edge_idx <- edge_idx + 1L
        existing_pairs <- c(existing_pairs, pair_key)
      }
    }
  }

  if (length(new_edges) == 0) return(object)

  enhanced_network <- dplyr::bind_rows(network, dplyr::bind_rows(new_edges))
  enhanced_network <- normalize_feature_network(enhanced_network)
  set_feature_network(object, enhanced_network)
}

metminer_prune_cross_rt_annotation_edges <- function(network, rt_tolerance = 10) {
  network <- normalize_feature_network(network)
  if (nrow(network) == 0) {
    return(network)
  }

  annotation_edge <- grepl("same_compound_annotation|same_mz_cross_rt", network$evidence %||% "", ignore.case = TRUE) |
    grepl("same-m/z same-annotation spectral redundancy", network$annotation %||% "", ignore.case = TRUE)
  cross_rt <- is.finite(network$rt_diff) & network$rt_diff > rt_tolerance
  network[!(annotation_edge & cross_rt), , drop = FALSE]
}

#' Classify mass difference as NL, adduct, or isotope
#'
#' @return NULL if no match, or a short label like "NL:H2O"
#' @noRd
classify_mass_difference <- function(delta, parent_mz, child_mz,
                                     nl_masses, nl_labels, mz_ppm) {
  # Check neutral losses
  for (k in seq_along(nl_masses)) {
    if (abs(delta - nl_masses[k]) <= max(nl_masses[k] * mz_ppm / 1e6, 0.002)) {
      return(paste0("NL:", nl_labels[k]))
    }
  }

  # Check isotope patterns
  if (abs(delta - 1.00328) <= max(1.00328 * mz_ppm / 1e6, 0.001)) {
    return("isotope:13C1")
  }
  if (abs(delta - 2.00656) <= max(2.00656 * mz_ppm / 1e6, 0.002)) {
    return("isotope:13C2/34S")
  }

  # Check common adduct mass differences (within-mode)
  pos_adducts <- c("[M+Na]+ - [M+H]+" = 21.981942,
                   "[M+K]+ - [M+H]+"  = 37.955882,
                   "[M+NH4]+ - [M+H]+" = 17.026547,
                   "[M+H]+ - [M-H2O+H]+" = 18.010565)
  for (k in seq_along(pos_adducts)) {
    if (abs(delta - pos_adducts[k]) <= max(pos_adducts[k] * mz_ppm / 1e6, 0.002)) {
      return(paste0("adduct:", names(pos_adducts)[k]))
    }
  }

  neg_adducts <- c("[M+Cl]- - [M-H]-" = 35.976678,
                   "[M+FA-H]- - [M-H]-" = 46.005477,
                   "[M+Hac-H]- - [M-H]-" = 60.021127)
  for (k in seq_along(neg_adducts)) {
    if (abs(delta - neg_adducts[k]) <= max(neg_adducts[k] * mz_ppm / 1e6, 0.002)) {
      return(paste0("adduct:", names(neg_adducts)[k]))
    }
  }

  NULL
}

metminer_annotation_ms2_index <- function(object, variable_info, mz_tol_ppm, rt_tol_sec) {
  if (!exists("prepare_ms2_feature_index", mode = "function")) {
    return(list(meta = data.frame(), spectra = list()))
  }
  tryCatch(
    prepare_ms2_feature_index(
      object = object,
      variable_info = variable_info,
      mz_tol_ppm = mz_tol_ppm,
      rt_tol_sec = rt_tol_sec
    ),
    error = function(e) list(meta = data.frame(), spectra = list())
  )
}

metminer_ms2_meta_row <- function(meta, feature_id) {
  if (is.null(meta) || nrow(meta) == 0) {
    return(meta[0, , drop = FALSE])
  }
  if (feature_id %in% rownames(meta)) {
    return(meta[feature_id, , drop = FALSE])
  }
  meta[0, , drop = FALSE]
}

metminer_same_annotation_spectral_redundancy <- function(feature_a,
                                                         feature_b,
                                                         ms2_index,
                                                         mz_tol,
                                                         score_cutoff,
                                                         ratio_cutoff,
                                                         min_matched_peaks) {
  if (is.null(ms2_index) || length(ms2_index$spectra) == 0) {
    return(NULL)
  }
  spectrum_a <- ms2_index$spectra[[feature_a]]
  spectrum_b <- ms2_index$spectra[[feature_b]]
  if (is.null(spectrum_a) || is.null(spectrum_b)) {
    return(NULL)
  }

  sim_ab <- reverse_ms2_similarity(spectrum_a, spectrum_b, mz_tol = mz_tol)
  sim_ba <- reverse_ms2_similarity(spectrum_b, spectrum_a, mz_tol = mz_tol)
  score <- max(c(sim_ab$score, sim_ba$score), na.rm = TRUE)
  matched_peaks <- max(c(sim_ab$matched_peaks, sim_ba$matched_peaks), na.rm = TRUE)
  matched_ratio <- max(c(sim_ab$matched_ratio, sim_ba$matched_ratio), na.rm = TRUE)
  if (!is.finite(score) || !is.finite(matched_ratio) || !is.finite(matched_peaks)) {
    return(NULL)
  }

  supported <- matched_peaks >= min_matched_peaks &&
    (score >= score_cutoff || matched_ratio >= ratio_cutoff)
  if (!supported) {
    return(NULL)
  }

  meta_a <- metminer_ms2_meta_row(ms2_index$meta, feature_a)
  meta_b <- metminer_ms2_meta_row(ms2_index$meta, feature_b)
  same_spectrum <- FALSE
  if (nrow(meta_a) > 0 && nrow(meta_b) > 0) {
    same_spectrum <- identical(
      paste(meta_a$ms2_set, meta_a$ms2_file, meta_a$ms2_spectrum_id, sep = "||"),
      paste(meta_b$ms2_set, meta_b$ms2_file, meta_b$ms2_spectrum_id, sep = "||")
    )
  }
  if (isTRUE(same_spectrum)) {
    return(NULL)
  }

  list(
    score = as.numeric(score),
    matched_peaks = as.integer(matched_peaks),
    matched_ratio = as.numeric(matched_ratio),
    same_spectrum = same_spectrum
  )
}

#' Build candidate-level annotation/network validation tables
#'
#' @noRd
metminer_build_annotation_network_validation <- function(annotation_table, network,
                                                         variable_info = NULL,
                                                         expression_data = NULL) {
  candidates <- metminer_prepare_annotation_candidates(annotation_table)
  network <- normalize_feature_network(network)
  feature_info <- prepare_annotation_feature_info(variable_info, expression_data)

  if (nrow(candidates) == 0) {
    empty <- data.frame()
    return(list(
      candidate_validation = empty,
      feature_selection = empty,
      edge_validation = empty,
      subnetwork_summary = empty,
      feature_role_interpretation = empty,
      subnetwork_hypothesis = empty
    ))
  }

  membership <- annotation_network_membership(network, candidates$variable_id)
  candidates$sub_network <- unname(membership[candidates$variable_id])
  candidates$same_compound_votes <- count_same_compound_votes(candidates)
  candidates$support_edges <- 0L
  candidates$conflict_edges <- 0L
  candidates$unknown_edges <- 0L

  edge_validation <- validate_annotation_edges(network, candidates)
  if (nrow(edge_validation) > 0) {
    candidates <- add_edge_votes_to_candidates(candidates, edge_validation)
  }

  candidates$metid_score_norm <- normalize_annotation_score(candidates$Total.score)
  candidates$network_support_score <- pmin(
    1,
    (candidates$same_compound_votes * 0.35) +
      (candidates$support_edges * 0.45)
  )
  candidates$network_conflict_score <- pmin(1, candidates$conflict_edges * 0.35)
  candidates$final_score <- round(
    (candidates$metid_score_norm * 0.55) +
      (candidates$network_support_score * 0.45) -
      (candidates$network_conflict_score * 0.25),
    4
  )
  candidates$candidate_status <- classify_candidate_status(candidates)

  feature_selection <- metminer_select_network_refined_candidates(candidates)
  subnetwork_summary <- summarize_annotation_subnetworks(candidates, feature_selection, edge_validation)
  network_interpretation <- interpret_annotation_subnetworks(
    candidates = candidates,
    feature_selection = feature_selection,
    edge_validation = edge_validation,
    network = network,
    feature_info = feature_info
  )

  list(
    candidate_validation = candidates,
    feature_selection = feature_selection,
    edge_validation = edge_validation,
    subnetwork_summary = subnetwork_summary,
    feature_role_interpretation = network_interpretation$feature_role_interpretation,
    subnetwork_hypothesis = network_interpretation$subnetwork_hypothesis
  )
}

# ---- Candidate preparation ----

prepare_annotation_feature_info <- function(variable_info = NULL, expression_data = NULL) {
  if (is.null(variable_info) || nrow(variable_info) == 0) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(), mean_area = numeric()))
  }

  info <- as.data.frame(variable_info, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(info)) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(), mean_area = numeric()))
  }
  info$variable_id <- as.character(info$variable_id)

  for (col in c("mz", "rt")) {
    if (!col %in% colnames(info)) {
      info[[col]] <- NA_real_
    }
  }

  info$mean_area <- NA_real_
  if (!is.null(expression_data)) {
    expr <- as.matrix(expression_data)
    mean_area <- rowMeans(expr, na.rm = TRUE)
    info$mean_area <- as.numeric(mean_area[match(info$variable_id, names(mean_area))])
  }

  info[, c("variable_id", "mz", "rt", "mean_area"), drop = FALSE]
}

#' Prepare annotation candidates table from raw annotation_table
#'
#' @noRd
metminer_prepare_annotation_candidates <- function(annotation_table) {
  if (is.null(annotation_table) || nrow(annotation_table) == 0) {
    return(data.frame())
  }
  candidates <- as.data.frame(annotation_table, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(candidates)) {
    return(data.frame())
  }
  if (!"Total.score" %in% colnames(candidates)) {
    candidates$Total.score <- NA_real_
  }
  if (!"Level" %in% colnames(candidates)) {
    candidates$Level <- NA_integer_
  }
  if (!"Adduct" %in% colnames(candidates)) {
    candidates$Adduct <- NA_character_
  }

  candidates <- metminer_standardize_review_annotation_cols(candidates)
  if (!"annotation_layer" %in% colnames(candidates) || all(!has_text(candidates$annotation_layer))) {
    plantcyc_id <- candidates$PlantCyc.ID
    plantcyc_id[!has_text(plantcyc_id) & has_text(candidates$BIOCYC.ID)] <-
      candidates$BIOCYC.ID[!has_text(plantcyc_id) & has_text(candidates$BIOCYC.ID)]
    candidates$annotation_layer <- metminer_annotation_evidence_layer(
      database = candidates$Database,
      kegg_id = candidates$KEGG.ID,
      plantcyc_id = plantcyc_id,
      lab_id = candidates$Lab.ID
    )
  }
  if (!"evidence_scope" %in% colnames(candidates) || all(!has_text(candidates$evidence_scope))) {
    candidates$evidence_scope <- metminer_annotation_evidence_scope(candidates$annotation_layer)
  }
  candidates <- metminer_fill_same_compound_annotation_ids(candidates)

  candidates$variable_id <- as.character(candidates$variable_id)
  candidates$Level <- suppressWarnings(as.integer(candidates$Level))
  candidates$Total.score <- suppressWarnings(as.numeric(candidates$Total.score))

  candidates <- candidates[order(
    candidates$variable_id,
    candidates$Level,
    -candidates$Total.score,
    na.last = TRUE
  ), , drop = FALSE]
  candidates$candidate_rank <- ave(
    seq_len(nrow(candidates)),
    candidates$variable_id,
    FUN = seq_along
  )
  candidates$candidate_id <- paste(candidates$variable_id, candidates$candidate_rank, sep = "::cand")
  candidates$compound_key <- make_annotation_compound_key(candidates)
  candidates
}

make_annotation_compound_key <- function(candidates) {
  id_cols <- c("HMDB.ID", "KEGG.ID", "CAS.ID", "Lab.ID")
  key <- rep(NA_character_, nrow(candidates))

  for (col in id_cols) {
    if (!col %in% colnames(candidates)) {
      next
    }
    val <- trimws(as.character(candidates[[col]]))
    ok <- is.na(key) & has_text(val)
    key[ok] <- paste(col, val[ok], sep = ":")
  }

  if ("Compound.name" %in% colnames(candidates)) {
    val <- tolower(trimws(as.character(candidates$Compound.name)))
    ok <- is.na(key) & has_text(val)
    key[ok] <- paste("name", val[ok], sep = ":")
  }

  key
}

# ---- Network membership & voting ----

annotation_network_membership <- function(network, feature_ids) {
  feature_ids <- unique(as.character(feature_ids))
  if (nrow(network) == 0) {
    # Without edges, each feature is its own single-node component
    out <- seq_along(feature_ids)
    names(out) <- feature_ids
    return(out)
  }

  edge_ids <- unique(c(network$from, network$to))
  all_ids <- unique(c(edge_ids, feature_ids))
  graph <- igraph::graph_from_data_frame(
    network[, c("from", "to"), drop = FALSE],
    directed = FALSE,
    vertices = data.frame(name = all_ids)
  )
  membership <- igraph::components(graph)$membership
  membership[feature_ids]
}

count_same_compound_votes <- function(candidates) {
  votes <- integer(nrow(candidates))
  candidates$compound_key <- as.character(candidates$compound_key)
  valid <- has_text(candidates$compound_key)
  if (!any(valid)) {
    return(votes)
  }

  split_idx <- split(which(valid), paste(candidates$sub_network[valid], candidates$compound_key[valid], sep = "::"))
  for (idx in split_idx) {
    feature_n <- length(unique(candidates$variable_id[idx]))
    if (feature_n > 1) {
      votes[idx] <- feature_n - 1L
    }
  }
  votes
}

# ---- Edge validation ----

validate_annotation_edges <- function(network, candidates) {
  if (nrow(network) == 0 || nrow(candidates) == 0) {
    return(data.frame())
  }

  by_feature <- split(candidates, candidates$variable_id)
  rows <- vector("list", nrow(network))
  same_compound_edge_types <- c("isotope", "adduct", "Cross-polarity")

  for (i in seq_len(nrow(network))) {
    edge <- network[i, , drop = FALSE]
    from_candidates <- by_feature[[edge$from]]
    to_candidates <- by_feature[[edge$to]]

    if (is.null(from_candidates) || is.null(to_candidates)) {
      rows[[i]] <- data.frame(
        from = edge$from, to = edge$to,
        type = edge$type, annotation = edge$annotation,
        consistency = "unknown",
        support_from_candidates = "", support_to_candidates = "",
        conflict_from_candidates = "", conflict_to_candidates = "",
        reason = "one or both features have no annotation candidates",
        stringsAsFactors = FALSE
      )
      next
    }

    same_pairs <- annotation_same_compound_pairs(from_candidates, to_candidates)
    require_same <- edge$type %in% same_compound_edge_types

    if (nrow(same_pairs) > 0) {
      consistency <- "supported"
      reason <- "edge endpoints share at least one compound candidate"
      support_from <- unique(same_pairs$from_candidate_id)
      support_to <- unique(same_pairs$to_candidate_id)
      conflict_from <- character()
      conflict_to <- character()
    } else if (require_same) {
      consistency <- "conflict"
      reason <- "edge type expects the same neutral compound, but no candidates match"
      support_from <- character()
      support_to <- character()
      conflict_from <- from_candidates$candidate_id
      conflict_to <- to_candidates$candidate_id
    } else {
      consistency <- "compatible"
      reason <- "edge type does not require both endpoints to share a compound candidate"
      support_from <- character()
      support_to <- character()
      conflict_from <- character()
      conflict_to <- character()
    }

    rows[[i]] <- data.frame(
      from = edge$from, to = edge$to,
      type = edge$type, annotation = edge$annotation,
      consistency = consistency,
      support_from_candidates = paste(support_from, collapse = ";"),
      support_to_candidates = paste(support_to, collapse = ";"),
      conflict_from_candidates = paste(conflict_from, collapse = ";"),
      conflict_to_candidates = paste(conflict_to, collapse = ";"),
      reason = reason,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

annotation_same_compound_pairs <- function(from_candidates, to_candidates) {
  from_candidates$compound_key <- as.character(from_candidates$compound_key)
  to_candidates$compound_key <- as.character(to_candidates$compound_key)
  from_candidates <- from_candidates[has_text(from_candidates$compound_key), , drop = FALSE]
  to_candidates <- to_candidates[has_text(to_candidates$compound_key), , drop = FALSE]
  if (nrow(from_candidates) == 0 || nrow(to_candidates) == 0) {
    return(data.frame())
  }

  pairs <- merge(
    from_candidates[, c("candidate_id", "compound_key"), drop = FALSE],
    to_candidates[, c("candidate_id", "compound_key"), drop = FALSE],
    by = "compound_key",
    suffixes = c("_from", "_to")
  )
  if (nrow(pairs) == 0) {
    return(data.frame())
  }

  data.frame(
    compound_key = pairs$compound_key,
    from_candidate_id = pairs$candidate_id_from,
    to_candidate_id = pairs$candidate_id_to,
    stringsAsFactors = FALSE
  )
}

add_edge_votes_to_candidates <- function(candidates, edge_validation) {
  add_votes <- function(ids, column) {
    ids <- unlist(strsplit(paste(ids, collapse = ";"), ";", fixed = TRUE), use.names = FALSE)
    ids <- ids[has_text(ids)]
    if (length(ids) == 0) {
      return()
    }
    tab <- table(ids)
    idx <- match(names(tab), candidates$candidate_id)
    idx <- idx[!is.na(idx)]
    candidates[[column]][idx] <<- candidates[[column]][idx] + as.integer(tab[names(tab) %in% candidates$candidate_id])
  }

  add_votes(c(edge_validation$support_from_candidates, edge_validation$support_to_candidates), "support_edges")
  add_votes(c(edge_validation$conflict_from_candidates, edge_validation$conflict_to_candidates), "conflict_edges")

  unknown_features <- unique(c(
    edge_validation$from[edge_validation$consistency == "unknown"],
    edge_validation$to[edge_validation$consistency == "unknown"]
  ))
  candidates$unknown_edges[candidates$variable_id %in% unknown_features] <-
    candidates$unknown_edges[candidates$variable_id %in% unknown_features] + 1L

  candidates
}

# ---- Scoring ----

normalize_annotation_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  x[is.na(x)] <- min(x, na.rm = TRUE)
  rng <- range(x, finite = TRUE)
  if (!is.finite(rng[1]) || diff(rng) == 0) {
    return(ifelse(x > 0, pmin(x, 1), 0))
  }
  (x - rng[1]) / diff(rng)
}

classify_candidate_status <- function(candidates) {
  out <- rep("network_ambiguous", nrow(candidates))
  out[candidates$support_edges > 0 | candidates$same_compound_votes > 0] <- "network_supported"
  out[candidates$conflict_edges > 0 & candidates$support_edges == 0 & candidates$same_compound_votes == 0] <- "network_conflict"
  out[candidates$support_edges == 0 & candidates$conflict_edges == 0 & candidates$same_compound_votes == 0] <- "no_network_evidence"
  out
}

# ---- Candidate selection ----

#' Select best candidate per feature after network refinement
#'
#' @noRd
metminer_select_network_refined_candidates <- function(candidates, use_network = TRUE) {
  if (!isTRUE(use_network) || all(candidates$final_score == candidates$metid_score_norm, na.rm = TRUE)) {
    # No network scores were applied — fall back to metid ranking
    selected_idx <- vapply(split(seq_len(nrow(candidates)), candidates$variable_id), function(idx) {
      idx[order(
        metminer_annotation_layer_priority(candidates$annotation_layer[idx]),
        candidates$Level[idx],
        -candidates$Total.score[idx],
        candidates$candidate_rank[idx],
        na.last = TRUE
      )[1]]
    }, integer(1))
  } else {
    selected_idx <- vapply(split(seq_len(nrow(candidates)), candidates$variable_id), function(idx) {
      idx[order(
        -candidates$final_score[idx],
        metminer_annotation_layer_priority(candidates$annotation_layer[idx]),
        candidates$Level[idx],
        -candidates$Total.score[idx],
        candidates$candidate_rank[idx],
        na.last = TRUE
      )[1]]
    }, integer(1))
  }

  selected <- candidates[selected_idx, , drop = FALSE]
  selected$selection_status <- ifelse(
    selected$candidate_rank == 1,
    "top1_retained",
    "reranked_by_network"
  )
  rownames(selected) <- NULL
  selected
}

# ---- Subnetwork summarization ----

summarize_annotation_subnetworks <- function(candidates, feature_selection, edge_validation) {
  if (nrow(candidates) == 0) {
    return(data.frame())
  }

  subnet_ids <- sort(unique(candidates$sub_network))
  rows <- lapply(subnet_ids, function(sid) {
    cand <- candidates[candidates$sub_network == sid, , drop = FALSE]
    sel <- feature_selection[feature_selection$sub_network == sid, , drop = FALSE]
    edges <- edge_validation[edge_validation$from %in% cand$variable_id | edge_validation$to %in% cand$variable_id, , drop = FALSE]

    supported <- if (nrow(edges) > 0) sum(edges$consistency == "supported") else 0L
    conflict <- if (nrow(edges) > 0) sum(edges$consistency == "conflict") else 0L
    best_idx <- if (nrow(sel) > 0) which.max(sel$final_score) else integer()

    data.frame(
      sub_network = sid,
      feature_count = length(unique(cand$variable_id)),
      candidate_count = nrow(cand),
      selected_annotation_count = nrow(sel),
      level1_selected = sum(sel$Level == 1, na.rm = TRUE),
      level2_selected = sum(sel$Level == 2, na.rm = TRUE),
      level3_selected = sum(sel$Level == 3, na.rm = TRUE),
      supported_edges = supported,
      conflict_edges = conflict,
      reranked_features = sum(sel$selection_status == "reranked_by_network"),
      representative_compound = if (length(best_idx) > 0 && "Compound.name" %in% colnames(sel)) sel$Compound.name[best_idx] else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# ---- Subnetwork interpretation ----

interpret_annotation_subnetworks <- function(candidates, feature_selection, edge_validation, network, feature_info) {
  feature_ids <- if (nrow(network) > 0) unique(c(network$from, network$to)) else unique(candidates$variable_id)
  if (length(feature_ids) == 0) {
    return(list(feature_role_interpretation = data.frame(), subnetwork_hypothesis = data.frame()))
  }

  membership <- annotation_network_membership(network, feature_ids)
  subnet_ids <- sort(unique(unname(membership)))
  role_rows <- list()
  hypothesis_rows <- list()

  selection_by_feature <- split(feature_selection, feature_selection$variable_id)
  info_by_feature <- split(feature_info, feature_info$variable_id)

  for (sid in subnet_ids) {
    ids <- names(membership)[membership == sid]
    subnet_edges <- network[network$from %in% ids | network$to %in% ids, , drop = FALSE]

    # Skip single-feature sub-networks when network is empty (no meaningful
    # parent/role structure to report — those features are handled as singles)
    if (length(ids) == 1 && nrow(network) == 0) {
      next
    }

    parent_id <- choose_subnetwork_parent(ids, subnet_edges, feature_selection, feature_info)
    parent_sel <- selection_by_feature[[parent_id]]
    parent_compound_key <- metminer_scalar_col(parent_sel, "compound_key", NA_character_)
    parent_compound <- metminer_scalar_col(parent_sel, "Compound.name", NA_character_)

    feature_roles <- lapply(ids, function(feature_id) {
      sel <- selection_by_feature[[feature_id]]
      info <- info_by_feature[[feature_id]]
      relation <- describe_feature_relation_to_parent(feature_id, parent_id, subnet_edges)
      interpretation <- interpret_feature_annotation_role(sel, relation$role, parent_compound_key)

      data.frame(
        sub_network = sid,
        rt = round(suppressWarnings(as.numeric(metminer_scalar_col(info, "rt", NA_real_))), 3),
        feature_id = feature_id,
        mz = round(suppressWarnings(as.numeric(metminer_scalar_col(info, "mz", NA_real_))), 5),
        mean_area = round(suppressWarnings(as.numeric(metminer_scalar_col(info, "mean_area", NA_real_))), 3),
        parent_feature_id = parent_id,
        network_role = metminer_scalar_value(relation$role, "network_neighbor"),
        relation_to_parent = metminer_scalar_value(relation$relation, "connected_indirectly"),
        edge_evidence = metminer_scalar_value(relation$edge_evidence, ""),
        selected_compound = metminer_scalar_col(sel, "Compound.name", NA_character_),
        selected_adduct = metminer_scalar_col(sel, "Adduct", NA_character_),
        compound_key = metminer_scalar_col(sel, "compound_key", NA_character_),
        Kingdom = metminer_scalar_col(sel, "Kingdom", NA_character_),
        Super_class = metminer_scalar_col(sel, "Super_class", NA_character_),
        Class = metminer_scalar_col(sel, "Class", NA_character_),
        Sub_class = metminer_scalar_col(sel, "Sub_class", NA_character_),
        direct_parent = metminer_scalar_col(sel, "direct_parent", NA_character_),
        molecular_framework = metminer_scalar_col(sel, "molecular_framework", NA_character_),
        classyfire_status = metminer_scalar_col(sel, "classyfire_status", NA_character_),
        classyfire_source = metminer_scalar_col(sel, "classyfire_source", NA_character_),
        metid_level = suppressWarnings(as.integer(metminer_scalar_col(sel, "Level", NA_integer_))),
        metid_rank = suppressWarnings(as.integer(metminer_scalar_col(sel, "candidate_rank", NA_integer_))),
        metid_total_score = round(suppressWarnings(as.numeric(metminer_scalar_col(sel, "Total.score", NA_real_))), 4),
        network_final_score = suppressWarnings(as.numeric(metminer_scalar_col(sel, "final_score", NA_real_))),
        annotation_interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    })

    feature_roles <- do.call(rbind, feature_roles)
    role_rows[[as.character(sid)]] <- feature_roles

    hypothesis_rows[[as.character(sid)]] <- data.frame(
      sub_network = sid,
      rt_center = round(stats::median(feature_roles$rt, na.rm = TRUE), 3),
      parent_feature_id = parent_id,
      putative_real_compound = metminer_scalar_value(parent_compound, NA_character_),
      parent_annotation_level = suppressWarnings(as.integer(metminer_scalar_col(parent_sel, "Level", NA_integer_))),
      parent_candidate_rank = suppressWarnings(as.integer(metminer_scalar_col(parent_sel, "candidate_rank", NA_integer_))),
      feature_count = nrow(feature_roles),
      isotope_features = sum(feature_roles$network_role == "isotope_of_parent"),
      adduct_features = sum(feature_roles$network_role == "adduct_of_parent"),
      isf_features = sum(feature_roles$network_role == "isf_fragment_of_parent"),
      unannotated_explained_features = sum(grepl("^unannotated_.*explained", feature_roles$annotation_interpretation)),
      annotation_conflicts = sum(grepl("conflict|review", feature_roles$annotation_interpretation)),
      interpretation = summarize_subnetwork_interpretation(feature_roles, parent_compound),
      stringsAsFactors = FALSE
    )
  }

  list(
    feature_role_interpretation = if (length(role_rows) > 0) do.call(rbind, role_rows) else data.frame(),
    subnetwork_hypothesis = if (length(hypothesis_rows) > 0) do.call(rbind, hypothesis_rows) else data.frame()
  )
}

metminer_scalar_col <- function(x, col, default = NA_character_) {
  if (is.null(x) || nrow(x) == 0 || !col %in% colnames(x)) {
    return(default)
  }
  metminer_scalar_value(x[[col]], default)
}

metminer_scalar_value <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  x[1]
}

choose_subnetwork_parent <- function(feature_ids, subnet_edges, feature_selection, feature_info) {
  score <- stats::setNames(rep(0, length(feature_ids)), feature_ids)

  if (nrow(subnet_edges) > 0) {
    out_isf <- table(subnet_edges$from[subnet_edges$type %in% c("ISF", "Cross-polarity ISF")])
    out_iso <- table(subnet_edges$from[subnet_edges$type == "Isotope"])
    degree <- table(c(subnet_edges$from, subnet_edges$to))
    score[names(out_isf)] <- score[names(out_isf)] + as.numeric(out_isf) * 3
    score[names(out_iso)] <- score[names(out_iso)] + as.numeric(out_iso) * 1.5
    score[names(degree)] <- score[names(degree)] + as.numeric(degree) * 0.2
  }

  sel <- feature_selection[feature_selection$variable_id %in% feature_ids, , drop = FALSE]
  if (nrow(sel) > 0) {
    sel$compound_key <- as.character(sel$compound_key)
    score[sel$variable_id] <- score[sel$variable_id] + ifelse(has_text(sel$compound_key), 1.5, 0)
    score[sel$variable_id] <- score[sel$variable_id] + normalize_annotation_score(sel$final_score)
  }

  info <- feature_info[feature_info$variable_id %in% feature_ids, , drop = FALSE]
  if (nrow(info) > 0 && any(is.finite(info$mean_area))) {
    area_score <- normalize_annotation_score(info$mean_area)
    names(area_score) <- info$variable_id
    score[names(area_score)] <- score[names(area_score)] + area_score * 0.8
  }

  names(score)[which.max(score)]
}

describe_feature_relation_to_parent <- function(feature_id, parent_id, subnet_edges) {
  if (identical(feature_id, parent_id)) {
    return(list(role = "putative_parent", relation = "self", edge_evidence = "selected as sub-network parent"))
  }

  direct <- subnet_edges[subnet_edges$from == parent_id & subnet_edges$to == feature_id, , drop = FALSE]
  reverse <- subnet_edges[subnet_edges$from == feature_id & subnet_edges$to == parent_id, , drop = FALSE]
  connected <- rbind(direct, reverse)

  if (nrow(connected) == 0) {
    return(list(role = "network_neighbor", relation = "connected_indirectly", edge_evidence = "no direct edge to parent"))
  }

  edge <- connected[order(edge_role_priority(connected$type)), , drop = FALSE][1, ]
  direct_parent_to_child <- edge$from == parent_id
  role <- switch(
    edge$type,
    "Isotope" = if (direct_parent_to_child) "isotope_of_parent" else "parent_isotope_or_reverse_isotope_edge",
    "Adduct" = "adduct_of_parent",
    "ISF" = if (direct_parent_to_child) "isf_fragment_of_parent" else "possible_parent_of_selected_parent",
    "Cross-polarity" = "cross_polarity_counterpart",
    "Cross-polarity ISF" = if (direct_parent_to_child) "cross_polarity_isf_fragment" else "possible_cross_polarity_parent",
    "network_neighbor"
  )

  list(
    role = role,
    relation = if (direct_parent_to_child) paste("parent_to_feature", edge$type, sep = "::") else paste("feature_to_parent", edge$type, sep = "::"),
    edge_evidence = paste(edge$type, edge$annotation, sprintf("r=%.3f", edge$abundance_cor), sep = " | ")
  )
}

edge_role_priority <- function(type) {
  match(type, c("ISF", "Cross-polarity ISF", "Isotope", "Adduct", "Cross-polarity"), nomatch = 99)
}

interpret_feature_annotation_role <- function(sel, role, parent_compound_key) {
  if (is.null(sel) || nrow(sel) == 0 || !has_text(sel$compound_key[1])) {
    if (role %in% c("isotope_of_parent", "adduct_of_parent", "isf_fragment_of_parent", "cross_polarity_counterpart", "cross_polarity_isf_fragment")) {
      return(paste0("unannotated_but_explained_as_", role))
    }
    return("unannotated_unresolved")
  }

  same_as_parent <- has_text(parent_compound_key) && identical(as.character(sel$compound_key[1]), as.character(parent_compound_key))

  if (role == "putative_parent") {
    return("putative_real_compound_annotation")
  }
  if (role %in% c("isotope_of_parent", "adduct_of_parent", "cross_polarity_counterpart")) {
    return(if (same_as_parent) "annotation_consistent_with_parent" else "annotation_conflicts_with_parent_role")
  }
  if (role %in% c("isf_fragment_of_parent", "cross_polarity_isf_fragment")) {
    return(if (same_as_parent) "fragment_annotated_as_parent_or_in_source_form" else "fragment_has_independent_annotation_review")
  }
  "network_neighbor_annotation_review"
}

summarize_subnetwork_interpretation <- function(feature_roles, parent_compound) {
  parent_text <- if (has_text(parent_compound)) as.character(parent_compound) else "unknown compound"
  paste0(
    "Putative parent: ", parent_text,
    "; isotopes=", sum(feature_roles$network_role == "isotope_of_parent"),
    "; adducts=", sum(feature_roles$network_role == "adduct_of_parent"),
    "; ISF/fragments=", sum(feature_roles$network_role == "isf_fragment_of_parent"),
    "; conflicts/review=", sum(grepl("conflict|review", feature_roles$annotation_interpretation))
  )
}

# ---- EIC helper ----

#' Build sample choice labels for EIC display
#'
#' @noRd
metminer_eic_sample_choices <- function(eic_data, object = NULL) {
  samples <- sort(unique(eic_data$data$sample))
  labels <- samples
  if (!is.null(object)) {
    sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
    if (nrow(sample_info) > 0 && all(c("sample_id", "raw_file_name") %in% colnames(sample_info))) {
      idx <- match(basename(samples), basename(as.character(sample_info$raw_file_name)))
      labels[!is.na(idx)] <- paste0(sample_info$sample_id[idx[!is.na(idx)]], " (", basename(samples[!is.na(idx)]), ")")
    }
  }
  stats::setNames(samples, labels)
}

# ---- Annotation filtering review tables ----

#' Normalize adduct labels for priority matching
#'
#' @noRd
metminer_normalize_adduct_label <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x <- gsub("\\s+", "", x)
  x <- gsub("[M-H2O+H]+", "[M+H-H2O]+", x, fixed = TRUE)
  x <- gsub("[M-H2O-H]-", "[M-H-H2O]-", x, fixed = TRUE)
  x <- gsub("[M+FA-H]-", "[M+HCOO]-", x, fixed = TRUE)
  x <- gsub("[M+Hac-H]-", "[M+CH3COO]-", x, fixed = TRUE)
  x
}

#' Default adduct-priority set used when no advisor file is supplied
#'
#' @noRd
metminer_default_adduct_advice <- function() {
  list(
    positive_core = c("[M+H]+", "[M+Na]+"),
    positive_optional = c("[M+K]+", "[M+H-H2O]+", "[2M+H]+", "[2M+Na]+"),
    negative_core = c("[M-H]-", "[M+HCOO]-"),
    negative_optional = c("[M+Cl]-", "[M-H-H2O]-", "[M-H2O-H]-", "[2M-H]-", "[M+CH3COO]-")
  )
}

metminer_split_adduct_recommendation <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = ",")
  x <- unlist(strsplit(x, ",|;", perl = TRUE), use.names = FALSE)
  x <- metminer_normalize_adduct_label(x)
  unique(x[has_text(x)])
}

#' Read parameter-advisor JSON/TSV/CSV and return adduct priority lists
#'
#' @noRd
metminer_read_parameter_adduct_advice <- function(path = NULL) {
  advice <- metminer_default_adduct_advice()
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    return(advice)
  }

  ext <- tolower(tools::file_ext(path))
  tab <- tryCatch({
    if (identical(ext, "json")) {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("Package 'jsonlite' is required to read parameter advice JSON.", call. = FALSE)
      }
      parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
      parsed$advice %||% parsed
    } else if (identical(ext, "csv")) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) NULL)

  if (is.null(tab) || nrow(tab) == 0 || !"parameter" %in% colnames(tab) || !"recommendation" %in% colnames(tab)) {
    return(advice)
  }

  get_row <- function(label) {
    idx <- which(tolower(tab$parameter) == tolower(label))
    if (length(idx) == 0) return(character())
    metminer_split_adduct_recommendation(tab$recommendation[idx[1]])
  }

  update_if_present <- function(name, value) {
    if (length(value) > 0) {
      advice[[name]] <<- value
    }
  }

  update_if_present("positive_core", get_row("Positive core adducts"))
  update_if_present("positive_optional", get_row("Positive optional adducts"))
  update_if_present("negative_core", get_row("Negative core adducts"))
  update_if_present("negative_optional", get_row("Negative optional adducts"))
  advice
}

metminer_format_adduct_advice <- function(advice) {
  advice <- advice %||% metminer_default_adduct_advice()
  paste0(
    "Positive core: ", paste(advice$positive_core, collapse = ", "), "\n",
    "Positive optional: ", paste(advice$positive_optional, collapse = ", "), "\n",
    "Negative core: ", paste(advice$negative_core, collapse = ", "), "\n",
    "Negative optional: ", paste(advice$negative_optional, collapse = ", ")
  )
}

metminer_annotation_adduct_priority <- function(adduct, mode = c("positive", "negative"), advice = NULL) {
  mode <- match.arg(mode)
  advice <- advice %||% metminer_default_adduct_advice()
  adduct <- metminer_normalize_adduct_label(adduct)
  core <- metminer_normalize_adduct_label(advice[[paste0(mode, "_core")]] %||% character())
  optional <- metminer_normalize_adduct_label(advice[[paste0(mode, "_optional")]] %||% character())
  out <- rep(3L, length(adduct))
  out[adduct %in% optional] <- 2L
  out[adduct %in% core] <- 1L
  out[!has_text(adduct)] <- 4L
  out
}

metminer_annotation_evidence_layer <- function(database = NULL,
                                               kegg_id = NULL,
                                               plantcyc_id = NULL,
                                               lab_id = NULL) {
  n <- max(length(database %||% NA_character_),
           length(kegg_id %||% NA_character_),
           length(plantcyc_id %||% NA_character_),
           length(lab_id %||% NA_character_))
  database <- rep_len(as.character(database %||% NA_character_), n)
  kegg_id <- rep_len(as.character(kegg_id %||% NA_character_), n)
  plantcyc_id <- rep_len(as.character(plantcyc_id %||% NA_character_), n)
  lab_id <- rep_len(as.character(lab_id %||% NA_character_), n)

  db_lower <- tolower(database)
  out <- rep("other_spectral", n)
  genome_hit <- grepl("plantcyc|biocyc|kegg|pgdb|cyc", db_lower) |
    has_text(kegg_id) | has_text(plantcyc_id)
  public_hit <- grepl("hmdb|massbank|mona|gnps|respect|rist|nist|mzcloud|massive", db_lower)
  local_hit <- grepl("lab|standard|in-house|inhouse|custom|user", db_lower) |
    (grepl("local", db_lower) & !genome_hit & !public_hit)

  out[genome_hit] <- "genome_reaction"
  out[public_hit] <- "public_ms2"
  out[local_hit] <- "local_spectral_optional"
  out
}

metminer_annotation_layer_priority <- function(layer) {
  layer <- as.character(layer %||% NA_character_)
  dplyr::case_when(
    layer == "local_spectral_optional" ~ 1L,
    layer == "public_ms2" ~ 2L,
    layer == "genome_reaction" ~ 3L,
    layer == "network_integrated" ~ 4L,
    TRUE ~ 5L
  )
}

metminer_annotation_evidence_scope <- function(layer) {
  layer <- as.character(layer %||% NA_character_)
  dplyr::case_when(
    layer == "genome_reaction" ~ "Layer 1: genome-informed KEGG/PlantCyc reaction candidate",
    layer == "public_ms2" ~ "Layer 2a: public MS2 spectral evidence",
    layer == "local_spectral_optional" ~ "Layer 2b: optional local standard/custom spectral evidence",
    TRUE ~ "Layer 2: spectral or external database evidence"
  )
}

metminer_is_genome_layer <- function(layer) {
  as.character(layer %||% NA_character_) == "genome_reaction"
}

metminer_is_strict_core_adduct <- function(adduct, mode = c("positive", "negative"), advice = NULL) {
  mode <- match.arg(mode)
  metminer_annotation_adduct_priority(adduct, mode, advice) == 1L
}

metminer_annotation_level_label <- function(level,
                                            layer = NA_character_,
                                            strict_core_adduct = NA,
                                            spectral_match = NA) {
  level <- suppressWarnings(as.integer(level))
  layer <- as.character(layer %||% NA_character_)
  n <- max(length(level), length(layer), length(strict_core_adduct %||% NA), length(spectral_match %||% NA))
  level <- rep_len(level, n)
  layer <- rep_len(layer, n)
  strict_core_adduct <- rep_len(as.logical(strict_core_adduct %||% NA), n)
  spectral_match <- rep_len(as.logical(spectral_match %||% NA), n)
  spectral_match <- (spectral_match %in% TRUE) | layer %in% c("public_ms2", "local_spectral_optional", "other_spectral")

  dplyr::case_when(
    layer == "local_spectral_optional" & !is.na(level) & level <= 1L ~ "Level 1: local standard/custom library match",
    layer == "local_spectral_optional" & !is.na(level) & level <= 2L ~ "Level 2b: local spectral evidence, optional module",
    layer == "public_ms2" & !is.na(level) & level <= 2L ~ "Level 2: public MS2 spectral evidence",
    layer == "genome_reaction" & spectral_match & !is.na(level) & level <= 2L ~ "Level 2a: MS2 evidence with genome/reaction support",
    layer == "genome_reaction" & strict_core_adduct ~ "Level 3: genome/reaction-supported strict-adduct candidate",
    !is.na(level) & level >= 4L ~ "Level 4: low-confidence formula/class-level candidate",
    TRUE ~ "Unassigned/needs review"
  )
}

metminer_add_annotation_layer_columns <- function(x, mode = c("positive", "negative"), adduct_advice = NULL) {
  mode <- match.arg(mode)
  if (is.null(x) || nrow(x) == 0) return(x)
  x <- metminer_standardize_review_annotation_cols(as.data.frame(x, stringsAsFactors = FALSE))
  plantcyc_id <- x$PlantCyc.ID
  plantcyc_id[!has_text(plantcyc_id) & has_text(x$BIOCYC.ID)] <- x$BIOCYC.ID[!has_text(plantcyc_id) & has_text(x$BIOCYC.ID)]
  x$annotation_layer <- metminer_annotation_evidence_layer(
    database = x$Database,
    kegg_id = x$KEGG.ID,
    plantcyc_id = plantcyc_id,
    lab_id = x$Lab.ID
  )
  x$evidence_scope <- metminer_annotation_evidence_scope(x$annotation_layer)
  x$core_adduct_match <- metminer_is_strict_core_adduct(x$Adduct, mode, adduct_advice)
  x$strict_genome_adduct_pass <- !metminer_is_genome_layer(x$annotation_layer) | x$core_adduct_match
  x$metminer_confidence_level <- metminer_annotation_level_label(
    level = x$Level,
    layer = x$annotation_layer,
    strict_core_adduct = x$core_adduct_match,
    spectral_match = x$annotation_layer %in% c("public_ms2", "local_spectral_optional", "other_spectral")
  )
  x
}

metminer_empty_annotation_review_table <- function(type = c("expand", "collapse")) {
  type <- match.arg(type)
  cols <- c(
    "Variable_id", "mz", "rt", "ms2_spectrum_id", "Compound.name",
    "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "mz.error", "mz.match.score",
    "Total.score", "Database", "Level", "annotation_layer", "evidence_scope",
    "core_adduct_match", "strict_genome_adduct_pass", "metminer_confidence_level",
    "Sub_net_id"
  )
  if (identical(type, "expand")) {
    cols <- c(cols, "coelution_type", "Represent_feature", "mode", "annotation_rank", "adduct_priority")
  }
  stats::setNames(as.data.frame(matrix(nrow = 0, ncol = length(cols)), stringsAsFactors = FALSE), cols)
}

metminer_ensure_columns <- function(x, cols, fill = NA) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  missing_cols <- setdiff(cols, colnames(x))
  for (col in missing_cols) {
    x[[col]] <- fill
  }
  x
}

metminer_standardize_review_annotation_cols <- function(x) {
  needed <- c(
    "variable_id", "Compound.name", "KEGG.ID", "PlantCyc.ID", "BIOCYC.ID", "Lab.ID", "Adduct",
    "mz.error", "mz.match.score", "Total.score", "Database", "Level",
    "CAS.ID", "HMDB.ID", "RT.error", "RT.match.score", "CE", "SS", "ms2_spectrum_id"
  )
  for (col in needed) {
    if (!col %in% colnames(x)) {
      x[[col]] <- NA
    }
  }
  lab <- trimws(as.character(x$Lab.ID %||% NA_character_))
  database <- tolower(trimws(as.character(x$Database %||% NA_character_)))
  plant <- trimws(as.character(x$PlantCyc.ID %||% NA_character_))
  biocyc <- trimws(as.character(x$BIOCYC.ID %||% NA_character_))
  kegg <- trimws(as.character(x$KEGG.ID %||% NA_character_))

  is_plantcyc <- grepl("plantcyc|pmn|biocyc|pgdb", database, perl = TRUE)
  fill_plant_from_biocyc <- !has_text(plant) & has_text(biocyc)
  plant[fill_plant_from_biocyc] <- biocyc[fill_plant_from_biocyc]
  fill_plant_from_lab <- !has_text(plant) & is_plantcyc & has_text(lab) & !grepl("^C[0-9]{5}$", lab, perl = TRUE)
  plant[fill_plant_from_lab] <- lab[fill_plant_from_lab]
  biocyc[!has_text(biocyc) & has_text(plant)] <- plant[!has_text(biocyc) & has_text(plant)]
  fill_kegg_from_lab <- !has_text(kegg) & has_text(lab) & grepl("^C[0-9]{5}$", lab, perl = TRUE)
  kegg[fill_kegg_from_lab] <- lab[fill_kegg_from_lab]

  x$PlantCyc.ID <- plant
  x$BIOCYC.ID <- biocyc
  x$KEGG.ID <- kegg
  x
}

metminer_annotation_compound_name_key <- function(x) {
  x <- tolower(trimws(as.character(x %||% NA_character_)))
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- gsub("[^[:alnum:]]+", "", x, perl = TRUE)
  x[!has_text(x)] <- NA_character_
  x
}

metminer_collapse_annotation_ids <- function(x) {
  x <- trimws(as.character(x %||% NA_character_))
  x <- unique(x[has_text(x)])
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(x, collapse = ";")
}

metminer_fill_same_compound_annotation_ids <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }
  x <- metminer_standardize_review_annotation_cols(as.data.frame(x, stringsAsFactors = FALSE))
  if (!all(c("variable_id", "Compound.name") %in% colnames(x))) {
    return(x)
  }

  name_key <- metminer_annotation_compound_name_key(x$Compound.name)
  group_key <- paste(as.character(x$variable_id), name_key, sep = "\r")
  group_key[!has_text(name_key)] <- NA_character_

  for (col in c("PlantCyc.ID", "BIOCYC.ID", "KEGG.ID")) {
    if (!col %in% colnames(x)) {
      x[[col]] <- NA_character_
    }
    fill_by_group <- vapply(seq_len(nrow(x)), function(i) {
      key <- group_key[i]
      if (!has_text(key)) {
        return(NA_character_)
      }
      metminer_collapse_annotation_ids(x[[col]][group_key == key])
    }, character(1))
    missing <- !has_text(x[[col]]) & has_text(fill_by_group)
    x[[col]][missing] <- fill_by_group[missing]
  }

  plant_missing <- !has_text(x$PlantCyc.ID) & has_text(x$BIOCYC.ID)
  x$PlantCyc.ID[plant_missing] <- x$BIOCYC.ID[plant_missing]
  biocyc_missing <- !has_text(x$BIOCYC.ID) & has_text(x$PlantCyc.ID)
  x$BIOCYC.ID[biocyc_missing] <- x$PlantCyc.ID[biocyc_missing]
  x
}

metminer_read_compound_id_mapping <- function(path = NULL) {
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    return(data.frame())
  }
  ext <- tolower(tools::file_ext(path))
  tab <- tryCatch({
    if (identical(ext, "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel mapping files.", call. = FALSE)
      }
      readxl::read_excel(path, sheet = 1)
    } else if (identical(ext, "csv")) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) data.frame())
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  if (nrow(tab) == 0) return(tab)
  if (!"PlantCyc.ID" %in% colnames(tab) && "BIOCYC.ID" %in% colnames(tab)) {
    tab$PlantCyc.ID <- tab$BIOCYC.ID
  }
  if (!all(c("PlantCyc.ID", "KEGG.ID") %in% colnames(tab))) {
    return(data.frame())
  }
  tab$PlantCyc.ID <- trimws(as.character(tab$PlantCyc.ID))
  tab$KEGG.ID <- trimws(as.character(tab$KEGG.ID))
  tab <- tab[has_text(tab$PlantCyc.ID) & has_text(tab$KEGG.ID), , drop = FALSE]
  tab <- tab[!duplicated(paste(tab$PlantCyc.ID, tab$KEGG.ID, sep = "\r")), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

metminer_apply_compound_id_mapping <- function(x, mapping = NULL) {
  if (is.null(x) || nrow(x) == 0) return(x)
  if (!"PlantCyc.ID" %in% colnames(x)) x$PlantCyc.ID <- NA_character_
  if (!"KEGG.ID" %in% colnames(x)) x$KEGG.ID <- NA_character_
  if (!"Lab.ID" %in% colnames(x)) x$Lab.ID <- NA_character_
  if (!"Database" %in% colnames(x)) x$Database <- NA_character_

  lab <- trimws(as.character(x$Lab.ID %||% NA_character_))
  kegg <- trimws(as.character(x$KEGG.ID %||% NA_character_))
  plant <- trimws(as.character(x$PlantCyc.ID %||% NA_character_))
  database <- tolower(trimws(as.character(x$Database %||% NA_character_)))
  is_plantcyc <- grepl("plantcyc|pmn|biocyc|pgdb", database, perl = TRUE)
  fill_plant_from_lab <- !has_text(plant) & is_plantcyc & has_text(lab) & !grepl("^C[0-9]{5}$", lab, perl = TRUE)
  plant[fill_plant_from_lab] <- lab[fill_plant_from_lab]
  kegg[!has_text(kegg) & has_text(lab) & grepl("^C[0-9]{5}$", lab, perl = TRUE)] <- lab[!has_text(kegg) & has_text(lab) & grepl("^C[0-9]{5}$", lab, perl = TRUE)]

  if (!is.null(mapping) && nrow(mapping) > 0 && all(c("PlantCyc.ID", "KEGG.ID") %in% colnames(mapping))) {
    k_by_plant <- stats::setNames(mapping$KEGG.ID, mapping$PlantCyc.ID)
    p_by_kegg <- stats::setNames(mapping$PlantCyc.ID, mapping$KEGG.ID)
    fill_k <- !has_text(kegg) & has_text(plant) & plant %in% names(k_by_plant)
    kegg[fill_k] <- unname(k_by_plant[plant[fill_k]])
    fill_p <- !has_text(plant) & has_text(kegg) & kegg %in% names(p_by_kegg)
    plant[fill_p] <- unname(p_by_kegg[kegg[fill_p]])
  }

  x$KEGG.ID <- kegg
  x$PlantCyc.ID <- plant
  x
}

metminer_select_top_annotation_candidates <- function(object, mode = c("positive", "negative"),
                                                      top_n = 3, adduct_advice = NULL) {
  mode <- match.arg(mode)
  annotation_table <- metminer_safe_extract_annotation_table(object)
  if (is.null(annotation_table) || nrow(annotation_table) == 0) {
    return(data.frame())
  }
  candidates <- metminer_standardize_review_annotation_cols(as.data.frame(annotation_table, stringsAsFactors = FALSE))
  if (!"variable_id" %in% colnames(candidates)) {
    return(data.frame())
  }

  candidates$variable_id <- as.character(candidates$variable_id)
  candidates$Level <- suppressWarnings(as.integer(candidates$Level))
  candidates$Total.score <- suppressWarnings(as.numeric(candidates$Total.score))
  candidates <- metminer_add_annotation_layer_columns(candidates, mode, adduct_advice)
  candidates <- metminer_fill_same_compound_annotation_ids(candidates)
  candidates <- candidates[candidates$strict_genome_adduct_pass %in% TRUE, , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(data.frame())
  }
  candidates$adduct_priority <- metminer_annotation_adduct_priority(candidates$Adduct, mode, adduct_advice)
  candidates$mode <- mode

  candidates <- candidates[order(
    candidates$variable_id,
    candidates$annotation_layer != "local_spectral_optional",
    candidates$annotation_layer != "public_ms2",
    candidates$Level,
    -candidates$Total.score,
    candidates$adduct_priority,
    na.last = TRUE
  ), , drop = FALSE]
  candidates$annotation_rank <- ave(seq_len(nrow(candidates)), candidates$variable_id, FUN = seq_along)
  candidates <- candidates[candidates$annotation_rank <= max(1L, as.integer(top_n %||% 3L)), , drop = FALSE]
  rownames(candidates) <- NULL
  candidates
}

metminer_object_review_rows <- function(object, mode = c("positive", "negative"), top_n = 3,
                                        adduct_advice = NULL) {
  mode <- match.arg(mode)
  if (is.null(object)) {
    return(metminer_empty_annotation_review_table("expand"))
  }

  variable_info <- metminer_safe_extract_variable_info(object)
  if (is.null(variable_info) || nrow(variable_info) == 0 || !"variable_id" %in% colnames(variable_info)) {
    return(metminer_empty_annotation_review_table("expand"))
  }
  variable_info <- as.data.frame(variable_info, stringsAsFactors = FALSE)
  variable_info$variable_id <- as.character(variable_info$variable_id)
  for (col in c("mz", "rt", "ms2_spectrum_id")) {
    if (!col %in% colnames(variable_info)) {
      variable_info[[col]] <- NA
    }
  }

  roles <- tryCatch(metminer_extract_annotation_validation(object)$feature_role_interpretation, error = function(e) data.frame())
  if (is.null(roles) || nrow(roles) == 0) {
    network <- normalize_feature_network(extract_feature_network(object))
    membership <- annotation_network_membership(network, variable_info$variable_id)
    roles <- data.frame(
      sub_network = unname(membership[variable_info$variable_id]),
      feature_id = variable_info$variable_id,
      parent_feature_id = variable_info$variable_id,
      network_role = "single_feature",
      annotation_interpretation = "single_feature_without_network_role",
      stringsAsFactors = FALSE
    )
  }

  roles <- as.data.frame(roles, stringsAsFactors = FALSE)
  if (!"feature_id" %in% colnames(roles)) {
    return(metminer_empty_annotation_review_table("expand"))
  }
  roles$feature_id <- as.character(roles$feature_id)
  for (col in c("sub_network", "parent_feature_id", "network_role", "annotation_interpretation")) {
    if (!col %in% colnames(roles)) {
      roles[[col]] <- NA_character_
    }
  }

  top <- metminer_select_top_annotation_candidates(object, mode, top_n, adduct_advice)
  role_features <- unique(roles$feature_id)
  missing_role_top <- setdiff(role_features, top$variable_id %||% character())
  if (nrow(top) > 0 && length(missing_role_top) > 0) {
    blank <- metminer_standardize_review_annotation_cols(data.frame(variable_id = missing_role_top, stringsAsFactors = FALSE))
    blank <- metminer_ensure_columns(blank, colnames(top))
    blank$adduct_priority <- NA_integer_
    blank$mode <- mode
    blank$annotation_rank <- NA_integer_
    top <- rbind(top, blank[, colnames(top), drop = FALSE])
  }
  if (nrow(top) == 0) {
    top <- metminer_standardize_review_annotation_cols(data.frame(variable_id = variable_info$variable_id, stringsAsFactors = FALSE))
    top <- metminer_add_annotation_layer_columns(top, mode, adduct_advice)
    top$adduct_priority <- NA_integer_
    top$mode <- mode
    top$annotation_rank <- NA_integer_
  }

  merged <- merge(
    top,
    variable_info[, unique(c("variable_id", "mz", "rt", "ms2_spectrum_id")), drop = FALSE],
    by = "variable_id",
    all.x = TRUE,
    suffixes = c("", ".feature")
  )
  for (col in c("mz", "rt", "ms2_spectrum_id")) {
    fcol <- paste0(col, ".feature")
    if (fcol %in% colnames(merged)) {
      merged[[col]] <- merged[[col]] %||% merged[[fcol]]
      idx <- is.na(merged[[col]]) | !has_text(merged[[col]])
      merged[[col]][idx] <- merged[[fcol]][idx]
      merged[[fcol]] <- NULL
    }
  }

  merged <- merge(
    merged,
    roles[, unique(c("feature_id", "sub_network", "parent_feature_id", "network_role", "annotation_interpretation")), drop = FALSE],
    by.x = "variable_id",
    by.y = "feature_id",
    all.x = TRUE
  )

  out <- data.frame(
    Variable_id = merged$variable_id,
    mz = suppressWarnings(as.numeric(merged$mz)),
    rt = suppressWarnings(as.numeric(merged$rt)),
    ms2_spectrum_id = merged$ms2_spectrum_id,
    Compound.name = merged$Compound.name,
    KEGG.ID = merged$KEGG.ID,
    PlantCyc.ID = ifelse(has_text(merged$PlantCyc.ID), merged$PlantCyc.ID, merged$BIOCYC.ID),
    Lab.ID = merged$Lab.ID,
    Adduct = merged$Adduct,
    mz.error = merged$mz.error,
    mz.match.score = merged$mz.match.score,
    Total.score = suppressWarnings(as.numeric(merged$Total.score)),
    Database = merged$Database,
    Level = suppressWarnings(as.integer(merged$Level)),
    annotation_layer = merged$annotation_layer,
    evidence_scope = merged$evidence_scope,
    core_adduct_match = merged$core_adduct_match,
    strict_genome_adduct_pass = merged$strict_genome_adduct_pass,
    metminer_confidence_level = merged$metminer_confidence_level,
    Sub_net_id = merged$sub_network,
    coelution_type = ifelse(has_text(merged$annotation_interpretation), merged$annotation_interpretation, merged$network_role),
    Represent_feature = ifelse(has_text(merged$parent_feature_id), merged$parent_feature_id, merged$variable_id),
    mode = mode,
    annotation_rank = merged$annotation_rank,
    adduct_priority = merged$adduct_priority,
    stringsAsFactors = FALSE
  )

  out <- out[order(out$Sub_net_id, out$Represent_feature, out$Variable_id, out$annotation_rank, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_collapse_annotation_review_table <- function(expand_table) {
  if (is.null(expand_table) || nrow(expand_table) == 0) {
    return(metminer_empty_annotation_review_table("collapse"))
  }

  x <- metminer_ensure_columns(
    expand_table,
    c(
      "Variable_id", "mz", "rt", "ms2_spectrum_id", "Compound.name",
      "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "mz.error", "mz.match.score",
      "Total.score", "Database", "Level", "annotation_layer", "evidence_scope",
      "core_adduct_match", "strict_genome_adduct_pass", "metminer_confidence_level",
      "Sub_net_id",
      "Represent_feature", "adduct_priority", "annotation_rank"
    )
  )
  x$Sub_net_id[!has_text(x$Sub_net_id)] <- paste0("single_", x$Variable_id[!has_text(x$Sub_net_id)])
  x$is_represent <- as.character(x$Variable_id) == as.character(x$Represent_feature)
  x$Level_sort <- suppressWarnings(as.integer(x$Level))
  x$Level_sort[is.na(x$Level_sort)] <- 99L
  x$Score_sort <- suppressWarnings(as.numeric(x$Total.score))
  x$Score_sort[is.na(x$Score_sort)] <- -Inf
  x$Adduct_sort <- suppressWarnings(as.integer(x$adduct_priority))
  x$Adduct_sort[is.na(x$Adduct_sort)] <- 99L
  x$Rank_sort <- suppressWarnings(as.integer(x$annotation_rank))
  x$Rank_sort[is.na(x$Rank_sort)] <- 99L

  idx <- vapply(split(seq_len(nrow(x)), x$Sub_net_id), function(ii) {
    ii[order(
      !x$is_represent[ii],
      x$Level_sort[ii],
      -x$Score_sort[ii],
      x$Adduct_sort[ii],
      x$Rank_sort[ii],
      na.last = TRUE
    )[1]]
  }, integer(1))

  out <- x[idx, c(
    "Variable_id", "mz", "rt", "ms2_spectrum_id", "Compound.name",
    "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "mz.error", "mz.match.score",
    "Total.score", "Database", "Level", "annotation_layer", "evidence_scope",
    "core_adduct_match", "strict_genome_adduct_pass", "metminer_confidence_level",
    "Sub_net_id"
  ), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Build human-review annotation filtering tables
#'
#' @noRd
metminer_build_annotation_review_tables <- function(positive_object = NULL, negative_object = NULL,
                                                    top_n = 3, adduct_advice = NULL,
                                                    id_mapping = NULL) {
  adduct_advice <- adduct_advice %||% metminer_default_adduct_advice()
  pos <- metminer_object_review_rows(positive_object, "positive", top_n, adduct_advice)
  neg <- metminer_object_review_rows(negative_object, "negative", top_n, adduct_advice)
  expand <- rbind(pos, neg)
  expand <- metminer_apply_compound_id_mapping(expand, id_mapping)
  expand_cols <- c(
    "Variable_id", "mz", "rt", "ms2_spectrum_id", "Compound.name",
    "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "mz.error", "mz.match.score",
    "Total.score", "Database", "Level", "annotation_layer", "evidence_scope",
    "core_adduct_match", "strict_genome_adduct_pass", "metminer_confidence_level",
    "Sub_net_id",
    "coelution_type", "Represent_feature", "mode", "annotation_rank", "adduct_priority"
  )
  expand <- metminer_ensure_columns(expand, expand_cols)
  if (nrow(expand) > 0) {
    expand <- expand[, expand_cols, drop = FALSE]
  }
  collapse <- metminer_collapse_annotation_review_table(expand)
  list(
    expand_table = expand,
    collapse_table = collapse,
    adduct_advice = adduct_advice
  )
}
