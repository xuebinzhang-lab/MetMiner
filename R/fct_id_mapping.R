# ---- Cross-database compound ID mapping helpers ----

metminer_idmap_read_table <- function(path) {
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    stop("Input file does not exist.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))

  read_rda <- function(path) {
    env <- new.env(parent = emptyenv())
    objs <- tryCatch(load(path, envir = env), error = function(e) character())
    for (obj_name in objs) {
      obj <- get(obj_name, envir = env)
      if (methods::is(obj, "databaseClass")) {
        return(as.data.frame(obj@spectra.info, stringsAsFactors = FALSE))
      }
      if (is.data.frame(obj)) {
        return(as.data.frame(obj, stringsAsFactors = FALSE))
      }
    }
    NULL
  }

  if (identical(ext, "rda") || identical(ext, "rdata") || !has_text(ext)) {
    rda <- read_rda(path)
    if (!is.null(rda)) return(rda)
    if (identical(ext, "rda") || identical(ext, "rdata")) {
      stop("No databaseClass or data.frame object found in RDA file.", call. = FALSE)
    }
  }

  if (identical(ext, "csv")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (!has_text(ext)) {
    return(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  rda <- read_rda(path)
  if (!is.null(rda)) return(rda)
  if (identical(ext, "csv")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
}

metminer_idmap_read_rda_object <- function(path) {
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    stop("Input file does not exist.", call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  objs <- tryCatch(load(path, envir = env), error = function(e) {
    stop("Uploaded file is not a valid RDA/RData database file.", call. = FALSE)
  })
  for (obj_name in objs) {
    obj <- get(obj_name, envir = env)
    if (methods::is(obj, "databaseClass") || is.data.frame(obj)) {
      return(list(object = obj, object_name = obj_name, source_file = basename(path)))
    }
  }
    stop("No databaseClass or data.frame object found in RDA file.", call. = FALSE)
}

metminer_idmap_object_spectra_info <- function(obj) {
  if (methods::is(obj, "databaseClass")) {
    return(as.data.frame(obj@spectra.info, stringsAsFactors = FALSE))
  }
  if (is.data.frame(obj)) {
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }
  stop("Object must be a databaseClass or data.frame.", call. = FALSE)
}

metminer_idmap_replace_spectra_info <- function(obj, spectra_info) {
  spectra_info <- as.data.frame(spectra_info, stringsAsFactors = FALSE, check.names = FALSE)
  if (methods::is(obj, "databaseClass")) {
    obj@spectra.info <- spectra_info
    return(obj)
  }
  if (is.data.frame(obj)) {
    return(spectra_info)
  }
  stop("Object must be a databaseClass or data.frame.", call. = FALSE)
}

metminer_idmap_collapse <- function(x) {
  x <- unique(trimws(as.character(x)))
  x <- x[has_text(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = ";")
}

metminer_idmap_first_col <- function(x, cols, fallback = NA_character_) {
  for (col in cols) {
    if (col %in% colnames(x)) {
      return(x[[col]])
    }
  }
  rep(fallback, nrow(x))
}

metminer_idmap_norm_id <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[!has_text(x)] <- NA_character_
  x
}

metminer_idmap_norm_name <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("&[a-z]+;", " ", x, perl = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

metminer_idmap_split_synonyms <- function(x) {
  lapply(as.character(x %||% ""), function(one) {
    vals <- unlist(strsplit(one, "\\|\\|\\||[;|]", perl = TRUE), use.names = FALSE)
    vals <- metminer_idmap_norm_name(vals)
    unique(vals[has_text(vals)])
  })
}

metminer_idmap_prepare <- function(x, source = c("auto", "plantcyc", "kegg", "other"),
                                   source_file = NA_character_) {
  source <- match.arg(source)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) == 0) {
    return(data.frame())
  }

  lab_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("Lab.ID", "Lab.IDs", "compound_id", "Compound.ID")))
  biocyc_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("BIOCYC.ID", "BioCyc.ID", "PlantCyc.ID", "plantcyc_id")))
  kegg_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("KEGG.ID", "KEGG", "kegg_id")))

  inferred <- source
  if (identical(source, "auto")) {
    has_biocyc <- any(has_text(biocyc_id), na.rm = TRUE)
    has_kegg <- any(has_text(kegg_id), na.rm = TRUE) || any(grepl("^C[0-9]{5}$", lab_id %||% "", perl = TRUE), na.rm = TRUE)
    inferred <- if (has_biocyc && !has_kegg) "plantcyc" else if (has_kegg && !has_biocyc) "kegg" else "other"
  }

  if (identical(inferred, "plantcyc")) {
    plantcyc_id <- biocyc_id
    plantcyc_id[!has_text(plantcyc_id)] <- lab_id[!has_text(plantcyc_id)]
    primary_id <- plantcyc_id
  } else if (identical(inferred, "kegg")) {
    kegg_id[!has_text(kegg_id)] <- lab_id[!has_text(kegg_id) & grepl("^C[0-9]{5}$", lab_id %||% "", perl = TRUE)]
    primary_id <- kegg_id
    plantcyc_id <- biocyc_id
  } else {
    plantcyc_id <- biocyc_id
    primary_id <- lab_id
  }

  compound_name <- as.character(metminer_idmap_first_col(x, c("Compound.name", "compound_name", "Name", "name")))
  formula <- toupper(trimws(as.character(metminer_idmap_first_col(x, c("Formula", "formula")))))
  formula[!has_text(formula)] <- NA_character_
  exact_mass <- suppressWarnings(as.numeric(metminer_idmap_first_col(
    x,
    c("monisotopic_molecular_weight", "monoisotopic_mw", "mz", "exact_mass", "mass")
  )))
  inchikey <- toupper(trimws(as.character(metminer_idmap_first_col(x, c("INCHIKEY.ID", "InChIKey", "inchi_key")))))
  inchikey[!has_text(inchikey)] <- NA_character_
  smiles <- as.character(metminer_idmap_first_col(x, c("SMILES.ID", "SMILES", "smiles")))
  synonyms <- as.character(metminer_idmap_first_col(x, c("Synonyms", "synonyms"), ""))

  out <- data.frame(
    source = inferred,
    source_file = basename(source_file %||% ""),
    source_id = primary_id,
    Lab.ID = lab_id,
    PlantCyc.ID = plantcyc_id,
    KEGG.ID = kegg_id,
    Compound.name = compound_name,
    normalized_name = metminer_idmap_norm_name(compound_name),
    Formula = formula,
    exact_mass = exact_mass,
    INCHIKEY.ID = inchikey,
    SMILES.ID = smiles,
    Synonyms = synonyms,
    stringsAsFactors = FALSE
  )
  out <- out[has_text(out$source_id) | has_text(out$normalized_name) | has_text(out$INCHIKEY.ID), , drop = FALSE]
  out <- out[!duplicated(paste(out$source, out$source_id, out$normalized_name, out$Formula, sep = "\r")), , drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_idmap_detect_source <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) == 0) return("other")
  lab_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("Lab.ID", "Lab.IDs", "compound_id", "Compound.ID")))
  biocyc_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("BIOCYC.ID", "BioCyc.ID", "PlantCyc.ID", "plantcyc_id")))
  kegg_id <- metminer_idmap_norm_id(metminer_idmap_first_col(x, c("KEGG.ID", "KEGG", "kegg_id")))
  plant_score <- sum(has_text(biocyc_id), na.rm = TRUE) + sum(has_text(lab_id) & !grepl("^C[0-9]{5}$", lab_id %||% ""), na.rm = TRUE)
  kegg_score <- sum(has_text(kegg_id), na.rm = TRUE) + sum(has_text(lab_id) & grepl("^C[0-9]{5}$", lab_id %||% ""), na.rm = TRUE)
  if (plant_score > kegg_score) return("plantcyc")
  if (kegg_score > plant_score) return("kegg")
  "other"
}

metminer_idmap_orient_tables <- function(first_table, second_table,
                                         first_label = "first input",
                                         second_label = "second input") {
  first_source <- metminer_idmap_detect_source(first_table)
  second_source <- metminer_idmap_detect_source(second_table)
  if (identical(first_source, "plantcyc") && identical(second_source, "kegg")) {
    return(list(
      plantcyc_table = first_table,
      kegg_table = second_table,
      swapped = FALSE,
      message = paste0("Detected PlantCyc input from ", first_label, " and KEGG input from ", second_label, ".")
    ))
  }
  if (identical(first_source, "kegg") && identical(second_source, "plantcyc")) {
    return(list(
      plantcyc_table = second_table,
      kegg_table = first_table,
      swapped = TRUE,
      message = paste0("Input files appeared to be swapped; detected PlantCyc input from ", second_label, " and KEGG input from ", first_label, ".")
    ))
  }
  stop(
    "Cannot identify PlantCyc and KEGG inputs. Please upload one PlantCyc database and one KEGG database.",
    call. = FALSE
  )
}

metminer_idmap_orient_databases <- function(first_db, second_db) {
  first_info <- metminer_idmap_object_spectra_info(first_db$object)
  second_info <- metminer_idmap_object_spectra_info(second_db$object)
  oriented <- metminer_idmap_orient_tables(first_info, second_info, first_db$source_file, second_db$source_file)
  if (isTRUE(oriented$swapped)) {
    list(plantcyc = second_db, kegg = first_db, message = oriented$message, swapped = TRUE)
  } else {
    list(plantcyc = first_db, kegg = second_db, message = oriented$message, swapped = FALSE)
  }
}

metminer_idmap_name_index <- function(x) {
  syn <- metminer_idmap_split_synonyms(x$Synonyms)
  rows <- lapply(seq_len(nrow(x)), function(i) {
    names_i <- unique(c(x$normalized_name[i], syn[[i]]))
    names_i <- names_i[has_text(names_i)]
    if (length(names_i) == 0) return(NULL)
    data.frame(row_id = i, normalized_name = names_i, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(row_id = integer(), normalized_name = character()))
  }
  do.call(rbind, rows)
}

metminer_idmap_mass_ppm <- function(a, b) {
  abs(a - b) / pmax(abs(a), abs(b), na.rm = TRUE) * 1e6
}

metminer_build_compound_id_mapping <- function(plantcyc_table,
                                               kegg_table,
                                               mass_ppm = 10) {
  plant <- metminer_idmap_prepare(plantcyc_table, "plantcyc")
  kegg <- metminer_idmap_prepare(kegg_table, "kegg")
  if (nrow(plant) == 0 || nrow(kegg) == 0) {
    return(list(mapping = data.frame(), plantcyc_index = plant, kegg_index = kegg))
  }

  rows <- list()
  add_row <- function(pi, ki, match_type, confidence, mass_error_ppm = NA_real_) {
    rows[[length(rows) + 1L]] <<- data.frame(
      PlantCyc.ID = plant$PlantCyc.ID[pi],
      KEGG.ID = kegg$KEGG.ID[ki],
      PlantCyc.name = plant$Compound.name[pi],
      KEGG.name = kegg$Compound.name[ki],
      Formula = plant$Formula[pi] %||% kegg$Formula[ki],
      PlantCyc.mass = plant$exact_mass[pi],
      KEGG.mass = kegg$exact_mass[ki],
      mass_error_ppm = mass_error_ppm,
      INCHIKEY.ID = plant$INCHIKEY.ID[pi] %||% kegg$INCHIKEY.ID[ki],
      match_type = match_type,
      confidence = confidence,
      stringsAsFactors = FALSE
    )
  }

  # Direct ID carry-over if a database row already contains the other namespace.
  for (pi in which(has_text(plant$KEGG.ID))) {
    ki <- which(kegg$KEGG.ID == plant$KEGG.ID[pi])
    if (length(ki) > 0) add_row(pi, ki[1], "direct_kegg_id", "high")
  }

  # InChIKey matching is the most reliable when both databases carry it.
  common_keys <- intersect(plant$INCHIKEY.ID[has_text(plant$INCHIKEY.ID)], kegg$INCHIKEY.ID[has_text(kegg$INCHIKEY.ID)])
  for (key in common_keys) {
    pi <- which(plant$INCHIKEY.ID == key)
    ki <- which(kegg$INCHIKEY.ID == key)
    for (p in pi) for (k in ki) add_row(p, k, "inchikey", "high")
  }

  # Formula + monoisotopic mass + name/synonym gives conservative practical coverage.
  plant_names <- metminer_idmap_name_index(plant)
  kegg_names <- metminer_idmap_name_index(kegg)
  if (nrow(plant_names) > 0 && nrow(kegg_names) > 0) {
    joined <- merge(plant_names, kegg_names, by = "normalized_name", suffixes = c("_plant", "_kegg"))
    if (nrow(joined) > 0) {
      for (i in seq_len(nrow(joined))) {
        pi <- joined$row_id_plant[i]
        ki <- joined$row_id_kegg[i]
        same_formula <- has_text(plant$Formula[pi]) && has_text(kegg$Formula[ki]) && identical(plant$Formula[pi], kegg$Formula[ki])
        mass_error <- metminer_idmap_mass_ppm(plant$exact_mass[pi], kegg$exact_mass[ki])
        mass_ok <- is.finite(mass_error) && mass_error <= mass_ppm
        if (same_formula && mass_ok) {
          add_row(pi, ki, "name_synonym_formula_mass", "high", mass_error)
        } else if (same_formula || mass_ok) {
          add_row(pi, ki, "name_synonym_partial_chemistry", "medium", if (is.finite(mass_error)) mass_error else NA_real_)
        }
      }
    }
  }

  mapping <- if (length(rows) == 0) {
    data.frame()
  } else {
    do.call(rbind, rows)
  }
  if (nrow(mapping) > 0) {
    key <- paste(mapping$PlantCyc.ID, mapping$KEGG.ID, mapping$match_type, sep = "\r")
    mapping <- mapping[!duplicated(key), , drop = FALSE]
    mapping <- mapping[order(mapping$PlantCyc.ID, mapping$KEGG.ID, mapping$confidence), , drop = FALSE]
    rownames(mapping) <- NULL
  }

  list(mapping = mapping, plantcyc_index = plant, kegg_index = kegg)
}

metminer_idmap_load_and_build <- function(plantcyc_file, kegg_file, mass_ppm = 10) {
  first <- metminer_idmap_read_table(plantcyc_file)
  second <- metminer_idmap_read_table(kegg_file)
  oriented <- metminer_idmap_orient_tables(first, second, basename(plantcyc_file), basename(kegg_file))
  result <- metminer_build_compound_id_mapping(oriented$plantcyc_table, oriented$kegg_table, mass_ppm = mass_ppm)
  result$input_swapped <- oriented$swapped
  result$input_message <- oriented$message
  result
}

metminer_update_database_id_mapping <- function(plantcyc_file,
                                                kegg_file,
                                                mapping) {
  mapping <- as.data.frame(mapping %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(mapping) == 0 || !all(c("PlantCyc.ID", "KEGG.ID") %in% colnames(mapping))) {
    stop("No PlantCyc/KEGG mapping table is available.", call. = FALSE)
  }

  first_db <- metminer_idmap_read_rda_object(plantcyc_file)
  second_db <- metminer_idmap_read_rda_object(kegg_file)
  oriented_db <- metminer_idmap_orient_databases(first_db, second_db)
  plant_db <- oriented_db$plantcyc
  kegg_db <- oriented_db$kegg
  plant_info <- metminer_idmap_object_spectra_info(plant_db$object)
  kegg_info <- metminer_idmap_object_spectra_info(kegg_db$object)

  plant_lab <- metminer_idmap_norm_id(metminer_idmap_first_col(plant_info, c("Lab.ID", "Lab.IDs", "compound_id", "Compound.ID")))
  plant_id <- metminer_idmap_norm_id(metminer_idmap_first_col(plant_info, c("BIOCYC.ID", "BioCyc.ID", "PlantCyc.ID", "plantcyc_id")))
  plant_id[!has_text(plant_id)] <- plant_lab[!has_text(plant_id)]
  kegg_lab <- metminer_idmap_norm_id(metminer_idmap_first_col(kegg_info, c("Lab.ID", "Lab.IDs", "compound_id", "Compound.ID")))
  kegg_id <- metminer_idmap_norm_id(metminer_idmap_first_col(kegg_info, c("KEGG.ID", "KEGG", "kegg_id")))
  kegg_id[!has_text(kegg_id) & grepl("^C[0-9]{5}$", kegg_lab %||% "", perl = TRUE)] <- kegg_lab[!has_text(kegg_id) & grepl("^C[0-9]{5}$", kegg_lab %||% "", perl = TRUE)]

  plant_map <- stats::aggregate(
    mapping$KEGG.ID,
    by = list(PlantCyc.ID = mapping$PlantCyc.ID),
    FUN = metminer_idmap_collapse
  )
  colnames(plant_map)[2] <- "mapped_KEGG.ID"
  kegg_map <- stats::aggregate(
    mapping$PlantCyc.ID,
    by = list(KEGG.ID = mapping$KEGG.ID),
    FUN = metminer_idmap_collapse
  )
  colnames(kegg_map)[2] <- "mapped_PlantCyc.ID"

  plant_hit <- match(plant_id, plant_map$PlantCyc.ID)
  kegg_hit <- match(kegg_id, kegg_map$KEGG.ID)
  if (!"KEGG.ID" %in% colnames(plant_info)) plant_info$KEGG.ID <- NA_character_
  if (!"PlantCyc.ID" %in% colnames(kegg_info)) kegg_info$PlantCyc.ID <- NA_character_
  plant_info$KEGG.ID[!is.na(plant_hit)] <- plant_map$mapped_KEGG.ID[plant_hit[!is.na(plant_hit)]]
  kegg_info$PlantCyc.ID[!is.na(kegg_hit)] <- kegg_map$mapped_PlantCyc.ID[kegg_hit[!is.na(kegg_hit)]]

  if (!"PlantCyc.ID" %in% colnames(plant_info)) plant_info$PlantCyc.ID <- plant_id
  if (!"KEGG.ID" %in% colnames(kegg_info)) kegg_info$KEGG.ID <- kegg_id
  plant_info$MetMiner.IDMapping <- ifelse(!is.na(plant_hit), "PlantCyc_to_KEGG", NA_character_)
  kegg_info$MetMiner.IDMapping <- ifelse(!is.na(kegg_hit), "KEGG_to_PlantCyc", NA_character_)

  plant_db$object <- metminer_idmap_replace_spectra_info(plant_db$object, plant_info)
  kegg_db$object <- metminer_idmap_replace_spectra_info(kegg_db$object, kegg_info)
  list(
    plantcyc = plant_db,
    kegg = kegg_db,
    plantcyc_updated = sum(!is.na(plant_hit)),
    kegg_updated = sum(!is.na(kegg_hit))
  )
}

metminer_update_ms1_database_id_mapping <- function(plantcyc_file,
                                                    kegg_file,
                                                    mapping) {
  metminer_update_database_id_mapping(plantcyc_file, kegg_file, mapping)
}

metminer_save_idmapped_database <- function(updated_database, file) {
  obj <- updated_database$object
  object_name <- updated_database$object_name %||% "database"
  env <- new.env(parent = emptyenv())
  assign(object_name, obj, envir = env)
  save(list = object_name, file = file, envir = env)
  invisible(file)
}

metminer_idmap_row_ids <- function(info, source = c("plantcyc", "kegg")) {
  source <- match.arg(source)
  lab_id <- metminer_idmap_norm_id(metminer_idmap_first_col(info, c("Lab.ID", "Lab.IDs", "compound_id", "Compound.ID")))
  plant_id <- metminer_idmap_norm_id(metminer_idmap_first_col(info, c("BIOCYC.ID", "BioCyc.ID", "PlantCyc.ID", "plantcyc_id")))
  fill_plant <- !has_text(plant_id) & !grepl("^C[0-9]{5}$", lab_id %||% "", perl = TRUE)
  plant_id[fill_plant] <- lab_id[fill_plant]
  kegg_id <- metminer_idmap_norm_id(metminer_idmap_first_col(info, c("KEGG.ID", "KEGG", "kegg_id")))
  fill_kegg <- !has_text(kegg_id) & grepl("^C[0-9]{5}$", lab_id %||% "", perl = TRUE)
  kegg_id[fill_kegg] <- lab_id[fill_kegg]
  if (identical(source, "plantcyc")) plant_id else kegg_id
}

metminer_mapping_components <- function(mapping) {
  mapping <- as.data.frame(mapping %||% data.frame(), stringsAsFactors = FALSE)
  mapping <- mapping[has_text(mapping$PlantCyc.ID) & has_text(mapping$KEGG.ID), , drop = FALSE]
  if (nrow(mapping) == 0) return(list())
  nodes <- unique(c(paste0("P:", mapping$PlantCyc.ID), paste0("K:", mapping$KEGG.ID)))
  adjacency <- stats::setNames(vector("list", length(nodes)), nodes)
  for (i in seq_len(nrow(mapping))) {
    p <- paste0("P:", mapping$PlantCyc.ID[i])
    k <- paste0("K:", mapping$KEGG.ID[i])
    adjacency[[p]] <- unique(c(adjacency[[p]], k))
    adjacency[[k]] <- unique(c(adjacency[[k]], p))
  }
  visited <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  comps <- list()
  for (node in nodes) {
    if (isTRUE(visited[[node]])) next
    queue <- node
    members <- character()
    visited[[node]] <- TRUE
    while (length(queue) > 0) {
      cur <- queue[1]
      queue <- queue[-1]
      members <- c(members, cur)
      for (nb in adjacency[[cur]]) {
        if (!isTRUE(visited[[nb]])) {
          visited[[nb]] <- TRUE
          queue <- c(queue, nb)
        }
      }
    }
    comps[[length(comps) + 1L]] <- list(
      PlantCyc.ID = sub("^P:", "", members[grepl("^P:", members)]),
      KEGG.ID = sub("^K:", "", members[grepl("^K:", members)])
    )
  }
  comps
}

metminer_merge_text_values <- function(x, sep = " // ") {
  x <- unique(trimws(as.character(unlist(x, use.names = FALSE))))
  x <- x[has_text(x) & !is.na(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

metminer_merge_database_rows <- function(rows, all_cols) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (col in setdiff(all_cols, colnames(rows))) rows[[col]] <- NA
  rows <- rows[, all_cols, drop = FALSE]
  out <- rows[1, , drop = FALSE]
  for (col in all_cols) {
    vals <- rows[[col]]
    if (col %in% c("Synonyms", "source", "Submitter", "source_database", "source_lab_id", "source_compound_name", "match_type", "MetMiner.source_database", "MetMiner.source_lab_id")) {
      out[[col]] <- metminer_merge_text_values(vals, sep = " // ")
    } else {
      idx <- which(has_text(vals))
      out[[col]] <- if (length(idx) == 0) NA else vals[idx[1]]
    }
  }
  out
}

metminer_prefix_spectrum_names <- function(spectra_list, prefix) {
  if (length(spectra_list) == 0) return(spectra_list)
  old_names <- names(spectra_list)
  if (is.null(old_names)) old_names <- paste0("spectrum_", seq_along(spectra_list))
  names(spectra_list) <- make.unique(paste(prefix, old_names, sep = "__"))
  spectra_list
}

metminer_merge_spectra_by_lab_id <- function(source_objects, old_lab_ids, new_lab_id) {
  out <- list(Spectra.positive = list(), Spectra.negative = list())
  for (i in seq_along(source_objects)) {
    obj <- source_objects[[i]]
    old_ids <- old_lab_ids[[i]]
    if (!methods::is(obj, "databaseClass") || length(obj@spectra.data) == 0) next
    for (mode_name in intersect(names(out), names(obj@spectra.data))) {
      mode_data <- obj@spectra.data[[mode_name]]
      hit <- intersect(old_ids, names(mode_data))
      if (length(hit) == 0) next
      merged <- unlist(lapply(hit, function(id) {
        metminer_prefix_spectrum_names(mode_data[[id]], id)
      }), recursive = FALSE)
      out[[mode_name]] <- c(out[[mode_name]], merged)
    }
  }
  out <- lapply(out, function(x) {
    if (length(x) == 0) return(list())
    stats::setNames(list(x), new_lab_id)
  })
  out
}

metminer_combine_spectra_data <- function(rows_meta, plant_obj, kegg_obj) {
  has_spectra <- methods::is(plant_obj, "databaseClass") && length(plant_obj@spectra.data) > 0 ||
    methods::is(kegg_obj, "databaseClass") && length(kegg_obj@spectra.data) > 0
  if (!has_spectra) return(list())
  out <- list(Spectra.positive = list(), Spectra.negative = list())
  for (i in seq_len(nrow(rows_meta))) {
    merged <- metminer_merge_spectra_by_lab_id(
      source_objects = list(plant_obj, kegg_obj),
      old_lab_ids = list(strsplit(rows_meta$plant_old_lab_id[i] %||% "", " // ", fixed = TRUE)[[1]],
                         strsplit(rows_meta$kegg_old_lab_id[i] %||% "", " // ", fixed = TRUE)[[1]]),
      new_lab_id = rows_meta$new_lab_id[i]
    )
    for (mode_name in names(out)) {
      out[[mode_name]] <- c(out[[mode_name]], merged[[mode_name]])
    }
  }
  out
}

metminer_make_merged_database_info <- function(version = as.character(Sys.Date()), mode_label = "merged") {
  list(
    Version = version,
    Source = "Merged PlantCyc and KEGG database generated by MetMiner2",
    Link = "https://github.com/",
    Creater = "MetMiner2",
    Email = NA,
    RT = FALSE,
    Description = paste0("Merged ", mode_label, " database with non-redundant PlantCyc/KEGG compound IDs.")
  )
}

metminer_merge_plantcyc_kegg_database <- function(plantcyc_file,
                                                  kegg_file,
                                                  mapping,
                                                  lab_prefix = "MMDB",
                                                  mode_label = "database",
                                                  object_name = "merged_database") {
  mapping <- as.data.frame(mapping %||% data.frame(), stringsAsFactors = FALSE)
  first_db <- metminer_idmap_read_rda_object(plantcyc_file)
  second_db <- metminer_idmap_read_rda_object(kegg_file)
  oriented_db <- metminer_idmap_orient_databases(first_db, second_db)
  plant_db <- oriented_db$plantcyc
  kegg_db <- oriented_db$kegg
  plant_info <- metminer_idmap_object_spectra_info(plant_db$object)
  kegg_info <- metminer_idmap_object_spectra_info(kegg_db$object)
  plant_info$MetMiner.source_database <- "PlantCyc"
  kegg_info$MetMiner.source_database <- "KEGG"
  plant_info$MetMiner.source_lab_id <- metminer_idmap_first_col(plant_info, c("Lab.ID"))
  kegg_info$MetMiner.source_lab_id <- metminer_idmap_first_col(kegg_info, c("Lab.ID"))
  plant_ids <- metminer_idmap_row_ids(plant_info, "plantcyc")
  kegg_ids <- metminer_idmap_row_ids(kegg_info, "kegg")

  all_cols <- unique(c(colnames(plant_info), colnames(kegg_info), "PlantCyc.ID", "KEGG.ID", "MetMiner.merge_group"))
  components <- metminer_mapping_components(mapping)
  used_plant <- rep(FALSE, nrow(plant_info))
  used_kegg <- rep(FALSE, nrow(kegg_info))
  merged_rows <- list()
  meta_rows <- list()

  add_merged <- function(p_idx, k_idx, group_label) {
    p_rows <- plant_info[p_idx, , drop = FALSE]
    k_rows <- kegg_info[k_idx, , drop = FALSE]
    for (col in setdiff(all_cols, colnames(p_rows))) p_rows[[col]] <- rep(NA, nrow(p_rows))
    for (col in setdiff(all_cols, colnames(k_rows))) k_rows[[col]] <- rep(NA, nrow(k_rows))
    rows <- rbind(p_rows[, all_cols, drop = FALSE], k_rows[, all_cols, drop = FALSE])
    if (nrow(rows) == 0) return(NULL)
    merged <- metminer_merge_database_rows(rows, all_cols)
    merged$PlantCyc.ID <- metminer_idmap_collapse(plant_ids[p_idx])
    merged$KEGG.ID <- metminer_idmap_collapse(kegg_ids[k_idx])
    merged$MetMiner.merge_group <- group_label
    merged_rows[[length(merged_rows) + 1L]] <<- merged
    meta_rows[[length(meta_rows) + 1L]] <<- data.frame(
      row_index = length(merged_rows),
      plant_old_lab_id = metminer_merge_text_values(plant_info$MetMiner.source_lab_id[p_idx], sep = " // "),
      kegg_old_lab_id = metminer_merge_text_values(kegg_info$MetMiner.source_lab_id[k_idx], sep = " // "),
      stringsAsFactors = FALSE
    )
    if (length(p_idx) > 0) used_plant[p_idx] <<- TRUE
    if (length(k_idx) > 0) used_kegg[k_idx] <<- TRUE
    invisible(NULL)
  }

  if (length(components) > 0) {
    for (i in seq_along(components)) {
      p_idx <- which(plant_ids %in% components[[i]]$PlantCyc.ID)
      k_idx <- which(kegg_ids %in% components[[i]]$KEGG.ID)
      add_merged(p_idx, k_idx, paste0("mapped_", i))
    }
  }
  for (i in which(!used_plant)) add_merged(i, integer(), "plantcyc_only")
  for (i in which(!used_kegg)) add_merged(integer(), i, "kegg_only")

  spectra_info <- if (length(merged_rows) == 0) data.frame() else do.call(rbind, merged_rows)
  row_meta <- if (length(meta_rows) == 0) data.frame() else do.call(rbind, meta_rows)
  new_ids <- paste0(lab_prefix, sprintf("%06d", seq_len(nrow(spectra_info))))
  row_meta$new_lab_id <- new_ids
  spectra_info$Lab.ID <- new_ids
  spectra_info <- spectra_info[, unique(c("Lab.ID", "PlantCyc.ID", "KEGG.ID", setdiff(colnames(spectra_info), c("Lab.ID", "PlantCyc.ID", "KEGG.ID")))), drop = FALSE]
  rownames(spectra_info) <- NULL
  spectra_data <- metminer_combine_spectra_data(row_meta, plant_db$object, kegg_db$object)
  merged_obj <- methods::new(
    Class = "databaseClass",
    database.info = metminer_make_merged_database_info(mode_label = mode_label),
    spectra.info = as.data.frame(spectra_info, stringsAsFactors = FALSE),
    spectra.data = spectra_data
  )
  list(
    object = merged_obj,
    object_name = object_name,
    spectra_info = spectra_info,
    row_meta = row_meta,
    mapped_groups = sum(spectra_info$MetMiner.merge_group != "plantcyc_only" & spectra_info$MetMiner.merge_group != "kegg_only", na.rm = TRUE),
    plantcyc_only = sum(spectra_info$MetMiner.merge_group == "plantcyc_only", na.rm = TRUE),
    kegg_only = sum(spectra_info$MetMiner.merge_group == "kegg_only", na.rm = TRUE)
  )
}

metminer_save_merged_database <- function(merged_database, file) {
  obj <- merged_database$object
  object_name <- merged_database$object_name %||% "merged_database"
  env <- new.env(parent = emptyenv())
  assign(object_name, obj, envir = env)
  save(list = object_name, file = file, envir = env)
  invisible(file)
}

metminer_build_merged_ms_databases <- function(plantcyc_ms1_file,
                                               kegg_ms1_file,
                                               plantcyc_ms2_file,
                                               kegg_ms2_file,
                                               mass_ppm = 10,
                                               lab_prefix = "MMDB") {
  lab_prefix <- gsub("[^A-Za-z0-9]+", "_", as.character(lab_prefix %||% "MMDB"))
  lab_prefix <- gsub("^_+|_+$", "", lab_prefix)
  if (!has_text(lab_prefix)) lab_prefix <- "MMDB"
  ms1_mapping <- metminer_idmap_load_and_build(plantcyc_ms1_file, kegg_ms1_file, mass_ppm = mass_ppm)
  ms2_mapping <- metminer_idmap_load_and_build(plantcyc_ms2_file, kegg_ms2_file, mass_ppm = mass_ppm)
  ms1 <- metminer_merge_plantcyc_kegg_database(
    plantcyc_file = plantcyc_ms1_file,
    kegg_file = kegg_ms1_file,
    mapping = ms1_mapping$mapping,
    lab_prefix = paste0(lab_prefix, "_MS1_"),
    mode_label = "MS1",
    object_name = "merged_zma_ms1"
  )
  ms2 <- metminer_merge_plantcyc_kegg_database(
    plantcyc_file = plantcyc_ms2_file,
    kegg_file = kegg_ms2_file,
    mapping = ms2_mapping$mapping,
    lab_prefix = paste0(lab_prefix, "_MS2_"),
    mode_label = "MS2",
    object_name = "merged_zma_ms2"
  )
  list(ms1 = ms1, ms2 = ms2, ms1_mapping = ms1_mapping, ms2_mapping = ms2_mapping)
}
