# ---- KEGG REST helpers for conservative organism metabolite databases ----

#' Read a KEGG REST endpoint with simple file cache and throttling
#'
#' @noRd
metminer_kegg_rest_text <- function(endpoint,
                                    cache_dir,
                                    sleep_sec = 0.3,
                                    force = FALSE,
                                    max_retries = 3) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  key <- gsub("[^A-Za-z0-9_.-]+", "_", endpoint)
  cache_file <- file.path(cache_dir, paste0(key, ".txt"))
  if (!isTRUE(force) && file.exists(cache_file)) {
    return(paste(readLines(cache_file, warn = FALSE), collapse = "\n"))
  }
  url <- paste0("https://rest.kegg.jp/", endpoint)
  last_error <- NULL
  text <- NULL
  for (attempt in seq_len(max_retries)) {
    text <- tryCatch({
      if (requireNamespace("httr2", quietly = TRUE)) {
        resp <- httr2::request(url) |>
          httr2::req_user_agent("MetMiner KEGG organism database builder; cached respectful requests") |>
          httr2::req_timeout(30) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        if (httr2::resp_status(resp) >= 400) {
          stop("HTTP ", httr2::resp_status(resp))
        }
        httr2::resp_body_string(resp)
      } else {
        paste(readLines(url, warn = FALSE), collapse = "\n")
      }
    }, error = function(e) {
      last_error <<- e$message
      NULL
    })
    if (!is.null(text)) break
    Sys.sleep(min(30, sleep_sec * attempt * 4))
  }
  if (is.null(text)) {
    stop("KEGG REST request failed for ", endpoint, ": ", last_error, call. = FALSE)
  }
  writeLines(text, cache_file, useBytes = TRUE)
  Sys.sleep(sleep_sec)
  text
}

#' Parse a two-column KEGG link response
#'
#' @noRd
metminer_kegg_parse_link <- function(text, col1, col2) {
  if (is.null(text) || !nzchar(text)) {
    return(data.frame(stats::setNames(list(character(), character()), c(col1, col2)), stringsAsFactors = FALSE))
  }
  out <- utils::read.delim(text = text, header = FALSE, stringsAsFactors = FALSE)
  if (ncol(out) < 2) {
    return(data.frame(stats::setNames(list(character(), character()), c(col1, col2)), stringsAsFactors = FALSE))
  }
  out <- out[, 1:2, drop = FALSE]
  colnames(out) <- c(col1, col2)
  out[] <- lapply(out, trimws)
  out[] <- lapply(out, metminer_kegg_strip_id)
  unique(out)
}

#' Remove KEGG REST namespace prefixes from IDs
#'
#' @noRd
metminer_kegg_strip_id <- function(x) {
  x <- as.character(x)
  sub("^(path|cpd|rn|ko|ec):", "", x)
}

#' Parse KEGG organism list response
#'
#' @noRd
metminer_kegg_parse_organism_list <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(data.frame(
      t_number = character(), organism_code = character(), organism_name = character(),
      taxonomy = character(), stringsAsFactors = FALSE
    ))
  }
  out <- utils::read.delim(text = text, header = FALSE, stringsAsFactors = FALSE, quote = "", comment.char = "")
  if (ncol(out) < 4) {
    return(data.frame(
      t_number = character(), organism_code = character(), organism_name = character(),
      taxonomy = character(), stringsAsFactors = FALSE
    ))
  }
  out <- out[, 1:4, drop = FALSE]
  colnames(out) <- c("t_number", "organism_code", "organism_name", "taxonomy")
  out[] <- lapply(out, trimws)
  out
}

#' Fetch KEGG green plant organisms for organism-code selectors
#'
#' @noRd
metminer_kegg_green_plant_organisms <- function(cache_dir = file.path(tools::R_user_dir("MetMiner", which = "cache"), "kegg"),
                                                sleep_sec = 0.3,
                                                force = FALSE) {
  organisms <- metminer_kegg_parse_organism_list(
    metminer_kegg_rest_text("list/organism", cache_dir = cache_dir, sleep_sec = sleep_sec, force = force)
  )
  plants <- organisms[grepl("^Eukaryotes;Plants", organisms$taxonomy), , drop = FALSE]
  plants <- plants[order(plants$organism_name), , drop = FALSE]
  plants$display_name <- paste0(plants$organism_code, " - ", plants$organism_name)
  rownames(plants) <- NULL
  plants
}

#' Parse KEGG list/pathway/<org> response
#'
#' @noRd
metminer_kegg_parse_pathway_list <- function(text) {
  out <- metminer_kegg_parse_link(text, "pathway_id", "pathway_name")
  out$pathway_name <- sub(" - .*?$", "", out$pathway_name)
  out
}

#' Convert organism pathway IDs to reference map IDs
#'
#' @noRd
metminer_kegg_path_to_map <- function(pathway_id) {
  sub("^[a-z]{3,4}([0-9]{5})$", "map\\1", pathway_id)
}

#' Split KEGG compound names
#'
#' @noRd
metminer_kegg_split_names <- function(x) {
  x <- gsub("\n", " ", x)
  x <- unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE)
  trimws(x[nzchar(trimws(x))])
}

#' Parse KEGG compound flat files
#'
#' @noRd
metminer_kegg_parse_compound_flat <- function(text) {
  entries <- unlist(strsplit(text, "\n///", fixed = TRUE), use.names = FALSE)
  rows <- lapply(entries, function(entry) {
    lines <- strsplit(entry, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0) return(NULL)
    fields <- list()
    current <- NULL
    for (line in lines) {
      tag <- trimws(substr(line, 1, 12))
      value <- trimws(substr(line, 13, nchar(line)))
      if (nzchar(tag)) current <- tag
      if (!is.null(current)) {
        fields[[current]] <- paste(c(fields[[current]], value), collapse = " ")
      }
    }
    entry_id <- sub("\\s+.*$", "", fields$ENTRY %||% "")
    names <- metminer_kegg_split_names(fields$NAME %||% "")
    data.frame(
      compound_id = entry_id,
      compound_name = if (length(names) > 0) names[1] else NA_character_,
      synonyms = if (length(names) > 1) paste(names[-1], collapse = " // ") else NA_character_,
      formula = fields$FORMULA %||% NA_character_,
      exact_mass = suppressWarnings(as.numeric(fields$EXACT_MASS %||% NA_character_)),
      mol_weight = suppressWarnings(as.numeric(fields$MOL_WEIGHT %||% NA_character_)),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      compound_id = character(), compound_name = character(), synonyms = character(),
      formula = character(), exact_mass = numeric(), mol_weight = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  unique(do.call(rbind, rows))
}

#' Fetch KEGG compound metadata in chunks of 10
#'
#' @noRd
metminer_kegg_get_compounds <- function(compound_ids,
                                        cache_dir,
                                        sleep_sec = 0.3) {
  compound_ids <- unique(metminer_kegg_strip_id(compound_ids))
  compound_ids <- compound_ids[nzchar(compound_ids)]
  chunks <- split(compound_ids, ceiling(seq_along(compound_ids) / 10))
  rows <- lapply(chunks, function(ids) {
    endpoint <- paste0("get/", paste(ids, collapse = "+"))
    metminer_kegg_parse_compound_flat(metminer_kegg_rest_text(endpoint, cache_dir, sleep_sec))
  })
  unique(do.call(rbind, rows))
}

#' Filter KEGG compounds for LC-MS annotation background
#'
#' @noRd
metminer_filter_kegg_compounds <- function(compounds,
                                           min_mw = 70,
                                           max_mw = 1500,
                                           remove_no_carbon = TRUE,
                                           remove_currency = TRUE) {
  compounds$mz <- ifelse(is.na(compounds$exact_mass), compounds$mol_weight, compounds$exact_mass)
  reason <- rep("", nrow(compounds))
  add_reason <- function(idx, value) {
    idx[is.na(idx)] <- FALSE
    reason[idx] <<- ifelse(nzchar(reason[idx]), paste(reason[idx], value, sep = ";"), value)
  }
  add_reason(!nzchar(compounds$formula) | is.na(compounds$formula), "missing_formula")
  add_reason(is.na(compounds$mz), "missing_mass")
  add_reason(!is.na(compounds$mz) & compounds$mz < min_mw, "mw_below_min")
  add_reason(!is.na(compounds$mz) & compounds$mz > max_mw, "mw_above_max")
  if (isTRUE(remove_no_carbon)) {
    add_reason(nzchar(compounds$formula) & !grepl("C", compounds$formula, fixed = TRUE), "no_carbon_formula")
  }
  if (isTRUE(remove_currency)) {
    currency <- c(
      "C00001", "C00002", "C00003", "C00004", "C00005",
      "C00006", "C00007", "C00008", "C00009", "C00010",
      "C00011", "C00013", "C00014", "C00015", "C00016",
      "C00020", "C00035", "C00044", "C00061", "C00080"
    )
    add_reason(compounds$compound_id %in% currency, "currency_metabolite")
  }
  compounds$filter_reason <- reason
  list(
    clean_compounds = compounds[!nzchar(reason), setdiff(colnames(compounds), "filter_reason"), drop = FALSE],
    removed_compounds = compounds[nzchar(reason), , drop = FALSE]
  )
}

#' Create metid spectra.info for KEGG compounds
#'
#' @noRd
metminer_kegg_spectra_info <- function(compounds) {
  data.frame(
    Lab.ID = compounds$compound_id,
    Compound.name = compounds$compound_name,
    mz = as.numeric(compounds$mz),
    RT = NA_real_,
    CAS.ID = NA_character_,
    HMDB.ID = NA_character_,
    KEGG.ID = compounds$compound_id,
    Formula = compounds$formula,
    mz.pos = NA_real_,
    mz.neg = NA_real_,
    Submitter = "KEGG <https://www.kegg.jp/>",
    Synonyms = compounds$synonyms,
    monisotopic_molecular_weight = as.numeric(compounds$mz),
    SMILES.ID = NA_character_,
    INCHI.ID = NA_character_,
    INCHIKEY.ID = NA_character_,
    source = "KEGG reaction-supported organism database",
    stringsAsFactors = FALSE
  )
}

#' Build a KEGG MS1 databaseClass object
#'
#' @noRd
metminer_build_kegg_ms1_database <- function(compounds,
                                             organism_code,
                                             organism_name,
                                             version = as.character(Sys.Date())) {
  if (!requireNamespace("metid", quietly = TRUE)) {
    stop("Package 'metid' is required to construct databaseClass objects.", call. = FALSE)
  }
  methods::new(
    Class = "databaseClass",
    database.info = list(
      Version = version,
      Source = "KEGG <https://www.kegg.jp/>",
      Link = "https://www.kegg.jp/",
      Creater = "codex+shawn",
      Email = NA_character_,
      RT = FALSE,
      Organism = paste0(organism_name, " (", organism_code, ")"),
      Description = "Compounds inferred from organism gene/KO/EC-supported KEGG reactions, not from full reference pathway compound lists."
    ),
    spectra.info = metminer_kegg_spectra_info(compounds),
    spectra.data = list(Spectra.positive = list(), Spectra.negative = list())
  )
}

#' Split KEGG identifier fields from public MS2 databases
#'
#' @noRd
metminer_kegg_split_public_ids <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  ids <- unlist(strsplit(x, "\\{\\}|[;,[:space:]]+", perl = TRUE), use.names = FALSE)
  ids <- metminer_kegg_strip_id(trimws(ids))
  unique(ids[nzchar(ids)])
}

#' Normalize text for conservative KEGG-name MS2 fallback matching
#'
#' @noRd
metminer_kegg_normalize_name <- function(x) {
  x <- tolower(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

#' Build KEGG compound match indices for public MS2 extraction
#'
#' @noRd
metminer_kegg_build_match_index <- function(compounds) {
  base <- data.frame(
    kegg_id = compounds$compound_id,
    compound_name = compounds$compound_name,
    formula = compounds$formula,
    mz = as.numeric(compounds$mz),
    stringsAsFactors = FALSE
  )
  synonym_list <- strsplit(as.character(compounds$synonyms %||% rep("", nrow(compounds))), " // ", fixed = TRUE)
  name_rows <- vector("list", nrow(compounds))
  for (i in seq_len(nrow(compounds))) {
    names_i <- unique(c(compounds$compound_name[i], synonym_list[[i]]))
    names_i <- metminer_kegg_normalize_name(names_i)
    names_i <- names_i[nzchar(names_i)]
    name_rows[[i]] <- data.frame(kegg_id = compounds$compound_id[i], name_key = names_i, stringsAsFactors = FALSE)
  }
  name_index <- unique(do.call(rbind, name_rows))
  list(base = base, name_index = name_index)
}

#' Match KEGG compounds to a public MS2 spectra.info table
#'
#' @noRd
metminer_match_kegg_public_ms2 <- function(compounds,
                                           public_info,
                                           source_database,
                                           mass_tolerance_ppm = 10,
                                           mass_tolerance_da = 0.01) {
  idx <- metminer_kegg_build_match_index(compounds)
  base <- idx$base
  name_index <- idx$name_index
  public <- as.data.frame(public_info, stringsAsFactors = FALSE)
  public$source_row <- seq_len(nrow(public))
  public$Lab.ID <- as.character(public$Lab.ID)
  public_mz <- suppressWarnings(as.numeric(public$mz))
  public_name <- if ("Compound.name" %in% colnames(public)) as.character(public$Compound.name) else rep("", nrow(public))
  public_synonyms <- if ("Synonyms" %in% colnames(public)) as.character(public$Synonyms) else rep("", nrow(public))
  public_formula <- if ("Formula" %in% colnames(public)) as.character(public$Formula) else rep("", nrow(public))
  public_kegg <- if ("KEGG.ID" %in% colnames(public)) as.character(public$KEGG.ID) else rep("", nrow(public))

  rows <- list()
  add_matches <- function(row_i, candidates, match_type, rank) {
    if (nrow(candidates) == 0) return(invisible(NULL))
    candidates$public_mz <- public_mz[row_i]
    candidates$mass_error <- abs(candidates$mz - candidates$public_mz)
    candidates$mass_error_ppm <- candidates$mass_error * 1e6 / ifelse(candidates$mz < 400, 400, candidates$mz)
    candidates$formula_match <- nzchar(public_formula[row_i]) & candidates$formula == public_formula[row_i]
    if (match_type != "kegg_id") {
      candidates <- candidates[
        candidates$formula_match &
          (candidates$mass_error <= mass_tolerance_da | candidates$mass_error_ppm <= mass_tolerance_ppm),
        ,
        drop = FALSE
      ]
    }
    if (nrow(candidates) == 0) return(invisible(NULL))
    rows[[length(rows) + 1L]] <<- data.frame(
      kegg_id = candidates$kegg_id,
      public_row = public$source_row[row_i],
      source_database = source_database,
      source_lab_id = public$Lab.ID[row_i],
      source_compound_name = public_name[row_i],
      match_type = match_type,
      match_rank = rank,
      formula_match = candidates$formula_match,
      mass_error = candidates$mass_error,
      mass_error_ppm = candidates$mass_error_ppm,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  for (i in seq_len(nrow(public))) {
    kegg_ids <- metminer_kegg_split_public_ids(public_kegg[i])
    if (length(kegg_ids) > 0) {
      candidates <- base[base$kegg_id %in% kegg_ids, , drop = FALSE]
      add_matches(i, candidates, "kegg_id", 1L)
      if (nrow(candidates) > 0) next
    }

    names_i <- unique(c(public_name[i], unlist(strsplit(public_synonyms[i], "\\{\\}", perl = TRUE), use.names = FALSE)))
    name_keys <- metminer_kegg_normalize_name(names_i)
    name_keys <- name_keys[nzchar(name_keys)]
    if (length(name_keys) > 0) {
      hit_ids <- unique(name_index$kegg_id[name_index$name_key %in% name_keys])
      candidates <- base[base$kegg_id %in% hit_ids, , drop = FALSE]
      add_matches(i, candidates, "name_formula_mass", 2L)
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      kegg_id = character(), public_row = integer(), source_database = character(),
      source_lab_id = character(), source_compound_name = character(), match_type = character(),
      match_rank = integer(), formula_match = logical(), mass_error = numeric(),
      mass_error_ppm = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$public_row, out$match_rank, out$mass_error_ppm), , drop = FALSE]
  out[!duplicated(paste(out$public_row, out$kegg_id)), , drop = FALSE]
}

#' Add one MS2 spectrum to a KEGG spectra.data list
#'
#' @noRd
metminer_kegg_add_spectrum <- function(spectra_data, polarity, kegg_id, ce_label, spectrum) {
  polarity_name <- if (tolower(polarity) == "negative") "Spectra.negative" else "Spectra.positive"
  if (is.null(spectra_data[[polarity_name]][[kegg_id]])) {
    spectra_data[[polarity_name]][[kegg_id]] <- list()
  }
  ce_label <- gsub("[^A-Za-z0-9_.+-]+", "_", ce_label)
  if (ce_label %in% names(spectra_data[[polarity_name]][[kegg_id]])) {
    ce_label <- paste0(ce_label, "__", length(spectra_data[[polarity_name]][[kegg_id]]) + 1L)
  }
  spectrum <- as.data.frame(spectrum)
  spectrum$mz <- suppressWarnings(as.numeric(spectrum$mz))
  spectrum$intensity <- suppressWarnings(as.numeric(spectrum$intensity))
  spectrum <- spectrum[!is.na(spectrum$mz) & !is.na(spectrum$intensity), c("mz", "intensity"), drop = FALSE]
  if (nrow(spectrum) > 0) {
    spectra_data[[polarity_name]][[kegg_id]][[ce_label]] <- spectrum
  }
  spectra_data
}

#' Build a KEGG-linked MS2 databaseClass object from massdbbuildin
#'
#' @noRd
metminer_build_kegg_ms2_database <- function(compounds,
                                             organism_code,
                                             organism_name,
                                             version = as.character(Sys.Date()),
                                             builtin_databases = c("hmdb_ms2", "massbank_ms2", "mona_ms2"),
                                             mass_tolerance_ppm = 10,
                                             mass_tolerance_da = 0.01) {
  if (!requireNamespace("metid", quietly = TRUE)) {
    stop("Package 'metid' is required to construct databaseClass objects.", call. = FALSE)
  }
  if (!requireNamespace("massdbbuildin", quietly = TRUE)) {
    stop("Package 'massdbbuildin' is required to extract built-in MS2 spectra.", call. = FALSE)
  }

  kegg_info <- metminer_kegg_spectra_info(compounds)
  all_matches <- list()
  spectra_data <- list(Spectra.positive = list(), Spectra.negative = list())

  for (db_id in builtin_databases) {
    env <- new.env(parent = emptyenv())
    utils::data(list = db_id, package = "massdbbuildin", envir = env)
    public_db <- get(db_id, envir = env)
    source_database <- public_db@database.info$Source %||% db_id
    public_info <- as.data.frame(public_db@spectra.info, stringsAsFactors = FALSE)
    matches <- metminer_match_kegg_public_ms2(
      compounds = compounds,
      public_info = public_info,
      source_database = source_database,
      mass_tolerance_ppm = mass_tolerance_ppm,
      mass_tolerance_da = mass_tolerance_da
    )
    if (nrow(matches) == 0) next
    all_matches[[source_database]] <- matches

    for (i in seq_len(nrow(matches))) {
      public_row <- matches$public_row[i]
      source_lab_id <- matches$source_lab_id[i]
      polarity <- public_info$Polarity[public_row] %||% NA_character_
      polarity <- ifelse(tolower(polarity) == "negative", "negative", "positive")
      source_spectra <- public_db@spectra.data[[if (polarity == "negative") "Spectra.negative" else "Spectra.positive"]][[source_lab_id]]
      if (is.null(source_spectra) || length(source_spectra) == 0) next
      for (ce_name in names(source_spectra)) {
        ce_label <- paste(source_database, source_lab_id, ce_name %||% "Unknown", sep = "__")
        spectra_data <- metminer_kegg_add_spectrum(
          spectra_data = spectra_data,
          polarity = polarity,
          kegg_id = matches$kegg_id[i],
          ce_label = ce_label,
          spectrum = source_spectra[[ce_name]]
        )
      }
    }
  }

  match_log <- if (length(all_matches) > 0) do.call(rbind, all_matches) else
    data.frame(
      kegg_id = character(), public_row = integer(), source_database = character(),
      source_lab_id = character(), source_compound_name = character(), match_type = character(),
      match_rank = integer(), formula_match = logical(), mass_error = numeric(),
      mass_error_ppm = numeric(), stringsAsFactors = FALSE
    )
  rownames(match_log) <- NULL

  ms2_ids <- unique(c(names(spectra_data$Spectra.positive), names(spectra_data$Spectra.negative)))
  spectra_info <- kegg_info[kegg_info$Lab.ID %in% ms2_ids, , drop = FALSE]
  if (nrow(spectra_info) > 0 && nrow(match_log) > 0) {
    provenance <- lapply(split(match_log, match_log$kegg_id), function(x) {
      data.frame(
        Lab.ID = x$kegg_id[1],
        source_database = metminer_plantcyc_collapse_unique(x$source_database),
        source_lab_id = metminer_plantcyc_collapse_unique(x$source_lab_id),
        source_compound_name = metminer_plantcyc_collapse_unique(x$source_compound_name),
        match_type = metminer_plantcyc_collapse_unique(x$match_type),
        stringsAsFactors = FALSE
      )
    })
    provenance <- do.call(rbind, provenance)
    spectra_info <- merge(spectra_info, provenance, by = "Lab.ID", all.x = TRUE, sort = FALSE)
  }

  database <- methods::new(
    Class = "databaseClass",
    database.info = list(
      Version = version,
      Source = "KEGG <https://www.kegg.jp/> + massdbbuildin MS2",
      Link = "https://www.kegg.jp/",
      Creater = "codex+shawn",
      Email = NA_character_,
      RT = FALSE,
      Organism = paste0(organism_name, " (", organism_code, ")"),
      Description = "MS2 spectra extracted from massdbbuildin by KEGG.ID, with conservative name/formula/mass fallback."
    ),
    spectra.info = as.data.frame(spectra_info, stringsAsFactors = FALSE),
    spectra.data = spectra_data
  )

  list(
    database = database,
    match_log = match_log,
    unmatched_compounds = kegg_info[!(kegg_info$Lab.ID %in% ms2_ids), , drop = FALSE]
  )
}

#' Build a KEGG metpath pathway database object
#'
#' @noRd
metminer_build_kegg_pathway_database <- function(pathway_compound_map,
                                                 pathway_reaction_map,
                                                 organism_code,
                                                 organism_name,
                                                 version = as.character(Sys.Date())) {
  if (!requireNamespace("metpath", quietly = TRUE)) {
    stop("Package 'metpath' is required to construct pathway_database objects.", call. = FALSE)
  }
  pathway_compound_map <- unique(pathway_compound_map[, c("pathway_id", "pathway_name", "compound_id", "compound_name"), drop = FALSE])
  pathway_compound_map <- pathway_compound_map[order(pathway_compound_map$pathway_id, pathway_compound_map$compound_id), , drop = FALSE]
  pathway_unique <- unique(pathway_compound_map[, c("pathway_id", "pathway_name"), drop = FALSE])
  pathway_ids <- pathway_unique$pathway_id
  pathway_names <- pathway_unique$pathway_name
  names(pathway_names) <- pathway_ids
  compound_list <- lapply(pathway_ids, function(pathway_id) {
    rows <- unique(pathway_compound_map[pathway_compound_map$pathway_id == pathway_id, c("compound_id", "compound_name"), drop = FALSE])
    rows <- rows[order(rows$compound_id), , drop = FALSE]
    colnames(rows) <- c("KEGG.ID", "compound_name")
    rownames(rows) <- NULL
    rows
  })
  names(compound_list) <- pathway_ids
  describtion <- lapply(pathway_ids, function(pathway_id) {
    reactions <- unique(pathway_reaction_map$reaction_id[pathway_reaction_map$pathway_id == pathway_id])
    paste(reactions, collapse = " // ")
  })
  names(describtion) <- pathway_ids
  pathway_class <- as.list(rep("KEGG pathway", length(pathway_ids)))
  names(pathway_class) <- pathway_ids
  empty_gene_df <- data.frame(gene_id = character(0), gene_name = character(0), stringsAsFactors = FALSE)
  gene_list <- replicate(length(pathway_ids), empty_gene_df, simplify = FALSE)
  names(gene_list) <- pathway_ids
  methods::new(
    "pathway_database",
    database_info = list(
      source = "KEGG <https://www.kegg.jp/>",
      version = version,
      source_url = "https://www.kegg.jp/",
      organism = paste0(organism_name, " (", organism_code, ")"),
      creater = "codex+shawn",
      email = NA_character_,
      description = "KEGG pathway database filtered to compounds from organism gene/KO/EC-supported reactions.",
      date_created = as.character(Sys.Date())
    ),
    pathway_id = pathway_ids,
    pathway_name = pathway_names,
    describtion = describtion,
    pathway_class = pathway_class,
    gene_list = gene_list,
    compound_list = compound_list,
    protein_list = gene_list,
    reference_list = list(),
    related_disease = list(),
    related_module = list()
  )
}

#' Count unique values safely
#'
#' @noRd
metminer_kegg_n_unique <- function(x) {
  length(unique(x[!is.na(x) & nzchar(as.character(x))]))
}

#' Build pathway-level QC metrics for KEGG organism pathways
#'
#' @noRd
metminer_kegg_pathway_qc <- function(pathways,
                                     path_rn,
                                     pathway_reaction_map,
                                     pathway_compound_map,
                                     min_reaction_coverage = 0.10,
                                     min_supported_reactions = 5,
                                     min_pathway_specific_compounds = 3,
                                     hub_compound_frequency_cutoff = 0.10) {
  if (nrow(pathway_compound_map) == 0) {
    return(data.frame())
  }

  ref_rn <- path_rn[grepl("^map", path_rn$pathway_id), , drop = FALSE]
  ref_rn$map_id <- ref_rn$pathway_id
  ref_counts <- aggregate(reaction_id ~ map_id, ref_rn, metminer_kegg_n_unique)
  colnames(ref_counts) <- c("map_id", "reference_reactions")

  supported_counts <- aggregate(reaction_id ~ pathway_id + map_id, pathway_reaction_map, metminer_kegg_n_unique)
  colnames(supported_counts) <- c("pathway_id", "map_id", "supported_reactions")

  compound_counts <- aggregate(compound_id ~ pathway_id, pathway_compound_map, metminer_kegg_n_unique)
  colnames(compound_counts) <- c("pathway_id", "supported_compounds")

  pathway_total <- length(unique(pathway_compound_map$pathway_id))
  compound_freq <- aggregate(pathway_id ~ compound_id, unique(pathway_compound_map[, c("compound_id", "pathway_id"), drop = FALSE]), metminer_kegg_n_unique)
  colnames(compound_freq) <- c("compound_id", "pathway_count")
  compound_freq$pathway_frequency <- compound_freq$pathway_count / max(pathway_total, 1)
  specific_ids <- compound_freq$compound_id[compound_freq$pathway_frequency <= hub_compound_frequency_cutoff]
  specific_map <- pathway_compound_map[pathway_compound_map$compound_id %in% specific_ids, , drop = FALSE]
  specific_counts <- aggregate(compound_id ~ pathway_id, specific_map, metminer_kegg_n_unique)
  colnames(specific_counts) <- c("pathway_id", "pathway_specific_compounds")

  qc <- merge(pathways, supported_counts, by = "pathway_id", all.x = FALSE)
  qc <- merge(qc, ref_counts, by = "map_id", all.x = TRUE)
  qc <- merge(qc, compound_counts, by = "pathway_id", all.x = TRUE)
  qc <- merge(qc, specific_counts, by = "pathway_id", all.x = TRUE)
  qc$reference_reactions[is.na(qc$reference_reactions)] <- 0
  qc$supported_compounds[is.na(qc$supported_compounds)] <- 0
  qc$pathway_specific_compounds[is.na(qc$pathway_specific_compounds)] <- 0
  qc$reaction_coverage <- ifelse(qc$reference_reactions > 0, qc$supported_reactions / qc$reference_reactions, NA_real_)

  reasons <- vector("list", nrow(qc))
  add_reason <- function(idx, reason) {
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) return(invisible(NULL))
    reasons[idx] <<- Map(c, reasons[idx], reason)
    invisible(NULL)
  }
  add_reason(!is.na(qc$reaction_coverage) & qc$reaction_coverage < min_reaction_coverage, "low_reaction_coverage")
  add_reason(qc$supported_reactions < min_supported_reactions, "few_supported_reactions")
  add_reason(qc$pathway_specific_compounds < min_pathway_specific_compounds, "few_pathway_specific_compounds")

  qc$review_reason <- vapply(reasons, function(x) {
    x <- unique(x)
    if (length(x) == 0) "" else paste(x, collapse = ";")
  }, character(1))
  qc$review_flag <- nzchar(qc$review_reason)
  qc <- qc[order(qc$review_flag, qc$reaction_coverage, qc$supported_reactions, decreasing = FALSE), , drop = FALSE]
  rownames(qc) <- NULL
  qc
}

#' Collapse KEGG evidence values for prompt display
#'
#' @noRd
metminer_kegg_prompt_collapse <- function(x, n = 30) {
  x <- unique(as.character(x[!is.na(x) & nzchar(as.character(x))]))
  if (length(x) == 0) return("None")
  if (length(x) > n) {
    return(paste0(paste(head(x, n), collapse = ", "), ", ... (", length(x), " total)"))
  }
  paste(x, collapse = ", ")
}

#' Build an external AI review prompt for weakly supported KEGG pathways
#'
#' @noRd
metminer_kegg_pathway_review_prompt <- function(pathway_qc,
                                                pathway_compound_map,
                                                pathway_reaction_map,
                                                organism_code,
                                                organism_name,
                                                max_pathways = 25) {
  review <- pathway_qc[pathway_qc$review_flag, , drop = FALSE]
  review <- review[order(review$reaction_coverage, review$supported_reactions), , drop = FALSE]
  if (nrow(review) > max_pathways) {
    review <- review[seq_len(max_pathways), , drop = FALSE]
  }

  header <- c(
    "# KEGG Pathway Biological Plausibility Review Prompt",
    "",
    "You are reviewing KEGG organism-specific metabolic pathways for LC-MS pathway enrichment background construction.",
    "Decide whether each pathway should be kept, removed, or marked for manual review.",
    "",
    "Important constraints:",
    "- Use only the structured evidence provided below.",
    "- You may use literature evidence to judge whether the pathway is biologically plausible in this organism or closely related taxa.",
    "- Literature evidence must come from traceable scholarly sources such as Google Scholar, Crossref, PubMed, Web of Science, journal websites, or DOI-resolved publisher pages.",
    "- Double check each cited source before using it as evidence: verify the title, authors, year, journal or source, DOI or URL, and whether the paper actually supports the pathway/species claim.",
    "- Do not use uncited general biological knowledge as decisive evidence. If literature support cannot be verified, state that clearly and keep the decision conservative.",
    "- If literature evidence affects the decision, cite it in the reason field using compact author-year-DOI style, for example: Wang et al., 2020, DOI:10.xxxx/xxxxx.",
    "- Also put all cited literature in the references array for that pathway. Each reference must include title, authors, year, source, DOI or URL, and a short support_note.",
    "- Do not use opaque source tags such as NCBI +1, PMC +1, web result, search result, or source bubble labels as citations.",
    "- A pathway with only common precursors or early upstream intermediates should usually be removed from the enrichment background.",
    "- Return only a valid JSON array inside one json code block. Do not include prose before or after the JSON code block.",
    "",
    paste0("Organism: ", organism_name, " (", organism_code, ")"),
    paste0("Number of pathways requiring review in this prompt: ", nrow(review)),
    "",
    "Requested JSON schema:",
    "```json",
    "[",
    "  {",
    "    \"pathway_id\": \"zma00950\",",
    "    \"decision\": \"keep | remove | review\",",
    "    \"confidence\": \"high | medium | low\",",
    "    \"reason_category\": \"sufficient_support | low_reaction_coverage | upstream_only_support | common_precursor_only | insufficient_evidence | other\",",
    "    \"reason\": \"Short evidence-based explanation. If literature is used, cite as Author et al., Year, DOI:xxxxx.\",",
    "    \"recommended_action\": \"Use in enrichment | Exclude from enrichment | Keep for manual curation\",",
    "    \"references\": [",
    "      {",
    "        \"title\": \"Full paper title\",",
    "        \"authors\": \"First author et al. or full author list\",",
    "        \"year\": \"2020\",",
    "        \"source\": \"Journal or database source\",",
    "        \"doi\": \"10.xxxx/xxxxx\",",
    "        \"url\": \"https://doi.org/10.xxxx/xxxxx\",",
    "        \"support_note\": \"What this source supports for the pathway/species decision\"",
    "      }",
    "    ]",
    "  }",
    "]",
    "```",
    ""
  )

  blocks <- lapply(seq_len(nrow(review)), function(i) {
    pathway_id <- review$pathway_id[i]
    compounds <- unique(pathway_compound_map[pathway_compound_map$pathway_id == pathway_id, c("compound_id", "compound_name"), drop = FALSE])
    compound_text <- paste0(compounds$compound_id, " ", compounds$compound_name)
    reactions <- pathway_reaction_map[pathway_reaction_map$pathway_id == pathway_id, , drop = FALSE]
    c(
      paste0("## ", pathway_id, " - ", review$pathway_name[i]),
      "",
      paste0("- Review reasons: ", review$review_reason[i]),
      paste0("- Reference reactions: ", review$reference_reactions[i]),
      paste0("- Supported reactions: ", review$supported_reactions[i]),
      paste0("- Reaction coverage: ", signif(review$reaction_coverage[i], 3)),
      paste0("- Supported compounds: ", review$supported_compounds[i]),
      paste0("- Pathway-specific compounds after hub filtering: ", review$pathway_specific_compounds[i]),
      paste0("- Supported reaction IDs: ", metminer_kegg_prompt_collapse(reactions$reaction_id, 40)),
      paste0("- Supported gene IDs: ", metminer_kegg_prompt_collapse(reactions$gene_id, 40)),
      paste0("- Supported compounds: ", metminer_kegg_prompt_collapse(compound_text, 60)),
      "",
      "Question: Should this pathway be kept in the organism-specific enrichment background, removed, or marked for manual review?",
      ""
    )
  })

  paste(c(header, unlist(blocks, use.names = FALSE)), collapse = "\n")
}

#' Parse AI pathway review JSON exported from an external LLM
#'
#' @noRd
metminer_kegg_parse_ai_review_json <- function(file) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to parse AI review JSON.", call. = FALSE)
  }
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  text <- trimws(text)
  if (!nzchar(text)) {
    stop("The uploaded AI review JSON file is empty.", call. = FALSE)
  }

  parse_json <- function(x) {
    jsonlite::fromJSON(x, simplifyDataFrame = TRUE, flatten = TRUE)
  }
  parsed <- tryCatch(parse_json(text), error = function(e) NULL)
  if (is.null(parsed)) {
    fenced <- regmatches(text, regexpr("```(?:json)?\\s*[\\s\\S]*?```", text, perl = TRUE))
    if (length(fenced) > 0 && nzchar(fenced)) {
      fenced <- sub("^```(?:json)?\\s*", "", fenced, perl = TRUE)
      fenced <- sub("\\s*```$", "", fenced, perl = TRUE)
      parsed <- tryCatch(parse_json(fenced), error = function(e) NULL)
    }
  }
  if (is.null(parsed)) {
    lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines)]
    rows <- lapply(lines, function(line) tryCatch(parse_json(line), error = function(e) NULL))
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) > 0) {
      parsed <- rows
    }
  }
  if (is.null(parsed)) {
    stop("Unable to parse the uploaded file as JSON. Please upload a JSON array or JSON-lines file.", call. = FALSE)
  }

  if (is.list(parsed) && !is.data.frame(parsed) && all(c("pathway_id", "decision") %in% names(parsed))) {
    parsed <- as.data.frame(parsed, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (is.list(parsed) && !is.data.frame(parsed)) {
    candidate_names <- intersect(c("reviews", "pathways", "results", "items", "data"), names(parsed))
    if (length(candidate_names) > 0) parsed <- parsed[[candidate_names[1]]]
  }
  review <- if (is.data.frame(parsed)) {
    parsed
  } else if (is.list(parsed)) {
    rows <- lapply(parsed, function(x) {
      if (is.data.frame(x)) return(x)
      if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
      NULL
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  } else {
    data.frame()
  }
  if (nrow(review) == 0) {
    stop("No pathway review rows were found in the uploaded JSON.", call. = FALSE)
  }

  colnames(review) <- gsub("[. ]+", "_", tolower(colnames(review)))
  required <- c("pathway_id", "decision")
  missing_cols <- setdiff(required, colnames(review))
  if (length(missing_cols) > 0) {
    stop("AI review JSON must contain columns: ", paste(required, collapse = ", "), call. = FALSE)
  }
  optional <- c("confidence", "reason_category", "reason", "recommended_action")
  for (col in optional) {
    if (!col %in% colnames(review)) review[[col]] <- NA_character_
  }
  review <- review[, unique(c(required, optional, colnames(review))), drop = FALSE]
  review$pathway_id <- trimws(as.character(review$pathway_id))
  review$decision <- tolower(trimws(as.character(review$decision)))
  review$decision[review$decision %in% c("retain", "kept", "include", "included", "use")] <- "keep"
  review$decision[review$decision %in% c("exclude", "excluded", "drop", "discard")] <- "remove"
  review$decision[!review$decision %in% c("keep", "remove", "review")] <- "review"
  review <- review[nzchar(review$pathway_id), , drop = FALSE]
  review[!duplicated(review$pathway_id), , drop = FALSE]
}

#' Escape text for compact DT child-row HTML
#'
#' @noRd
metminer_kegg_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Collapse references for AI review detail display
#'
#' @noRd
metminer_kegg_reference_summary <- function(row) {
  if ("references" %in% colnames(row) && is.list(row$references)) {
    refs <- row$references[[1]]
    if (is.data.frame(refs) && nrow(refs) > 0) {
      one_ref <- refs[1, , drop = FALSE]
      title <- one_ref$title %||% ""
      doi <- one_ref$doi %||% ""
      more <- if (nrow(refs) > 1) paste0(" +", nrow(refs) - 1, " more") else ""
      return(paste(c(title, doi, more)[nzchar(c(title, doi, more))], collapse = "; "))
    }
  }
  ref_cols <- grep("^references[._]", colnames(row), value = TRUE)
  if (length(ref_cols) == 0) return("")
  values <- unlist(row[, ref_cols, drop = FALSE], use.names = TRUE)
  values <- values[!is.na(values) & nzchar(as.character(values))]
  if (length(values) == 0) return("")
  doi_cols <- grep("doi$", names(values), ignore.case = TRUE, value = TRUE)
  title_cols <- grep("title$", names(values), ignore.case = TRUE, value = TRUE)
  doi <- if (length(doi_cols) > 0) as.character(values[[doi_cols[1]]]) else ""
  title <- if (length(title_cols) > 0) as.character(values[[title_cols[1]]]) else ""
  paste(c(title, doi)[nzchar(c(title, doi))], collapse = "; ")
}

#' Build model-level detail HTML for one pathway
#'
#' @noRd
metminer_kegg_ai_detail_html <- function(rows) {
  if (nrow(rows) == 0) return("")
  rows$model <- rows$model %||% "AI"
  detail_rows <- vapply(seq_len(nrow(rows)), function(i) {
    reference_text <- metminer_kegg_reference_summary(rows[i, , drop = FALSE])
    paste0(
      "<tr>",
      "<td>", metminer_kegg_html_escape(rows$model[i]), "</td>",
      "<td>", metminer_kegg_html_escape(rows$decision[i]), "</td>",
      "<td>", metminer_kegg_html_escape(rows$confidence[i]), "</td>",
      "<td>", metminer_kegg_html_escape(rows$reason_category[i]), "</td>",
      "<td>", metminer_kegg_html_escape(rows$reason[i]), "</td>",
      "<td>", metminer_kegg_html_escape(reference_text), "</td>",
      "</tr>"
    )
  }, character(1))
  paste0(
    "<table class='table table-sm table-bordered mb-0'>",
    "<thead><tr>",
    "<th>Model</th><th>Decision</th><th>Confidence</th><th>Category</th><th>Reason</th><th>References</th>",
    "</tr></thead><tbody>",
    paste(detail_rows, collapse = ""),
    "</tbody></table>"
  )
}

#' Merge multiple AI review files with KEGG pathway QC for manual curation
#'
#' @noRd
metminer_kegg_prepare_multi_review_curation <- function(pathway_qc, ai_review) {
  if (!"model" %in% colnames(ai_review)) ai_review$model <- "AI"
  ai_review$model <- as.character(ai_review$model)
  ai_review$model[is.na(ai_review$model) | !nzchar(ai_review$model)] <- "AI"
  review_pathway_ids <- unique(ai_review$pathway_id[!is.na(ai_review$pathway_id) & nzchar(ai_review$pathway_id)])
  if (is.null(pathway_qc) || nrow(pathway_qc) == 0 || !"pathway_id" %in% colnames(pathway_qc)) {
    pathway_qc <- data.frame(pathway_id = review_pathway_ids, stringsAsFactors = FALSE)
  } else {
    pathway_qc <- pathway_qc[pathway_qc$pathway_id %in% review_pathway_ids, , drop = FALSE]
    missing_ids <- setdiff(review_pathway_ids, pathway_qc$pathway_id)
    if (length(missing_ids) > 0) {
      filler <- as.data.frame(matrix(NA, nrow = length(missing_ids), ncol = ncol(pathway_qc)), stringsAsFactors = FALSE)
      colnames(filler) <- colnames(pathway_qc)
      filler$pathway_id <- missing_ids
      pathway_qc <- rbind(pathway_qc, filler)
    }
  }
  if (!"pathway_name" %in% colnames(pathway_qc)) pathway_qc$pathway_name <- pathway_qc$pathway_id
  pathway_qc$pathway_name[is.na(pathway_qc$pathway_name) | !nzchar(as.character(pathway_qc$pathway_name))] <-
    pathway_qc$pathway_id[is.na(pathway_qc$pathway_name) | !nzchar(as.character(pathway_qc$pathway_name))]
  if (!"review_flag" %in% colnames(pathway_qc)) pathway_qc$review_flag <- TRUE
  if (!"reaction_coverage" %in% colnames(pathway_qc)) pathway_qc$reaction_coverage <- NA_real_
  if (!"supported_reactions" %in% colnames(pathway_qc)) pathway_qc$supported_reactions <- NA_integer_
  if (!"pathway_specific_compounds" %in% colnames(pathway_qc)) pathway_qc$pathway_specific_compounds <- NA_integer_

  summaries <- lapply(seq_len(nrow(pathway_qc)), function(i) {
    pathway_id <- pathway_qc$pathway_id[i]
    rows <- ai_review[ai_review$pathway_id == pathway_id, , drop = FALSE]
    decisions <- rows$decision
    keep_n <- sum(decisions == "keep", na.rm = TRUE)
    remove_n <- sum(decisions == "remove", na.rm = TRUE)
    review_n <- sum(decisions == "review", na.rm = TRUE)
    counts <- c(keep = keep_n, remove = remove_n, review = review_n)
    max_n <- max(counts)
    winners <- names(counts)[counts == max_n]
    consensus <- if (length(rows$decision) == 0) {
      if (isTRUE(pathway_qc$review_flag[i])) "review" else "keep"
    } else if (length(winners) == 1) {
      winners
    } else {
      "review"
    }
    data.frame(
      pathway_id = pathway_id,
      models_reviewed = nrow(rows),
      keep_n = keep_n,
      remove_n = remove_n,
      review_n = review_n,
      consensus_decision = consensus,
      consensus_note = if (length(winners) > 1) "tie_or_conflict" else "majority_vote",
      ai_detail_html = metminer_kegg_ai_detail_html(rows),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summaries)
  out <- merge(pathway_qc, summary, by = "pathway_id", all.x = TRUE, sort = FALSE)
  out$retain_default <- out$consensus_decision != "remove"
  out[order(out$retain_default, out$review_flag, out$reaction_coverage, decreasing = FALSE), , drop = FALSE]
}

#' Merge AI review decisions with KEGG pathway QC for manual curation
#'
#' @noRd
metminer_kegg_prepare_review_curation <- function(pathway_qc, ai_review) {
  base <- pathway_qc
  ai_cols <- c("pathway_id", "decision", "confidence", "reason_category", "reason", "recommended_action")
  ai <- ai_review[, intersect(ai_cols, colnames(ai_review)), drop = FALSE]
  out <- merge(base, ai, by = "pathway_id", all.x = TRUE, sort = FALSE)
  missing_decision <- is.na(out$decision)
  out$decision[missing_decision] <- ifelse(out$review_flag[missing_decision], "review", "keep")
  out$confidence[is.na(out$confidence)] <- ""
  out$reason_category[is.na(out$reason_category)] <- ""
  out$reason[is.na(out$reason)] <- ""
  out$recommended_action[is.na(out$recommended_action)] <- ""
  out$retain_default <- out$decision != "remove"
  out[order(out$retain_default, out$review_flag, out$reaction_coverage, decreasing = FALSE), , drop = FALSE]
}

#' Build and save a curated KEGG pathway database from retained pathway IDs
#'
#' @noRd
metminer_kegg_save_curated_pathway_database <- function(result,
                                                        retained_pathway_ids,
                                                        organism_code,
                                                        organism_name,
                                                        output_dir = result$output_dir,
                                                        version = as.character(Sys.Date())) {
  retained_pathway_ids <- unique(as.character(retained_pathway_ids))
  if (length(retained_pathway_ids) == 0) {
    stop("No KEGG pathway is selected for retention.", call. = FALSE)
  }
  pathway_compound_map <- result$pathway_compound_map[result$pathway_compound_map$pathway_id %in% retained_pathway_ids, , drop = FALSE]
  pathway_reaction_map <- result$pathway_reaction_map[result$pathway_reaction_map$pathway_id %in% retained_pathway_ids, , drop = FALSE]
  curated_db <- metminer_build_kegg_pathway_database(
    pathway_compound_map = pathway_compound_map,
    pathway_reaction_map = pathway_reaction_map,
    organism_code = organism_code,
    organism_name = organism_name,
    version = version
  )
  object_name <- paste0("kegg_", organism_code, "_pathway_curated")
  file_name <- paste0("kegg_", organism_code, "_pathway_curated.rda")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  env <- new.env(parent = emptyenv())
  assign(object_name, curated_db, envir = env)
  save(list = object_name, file = file.path(output_dir, file_name), envir = env)
  utils::write.table(
    data.frame(pathway_id = retained_pathway_ids, stringsAsFactors = FALSE),
    file.path(output_dir, paste0("kegg_", organism_code, "_retained_pathways.tsv")),
    sep = "\t", quote = FALSE, row.names = FALSE, na = ""
  )
  list(database = curated_db, file_name = file_name, retained_pathway_ids = retained_pathway_ids)
}

#' Build a conservative KEGG organism metabolite database
#'
#' @noRd
metminer_kegg_existing_database_files <- function(output_dir, organism_code) {
  prefix <- paste0("kegg_", organism_code)
  required <- file.path(output_dir, paste0(prefix, c("_ms1.rda", "_ms2.rda", "_pathway.rda")))
  list(
    exists = all(file.exists(required)),
    required = required,
    optional = file.path(output_dir, paste0(prefix, c(
      "_clean_compounds.tsv",
      "_removed_compounds.tsv",
      "_pathway_reaction_map.tsv",
      "_pathway_compound_map.tsv",
      "_pathway_qc.tsv",
      "_pathway_review_prompt.md",
      "_ms2_match_log.tsv",
      "_ms2_unmatched_compounds.tsv",
      "_summary.tsv"
    )))
  )
}

metminer_kegg_load_database_object <- function(file) {
  env <- new.env(parent = emptyenv())
  object_names <- load(file, envir = env)
  for (object_name in object_names) {
    obj <- get(object_name, envir = env)
    if (methods::is(obj, "databaseClass") || methods::is(obj, "pathway_database")) {
      return(obj)
    }
  }
  get(object_names[1], envir = env)
}

metminer_kegg_read_tsv_if_exists <- function(file) {
  if (!file.exists(file)) return(data.frame())
  utils::read.delim(file, stringsAsFactors = FALSE, check.names = FALSE)
}

metminer_load_kegg_organism_database_result <- function(output_dir,
                                                        organism_code,
                                                        organism_name = organism_code) {
  files <- metminer_kegg_existing_database_files(output_dir, organism_code)
  if (!isTRUE(files$exists)) {
    stop("No complete KEGG database was found in: ", output_dir, call. = FALSE)
  }
  prefix <- paste0("kegg_", organism_code)
  path <- function(suffix) file.path(output_dir, paste0(prefix, suffix))
  ms1_db <- metminer_kegg_load_database_object(path("_ms1.rda"))
  ms2_db <- metminer_kegg_load_database_object(path("_ms2.rda"))
  pathway_db <- metminer_kegg_load_database_object(path("_pathway.rda"))
  prompt_file <- path("_pathway_review_prompt.md")
  pathway_review_prompt <- if (file.exists(prompt_file)) paste(readLines(prompt_file, warn = FALSE), collapse = "\n") else ""
  summary <- metminer_kegg_read_tsv_if_exists(path("_summary.tsv"))
  pathway_qc <- metminer_kegg_read_tsv_if_exists(path("_pathway_qc.tsv"))
  list(
    organism_code = organism_code,
    organism_name = organism_name,
    ms1_database = ms1_db,
    ms2_database = ms2_db,
    pathway_database = pathway_db,
    clean_compounds = metminer_kegg_read_tsv_if_exists(path("_clean_compounds.tsv")),
    removed_compounds = metminer_kegg_read_tsv_if_exists(path("_removed_compounds.tsv")),
    pathway_reaction_map = metminer_kegg_read_tsv_if_exists(path("_pathway_reaction_map.tsv")),
    pathway_compound_map = metminer_kegg_read_tsv_if_exists(path("_pathway_compound_map.tsv")),
    pathway_qc = pathway_qc,
    pathway_review_prompt = pathway_review_prompt,
    ms2_match_log = metminer_kegg_read_tsv_if_exists(path("_ms2_match_log.tsv")),
    ms2_unmatched_compounds = metminer_kegg_read_tsv_if_exists(path("_ms2_unmatched_compounds.tsv")),
    summary = summary,
    output_dir = output_dir,
    loaded_from_existing = TRUE
  )
}

metminer_build_kegg_organism_database <- function(organism_code = "zma",
                                                  organism_name = "Zea mays",
                                                  output_dir = file.path("Temp", paste0("kegg_", organism_code, "_database")),
                                                  min_mw = 70,
                                                  max_mw = 1500,
                                                  sleep_sec = 0.3,
                                                  review_min_reaction_coverage = 0.10,
                                                  review_min_supported_reactions = 5,
                                                  review_min_pathway_specific_compounds = 3,
                                                  review_hub_compound_frequency_cutoff = 0.10,
                                                  review_prompt_max_pathways = 25,
                                                  version = as.character(Sys.Date())) {
  cache_dir <- file.path(output_dir, "kegg_cache")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  pathways <- metminer_kegg_parse_pathway_list(
    metminer_kegg_rest_text(paste0("list/pathway/", organism_code), cache_dir, sleep_sec)
  )
  path_gene <- metminer_kegg_parse_link(
    metminer_kegg_rest_text(paste0("link/", organism_code, "/pathway"), cache_dir, sleep_sec),
    "pathway_id", "gene_id"
  )
  gene_ko <- metminer_kegg_parse_link(
    metminer_kegg_rest_text(paste0("link/ko/", organism_code), cache_dir, sleep_sec),
    "gene_id", "ko_id"
  )
  gene_ec <- metminer_kegg_parse_link(
    metminer_kegg_rest_text(paste0("link/enzyme/", organism_code), cache_dir, sleep_sec),
    "gene_id", "enzyme_id"
  )
  ko_rn <- metminer_kegg_parse_link(
    metminer_kegg_rest_text("link/rn/ko", cache_dir, sleep_sec),
    "ko_id", "reaction_id"
  )
  ec_rn <- metminer_kegg_parse_link(
    metminer_kegg_rest_text("link/rn/enzyme", cache_dir, sleep_sec),
    "enzyme_id", "reaction_id"
  )
  path_rn <- metminer_kegg_parse_link(
    metminer_kegg_rest_text("link/rn/pathway", cache_dir, sleep_sec),
    "pathway_id", "reaction_id"
  )
  rn_cpd <- metminer_kegg_parse_link(
    metminer_kegg_rest_text("link/cpd/rn", cache_dir, sleep_sec),
    "reaction_id", "compound_id"
  )

  path_gene$map_id <- metminer_kegg_path_to_map(path_gene$pathway_id)
  supported_ko <- merge(path_gene, gene_ko, by = "gene_id")
  supported_ko <- merge(supported_ko, ko_rn, by = "ko_id")
  supported_ko$evidence_type <- "gene_ko_reaction"
  supported_ec <- merge(path_gene, gene_ec, by = "gene_id")
  supported_ec <- merge(supported_ec, ec_rn, by = "enzyme_id")
  supported_ec$evidence_type <- "gene_ec_reaction"
  supported <- rbind(
    supported_ko[, c("pathway_id", "map_id", "gene_id", "reaction_id", "evidence_type"), drop = FALSE],
    supported_ec[, c("pathway_id", "map_id", "gene_id", "reaction_id", "evidence_type"), drop = FALSE]
  )
  supported <- unique(supported)

  map_rn <- path_rn[grepl("^map", path_rn$pathway_id), , drop = FALSE]
  colnames(map_rn)[colnames(map_rn) == "pathway_id"] <- "map_id"
  pathway_reaction_map <- merge(supported, map_rn, by = c("map_id", "reaction_id"))
  pathway_reaction_map <- unique(pathway_reaction_map[, c("pathway_id", "map_id", "gene_id", "reaction_id", "evidence_type"), drop = FALSE])

  pathway_compound_map <- merge(pathway_reaction_map, rn_cpd, by = "reaction_id")
  pathway_compound_map <- merge(pathway_compound_map, pathways, by = "pathway_id", all.x = TRUE)
  compound_ids <- unique(pathway_compound_map$compound_id)
  compounds <- metminer_kegg_get_compounds(compound_ids, cache_dir, sleep_sec)
  filtered <- metminer_filter_kegg_compounds(compounds, min_mw = min_mw, max_mw = max_mw)
  clean_compounds <- filtered$clean_compounds
  removed_compounds <- filtered$removed_compounds

  pathway_compound_map <- merge(
    pathway_compound_map,
    clean_compounds[, c("compound_id", "compound_name"), drop = FALSE],
    by = "compound_id"
  )
  pathway_compound_map <- unique(pathway_compound_map[, c(
    "pathway_id", "pathway_name", "compound_id", "compound_name", "reaction_id", "gene_id", "evidence_type"
  ), drop = FALSE])
  clean_compounds <- clean_compounds[clean_compounds$compound_id %in% unique(pathway_compound_map$compound_id), , drop = FALSE]

  ms1_db <- metminer_build_kegg_ms1_database(clean_compounds, organism_code, organism_name, version)
  ms2_result <- metminer_build_kegg_ms2_database(clean_compounds, organism_code, organism_name, version)
  ms2_db <- ms2_result$database
  pathway_qc <- metminer_kegg_pathway_qc(
    pathways = pathways,
    path_rn = path_rn,
    pathway_reaction_map = pathway_reaction_map,
    pathway_compound_map = pathway_compound_map,
    min_reaction_coverage = review_min_reaction_coverage,
    min_supported_reactions = review_min_supported_reactions,
    min_pathway_specific_compounds = review_min_pathway_specific_compounds,
    hub_compound_frequency_cutoff = review_hub_compound_frequency_cutoff
  )
  pathway_review_prompt <- metminer_kegg_pathway_review_prompt(
    pathway_qc = pathway_qc,
    pathway_compound_map = pathway_compound_map,
    pathway_reaction_map = pathway_reaction_map,
    organism_code = organism_code,
    organism_name = organism_name,
    max_pathways = review_prompt_max_pathways
  )
  pathway_db <- metminer_build_kegg_pathway_database(pathway_compound_map, pathway_reaction_map, organism_code, organism_name, version)

  object_name_ms1 <- paste0("kegg_", organism_code, "_ms1")
  object_name_ms2 <- paste0("kegg_", organism_code, "_ms2")
  object_name_pathway <- paste0("kegg_", organism_code, "_pathway")
  assign(object_name_ms1, ms1_db)
  assign(object_name_ms2, ms2_db)
  assign(object_name_pathway, pathway_db)
  save(list = object_name_ms1, file = file.path(output_dir, paste0("kegg_", organism_code, "_ms1.rda")))
  save(list = object_name_ms2, file = file.path(output_dir, paste0("kegg_", organism_code, "_ms2.rda")))
  save(list = object_name_pathway, file = file.path(output_dir, paste0("kegg_", organism_code, "_pathway.rda")))
  utils::write.table(clean_compounds, file.path(output_dir, paste0("kegg_", organism_code, "_clean_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(removed_compounds, file.path(output_dir, paste0("kegg_", organism_code, "_removed_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_reaction_map, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_reaction_map.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_compound_map, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_compound_map.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_qc, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_qc.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  writeLines(pathway_review_prompt, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_review_prompt.md")), useBytes = TRUE)
  utils::write.table(ms2_db@spectra.info, file.path(output_dir, paste0("kegg_", organism_code, "_ms2_spectra_info.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$match_log, file.path(output_dir, paste0("kegg_", organism_code, "_ms2_match_log.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$unmatched_compounds, file.path(output_dir, paste0("kegg_", organism_code, "_ms2_unmatched_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  positive_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.positive, length, integer(1)))
  negative_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.negative, length, integer(1)))

  summary <- data.frame(
    metric = c(
      "pathways", "pathway_gene_links", "gene_ko_links", "gene_ec_links",
      "supported_pathway_reactions", "raw_reaction_supported_compounds",
      "clean_compounds", "removed_compounds", "pathway_compound_links",
      "ms2_compounds", "ms2_positive_compounds", "ms2_negative_compounds",
      "ms2_positive_spectra", "ms2_negative_spectra", "ms2_match_rows",
      "ms2_unmatched_compounds", "pathways_flagged_for_review"
    ),
    value = c(
      nrow(pathways), nrow(path_gene), nrow(gene_ko), nrow(gene_ec),
      length(unique(pathway_reaction_map$reaction_id)), length(unique(compounds$compound_id)),
      nrow(clean_compounds), nrow(removed_compounds), nrow(pathway_compound_map),
      nrow(ms2_db@spectra.info), length(ms2_db@spectra.data$Spectra.positive),
      length(ms2_db@spectra.data$Spectra.negative), positive_spectra, negative_spectra,
      nrow(ms2_result$match_log), nrow(ms2_result$unmatched_compounds),
      sum(pathway_qc$review_flag, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  if (nrow(ms2_result$match_log) > 0) {
    source_table <- table(ms2_result$match_log$source_database)
    type_table <- table(ms2_result$match_log$match_type)
    summary <- rbind(
      summary,
      data.frame(metric = paste0("match_source_", names(source_table)), value = as.integer(source_table)),
      data.frame(metric = paste0("match_type_", names(type_table)), value = as.integer(type_table))
    )
  }
  if (nrow(removed_compounds) > 0) {
    reason_table <- sort(table(unlist(strsplit(removed_compounds$filter_reason, ";", fixed = TRUE))), decreasing = TRUE)
    summary <- rbind(summary, data.frame(metric = paste0("removed_", names(reason_table)), value = as.integer(reason_table)))
  }
  utils::write.table(summary, file.path(output_dir, paste0("kegg_", organism_code, "_summary.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  list(
    organism_code = organism_code,
    organism_name = organism_name,
    ms1_database = ms1_db,
    ms2_database = ms2_db,
    pathway_database = pathway_db,
    clean_compounds = clean_compounds,
    removed_compounds = removed_compounds,
    pathway_reaction_map = pathway_reaction_map,
    pathway_compound_map = pathway_compound_map,
    pathway_qc = pathway_qc,
    pathway_review_prompt = pathway_review_prompt,
    ms2_match_log = ms2_result$match_log,
    ms2_unmatched_compounds = ms2_result$unmatched_compounds,
    summary = summary,
    output_dir = output_dir
  )
}
