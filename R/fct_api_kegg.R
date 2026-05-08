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
  unique(out)
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
  sub("^path:[a-z]{3,4}([0-9]{5})$", "path:map\\1", pathway_id)
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
      compound_id = paste0("cpd:", entry_id),
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
  compound_ids <- unique(sub("^cpd:", "", compound_ids))
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
      "cpd:C00001", "cpd:C00002", "cpd:C00003", "cpd:C00004", "cpd:C00005",
      "cpd:C00006", "cpd:C00007", "cpd:C00008", "cpd:C00009", "cpd:C00010",
      "cpd:C00011", "cpd:C00013", "cpd:C00014", "cpd:C00015", "cpd:C00016",
      "cpd:C00020", "cpd:C00035", "cpd:C00044", "cpd:C00061", "cpd:C00080"
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
    KEGG.ID = sub("^cpd:", "", compounds$compound_id),
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

#' Build a conservative KEGG organism metabolite database
#'
#' @noRd
metminer_build_kegg_organism_database <- function(organism_code = "zma",
                                                  organism_name = "Zea mays",
                                                  output_dir = file.path("Temp", paste0("kegg_", organism_code, "_database")),
                                                  min_mw = 70,
                                                  max_mw = 1500,
                                                  sleep_sec = 0.3,
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

  map_rn <- path_rn[grepl("^path:map", path_rn$pathway_id), , drop = FALSE]
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
  pathway_db <- metminer_build_kegg_pathway_database(pathway_compound_map, pathway_reaction_map, organism_code, organism_name, version)

  object_name_ms1 <- paste0("kegg_", organism_code, "_ms1")
  object_name_pathway <- paste0("kegg_", organism_code, "_pathway")
  assign(object_name_ms1, ms1_db)
  assign(object_name_pathway, pathway_db)
  save(list = object_name_ms1, file = file.path(output_dir, paste0("kegg_", organism_code, "_ms1.rda")))
  save(list = object_name_pathway, file = file.path(output_dir, paste0("kegg_", organism_code, "_pathway.rda")))
  utils::write.table(clean_compounds, file.path(output_dir, paste0("kegg_", organism_code, "_clean_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(removed_compounds, file.path(output_dir, paste0("kegg_", organism_code, "_removed_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_reaction_map, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_reaction_map.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_compound_map, file.path(output_dir, paste0("kegg_", organism_code, "_pathway_compound_map.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  summary <- data.frame(
    metric = c(
      "pathways", "pathway_gene_links", "gene_ko_links", "gene_ec_links",
      "supported_pathway_reactions", "raw_reaction_supported_compounds",
      "clean_compounds", "removed_compounds", "pathway_compound_links"
    ),
    value = c(
      nrow(pathways), nrow(path_gene), nrow(gene_ko), nrow(gene_ec),
      length(unique(pathway_reaction_map$reaction_id)), length(unique(compounds$compound_id)),
      nrow(clean_compounds), nrow(removed_compounds), nrow(pathway_compound_map)
    ),
    stringsAsFactors = FALSE
  )
  if (nrow(removed_compounds) > 0) {
    reason_table <- sort(table(unlist(strsplit(removed_compounds$filter_reason, ";", fixed = TRUE))), decreasing = TRUE)
    summary <- rbind(summary, data.frame(metric = paste0("removed_", names(reason_table)), value = as.integer(reason_table)))
  }
  utils::write.table(summary, file.path(output_dir, paste0("kegg_", organism_code, "_summary.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  list(
    ms1_database = ms1_db,
    pathway_database = pathway_db,
    clean_compounds = clean_compounds,
    removed_compounds = removed_compounds,
    pathway_reaction_map = pathway_reaction_map,
    pathway_compound_map = pathway_compound_map,
    summary = summary,
    output_dir = output_dir
  )
}
