# ---- PlantCyc / PMN local PGDB flat-file builder ----

metminer_pgdb_locate_data_dir <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  if (dir.exists(file.path(path, "data"))) return(file.path(path, "data"))
  if (basename(path) == "data" && dir.exists(path)) return(path)
  candidates <- list.dirs(path, recursive = TRUE, full.names = TRUE)
  candidates <- candidates[basename(candidates) == "data"]
  candidates <- candidates[file.exists(file.path(candidates, "compounds.dat"))]
  if (length(candidates) == 0) {
    stop("Cannot find a PGDB data directory containing compounds.dat.", call. = FALSE)
  }
  candidates[1]
}

metminer_pgdb_unpack_archive <- function(path, output_dir = tempfile("metminer_pgdb_")) {
  if (is.null(path) || !file.exists(path)) stop("PGDB archive does not exist.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("zip")) {
    utils::unzip(path, exdir = output_dir)
  } else {
    utils::untar(path, exdir = output_dir)
  }
  metminer_pgdb_locate_data_dir(output_dir)
}

metminer_pgdb_read_records <- function(file) {
  if (!file.exists(file)) return(list())
  lines <- readLines(file, warn = FALSE)
  lines <- iconv(lines, from = "", to = "UTF-8", sub = " ")
  lines <- lines[!startsWith(lines, "#")]
  records <- list()
  current <- list()
  last_key <- NULL
  add_value <- function(key, value) {
    current[[key]] <<- c(current[[key]], value)
    last_key <<- key
  }
  for (line in lines) {
    if (identical(line, "//")) {
      if (length(current) > 0) records[[length(records) + 1L]] <- current
      current <- list()
      last_key <- NULL
      next
    }
    if (!nzchar(line)) next
    if (grepl("^[/]", line) && !is.null(last_key)) {
      current[[last_key]][length(current[[last_key]])] <- paste(current[[last_key]][length(current[[last_key]])], sub("^/ ?", "", line), sep = "\n")
      next
    }
    if (grepl("^\\^", line) && grepl(" - ", line, fixed = TRUE)) {
      parts <- strsplit(line, " - ", fixed = TRUE)[[1]]
      add_value(parts[1], paste(parts[-1], collapse = " - "))
      next
    }
    if (grepl(" - ", line, fixed = TRUE)) {
      parts <- strsplit(line, " - ", fixed = TRUE)[[1]]
      add_value(parts[1], paste(parts[-1], collapse = " - "))
    }
  }
  if (length(current) > 0) records[[length(records) + 1L]] <- current
  records
}

metminer_pgdb_first <- function(record, field, default = NA_character_) {
  x <- record[[field]]
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

metminer_pgdb_collapse <- function(x, sep = " // ") {
  x <- unique(trimws(as.character(x %||% character())))
  x <- x[has_text(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

metminer_pgdb_clean_text <- function(x) {
  x <- as.character(x %||% NA_character_)
  x <- gsub("\\|FRAME:([^| ]+)( [^|]+)?\\|", "\\1", x, perl = TRUE)
  x <- gsub("\\|CITS:\\s*\\[([^]]+)\\]\\|", "\\1", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  metminer_plantcyc_decode_html(x)
}

metminer_pgdb_parse_formula <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  atoms <- list()
  for (one in x) {
    m <- regmatches(one, gregexpr("\\(([A-Za-z][A-Za-z0-9]*)\\s+([0-9.]+)\\)", one, perl = TRUE))[[1]]
    for (hit in m) {
      parts <- strcapture("\\(([A-Za-z][A-Za-z0-9]*)\\s+([0-9.]+)\\)", hit, data.frame(atom = character(), count = numeric()))
      atoms[[parts$atom]] <- (atoms[[parts$atom]] %||% 0) + parts$count
    }
  }
  if (length(atoms) == 0) return(NA_character_)
  order_atoms <- c("C", "H", "N", "O", "P", "S")
  atom_names <- c(intersect(order_atoms, names(atoms)), sort(setdiff(names(atoms), order_atoms)))
  paste0(atom_names, vapply(atom_names, function(a) {
    n <- atoms[[a]]
    if (isTRUE(all.equal(n, 1))) "" else as.character(n)
  }, character(1)), collapse = "")
}

metminer_pgdb_parse_dblinks <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.frame(database = character(), id = character()))
  rows <- lapply(x, function(one) {
    m <- regexec("^\\(([^\\s]+)\\s+\"([^\"]+)\"", one, perl = TRUE)
    hit <- regmatches(one, m)[[1]]
    if (length(hit) < 3) return(NULL)
    data.frame(database = gsub("^\\||\\|$", "", hit[2]), id = hit[3], stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame(database = character(), id = character()))
  do.call(rbind, rows)
}

metminer_pgdb_xref_value <- function(xrefs, db_names) {
  if (nrow(xrefs) == 0) return(NA_character_)
  ids <- xrefs$id[toupper(xrefs$database) %in% toupper(db_names)]
  metminer_pgdb_collapse(ids, sep = ";")
}

metminer_pgdb_parse_compounds <- function(records) {
  rows <- lapply(records, function(rec) {
    xrefs <- metminer_pgdb_parse_dblinks(rec$DBLINKS)
    data.frame(
      PlantCyc.ID = metminer_pgdb_first(rec, "UNIQUE-ID"),
      Compound.name = metminer_pgdb_clean_text(metminer_pgdb_first(rec, "COMMON-NAME")),
      Synonyms = metminer_pgdb_clean_text(metminer_pgdb_collapse(rec$SYNONYMS)),
      Formula = metminer_pgdb_parse_formula(rec$`CHEMICAL-FORMULA`),
      mz = suppressWarnings(as.numeric(metminer_pgdb_first(rec, "MONOISOTOPIC-MW"))),
      molecular_weight = suppressWarnings(as.numeric(metminer_pgdb_first(rec, "MOLECULAR-WEIGHT"))),
      SMILES.ID = metminer_pgdb_first(rec, "SMILES"),
      INCHI.ID = metminer_pgdb_first(rec, "INCHI", metminer_pgdb_first(rec, "NON-STANDARD-INCHI")),
      INCHIKEY.ID = sub("^InChIKey=", "", metminer_pgdb_first(rec, "INCHI-KEY")),
      KEGG.ID = metminer_pgdb_xref_value(xrefs, c("LIGAND-CPD", "KEGG")),
      PubChem.ID = metminer_pgdb_xref_value(xrefs, "PUBCHEM"),
      ChEBI.ID = metminer_pgdb_xref_value(xrefs, "CHEBI"),
      MetaNetX.ID = metminer_pgdb_xref_value(xrefs, "METANETX"),
      SEED.ID = metminer_pgdb_xref_value(xrefs, "SEED"),
      Types = metminer_pgdb_collapse(rec$TYPES),
      Citations = metminer_pgdb_collapse(rec$CITATIONS),
      Comment = metminer_pgdb_clean_text(metminer_pgdb_collapse(rec$COMMENT, sep = "\n")),
      has_no_structure = any(rec$`HAS-NO-STRUCTURE?` %in% "T"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$Lab.ID <- out$PlantCyc.ID
  out <- out[, c("Lab.ID", setdiff(colnames(out), "Lab.ID")), drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_pgdb_parse_reactions <- function(records) {
  rows <- lapply(records, function(rec) {
    data.frame(
      reaction_id = metminer_pgdb_first(rec, "UNIQUE-ID"),
      reaction_name = metminer_pgdb_clean_text(metminer_pgdb_first(rec, "COMMON-NAME")),
      left = metminer_pgdb_collapse(rec$LEFT, sep = ";"),
      right = metminer_pgdb_collapse(rec$RIGHT, sep = ";"),
      in_pathway = metminer_pgdb_collapse(rec$`IN-PATHWAY`, sep = ";"),
      ec_number = metminer_pgdb_collapse(rec$`EC-NUMBER`, sep = ";"),
      direction = metminer_pgdb_first(rec, "REACTION-DIRECTION"),
      physiologically_relevant = metminer_pgdb_first(rec, "PHYSIOLOGICALLY-RELEVANT?"),
      citations = metminer_pgdb_collapse(rec$CITATIONS),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[has_text(out$reaction_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_pgdb_reaction_compounds <- function(reactions) {
  rows <- lapply(seq_len(nrow(reactions)), function(i) {
    left <- strsplit(reactions$left[i] %||% "", ";", fixed = TRUE)[[1]]
    right <- strsplit(reactions$right[i] %||% "", ";", fixed = TRUE)[[1]]
    data.frame(
      reaction_id = reactions$reaction_id[i],
      PlantCyc.ID = c(left, right),
      side = c(rep("left", length(left)), rep("right", length(right))),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[has_text(out$PlantCyc.ID), , drop = FALSE]
  rownames(out) <- NULL
  unique(out)
}

metminer_pgdb_parse_pathways <- function(records) {
  rows <- lapply(records, function(rec) {
    data.frame(
      pathway_id = metminer_pgdb_first(rec, "UNIQUE-ID"),
      pathway_name = metminer_pgdb_clean_text(metminer_pgdb_first(rec, "COMMON-NAME")),
      types = metminer_pgdb_collapse(rec$TYPES),
      reaction_list = metminer_pgdb_collapse(rec$`REACTION-LIST`, sep = ";"),
      sub_pathways = metminer_pgdb_collapse(rec$`SUB-PATHWAYS`, sep = ";"),
      super_pathways = metminer_pgdb_collapse(rec$`SUPER-PATHWAYS`, sep = ";"),
      score = suppressWarnings(as.numeric(metminer_pgdb_first(rec, "SCORE"))),
      evidence_code = metminer_pgdb_collapse(rec$`EXPLANATION-CODE`, sep = ";"),
      citations = metminer_pgdb_collapse(rec$CITATIONS),
      comment = metminer_pgdb_clean_text(metminer_pgdb_collapse(rec$COMMENT, sep = "\n")),
      synonyms = metminer_pgdb_clean_text(metminer_pgdb_collapse(rec$SYNONYMS)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[has_text(out$pathway_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_pgdb_pathway_reactions <- function(pathways, reaction_ids) {
  rows <- lapply(seq_len(nrow(pathways)), function(i) {
    rxn <- strsplit(pathways$reaction_list[i] %||% "", ";", fixed = TRUE)[[1]]
    rxn <- rxn[rxn %in% reaction_ids]
    if (length(rxn) == 0) return(NULL)
    data.frame(pathway_id = pathways$pathway_id[i], reaction_id = rxn, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame(pathway_id = character(), reaction_id = character()))
  unique(do.call(rbind, rows))
}

metminer_pgdb_make_local_object <- function(pgdb_dir) {
  data_dir <- metminer_pgdb_locate_data_dir(pgdb_dir)
  compounds <- metminer_pgdb_parse_compounds(metminer_pgdb_read_records(file.path(data_dir, "compounds.dat")))
  filter_input <- data.frame(
    compound_id = compounds$PlantCyc.ID,
    formula = compounds$Formula,
    monoisotopic_mw = compounds$mz,
    compound_name = compounds$Compound.name,
    synonyms = compounds$Synonyms,
    stringsAsFactors = FALSE
  )
  compounds$filter_reason <- metminer_plantcyc_filter_reasons(
    filter_input,
    min_mw = 70,
    max_mw = 1500,
    remove_no_carbon = TRUE,
    remove_currency = TRUE,
    remove_coa = TRUE,
    remove_carrier_bound = TRUE,
    remove_reactive_small = TRUE
  )
  compounds$ms2_filter_reason <- metminer_plantcyc_filter_reasons(
    filter_input,
    min_mw = 70,
    max_mw = 1500,
    remove_no_carbon = TRUE,
    remove_currency = TRUE,
    remove_coa = FALSE,
    remove_carrier_bound = TRUE,
    remove_reactive_small = TRUE
  )
  reactions <- metminer_pgdb_parse_reactions(metminer_pgdb_read_records(file.path(data_dir, "reactions.dat")))
  pathways <- metminer_pgdb_parse_pathways(metminer_pgdb_read_records(file.path(data_dir, "pathways.dat")))
  reaction_compounds <- metminer_pgdb_reaction_compounds(reactions)
  pathway_reactions <- metminer_pgdb_pathway_reactions(pathways, reactions$reaction_id)
  pathway_compounds <- merge(pathway_reactions, reaction_compounds, by = "reaction_id", all.x = TRUE, sort = FALSE)
  pathway_compounds <- merge(pathway_compounds, compounds[, c("PlantCyc.ID", "Compound.name", "KEGG.ID", "PubChem.ID", "ChEBI.ID"), drop = FALSE],
                             by = "PlantCyc.ID", all.x = TRUE, sort = FALSE)
  pathway_compounds <- unique(pathway_compounds[has_text(pathway_compounds$PlantCyc.ID), , drop = FALSE])
  meta <- list(
    source_dir = normalizePath(dirname(data_dir), mustWork = FALSE),
    data_dir = normalizePath(data_dir, mustWork = FALSE),
    built_at = as.character(Sys.time())
  )
  structure(
    list(
      meta = meta,
      compounds = compounds,
      reactions = reactions,
      reaction_compounds = reaction_compounds,
      pathways = pathways,
      pathway_reactions = pathway_reactions,
      pathway_compounds = pathway_compounds,
      removed_compounds = compounds[has_text(compounds$filter_reason), , drop = FALSE],
      ms2_eligible_compounds = compounds[!has_text(compounds$ms2_filter_reason), , drop = FALSE],
      coa_fragment_rules = metminer_plantcyc_coa_fragment_rules()
    ),
    class = "metminer_plantcyc_db"
  )
}

metminer_pgdb_spectra_info <- function(plantcyc_db) {
  x <- plantcyc_db$compounds
  x <- x[!has_text(x$filter_reason), , drop = FALSE]
  data.frame(
    Lab.ID = x$PlantCyc.ID,
    PlantCyc.ID = x$PlantCyc.ID,
    BIOCYC.ID = x$PlantCyc.ID,
    KEGG.ID = x$KEGG.ID,
    PubChem.ID = x$PubChem.ID,
    ChEBI.ID = x$ChEBI.ID,
    MetaNetX.ID = x$MetaNetX.ID,
    SEED.ID = x$SEED.ID,
    Compound.name = x$Compound.name,
    mz = x$mz,
    RT = NA_real_,
    CAS.ID = NA_character_,
    HMDB.ID = NA_character_,
    Formula = x$Formula,
    mz.pos = NA_real_,
    mz.neg = NA_real_,
    Submitter = "PlantCyc/PMN local PGDB",
    Synonyms = x$Synonyms,
    monisotopic_molecular_weight = x$mz,
    SMILES.ID = x$SMILES.ID,
    INCHI.ID = x$INCHI.ID,
    INCHIKEY.ID = x$INCHIKEY.ID,
    Types = x$Types,
    Citations = x$Citations,
    Comment = x$Comment,
    source = "PlantCyc/PMN local PGDB",
    stringsAsFactors = FALSE
  )
}

metminer_pgdb_build_ms1_database <- function(plantcyc_db, version = as.character(Sys.Date())) {
  methods::new(
    Class = "databaseClass",
    database.info = list(
      Version = version,
      Source = "PlantCyc/PMN local PGDB",
      Link = "https://pmn.plantcyc.org/",
      Creater = "MetMiner2",
      Email = NA,
      RT = FALSE
    ),
    spectra.info = metminer_pgdb_spectra_info(plantcyc_db),
    spectra.data = list(Spectra.positive = list(), Spectra.negative = list())
  )
}

metminer_pgdb_as_plantcyc_compounds <- function(compounds) {
  data.frame(
    compound_id = compounds$PlantCyc.ID,
    compound_name = compounds$Compound.name,
    synonyms = compounds$Synonyms,
    formula = compounds$Formula,
    monoisotopic_mw = compounds$mz,
    smiles = compounds$SMILES.ID,
    inchi = compounds$INCHI.ID,
    inchi_key = compounds$INCHIKEY.ID,
    kegg_id = compounds$KEGG.ID,
    pubchem_id = compounds$PubChem.ID,
    chebi_id = compounds$ChEBI.ID,
    comment = compounds$Comment,
    source = "PlantCyc/PMN local PGDB",
    stringsAsFactors = FALSE
  )
}

metminer_pgdb_build_ms2_database <- function(plantcyc_db,
                                             version = as.character(Sys.Date()),
                                             builtin_databases = c("hmdb_ms2", "massbank_ms2", "mona_ms2"),
                                             mass_tolerance_ppm = 10,
                                             mass_tolerance_da = 0.01) {
  clean_compounds <- metminer_pgdb_as_plantcyc_compounds(plantcyc_db$ms2_eligible_compounds)
  metminer_build_plantcyc_ms2_database(
    clean_compounds = clean_compounds,
    version = version,
    builtin_databases = builtin_databases,
    mass_tolerance_ppm = mass_tolerance_ppm,
    mass_tolerance_da = mass_tolerance_da
  )
}

metminer_pgdb_build_pathway_database <- function(plantcyc_db, organism = "PlantCyc local PGDB",
                                                 version = as.character(Sys.Date())) {
  if (!requireNamespace("metpath", quietly = TRUE)) {
    stop("Package 'metpath' is required to construct pathway_database objects.", call. = FALSE)
  }
  pwy <- plantcyc_db$pathways
  pmap <- plantcyc_db$pathway_compounds
  pmap <- pmap[has_text(pmap$PlantCyc.ID), , drop = FALSE]
  ids <- pwy$pathway_id
  compound_list <- lapply(ids, function(pid) {
    x <- pmap[pmap$pathway_id == pid, c("PlantCyc.ID", "Compound.name", "KEGG.ID"), drop = FALSE]
    unique(x[has_text(x$PlantCyc.ID), , drop = FALSE])
  })
  names(compound_list) <- ids
  methods::new(
    Class = "pathway_database",
    database_info = list(
      source = "PlantCyc/PMN local PGDB",
      version = version,
      organism = organism,
      source_url = "https://pmn.plantcyc.org/"
    ),
    pathway_id = ids,
    pathway_name = pwy$pathway_name,
    describtion = lapply(seq_len(nrow(pwy)), function(i) {
      c(pwy$comment[i], paste0("Evidence: ", pwy$evidence_code[i]), paste0("Score: ", pwy$score[i]))
    }),
    pathway_class = lapply(pwy$types, function(x) strsplit(x %||% "", " // ", fixed = TRUE)[[1]]),
    gene_list = vector("list", length(ids)),
    compound_list = compound_list,
    protein_list = vector("list", length(ids)),
    reference_list = lapply(pwy$citations, function(x) strsplit(x %||% "", " // ", fixed = TRUE)[[1]]),
    related_disease = vector("list", length(ids)),
    related_module = vector("list", length(ids))
  )
}

metminer_pgdb_build_outputs <- function(pgdb_dir,
                                        output_dir,
                                        output_prefix = "plantcyc_local",
                                        organism = "PlantCyc local PGDB") {
  output_prefix <- metminer_plantcyc_sanitize_prefix(output_prefix)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  local_db <- metminer_pgdb_make_local_object(pgdb_dir)
  ms1_db <- metminer_pgdb_build_ms1_database(local_db)
  ms2_result <- metminer_pgdb_build_ms2_database(local_db)
  ms2_db <- ms2_result$database
  pathway_db <- metminer_pgdb_build_pathway_database(local_db, organism = organism)

  metminer_save_named_rda(local_db, paste0(output_prefix, "_local_object"), file.path(output_dir, paste0(output_prefix, "_local_object.rda")))
  metminer_save_named_rda(ms1_db, paste0(output_prefix, "_ms1"), file.path(output_dir, paste0(output_prefix, "_ms1.rda")))
  metminer_save_named_rda(ms2_db, paste0(output_prefix, "_ms2"), file.path(output_dir, paste0(output_prefix, "_ms2.rda")))
  metminer_save_named_rda(pathway_db, paste0(output_prefix, "_pathway"), file.path(output_dir, paste0(output_prefix, "_pathway.rda")))
  utils::write.table(local_db$compounds, file.path(output_dir, paste0(output_prefix, "_compound_metadata.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(local_db$pathway_compounds, file.path(output_dir, paste0(output_prefix, "_pathway_compound.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(local_db$reaction_compounds, file.path(output_dir, paste0(output_prefix, "_reaction_compound.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(local_db$removed_compounds, file.path(output_dir, paste0(output_prefix, "_removed_compounds.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(local_db$coa_fragment_rules, file.path(output_dir, paste0(output_prefix, "_coa_fragment_rules.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_db@spectra.info, file.path(output_dir, paste0(output_prefix, "_ms2_spectra_info.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$match_log, file.path(output_dir, paste0(output_prefix, "_ms2_match_log.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$unmatched_compounds, file.path(output_dir, paste0(output_prefix, "_ms2_unmatched_compounds.tsv")), sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  removed <- local_db$removed_compounds
  positive_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.positive, length, integer(1)))
  negative_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.negative, length, integer(1)))
  reason_table <- sort(
    table(unlist(strsplit(removed$filter_reason, ";", fixed = TRUE))),
    decreasing = TRUE
  )
  filter_summary <- data.frame(
    item = paste0("filter_reason:", names(reason_table)),
    count = as.integer(reason_table),
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    item = c(
      "raw_compounds",
      "ms1_database_records",
      "removed_from_ms1",
      "ms2_eligible_compounds",
      "ms2_database_records",
      "ms2_positive_compounds",
      "ms2_negative_compounds",
      "ms2_positive_spectra",
      "ms2_negative_spectra",
      "ms2_match_rows",
      "coa_retained_for_ms2",
      "pathways",
      "reactions",
      "pathway_compound_links"
    ),
    count = c(
      nrow(local_db$compounds),
      nrow(ms1_db@spectra.info),
      nrow(local_db$removed_compounds),
      nrow(local_db$ms2_eligible_compounds),
      nrow(ms2_db@spectra.info),
      length(ms2_db@spectra.data$Spectra.positive),
      length(ms2_db@spectra.data$Spectra.negative),
      positive_spectra,
      negative_spectra,
      nrow(ms2_result$match_log),
      sum(grepl("coa_related", removed$filter_reason, fixed = TRUE) &
            !has_text(removed$ms2_filter_reason), na.rm = TRUE),
      nrow(local_db$pathways),
      nrow(local_db$reactions),
      nrow(local_db$pathway_compounds)
    ),
    stringsAsFactors = FALSE
  )
  summary <- rbind(summary, filter_summary)
  list(
    local_db = local_db,
    ms1_db = ms1_db,
    ms2_db = ms2_db,
    pathway_db = pathway_db,
    summary = summary,
    output_dir = output_dir,
    output_prefix = output_prefix,
    files = list(
      local_object = file.path(output_dir, paste0(output_prefix, "_local_object.rda")),
      ms1 = file.path(output_dir, paste0(output_prefix, "_ms1.rda")),
      ms2 = file.path(output_dir, paste0(output_prefix, "_ms2.rda")),
      pathway = file.path(output_dir, paste0(output_prefix, "_pathway.rda")),
      compound_metadata = file.path(output_dir, paste0(output_prefix, "_compound_metadata.tsv")),
      pathway_compound = file.path(output_dir, paste0(output_prefix, "_pathway_compound.tsv")),
      reaction_compound = file.path(output_dir, paste0(output_prefix, "_reaction_compound.tsv")),
      removed_compounds = file.path(output_dir, paste0(output_prefix, "_removed_compounds.tsv")),
      coa_fragment_rules = file.path(output_dir, paste0(output_prefix, "_coa_fragment_rules.tsv")),
      ms2_spectra_info = file.path(output_dir, paste0(output_prefix, "_ms2_spectra_info.tsv")),
      ms2_match_log = file.path(output_dir, paste0(output_prefix, "_ms2_match_log.tsv")),
      ms2_unmatched_compounds = file.path(output_dir, paste0(output_prefix, "_ms2_unmatched_compounds.tsv"))
    )
  )
}
