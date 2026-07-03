# ---- PlantCyc SmartTable cleaning helpers ----

#' Decode the small HTML subset commonly found in PlantCyc SmartTable exports
#'
#' @noRd
metminer_plantcyc_decode_html <- function(x) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  x[is.na(x)] <- ""

  replacements <- c(
    "&alpha;" = "alpha",
    "&beta;" = "beta",
    "&gamma;" = "gamma",
    "&delta;" = "delta",
    "&epsilon;" = "epsilon",
    "&mu;" = "mu",
    "&plusmn;" = "+/-",
    "&rarr;" = "->",
    "&larr;" = "<-",
    "&harr;" = "<->",
    "&nbsp;" = " ",
    "&amp;" = "&",
    "&lt;" = "<",
    "&gt;" = ">",
    "&quot;" = "\"",
    "&#39;" = "'",
    "&apos;" = "'"
  )

  for (pattern in names(replacements)) {
    x <- gsub(pattern, replacements[[pattern]], x, fixed = TRUE)
  }

  x <- gsub("(?i)</?(sub|sup|i|em|b|strong)>", "", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

#' Parse PlantCyc monoisotopic mass values
#'
#' @noRd
metminer_plantcyc_parse_mw <- function(x) {
  x <- as.character(x)
  x <- gsub("[dD]([+-]?[0-9]+)$", "e\\1", x, perl = TRUE)
  suppressWarnings(as.numeric(x))
}

#' Normalize names for PlantCyc compound matching
#'
#' @noRd
metminer_plantcyc_normalize_name <- function(x) {
  x <- metminer_plantcyc_decode_html(x)
  x <- tolower(x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

#' Split PlantCyc synonym fields
#'
#' @noRd
metminer_plantcyc_split_synonyms <- function(x) {
  x <- metminer_plantcyc_decode_html(x)
  out <- strsplit(x, " // ", fixed = TRUE)
  lapply(out, function(value) {
    value <- trimws(value)
    value[nzchar(value)]
  })
}

#' Detect true CoA/coenzyme A text without matching unrelated words such as disecoandrosta
#'
#' @noRd
metminer_plantcyc_is_coa <- function(...) {
  text <- paste(..., sep = " ")
  text <- tolower(metminer_plantcyc_decode_html(text))
  grepl("(^|[^a-z0-9])coa([^a-z0-9]|$)|coenzyme[[:space:]]+a", text, perl = TRUE)
}

#' Identify PlantCyc rows that should not be used as LC-MS annotation targets
#'
#' @noRd
metminer_plantcyc_filter_reasons <- function(compounds,
                                             min_mw = 70,
                                             max_mw = 1500,
                                             remove_no_carbon = TRUE,
                                             remove_currency = TRUE,
                                             remove_coa = TRUE,
                                             remove_carrier_bound = TRUE,
                                             remove_reactive_small = TRUE,
                                             low_mw_allowlist = character()) {
  n <- nrow(compounds)
  reasons <- vector("list", n)
  add_reason <- function(idx, reason) {
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) return(invisible(NULL))
    reasons[idx] <<- Map(c, reasons[idx], reason)
    invisible(NULL)
  }

  id <- compounds$compound_id
  formula <- compounds$formula
  mw <- compounds$monoisotopic_mw
  name <- compounds$compound_name
  synonyms <- compounds$synonyms
  text <- paste(id, name, synonyms)
  text_lower <- tolower(text)
  name_norm <- metminer_plantcyc_normalize_name(name)
  allow_norm <- metminer_plantcyc_normalize_name(low_mw_allowlist)
  allow_low_mw <- name_norm %in% allow_norm | id %in% low_mw_allowlist

  add_reason(!has_text(formula), "missing_formula")
  add_reason(is.na(mw), "missing_monoisotopic_mw")
  add_reason(!is.na(mw) & mw < min_mw & !allow_low_mw, "mw_below_min")
  add_reason(!is.na(mw) & mw > max_mw, "mw_above_max")

  if (isTRUE(remove_no_carbon)) {
    add_reason(has_text(formula) & !grepl("C", formula, fixed = TRUE), "no_carbon_formula")
  }

  metal_formula <- "^(Na|K|Mg|Ca|Fe|Zn|Cu|Mn|Co|Mo|Ni|Se|Cl|S)$"
  metal_name <- paste(
    "sodium|potassium|magnesium|calcium|iron|ferrous|ferric|zinc|copper",
    "manganese|cobalt|molybdenum|nickel|chloride|elemental sulfur",
    sep = "|"
  )
  add_reason(grepl(metal_formula, formula, perl = TRUE) |
               grepl(metal_name, text_lower, perl = TRUE),
             "inorganic_or_metal_ion")

  if (isTRUE(remove_currency)) {
    currency_ids <- c(
      "PROTON", "WATER", "OH", "AMMONIA", "AMMONIUM", "CARBON-DIOXIDE",
      "OXYGEN-MOLECULE", "CARBON-MONOXIDE", "NITRIC-OXIDE", "NITROUS-OXIDE",
      "PHOSPHATE", "Pi", "PPI", "ATP", "ADP", "AMP", "GTP", "GDP", "GMP",
      "CTP", "CDP", "CMP", "UTP", "UDP", "UMP", "NAD", "NADH", "NADP",
      "NADPH", "FAD", "FMN"
    )
    currency_name <- paste(
      "^h\\+$|^h2o$|^water$|^oh-$|^ammonia$|^ammonium$|^carbon dioxide$",
      "^co2$|^dioxygen$|^oxygen$|^phosphate$|^diphosphate$|^pyrophosphate$",
      "^atp$|^adp$|^amp$|^gtp$|^gdp$|^gmp$|^ctp$|^cdp$|^cmp$",
      "^utp$|^udp$|^ump$|^nad$|^nadh$|^nadp\\+$|^nadph$|^fad$|^fmn$",
      sep = "|"
    )
    add_reason(id %in% currency_ids | grepl(currency_name, name_norm, perl = TRUE),
               "currency_metabolite")
  }

  if (isTRUE(remove_coa)) {
    add_reason(metminer_plantcyc_is_coa(id, name, synonyms), "coa_related")
  }

  if (isTRUE(remove_carrier_bound)) {
    carrier_pattern <- paste(
      "\\[(glycerolipid|protein|enzyme|reductase|ferredoxin|hemoprotein|acp|acyl-carrier|carrier protein)[^]]*\\]",
      "ferredoxin|hemoprotein|reductase|protein|enzyme",
      " acp|acyl-carrier|carrier protein",
      "glucan|xylan|cellulose|starch|pectin|lignin",
      sep = "|"
    )
    add_reason(grepl(carrier_pattern, text_lower, perl = TRUE), "carrier_or_polymer_bound")
  }

  if (isTRUE(remove_reactive_small)) {
    reactive_pattern <- paste(
      "radical|superoxide|hydroxyl radical|nitric oxide|hydrogen peroxide",
      "peroxide radical|cyanide|hydrogen cyanide|carbon monoxide",
      sep = "|"
    )
    add_reason(grepl(reactive_pattern, text_lower, perl = TRUE), "reactive_or_transient_small_molecule")
  }

  vapply(reasons, function(value) {
    value <- unique(value)
    if (length(value) == 0) "" else paste(value, collapse = ";")
  }, character(1))
}

#' Clean PlantCyc compound SmartTable output for LC-MS annotation workflows
#'
#' @noRd
metminer_clean_plantcyc_compounds <- function(file,
                                              min_mw = 70,
                                              max_mw = 1500,
                                              low_mw_allowlist = character()) {
  raw <- utils::read.delim(
    file,
    check.names = FALSE,
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE
  )
  required <- c(
    "Compounds", "Chemical Formula", "Monoisotopic-Molecular-Weight",
    "InChI-Key", "SMILES", "Synonyms", "Common-Name"
  )
  missing_cols <- setdiff(required, colnames(raw))
  if (length(missing_cols) > 0) {
    stop("Missing required PlantCyc compound columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  compounds <- data.frame(
    compound_id = metminer_plantcyc_decode_html(raw[["Compounds"]]),
    formula = trimws(raw[["Chemical Formula"]]),
    monoisotopic_mw = metminer_plantcyc_parse_mw(raw[["Monoisotopic-Molecular-Weight"]]),
    inchi_key = sub("^InChIKey=", "", metminer_plantcyc_decode_html(raw[["InChI-Key"]])),
    smiles = metminer_plantcyc_decode_html(raw[["SMILES"]]),
    synonyms = metminer_plantcyc_decode_html(raw[["Synonyms"]]),
    compound_name = metminer_plantcyc_decode_html(raw[["Common-Name"]]),
    source = "PlantCyc_Fagopyrum_tataricum",
    stringsAsFactors = FALSE
  )

  compounds$filter_reason <- metminer_plantcyc_filter_reasons(
    compounds,
    min_mw = min_mw,
    max_mw = max_mw,
    remove_coa = TRUE,
    low_mw_allowlist = low_mw_allowlist
  )
  compounds$ms2_filter_reason <- metminer_plantcyc_filter_reasons(
    compounds,
    min_mw = min_mw,
    max_mw = max_mw,
    remove_coa = FALSE,
    low_mw_allowlist = low_mw_allowlist
  )

  clean <- compounds[!has_text(compounds$filter_reason), setdiff(colnames(compounds), c("filter_reason", "ms2_filter_reason")), drop = FALSE]
  ms2_eligible <- compounds[!has_text(compounds$ms2_filter_reason), setdiff(colnames(compounds), c("filter_reason", "ms2_filter_reason")), drop = FALSE]
  removed <- compounds[has_text(compounds$filter_reason), , drop = FALSE]
  removed$ms2_eligible <- !has_text(removed$ms2_filter_reason)

  list(
    clean_compounds = clean,
    ms2_eligible_compounds = ms2_eligible,
    removed_compounds = removed,
    summary = data.frame(
      metric = c("raw_compounds", "clean_compounds", "ms2_eligible_compounds", "removed_compounds", "coa_retained_for_ms2"),
      value = c(
        nrow(compounds), nrow(clean), nrow(ms2_eligible), nrow(removed),
        sum(grepl("coa_related", removed$filter_reason, fixed = TRUE) & removed$ms2_eligible, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  )
}

#' CoA/acyl-CoA MS2 diagnostic fragment rules
#'
#' @noRd
metminer_plantcyc_coa_fragment_rules <- function() {
  data.frame(
    rule_id = c(
      "coa_backbone_428",
      "adenosine_phosphate_261",
      "phosphate_related_159",
      "coa_neutral_loss_507",
      "high_mz_precursor",
      "formula_contains_P_S_N"
    ),
    evidence_type = c(
      "diagnostic_fragment",
      "supporting_fragment",
      "supporting_fragment",
      "neutral_loss",
      "precursor_context",
      "formula_context"
    ),
    mz = c(428.036, 261, 159, NA, NA, NA),
    tolerance_da = c(0.02, 0.05, 0.05, 0.05, NA, NA),
    polarity = c("negative_or_positive", "negative_or_positive", "negative_or_positive", "negative_or_positive", "any", "any"),
    description = c(
      "Characteristic CoA/ADP backbone fragment. Treat as the strongest diagnostic ion for CoA-related candidates.",
      "Adenosine phosphate-related fragment supporting a CoA backbone assignment.",
      "Phosphate-related fragment supporting a CoA backbone assignment.",
      "CoA-related neutral loss; exact expected loss depends on adduct and ionization mode.",
      "Acyl-CoA candidates usually have high precursor m/z and complex adduct/multiple-charge behavior.",
      "CoA-related candidates should contain phosphorus, sulfur, and nitrogen in the molecular formula."
    ),
    acceptance_note = c(
      "Require precursor mass consistency plus this fragment, or multiple supporting CoA fragments, before accepting a CoA candidate.",
      "Supporting evidence only; do not accept CoA solely from this ion.",
      "Supporting evidence only; do not accept CoA solely from this ion.",
      "Supporting evidence only; verify with observed product ions.",
      "Context only; high m/z is not diagnostic.",
      "Context only; formula pattern is not diagnostic."
    ),
    stringsAsFactors = FALSE
  )
}

#' Build a name-to-compound lookup from cleaned PlantCyc compounds
#'
#' @noRd
metminer_plantcyc_compound_lookup <- function(clean_compounds) {
  rows <- vector("list", nrow(clean_compounds))
  synonyms <- metminer_plantcyc_split_synonyms(clean_compounds$synonyms)
  for (i in seq_len(nrow(clean_compounds))) {
    names_i <- unique(c(clean_compounds$compound_name[i], synonyms[[i]]))
    names_i <- names_i[has_text(names_i)]
    rows[[i]] <- data.frame(
      compound_id = clean_compounds$compound_id[i],
      compound_name = clean_compounds$compound_name[i],
      match_name = names_i,
      match_key = metminer_plantcyc_normalize_name(names_i),
      stringsAsFactors = FALSE
    )
  }
  lookup <- do.call(rbind, rows)
  lookup <- lookup[has_text(lookup$match_key), , drop = FALSE]
  lookup[!duplicated(lookup$match_key), , drop = FALSE]
}

#' Clean PlantCyc pathway SmartTable output against a cleaned compound set
#'
#' @noRd
metminer_clean_plantcyc_pathways <- function(file, clean_compounds) {
  raw <- utils::read.delim(
    file,
    check.names = FALSE,
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE
  )
  required <- c("Pathways", "Object ID", "Compounds of pathway", "Reactions of pathway")
  missing_cols <- setdiff(required, colnames(raw))
  if (length(missing_cols) > 0) {
    stop("Missing required PlantCyc pathway columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  pathways <- data.frame(
    pathway_name = metminer_plantcyc_decode_html(raw[["Pathways"]]),
    pathway_id = metminer_plantcyc_decode_html(raw[["Object ID"]]),
    compounds_raw = metminer_plantcyc_decode_html(raw[["Compounds of pathway"]]),
    reactions = metminer_plantcyc_decode_html(raw[["Reactions of pathway"]]),
    stringsAsFactors = FALSE
  )

  lookup <- metminer_plantcyc_compound_lookup(clean_compounds)
  split_compounds <- strsplit(pathways$compounds_raw, " // ", fixed = TRUE)
  map_rows <- list()
  k <- 0L
  for (i in seq_len(nrow(pathways))) {
    compound_names <- trimws(split_compounds[[i]])
    compound_names <- compound_names[has_text(compound_names)]
    if (length(compound_names) == 0) next
    keys <- metminer_plantcyc_normalize_name(compound_names)
    matched <- lookup[match(keys, lookup$match_key), , drop = FALSE]
    keep <- !is.na(matched$compound_id)
    if (!any(keep)) next
    for (j in which(keep)) {
      k <- k + 1L
      map_rows[[k]] <- data.frame(
        pathway_id = pathways$pathway_id[i],
        pathway_name = pathways$pathway_name[i],
        compound_id = matched$compound_id[j],
        compound_name = matched$compound_name[j],
        pathway_compound_name = compound_names[j],
        reactions = pathways$reactions[i],
        stringsAsFactors = FALSE
      )
    }
  }

  pathway_compound_map <- if (length(map_rows) > 0) {
    do.call(rbind, map_rows)
  } else {
    data.frame(
      pathway_id = character(), pathway_name = character(),
      compound_id = character(), compound_name = character(),
      pathway_compound_name = character(), reactions = character(),
      stringsAsFactors = FALSE
    )
  }
  pathway_compound_map <- unique(pathway_compound_map)

  clean_pathways <- unique(pathway_compound_map[, c("pathway_id", "pathway_name", "reactions"), drop = FALSE])
  dropped_pathways <- pathways[!(pathways$pathway_id %in% clean_pathways$pathway_id), , drop = FALSE]

  list(
    clean_pathways = clean_pathways,
    pathway_compound_map = pathway_compound_map,
    dropped_pathways = dropped_pathways,
    summary = data.frame(
      metric = c(
        "raw_pathways", "clean_pathways", "dropped_pathways",
        "pathway_compound_links", "unique_pathway_compounds"
      ),
      value = c(
        nrow(pathways), nrow(clean_pathways), nrow(dropped_pathways),
        nrow(pathway_compound_map), length(unique(pathway_compound_map$compound_id))
      ),
      stringsAsFactors = FALSE
    )
  )
}

#' Clean PlantCyc compound and pathway SmartTable exports
#'
#' @noRd
metminer_clean_plantcyc_smarttables <- function(compound_file,
                                                pathway_file,
                                                output_dir = NULL,
                                                min_mw = 70,
                                                max_mw = 1500,
                                                low_mw_allowlist = character()) {
  compound_result <- metminer_clean_plantcyc_compounds(
    compound_file,
    min_mw = min_mw,
    max_mw = max_mw,
    low_mw_allowlist = low_mw_allowlist
  )
  pathway_result <- metminer_clean_plantcyc_pathways(
    pathway_file,
    clean_compounds = compound_result$clean_compounds
  )

  reason_table <- sort(table(unlist(strsplit(compound_result$removed_compounds$filter_reason, ";", fixed = TRUE))),
                       decreasing = TRUE)
  summary <- rbind(
    data.frame(section = "compounds", compound_result$summary, stringsAsFactors = FALSE),
    data.frame(section = "pathways", pathway_result$summary, stringsAsFactors = FALSE),
    data.frame(section = "compound_filter_reasons", metric = names(reason_table), value = as.integer(reason_table),
               stringsAsFactors = FALSE)
  )

  result <- list(
    clean_compounds = compound_result$clean_compounds,
    ms2_eligible_compounds = compound_result$ms2_eligible_compounds,
    removed_compounds = compound_result$removed_compounds,
    clean_pathways = pathway_result$clean_pathways,
    pathway_compound_map = pathway_result$pathway_compound_map,
    dropped_pathways = pathway_result$dropped_pathways,
    summary = summary
  )

  if (isTRUE(any(has_text(output_dir)))) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    utils::write.table(result$clean_compounds, file.path(output_dir, "plantcyc_clean_compounds.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$ms2_eligible_compounds, file.path(output_dir, "plantcyc_ms2_eligible_compounds.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$removed_compounds, file.path(output_dir, "plantcyc_removed_compounds.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$clean_pathways, file.path(output_dir, "plantcyc_clean_pathways.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$pathway_compound_map, file.path(output_dir, "plantcyc_pathway_compound_map.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$dropped_pathways, file.path(output_dir, "plantcyc_dropped_pathways.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(result$summary, file.path(output_dir, "plantcyc_cleaning_summary.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  }

  result
}

# ---- PlantCyc metid database construction ----

#' Build metid-compatible spectra.info rows from cleaned PlantCyc compounds
#'
#' @noRd
metminer_plantcyc_spectra_info <- function(clean_compounds) {
  get_col <- function(name, fallback = NA_character_) {
    if (name %in% colnames(clean_compounds)) clean_compounds[[name]] else rep(fallback, nrow(clean_compounds))
  }
  data.frame(
    Lab.ID = clean_compounds$compound_id,
    BIOCYC.ID = clean_compounds$compound_id,
    Compound.name = clean_compounds$compound_name,
    mz = as.numeric(clean_compounds$monoisotopic_mw),
    RT = NA_real_,
    CAS.ID = NA_character_,
    HMDB.ID = NA_character_,
    KEGG.ID = NA_character_,
    Formula = clean_compounds$formula,
    mz.pos = NA_real_,
    mz.neg = NA_real_,
    Submitter = "plantcyc <https://plantcyc.org/>",
    Synonyms = clean_compounds$synonyms,
    monisotopic_molecular_weight = as.numeric(clean_compounds$monoisotopic_mw),
    SMILES.ID = clean_compounds$smiles,
    INCHI.ID = NA_character_,
    INCHIKEY.ID = clean_compounds$inchi_key,
    Kingdom = get_col("Kingdom"),
    Super_class = get_col("Super_class"),
    Class = get_col("Class"),
    Sub_class = get_col("Sub_class"),
    direct_parent = get_col("direct_parent"),
    molecular_framework = get_col("molecular_framework"),
    classyfire_status = get_col("classyfire_status"),
    classyfire_source = get_col("classyfire_source"),
    source = clean_compounds$source,
    stringsAsFactors = FALSE
  )
}

# ---- Fiehn ClassyFire Batch helpers ----

#' Safely extract a ClassyFire node name
#'
#' @noRd
metminer_classyfire_node_name <- function(x) {
  if (is.null(x) || is.null(x$name) || !has_text(x$name)) NA_character_ else as.character(x$name)[1]
}

#' Parse a ClassyFire JSON object into one row
#'
#' @noRd
metminer_parse_classyfire_json <- function(x, inchikey) {
  data.frame(
    INCHIKEY.ID = inchikey,
    Kingdom = metminer_classyfire_node_name(x$kingdom),
    Super_class = metminer_classyfire_node_name(x$superclass),
    Class = metminer_classyfire_node_name(x$class),
    Sub_class = metminer_classyfire_node_name(x$subclass),
    direct_parent = metminer_classyfire_node_name(x$direct_parent),
    molecular_framework = if (!is.null(x$molecular_framework) && has_text(x$molecular_framework)) as.character(x$molecular_framework)[1] else NA_character_,
    classyfire_status = "completed",
    classyfire_source = "Fiehn ClassyFire Batch <https://cfb.fiehnlab.ucdavis.edu/>",
    stringsAsFactors = FALSE
  )
}

#' Fetch ClassyFire classifications from Fiehn CFB with cache and throttling
#'
#' @noRd
metminer_fetch_classyfire_batch <- function(inchikeys,
                                            cache_dir,
                                            sleep_sec = 2,
                                            max_retries = 3,
                                            progress = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for ClassyFire classification lookup.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for ClassyFire classification parsing.", call. = FALSE)
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  inchikeys <- unique(trimws(as.character(inchikeys)))
  inchikeys <- inchikeys[has_text(inchikeys)]
  inchikeys <- inchikeys[grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", inchikeys)]

  rows <- list()
  if (length(inchikeys) == 0) {
    return(data.frame(
      INCHIKEY.ID = character(), Kingdom = character(), Super_class = character(),
      Class = character(), Sub_class = character(), direct_parent = character(),
      molecular_framework = character(), classyfire_status = character(),
      classyfire_source = character(), stringsAsFactors = FALSE
    ))
  }

  for (i in seq_along(inchikeys)) {
    inchikey <- inchikeys[i]
    json_file <- file.path(cache_dir, paste0(inchikey, ".json"))
    miss_file <- file.path(cache_dir, paste0(inchikey, ".not_found"))
    fail_file <- file.path(cache_dir, paste0(inchikey, ".failed"))

    if (!is.null(progress)) {
      progress(i, length(inchikeys), inchikey)
    }

    if (file.exists(json_file)) {
      parsed <- tryCatch(jsonlite::fromJSON(json_file, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        rows[[length(rows) + 1L]] <- metminer_parse_classyfire_json(parsed, inchikey)
        next
      }
    }

    if (file.exists(miss_file)) {
      rows[[length(rows) + 1L]] <- data.frame(
        INCHIKEY.ID = inchikey, Kingdom = NA_character_, Super_class = NA_character_,
        Class = NA_character_, Sub_class = NA_character_, direct_parent = NA_character_,
        molecular_framework = NA_character_, classyfire_status = "not_found",
        classyfire_source = "Fiehn ClassyFire Batch <https://cfb.fiehnlab.ucdavis.edu/>",
        stringsAsFactors = FALSE
      )
      next
    }

    url <- paste0("https://cfb.fiehnlab.ucdavis.edu/entities/", inchikey, ".json")
    response <- NULL
    last_error <- NULL
    for (attempt in seq_len(max_retries)) {
      response <- tryCatch({
        httr2::request(url) |>
          httr2::req_user_agent("MetMiner PlantCyc database toolkit; respectful cached ClassyFire lookup") |>
          httr2::req_timeout(30) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
      }, error = function(e) {
        last_error <<- e$message
        NULL
      })
      if (!is.null(response) && httr2::resp_status(response) %in% c(200, 404)) break
      Sys.sleep(min(45, sleep_sec * attempt * 2))
    }

    if (is.null(response)) {
      writeLines(last_error %||% "request_failed", fail_file)
      rows[[length(rows) + 1L]] <- data.frame(
        INCHIKEY.ID = inchikey, Kingdom = NA_character_, Super_class = NA_character_,
        Class = NA_character_, Sub_class = NA_character_, direct_parent = NA_character_,
        molecular_framework = NA_character_, classyfire_status = "failed",
        classyfire_source = "Fiehn ClassyFire Batch <https://cfb.fiehnlab.ucdavis.edu/>",
        stringsAsFactors = FALSE
      )
      next
    }

    status <- httr2::resp_status(response)
    if (status == 404) {
      writeLines("not_found", miss_file)
      rows[[length(rows) + 1L]] <- data.frame(
        INCHIKEY.ID = inchikey, Kingdom = NA_character_, Super_class = NA_character_,
        Class = NA_character_, Sub_class = NA_character_, direct_parent = NA_character_,
        molecular_framework = NA_character_, classyfire_status = "not_found",
        classyfire_source = "Fiehn ClassyFire Batch <https://cfb.fiehnlab.ucdavis.edu/>",
        stringsAsFactors = FALSE
      )
      Sys.sleep(sleep_sec)
      next
    }

    body <- httr2::resp_body_string(response)
    writeLines(body, json_file, useBytes = TRUE)
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed) && length(parsed) > 1) {
      rows[[length(rows) + 1L]] <- metminer_parse_classyfire_json(parsed, inchikey)
    } else {
      writeLines("no_data", miss_file)
      rows[[length(rows) + 1L]] <- data.frame(
        INCHIKEY.ID = inchikey, Kingdom = NA_character_, Super_class = NA_character_,
        Class = NA_character_, Sub_class = NA_character_, direct_parent = NA_character_,
        molecular_framework = NA_character_, classyfire_status = "no_data",
        classyfire_source = "Fiehn ClassyFire Batch <https://cfb.fiehnlab.ucdavis.edu/>",
        stringsAsFactors = FALSE
      )
    }
    Sys.sleep(sleep_sec)
  }

  do.call(rbind, rows)
}

#' ClassyFire column names used by the PlantCyc toolkit
#'
#' @noRd
metminer_plantcyc_classyfire_cols <- function() {
  c(
    "Kingdom", "Super_class", "Class", "Sub_class", "direct_parent",
    "molecular_framework", "classyfire_status", "classyfire_source"
  )
}

#' Return the persistent PlantCyc ClassyFire cache file
#'
#' @noRd
metminer_plantcyc_classyfire_cache_file <- function() {
  file.path(tools::R_user_dir("MetMiner", which = "cache"), "plantcyc_classyfire", "plantcyc_classyfire_local.tsv")
}

#' Read the persistent PlantCyc ClassyFire cache
#'
#' @noRd
metminer_read_plantcyc_classyfire_cache <- function(cache_file = metminer_plantcyc_classyfire_cache_file()) {
  cols <- c(
    "compound_id", "inchi_key", "formula", "monoisotopic_mw", "smiles",
    "compound_name", metminer_plantcyc_classyfire_cols(), "cache_updated_at"
  )
  if (!file.exists(cache_file)) {
    out <- as.data.frame(stats::setNames(rep(list(character()), length(cols)), cols), stringsAsFactors = FALSE)
    return(out)
  }
  out <- utils::read.delim(cache_file, check.names = FALSE, quote = "", comment.char = "", stringsAsFactors = FALSE)
  missing_cols <- setdiff(cols, colnames(out))
  for (col in missing_cols) out[[col]] <- NA_character_
  out <- out[, cols, drop = FALSE]
  out$compound_id <- trimws(out$compound_id)
  out$inchi_key <- trimws(out$inchi_key)
  out <- out[has_text(out$compound_id) | has_text(out$inchi_key), , drop = FALSE]
  out[!duplicated(paste(out$compound_id, out$inchi_key, sep = "\r")), , drop = FALSE]
}

#' Write/update the persistent PlantCyc ClassyFire cache
#'
#' @noRd
metminer_update_plantcyc_classyfire_cache <- function(clean_compounds,
                                                      classification,
                                                      cache_file = metminer_plantcyc_classyfire_cache_file()) {
  if (nrow(classification) == 0) {
    return(metminer_read_plantcyc_classyfire_cache(cache_file))
  }
  cols <- metminer_plantcyc_classyfire_cols()
  keep <- match(clean_compounds$inchi_key, classification$INCHIKEY.ID)
  idx <- !is.na(keep)
  if (!any(idx)) {
    return(metminer_read_plantcyc_classyfire_cache(cache_file))
  }

  new_rows <- data.frame(
    compound_id = clean_compounds$compound_id[idx],
    inchi_key = clean_compounds$inchi_key[idx],
    formula = clean_compounds$formula[idx],
    monoisotopic_mw = as.character(clean_compounds$monoisotopic_mw[idx]),
    smiles = clean_compounds$smiles[idx],
    compound_name = clean_compounds$compound_name[idx],
    stringsAsFactors = FALSE
  )
  class_rows <- classification[keep[idx], cols, drop = FALSE]
  new_rows <- cbind(new_rows, class_rows)
  new_rows$cache_updated_at <- as.character(Sys.time())

  old_rows <- metminer_read_plantcyc_classyfire_cache(cache_file)
  all_rows <- rbind(old_rows, new_rows[, colnames(old_rows), drop = FALSE])
  cache_key <- paste(all_rows$compound_id, all_rows$inchi_key, sep = "\r")
  all_rows <- all_rows[rev(!duplicated(rev(cache_key))), , drop = FALSE]
  all_rows <- all_rows[order(all_rows$compound_id, all_rows$inchi_key), , drop = FALSE]

  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  utils::write.table(all_rows, cache_file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  all_rows
}

#' Match compounds against the persistent PlantCyc ClassyFire cache
#'
#' @noRd
metminer_match_plantcyc_classyfire_cache <- function(clean_compounds,
                                                     cache_file = metminer_plantcyc_classyfire_cache_file()) {
  cache <- metminer_read_plantcyc_classyfire_cache(cache_file)
  cols <- metminer_plantcyc_classyfire_cols()
  out <- data.frame(
    INCHIKEY.ID = clean_compounds$inchi_key,
    matrix(NA_character_, nrow = nrow(clean_compounds), ncol = length(cols), dimnames = list(NULL, cols)),
    stringsAsFactors = FALSE
  )
  cache_hit_type <- rep(NA_character_, nrow(clean_compounds))
  if (nrow(cache) == 0 || nrow(clean_compounds) == 0) {
    return(list(classification = out, hit_type = cache_hit_type, cache = cache))
  }

  by_id <- match(clean_compounds$compound_id, cache$compound_id)
  hit_id <- !is.na(by_id) & has_text(clean_compounds$compound_id)
  if (any(hit_id)) {
    out[hit_id, cols] <- cache[by_id[hit_id], cols, drop = FALSE]
    cache_hit_type[hit_id] <- "plantcyc_id"
  }

  by_key <- match(clean_compounds$inchi_key, cache$inchi_key)
  hit_key <- is.na(cache_hit_type) & !is.na(by_key) & has_text(clean_compounds$inchi_key)
  if (any(hit_key)) {
    out[hit_key, cols] <- cache[by_key[hit_key], cols, drop = FALSE]
    cache_hit_type[hit_key] <- "inchi_key"
  }

  out$classyfire_source[!is.na(cache_hit_type) & has_text(out$classyfire_source)] <- paste0(
    out$classyfire_source[!is.na(cache_hit_type) & has_text(out$classyfire_source)],
    "; local PlantCyc cache"
  )
  list(classification = out, hit_type = cache_hit_type, cache = cache)
}

#' Add Fiehn CFB/ClassyFire classification columns to cleaned PlantCyc compounds
#'
#' @noRd
metminer_add_classyfire_to_compounds <- function(clean_compounds,
                                                 cache_dir,
                                                 sleep_sec = 2,
                                                 max_retries = 3,
                                                 progress = NULL,
                                                 local_cache_file = metminer_plantcyc_classyfire_cache_file(),
                                                 use_local_cache = TRUE,
                                                 update_local_cache = TRUE) {
  cols <- metminer_plantcyc_classyfire_cols()
  cache_match <- if (isTRUE(use_local_cache)) {
    metminer_match_plantcyc_classyfire_cache(clean_compounds, local_cache_file)
  } else {
    list(
      classification = data.frame(
        INCHIKEY.ID = clean_compounds$inchi_key,
        matrix(NA_character_, nrow = nrow(clean_compounds), ncol = length(cols), dimnames = list(NULL, cols)),
        stringsAsFactors = FALSE
      ),
      hit_type = rep(NA_character_, nrow(clean_compounds)),
      cache = data.frame()
    )
  }

  needs_fetch <- is.na(cache_match$hit_type) & has_text(clean_compounds$inchi_key)
  fetched <- metminer_fetch_classyfire_batch(
    inchikeys = clean_compounds$inchi_key[needs_fetch],
    cache_dir = cache_dir,
    sleep_sec = sleep_sec,
    max_retries = max_retries,
    progress = progress
  )

  classification <- cache_match$classification
  if (nrow(fetched) > 0) {
    fetched_idx <- match(clean_compounds$inchi_key, fetched$INCHIKEY.ID)
    update_idx <- !is.na(fetched_idx)
    classification[update_idx, cols] <- fetched[fetched_idx[update_idx], cols, drop = FALSE]
  }
  classification$local_cache_hit <- cache_match$hit_type

  if (isTRUE(update_local_cache)) {
    metminer_update_plantcyc_classyfire_cache(
      clean_compounds = clean_compounds,
      classification = classification[has_text(classification$classyfire_status), c("INCHIKEY.ID", cols), drop = FALSE],
      cache_file = local_cache_file
    )
  }

  out <- clean_compounds
  for (col in cols) {
    if (!col %in% colnames(out)) out[[col]] <- NA_character_
    out[[col]] <- classification[[col]]
  }
  list(
    compounds = out,
    classification = classification,
    fetched_classification = fetched,
    local_cache_file = local_cache_file,
    local_cache_hits = sum(!is.na(cache_match$hit_type)),
    cfb_queries = length(unique(clean_compounds$inchi_key[needs_fetch & grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", clean_compounds$inchi_key)]))
  )
}

#' Create database.info metadata for PlantCyc metid databases
#'
#' @noRd
metminer_plantcyc_database_info <- function(version = as.character(Sys.Date())) {
  list(
    Version = version,
    Source = "plantcyc <https://plantcyc.org/>",
    Link = "https://plantcyc.org/",
    Creater = "codex+shawn",
    Email = NA_character_,
    RT = FALSE
  )
}

#' Sanitize user-defined PlantCyc output prefix
#'
#' @noRd
metminer_plantcyc_sanitize_prefix <- function(prefix = "plantcyc_custom") {
  prefix <- trimws(as.character(prefix %||% ""))
  if (!nzchar(prefix)) prefix <- "plantcyc_custom"
  prefix <- gsub("[^A-Za-z0-9_]+", "_", prefix)
  prefix <- gsub("_+", "_", prefix)
  prefix <- gsub("^_|_$", "", prefix)
  if (!nzchar(prefix)) prefix <- "plantcyc_custom"
  if (!grepl("^[A-Za-z]", prefix)) prefix <- paste0("plantcyc_", prefix)
  prefix
}

#' Save an R object with a user-defined object name inside an RDA file
#'
#' @noRd
metminer_save_named_rda <- function(object, object_name, file) {
  env <- new.env(parent = emptyenv())
  assign(object_name, object, envir = env)
  save(list = object_name, file = file, envir = env)
  invisible(file)
}

#' Construct a PlantCyc MS1-only metid databaseClass object
#'
#' @noRd
metminer_build_plantcyc_ms1_database <- function(clean_compounds,
                                                 version = as.character(Sys.Date())) {
  if (!requireNamespace("metid", quietly = TRUE)) {
    stop("Package 'metid' is required to construct databaseClass objects.", call. = FALSE)
  }
  methods::new(
    Class = "databaseClass",
    database.info = metminer_plantcyc_database_info(version),
    spectra.info = metminer_plantcyc_spectra_info(clean_compounds),
    spectra.data = list(Spectra.positive = list(), Spectra.negative = list())
  )
}

#' Build lookup tables for PlantCyc compound matching
#'
#' @noRd
metminer_plantcyc_build_match_index <- function(clean_compounds) {
  base <- data.frame(
    plantcyc_id = clean_compounds$compound_id,
    plantcyc_name = clean_compounds$compound_name,
    plantcyc_formula = clean_compounds$formula,
    plantcyc_mw = as.numeric(clean_compounds$monoisotopic_mw),
    plantcyc_inchikey = clean_compounds$inchi_key,
    plantcyc_smiles = clean_compounds$smiles,
    stringsAsFactors = FALSE
  )
  base$inchikey_key <- metminer_plantcyc_normalize_name(base$plantcyc_inchikey)
  base$inchikey_connectivity_key <- metminer_plantcyc_inchikey_connectivity(base$plantcyc_inchikey)
  base$smiles_key <- metminer_plantcyc_normalize_name(base$plantcyc_smiles)

  synonyms <- metminer_plantcyc_split_synonyms(clean_compounds$synonyms)
  name_rows <- vector("list", nrow(clean_compounds))
  for (i in seq_len(nrow(clean_compounds))) {
    names_i <- unique(c(clean_compounds$compound_name[i], synonyms[[i]]))
    names_i <- names_i[has_text(names_i)]
    if (length(names_i) == 0) {
      name_rows[[i]] <- data.frame(plantcyc_id = character(), name_key = character(), stringsAsFactors = FALSE)
      next
    }
    name_rows[[i]] <- data.frame(
      plantcyc_id = clean_compounds$compound_id[i],
      name_key = metminer_plantcyc_normalize_name(names_i),
      stringsAsFactors = FALSE
    )
  }
  name_index <- unique(do.call(rbind, name_rows))
  name_index <- name_index[has_text(name_index$name_key), , drop = FALSE]

  list(base = base, name_index = name_index)
}

#' Collapse text values for spectra.info provenance fields
#'
#' @noRd
metminer_plantcyc_collapse_unique <- function(x) {
  x <- as.character(x)
  x <- x[has_text(x)]
  x <- unique(x)
  if (length(x) == 0) NA_character_ else paste(x, collapse = "{}")
}

metminer_plantcyc_formula_counts <- function(formula) {
  formula <- trimws(as.character(formula %||% NA_character_))
  if (!has_text(formula)) return(NULL)
  formula <- gsub("[+-]+$", "", formula, perl = TRUE)
  pieces <- regmatches(formula, gregexpr("([A-Z][a-z]?)([0-9.]*)", formula, perl = TRUE))[[1]]
  if (length(pieces) == 0) return(NULL)
  counts <- numeric()
  for (piece in pieces) {
    hit <- regmatches(piece, regexec("^([A-Z][a-z]?)([0-9.]*)$", piece, perl = TRUE))[[1]]
    if (length(hit) < 3) next
    value <- if (has_text(hit[3])) suppressWarnings(as.numeric(hit[3])) else 1
    if (is.na(value)) return(NULL)
    current <- counts[hit[2]]
    if (is.na(current)) current <- 0
    counts[hit[2]] <- current + value
  }
  counts
}

metminer_plantcyc_formula_compatible <- function(plant_formula, public_formula, max_h_delta = 2) {
  n <- max(length(plant_formula), length(public_formula))
  plant_formula <- rep_len(as.character(plant_formula %||% NA_character_), n)
  public_formula <- rep_len(as.character(public_formula %||% NA_character_), n)
  vapply(seq_len(n), function(i) {
    a <- metminer_plantcyc_formula_counts(plant_formula[i])
    b <- metminer_plantcyc_formula_counts(public_formula[i])
    if (is.null(a) || is.null(b)) return(FALSE)
    elems <- union(names(a), names(b))
    non_h <- setdiff(elems, "H")
    a_non_h <- a[non_h]
    b_non_h <- b[non_h]
    a_non_h[is.na(a_non_h)] <- 0
    b_non_h[is.na(b_non_h)] <- 0
    same_non_h <- all(a_non_h == b_non_h)
    h_a <- a["H"]
    h_b <- b["H"]
    if (is.na(h_a)) h_a <- 0
    if (is.na(h_b)) h_b <- 0
    h_delta <- abs(h_a - h_b)
    same_non_h && !is.na(h_delta) && h_delta <= max_h_delta
  }, logical(1))
}

metminer_plantcyc_inchikey_connectivity <- function(x) {
  x <- toupper(trimws(as.character(x %||% NA_character_)))
  out <- rep(NA_character_, length(x))
  hit <- grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", x)
  out[hit] <- sub("^([A-Z]{14}-[A-Z]{10})-[A-Z]$", "\\1", x[hit])
  out
}

#' Add one library spectrum to a PlantCyc spectra.data list
#'
#' @noRd
metminer_plantcyc_add_spectrum <- function(spectra_data, polarity, plantcyc_id, ce_label, spectrum) {
  polarity_name <- if (tolower(polarity) == "negative") "Spectra.negative" else "Spectra.positive"
  if (is.null(spectra_data[[polarity_name]][[plantcyc_id]])) {
    spectra_data[[polarity_name]][[plantcyc_id]] <- list()
  }
  ce_label <- gsub("[^A-Za-z0-9_.+-]+", "_", ce_label)
  if (ce_label %in% names(spectra_data[[polarity_name]][[plantcyc_id]])) {
    ce_label <- paste0(ce_label, "__", length(spectra_data[[polarity_name]][[plantcyc_id]]) + 1L)
  }
  spectrum <- as.data.frame(spectrum)
  spectrum$mz <- suppressWarnings(as.numeric(spectrum$mz))
  spectrum$intensity <- suppressWarnings(as.numeric(spectrum$intensity))
  spectrum <- spectrum[!is.na(spectrum$mz) & !is.na(spectrum$intensity), c("mz", "intensity"), drop = FALSE]
  if (nrow(spectrum) > 0) {
    spectra_data[[polarity_name]][[plantcyc_id]][[ce_label]] <- spectrum
  }
  spectra_data
}

#' Find PlantCyc matches for public MS2 database rows
#'
#' @noRd
metminer_match_plantcyc_public_ms2 <- function(clean_compounds,
                                               public_info,
                                               source_database,
                                               mass_tolerance_ppm = 10,
                                               mass_tolerance_da = 0.01) {
  idx <- metminer_plantcyc_build_match_index(clean_compounds)
  base <- idx$base
  name_index <- idx$name_index

  public <- public_info
  public$source_database <- source_database
  public$source_row <- seq_len(nrow(public))
  for (col in c("Lab.ID", "Formula", "INCHIKEY.ID", "SMILES.ID", "Compound.name", "Synonyms", "Polarity")) {
    if (col %in% colnames(public)) public[[col]] <- as.character(public[[col]])
  }
  public_mz <- suppressWarnings(as.numeric(public$mz))
  public_inchikey <- if ("INCHIKEY.ID" %in% colnames(public)) public$INCHIKEY.ID else NA_character_
  public_smiles <- if ("SMILES.ID" %in% colnames(public)) public$SMILES.ID else NA_character_
  public_name <- if ("Compound.name" %in% colnames(public)) public$Compound.name else NA_character_
  public_synonyms <- if ("Synonyms" %in% colnames(public)) public$Synonyms else NA_character_

  rows <- list()
  add_matches <- function(row_i, candidates, match_type, rank) {
    if (nrow(candidates) == 0) return(invisible(NULL))
    candidates$public_mz <- public_mz[row_i]
    candidates$mass_error <- abs(candidates$plantcyc_mw - candidates$public_mz)
    candidates$mass_error_ppm <- candidates$mass_error * 1e6 / ifelse(candidates$plantcyc_mw < 400, 400, candidates$plantcyc_mw)
    candidates$formula_match <- !is.na(public$Formula[row_i]) & has_text(public$Formula[row_i]) &
      candidates$plantcyc_formula == public$Formula[row_i]
    candidates$formula_compatible <- candidates$formula_match |
      metminer_plantcyc_formula_compatible(candidates$plantcyc_formula, public$Formula[row_i])
    if (match_type != "inchikey") {
      candidates <- candidates[
        isTRUE(nrow(candidates) > 0) &
          candidates$formula_compatible &
          (candidates$mass_error <= mass_tolerance_da | candidates$mass_error_ppm <= mass_tolerance_ppm),
        ,
        drop = FALSE
      ]
    }
    if (nrow(candidates) == 0) return(invisible(NULL))
    rows[[length(rows) + 1L]] <<- data.frame(
      plantcyc_id = candidates$plantcyc_id,
      public_row = public$source_row[row_i],
      source_database = source_database,
      source_lab_id = public$Lab.ID[row_i],
      source_compound_name = public$Compound.name[row_i],
      match_type = match_type,
      match_rank = rank,
      formula_match = candidates$formula_match,
      formula_compatible = candidates$formula_compatible,
      mass_error = candidates$mass_error,
      mass_error_ppm = candidates$mass_error_ppm,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  for (i in seq_len(nrow(public))) {
    key <- metminer_plantcyc_normalize_name(public_inchikey[i])
    if (has_text(key)) {
      candidates <- base[base$inchikey_key == key & has_text(base$inchikey_key), , drop = FALSE]
      add_matches(i, candidates, "inchikey", 1L)
      if (nrow(candidates) > 0) next
    }

    connectivity_key <- metminer_plantcyc_inchikey_connectivity(public_inchikey[i])
    if (has_text(connectivity_key)) {
      candidates <- base[
        base$inchikey_connectivity_key == connectivity_key & has_text(base$inchikey_connectivity_key),
        ,
        drop = FALSE
      ]
      add_matches(i, candidates, "inchikey_connectivity_formula_mass", 2L)
      if (length(rows) > 0 && any(rows[[length(rows)]]$public_row == i)) next
    }

    key <- metminer_plantcyc_normalize_name(public_smiles[i])
    if (has_text(key)) {
      candidates <- base[base$smiles_key == key & has_text(base$smiles_key), , drop = FALSE]
      add_matches(i, candidates, "smiles_formula_mass", 3L)
      if (length(rows) > 0 && any(rows[[length(rows)]]$public_row == i)) next
    }

    names_i <- unique(c(public_name[i], unlist(strsplit(coerce_text(public_synonyms[i]), "\\{\\}", perl = TRUE))))
    name_keys <- unique(metminer_plantcyc_normalize_name(names_i))
    name_keys <- name_keys[has_text(name_keys)]
    if (length(name_keys) > 0) {
      hit_ids <- unique(name_index$plantcyc_id[name_index$name_key %in% name_keys])
      candidates <- base[base$plantcyc_id %in% hit_ids, , drop = FALSE]
      add_matches(i, candidates, "name_formula_mass", 4L)
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      plantcyc_id = character(), public_row = integer(), source_database = character(),
      source_lab_id = character(), source_compound_name = character(), match_type = character(),
      match_rank = integer(), formula_match = logical(), formula_compatible = logical(), mass_error = numeric(),
      mass_error_ppm = numeric(), stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$public_row, out$match_rank, out$mass_error_ppm), , drop = FALSE]
  out <- out[!duplicated(paste(out$public_row, out$plantcyc_id)), , drop = FALSE]
  out
}

#' Construct a PlantCyc-linked MS2 metid databaseClass object from massdbbuildin
#'
#' @noRd
metminer_build_plantcyc_ms2_database <- function(clean_compounds,
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

  plant_info <- metminer_plantcyc_spectra_info(clean_compounds)
  all_matches <- list()
  spectra_data <- list(Spectra.positive = list(), Spectra.negative = list())

  for (db_id in builtin_databases) {
    env <- new.env(parent = emptyenv())
    utils::data(list = db_id, package = "massdbbuildin", envir = env)
    public_db <- get(db_id, envir = env)
    source_database <- public_db@database.info$Source %||% db_id
    public_info <- as.data.frame(public_db@spectra.info, stringsAsFactors = FALSE)
    public_info$Lab.ID <- as.character(public_info$Lab.ID)

    matches <- metminer_match_plantcyc_public_ms2(
      clean_compounds = clean_compounds,
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
        spectra_data <- metminer_plantcyc_add_spectrum(
          spectra_data = spectra_data,
          polarity = polarity,
          plantcyc_id = matches$plantcyc_id[i],
          ce_label = ce_label,
          spectrum = source_spectra[[ce_name]]
        )
      }
    }
  }

  match_log <- if (length(all_matches) > 0) do.call(rbind, all_matches) else
    data.frame(
      plantcyc_id = character(), public_row = integer(), source_database = character(),
      source_lab_id = character(), source_compound_name = character(), match_type = character(),
      match_rank = integer(), formula_match = logical(), formula_compatible = logical(), mass_error = numeric(),
      mass_error_ppm = numeric(), stringsAsFactors = FALSE
    )
  rownames(match_log) <- NULL

  ms2_ids <- unique(c(names(spectra_data$Spectra.positive), names(spectra_data$Spectra.negative)))
  spectra_info <- plant_info[plant_info$Lab.ID %in% ms2_ids, , drop = FALSE]

  if (nrow(spectra_info) > 0 && nrow(match_log) > 0) {
    provenance <- lapply(split(match_log, match_log$plantcyc_id), function(x) {
      data.frame(
        Lab.ID = x$plantcyc_id[1],
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
    database.info = metminer_plantcyc_database_info(version),
    spectra.info = as.data.frame(spectra_info, stringsAsFactors = FALSE),
    spectra.data = spectra_data
  )

  list(
    database = database,
    match_log = match_log,
    unmatched_compounds = plant_info[!(plant_info$Lab.ID %in% ms2_ids), , drop = FALSE]
  )
}

#' Build both PlantCyc MS1 and MS2 metid databases and optional QC files
#'
#' @noRd
metminer_build_plantcyc_databases <- function(compound_file,
                                              pathway_file,
                                              output_dir,
                                              output_prefix = "plantcyc_custom",
                                              version = as.character(Sys.Date()),
                                              min_mw = 70,
                                              max_mw = 1500,
                                              mass_tolerance_ppm = 10,
                                              mass_tolerance_da = 0.01,
                                              fetch_classyfire = FALSE,
                                              classyfire_cache_dir = file.path(output_dir, "classyfire_cache"),
                                              classyfire_sleep_sec = 2,
                                              classyfire_max_retries = 3,
                                              classyfire_local_cache_file = metminer_plantcyc_classyfire_cache_file()) {
  output_prefix <- metminer_plantcyc_sanitize_prefix(output_prefix)
  clean_result <- metminer_clean_plantcyc_smarttables(
    compound_file = compound_file,
    pathway_file = pathway_file,
    output_dir = file.path(output_dir, "cleaned"),
    min_mw = min_mw,
    max_mw = max_mw
  )

  classyfire_result <- NULL
  classyfire_local_cache_hits <- 0
  classyfire_cfb_queries <- 0
  if (isTRUE(fetch_classyfire)) {
    classyfire_result <- metminer_add_classyfire_to_compounds(
      clean_compounds = clean_result$clean_compounds,
      cache_dir = classyfire_cache_dir,
      sleep_sec = classyfire_sleep_sec,
      max_retries = classyfire_max_retries,
      local_cache_file = classyfire_local_cache_file
    )
    clean_result$clean_compounds <- classyfire_result$compounds
    ms2_classyfire_result <- metminer_add_classyfire_to_compounds(
      clean_compounds = clean_result$ms2_eligible_compounds,
      cache_dir = classyfire_cache_dir,
      sleep_sec = classyfire_sleep_sec,
      max_retries = classyfire_max_retries,
      local_cache_file = classyfire_local_cache_file
    )
    clean_result$ms2_eligible_compounds <- ms2_classyfire_result$compounds
    classyfire_result$classification <- unique(rbind(
      classyfire_result$classification,
      ms2_classyfire_result$classification
    ))
    classyfire_local_cache_hits <- classyfire_result$local_cache_hits + ms2_classyfire_result$local_cache_hits
    classyfire_cfb_queries <- classyfire_result$cfb_queries + ms2_classyfire_result$cfb_queries
  }

  ms1_db <- metminer_build_plantcyc_ms1_database(clean_result$clean_compounds, version = version)
  ms2_result <- metminer_build_plantcyc_ms2_database(
    clean_compounds = clean_result$ms2_eligible_compounds,
    version = version,
    mass_tolerance_ppm = mass_tolerance_ppm,
    mass_tolerance_da = mass_tolerance_da
  )
  ms2_db <- ms2_result$database
  pathway_result <- metminer_build_plantcyc_pathway_database(
    pathway_compound_map = clean_result$pathway_compound_map,
    clean_pathways = clean_result$clean_pathways,
    version = version
  )
  pathway_db <- pathway_result$database

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  rda_files <- list(
    ms1 = paste0(output_prefix, "_ms1.rda"),
    ms2 = paste0(output_prefix, "_ms2.rda"),
    pathway = paste0(output_prefix, "_pathway.rda")
  )
  metminer_save_named_rda(ms1_db, paste0(output_prefix, "_ms1"), file.path(output_dir, rda_files$ms1))
  metminer_save_named_rda(ms2_db, paste0(output_prefix, "_ms2"), file.path(output_dir, rda_files$ms2))
  metminer_save_named_rda(pathway_db, paste0(output_prefix, "_pathway"), file.path(output_dir, rda_files$pathway))

  utils::write.table(ms1_db@spectra.info, file.path(output_dir, paste0(output_prefix, "_ms1_spectra_info.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(clean_result$ms2_eligible_compounds, file.path(output_dir, paste0(output_prefix, "_ms2_eligible_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(metminer_plantcyc_coa_fragment_rules(), file.path(output_dir, paste0(output_prefix, "_coa_fragment_rules.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_db@spectra.info, file.path(output_dir, paste0(output_prefix, "_ms2_spectra_info.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$match_log, file.path(output_dir, paste0(output_prefix, "_ms2_match_log.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(ms2_result$unmatched_compounds, file.path(output_dir, paste0(output_prefix, "_ms2_unmatched_compounds.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  utils::write.table(pathway_result$reaction_table, file.path(output_dir, paste0(output_prefix, "_pathway_reaction_table.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  if (!is.null(classyfire_result)) {
    utils::write.table(classyfire_result$classification, file.path(output_dir, paste0(output_prefix, "_classyfire_classification.tsv")),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    utils::write.table(clean_result$clean_compounds, file.path(output_dir, paste0(output_prefix, "_clean_compounds_with_classyfire.tsv")),
                       sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    file.copy(classyfire_result$local_cache_file, file.path(output_dir, paste0(output_prefix, "_classyfire_local_cache_snapshot.tsv")), overwrite = TRUE)
  }

  positive_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.positive, length, integer(1)))
  negative_spectra <- sum(vapply(ms2_db@spectra.data$Spectra.negative, length, integer(1)))
  summary <- data.frame(
    metric = c(
      "ms1_compounds", "ms2_compounds", "ms2_positive_compounds", "ms2_negative_compounds",
      "ms2_positive_spectra", "ms2_negative_spectra", "ms2_match_rows", "ms2_unmatched_compounds",
      "ms2_eligible_compounds", "coa_retained_for_ms2",
      "pathway_count", "pathway_compound_links", "pathway_reaction_links",
      "classyfire_completed", "classyfire_not_found", "classyfire_failed",
      "classyfire_local_cache_hits", "classyfire_cfb_queries"
    ),
    value = c(
      nrow(ms1_db@spectra.info), nrow(ms2_db@spectra.info),
      length(ms2_db@spectra.data$Spectra.positive), length(ms2_db@spectra.data$Spectra.negative),
      positive_spectra, negative_spectra, nrow(ms2_result$match_log), nrow(ms2_result$unmatched_compounds),
      nrow(clean_result$ms2_eligible_compounds),
      sum(grepl("coa_related", clean_result$removed_compounds$filter_reason, fixed = TRUE) &
            clean_result$removed_compounds$ms2_eligible, na.rm = TRUE),
      length(pathway_db@pathway_id), nrow(clean_result$pathway_compound_map), nrow(pathway_result$reaction_table),
      if (!is.null(classyfire_result)) sum(classyfire_result$classification$classyfire_status == "completed", na.rm = TRUE) else 0,
      if (!is.null(classyfire_result)) sum(classyfire_result$classification$classyfire_status %in% c("not_found", "no_data"), na.rm = TRUE) else 0,
      if (!is.null(classyfire_result)) sum(classyfire_result$classification$classyfire_status == "failed", na.rm = TRUE) else 0,
      classyfire_local_cache_hits,
      classyfire_cfb_queries
    ),
    stringsAsFactors = FALSE
  )
  source_summary <- if (nrow(ms2_result$match_log) > 0) {
    data.frame(metric = paste0("match_source_", names(table(ms2_result$match_log$source_database))),
               value = as.integer(table(ms2_result$match_log$source_database)),
               stringsAsFactors = FALSE)
  } else data.frame(metric = character(), value = integer(), stringsAsFactors = FALSE)
  type_summary <- if (nrow(ms2_result$match_log) > 0) {
    data.frame(metric = paste0("match_type_", names(table(ms2_result$match_log$match_type))),
               value = as.integer(table(ms2_result$match_log$match_type)),
               stringsAsFactors = FALSE)
  } else data.frame(metric = character(), value = integer(), stringsAsFactors = FALSE)
  summary <- rbind(summary, source_summary, type_summary)
  utils::write.table(summary, file.path(output_dir, paste0(output_prefix, "_database_summary.tsv")),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "")

  list(
    ms1_database = ms1_db,
    ms2_database = ms2_db,
    pathway_database = pathway_db,
    clean_result = clean_result,
    ms2_match_log = ms2_result$match_log,
    ms2_unmatched_compounds = ms2_result$unmatched_compounds,
    pathway_reaction_table = pathway_result$reaction_table,
    coa_fragment_rules = metminer_plantcyc_coa_fragment_rules(),
    classyfire_classification = if (!is.null(classyfire_result)) classyfire_result$classification else data.frame(),
    summary = summary,
    output_dir = output_dir,
    output_prefix = output_prefix,
    rda_files = rda_files
  )
}

#' Validate PlantCyc metid database objects
#'
#' @noRd
metminer_validate_plantcyc_database <- function(database, clean_compounds = NULL) {
  required <- c("Lab.ID", "Compound.name", "mz", "RT", "CAS.ID", "HMDB.ID", "KEGG.ID", "Formula")
  data.frame(
    check = c("is_databaseClass", "has_required_columns", "lab_id_in_plantcyc"),
    ok = c(
      methods::is(database, "databaseClass"),
      all(required %in% colnames(database@spectra.info)),
      is.null(clean_compounds) || all(database@spectra.info$Lab.ID %in% clean_compounds$compound_id)
    ),
    stringsAsFactors = FALSE
  )
}

# ---- PlantCyc metpath pathway database construction ----

#' Assign broad pathway classes from PlantCyc pathway names
#'
#' @noRd
metminer_plantcyc_assign_pathway_class <- function(pathway_name) {
  nm <- tolower(pathway_name %||% "")
  if (grepl("degradation|catabolism|oxidation|lyase|hydrolysis", nm)) {
    "Metabolism; Catabolism"
  } else if (grepl("biosynthesis|synthase|synthetase|cyclase|carboxylase|polymerization", nm)) {
    "Metabolism; Biosynthesis"
  } else if (grepl("respiration|electron trans|photosynthesis|photo|calvin|tca|glycolysis|gluconeogenesis|pentose phosphate|fermentation|glyoxylate", nm)) {
    "Metabolism; Energy metabolism"
  } else if (grepl("activation|conjugation|glucosylation|glycosylation|methylation|acetylation|hydroxylation|detoxification|salvage", nm)) {
    "Metabolism; Biotransformation"
  } else if (grepl("signaling|signal|hormone|jasmonic|salicylic|abscisic|gibberellin|auxin|cytokinin|brassinosteroid|ethylene|strigolactone", nm)) {
    "Metabolism; Signaling and hormones"
  } else if (grepl("transport|transloc|channel|pump|carrier|exporter|importer", nm)) {
    "Transport"
  } else {
    "Metabolism; Other"
  }
}

#' Split PlantCyc reaction strings and keep reaction-like entries
#'
#' @noRd
metminer_plantcyc_reaction_table <- function(clean_pathways) {
  if (is.null(clean_pathways) || nrow(clean_pathways) == 0) {
    return(data.frame(pathway_id = character(), pathway_name = character(), reaction = character(),
                      stringsAsFactors = FALSE))
  }
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(clean_pathways))) {
    reactions <- clean_pathways$reactions[i]
    if (!has_text(reactions)) next
    parts <- trimws(strsplit(reactions, " // ", fixed = TRUE)[[1]])
    parts <- parts[has_text(parts)]
    parts <- parts[grepl("->|<-|<->|<=>|-->|<--", parts)]
    if (length(parts) == 0) next
    for (reaction in parts) {
      k <- k + 1L
      rows[[k]] <- data.frame(
        pathway_id = clean_pathways$pathway_id[i],
        pathway_name = clean_pathways$pathway_name[i],
        reaction = reaction,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(pathway_id = character(), pathway_name = character(), reaction = character(),
                      stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, rows))
}

#' Construct a PlantCyc metpath pathway_database object
#'
#' @noRd
metminer_build_plantcyc_pathway_database <- function(pathway_compound_map,
                                                     clean_pathways = NULL,
                                                     version = as.character(Sys.Date()),
                                                     organism = "Fagopyrum tataricum") {
  if (!requireNamespace("metpath", quietly = TRUE)) {
    stop("Package 'metpath' is required to construct pathway_database objects.", call. = FALSE)
  }
  if (is.null(pathway_compound_map) || nrow(pathway_compound_map) == 0) {
    stop("No pathway-compound links are available for pathway database construction.", call. = FALSE)
  }

  pathway_compound_map <- unique(pathway_compound_map[, c("pathway_id", "pathway_name", "compound_id", "compound_name"), drop = FALSE])
  pathway_compound_map <- pathway_compound_map[order(pathway_compound_map$pathway_id, pathway_compound_map$compound_id), , drop = FALSE]
  pathway_unique <- unique(pathway_compound_map[, c("pathway_id", "pathway_name"), drop = FALSE])
  pathway_unique <- pathway_unique[order(pathway_unique$pathway_id), , drop = FALSE]

  pathway_ids <- pathway_unique$pathway_id
  pathway_names <- pathway_unique$pathway_name
  names(pathway_names) <- pathway_ids

  compound_list <- lapply(pathway_ids, function(pathway_id) {
    rows <- pathway_compound_map[pathway_compound_map$pathway_id == pathway_id, c("compound_id", "compound_name"), drop = FALSE]
    rows <- unique(rows)
    rows <- rows[order(rows$compound_id), , drop = FALSE]
    colnames(rows) <- c("PlantCyc.ID", "compound_name")
    rownames(rows) <- NULL
    rows
  })
  names(compound_list) <- pathway_ids

  reaction_table <- metminer_plantcyc_reaction_table(clean_pathways)
  describtion <- lapply(pathway_ids, function(pathway_id) {
    reactions <- reaction_table$reaction[reaction_table$pathway_id == pathway_id]
    if (length(reactions) == 0) "" else paste(unique(reactions), collapse = " // ")
  })
  names(describtion) <- pathway_ids

  pathway_class <- lapply(pathway_names, metminer_plantcyc_assign_pathway_class)
  names(pathway_class) <- pathway_ids

  empty_gene_df <- data.frame(gene_id = character(0), gene_name = character(0), stringsAsFactors = FALSE)
  gene_list <- replicate(length(pathway_ids), empty_gene_df, simplify = FALSE)
  names(gene_list) <- pathway_ids
  protein_list <- gene_list

  database_info <- list(
    source = "plantcyc <https://plantcyc.org/>",
    version = version,
    source_url = "https://plantcyc.org/",
    organism = organism,
    creater = "codex+shawn",
    email = NA_character_,
    description = paste0(
      "PlantCyc pathway database for ", organism,
      ", filtered to the cleaned PlantCyc LC-MS metabolite background. ",
      "Compound identifiers are PlantCyc frame IDs."
    ),
    date_created = as.character(Sys.Date())
  )

  database <- methods::new(
    "pathway_database",
    database_info = database_info,
    pathway_id = pathway_ids,
    pathway_name = pathway_names,
    describtion = describtion,
    pathway_class = pathway_class,
    gene_list = gene_list,
    compound_list = compound_list,
    protein_list = protein_list,
    reference_list = list(),
    related_disease = list(),
    related_module = list()
  )

  list(database = database, reaction_table = reaction_table)
}
