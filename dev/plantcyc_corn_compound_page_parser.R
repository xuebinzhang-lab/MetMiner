#!/usr/bin/env Rscript

# PlantCyc compound-page cache parser for CornCyc.
#
# Important:
# PlantCyc robots.txt currently disallows automated crawling. This script does
# not fetch remote pages. It only:
#   1. builds a URL manifest for PlantCyc compound pages/tabs;
#   2. parses HTML files that have already been saved locally with permission or
#      by manual browser export.
#
# Expected cache filenames:
#   <compound_id>_SUMMARY.html, e.g. CPD-1777_SUMMARY.html
#   <compound_id>_RXNS.html    optional
#   <compound_id>_MAIN.html    optional
#
# Example:
#   Rscript dev/plantcyc_corn_compound_page_parser.R \
#     --ids Temp/maize_clean_compounds.tsv \
#     --cache Temp/plantcyc_corn_cache \
#     --out Temp/plantcyc_corn_compound_details.tsv

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[[idx + 1]]
}

has_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(rep(FALSE, length(x)))
  x <- as.character(x)
  !is.na(x) & nzchar(x)
}

ids_file <- arg_value("--ids")
cache_dir <- arg_value("--cache", "Temp/plantcyc_corn_cache")
out_file <- arg_value("--out", file.path(cache_dir, "plantcyc_corn_compound_details.tsv"))
manifest_file <- arg_value("--manifest", file.path(cache_dir, "plantcyc_corn_manifest.tsv"))
orgid <- arg_value("--orgid", "CORN")

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

read_ids <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    html_files <- list.files(cache_dir, pattern = "_SUMMARY[.]html$", full.names = FALSE)
    return(sort(unique(sub("_SUMMARY[.]html$", "", html_files))))
  }

  ext <- tolower(tools::file_ext(path))
  tab <- if (identical(ext, "csv")) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  }

  id_cols <- c("compound_id", "Lab.ID", "BIOCYC.ID", "PlantCyc.ID", "Compounds", "Object ID")
  id_col <- id_cols[id_cols %in% colnames(tab)][1]
  if (is.na(id_col)) {
    stop("Cannot find a compound ID column. Expected one of: ", paste(id_cols, collapse = ", "), call. = FALSE)
  }
  ids <- trimws(as.character(tab[[id_col]]))
  sort(unique(ids[has_text(ids)]))
}

compound_ids <- read_ids(ids_file)

manifest <- data.frame(
  compound_id = compound_ids,
  main_url = sprintf("https://pmn.plantcyc.org/compound?orgid=%s&id=%s", orgid, compound_ids),
  summary_url = sprintf("https://pmn.plantcyc.org/cpd-tab?id=%s&orgid=%s&tab=SUMMARY", compound_ids, orgid),
  rxns_url = sprintf("https://pmn.plantcyc.org/cpd-tab?id=%s&orgid=%s&tab=RXNS", compound_ids, orgid),
  summary_cache_file = file.path(cache_dir, paste0(compound_ids, "_SUMMARY.html")),
  rxns_cache_file = file.path(cache_dir, paste0(compound_ids, "_RXNS.html")),
  main_cache_file = file.path(cache_dir, paste0(compound_ids, "_MAIN.html")),
  stringsAsFactors = FALSE
)
utils::write.table(manifest, manifest_file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")

html_entities <- function(x) {
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&beta;", "beta", x, fixed = TRUE)
  x <- gsub("&alpha;", "alpha", x, fixed = TRUE)
  x <- gsub("&gamma;", "gamma", x, fixed = TRUE)
  x <- gsub("&Delta;", "Delta", x, fixed = TRUE)
  x <- gsub("&deg;", "deg", x, fixed = TRUE)
  x <- gsub("&rarr;", "->", x, fixed = TRUE)
  x <- gsub("&#945;", "alpha", x, fixed = TRUE)
  x <- gsub("&#946;", "beta", x, fixed = TRUE)
  x
}

html_to_text <- function(file) {
  if (!file.exists(file)) return("")
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- gsub("<script[^>]*>.*?</script>", " ", html, ignore.case = TRUE, perl = TRUE)
  html <- gsub("<style[^>]*>.*?</style>", " ", html, ignore.case = TRUE, perl = TRUE)
  html <- gsub("<[^>]+>", " ", html, perl = TRUE)
  html <- html_entities(html)
  html <- gsub("\\s+", " ", html, perl = TRUE)
  trimws(html)
}

strip_html_fragment <- function(x) {
  x <- gsub("<[sS][uU][bB]>(.*?)</[sS][uU][bB]>", "\\1", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- html_entities(x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

extract_between <- function(text, left, right) {
  pattern <- paste0(left, "\\s*(.*?)\\s*", right)
  m <- regexec(pattern, text, ignore.case = TRUE, perl = TRUE)
  hit <- regmatches(text, m)[[1]]
  if (length(hit) >= 2) trimws(hit[[2]]) else NA_character_
}

extract_regex <- function(text, pattern) {
  m <- regexec(pattern, text, ignore.case = TRUE, perl = TRUE)
  hit <- regmatches(text, m)[[1]]
  if (length(hit) >= 2) trimws(hit[[2]]) else NA_character_
}

parse_summary <- function(compound_id, file) {
  text <- html_to_text(file)
  if (!has_text(text)) {
    return(data.frame(
      compound_id = compound_id,
      parse_status = "missing_summary_cache",
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    compound_id = compound_id,
    parse_status = "ok",
    biocyc_id = extract_between(text, "BioCyc Id", "Synonyms|SMILES|InChI|InChIKey|Standard Gibbs"),
    synonyms = extract_between(text, "Synonyms", "SMILES|InChI|InChIKey|Standard Gibbs|Unification Links"),
    smiles = extract_between(text, "SMILES", "InChI\\s+InChI=|InChIKey|Standard Gibbs|Unification Links"),
    inchi = extract_regex(text, "(InChI=1S?\\/.*?)(?:\\s+InChIKey|\\s+Standard Gibbs|\\s+Unification Links)"),
    inchikey = extract_regex(text, "InChIKey(?:=|\\s+)([A-Z]{14}-[A-Z]{10}-[A-Z])"),
    chebi_id = extract_regex(text, "ChEBI\\s+([0-9]+)"),
    chemspider_id = extract_regex(text, "ChemSpider\\s+([0-9]+)"),
    hmdb_id = extract_regex(text, "HMDB\\s+(HMDB[0-9]+)"),
    kegg_id = extract_regex(text, "Kegg\\s+(C[0-9]{5})"),
    metanetx_id = extract_regex(text, "MetaNetX\\s+(MNXM[0-9]+)"),
    pubchem_id = extract_regex(text, "PubChem\\s+([0-9]+)"),
    refmet_name = extract_between(text, "RefMet", "Seed|ZINC|Credits|$"),
    seed_id = extract_regex(text, "Seed\\s+(cpd[0-9]+)"),
    zinc_id = extract_regex(text, "ZINC\\s+(ZINC[0-9]+)"),
    stringsAsFactors = FALSE
  )
}

parse_main <- function(file) {
  if (!file.exists(file)) {
    return(data.frame(
      compound_name = NA_character_,
      chemical_formula = NA_character_,
      molecular_weight = NA_real_,
      monoisotopic_mass = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  text <- html_to_text(file)
  compound_name <- extract_regex(html, "<font class=['\"]header['\"]>(.*?)</font>")
  compound_name <- strip_html_fragment(compound_name)
  formula_html <- extract_regex(html, "(?s)Chemical Formula</th>\\s*<td[^>]*>(.*?)</td>")
  formula <- strip_html_fragment(formula_html)
  formula <- gsub("\\s+", "", formula)

  data.frame(
    compound_name = compound_name,
    chemical_formula = formula,
    molecular_weight = suppressWarnings(as.numeric(extract_regex(text, "Molecular Weight\\s+([0-9.]+)\\s+Daltons"))),
    monoisotopic_mass = suppressWarnings(as.numeric(extract_regex(text, "Monoisotopic Mass\\s+([0-9.]+)\\s+Daltons"))),
    stringsAsFactors = FALSE
  )
}

parse_rxns <- function(file) {
  text <- html_to_text(file)
  if (!has_text(text)) {
    return(data.frame(
      reaction_ids = NA_character_,
      pathway_ids = NA_character_,
      pathway_names = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  reaction_ids <- unique(unlist(regmatches(text, gregexpr("\\b[A-Z0-9_.-]+-RXN\\b|\\bRXN-[0-9]+\\b", text, perl = TRUE))))
  pathway_ids <- unique(unlist(regmatches(text, gregexpr("\\bPWY-[0-9A-Z_.-]+\\b", text, perl = TRUE))))
  pathway_names <- unique(unlist(regmatches(text, gregexpr("Pathway\\s*:\\s*([^']+?)\\s+ID\\s*:", text, perl = TRUE))))
  pathway_names <- gsub("^Pathway\\s*:\\s*", "", pathway_names)
  pathway_names <- gsub("\\s+ID\\s*:$", "", pathway_names)
  data.frame(
    reaction_ids = paste(reaction_ids, collapse = ";"),
    pathway_ids = paste(pathway_ids, collapse = ";"),
    pathway_names = paste(pathway_names, collapse = ";"),
    stringsAsFactors = FALSE
  )
}

rows <- lapply(seq_len(nrow(manifest)), function(i) {
  compound_id <- manifest$compound_id[i]
  summary <- parse_summary(compound_id, manifest$summary_cache_file[i])
  main <- parse_main(manifest$main_cache_file[i])
  rxns <- parse_rxns(manifest$rxns_cache_file[i])
  cbind(summary, main, rxns, stringsAsFactors = FALSE)
})

out <- do.call(rbind, rows)
utils::write.table(out, out_file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")

message("Manifest written: ", manifest_file)
message("Parsed rows: ", nrow(out))
message("Output written: ", out_file)
