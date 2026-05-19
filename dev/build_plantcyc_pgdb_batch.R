#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "/Users/shawn/temp_file/MetminerV2-Plantcyc"
output_dir <- if (length(args) >= 2) args[[2]] else "data"
table_dir <- if (length(args) >= 3) args[[3]] else file.path("inst", "extdata", "plantcyc_pgdb_tables")

if (!dir.exists(input_dir)) {
  stop("Input directory does not exist: ", input_dir, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

if (!"package:MetMiner" %in% search()) {
  devtools::load_all(".", quiet = TRUE)
}

specs <- data.frame(
  file = c(
    "aracyc.tar",
    "breadwheatcyc.tar",
    "corncyc.tar",
    "fagopyrumtataricumcyc.tar",
    "gossypium_hirsutumcyc.tar",
    "oilseedrapecyc.tar",
    "oryzacyc.tar",
    "plantcyc.tar",
    "soycyc.tar",
    "tomatocyc.tar"
  ),
  prefix = c(
    "plantcyc_ath",
    "plantcyc_tae",
    "plantcyc_zma",
    "plantcyc_ftt",
    "plantcyc_ghi",
    "plantcyc_bna",
    "plantcyc_osa",
    "plantcyc_ref",
    "plantcyc_gma",
    "plantcyc_sly"
  ),
  organism = c(
    "Arabidopsis thaliana",
    "Triticum aestivum (bread wheat)",
    "Zea mays (maize)",
    "Fagopyrum tataricum",
    "Gossypium hirsutum",
    "Brassica napus (oilseed rape)",
    "Oryza sativa (rice)",
    "PlantCyc reference database",
    "Glycine max (soybean)",
    "Solanum lycopersicum (tomato)"
  ),
  stringsAsFactors = FALSE
)

rows <- list()
for (i in seq_len(nrow(specs))) {
  archive <- file.path(input_dir, specs$file[i])
  if (!file.exists(archive)) {
    warning("Skipping missing archive: ", archive, call. = FALSE)
    next
  }
  message("[", i, "/", nrow(specs), "] Building ", specs$prefix[i], " from ", basename(archive))
  data_dir <- metminer_pgdb_unpack_archive(archive)
  version <- basename(dirname(data_dir))
  pgdb_id <- basename(dirname(dirname(data_dir)))
  result <- metminer_pgdb_build_outputs(
    pgdb_dir = dirname(data_dir),
    output_dir = output_dir,
    output_prefix = specs$prefix[i],
    organism = specs$organism[i]
  )
  table_files <- result$files[grepl("[.]tsv$", unlist(result$files), ignore.case = TRUE)]
  for (table_file in unlist(table_files, use.names = FALSE)) {
    if (file.exists(table_file)) {
      file.copy(table_file, file.path(table_dir, basename(table_file)), overwrite = TRUE)
      unlink(table_file)
    }
  }
  summary_named <- stats::setNames(result$summary$count, result$summary$item)
  rows[[length(rows) + 1]] <- data.frame(
    prefix = specs$prefix[i],
    archive = basename(archive),
    pgdb_id = pgdb_id,
    version = version,
    organism = specs$organism[i],
    raw_compounds = unname(summary_named["raw_compounds"] %||% NA_integer_),
    ms1_database_records = unname(summary_named["ms1_database_records"] %||% NA_integer_),
    ms2_database_records = unname(summary_named["ms2_database_records"] %||% NA_integer_),
    removed_from_ms1 = unname(summary_named["removed_from_ms1"] %||% NA_integer_),
    ms2_eligible_compounds = unname(summary_named["ms2_eligible_compounds"] %||% NA_integer_),
    coa_retained_for_ms2 = unname(summary_named["coa_retained_for_ms2"] %||% NA_integer_),
    pathways = unname(summary_named["pathways"] %||% NA_integer_),
    reactions = unname(summary_named["reactions"] %||% NA_integer_),
    pathway_compound_links = unname(summary_named["pathway_compound_links"] %||% NA_integer_),
    ms1_file = basename(result$files$ms1),
    ms2_file = basename(result$files$ms2),
    pathway_file = basename(result$files$pathway),
    local_object_file = basename(result$files$local_object),
    stringsAsFactors = FALSE
  )
}

plantcyc_pgdb_manifest <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
save(plantcyc_pgdb_manifest, file = file.path(output_dir, "plantcyc_pgdb_manifest.rda"))
utils::write.table(
  plantcyc_pgdb_manifest,
  file.path(table_dir, "plantcyc_pgdb_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)

print(plantcyc_pgdb_manifest)
message("Batch PlantCyc PGDB build completed: ", normalizePath(output_dir, mustWork = FALSE))
