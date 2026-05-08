# ---- Annotation database helpers ----

#' Built-in metid database metadata
#'
#' @noRd
metminer_builtin_annotation_databases <- function() {
  choices <- c(
    "HMDB MS2" = "hmdb_ms2",
    "MassBank MS2" = "massbank_ms2",
    "MoNA MS2" = "mona_ms2"
  )

  if (!requireNamespace("massdbbuildin", quietly = TRUE)) {
    return(choices)
  }

  available <- tryCatch({
    utils::data(package = "massdbbuildin")$results[, "Item"]
  }, error = function(e) character())

  if (length(available) == 0) {
    return(choices)
  }
  choices[choices %in% available]
}

#' Load a databaseClass object from massdbbuildin
#'
#' @noRd
metminer_load_builtin_database <- function(database_id) {
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
  if (!has_text(directory)) {
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

#' Collect annotation databases from built-in ids and a local folder
#'
#' @noRd
metminer_collect_annotation_databases <- function(builtin_ids = character(), local_dir = NULL) {
  builtin_ids <- builtin_ids %||% character()
  dbs <- c(
    lapply(builtin_ids, metminer_load_builtin_database),
    metminer_load_local_databases(local_dir)
  )

  if (length(dbs) == 0) {
    stop("No annotation database selected or loaded.", call. = FALSE)
  }
  dbs
}

# ---- Annotation execution ----

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

  for (db in databases) {
    out <- metid::annotate_metabolites_mass_dataset(
      object = out,
      ms1.match.ppm = ms1.match.ppm,
      ms2.match.ppm = ms2.match.ppm,
      rt.match.tol = rt.match.tol,
      polarity = polarity,
      column = column,
      candidate.num = candidate.num,
      database = db$database,
      threads = threads
    )
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
      databases = databases, table = data.frame()
    ))
  }

  variable_info <- massdataset::extract_variable_info(object)
  total_features <- nrow(variable_info)
  annotation_table <- metminer_safe_extract_annotation_table(object)

  if (is.null(annotation_table) || nrow(annotation_table) == 0 || !"Level" %in% colnames(annotation_table)) {
    return(list(
      total_features = total_features, level1 = 0, level2 = 0, level3 = 0,
      annotated = 0, unannotated = total_features,
      databases = databases, table = annotation_table %||% data.frame()
    ))
  }

  levels <- annotation_table[, c("variable_id", "Level")]
  levels <- levels[!is.na(levels$variable_id) & !is.na(levels$Level), , drop = FALSE]
  levels$Level <- as.integer(levels$Level)
  if (nrow(levels) == 0) {
    return(list(
      total_features = total_features, level1 = 0, level2 = 0, level3 = 0,
      annotated = 0, unannotated = total_features,
      databases = databases, table = annotation_table
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
    table = annotation_table
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

  paste0(
    "-- Metabolite Annotation (", tools::toTitleCase(mode), " Mode) --\n\n",
    "Databases: ", database_text, "\n",
    "Total features: ", status$total_features, "\n",
    "Level 1 features: ", status$level1, "\n",
    "Level 2 features: ", status$level2, "\n",
    "Level 3 features: ", status$level3, "\n",
    "Annotated features (Level 1-3): ", status$annotated, "\n",
    "Unannotated features: ", status$unannotated
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
    parent_compound_key <- if (!is.null(parent_sel) && nrow(parent_sel) > 0) parent_sel$compound_key[1] else NA_character_
    parent_compound <- if (!is.null(parent_sel) && "Compound.name" %in% colnames(parent_sel)) parent_sel$Compound.name[1] else NA_character_

    feature_roles <- lapply(ids, function(feature_id) {
      sel <- selection_by_feature[[feature_id]]
      info <- info_by_feature[[feature_id]]
      relation <- describe_feature_relation_to_parent(feature_id, parent_id, subnet_edges)
      interpretation <- interpret_feature_annotation_role(sel, relation$role, parent_compound_key)

      data.frame(
        sub_network = sid,
        rt = if (!is.null(info) && nrow(info) > 0) round(info$rt[1], 3) else NA_real_,
        feature_id = feature_id,
        mz = if (!is.null(info) && nrow(info) > 0) round(info$mz[1], 5) else NA_real_,
        mean_area = if (!is.null(info) && nrow(info) > 0) round(info$mean_area[1], 3) else NA_real_,
        parent_feature_id = parent_id,
        network_role = relation$role,
        relation_to_parent = relation$relation,
        edge_evidence = relation$edge_evidence,
        selected_compound = if (!is.null(sel) && "Compound.name" %in% colnames(sel)) sel$Compound.name[1] else NA_character_,
        selected_adduct = if (!is.null(sel) && "Adduct" %in% colnames(sel)) sel$Adduct[1] else NA_character_,
        compound_key = if (!is.null(sel) && "compound_key" %in% colnames(sel)) sel$compound_key[1] else NA_character_,
        metid_level = if (!is.null(sel) && "Level" %in% colnames(sel)) sel$Level[1] else NA_integer_,
        metid_rank = if (!is.null(sel) && "candidate_rank" %in% colnames(sel)) sel$candidate_rank[1] else NA_integer_,
        metid_total_score = if (!is.null(sel) && "Total.score" %in% colnames(sel)) round(sel$Total.score[1], 4) else NA_real_,
        network_final_score = if (!is.null(sel) && "final_score" %in% colnames(sel)) sel$final_score[1] else NA_real_,
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
      putative_real_compound = parent_compound %||% NA_character_,
      parent_annotation_level = if (!is.null(parent_sel) && "Level" %in% colnames(parent_sel)) parent_sel$Level[1] else NA_integer_,
      parent_candidate_rank = if (!is.null(parent_sel) && "candidate_rank" %in% colnames(parent_sel)) parent_sel$candidate_rank[1] else NA_integer_,
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
