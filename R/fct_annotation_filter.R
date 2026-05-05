# ---- Build non-redundant annotation tables ----

#' Build non-redundant metabolite annotation tables across polarities
#'
#' @noRd
metminer_filter_redundant_annotations <- function(positive_object = NULL,
                                                   negative_object = NULL,
                                                   rt_tolerance = 10,
                                                   min_high_conf_level = 2,
                                                   use_network_validation = TRUE) {
  pos <- metminer_build_annotation_filter_mode_table(
    positive_object,
    mode = "positive",
    min_high_conf_level = min_high_conf_level,
    use_network_validation = use_network_validation
  )
  neg <- metminer_build_annotation_filter_mode_table(
    negative_object,
    mode = "negative",
    min_high_conf_level = min_high_conf_level,
    use_network_validation = use_network_validation
  )

  candidates <- dplyr::bind_rows(pos$candidates, neg$candidates)
  if (nrow(candidates) == 0) {
    return(list(final_table = data.frame(), redundancy_table = data.frame(), mode_tables = list(positive = pos, negative = neg)))
  }

  candidates <- metminer_dedup_same_compound_features(candidates)
  candidates <- metminer_mark_cross_polarity_duplicates(candidates, rt_tolerance = rt_tolerance)
  final_table <- candidates[candidates$keep, , drop = FALSE]
  final_table <- final_table[order(final_table$rt, final_table$metabolite_id), , drop = FALSE]

  list(
    final_table = final_table,
    redundancy_table = candidates,
    mode_tables = list(positive = pos, negative = neg)
  )
}

# ---- Per-mode table builders ----

metminer_build_annotation_filter_mode_table <- function(object,
                                                        mode,
                                                        min_high_conf_level = 2,
                                                        use_network_validation = TRUE) {
  if (is.null(object)) {
    return(list(candidates = data.frame(), feature_table = data.frame()))
  }

  annotation_table <- metminer_safe_extract_annotation_table(object)
  variable_info <- metminer_safe_extract_variable_info(object)
  expression_data <- metminer_safe_extract_expression_data(object)
  feature_info <- metminer_annotation_filter_feature_info(variable_info, expression_data)
  best_feature_annotations <- metminer_annotation_filter_best_candidates(
    annotation_table, use_network_validation = use_network_validation, object = object
  )

  validation <- if (isTRUE(use_network_validation)) metminer_extract_annotation_validation(object) else list()
  roles <- validation$feature_role_interpretation
  hypothesis <- validation$subnetwork_hypothesis

  if (!is.null(roles) && nrow(roles) > 0 && !is.null(hypothesis) && nrow(hypothesis) > 0) {
    network_rows <- metminer_build_network_metabolite_rows(roles, hypothesis, feature_info, mode, min_high_conf_level)
  } else {
    network_rows <- data.frame()
  }

  network_features <- if (nrow(network_rows) > 0) {
    unique(unlist(strsplit(paste(network_rows$member_features, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
  } else {
    character()
  }

  single_rows <- metminer_build_single_feature_metabolite_rows(
    best_feature_annotations = best_feature_annotations,
    feature_info = feature_info,
    mode = mode,
    network_features = network_features,
    min_high_conf_level = min_high_conf_level
  )

  candidates <- dplyr::bind_rows(network_rows, single_rows)
  candidates$keep <- TRUE
  candidates$redundancy_reason <- ifelse(candidates$record_type == "sub_network", "network_representative", "single_feature")
  candidates$cross_polarity_group <- NA_character_
  candidates$preferred_polarity <- NA_character_
  candidates$drop_reason <- NA_character_
  candidates <- metminer_add_recurrent_annotation_context(candidates, object)

  list(candidates = candidates, feature_table = best_feature_annotations)
}

# ---- Feature info & candidate selection ----

metminer_annotation_filter_feature_info <- function(variable_info, expression_data = NULL) {
  info <- as.data.frame(variable_info, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(info)) {
    return(data.frame(variable_id = character(), mz = numeric(), rt = numeric(), mean_area = numeric()))
  }
  info$variable_id <- as.character(info$variable_id)
  for (col in c("mz", "rt")) {
    if (!col %in% colnames(info)) info[[col]] <- NA_real_
  }
  info$mean_area <- NA_real_
  if (!is.null(expression_data)) {
    expr <- as.matrix(expression_data)
    mean_area <- rowMeans(expr, na.rm = TRUE)
    info$mean_area <- as.numeric(mean_area[match(info$variable_id, names(mean_area))])
  }
  info[, c("variable_id", "mz", "rt", "mean_area"), drop = FALSE]
}

#' Pick best annotation candidate per feature, optionally using network scores
#'
#' @noRd
metminer_annotation_filter_best_candidates <- function(annotation_table, use_network_validation = TRUE, object = NULL) {
  candidates <- metminer_prepare_annotation_candidates(annotation_table)
  if (nrow(candidates) == 0) return(data.frame())

  if (isTRUE(use_network_validation) && !is.null(object)) {
    validation <- metminer_extract_annotation_validation(object)
    validated <- validation$candidate_validation
    if (!is.null(validated) && nrow(validated) > 0) {
      candidates <- validated
    } else {
      # No validation ran yet — fall back to metid-only scores
      candidates$metid_score_norm <- normalize_annotation_score(candidates$Total.score)
      candidates$network_support_score <- 0
      candidates$network_conflict_score <- 0
      candidates$final_score <- candidates$metid_score_norm
    }
  } else {
    candidates$metid_score_norm <- normalize_annotation_score(candidates$Total.score)
    candidates$network_support_score <- 0
    candidates$network_conflict_score <- 0
    candidates$final_score <- candidates$metid_score_norm
  }

  metminer_select_network_refined_candidates(candidates, use_network = use_network_validation)
}

# ---- Network metabolite rows ----

metminer_build_network_metabolite_rows <- function(roles, hypothesis, feature_info, mode, min_high_conf_level) {
  rows <- lapply(seq_len(nrow(hypothesis)), function(i) {
    hyp <- hypothesis[i, , drop = FALSE]
    sub_roles <- roles[roles$sub_network == hyp$sub_network, , drop = FALSE]
    if (nrow(sub_roles) == 0) return(NULL)

    parent <- sub_roles[sub_roles$feature_id == hyp$parent_feature_id, , drop = FALSE]
    if (nrow(parent) == 0) parent <- sub_roles[1, , drop = FALSE]

    compound <- parent$selected_compound[1]
    level <- suppressWarnings(as.integer(parent$metid_level[1]))
    representative_adduct <- if ("selected_adduct" %in% colnames(parent)) parent$selected_adduct[1] else NA_character_
    semi <- !has_text(compound) || is.na(level) || level > min_high_conf_level
    semi_label <- infer_semi_annotation_label(sub_roles)

    member_features <- unique(sub_roles$feature_id)
    member_info <- feature_info[match(member_features, feature_info$variable_id), , drop = FALSE]
    total_area <- sum(member_info$mean_area, na.rm = TRUE)

    data.frame(
      metabolite_id = paste0(toupper(substr(mode, 1, 3)), "_SN", hyp$sub_network),
      record_type = "sub_network",
      mode = mode,
      representative_feature = hyp$parent_feature_id,
      member_features = paste(member_features, collapse = ";"),
      compound_name = if (has_text(compound)) as.character(compound) else semi_label,
      annotation_level = level,
      representative_adduct = representative_adduct,
      member_adducts = metminer_collapse_unique_text(sub_roles$selected_adduct),
      member_annotation_levels = metminer_collapse_unique_text(sub_roles$metid_level),
      confidence_class = if (!semi && level <= min_high_conf_level) "high_confidence_network" else "semi_annotated_network",
      semi_annotation = if (semi) semi_label else NA_character_,
      mz = parent$mz[1],
      rt = parent$rt[1],
      mean_area = total_area,
      n_features = length(member_features),
      isotope_features = hyp$isotope_features,
      adduct_features = hyp$adduct_features,
      isf_features = hyp$isf_features,
      compound_key = parent$compound_key[1],
      source_note = hyp$interpretation,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

# ---- Single-feature rows ----

metminer_build_single_feature_metabolite_rows <- function(best_feature_annotations,
                                                          feature_info,
                                                          mode,
                                                          network_features,
                                                          min_high_conf_level) {
  if (nrow(best_feature_annotations) == 0) return(data.frame())
  singles <- best_feature_annotations[!best_feature_annotations$variable_id %in% network_features, , drop = FALSE]
  if (nrow(singles) == 0) return(data.frame())

  info <- feature_info[match(singles$variable_id, feature_info$variable_id), , drop = FALSE]
  level <- suppressWarnings(as.integer(singles$Level))
  compound <- if ("Compound.name" %in% colnames(singles)) singles$Compound.name else NA_character_
  adduct <- if ("Adduct" %in% colnames(singles)) singles$Adduct else NA_character_

  data.frame(
    metabolite_id = singles$variable_id,
    record_type = "single_feature",
    mode = mode,
    representative_feature = singles$variable_id,
    member_features = singles$variable_id,
    compound_name = ifelse(has_text(compound), as.character(compound), "unknown_feature"),
    annotation_level = level,
    representative_adduct = adduct,
    member_adducts = adduct,
    member_annotation_levels = as.character(level),
    confidence_class = ifelse(!is.na(level) & level <= min_high_conf_level, "high_confidence_single", "low_confidence_best_candidate"),
    semi_annotation = NA_character_,
    mz = info$mz,
    rt = info$rt,
    mean_area = info$mean_area,
    n_features = 1L,
    isotope_features = 0L,
    adduct_features = 0L,
    isf_features = 0L,
    compound_key = singles$compound_key,
    source_note = ifelse(!is.na(level) & level <= min_high_conf_level, "single feature level<=threshold", "single feature best level3/unknown candidate"),
    stringsAsFactors = FALSE
  )
}

metminer_collapse_unique_text <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  x <- trimws(as.character(x))
  x <- unique(x[has_text(x)])
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(x, collapse = ";")
}

metminer_ensure_filter_context_columns <- function(candidates) {
  defaults <- list(
    keep = TRUE,
    redundancy_reason = NA_character_,
    cross_polarity_group = NA_character_,
    preferred_polarity = NA_character_,
    drop_reason = NA_character_,
    network_roles = NA_character_,
    parent_feature_ids = NA_character_,
    recurrent_ion_groups = NA_character_,
    recurrent_center_mz = NA_character_,
    recurrent_instance_count = NA_integer_,
    recurrent_parent_resolved_n = NA_integer_,
    recurrent_status = "none",
    suspected_interference = FALSE,
    interference_reason = NA_character_
  )
  for (nm in names(defaults)) {
    if (!nm %in% colnames(candidates)) {
      candidates[[nm]] <- defaults[[nm]]
    }
  }
  candidates
}

metminer_add_recurrent_annotation_context <- function(candidates, object) {
  candidates <- metminer_ensure_filter_context_columns(candidates)
  if (is.null(object) || nrow(candidates) == 0) {
    return(candidates)
  }

  feature_roles <- tryCatch(extract_feature_network_roles(object), error = function(e) empty_feature_network_roles())
  recurrent <- tryCatch(extract_recurrent_ion_network(object), error = function(e) empty_recurrent_ion_network())
  if (nrow(recurrent$groups) == 0) {
    recurrent <- tryCatch(
      build_recurrent_ion_network(
        variable_info = metminer_safe_extract_variable_info(object),
        expression_data = metminer_safe_extract_expression_data(object),
        feature_roles = feature_roles
      ),
      error = function(e) empty_recurrent_ion_network()
    )
  }

  if (nrow(feature_roles) == 0 && nrow(recurrent$nodes) == 0) {
    return(candidates)
  }

  role_by_feature <- if (nrow(feature_roles) > 0) split(feature_roles, feature_roles$feature_id) else list()
  rec_nodes <- recurrent$nodes
  rec_nodes <- rec_nodes[has_text(rec_nodes$feature_id), , drop = FALSE]
  rec_by_feature <- if (nrow(rec_nodes) > 0) split(rec_nodes, rec_nodes$feature_id) else list()
  group_by_id <- if (nrow(recurrent$groups) > 0) split(recurrent$groups, recurrent$groups$ion_group_id) else list()

  for (i in seq_len(nrow(candidates))) {
    features <- metminer_split_feature_list(candidates$member_features[i])
    roles <- dplyr::bind_rows(role_by_feature[intersect(features, names(role_by_feature))])
    rec <- dplyr::bind_rows(rec_by_feature[intersect(features, names(rec_by_feature))])

    if (nrow(roles) > 0 && "network_role" %in% colnames(roles)) {
      candidates$network_roles[i] <- paste(unique(roles$network_role[has_text(roles$network_role)]), collapse = ";")
      parents <- if ("parent_feature_id" %in% colnames(roles)) unique(roles$parent_feature_id[has_text(roles$parent_feature_id)]) else character()
      candidates$parent_feature_ids[i] <- if (length(parents) > 0) paste(parents, collapse = ";") else NA_character_
    }

    if (nrow(rec) == 0) {
      next
    }

    group_ids <- unique(rec$ion_group_id[has_text(rec$ion_group_id)])
    group_rows <- dplyr::bind_rows(group_by_id[intersect(group_ids, names(group_by_id))])
    candidates$recurrent_ion_groups[i] <- paste(group_ids, collapse = ";")
    candidates$recurrent_center_mz[i] <- if (nrow(group_rows) > 0) {
      paste(sprintf("%.5f", group_rows$center_mz), collapse = ";")
    } else {
      NA_character_
    }
    candidates$recurrent_instance_count[i] <- if (nrow(group_rows) > 0) metminer_safe_int_max(group_rows$n_instances) else nrow(rec)
    candidates$recurrent_parent_resolved_n[i] <- if (nrow(group_rows) > 0) metminer_safe_int_max(group_rows$parent_resolved_n) else 0L

    role_text <- if (nrow(roles) > 0 && "network_role" %in% colnames(roles)) {
      unique(roles$network_role[has_text(roles$network_role)])
    } else {
      character()
    }
    resolved_isf <- any(role_text %in% c("isf_fragment_of_parent", "cross_polarity_isf_fragment"))
    parent_like <- any(role_text %in% c("putative_parent", "parent_ion"))
    if (resolved_isf && parent_like) {
      candidates$recurrent_status[i] <- "recurrent_network_contains_isf"
      candidates$suspected_interference[i] <- TRUE
      candidates$interference_reason[i] <- "network contains recurrent m/z features resolved as ISF; representative is kept as the RT-local parent hypothesis"
    } else if (resolved_isf) {
      candidates$recurrent_status[i] <- "resolved_recurrent_isf"
      candidates$suspected_interference[i] <- TRUE
      candidates$interference_reason[i] <- "same m/z appears at multiple RTs; RT-local network resolves this feature as an ISF"
    } else if (parent_like) {
      candidates$recurrent_status[i] <- "recurrent_parent_candidate"
    } else {
      candidates$recurrent_status[i] <- "unresolved_recurrent_ion"
      candidates$suspected_interference[i] <- TRUE
      candidates$interference_reason[i] <- "same m/z appears at multiple RTs; parent source is unresolved"
    }
  }

  candidates
}

metminer_safe_int_max <- function(x) {
  x <- suppressWarnings(as.integer(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(0L)
  max(x)
}

metminer_split_feature_list <- function(x) {
  if (is.null(x) || length(x) == 0 || !has_text(x[1])) {
    return(character())
  }
  out <- unique(unlist(strsplit(as.character(x[1]), ";", fixed = TRUE), use.names = FALSE))
  trimws(out[has_text(out)])
}

infer_semi_annotation_label <- function(sub_roles) {
  evidence <- sub_roles$edge_evidence[has_text(sub_roles$edge_evidence)]
  if (length(evidence) == 0) {
    return("semi-annotated network compound")
  }

  # Parse edge evidence: "TYPE | ANNOTATION | r=X.XXX"
  parsed <- lapply(strsplit(evidence, " \\| "), function(parts) {
    list(
      type = if (length(parts) >= 1) trimws(parts[1]) else NA_character_,
      annotation = if (length(parts) >= 2) trimws(parts[2]) else NA_character_
    )
  })

  types <- vapply(parsed, `[[`, character(1), "type")
  annotations <- vapply(parsed, `[[`, character(1), "annotation")
  annotations <- annotations[has_text(annotations)]

  # Collect neutral losses from ISF edges
  isf_mask <- grepl("ISF", types, ignore.case = TRUE)
  nl_set <- character()
  if (any(isf_mask)) {
    isf_ann <- annotations[isf_mask]
    # Within-mode ISF: annotation = "CH2O"
    # Cross-polarity ISF: annotation = "... -CH2O"
    nl_set <- unique(trimws(gsub("^.*\\s-", "", isf_ann)))
    nl_set <- nl_set[has_text(nl_set)]
  }

  # Count edge-type evidence
  iso_n <- sum(grepl("isotope", types, ignore.case = TRUE))
  add_n <- sum(grepl("adduct", types, ignore.case = TRUE))
  isf_n <- sum(isf_mask)

  # Build descriptive parts
  detail_parts <- character()
  if (length(nl_set) > 0) {
    detail_parts <- c(detail_parts, paste0("NL: ", paste(utils::head(nl_set, 5), collapse = ", ")))
  }

  edge_parts <- c()
  if (isf_n > 0) edge_parts <- c(edge_parts, paste0("ISF×", isf_n))
  if (iso_n > 0) edge_parts <- c(edge_parts, paste0("iso×", iso_n))
  if (add_n > 0) edge_parts <- c(edge_parts, paste0("adduct×", add_n))
  if (length(edge_parts) > 0) {
    detail_parts <- c(detail_parts, paste(edge_parts, collapse = ", "))
  }

  if (length(detail_parts) > 0) {
    return(paste0("semi-annotated (", paste(detail_parts, collapse = " | "), ")"))
  }
  "semi-annotated network compound"
}

# ---- Within-mode compound-based deduplication ----

#' Deduplicate features that share the same compound_key within a polarity
#'
#' Two strategies:
#' **Type A** — same m/z (within 10 ppm), different RT → merge, keep highest area.
#' **Type B** — different m/z, same compound_key → check mass difference
#' against neutral-loss / adduct / isotope tables. If a known relationship is
#' found the child feature is merged into the parent. Features whose mass
#' difference cannot be explained stay as separate rows.
#'
#' @noRd
metminer_dedup_same_compound_features <- function(candidates, mz_ppm = 10) {
  if (nrow(candidates) == 0) return(candidates)
  candidates <- metminer_ensure_filter_context_columns(candidates)

  # Operate within each mode, within each compound_key
  modes <- unique(candidates$mode)
  out <- vector("list", length(modes))
  names(out) <- modes

  for (md in modes) {
    mode_rows <- candidates[candidates$mode == md, , drop = FALSE]
    key_groups <- split(seq_len(nrow(mode_rows)),
                        ifelse(has_text(mode_rows$compound_key),
                               mode_rows$compound_key,
                               paste0("__nokey_", seq_len(nrow(mode_rows)))))

    merged_rows <- list()
    merge_idx <- 1L

    for (group_name in names(key_groups)) {
      idx <- key_groups[[group_name]]
      if (length(idx) == 1) {
        merged_rows[[merge_idx]] <- mode_rows[idx, , drop = FALSE]
        merge_idx <- merge_idx + 1L
        next
      }

      rows <- mode_rows[idx, , drop = FALSE]
      # Separate network rows from single-feature rows
      net_mask <- rows$record_type == "sub_network"
      single_mask <- !net_mask

      net_rows <- rows[net_mask, , drop = FALSE]
      single_rows <- rows[single_mask, , drop = FALSE]

      # --- Type A: same m/z (within ppm), different RT ---
      if (nrow(single_rows) > 1) {
        single_rows <- metminer_resolve_same_mz_recurrent_rows(single_rows, mz_ppm)
        active_single_rows <- single_rows[single_rows$keep, , drop = FALSE]
        inactive_single_rows <- single_rows[!single_rows$keep, , drop = FALSE]
        if (nrow(active_single_rows) > 1) {
          active_single_rows <- metminer_merge_same_mz_rows(active_single_rows, mz_ppm)
        }
        single_rows <- dplyr::bind_rows(active_single_rows, inactive_single_rows)
      }

      # --- Type B: mass-difference check ---
      if (nrow(single_rows) > 1) {
        single_rows <- metminer_merge_mass_difference_rows(single_rows, mz_ppm)
      }

      # Merge single rows into existing network rows if compound_key matches
      if (nrow(net_rows) > 0 && nrow(single_rows) > 0) {
        result <- metminer_merge_singles_into_networks(net_rows, single_rows, mz_ppm)
        net_rows <- result$net_rows
        single_rows <- result$single_rows
      }

      merged_rows[[merge_idx]] <- dplyr::bind_rows(net_rows, single_rows)
      merge_idx <- merge_idx + 1L
    }

    out[[md]] <- do.call(rbind, merged_rows)
  }

  do.call(rbind, unname(out))
}

# ---- Type A helpers ----

#' Merge rows with the same m/z (within ppm) — keep highest mean_area
#'
#' @noRd
metminer_resolve_same_mz_recurrent_rows <- function(rows, mz_ppm = 10) {
  rows <- metminer_ensure_filter_context_columns(rows)
  if (nrow(rows) <= 1) return(rows)

  n <- nrow(rows)
  mz <- rows$mz
  groups <- rep(NA_integer_, n)
  gid <- 1L
  for (i in seq_len(n)) {
    if (!is.na(groups[i]) || !is.finite(mz[i])) next
    ppm_window <- mz[i] * mz_ppm / 1e6
    hits <- which(is.na(groups) & is.finite(mz) & abs(mz - mz[i]) <= ppm_window)
    groups[hits] <- gid
    gid <- gid + 1L
  }

  for (g in unique(groups[is.finite(groups)])) {
    idx <- which(groups == g)
    if (length(idx) <= 1) next
    statuses <- rows$recurrent_status[idx]
    resolved_isf <- idx[statuses == "resolved_recurrent_isf"]
    parent_like <- idx[statuses %in% c("recurrent_parent_candidate", "none")]
    unresolved <- idx[statuses == "unresolved_recurrent_ion"]

    if (length(resolved_isf) > 0 && length(parent_like) > 0) {
      rows$keep[resolved_isf] <- FALSE
      rows$drop_reason[resolved_isf] <- "recurrent_same_mz_resolved_isf"
      rows$redundancy_reason[idx] <- paste(rows$redundancy_reason[idx], "recurrent_same_mz_detected", sep = ";")
      rows$redundancy_reason[resolved_isf] <- paste(rows$redundancy_reason[resolved_isf], "drop_resolved_isf", sep = ";")
    }

    if (length(resolved_isf) > 0 && length(parent_like) == 0 && length(unresolved) > 0) {
      rows$keep[resolved_isf] <- FALSE
      rows$drop_reason[resolved_isf] <- "recurrent_same_mz_resolved_isf_keep_unresolved_candidate"
      rows$redundancy_reason[idx] <- paste(rows$redundancy_reason[idx], "recurrent_same_mz_detected", sep = ";")
      rows$redundancy_reason[resolved_isf] <- paste(rows$redundancy_reason[resolved_isf], "drop_resolved_isf", sep = ";")
    }

    if (length(parent_like) == 0 && length(resolved_isf) == 0 && length(unresolved) > 1) {
      rows$redundancy_reason[idx] <- paste(rows$redundancy_reason[idx], "recurrent_same_mz_unresolved_review", sep = ";")
    }
  }

  rows
}

metminer_merge_same_mz_rows <- function(rows, mz_ppm = 10) {
  if (nrow(rows) <= 1) return(rows)

  n <- nrow(rows)
  mz <- rows$mz
  groups <- rep(NA_integer_, n)
  gid <- 1L

  for (i in seq_len(n)) {
    if (!is.na(groups[i])) next
    ppm_window <- mz[i] * mz_ppm / 1e6
    hits <- which(is.na(groups) &
                  is.finite(mz) &
                  abs(mz - mz[i]) <= ppm_window)
    groups[hits] <- gid
    gid <- gid + 1L
  }

  if (length(unique(groups)) == n) return(rows)

  merged <- lapply(split(seq_len(n), groups), function(idx) {
    if (length(idx) == 1) return(rows[idx, , drop = FALSE])
    best <- idx[which.max(rows$mean_area[idx])]
    merged_row <- rows[best, , drop = FALSE]
    all_features <- unique(unlist(
      strsplit(paste(rows$member_features[idx], collapse = ";"), ";", fixed = TRUE),
        use.names = FALSE))
    merged_row$member_features <- paste(all_features, collapse = ";")
    merged_row$member_adducts <- metminer_collapse_unique_text(rows$member_adducts[idx])
    merged_row$member_annotation_levels <- metminer_collapse_unique_text(rows$member_annotation_levels[idx])
    merged_row$n_features <- length(all_features)
    merged_row$record_type <- ifelse(
      merged_row$record_type == "sub_network", "sub_network",
      "merged_compound")
    merged_row$source_note <- paste0(merged_row$source_note,
      sprintf(" | same-m/z merged %d features", length(idx)))
    merged_row
  })

  do.call(rbind, merged)
}

# ---- Type B helpers ----

#' Merge rows that share a compound_key and whose mass difference matches a
#' known neutral-loss, adduct, or isotope pattern.
#'
#' @noRd
metminer_merge_mass_difference_rows <- function(rows, mz_ppm = 10) {
  if (nrow(rows) <= 1) return(rows)

  nl_table <- default_plant_neutral_loss_table()
  # Build within-mode adduct mass-offset list
  nl_masses <- nl_table$mass[nl_table$use_in_isf & is.finite(nl_table$mass)]
  nl_labels <- nl_table$annotation[nl_table$use_in_isf & is.finite(nl_table$mass)]

  # Sort by mean_area descending — parent candidates first
  rows <- rows[order(-rows$mean_area), , drop = FALSE]
  n <- nrow(rows)
  merged <- rep(FALSE, n)
  parent_of <- vector("list", n)

  for (i in seq_len(n)) {
    if (merged[i]) next
    for (j in seq_len(n)) {
      if (j <= i || merged[j]) next
      delta <- abs(rows$mz[i] - rows$mz[j])
      if (!is.finite(delta) || delta == 0) next

      rel <- classify_mass_difference(delta, rows$mz[i], rows$mz[j],
                                      nl_masses, nl_labels, mz_ppm)
      if (!is.null(rel)) {
        merged[j] <- TRUE
        parent_of[[i]] <- c(parent_of[[i]],
          sprintf("%s (Δm=%.4f,%s)", rows$metabolite_id[j], delta, rel))
      }
    }
  }

  if (!any(merged)) return(rows)

  out <- list()
  out_idx <- 1L
  for (i in seq_len(n)) {
    if (merged[i]) next
    row_i <- rows[i, , drop = FALSE]
    children <- parent_of[[i]]
    if (length(children) > 0 || any(merged[seq_len(n) != i])) {
      all_features <- unique(unlist(
        strsplit(paste(rows$member_features[c(i, which(merged))], collapse = ";"),
                 ";", fixed = TRUE),
        use.names = FALSE))
      row_i$member_features <- paste(all_features, collapse = ";")
      row_i$member_adducts <- metminer_collapse_unique_text(rows$member_adducts[c(i, which(merged))])
      row_i$member_annotation_levels <- metminer_collapse_unique_text(rows$member_annotation_levels[c(i, which(merged))])
      row_i$n_features <- length(all_features)
      row_i$record_type <- ifelse(
        row_i$record_type == "sub_network", "sub_network",
        "merged_compound")
      if (length(children) > 0) {
        row_i$source_note <- paste0(row_i$source_note,
          " | merged children: ", paste(children, collapse = "; "))
      }
    }
    out[[out_idx]] <- row_i
    out_idx <- out_idx + 1L
  }

  do.call(rbind, out)
}

# classify_mass_difference is defined in fct_annotation.R

#' Merge single-feature rows into existing network rows if mass difference
#' indicates an ISF/adduct/isotope relationship to the network parent.
#'
#' @noRd
metminer_merge_singles_into_networks <- function(net_rows, single_rows, mz_ppm) {
  nl_table <- default_plant_neutral_loss_table()
  nl_masses <- nl_table$mass[nl_table$use_in_isf & is.finite(nl_table$mass)]
  nl_labels <- nl_table$annotation[nl_table$use_in_isf & is.finite(nl_table$mass)]

  single_merged <- rep(FALSE, nrow(single_rows))

  for (i in seq_len(nrow(net_rows))) {
    for (j in seq_len(nrow(single_rows))) {
      if (single_merged[j]) next
      delta <- abs(net_rows$mz[i] - single_rows$mz[j])
      if (!is.finite(delta) || delta == 0) next

      rel <- classify_mass_difference(delta, net_rows$mz[i], single_rows$mz[j],
                                      nl_masses, nl_labels, mz_ppm)
      if (!is.null(rel)) {
        single_merged[j] <- TRUE
        all_features <- unique(unlist(
          strsplit(paste(c(net_rows$member_features[i],
                           single_rows$member_features[j]),
                         collapse = ";"), ";", fixed = TRUE),
          use.names = FALSE))
        net_rows$member_features[i] <- paste(all_features, collapse = ";")
        net_rows$member_adducts[i] <- metminer_collapse_unique_text(c(net_rows$member_adducts[i], single_rows$member_adducts[j]))
        net_rows$member_annotation_levels[i] <- metminer_collapse_unique_text(c(net_rows$member_annotation_levels[i], single_rows$member_annotation_levels[j]))
        net_rows$n_features[i] <- length(all_features)
        net_rows$source_note[i] <- paste0(net_rows$source_note[i],
          sprintf(" | +single %s (Δm=%.4f,%s)", single_rows$metabolite_id[j], delta, rel))
      }
    }
  }

  list(
    net_rows = net_rows,
    single_rows = single_rows[!single_merged, , drop = FALSE]
  )
}

# ---- Cross-polarity duplicate marking ----

#' Mark cross-polarity duplicate records, keeping the one with higher mean area
#'
#' Checks both directions: positive→negative and negative→positive, so every
#' compound-key / RT group is resolved consistently regardless of which
#' polarity happens to be iterated first.
#'
#' @noRd
metminer_mark_cross_polarity_duplicates <- function(candidates, rt_tolerance = 10) {
  candidates <- metminer_ensure_filter_context_columns(candidates)
  if (!all(c("positive", "negative") %in% candidates$mode)) return(candidates)

  # Build all cross-polarity duplicate groups
  pairs <- metminer_find_cross_polarity_pairs(candidates, rt_tolerance)
  if (nrow(pairs) == 0) return(candidates)

  # Within each group, keep the record with the highest mean area
  group_id <- 1L
  for (g in unique(pairs$group)) {
    members <- pairs$idx[pairs$group == g]
    best <- members[which.max(candidates$mean_area[members])]
    drop <- setdiff(members, best)
    gid <- paste0("XPM_", sprintf("%04d", group_id))
    group_id <- group_id + 1L

    candidates$cross_polarity_group[members] <- gid
    candidates$preferred_polarity[members] <- candidates$mode[best]
    candidates$keep[drop] <- FALSE
    candidates$drop_reason[drop] <- paste0("cross_polarity_duplicate_keep_", candidates$mode[best])
    candidates$redundancy_reason[members] <- paste(candidates$redundancy_reason[members], "cross_polarity_detected", sep = ";")
  }

  candidates
}

#' Find pairs of records from opposite polarities that likely represent the same metabolite
#'
#' @noRd
metminer_find_cross_polarity_pairs <- function(candidates, rt_tolerance) {
  pos_idx <- which(candidates$mode == "positive" & has_text(candidates$compound_key))
  neg_idx <- which(candidates$mode == "negative" & has_text(candidates$compound_key))

  if (length(pos_idx) == 0 || length(neg_idx) == 0) {
    return(data.frame(idx = integer(), group = integer()))
  }

  pairs <- list()
  group_counter <- 1L
  assigned <- rep(FALSE, nrow(candidates))

  # Collect all pos→neg matches by compound key + RT proximity
  for (p in pos_idx) {
    key <- as.character(candidates$compound_key[p])
    rt_p <- candidates$rt[p]
    if (!is.finite(rt_p)) next

    hits <- neg_idx[!assigned[neg_idx] &
                    has_text(candidates$compound_key[neg_idx]) &
                    as.character(candidates$compound_key[neg_idx]) == key &
                    is.finite(candidates$rt[neg_idx]) &
                    abs(candidates$rt[neg_idx] - rt_p) <= rt_tolerance]
    if (length(hits) == 0) next

    all_members <- c(p, hits)
    assigned[all_members] <- TRUE
    pairs[[length(pairs) + 1]] <- data.frame(
      idx = all_members,
      group = group_counter,
      stringsAsFactors = FALSE
    )
    group_counter <- group_counter + 1
  }

  if (length(pairs) == 0) {
    return(data.frame(idx = integer(), group = integer()))
  }
  do.call(rbind, pairs)
}
