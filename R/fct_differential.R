# ---- Differential abundance metabolite helpers ----

metminer_drop_qc_samples <- function(object) {
  if (is.null(object) || !inherits(object, "mass_dataset")) return(object)
  sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
  if (nrow(sample_info) == 0 || !"sample_id" %in% colnames(sample_info)) return(object)
  qc_cols <- intersect(c("class", "sample_type", "group"), colnames(sample_info))
  if (length(qc_cols) == 0) return(object)
  qc <- Reduce(`|`, lapply(qc_cols, function(col) tolower(as.character(sample_info[[col]])) == "qc"))
  if (!any(qc, na.rm = TRUE)) return(object)
  object |>
    massdataset::activate_mass_dataset(what = "sample_info") |>
    dplyr::filter(!(.data$sample_id %in% sample_info$sample_id[qc]))
}

metminer_split_feature_ids <- function(x) {
  x <- as.character(x %||% character())
  x <- unlist(strsplit(paste(x, collapse = ";"), "[;,|]", perl = TRUE), use.names = FALSE)
  x <- trimws(x)
  unique(x[has_text(x)])
}

metminer_clean_annotation_fill_ids <- function(x) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(x)
  if (!"Compound.name" %in% colnames(x) && "compound_name" %in% colnames(x)) x$Compound.name <- x$compound_name
  for (col in c("KEGG.ID", "PlantCyc.ID", "Lab.ID")) {
    if (!col %in% colnames(x)) x[[col]] <- NA_character_
  }
  if ("compound_key" %in% colnames(x)) {
    key <- as.character(x$compound_key)
    hit <- !has_text(x$KEGG.ID) & grepl("KEGG\\.ID:", key)
    x$KEGG.ID[hit] <- sub("^.*KEGG\\.ID:([^;|]+).*$", "\\1", key[hit])
    hit <- !has_text(x$PlantCyc.ID) & grepl("PlantCyc\\.ID:", key)
    x$PlantCyc.ID[hit] <- sub("^.*PlantCyc\\.ID:([^;|]+).*$", "\\1", key[hit])
    hit <- !has_text(x$Lab.ID) & grepl("Lab\\.ID:", key)
    x$Lab.ID[hit] <- sub("^.*Lab\\.ID:([^;|]+).*$", "\\1", key[hit])
  }
  x
}

metminer_choose_pseudo_compound <- function(row, pseudo_area) {
  if (is.null(pseudo_area) || !"feature_mapping" %in% names(pseudo_area)) return(NA_character_)
  fmap <- as.data.frame(pseudo_area$feature_mapping %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(fmap) == 0 || !all(c("compound_id", "variable_id") %in% colnames(fmap))) return(NA_character_)
  rep_feature <- as.character(row$representative_feature %||% NA_character_)
  members <- unique(c(rep_feature, metminer_split_feature_ids(row$member_features %||% "")))
  hit <- fmap[fmap$variable_id %in% members, , drop = FALSE]
  if (nrow(hit) == 0) return(NA_character_)
  rep_hit <- hit[hit$variable_id == rep_feature, , drop = FALSE]
  if (nrow(rep_hit) > 0) return(rep_hit$compound_id[1])
  tab <- sort(table(hit$compound_id), decreasing = TRUE)
  names(tab)[1]
}

metminer_clean_annotation_expr_row <- function(row, pseudo_area, object) {
  compound_id <- metminer_choose_pseudo_compound(row, pseudo_area)
  if (has_text(compound_id) &&
      "expression_data" %in% names(pseudo_area) &&
      compound_id %in% rownames(pseudo_area$expression_data)) {
    values <- as.numeric(pseudo_area$expression_data[compound_id, , drop = TRUE])
    names(values) <- colnames(pseudo_area$expression_data)
    return(list(values = values, source_compound_id = compound_id, source = "pseudo_area"))
  }
  expr <- as.data.frame(massdataset::extract_expression_data(object), stringsAsFactors = FALSE)
  rep_feature <- as.character(row$representative_feature %||% NA_character_)
  if (has_text(rep_feature) && rep_feature %in% rownames(expr)) {
    values <- as.numeric(expr[rep_feature, , drop = TRUE])
    names(values) <- colnames(expr)
    return(list(values = values, source_compound_id = NA_character_, source = "representative_feature"))
  }
  list(values = setNames(rep(NA_real_, ncol(expr)), colnames(expr)), source_compound_id = NA_character_, source = "missing")
}

metminer_build_clean_annotation_dataset <- function(annotation_filter_result,
                                                    positive_object = NULL,
                                                    negative_object = NULL,
                                                    pseudo_area_pos = NULL,
                                                    pseudo_area_neg = NULL) {
  final <- as.data.frame(annotation_filter_result$final_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(final) == 0) stop("No final non-redundant annotation table is available.", call. = FALSE)
  if (!"metabolite_id" %in% colnames(final)) stop("final_table must contain metabolite_id.", call. = FALSE)
  if (!"mode" %in% colnames(final)) final$mode <- NA_character_

  pos <- metminer_drop_qc_samples(positive_object)
  neg <- metminer_drop_qc_samples(negative_object)
  template <- pos %||% neg
  if (is.null(template)) stop("No mass_dataset object is available for clean annotation dataset.", call. = FALSE)

  rows <- vector("list", nrow(final))
  source_compound_id <- area_source <- rep(NA_character_, nrow(final))
  for (i in seq_len(nrow(final))) {
    mode <- tolower(as.character(final$mode[i]))
    if (mode %in% c("positive", "pos", "+")) {
      got <- metminer_clean_annotation_expr_row(final[i, , drop = FALSE], pseudo_area_pos, pos)
    } else if (mode %in% c("negative", "neg", "-")) {
      got <- metminer_clean_annotation_expr_row(final[i, , drop = FALSE], pseudo_area_neg, neg)
    } else {
      got <- metminer_clean_annotation_expr_row(final[i, , drop = FALSE], pseudo_area_pos, pos)
      if (identical(got$source, "missing")) got <- metminer_clean_annotation_expr_row(final[i, , drop = FALSE], pseudo_area_neg, neg)
    }
    rows[[i]] <- got$values
    source_compound_id[i] <- got$source_compound_id
    area_source[i] <- got$source
  }

  sample_info <- as.data.frame(massdataset::extract_sample_info(template), stringsAsFactors = FALSE)
  all_samples <- intersect(as.character(sample_info$sample_id), unique(unlist(lapply(rows, names), use.names = FALSE)))
  expr <- do.call(rbind, lapply(rows, function(x) {
    out <- setNames(rep(NA_real_, length(all_samples)), all_samples)
    hit <- intersect(names(x), all_samples)
    out[hit] <- x[hit]
    out
  }))
  expr <- as.data.frame(expr, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(expr) <- as.character(final$metabolite_id)

  variable_info <- metminer_clean_annotation_fill_ids(final)
  variable_info$variable_id <- as.character(final$metabolite_id)
  variable_info$source_compound_id <- source_compound_id
  variable_info$area_source <- area_source
  variable_info <- variable_info[, unique(c("variable_id", colnames(variable_info))), drop = FALSE]
  rownames(variable_info) <- variable_info$variable_id

  annotation_table <- metminer_clean_annotation_fill_ids(final)
  annotation_table$variable_id <- as.character(final$metabolite_id)
  if (!"ms2_files_id" %in% colnames(annotation_table)) annotation_table$ms2_files_id <- NA_character_
  if (!"ms2_spectrum_id" %in% colnames(annotation_table)) annotation_table$ms2_spectrum_id <- NA_character_
  annotation_table <- annotation_table[, unique(c("variable_id", colnames(annotation_table))), drop = FALSE]

  object <- template
  object@expression_data <- expr
  object@variable_info <- variable_info
  object@annotation_table <- annotation_table
  object@sample_info <- sample_info[match(all_samples, sample_info$sample_id), , drop = FALSE]
  object@variable_info_note <- data.frame(name = colnames(variable_info), meaning = colnames(variable_info), stringsAsFactors = FALSE)
  object@other_files$clean_annotation_dataset <- list(
    source = "annotation_filter_result$final_table + feature-network pseudo_area",
    n_metabolites = nrow(expr),
    n_samples = ncol(expr),
    area_source_summary = as.list(table(area_source, useNA = "ifany"))
  )
  object <- massdataset::update_sample_info(object)
  massdataset::update_variable_info(object)
}

metminer_analysis_object <- function(global_data, mode = c("merged", "positive", "negative")) {
  mode <- match.arg(mode)
  if (identical(mode, "merged")) {
    return(metminer_merge_polarity_analysis_objects(
      positive_object = metminer_analysis_object(global_data, "positive"),
      negative_object = metminer_analysis_object(global_data, "negative")
    ))
  }
  slots <- if (identical(mode, "positive")) {
    c("object_pos_norm", "object_pos_annotated", "object_pos_clean", "object_pos_raw")
  } else {
    c("object_neg_norm", "object_neg_annotated", "object_neg_clean", "object_neg_raw")
  }
  for (slot in slots) {
    obj <- global_data[[slot]]
    if (!is.null(obj)) return(obj)
  }
  NULL
}

metminer_prefix_massdataset_variables <- function(object, prefix) {
  if (is.null(object)) return(NULL)
  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a mass_dataset object.", call. = FALSE)
  }
  prefix <- as.character(prefix)
  expr <- as.data.frame(massdataset::extract_expression_data(object), stringsAsFactors = FALSE)
  old_ids <- rownames(expr)
  if (is.null(old_ids) || any(!has_text(old_ids))) {
    old_ids <- as.character(massdataset::extract_variable_info(object)$variable_id)
  }
  new_ids <- paste(prefix, old_ids, sep = "::")
  rownames(expr) <- new_ids
  object@expression_data <- expr

  variable_info <- as.data.frame(massdataset::extract_variable_info(object), stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(variable_info)) {
    variable_info$variable_id <- old_ids
  }
  variable_info$source_variable_id <- as.character(variable_info$variable_id)
  variable_info$variable_id <- paste(prefix, variable_info$source_variable_id, sep = "::")
  variable_info$polarity <- if (identical(prefix, "pos")) "positive" else if (identical(prefix, "neg")) "negative" else prefix
  object@variable_info <- variable_info

  ann <- as.data.frame(object@annotation_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(ann) > 0) {
    for (col in intersect(c("variable_id", "Variable_id"), colnames(ann))) {
      ann[[paste0("source_", col)]] <- as.character(ann[[col]])
      ann[[col]] <- paste(prefix, ann[[paste0("source_", col)]], sep = "::")
    }
    object@annotation_table <- ann
  }
  massdataset::update_variable_info(object)
}

metminer_merge_polarity_analysis_objects <- function(positive_object = NULL, negative_object = NULL) {
  objs <- list()
  if (!is.null(positive_object)) {
    objs$positive <- metminer_prefix_massdataset_variables(positive_object, "pos")
  }
  if (!is.null(negative_object)) {
    objs$negative <- metminer_prefix_massdataset_variables(negative_object, "neg")
  }
  objs <- objs[!vapply(objs, is.null, logical(1))]
  if (length(objs) == 0) return(NULL)
  if (length(objs) == 1) return(objs[[1]])
  massdataset::rbind_mass_dataset(objs[[1]], objs[[2]])
}

metminer_sample_group_columns <- function(object) {
  if (is.null(object)) return(character())
  sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
  if (nrow(sample_info) == 0) return(character())
  cols <- setdiff(colnames(sample_info), c("sample_id", "raw_file_name", "injection.order", "class"))
  cols[vapply(sample_info[cols], function(x) length(unique(x[has_text(x)])) >= 2, logical(1))]
}

metminer_group_sample_ids <- function(object, group_column, group_value) {
  sample_info <- massdataset::extract_sample_info(object)
  if (!all(c("sample_id", group_column) %in% colnames(sample_info))) return(character())
  ids <- sample_info$sample_id[as.character(sample_info[[group_column]]) == as.character(group_value)]
  ids <- intersect(as.character(ids), colnames(massdataset::extract_expression_data(object)))
  ids[has_text(ids)]
}

metminer_group_values <- function(object, group_column) {
  if (is.null(object) || !has_text(group_column)) return(character())
  sample_info <- massdataset::extract_sample_info(object)
  if (!group_column %in% colnames(sample_info)) return(character())
  groups <- as.character(sample_info[[group_column]])
  sort(unique(groups[has_text(groups)]))
}

metminer_comparison_choices <- function(groups) {
  groups <- sort(unique(as.character(groups[has_text(groups)])))
  if (length(groups) < 2) return(stats::setNames(character(), character()))
  pairs <- utils::combn(groups, 2, simplify = FALSE)
  values <- unlist(lapply(pairs, function(x) {
    c(
      paste(x[2], x[1], sep = "\r"),
      paste(x[1], x[2], sep = "\r")
    )
  }), use.names = FALSE)
  labels <- vapply(strsplit(values, "\r", fixed = TRUE), function(x) paste0(x[1], " vs ", x[2]), character(1))
  stats::setNames(values, labels)
}

metminer_parse_comparison <- function(comparison_pair) {
  x <- strsplit(as.character(comparison_pair %||% ""), "\r", fixed = TRUE)[[1]]
  if (length(x) != 2 || !all(has_text(x))) {
    return(list(case_group = NA_character_, control_group = NA_character_))
  }
  list(case_group = x[1], control_group = x[2])
}

metminer_standardize_annotation_columns <- function(x) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(x)
  canonical <- c(
    variable_id = "Variable_id",
    variable.id = "Variable_id",
    variableid = "Variable_id",
    compound.name = "Compound.name",
    compound_name = "Compound.name",
    kegg.id = "KEGG.ID",
    kegg_id = "KEGG.ID",
    plantcyc.id = "PlantCyc.ID",
    plantcyc_id = "PlantCyc.ID",
    lab.id = "Lab.ID",
    lab_id = "Lab.ID",
    adduct = "Adduct",
    database = "Database",
    level = "Level",
    sub_net_id = "Sub_net_id",
    mode = "mode"
  )
  keys <- tolower(colnames(x))
  keys <- gsub("[^a-z0-9]+", ".", keys)
  keys <- gsub("^\\.|\\.$", "", keys)
  for (i in seq_along(keys)) {
    if (keys[i] %in% names(canonical)) {
      colnames(x)[i] <- canonical[[keys[i]]]
    }
  }
  x
}

metminer_annotation_polarity_prefix <- function(x) {
  x <- tolower(trimws(as.character(x %||% NA_character_)))
  out <- rep(NA_character_, length(x))
  out[x %in% c("positive", "pos", "+")] <- "pos"
  out[x %in% c("negative", "neg", "-")] <- "neg"
  out
}

metminer_infer_polarity_prefix <- function(variable_id) {
  variable_id <- as.character(variable_id %||% NA_character_)
  out <- rep(NA_character_, length(variable_id))
  out[grepl("(^pos::|_pos$)", variable_id, ignore.case = TRUE)] <- "pos"
  out[grepl("(^neg::|_neg$)", variable_id, ignore.case = TRUE)] <- "neg"
  out
}

metminer_dam_readable_annotation <- function(annotation_filter_result, mode) {
  if (is.null(annotation_filter_result)) return(data.frame(variable_id = character()))
  x <- annotation_filter_result$collapse_table %||% annotation_filter_result$expand_table %||% data.frame()
  x <- metminer_standardize_annotation_columns(x)
  if (nrow(x) == 0 || !"Variable_id" %in% colnames(x)) return(data.frame(variable_id = character()))
  if ("mode" %in% colnames(x) && !identical(mode, "merged")) {
    mode_prefix <- metminer_annotation_polarity_prefix(mode)
    row_prefix <- metminer_annotation_polarity_prefix(x$mode)
    x <- x[row_prefix == mode_prefix | is.na(row_prefix), , drop = FALSE]
  }
  x$variable_id <- as.character(x$Variable_id)
  if (identical(mode, "merged")) {
    prefix <- if ("mode" %in% colnames(x)) metminer_annotation_polarity_prefix(x$mode) else rep(NA_character_, nrow(x))
    inferred <- metminer_infer_polarity_prefix(x$variable_id)
    prefix[!has_text(prefix) & has_text(inferred)] <- inferred[!has_text(prefix) & has_text(inferred)]
    has_prefix <- grepl("^pos::|^neg::", x$variable_id)
    use_prefix <- has_text(prefix) & !has_prefix
    x$variable_id[use_prefix] <- paste(prefix[use_prefix], x$variable_id[use_prefix], sep = "::")
  }
  keep <- intersect(
    c("variable_id", "Compound.name", "KEGG.ID", "PlantCyc.ID", "Lab.ID", "Adduct", "Database", "Level", "Sub_net_id", "mode"),
    colnames(x)
  )
  x <- x[, keep, drop = FALSE]
  x <- x[!duplicated(x$variable_id), , drop = FALSE]
  rownames(x) <- NULL
  x
}

metminer_update_variable_info_columns <- function(object, values) {
  if (is.null(object) || !inherits(object, "mass_dataset")) {
    stop("Input object must be a mass_dataset object.", call. = FALSE)
  }
  values <- as.data.frame(values %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(values) == 0 || !"variable_id" %in% colnames(values)) return(object)
  variable_info <- as.data.frame(metminer_safe_extract_variable_info(object), stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(variable_info)) {
    variable_info$variable_id <- rownames(variable_info)
  }
  values$variable_id <- as.character(values$variable_id)
  variable_info$variable_id <- as.character(variable_info$variable_id)
  values <- values[!duplicated(values$variable_id), , drop = FALSE]
  idx <- match(variable_info$variable_id, values$variable_id)
  value_cols <- setdiff(colnames(values), "variable_id")
  for (col in value_cols) {
    old <- variable_info[[col]]
    new <- values[[col]]
    if (is.null(old)) {
      fill <- if (is.numeric(new)) NA_real_ else if (is.integer(new)) NA_integer_ else NA_character_
      variable_info[[col]] <- rep(fill, nrow(variable_info))
    }
    hit <- !is.na(idx)
    variable_info[[col]][hit] <- new[idx[hit]]
  }
  object@variable_info <- variable_info
  object@variable_info_note <- data.frame(
    name = colnames(variable_info),
    meaning = colnames(variable_info),
    stringsAsFactors = FALSE
  )
  massdataset::update_variable_info(object)
}

metminer_build_dam_statistics <- function(object,
                                          group_column = "group",
                                          control_group,
                                          case_group,
                                          mode = c("merged", "positive", "negative"),
                                          annotation_filter_result = NULL,
                                          mean_median = c("mean", "median"),
                                          test_method = c("t.test", "wilcox.test"),
                                          p_adjust_method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),
                                          fc_cutoff = 1.5,
                                          p_cutoff = 0.05,
                                          use_fdr = TRUE) {
  mode <- match.arg(mode)
  mean_median <- match.arg(mean_median)
  test_method <- match.arg(test_method)
  p_adjust_method <- match.arg(p_adjust_method)
  if (is.null(object)) stop("No mass_dataset object is available.", call. = FALSE)

  control_ids <- metminer_group_sample_ids(object, group_column, control_group)
  case_ids <- metminer_group_sample_ids(object, group_column, case_group)
  if (length(control_ids) < 3 || length(case_ids) < 3) {
    stop("Each comparison group requires at least 3 biological replicates for mutate_fc/mutate_p_value.", call. = FALSE)
  }

  fc <- massstat::mutate_fc(
    object = object,
    control_sample_id = control_ids,
    case_sample_id = case_ids,
    mean_median = mean_median,
    return_mass_dataset = FALSE
  )
  ptab <- massstat::mutate_p_value(
    object = object,
    control_sample_id = control_ids,
    case_sample_id = case_ids,
    method = test_method,
    p_adjust_methods = p_adjust_method,
    return_mass_dataset = FALSE
  )

  variable_info <- metminer_safe_extract_variable_info(object)
  variable_info <- as.data.frame(variable_info, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(variable_info)) {
    variable_info$variable_id <- names(fc)
  }
  out <- merge(
    variable_info,
    data.frame(variable_id = names(fc), fc = as.numeric(fc), stringsAsFactors = FALSE),
    by = "variable_id",
    all.y = TRUE,
    sort = FALSE
  )
  out <- merge(out, ptab, by = "variable_id", all.x = TRUE, sort = FALSE)
  ann <- metminer_dam_readable_annotation(annotation_filter_result, mode)
  if (nrow(ann) > 0) {
    out <- merge(out, ann, by = "variable_id", all.x = TRUE, sort = FALSE)
  }

  out$mode <- mode
  out$control_group <- control_group
  out$case_group <- case_group
  out$control_n <- length(control_ids)
  out$case_n <- length(case_ids)
  out$log2_fc <- log2(out$fc)
  out$neg_log10_p <- -log10(pmax(out$p_value, .Machine$double.xmin))
  out$neg_log10_fdr <- -log10(pmax(out$p_value_adjust, .Machine$double.xmin))
  p_col <- if (isTRUE(use_fdr)) "p_value_adjust" else "p_value"
  sig <- is.finite(out$fc) & out[[p_col]] < p_cutoff & abs(out$log2_fc) >= log2(fc_cutoff)
  out$change <- "Not significant"
  out$change[sig & out$log2_fc > 0] <- "Up"
  out$change[sig & out$log2_fc < 0] <- "Down"
  out <- out[order(out[[p_col]], -abs(out$log2_fc), na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL

  list(
    table = out,
    control_ids = control_ids,
    case_ids = case_ids,
    p_column = p_col,
    parameters = list(
      group_column = group_column,
      control_group = control_group,
      case_group = case_group,
      mode = mode,
      mean_median = mean_median,
      test_method = test_method,
      p_adjust_method = p_adjust_method,
      fc_cutoff = fc_cutoff,
      p_cutoff = p_cutoff,
      use_fdr = use_fdr
    )
  )
}

metminer_mutate_dam <- function(object,
                                group_column = "group",
                                control_group,
                                case_group,
                                mode = c("merged", "positive", "negative"),
                                annotation_filter_result = NULL,
                                mean_median = c("mean", "median"),
                                test_method = c("t.test", "wilcox.test"),
                                p_adjust_method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),
                                fc_cutoff = 1.5,
                                p_cutoff = 0.05,
                                use_fdr = TRUE) {
  stats <- metminer_build_dam_statistics(
    object = object,
    group_column = group_column,
    control_group = control_group,
    case_group = case_group,
    mode = mode,
    annotation_filter_result = annotation_filter_result,
    mean_median = mean_median,
    test_method = test_method,
    p_adjust_method = p_adjust_method,
    fc_cutoff = fc_cutoff,
    p_cutoff = p_cutoff,
    use_fdr = use_fdr
  )
  mutate_cols <- intersect(
    c("variable_id", "fc", "log2_fc", "p_value", "p_value_adjust", "neg_log10_p", "neg_log10_fdr",
      "change", "mode", "control_group", "case_group", "control_n", "case_n"),
    colnames(stats$table)
  )
  object <- metminer_update_variable_info_columns(object, stats$table[, mutate_cols, drop = FALSE])
  attr(object, "metminer_dam") <- stats
  object
}

metminer_extract_dam_table <- function(object, annotation_filter_result = NULL, mode = c("merged", "positive", "negative"),
                                       p_column = NULL) {
  mode <- match.arg(mode)
  stats <- attr(object, "metminer_dam", exact = TRUE)
  if (!is.null(stats$table)) {
    return(stats$table)
  }
  x <- as.data.frame(metminer_safe_extract_variable_info(object), stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(x)) x$variable_id <- rownames(x)
  ann <- metminer_dam_readable_annotation(annotation_filter_result, mode)
  if (nrow(ann) > 0) {
    drop_cols <- intersect(setdiff(colnames(ann), "variable_id"), colnames(x))
    x <- x[, setdiff(colnames(x), drop_cols), drop = FALSE]
    x <- merge(x, ann, by = "variable_id", all.x = TRUE, sort = FALSE)
  }
  p_column <- p_column %||% if ("p_value_adjust" %in% colnames(x)) "p_value_adjust" else "p_value"
  if (p_column %in% colnames(x) && "log2_fc" %in% colnames(x)) {
    x <- x[order(x[[p_column]], -abs(x$log2_fc), na.last = TRUE), , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

metminer_run_dam <- function(object,
                             group_column = "group",
                             control_group,
                             case_group,
                             mode = c("merged", "positive", "negative"),
                             annotation_filter_result = NULL,
                             mean_median = c("mean", "median"),
                             test_method = c("t.test", "wilcox.test"),
                             p_adjust_method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),
                             fc_cutoff = 1.5,
                             p_cutoff = 0.05,
                             use_fdr = TRUE) {
  object <- metminer_mutate_dam(
    object = object,
    group_column = group_column,
    control_group = control_group,
    case_group = case_group,
    mode = mode,
    annotation_filter_result = annotation_filter_result,
    mean_median = mean_median,
    test_method = test_method,
    p_adjust_method = p_adjust_method,
    fc_cutoff = fc_cutoff,
    p_cutoff = p_cutoff,
    use_fdr = use_fdr
  )
  stats <- attr(object, "metminer_dam", exact = TRUE)
  list(
    object = object,
    result = stats$table,
    control_ids = stats$control_ids,
    case_ids = stats$case_ids,
    p_column = stats$p_column,
    parameters = stats$parameters
  )
}

metminer_has_annotation <- function(x) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(logical())
  ann_cols <- intersect(c("Compound.name", "KEGG.ID", "PlantCyc.ID", "Lab.ID"), colnames(x))
  if (length(ann_cols) == 0) return(rep(FALSE, nrow(x)))
  Reduce(`|`, lapply(ann_cols, function(col) has_text(x[[col]])))
}

metminer_plot_volcano <- function(dam_result, fc_cutoff = 1.5, p_cutoff = 0.05,
                                  p_column = "p_value_adjust", interactive = TRUE,
                                  annotated_only = FALSE) {
  x <- as.data.frame(dam_result %||% data.frame(), stringsAsFactors = FALSE)
  if (isTRUE(annotated_only) && nrow(x) > 0) {
    x <- x[metminer_has_annotation(x), , drop = FALSE]
  }
  if (nrow(x) == 0) {
    if (interactive) return(plotly::plot_ly())
    return(ggplot2::ggplot())
  }
  y_col <- if (identical(p_column, "p_value")) "neg_log10_p" else "neg_log10_fdr"
  label <- if ("Compound.name" %in% colnames(x)) ifelse(has_text(x$Compound.name), x$Compound.name, x$variable_id) else x$variable_id
  x$hover_text <- paste0(
    "Metabolite/feature: ", x$variable_id,
    "<br>Compound: ", label,
    if ("mz" %in% colnames(x)) paste0("<br>m/z: ", signif(suppressWarnings(as.numeric(x$mz)), 6)) else "",
    if ("rt" %in% colnames(x)) paste0("<br>RT: ", signif(suppressWarnings(as.numeric(x$rt)), 5)) else "",
    if ("KEGG.ID" %in% colnames(x)) paste0("<br>KEGG: ", x$KEGG.ID) else "",
    if ("PlantCyc.ID" %in% colnames(x)) paste0("<br>PlantCyc: ", x$PlantCyc.ID) else "",
    if ("Lab.ID" %in% colnames(x)) paste0("<br>Lab ID: ", x$Lab.ID) else "",
    if ("Adduct" %in% colnames(x)) paste0("<br>Adduct: ", x$Adduct) else "",
    "<br>log2FC: ", round(x$log2_fc, 3),
    "<br>p: ", signif(x$p_value, 3),
    "<br>FDR: ", signif(x$p_value_adjust, 3),
    "<br>Change: ", x$change
  )

  p <- ggplot2::ggplot(x, ggplot2::aes(x = log2_fc, y = .data[[y_col]], color = change, key = variable_id, text = hover_text)) +
    ggplot2::geom_point(alpha = 0.75, size = 1.8) +
    ggplot2::geom_vline(xintercept = c(-log2(fc_cutoff), log2(fc_cutoff)), linetype = "dashed", color = "grey55") +
    ggplot2::geom_hline(yintercept = -log10(p_cutoff), linetype = "dashed", color = "grey55") +
    ggplot2::scale_color_manual(values = c("Up" = "#c0392b", "Down" = "#2f6fbb", "Not significant" = "#90979f")) +
    ggplot2::labs(x = "log2 fold change", y = if (identical(p_column, "p_value")) "-log10(p)" else "-log10(FDR)", color = NULL) +
    ggplot2::theme_minimal(base_size = 12)

  if (!interactive) return(p)
  plt <- plotly::ggplotly(p, tooltip = "text", source = "dam_volcano") |>
    plotly::layout(legend = list(orientation = "h", x = 0, y = -0.18))
  plotly::event_register(plt, "plotly_click")
}

metminer_feature_boxplot_data <- function(object, feature_id, group_column, control_group, case_group) {
  expr <- as.data.frame(metminer_safe_extract_expression_data(object), stringsAsFactors = FALSE)
  sample_info <- massdataset::extract_sample_info(object)
  if (!feature_id %in% rownames(expr)) return(data.frame())
  ids <- c(
    metminer_group_sample_ids(object, group_column, control_group),
    metminer_group_sample_ids(object, group_column, case_group)
  )
  ids <- intersect(ids, colnames(expr))
  data.frame(
    sample_id = ids,
    intensity = as.numeric(expr[feature_id, ids]),
    group = as.character(sample_info[[group_column]][match(ids, sample_info$sample_id)]),
    stringsAsFactors = FALSE
  )
}

metminer_plot_feature_boxplot <- function(object, feature_id, group_column, control_group, case_group,
                                          p_value = NA_real_, p_adjust = NA_real_) {
  dat <- metminer_feature_boxplot_data(object, feature_id, group_column, control_group, case_group)
  validate(need(nrow(dat) > 0, "No expression values available for selected feature."))
  title <- paste0("p=", signif(p_value, 3), "; FDR=", signif(p_adjust, 3))
  ggplot2::ggplot(dat, ggplot2::aes(x = group, y = intensity, fill = group)) +
    ggplot2::geom_boxplot(alpha = 0.65, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.12, size = 2, alpha = 0.8) +
    ggplot2::labs(x = NULL, y = "Intensity", title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

metminer_opls_ready <- function(object, group_column, control_group, case_group, min_replicates = 3) {
  if (is.null(object)) return(list(ready = FALSE, message = "No mass_dataset object is available."))
  control_ids <- metminer_group_sample_ids(object, group_column, control_group)
  case_ids <- metminer_group_sample_ids(object, group_column, case_group)
  if (length(control_ids) < min_replicates || length(case_ids) < min_replicates) {
    return(list(
      ready = FALSE,
      message = paste0("Biological replicates are insufficient for OPLS-DA cross-validation: ",
                       control_group, " n=", length(control_ids), ", ",
                       case_group, " n=", length(case_ids), ".")
    ))
  }
  list(ready = TRUE, message = paste0("OPLS-DA available: ", control_group, " n=", length(control_ids), ", ", case_group, " n=", length(case_ids), "."))
}

metminer_scale_opls_matrix <- function(x, scale = c("pareto", "standard", "center", "none"), center = NULL, divider = NULL) {
  scale <- match.arg(scale)
  x <- as.matrix(x)
  if (is.null(center)) {
    center <- if (identical(scale, "none")) rep(0, ncol(x)) else colMeans(x, na.rm = TRUE)
  }
  if (is.null(divider)) {
    sdv <- apply(x, 2, stats::sd, na.rm = TRUE)
    divider <- switch(
      scale,
      standard = sdv,
      pareto = sqrt(sdv),
      center = rep(1, ncol(x)),
      none = rep(1, ncol(x))
    )
    divider[!is.finite(divider) | divider == 0] <- 1
  }
  x_scaled <- sweep(x, 2, center, "-")
  x_scaled <- sweep(x_scaled, 2, divider, "/")
  list(x = x_scaled, center = center, divider = divider, scale = scale)
}

metminer_fit_opls_core <- function(x, y_numeric) {
  x <- as.matrix(x)
  y_numeric <- as.numeric(y_numeric)
  y_mean <- mean(y_numeric)
  y_centered <- y_numeric - y_mean
  total_x_ss <- sum(x^2)
  total_y_ss <- sum(y_centered^2)
  w <- drop(crossprod(x, y_centered))
  w_norm <- sqrt(sum(w^2))
  if (!is.finite(w_norm) || w_norm == 0) {
    stop("Cannot estimate a predictive component from the selected groups.", call. = FALSE)
  }
  w <- w / w_norm
  t_pred <- drop(x %*% w)
  t_denom <- sum(t_pred^2)
  if (!is.finite(t_denom) || t_denom == 0) {
    stop("Predictive component has zero variance.", call. = FALSE)
  }
  p_pred <- drop(crossprod(x, t_pred)) / t_denom
  c_pred <- sum(y_centered * t_pred) / t_denom
  x_pred <- tcrossprod(t_pred, p_pred)
  y_fit <- t_pred * c_pred + y_mean
  residual_x <- x - x_pred

  if (min(dim(residual_x)) >= 1 && sum(residual_x^2) > sqrt(.Machine$double.eps)) {
    sv <- svd(residual_x, nu = 1, nv = 1)
    t_orth <- sv$u[, 1] * sv$d[1]
    p_orth <- sv$v[, 1]
    x_orth <- tcrossprod(t_orth, p_orth)
  } else {
    t_orth <- rep(0, nrow(x))
    p_orth <- rep(0, ncol(x))
    x_orth <- matrix(0, nrow(x), ncol(x))
  }

  r2x_pred <- if (total_x_ss > 0) sum(x_pred^2) / total_x_ss else NA_real_
  r2x_orth <- if (total_x_ss > 0) sum(x_orth^2) / total_x_ss else NA_real_
  r2y <- if (total_y_ss > 0) 1 - sum((y_numeric - y_fit)^2) / total_y_ss else NA_real_
  vip_like <- abs(w) / sqrt(sum(w^2)) * sqrt(length(w))

  list(
    weights = w,
    predictive_loading = p_pred,
    orthogonal_loading = p_orth,
    predictive_score = t_pred,
    orthogonal_score = t_orth,
    y_mean = y_mean,
    y_coefficient = c_pred,
    fitted_y = y_fit,
    r2x_pred = r2x_pred,
    r2x_orth = r2x_orth,
    r2y = r2y,
    vip_like = vip_like
  )
}

metminer_opls_loocv <- function(x, y_numeric, scale) {
  n <- nrow(x)
  pred <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    train <- setdiff(seq_len(n), i)
    scaled_train <- metminer_scale_opls_matrix(x[train, , drop = FALSE], scale = scale)
    scaled_test <- metminer_scale_opls_matrix(
      x[i, , drop = FALSE],
      scale = scale,
      center = scaled_train$center,
      divider = scaled_train$divider
    )
    fit <- tryCatch(metminer_fit_opls_core(scaled_train$x, y_numeric[train]), error = function(e) NULL)
    if (!is.null(fit)) {
      pred[i] <- drop(scaled_test$x %*% fit$weights) * fit$y_coefficient + fit$y_mean
    }
  }
  total <- sum((y_numeric - mean(y_numeric))^2)
  press <- sum((y_numeric - pred)^2, na.rm = TRUE)
  q2 <- if (total > 0 && all(is.finite(pred))) 1 - press / total else NA_real_
  list(predicted_y = pred, q2 = q2)
}

metminer_run_opls_da <- function(object, group_column, control_group, case_group,
                                 scale = c("pareto", "standard", "center", "none")) {
  scale <- match.arg(scale)
  ready <- metminer_opls_ready(object, group_column, control_group, case_group)
  if (!isTRUE(ready$ready)) {
    stop(ready$message, call. = FALSE)
  }

  control_ids <- metminer_group_sample_ids(object, group_column, control_group)
  case_ids <- metminer_group_sample_ids(object, group_column, case_group)
  ids <- c(control_ids, case_ids)
  expr <- as.matrix(metminer_safe_extract_expression_data(object))
  expr <- expr[, ids, drop = FALSE]
  x <- t(expr)
  x[!is.finite(x)] <- NA_real_
  keep <- colSums(is.na(x)) == 0 & apply(x, 2, stats::sd, na.rm = TRUE) > 0
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 2) {
    stop("Not enough non-missing variable features for OPLS-DA.", call. = FALSE)
  }
  group <- factor(c(rep(control_group, length(control_ids)), rep(case_group, length(case_ids))),
                  levels = c(control_group, case_group))
  y_numeric <- ifelse(group == case_group, 1, -1)
  scaled <- metminer_scale_opls_matrix(x, scale = scale)
  fit <- metminer_fit_opls_core(scaled$x, y_numeric)
  cv <- metminer_opls_loocv(x, y_numeric, scale = scale)
  pred_group <- ifelse(fit$fitted_y >= 0, case_group, control_group)
  accuracy <- mean(pred_group == as.character(group))

  score <- data.frame(
    sample_id = ids,
    group = as.character(group),
    predictive_score = fit$predictive_score,
    ortho_score = fit$orthogonal_score,
    fitted_y = fit$fitted_y,
    cv_predicted_y = cv$predicted_y,
    predicted_group = pred_group,
    stringsAsFactors = FALSE
  )
  vip <- data.frame(
    variable_id = colnames(x),
    vip_like = fit$vip_like,
    loading_predictive = fit$predictive_loading,
    loading_orthogonal = fit$orthogonal_loading,
    stringsAsFactors = FALSE
  )
  vip <- vip[order(vip$vip_like, decreasing = TRUE), , drop = FALSE]
  rownames(vip) <- NULL
  summary <- data.frame(
    Metric = c("Samples", "Variables", "Predictive components", "Orthogonal components", "Scaling", "R2X predictive", "R2X orthogonal", "R2Y", "Q2 leave-one-out", "Training accuracy"),
    Value = c(
      nrow(x),
      ncol(x),
      1,
      1,
      scale,
      signif(fit$r2x_pred, 4),
      signif(fit$r2x_orth, 4),
      signif(fit$r2y, 4),
      signif(cv$q2, 4),
      signif(accuracy, 4)
    ),
    stringsAsFactors = FALSE
  )
  list(
    model = fit,
    score = score,
    summary = summary,
    vip = vip,
    ready = ready,
    scale = scaled
  )
}

metminer_mutate_oplsda <- function(object, group_column, control_group, case_group,
                                   scale = c("pareto", "standard", "center", "none")) {
  opls <- metminer_run_opls_da(
    object = object,
    group_column = group_column,
    control_group = control_group,
    case_group = case_group,
    scale = scale
  )
  vip <- opls$vip
  colnames(vip)[colnames(vip) == "vip_like"] <- "opls_vip_like"
  colnames(vip)[colnames(vip) == "loading_predictive"] <- "opls_loading_predictive"
  colnames(vip)[colnames(vip) == "loading_orthogonal"] <- "opls_loading_orthogonal"
  dam <- attr(object, "metminer_dam", exact = TRUE)
  object <- metminer_update_variable_info_columns(object, vip)
  if (!is.null(dam$table) && "variable_id" %in% colnames(dam$table)) {
    drop_cols <- intersect(setdiff(colnames(vip), "variable_id"), colnames(dam$table))
    dam$table <- dam$table[, setdiff(colnames(dam$table), drop_cols), drop = FALSE]
    dam$table <- merge(dam$table, vip, by = "variable_id", all.x = TRUE, sort = FALSE)
    attr(object, "metminer_dam") <- dam
  }
  attr(object, "metminer_oplsda") <- opls
  object
}

metminer_extract_oplsda_result <- function(object) {
  attr(object, "metminer_oplsda", exact = TRUE) %||% NULL
}

metminer_plot_opls_score <- function(opls_result) {
  validate(need(!is.null(opls_result) && nrow(opls_result$score) > 0, "No OPLS-DA model result available."))
  plotly::plot_ly(
    opls_result$score,
    x = ~predictive_score,
    y = ~ortho_score,
    color = ~group,
    type = "scatter",
    mode = "markers",
    text = ~paste0("Sample: ", sample_id, "<br>Group: ", group),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = "Lightweight OPLS-DA score plot",
      xaxis = list(title = "Predictive score (t[1])"),
      yaxis = list(title = "Orthogonal score (to[1])"),
      legend = list(orientation = "h", x = 0, y = -0.2)
    )
}
