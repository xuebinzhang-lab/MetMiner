# ---- Pathway enrichment helpers ----

metminer_load_pathway_database <- function(path) {
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    stop("Pathway database file does not exist.", call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  objs <- load(path, envir = env)
  for (nm in objs) {
    obj <- get(nm, envir = env)
    if (methods::is(obj, "pathway_database")) {
      return(obj)
    }
  }
  stop("No metpath pathway_database object found in the RDA file.", call. = FALSE)
}

metminer_extract_pathway_compound_ids <- function(pathway_database, id_column = "KEGG.ID", allow_first_column_fallback = TRUE) {
  if (!methods::is(pathway_database, "pathway_database")) {
    stop("pathway_database must be a metpath pathway_database object.", call. = FALSE)
  }

  compound_list <- pathway_database@compound_list
  ids <- lapply(compound_list, function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (nrow(x) == 0) return(character())
    if (!id_column %in% colnames(x)) {
      if (!isTRUE(allow_first_column_fallback)) {
        stop("The compound list should contain ", id_column, ".", call. = FALSE)
      }
      # Some older/custom PlantCyc pathway databases use the first column as the
      # compound identifier. This fallback keeps them usable while preserving
      # explicit ID columns when available.
      id_col <- colnames(x)[1]
    } else {
      id_col <- id_column
    }
    out <- trimws(as.character(x[[id_col]]))
    unique(out[has_text(out)])
  })
  names(ids) <- pathway_database@pathway_id
  ids
}

metminer_pathway_info_table <- function(pathway_database) {
  description <- vapply(pathway_database@describtion, function(x) {
    if (is.null(x) || length(x) == 0) "" else paste(x, collapse = " // ")
  }, character(1))
  pathway_class <- vapply(pathway_database@pathway_class, function(x) {
    if (is.null(x) || length(x) == 0) "" else paste(x, collapse = " // ")
  }, character(1))
  data.frame(
    pathway_id = as.character(pathway_database@pathway_id),
    pathway_name = as.character(pathway_database@pathway_name),
    describtion = description,
    pathway_class = pathway_class,
    stringsAsFactors = FALSE
  )
}

metminer_make_enrich_result <- function(pathway_database, result, id_column,
                                        method, p_adjust_method, query_id) {
  if (requireNamespace("metpath", quietly = TRUE)) {
    parameter <- tryCatch(
      methods::new(
        Class = "tidymass_parameter",
        pacakge_name = "MetMiner",
        function_name = "metminer_enrich_pathway()",
        parameter = list(
          query_id = query_id,
          id_column = id_column,
          pathway_database = paste(pathway_database@database_info$source %||% "custom",
                                   pathway_database@database_info$version %||% "",
                                   sep = ","),
          p_adjust_method = p_adjust_method,
          method = method
        ),
        time = Sys.time()
      ),
      error = function(e) NULL
    )
    if (!is.null(parameter)) {
      return(methods::new(
        Class = "enrich_result",
        pathway_database = as.character(pathway_database@database_info$source %||% "custom"),
        pathway_version = as.character(pathway_database@database_info$version %||% ""),
        result = result,
        parameter = parameter
      ))
    }
  }
  list(
    pathway_database = pathway_database@database_info$source %||% "custom",
    pathway_version = pathway_database@database_info$version %||% "",
    result = result,
    parameter = list(id_column = id_column, method = method, p_adjust_method = p_adjust_method)
  )
}

metminer_enrich_pathway <- function(query_id,
                                    pathway_database,
                                    id_column = "KEGG.ID",
                                    p_adjust_method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),
                                    method = c("hypergeometric", "fisher"),
                                    min_pathway_size = 2,
                                    allow_first_column_fallback = TRUE) {
  method <- match.arg(method)
  p_adjust_method <- match.arg(p_adjust_method)

  query_id <- unique(trimws(as.character(query_id %||% character())))
  query_id <- query_id[has_text(query_id)]
  if (length(query_id) == 0) {
    return(NULL)
  }

  database <- metminer_extract_pathway_compound_ids(
    pathway_database,
    id_column = id_column,
    allow_first_column_fallback = allow_first_column_fallback
  )
  database <- lapply(database, unique)
  keep <- vapply(database, length, integer(1)) >= min_pathway_size
  database <- database[keep]
  if (length(database) == 0) {
    stop("No pathway contains enough compound IDs for enrichment.", call. = FALSE)
  }

  all_id <- unique(unlist(database, use.names = FALSE))
  query_id <- unique(query_id[query_id %in% all_id])
  if (length(query_id) == 0) {
    return(NULL)
  }

  num_all <- length(all_id)
  num_sig <- length(query_id)
  rows <- lapply(names(database), function(pid) {
    members <- unique(database[[pid]])
    overlap <- query_id[query_id %in% members]
    a1 <- length(overlap)
    a2 <- length(members) - a1
    a3 <- num_sig - a1
    a4 <- (num_all - num_sig) - a2
    p_value <- if (identical(method, "hypergeometric")) {
      stats::phyper(q = a1 - 1, m = length(members), n = num_all - length(members), k = num_sig, lower.tail = FALSE)
    } else {
      stats::fisher.test(matrix(c(a1, a2, a3, a4), nrow = 2, byrow = TRUE), alternative = "greater")$p.value
    }
    data.frame(
      pathway_id = pid,
      p_value = p_value,
      all_id = paste(members, collapse = ";"),
      all_number = length(members),
      mapped_id = paste(overlap, collapse = ";"),
      mapped_number = length(overlap),
      mapped_percentage = if (length(members) == 0) 0 else length(overlap) * 100 / length(members),
      stringsAsFactors = FALSE
    )
  })

  stats <- do.call(rbind, rows)
  stats$p_value_adjust <- stats::p.adjust(stats$p_value, method = p_adjust_method)
  info <- metminer_pathway_info_table(pathway_database)
  result <- merge(info, stats, by = "pathway_id", all.y = TRUE, sort = FALSE)
  result <- result[order(result$p_value_adjust, result$p_value, -result$mapped_number, na.last = TRUE), , drop = FALSE]
  rownames(result) <- NULL

  metminer_make_enrich_result(
    pathway_database = pathway_database,
    result = result,
    id_column = id_column,
    method = method,
    p_adjust_method = p_adjust_method,
    query_id = query_id
  )
}

metminer_enrich_kegg <- function(query_id, pathway_database, ...) {
  metminer_enrich_pathway(query_id, pathway_database, id_column = "KEGG.ID", ...)
}

metminer_enrich_plantcyc <- function(query_id, pathway_database, ...) {
  if (!grepl("plantcyc|pmn|biocyc|pgdb", pathway_database@database_info$source %||% "",
             ignore.case = TRUE, perl = TRUE)) {
    stop("pathway_database must be a PlantCyc/PMN pathway database.", call. = FALSE)
  }
  metminer_enrich_pathway(
    query_id,
    pathway_database,
    id_column = "PlantCyc.ID",
    allow_first_column_fallback = FALSE,
    ...
  )
}

metminer_extract_enrich_result_table <- function(x) {
  if (is.null(x)) return(data.frame())
  if (methods::is(x, "enrich_result")) return(as.data.frame(x@result, stringsAsFactors = FALSE))
  as.data.frame(x$result %||% data.frame(), stringsAsFactors = FALSE)
}

metminer_read_query_ids <- function(path = NULL, id_column = NULL) {
  if (is.null(path) || !has_text(path) || !file.exists(path)) {
    return(character())
  }
  ext <- tolower(tools::file_ext(path))
  tab <- if (identical(ext, "csv")) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (nrow(tab) == 0) return(character())
  if (!is.null(id_column) && id_column %in% colnames(tab)) {
    ids <- tab[[id_column]]
  } else {
    ids <- tab[[1]]
  }
  ids <- trimws(as.character(ids))
  unique(ids[has_text(ids)])
}

metminer_query_ids_from_annotation_filter <- function(filter_result, id_column = "KEGG.ID",
                                                      table = c("collapse", "expand", "final")) {
  table <- match.arg(table)
  if (is.null(filter_result)) return(character())
  x <- switch(
    table,
    collapse = filter_result$collapse_table,
    expand = filter_result$expand_table,
    final = filter_result$final_table
  )
  if (is.null(x) || nrow(x) == 0 || !id_column %in% colnames(x)) return(character())
  ids <- trimws(as.character(x[[id_column]]))
  unique(ids[has_text(ids)])
}

metminer_standardize_enrichment_columns <- function(x) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(x)
  canonical <- c(
    variable_id = "variable_id",
    variable.id = "variable_id",
    variableid = "variable_id",
    compound.name = "Compound.name",
    compound_name = "Compound.name",
    kegg.id = "KEGG.ID",
    kegg_id = "KEGG.ID",
    plantcyc.id = "PlantCyc.ID",
    plantcyc_id = "PlantCyc.ID",
    biocyc.id = "BIOCYC.ID",
    biocyc_id = "BIOCYC.ID",
    lab.id = "Lab.ID",
    lab_id = "Lab.ID",
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
  if ("BIOCYC.ID" %in% colnames(x)) {
    if (!"PlantCyc.ID" %in% colnames(x)) {
      x$PlantCyc.ID <- x$BIOCYC.ID
    } else {
      plant <- as.character(x$PlantCyc.ID)
      biocyc <- as.character(x$BIOCYC.ID)
      fill <- !has_text(plant) & has_text(biocyc)
      plant[fill] <- biocyc[fill]
      x$PlantCyc.ID <- plant
    }
  }
  x <- metminer_fill_plantcyc_id_from_lab_id(x)
  x
}

metminer_fill_plantcyc_id_from_lab_id <- function(x) {
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(x)
  for (col in c("PlantCyc.ID", "Lab.ID", "Database")) {
    if (!col %in% colnames(x)) x[[col]] <- NA_character_
  }
  plant <- trimws(as.character(x$PlantCyc.ID))
  lab <- trimws(as.character(x$Lab.ID))
  database <- tolower(trimws(as.character(x$Database)))
  is_plantcyc <- grepl("plantcyc|pmn|biocyc|pgdb", database, perl = TRUE)
  looks_plantcyc_id <- has_text(lab) & !grepl("^C[0-9]{5}$", lab, perl = TRUE)
  fill <- !has_text(plant) & is_plantcyc & looks_plantcyc_id
  plant[fill] <- lab[fill]
  x$PlantCyc.ID <- plant
  x
}

metminer_object_annotation_lookup <- function(object, mode = NA_character_) {
  if (is.null(object)) return(data.frame())
  ann <- tryCatch(as.data.frame(object@annotation_table, stringsAsFactors = FALSE), error = function(e) data.frame())
  ann <- metminer_standardize_enrichment_columns(ann)
  if (nrow(ann) == 0) return(ann)
  if (!"variable_id" %in% colnames(ann)) return(data.frame())
  ann$mode <- as.character(mode)
  ann
}

metminer_differential_annotation_lookup <- function(annotation_filter_result, annotated_objects = NULL) {
  tabs <- list(
    annotation_filter_result$collapse_table,
    annotation_filter_result$expand_table,
    annotation_filter_result$final_table
  )
  if (!is.null(annotated_objects)) {
    tabs <- c(
      tabs,
      list(
        metminer_object_annotation_lookup(annotated_objects$positive, mode = "positive"),
        metminer_object_annotation_lookup(annotated_objects$negative, mode = "negative")
      )
    )
  }
  tabs <- lapply(tabs, metminer_standardize_enrichment_columns)
  tabs <- tabs[vapply(tabs, function(x) nrow(x) > 0 && "variable_id" %in% colnames(x), logical(1))]
  if (length(tabs) == 0) return(data.frame())
  tabs <- lapply(tabs, function(x) {
    as.data.frame(lapply(x, function(col) as.character(col)), stringsAsFactors = FALSE, check.names = FALSE)
  })
  ann <- dplyr::bind_rows(tabs)
  ann$variable_id <- as.character(ann$variable_id)
  if ("mode" %in% colnames(ann)) {
    mode <- tolower(trimws(as.character(ann$mode)))
    prefix <- ifelse(mode %in% c("positive", "pos", "+"), "pos",
                     ifelse(mode %in% c("negative", "neg", "-"), "neg", NA_character_))
    use_prefix <- has_text(prefix) & !grepl("^pos::|^neg::", ann$variable_id)
    ann$prefixed_variable_id <- ann$variable_id
    ann$prefixed_variable_id[use_prefix] <- paste(prefix[use_prefix], ann$variable_id[use_prefix], sep = "::")
  } else {
    ann$prefixed_variable_id <- ann$variable_id
  }
  ann$raw_variable_id <- sub("^(pos|neg)::", "", ann$variable_id)
  ann <- ann[!duplicated(paste(ann$prefixed_variable_id, ann$raw_variable_id, sep = "\r")), , drop = FALSE]
  ann
}

metminer_fill_differential_ids_from_annotation <- function(x, id_column, annotation_filter_result, annotated_objects = NULL) {
  ann <- metminer_differential_annotation_lookup(annotation_filter_result, annotated_objects = annotated_objects)
  if (nrow(x) == 0 || nrow(ann) == 0 || !id_column %in% colnames(ann) || !"variable_id" %in% colnames(x)) {
    return(x)
  }
  if (!id_column %in% colnames(x)) x[[id_column]] <- NA_character_
  x[[id_column]] <- as.character(x[[id_column]])
  need <- !has_text(x[[id_column]])
  if (!any(need)) return(x)

  keys <- as.character(x$variable_id)
  raw_keys <- sub("^(pos|neg)::", "", keys)
  ann_id_by_prefixed <- stats::setNames(as.character(ann[[id_column]]), ann$prefixed_variable_id)
  ann_id_by_raw <- stats::setNames(as.character(ann[[id_column]]), ann$raw_variable_id)
  filled <- ann_id_by_prefixed[keys]
  miss <- !has_text(filled)
  filled[miss] <- ann_id_by_raw[raw_keys[miss]]
  x[[id_column]][need & has_text(filled)] <- filled[need & has_text(filled)]
  x
}

metminer_query_ids_from_differential_result <- function(differential_result,
                                                        annotation_filter_result = NULL,
                                                        annotated_objects = NULL,
                                                        id_column = "KEGG.ID",
                                                        change_values = c("Up", "Down")) {
  x <- differential_result$result %||% differential_result$table %||% differential_result
  x <- metminer_standardize_enrichment_columns(x)
  if (nrow(x) == 0) {
    return(list(ids = character(), table = data.frame(), diagnostics = list(total_rows = 0, changed_rows = 0, id_rows = 0, lookup_rows = 0)))
  }
  total_rows <- nrow(x)
  if ("change" %in% colnames(x)) {
    x <- x[x$change %in% change_values, , drop = FALSE]
  }
  changed_rows <- nrow(x)
  lookup_rows <- nrow(metminer_differential_annotation_lookup(annotation_filter_result, annotated_objects = annotated_objects))
  x <- metminer_fill_differential_ids_from_annotation(
    x = x,
    id_column = id_column,
    annotation_filter_result = annotation_filter_result,
    annotated_objects = annotated_objects
  )
  if (!id_column %in% colnames(x)) {
    return(list(ids = character(), table = x[0, , drop = FALSE], diagnostics = list(total_rows = total_rows, changed_rows = changed_rows, id_rows = 0, lookup_rows = lookup_rows)))
  }
  ids <- trimws(as.character(x[[id_column]]))
  ids <- ids[has_text(ids)]
  if (length(ids) == 0) {
    return(list(ids = character(), table = x[0, , drop = FALSE], diagnostics = list(total_rows = total_rows, changed_rows = changed_rows, id_rows = 0, lookup_rows = lookup_rows)))
  }
  x$query_id <- trimws(as.character(x[[id_column]]))
  list(
    ids = unique(ids),
    table = x[has_text(x$query_id), , drop = FALSE],
    diagnostics = list(total_rows = total_rows, changed_rows = changed_rows, id_rows = length(unique(ids)), lookup_rows = lookup_rows)
  )
}

metminer_split_ids <- function(x) {
  x <- as.character(x %||% character())
  ids <- unlist(strsplit(paste(x, collapse = ";"), "[;,|]", perl = TRUE), use.names = FALSE)
  ids <- trimws(ids)
  unique(ids[has_text(ids)])
}

metminer_enrichment_pathway_feature_table <- function(result_table,
                                                      query_table,
                                                      pathway_id,
                                                      id_column = "KEGG.ID") {
  result_table <- as.data.frame(result_table %||% data.frame(), stringsAsFactors = FALSE)
  query_table <- as.data.frame(query_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(result_table) == 0 || nrow(query_table) == 0 || is.null(pathway_id)) {
    return(data.frame())
  }
  hit <- result_table[result_table$pathway_id == pathway_id, , drop = FALSE]
  if (nrow(hit) == 0 || !"mapped_id" %in% colnames(hit)) {
    return(data.frame())
  }
  mapped_ids <- metminer_split_ids(hit$mapped_id[1])
  if (length(mapped_ids) == 0) return(data.frame())

  if (!"query_id" %in% colnames(query_table)) {
    if (!id_column %in% colnames(query_table)) return(data.frame())
    query_table$query_id <- query_table[[id_column]]
  }
  query_table$query_id <- trimws(as.character(query_table$query_id))
  out <- query_table[query_table$query_id %in% mapped_ids, , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())

  preferred <- intersect(
    c("variable_id", "query_id", "mz", "rt", "Compound.name", "KEGG.ID", "PlantCyc.ID",
      "Lab.ID", "Adduct", "fc", "log2_fc", "p_value", "p_value_adjust", "change", "mode"),
    colnames(out)
  )
  out <- out[, unique(c(preferred, setdiff(colnames(out), preferred))), drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_enrichment_plot_table <- function(result_table, top_n = 20) {
  x <- as.data.frame(result_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) return(x)
  x$pathway_label <- ifelse(has_text(x$pathway_name), x$pathway_name, x$pathway_id)
  x$neg_log10_fdr <- -log10(pmax(suppressWarnings(as.numeric(x$p_value_adjust)), .Machine$double.xmin))
  x$mapped_number <- suppressWarnings(as.numeric(x$mapped_number))
  x$mapped_percentage <- suppressWarnings(as.numeric(x$mapped_percentage))
  x$hover_text <- paste0(
    "Pathway: ", x$pathway_label,
    "<br>ID: ", x$pathway_id,
    "<br>Mapped DAMs: ", x$mapped_number,
    "<br>Mapped %: ", signif(x$mapped_percentage, 3),
    "<br>p: ", signif(x$p_value, 3),
    "<br>FDR: ", signif(x$p_value_adjust, 3),
    "<br>Mapped IDs: ", x$mapped_id
  )
  x <- x[order(x$p_value_adjust, x$p_value, -x$mapped_number, na.last = TRUE), , drop = FALSE]
  utils::head(x, top_n)
}

metminer_plot_enrichment_bubble <- function(enrich_result = NULL, result_table = NULL, top_n = 20) {
  x <- metminer_enrichment_plot_table(result_table %||% metminer_extract_enrich_result_table(enrich_result), top_n = top_n)
  if (nrow(x) == 0) return(plotly::plot_ly())
  x$pathway_label <- factor(x$pathway_label, levels = rev(x$pathway_label))
  p <- plotly::plot_ly(
    data = x,
    x = ~mapped_percentage,
    y = ~pathway_label,
    key = ~pathway_id,
    source = "enrichment_bubble",
    type = "scatter",
    mode = "markers",
    size = ~mapped_number,
    color = ~neg_log10_fdr,
    colors = "Viridis",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(opacity = 0.82, sizemode = "diameter", sizeref = 0.4)
  ) |>
    plotly::layout(
      xaxis = list(title = "Mapped differential metabolites (%)"),
      yaxis = list(title = ""),
      coloraxis = list(colorbar = list(title = "-log10(FDR)")),
      margin = list(l = 160, r = 30, t = 20, b = 55)
    )
  plotly::event_register(p, "plotly_click")
}

metminer_plot_enrichment_bar <- function(enrich_result = NULL, result_table = NULL, top_n = 20) {
  x <- metminer_enrichment_plot_table(result_table %||% metminer_extract_enrich_result_table(enrich_result), top_n = top_n)
  if (nrow(x) == 0) return(plotly::plot_ly())
  if (methods::is(enrich_result, "enrich_result") &&
      requireNamespace("metpath", quietly = TRUE) &&
      requireNamespace("ggplot2", quietly = TRUE)) {
    metpath_plot <- tryCatch({
      p <- metpath::enrich_bar_plot(
        enrich_result,
        x_axis = "p_value_adjust",
        cutoff = 1,
        top = top_n
      )
      plotly::ggplotly(p)
    }, error = function(e) NULL)
    if (!is.null(metpath_plot)) {
      return(metpath_plot)
    }
  }

  x$pathway_label <- factor(x$pathway_label, levels = rev(x$pathway_label))
  plotly::plot_ly(
    data = x,
    x = ~neg_log10_fdr,
    y = ~pathway_label,
    type = "bar",
    orientation = "h",
    marker = list(color = "#008080"),
    text = ~hover_text,
    hoverinfo = "text"
  ) |>
    plotly::layout(
      xaxis = list(title = "-log10(FDR)"),
      yaxis = list(title = ""),
      margin = list(l = 160, r = 30, t = 20, b = 55)
    )
}
