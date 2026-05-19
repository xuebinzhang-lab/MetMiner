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

metminer_extract_pathway_compound_ids <- function(pathway_database, id_column = "KEGG.ID") {
  if (!methods::is(pathway_database, "pathway_database")) {
    stop("pathway_database must be a metpath pathway_database object.", call. = FALSE)
  }

  compound_list <- pathway_database@compound_list
  ids <- lapply(compound_list, function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (nrow(x) == 0) return(character())
    if (!id_column %in% colnames(x)) {
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
                                    min_pathway_size = 2) {
  method <- match.arg(method)
  p_adjust_method <- match.arg(p_adjust_method)

  query_id <- unique(trimws(as.character(query_id %||% character())))
  query_id <- query_id[has_text(query_id)]
  if (length(query_id) == 0) {
    return(NULL)
  }

  database <- metminer_extract_pathway_compound_ids(pathway_database, id_column = id_column)
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
  metminer_enrich_pathway(query_id, pathway_database, id_column = "PlantCyc.ID", ...)
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
