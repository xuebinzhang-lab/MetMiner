# ---- ClassyFire classification summaries for final annotations ----

metminer_classification_empty_records <- function() {
  data.frame(
    metabolite_id = character(),
    mode = character(),
    representative_feature = character(),
    compound_name = character(),
    annotation_level = integer(),
    Super_class = character(),
    Class = character(),
    Sub_class = character(),
    n_features = integer(),
    mean_area = numeric(),
    stringsAsFactors = FALSE
  )
}

metminer_classification_empty_summary <- function() {
  data.frame(
    dimension = character(),
    class_name = character(),
    count = integer(),
    percent = numeric(),
    metabolite_ids = character(),
    compound_names = character(),
    stringsAsFactors = FALSE
  )
}

metminer_classification_first_col <- function(x, candidates, default = NA_character_) {
  hit <- intersect(candidates, colnames(x))
  if (length(hit) == 0) {
    return(rep(default, nrow(x)))
  }
  out <- x[[hit[1]]]
  out <- as.character(out %||% default)
  out[!has_text(out)] <- default
  out
}

metminer_classification_name_key <- function(x) {
  x <- as.character(x %||% NA_character_)
  x <- tolower(trimws(x))
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

metminer_classification_lookup_from_object <- function(object, mode = NA_character_) {
  if (is.null(object)) {
    return(data.frame())
  }
  tab <- metminer_safe_extract_annotation_table(object)
  if (is.null(tab) || nrow(tab) == 0) {
    return(data.frame())
  }
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  compound_name <- metminer_classification_first_col(tab, c("Compound.name", "compound_name"))
  out <- data.frame(
    mode = mode,
    compound_name = compound_name,
    compound_key = metminer_classification_name_key(compound_name),
    Super_class = metminer_classification_first_col(tab, c("Super_class", "super_class", "Superclass", "superclass")),
    Class = metminer_classification_first_col(tab, c("Class", "classyfire_class", "class")),
    Sub_class = metminer_classification_first_col(tab, c("Sub_class", "sub_class", "Subclass", "subclass")),
    stringsAsFactors = FALSE
  )
  out <- out[has_text(out$compound_key), , drop = FALSE]
  out <- out[has_text(out$Super_class) | has_text(out$Class) | has_text(out$Sub_class), , drop = FALSE]
  if (nrow(out) == 0) {
    return(out)
  }
  out[!duplicated(paste(out$mode, out$compound_key, sep = "\r")), , drop = FALSE]
}

metminer_classification_lookup <- function(positive_object = NULL, negative_object = NULL) {
  out <- dplyr::bind_rows(
    metminer_classification_lookup_from_object(positive_object, "positive"),
    metminer_classification_lookup_from_object(negative_object, "negative")
  )
  if (nrow(out) == 0) {
    return(out)
  }
  out[!duplicated(paste(out$mode, out$compound_key, sep = "\r")), , drop = FALSE]
}

metminer_classification_records <- function(filter_result = NULL,
                                            positive_object = NULL,
                                            negative_object = NULL) {
  final <- as.data.frame(filter_result$final_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(final) == 0) {
    return(metminer_classification_empty_records())
  }

  for (col in c("metabolite_id", "mode", "representative_feature", "compound_name")) {
    if (!col %in% colnames(final)) {
      final[[col]] <- NA_character_
    }
  }
  if (!"annotation_level" %in% colnames(final)) final$annotation_level <- NA_integer_
  if (!"n_features" %in% colnames(final)) final$n_features <- NA_integer_
  if (!"mean_area" %in% colnames(final)) final$mean_area <- NA_real_

  records <- data.frame(
    metabolite_id = as.character(final$metabolite_id),
    mode = as.character(final$mode),
    representative_feature = as.character(final$representative_feature),
    compound_name = as.character(final$compound_name),
    annotation_level = suppressWarnings(as.integer(final$annotation_level)),
    Super_class = metminer_classification_first_col(final, c("Super_class", "super_class", "Superclass", "superclass")),
    Class = metminer_classification_first_col(final, c("Class", "classyfire_class", "class")),
    Sub_class = metminer_classification_first_col(final, c("Sub_class", "sub_class", "Subclass", "subclass")),
    n_features = suppressWarnings(as.integer(final$n_features)),
    mean_area = suppressWarnings(as.numeric(final$mean_area)),
    stringsAsFactors = FALSE
  )

  missing_class <- !(has_text(records$Super_class) | has_text(records$Class) | has_text(records$Sub_class))
  if (any(missing_class)) {
    lookup <- metminer_classification_lookup(positive_object, negative_object)
    if (nrow(lookup) > 0) {
      key <- metminer_classification_name_key(records$compound_name)
      hit <- match(paste(records$mode, key, sep = "\r"), paste(lookup$mode, lookup$compound_key, sep = "\r"))
      fallback_hit <- match(key, lookup$compound_key)
      hit[is.na(hit)] <- fallback_hit[is.na(hit)]
      fill <- missing_class & !is.na(hit)
      for (col in c("Super_class", "Class", "Sub_class")) {
        records[[col]][fill] <- lookup[[col]][hit[fill]]
      }
    }
  }

  for (col in c("Super_class", "Class", "Sub_class")) {
    records[[col]][!has_text(records[[col]])] <- "Unclassified"
  }
  records
}

metminer_classification_summary <- function(records) {
  records <- as.data.frame(records %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(records) == 0) {
    return(metminer_classification_empty_summary())
  }

  dims <- c("Super_class", "Class", "Sub_class")
  rows <- lapply(dims, function(dim_col) {
    x <- records
    x$class_name <- as.character(x[[dim_col]] %||% "Unclassified")
    x$class_name[!has_text(x$class_name)] <- "Unclassified"
    split_rows <- split(x, x$class_name)
    out <- lapply(names(split_rows), function(nm) {
      one <- split_rows[[nm]]
      data.frame(
        dimension = dim_col,
        class_name = nm,
        count = nrow(one),
        percent = round(100 * nrow(one) / nrow(records), 3),
        metabolite_ids = paste(unique(one$metabolite_id[has_text(one$metabolite_id)]), collapse = ";"),
        compound_names = paste(utils::head(unique(one$compound_name[has_text(one$compound_name)]), 50), collapse = ";"),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, out)
  })
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$dimension, -out$count, out$class_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

metminer_classification_plot_data <- function(summary_table,
                                              dimension = c("Super_class", "Class", "Sub_class"),
                                              top_n = 12,
                                              include_unclassified = TRUE) {
  dimension <- match.arg(dimension)
  x <- as.data.frame(summary_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) {
    return(metminer_classification_empty_summary())
  }
  x <- x[x$dimension == dimension, , drop = FALSE]
  if (!isTRUE(include_unclassified)) {
    x <- x[x$class_name != "Unclassified", , drop = FALSE]
  }
  if (nrow(x) == 0) {
    return(metminer_classification_empty_summary())
  }
  x <- x[order(-x$count, x$class_name), , drop = FALSE]
  top_n <- max(1L, as.integer(top_n %||% 12L))
  if (nrow(x) > top_n) {
    kept <- x[seq_len(top_n), , drop = FALSE]
    other <- x[-seq_len(top_n), , drop = FALSE]
    other_row <- data.frame(
      dimension = dimension,
      class_name = "Other",
      count = sum(other$count, na.rm = TRUE),
      percent = round(sum(other$percent, na.rm = TRUE), 3),
      metabolite_ids = paste(other$metabolite_ids[has_text(other$metabolite_ids)], collapse = ";"),
      compound_names = paste(utils::head(unlist(strsplit(paste(other$compound_names, collapse = ";"), ";", fixed = TRUE)), 50), collapse = ";"),
      stringsAsFactors = FALSE
    )
    x <- rbind(kept, other_row)
  }
  x$label <- paste0(x$class_name, " (", x$count, ", ", sprintf("%.1f", x$percent), "%)")
  x
}

metminer_classification_static_pie <- function(plot_data, title = "ClassyFire classification") {
  x <- as.data.frame(plot_data %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No classification data"))
  }
  x$class_name <- factor(x$class_name, levels = rev(x$class_name))
  ggplot2::ggplot(x, ggplot2::aes(x = "", y = count, fill = class_name)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.35) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
    )
}

metminer_classification_interactive_pie <- function(plot_data, title = "ClassyFire classification") {
  x <- as.data.frame(plot_data %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0) {
    return(plotly::plot_ly())
  }
  plotly::plot_ly(
    data = x,
    labels = ~class_name,
    values = ~count,
    type = "pie",
    text = ~label,
    hovertext = ~paste0(
      class_name,
      "<br>Count: ", count,
      "<br>Percent: ", sprintf("%.2f", percent), "%",
      "<br>Examples: ", compound_names
    ),
    hoverinfo = "text",
    textinfo = "percent",
    sort = FALSE
  ) |>
    plotly::layout(
      title = list(text = title),
      legend = list(orientation = "v"),
      margin = list(l = 10, r = 10, t = 60, b = 10)
    )
}
