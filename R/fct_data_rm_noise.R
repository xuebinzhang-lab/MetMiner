#' Find and Remove Noise
#'
#' Detects errors or batch effects in untargeted metabolomics data
#' based on missing value frequency in QC and sample groups,
#' with optional RSD-based filtering.
#'
#' @param object A mass_dataset class object.
#' @param tag Column name in sample_info used for grouping (default "class").
#' @param qc_na_freq Missing value ratio threshold for QC samples.
#' @param S_na_freq Missing value ratio threshold for sample groups.
#' @param do_rsd Whether to apply RSD-based filtering after MV filtering.
#' @param rsd_cutoff RSD threshold (%) for QC samples (only used if do_rsd = TRUE).
#' @return A cleaned mass_dataset object, or NULL if no features survive filtering.
#' @importFrom massdataset extract_sample_info extract_expression_data
#'   extract_variable_info activate_mass_dataset mutate_rsd
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull
#' @export
find_noise_multiple <- function(object, tag = "class", qc_na_freq = 0.2, S_na_freq = 0.5,
                                 do_rsd = FALSE, rsd_cutoff = 30) {

  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' class object.")
  }

  sample_info <- massdataset::extract_sample_info(object)

  # Build grouping vector
  if (tag == "class") {
    group_vector <- sample_info$class
  } else {
    if (!tag %in% colnames(sample_info)) {
      stop(paste("Column", tag, "not found in sample_info"))
    }
    group_vector <- ifelse(sample_info$class == "QC", "QC",
                           as.character(sample_info[[tag]]))
  }
  unique_groups <- unique(group_vector)
  expression_data <- massdataset::extract_expression_data(object)

  # 1. QC NA check
  qc_idx <- which(group_vector == "QC")
  qc_na_ratios <- if (length(qc_idx) > 0) {
    rowMeans(is.na(expression_data[, qc_idx, drop = FALSE]))
  } else {
    rep(0, nrow(expression_data))
  }

  # 2. Group NA check (exclude QC)
  non_qc_groups <- setdiff(unique_groups, "QC")
  if (length(non_qc_groups) > 0) {
    group_pass_list <- lapply(non_qc_groups, function(grp) {
      g_idx <- which(group_vector == grp)
      if (length(g_idx) == 0) return(rep(FALSE, nrow(expression_data)))
      g_na <- rowMeans(is.na(expression_data[, g_idx, drop = FALSE]))
      return(g_na <= S_na_freq)
    })
    any_group_pass <- Reduce(`|`, group_pass_list)
  } else {
    any_group_pass <- rep(TRUE, nrow(expression_data))
  }

  keep_mv <- (qc_na_ratios <= qc_na_freq) & any_group_pass
  keep_mv[is.na(keep_mv)] <- FALSE

  keep_indices <- which(unname(keep_mv))
  if (length(keep_indices) == 0) {
    warning("No features kept after MV filter.")
    return(NULL)
  }
  object_mv <- object[keep_indices, ]

  # 3. Optional RSD filtering
  if (do_rsd) {
    s_info_curr <- massdataset::extract_sample_info(object_mv)
    qc_ids <- s_info_curr %>%
      dplyr::filter(class == "QC") %>%
      dplyr::pull(sample_id)

    if (length(qc_ids) > 1) {
      object_mv <- massdataset::mutate_rsd(object_mv,
                                            according_to_samples = qc_ids)
      object_mv <- object_mv %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::filter(rsd < rsd_cutoff)
    }
  }

  return(object_mv)
}

#' Filter Features by Blank Ratio
#'
#' Removes features whose mean intensity in real samples is not sufficiently
#' higher than in blank samples, indicating the signal is primarily
#' background/contamination.
#'
#' @param object A mass_dataset class object.
#' @param blank_label Character. The class value identifying blank samples
#'   (default "Blank").
#' @param sample_label Character. The class value identifying real biological
#'   samples (default "Subject").
#' @param ratio_cutoff Numeric. Minimum acceptable sample/blank ratio
#'   (default 3).
#' @return A mass_dataset object with low-ratio features removed.
#' @importFrom massdataset extract_sample_info extract_expression_data
#' @export
find_noise_blank <- function(object, blank_label = "Blank", sample_label = "Subject",
                              ratio_cutoff = 3) {

  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' class object.")
  }

  sample_info <- massdataset::extract_sample_info(object)

  blank_idx <- which(sample_info$class == blank_label)
  sample_idx <- which(sample_info$class == sample_label)

  if (length(blank_idx) == 0) {
    warning("No blank samples found with class='", blank_label,
            "'. Skipping blank ratio filter.")
    return(object)
  }
  if (length(sample_idx) == 0) {
    warning("No samples found with class='", sample_label,
            "'. Skipping blank ratio filter.")
    return(object)
  }

  expr_mat <- massdataset::extract_expression_data(object)

  blank_means <- rowMeans(as.matrix(expr_mat[, blank_idx, drop = FALSE]), na.rm = TRUE)
  sample_means <- rowMeans(as.matrix(expr_mat[, sample_idx, drop = FALSE]), na.rm = TRUE)

  ratios <- sample_means / blank_means
  # Keep features with ratio >= cutoff, or blank_mean = 0 (pure signal), or NA ratios (all NAs)
  keep_idx <- which(is.na(ratios) | is.infinite(ratios) | ratios >= ratio_cutoff)

  n_removed <- nrow(expr_mat) - length(keep_idx)
  if (n_removed > 0) {
    message("Blank ratio filter: removed ", n_removed,
            " features (ratio < ", ratio_cutoff, ")")
    object <- object[keep_idx, ]
  } else {
    message("Blank ratio filter: all features pass.")
  }

  return(object)
}

#' Filter Low-Intensity Peaks
#'
#' Marks individual peak intensities as NA when they fall below a noise
#' threshold estimated from the data. After intensity filtering, downstream
#' MV-based filtering naturally removes features that accumulate too many NAs.
#'
#' Two methods are available:
#' - "blank": Estimates per-feature noise baseline from blank samples
#'   (blank_mean + blank_sd_multiplier * blank_SD). The gold standard when
#'   blank samples exist.
#' - "distribution": Per-sample, finds the antimode between noise and signal
#'   peaks in the log-intensity distribution. Falls back to a percentile cutoff
#'   when no clear bimodal structure is found.
#'
#' @param object A mass_dataset class object.
#' @param blank_label Character. Class value for blank samples (default "Blank").
#' @param method Character. "blank" or "distribution". If "blank" and fewer
#'   than 2 blank samples exist, automatically falls back to "distribution".
#' @param blank_sd_multiplier Numeric. Multiplier for blank SD when computing
#'   the per-feature noise cutoff (default 3).
#' @param percentile_fallback Numeric. Fallback percentile (0–1) when the
#'   distribution method cannot find a clear noise/signal antimode
#'   (default 0.01).
#' @return A mass_dataset object with low-intensity peaks set to NA.
#' @importFrom massdataset extract_sample_info extract_expression_data
#' @importFrom stats sd density quantile
#' @export
find_noise_intensity <- function(object,
                                  blank_label = "Blank",
                                  method = c("blank", "distribution"),
                                  blank_sd_multiplier = 3,
                                  percentile_fallback = 0.01) {

  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' class object.")
  }

  method <- match.arg(method)
  sample_info <- massdataset::extract_sample_info(object)
  expr_mat <- as.matrix(massdataset::extract_expression_data(object))

  blank_idx <- which(sample_info$class == blank_label)

  if (method == "blank" && length(blank_idx) < 2) {
    warning("Fewer than 2 blank samples; falling back to distribution method.")
    method <- "distribution"
  }

  na_before <- sum(is.na(expr_mat))

  if (method == "blank") {
    blank_data <- expr_mat[, blank_idx, drop = FALSE]
    blank_means <- rowMeans(blank_data, na.rm = TRUE)
    blank_sds  <- apply(blank_data, 1, sd, na.rm = TRUE)
    blank_sds[is.na(blank_sds) | blank_sds == 0] <- stats::median(blank_sds, na.rm = TRUE)

    thresholds <- blank_means + blank_sd_multiplier * blank_sds

    sample_idx <- setdiff(seq_len(ncol(expr_mat)), blank_idx)
    for (j in sample_idx) {
      below <- which(expr_mat[, j] < thresholds & !is.na(expr_mat[, j]))
      expr_mat[below, j] <- NA_real_
    }
  } else {
    for (j in seq_len(ncol(expr_mat))) {
      x <- expr_mat[, j]
      x <- x[!is.na(x) & x > 0]
      if (length(x) < 20) next

      log_x <- log10(x)
      cutoff_log <- .find_intensity_antimode(log_x, percentile_fallback)

      below <- which(expr_mat[, j] < 10^cutoff_log & !is.na(expr_mat[, j]))
      expr_mat[below, j] <- NA_real_
    }
  }

  na_after <- sum(is.na(expr_mat))
  n_new <- na_after - na_before
  if (n_new > 0) {
    message("Intensity filter: marked ", n_new,
            " low-intensity peaks as NA (",
            sprintf("%.1f", 100 * n_new / length(expr_mat)), "% of matrix)")
  } else {
    message("Intensity filter: no peaks fell below noise threshold.")
  }

  object@expression_data <- as.data.frame(expr_mat)
  return(object)
}

# Find the antimode (valley) between noise and signal peaks in log-intensity
.find_intensity_antimode <- function(log_x, percentile_fallback = 0.01) {
  d <- stats::density(log_x, n = 512)
  n <- length(d$y)

  minima_idx <- which(d$y[2:(n - 1)] < d$y[1:(n - 2)] &
                      d$y[2:(n - 1)] < d$y[3:n]) + 1

  if (length(minima_idx) == 0) {
    return(stats::quantile(log_x, percentile_fallback, na.rm = TRUE))
  }

  peak_idx <- which.max(d$y)
  valleys_left <- minima_idx[minima_idx < peak_idx]

  if (length(valleys_left) > 0) {
    valley_idx <- valleys_left[length(valleys_left)]
  } else {
    return(stats::quantile(log_x, percentile_fallback, na.rm = TRUE))
  }

  return(d$x[valley_idx])
}
