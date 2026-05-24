#' Internal: Setup files and parallel plan for paramounter
#'
#' @param directory The data path for mzXML files.
#' @param filenum Number of mzXML files to use (3, 5, or "all"). Numeric
#'   choices are sampled evenly across the file list rather than taking only
#'   the first files.
#' @param thread Number of threads for parallel processing.
#' @return A list with `files` (character vector of selected file paths) and
#'   `n_chunks` (integer for chunking).
#' @noRd
.paramounter_setup <- function(directory, filenum, thread) {
  files <- list.files(path = directory, pattern = "\\.mzXML$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop("No .mzXML files found in directory")

  n_files <- length(files)
  if (identical(filenum, "all") || (is.character(filenum) && filenum == "all")) {
    file_idxs <- 1:n_files
  } else {
    num <- as.numeric(filenum[1])
    if (n_files <= num) {
      file_idxs <- 1:n_files
    } else {
      file_idxs <- unique(round(seq(1, n_files, length.out = num)))
    }
  }
  selected_files <- files[file_idxs]

  if (thread > 1) {
    if (.Platform$OS.type == "unix") {
      future::plan(future::multicore, workers = thread)
    } else {
      future::plan(future::multisession, workers = thread)
    }
  } else {
    future::plan(future::sequential)
  }

  n_chunks <- if (thread > 1) thread * 10 else 1
  list(files = selected_files, n_chunks = n_chunks)
}

#' Paramounter part1 (Optimized)
#'
#' Optimize XCMS parameters (detect PPM cutoff) using a binned EIC approach.
#' Fixed for parallel processing environment isolation.
#'
#' @param directory The data path for mzXML files.
#' @param massSDrange Numeric scalar. Multiplier applied to the within-peak m/z
#'   standard deviation when estimating mass tolerance in ppm; default 2.
#' @param smooth Numeric smoothing window level for the internal EIC smoother;
#'   0 disables smoothing (default 0).
#' @param cutoff Quantile of peak-level ppm estimates used as the suggested
#'   ppm cutoff (default 0.95).
#' @param thread Number of threads for parallel processing.
#' @param filenum Number of mzXML files to use (3, 5, or "all"). Numeric
#'   choices are sampled evenly across the file list rather than taking only
#'   the first files.
#'
#' @importFrom future plan multisession multicore
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom MSnbase readMSData mz intensity rtime
#' @importFrom stats sd median fivenum complete.cases
#' @importFrom utils head tail
#' @importFrom dplyr bind_rows
#' @import ggplot2
#' @export
paramounter_part1 <- function(directory, massSDrange = 2, smooth = 0, cutoff = 0.95, thread = 1, filenum = c(3, 5, "all")) {

  setup <- .paramounter_setup(directory, filenum, thread)
  selected_files <- setup$files
  n_chunks <- setup$n_chunks

  # Process Files
  ppm2D <- do.call(rbind, lapply(selected_files, function(f) {

    # Read Data
    msd <- MSnbase::readMSData(f, mode = "onDisk", msLevel. = 1)
    mz_list <- MSnbase::mz(msd)
    int_list <- MSnbase::intensity(msd)
    rts <- MSnbase::rtime(msd)
    n_scans <- length(mz_list)

    scan_lens <- lengths(mz_list)
    scan_ids <- rep(seq_len(n_scans), scan_lens)
    all_mz <- unlist(mz_list, use.names = FALSE)
    all_int <- unlist(int_list, use.names = FALSE)

    if (length(all_mz) == 0) return(NULL)

    # Binning
    bin_width <- 0.05
    min_mz <- min(all_mz)
    bins <- floor((all_mz - min_mz) / bin_width)
    bin_indices <- split(seq_along(all_mz), bins)
    bin_names <- names(bin_indices)

    # Chunking — cut(breaks = n) requires n >= 2
    n_cuts <- min(length(bin_names), n_chunks)
    if (n_cuts <= 1) {
      chunks <- list(bin_names)
    } else {
      chunks <- split(bin_names, cut(seq_along(bin_names),
                      breaks = n_cuts, labels = FALSE))
    }

    # Parallel Processing with Package Export
    res_df <- furrr::future_map_dfr(chunks, function(chunk_bins) {

      # Safe execution wrapper
      tryCatch({
        chunk_res <- lapply(chunk_bins, function(b_name) {
          idx <- bin_indices[[b_name]]
          b_scans <- scan_ids[idx]
          b_ints <- all_int[idx]

          scan_means <- tapply(b_ints, b_scans, mean)
          scan_idxs <- as.integer(names(scan_means))

          eic <- numeric(n_scans)
          eic[scan_idxs] <- scan_means

          # Smoothing
          eic_smooth <- .peak_smooth_opt(eic, smooth)

          # Noise
          eic_non0 <- sort(eic_smooth[eic_smooth > 0])
          if (length(eic_non0) == 0) return(NULL)

          cutoff_val <- max(eic_non0)
          if (length(eic_non0) > 10) {
            for (x in seq(10, length(eic_non0), 10)) {
              sub_vec <- eic_non0[1:x]
              thres <- mean(sub_vec) + 3 * sd(sub_vec)
              if (x + 1 <= length(eic_non0) && eic_non0[x + 1] >= thres) {
                cutoff_val <- eic_non0[x]
                break
              }
            }
          }

          # Peaks
          above_th <- which(eic_smooth > cutoff_val)
          if (length(above_th) == 0) return(NULL)

          breaks <- cumsum(c(1, diff(above_th) != 1))
          segments <- split(above_th, breaks)

          bin_peaks <- lapply(segments, function(seg) {
            apex_idx_in_seg <- which.max(eic_smooth[seg])
            peak_apex_scan <- seg[apex_idx_in_seg]

            relevant_indices <- idx[b_scans == peak_apex_scan]
            if (length(relevant_indices) == 0) return(NULL)

            apex_mz_idx <- which.max(all_int[relevant_indices])
            curr_ref_mz <- all_mz[relevant_indices[apex_mz_idx]]

            peak_indices <- idx[b_scans %in% seg]
            peak_mzs <- all_mz[peak_indices]

            if(length(peak_mzs) > 5) {
              stats <- fivenum(peak_mzs)
              iqr <- stats[4] - stats[2]
              peak_mzs <- peak_mzs[peak_mzs >= (stats[2] - 1.5*iqr) & peak_mzs <= (stats[4] + 1.5*iqr)]
            }

            if(length(peak_mzs) > 1) {
              ppm_diff <- (massSDrange * sd(peak_mzs)) / curr_ref_mz * 1e6
              return(data.frame(mz = curr_ref_mz, rt = rts[peak_apex_scan], ppm = ppm_diff))
            }
            return(NULL)
          })

          return(do.call(rbind, bin_peaks))
        })
        return(bind_rows(chunk_res))
      }, error = function(e) {
        # Return NULL on error to prevent crashing the whole future
        warning("Error in worker chunk: ", e$message)
        return(NULL)
      })

    }, .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("MSnbase", "stats") # IMPORTANT: Load packages on workers
    ))

    return(res_df)
  }))

  # 4. Output
  if (is.null(ppm2D) || nrow(ppm2D) == 0) {
    p <- ggplot() + annotate("text", x=0.5, y=0.5, label="No peaks detected") + theme_void()
    return(list(plot = p, ppmCut = 15))
  }

  ppm2D <- ppm2D[complete.cases(ppm2D), ]
  ppm2D <- ppm2D[order(ppm2D$ppm), ]
  ppm2D <- ppm2D[1:round(nrow(ppm2D) * 0.97), ]

  idx_cut <- round(nrow(ppm2D) * cutoff)
  dashline <- if (idx_cut > 0) ppm2D$ppm[idx_cut] else 0
  dashline <- round(dashline, 2)

  p <- ggplot(ppm2D, aes(x = mz, y = ppm)) +
    geom_point(alpha=0.5, size=0.8, color = "#2c3e50") +
    geom_hline(yintercept = dashline, linetype = "dashed", color = '#e74c3c', linewidth = 1) +
    labs(y = "Mass Tolerance (ppm)", x = "m/z", title = paste0("Recommended PPM Cutoff: ", dashline)) +
    theme_bw() + theme(plot.title = element_text(face = "bold", hjust = 0.5))

  message("Paramounter Part 1 Finish.")
  return(list(plot = p, ppmCut = dashline))
}


#' Paramounter part2 (Optimized)
#'
#' Optimize remaining XCMS parameters.
#' Fixed for parallel processing environment isolation.
#'
#' @inheritParams paramounter_part1
#' @param massSDrange Retained for backward compatibility; currently not used
#'   by part 2.
#' @param cutoff Retained for backward compatibility; currently not used by
#'   part 2.
#' @param ppmCut The PPM cutoff calculated from part 1.
#' @export
paramounter_part2 <- function(directory, massSDrange = 2, smooth = 0, cutoff = 0.95,
                              filenum = c(3, 5, "all"), thread = 1, ppmCut) {

  setup <- .paramounter_setup(directory, filenum, thread)
  selected_files <- setup$files
  n_chunks <- setup$n_chunks

  all_results <- do.call(rbind, lapply(selected_files, function(f) {
    msd <- MSnbase::readMSData(f, mode = "onDisk", msLevel. = 1)
    mz_list <- MSnbase::mz(msd); int_list <- MSnbase::intensity(msd); rts <- MSnbase::rtime(msd)
    n_scans <- length(mz_list)

    scan_ids <- rep(seq_len(n_scans), lengths(mz_list))
    all_mz <- unlist(mz_list, use.names = FALSE)
    all_int <- unlist(int_list, use.names = FALSE)

    if(length(all_mz) == 0) return(NULL)

    bin_width <- 0.05
    min_mz <- min(all_mz)
    bins <- floor((all_mz - min_mz) / bin_width)
    bin_indices <- split(seq_along(all_mz), bins)
    bin_names <- names(bin_indices)

    n_cuts <- min(length(bin_names), n_chunks)
    if (n_cuts <= 1) {
      chunks <- list(bin_names)
    } else {
      chunks <- split(bin_names, cut(seq_along(bin_names),
                      breaks = n_cuts, labels = FALSE))
    }

    furrr::future_map_dfr(chunks, function(chunk_bins) {
      tryCatch({
        chunk_res <- lapply(chunk_bins, function(b_name) {
          idx <- bin_indices[[b_name]]
          b_scans <- scan_ids[idx]; b_ints <- all_int[idx]

          scan_means <- tapply(b_ints, b_scans, mean)
          eic <- numeric(n_scans); eic[as.integer(names(scan_means))] <- scan_means

          eic_smooth <- .peak_smooth_opt(eic, smooth)

          eic_non0 <- sort(eic_smooth[eic_smooth > 0])
          if (length(eic_non0) == 0) return(NULL)
          cutoff_val <- max(eic_non0)
          if (length(eic_non0) > 10) {
            for (x in seq(10, length(eic_non0), 10)) {
              sub <- eic_non0[1:x]
              if (x+1 <= length(eic_non0) && eic_non0[x+1] >= (mean(sub) + 3*sd(sub))) {
                cutoff_val <- eic_non0[x]; break
              }
            }
          }

          above <- which(eic_smooth > cutoff_val)
          if (length(above) == 0) return(NULL)
          segments <- split(above, cumsum(c(1, diff(above) != 1)))

          res <- lapply(segments, function(seg) {
            apex <- seg[which.max(eic_smooth[seg])]
            width <- rts[tail(seg,1)] - rts[head(seg,1)]
            height <- eic_smooth[seg[apex]]
            sn <- if(cutoff_val > 0) height/cutoff_val else height
            data.frame(width=width, scans=length(seg), height=height, sn=sn, noise=cutoff_val)
          })
          do.call(rbind, res)
        })
        return(bind_rows(chunk_res))
      }, error = function(e) return(NULL))

    }, .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("MSnbase", "stats") # IMPORTANT
    ))
  }))

  if (is.null(all_results) || nrow(all_results) == 0) return(NULL)

  calc_stat <- function(x, prob = 0.03) {
    x <- sort(x[!is.na(x)])
    if(length(x) == 0) return(0)
    x[1:round(length(x) * (1-prob))]
  }

  widths <- calc_stat(all_results$width)
  scans_n <- calc_stat(all_results$scans)
  heights <- calc_stat(all_results$height)
  noises <- calc_stat(all_results$noise)
  sns <- calc_stat(all_results$sn)

  min_pw <- if(length(widths)>0) floor(min(widths)) else 5
  max_pw <- if(length(widths)>0) ceiling(max(widths)) else 30

  p_min <- max(1, min_pw - 2)
  p_max <- max_pw + 10

  sn_thresh <- if(length(sns)>0) max(3, floor(min(sns))) else 10
  noise_val <- if(length(noises)>0) floor(min(noises)) else 500

  res_table <- data.frame(
    para = c("ppm", "p_min", "p_max", "snthresh", "mzdiff", "integrate", "pre_left", "pre_right", "noise", "bw", "min_fraction"),
    desc = c("ppm", "peakwidth min", "peakwidth max", "signal/noise", "mzdiff", "integrate", "prefilter peaks", "prefilter int", "noise", "bw", "min_fraction"),
    Value = c(round(ppmCut, 2), p_min, p_max, sn_thresh, -0.01, 1, 3, noise_val, noise_val, 5, 0.5),
    stringsAsFactors = FALSE
  )

  return(res_table)
}

#' Internal Smoothing Function (Optimized)
#' @noRd
.peak_smooth_opt <- function(x, level) {
  n <- level
  if (n <= 0 || length(x) < 2 * n) return(x)

  y <- x
  len <- length(x)

  # Center
  for(i in 1:n) {
    w <- c((n-i+2):(n+1), n:1)
    end_idx <- min(len, i+n)
    if(end_idx >= 1) y[i] <- sum(w[1:length(x[1:end_idx])] * x[1:end_idx]) / sum(w[1:length(x[1:end_idx])])
  }

  # Middle
  for(i in (n+1):(len-n)){
    w <- c(1:(n+1), n:1)
    y[i] <- sum(w * x[(i-n):(i+n)]) / sum(w)
  }

  # End
  for(i in (len-n+1):len){
    w <- c(1:n, (n+1):(n+i-len+1))
    start_idx <- i-n
    if(start_idx <= len) {
      sub_x <- x[start_idx:len]
      w_sub <- w[1:length(sub_x)]
      y[i] <- sum(w_sub * sub_x) / sum(w_sub)
    }
  }
  return(y)
}
