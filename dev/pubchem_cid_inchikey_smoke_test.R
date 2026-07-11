#!/usr/bin/env Rscript

# Respectful PubChem PUG-REST CID -> InChIKey smoke test.
# PubChem asks programmatic clients to stay below 5 requests/second.

pubchem_fetch_cid_properties <- function(cids,
                                         properties = c("InChIKey", "CanonicalSMILES"),
                                         batch_size = 25,
                                         sleep_sec = 1.2,
                                         timeout_sec = 30,
                                         max_retries = 3,
                                         user_agent = "MetMiner/KEGG-ID-mapping smoke test (contact: local developer; compliant PUG-REST batch requests)") {
  cids <- unique(as.character(cids))
  cids <- cids[nzchar(cids) & grepl("^[0-9]+$", cids)]
  if (length(cids) == 0) {
    return(data.frame(CID = character(), InChIKey = character(), CanonicalSMILES = character()))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }
  batches <- split(cids, ceiling(seq_along(cids) / batch_size))
  rows <- vector("list", length(batches))
  status_log <- data.frame(
    batch = integer(),
    n_cid = integer(),
    status = integer(),
    elapsed_sec = numeric(),
    throttling = character(),
    error = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(batches)) {
    cid_text <- paste(batches[[i]], collapse = ",")
    prop_text <- paste(properties, collapse = ",")
    url <- paste0(
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",
      cid_text,
      "/property/",
      prop_text,
      "/JSON"
    )

    batch_error <- ""
    batch_status <- NA_integer_
    batch_elapsed <- NA_real_
    batch_throttling <- ""
    parsed <- NULL
    for (attempt in seq_len(max_retries)) {
      started <- proc.time()[["elapsed"]]
      resp <- tryCatch({
        httr2::request(url) |>
          httr2::req_user_agent(user_agent) |>
          httr2::req_timeout(timeout_sec) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
      }, error = function(e) {
        batch_error <<- e$message
        NULL
      })
      batch_elapsed <- proc.time()[["elapsed"]] - started
      if (!is.null(resp)) {
        batch_status <- httr2::resp_status(resp)
        batch_throttling <- httr2::resp_headers(resp)[["x-throttling-control"]] %||% ""
        if (batch_status >= 200 && batch_status < 300) {
          parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)
          batch_error <- ""
          break
        }
        batch_error <- httr2::resp_body_string(resp)
      }
      Sys.sleep(min(30, sleep_sec * attempt * 2))
    }

    status_log <- rbind(
      status_log,
      data.frame(
        batch = i,
        n_cid = length(batches[[i]]),
        status = batch_status,
        elapsed_sec = batch_elapsed,
        throttling = batch_throttling,
        error = batch_error,
        stringsAsFactors = FALSE
      )
    )
    if (!is.null(parsed) && !is.null(parsed$PropertyTable$Properties)) {
      rows[[i]] <- as.data.frame(parsed$PropertyTable$Properties, stringsAsFactors = FALSE)
    }
    Sys.sleep(sleep_sec)
  }

  result <- if (length(Filter(Negate(is.null), rows)) > 0) {
    do.call(rbind, Filter(Negate(is.null), rows))
  } else {
    data.frame()
  }
  list(result = result, status_log = status_log)
}

if (identical(environment(), globalenv())) {
  cids <- 1:100
  out <- pubchem_fetch_cid_properties(cids)
  print(out$status_log)
  cat("\nRows returned:", nrow(out$result), "\n")
  cat("InChIKey filled:", sum(!is.na(out$result$InChIKey) & nzchar(out$result$InChIKey)), "\n")
  smiles_col <- intersect(c("CanonicalSMILES", "ConnectivitySMILES", "IsomericSMILES"), colnames(out$result))[1]
  if (!is.na(smiles_col)) {
    cat(smiles_col, "filled:", sum(!is.na(out$result[[smiles_col]]) & nzchar(out$result[[smiles_col]])), "\n")
  }
  cat("\nPreview:\n")
  print(utils::head(out$result, 10))
  if (any(out$status_log$status %in% c(403L, 429L), na.rm = TRUE)) {
    quit(status = 2)
  }
}
