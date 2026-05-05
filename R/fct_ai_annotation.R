# ---- AI-assisted annotation review helpers ----

metminer_ai_provider_defaults <- function(provider) {
  provider <- provider %||% "openai"
  defaults <- list(
    openai = list(
      model = "gpt-4o-mini",
      base_url = "https://api.openai.com/v1/chat/completions",
      api_style = "openai"
    ),
    gemini = list(
      model = "gemini-1.5-flash",
      base_url = "https://generativelanguage.googleapis.com/v1beta",
      api_style = "gemini"
    ),
    deepseek = list(
      model = "deepseek-chat",
      base_url = "https://api.deepseek.com/chat/completions",
      api_style = "openai"
    ),
    qwen = list(
      model = "qwen-plus",
      base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions",
      api_style = "openai"
    ),
    kimi = list(
      model = "moonshot-v1-8k",
      base_url = "https://api.moonshot.cn/v1/chat/completions",
      api_style = "openai"
    ),
    grok = list(
      model = "grok-2-latest",
      base_url = "https://api.x.ai/v1/chat/completions",
      api_style = "openai"
    )
  )
  defaults[[provider]] %||% defaults$openai
}

metminer_ai_paper_sources <- function() {
  c(
    "PubMed" = "pubmed",
    "PubMed Central (PMC)" = "pmc",
    "Europe PMC" = "europepmc",
    "bioRxiv" = "biorxiv",
    "medRxiv" = "medrxiv",
    "Semantic Scholar" = "semantic",
    "Crossref" = "crossref",
    "CORE (optional API key recommended)" = "core",
    "arXiv (optional, broad preprints)" = "arxiv",
    "Google Scholar (unstable, proxy recommended)" = "google_scholar"
  )
}

metminer_ai_default_paper_sources <- function() {
  c("pubmed", "pmc", "europepmc", "biorxiv", "medrxiv", "semantic", "crossref")
}

metminer_ai_config_persistence_available <- function() {
  server_env_vars <- c(
    "SHINY_SERVER_VERSION",
    "SHINY_PORT",
    "POSIT_CONNECT_VERSION",
    "RSTUDIO_SERVER",
    "RS_SERVER_URL"
  )
  server_env <- any(nzchar(Sys.getenv(server_env_vars, unset = "")))
  !isTRUE(server_env)
}

metminer_ai_config_path <- function() {
  file.path(tools::R_user_dir("MetMiner", "config"), "ai_llm_config.rds")
}

metminer_ai_load_config <- function() {
  if (!isTRUE(metminer_ai_config_persistence_available())) {
    return(NULL)
  }
  path <- metminer_ai_config_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  cfg <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(cfg)) {
    return(NULL)
  }
  cfg
}

metminer_ai_save_config <- function(provider,
                                    model,
                                    api_key,
                                    base_url,
                                    temperature = 0.3) {
  if (!isTRUE(metminer_ai_config_persistence_available())) {
    stop("Local AI configuration persistence is disabled in server environments.", call. = FALSE)
  }
  cfg <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = base_url,
    temperature = as.numeric(temperature),
    os = Sys.info()[["sysname"]],
    updated_at = as.character(Sys.time())
  )
  path <- metminer_ai_config_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cfg, path)
  path
}

metminer_ai_system_prompt <- function() {
  paste(
    "You are an LC-MS plant metabolomics annotation reviewer.",
    "You must judge metabolite annotation credibility from the provided evidence bundle, not from compound names alone.",
    "Prioritize annotation level, metID adduct evidence, MS1 m/z/RT, MS2 fragments, feature-network role, recurrent-ion status, and non-redundancy audit fields.",
    "Treat metID adducts as database/adduct-dictionary annotation evidence, not as feature-network relationships.",
    "If literature_evidence is provided, use it only as contextual support for known biology, plant occurrence, metabolism, and analytical reports; do not let literature override weak spectral evidence.",
    "Never invent papers, authors, years, journals, or DOIs. You may cite only papers present in literature_evidence.papers.",
    "When citing literature in the review body, use author-year style such as Wang et al., 2024. If author metadata is unavailable, cite the source and title briefly.",
    "Every cited paper must appear in a final References section. Format references as: FirstAuthor., SecondAuthor., et al., Paper title, Year, Journal/source, DOI. If DOI is missing, write DOI not available in paper-search result.",
    "Before using a DOI, verify it is exactly present in the literature_evidence entry. Do not infer or fabricate missing DOI values.",
    "A same compound name appearing at multiple RTs is suspicious unless explained by isomers or explicit chromatographic evidence.",
    "A feature marked as resolved_recurrent_isf or suspected_interference is likely a fragment/interference candidate.",
    "If evidence is insufficient, say so clearly.",
    "Return a concise structured review with these headings: Verdict, Confidence score 0-100, Best-supported feature, Likely interference features, Key supporting evidence, Key conflicts, Literature context, Suggested next checks, References.",
    "If no literature_evidence is provided or paper search failed, write Literature context: not used and References: not used.",
    sep = "\n"
  )
}

metminer_ai_build_review_user_prompt <- function(compound_name,
                                                 evidence_bundle,
                                                 lc_conditions = "",
                                                 user_question = "") {
  evidence_json <- jsonlite::toJSON(evidence_bundle, auto_unbox = TRUE, pretty = TRUE, na = "null")
  paste0(
    "Review the LC-MS annotation credibility for compound query: ", compound_name, "\n\n",
    "User LC-MS conditions:\n", coerce_text(lc_conditions, "not provided"), "\n\n",
    "User question:\n", coerce_text(user_question, "Please judge annotation credibility."), "\n\n",
    "Evidence bundle JSON:\n", evidence_json
  )
}

metminer_ai_find_paper_search_cli <- function(cli = NULL) {
  candidates <- unique(c(
    trimws(cli %||% ""),
    trimws(Sys.getenv("PAPER_SEARCH_MCP_CLI", "")),
    file.path(path.expand("~"), ".local", "bin", "paper-search"),
    "paper-search"
  ))
  candidates <- candidates[nzchar(candidates)]
  for (candidate in candidates) {
    if (file.exists(candidate) && file.access(candidate, mode = 1) == 0) {
      return(candidate)
    }
    found <- Sys.which(candidate)
    if (nzchar(found)) {
      return(unname(found))
    }
  }
  NA_character_
}

metminer_ai_paper_search_available <- function(cli = NULL) {
  has_text(metminer_ai_find_paper_search_cli(cli))
}

metminer_ai_search_papers <- function(query,
                                      sources = metminer_ai_default_paper_sources(),
                                      max_results_per_source = 3,
                                      cli = NULL) {
  query <- trimws(query %||% "")
  if (!nzchar(query)) {
    stop("Paper search query is empty.", call. = FALSE)
  }
  sources <- intersect(sources, unname(metminer_ai_paper_sources()))
  if (length(sources) == 0) {
    stop("No valid paper-search sources selected.", call. = FALSE)
  }
  command <- metminer_ai_find_paper_search_cli(cli)
  if (!has_text(command)) {
    stop("paper-search CLI was not found. Install paper-search-mcp and make the paper-search command available, or set PAPER_SEARCH_MCP_CLI.", call. = FALSE)
  }

  stderr_file <- tempfile("paper-search-stderr-")
  on.exit(unlink(stderr_file), add = TRUE)
  args <- c(
    "search",
    query,
    "-n", as.character(max(1L, as.integer(max_results_per_source))),
    "-s", paste(sources, collapse = ",")
  )
  output <- tryCatch(
    system2(command, args = shQuote(args), stdout = TRUE, stderr = stderr_file),
    error = function(e) structure(character(), status = 1L, error = e$message)
  )
  status <- attr(output, "status") %||% 0L
  stderr <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!identical(as.integer(status), 0L)) {
    detail <- attr(output, "error") %||% stderr %||% paste(output, collapse = "\n")
    stop(paste("paper-search failed:", detail), call. = FALSE)
  }
  json_text <- metminer_ai_extract_json_object(paste(output, collapse = "\n"))
  parsed <- tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = FALSE),
    error = function(e) stop(paste("paper-search returned invalid JSON:", e$message), call. = FALSE)
  )
  metminer_ai_compact_paper_search(parsed)
}

metminer_ai_extract_json_object <- function(x) {
  x <- paste(x, collapse = "\n")
  start <- regexpr("\\{", x)
  end_matches <- gregexpr("\\}", x)[[1]]
  if (start[1] < 0 || end_matches[1] < 0) {
    return(x)
  }
  substr(x, start[1], max(end_matches))
}

metminer_ai_compact_paper_search <- function(result, max_papers = 20) {
  papers <- result$papers %||% list()
  compact <- lapply(utils::head(papers, max_papers), function(paper) {
    list(
      title = paper$title %||% NA_character_,
      authors = paper$authors %||% NA_character_,
      year = paper$year %||% paper$published %||% paper$published_date %||% NA_character_,
      journal = paper$journal %||% paper$venue %||% paper$container_title %||% paper$categories %||% paper$source %||% NA_character_,
      source = paper$source %||% NA_character_,
      doi = paper$doi %||% NA_character_,
      paper_id = paper$paper_id %||% NA_character_,
      url = paper$url %||% paper$pdf_url %||% NA_character_,
      abstract = metminer_ai_truncate_text(paper$abstract %||% paper$summary %||% NA_character_, 900)
    )
  })
  list(
    status = "ok",
    query = result$query %||% NA_character_,
    sources_used = result$sources_used %||% character(),
    source_results = result$source_results %||% list(),
    errors = result$errors %||% list(),
    total = result$total %||% length(compact),
    papers = compact
  )
}

metminer_ai_truncate_text <- function(x, max_chars = 900) {
  x <- coerce_text(x, "")
  if (!nzchar(x) || nchar(x) <= max_chars) {
    return(x)
  }
  paste0(substr(x, 1L, max_chars), "...")
}

metminer_ai_collect_evidence <- function(compound_name,
                                         positive_object = NULL,
                                         negative_object = NULL,
                                         filter_result = NULL,
                                         max_features = 12,
                                         ms2_top_n = 12,
                                         paper_search = NULL) {
  compound_name <- trimws(compound_name %||% "")
  if (!nzchar(compound_name)) {
    stop("Please provide a compound name or feature id.", call. = FALSE)
  }

  filter_tables <- metminer_ai_filter_rows(compound_name, filter_result)
  feature_ids <- unique(unlist(strsplit(paste(filter_tables$all$member_features, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
  feature_ids <- trimws(feature_ids[has_text(feature_ids)])

  raw_rows <- dplyr::bind_rows(
    metminer_ai_annotation_rows(compound_name, positive_object, "positive"),
    metminer_ai_annotation_rows(compound_name, negative_object, "negative")
  )
  feature_ids <- unique(c(feature_ids, raw_rows$variable_id[has_text(raw_rows$variable_id)]))
  if (length(feature_ids) == 0 && grepl("^M[0-9]", compound_name)) {
    feature_ids <- compound_name
  }
  feature_ids <- utils::head(feature_ids, max_features)

  feature_rows <- dplyr::bind_rows(
    metminer_ai_feature_rows(feature_ids, positive_object, "positive", ms2_top_n = ms2_top_n),
    metminer_ai_feature_rows(feature_ids, negative_object, "negative", ms2_top_n = ms2_top_n)
  )

  list(
    query = compound_name,
    nonredundant_rows = metminer_ai_compact_table(filter_tables$final),
    redundancy_audit_rows = metminer_ai_compact_table(filter_tables$all),
    raw_annotation_candidates = metminer_ai_compact_table(raw_rows),
    feature_evidence = feature_rows,
    literature_evidence = paper_search %||% list(status = "not_requested"),
    feature_count = length(unique(feature_rows$feature_id)),
    generated_at = as.character(Sys.time())
  )
}

metminer_ai_filter_rows <- function(compound_name, filter_result = NULL) {
  empty <- data.frame()
  if (is.null(filter_result) || is.null(filter_result$redundancy_table)) {
    return(list(final = empty, all = empty))
  }
  audit <- as.data.frame(filter_result$redundancy_table, stringsAsFactors = FALSE)
  final <- as.data.frame(filter_result$final_table %||% audit[audit$keep, , drop = FALSE], stringsAsFactors = FALSE)
  hit_audit <- metminer_ai_match_compound_rows(audit, compound_name)
  hit_final <- metminer_ai_match_compound_rows(final, compound_name)
  list(final = hit_final, all = hit_audit)
}

metminer_ai_match_compound_rows <- function(tbl, query) {
  if (is.null(tbl) || nrow(tbl) == 0) return(data.frame())
  query <- trimws(query)
  query_lower <- tolower(query)
  mask <- rep(FALSE, nrow(tbl))
  for (col in intersect(c("compound_name", "representative_feature", "member_features", "metabolite_id", "compound_key"), colnames(tbl))) {
    mask <- mask | grepl(query_lower, tolower(as.character(tbl[[col]])), fixed = TRUE)
  }
  tbl[mask, , drop = FALSE]
}

metminer_ai_annotation_rows <- function(compound_name, object, mode) {
  if (is.null(object)) return(data.frame())
  ann <- metminer_safe_extract_annotation_table(object)
  if (nrow(ann) == 0) return(data.frame())
  ann <- as.data.frame(ann, stringsAsFactors = FALSE)
  if (!"variable_id" %in% colnames(ann)) return(data.frame())
  query_lower <- tolower(compound_name)
  mask <- rep(FALSE, nrow(ann))
  for (col in intersect(c("Compound.name", "compound_name", "HMDB.ID", "KEGG.ID", "CAS.ID", "Lab.ID", "variable_id"), colnames(ann))) {
    mask <- mask | grepl(query_lower, tolower(as.character(ann[[col]])), fixed = TRUE)
  }
  ann <- ann[mask, , drop = FALSE]
  if (nrow(ann) == 0) return(data.frame())
  keep_cols <- intersect(c("variable_id", "Compound.name", "Adduct", "Level", "Total.score",
                           "SS", "RT.error", "CE", "Database", "HMDB.ID", "KEGG.ID", "CAS.ID", "Lab.ID"),
                         colnames(ann))
  out <- ann[, keep_cols, drop = FALSE]
  out$mode <- mode
  out
}

metminer_ai_feature_rows <- function(feature_ids, object, mode, ms2_top_n = 12) {
  if (is.null(object) || length(feature_ids) == 0) return(data.frame())
  variable_info <- metminer_safe_extract_variable_info(object)
  expression_data <- metminer_safe_extract_expression_data(object)
  if (!"variable_id" %in% colnames(variable_info)) return(data.frame())
  variable_info$variable_id <- as.character(variable_info$variable_id)
  local_ids <- intersect(feature_ids, variable_info$variable_id)
  if (length(local_ids) == 0) return(data.frame())

  mean_area <- rep(NA_real_, length(local_ids))
  names(mean_area) <- local_ids
  if (!is.null(expression_data) && nrow(expression_data) > 0) {
    expr <- as.matrix(expression_data)
    area <- rowMeans(expr, na.rm = TRUE)
    mean_area <- as.numeric(area[match(local_ids, names(area))])
  }

  info <- variable_info[match(local_ids, variable_info$variable_id), , drop = FALSE]
  roles <- tryCatch(extract_feature_network_roles(object), error = function(e) empty_feature_network_roles())
  recurrent <- tryCatch(extract_recurrent_ion_network(object), error = function(e) empty_recurrent_ion_network())
  network <- tryCatch(extract_feature_network(object), error = function(e) empty_feature_network())

  rows <- lapply(seq_along(local_ids), function(i) {
    fid <- local_ids[i]
    role <- roles[roles$feature_id == fid, , drop = FALSE]
    rec <- recurrent$nodes[recurrent$nodes$feature_id == fid, , drop = FALSE]
    edges <- network[network$from == fid | network$to == fid, , drop = FALSE]
    ms2_summary <- metminer_ai_ms2_summary(object, fid, info$mz[i], info$rt[i], top_n = ms2_top_n)
    data.frame(
      feature_id = fid,
      mode = mode,
      mz = suppressWarnings(as.numeric(info$mz[i])),
      rt = suppressWarnings(as.numeric(info$rt[i])),
      mean_area = mean_area[i],
      network_role = if (nrow(role) > 0) metminer_collapse_unique_text(role$network_role) else NA_character_,
      parent_feature_id = if (nrow(role) > 0) metminer_collapse_unique_text(role$parent_feature_id) else NA_character_,
      relation_to_parent = if (nrow(role) > 0) metminer_collapse_unique_text(role$relation_to_parent) else NA_character_,
      recurrent_ion_group = if (nrow(rec) > 0) metminer_collapse_unique_text(rec$ion_group_id) else NA_character_,
      local_network_edge_count = nrow(edges),
      local_network_edges = metminer_ai_edge_summary(edges),
      ms2_precursor_mz = ms2_summary$precursor_mz,
      ms2_precursor_rt = ms2_summary$precursor_rt,
      ms2_top_peaks = ms2_summary$top_peaks,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

metminer_ai_edge_summary <- function(edges, max_edges = 8) {
  edges <- normalize_feature_network(edges)
  if (nrow(edges) == 0) return(NA_character_)
  edges <- utils::head(edges[order(edges$confidence, decreasing = TRUE), , drop = FALSE], max_edges)
  paste(sprintf("%s->%s %s %s score=%.2f r=%.2f",
                edges$from, edges$to, edges$type, edges$annotation,
                edges$confidence, edges$abundance_cor),
        collapse = " | ")
}

metminer_ai_ms2_summary <- function(object, feature_id, mz = NA_real_, rt = NA_real_, top_n = 12) {
  out <- list(precursor_mz = mz, precursor_rt = rt, top_peaks = NA_character_)
  ms2_data <- tryCatch(
    make_ms2_spectrum_data(
      object = object,
      feature_id = feature_id,
      mz_tol = 0.02,
      ms2_mz_tol_ppm = 10,
      ms2_rt_tol = 30,
      top_n = top_n
    ),
    error = function(e) NULL
  )
  if (is.null(ms2_data) || is.null(ms2_data$peaks) || nrow(ms2_data$peaks) == 0) {
    return(out)
  }
  peaks <- ms2_data$peaks
  peaks <- peaks[order(peaks$relative_intensity, decreasing = TRUE), , drop = FALSE]
  peaks <- utils::head(peaks, top_n)
  ann <- if ("fragment_annotation" %in% colnames(peaks)) peaks$fragment_annotation else ""
  out$precursor_mz <- ms2_data$precursor_mz
  out$precursor_rt <- ms2_data$precursor_rt
  out$top_peaks <- paste(sprintf("%.5f:%.1f%%:%s", peaks$mz, peaks$relative_intensity, coerce_text(ann, "-")), collapse = "; ")
  out
}

metminer_ai_compact_table <- function(tbl, max_rows = 20) {
  if (is.null(tbl) || nrow(tbl) == 0) return(data.frame())
  keep_cols <- intersect(
    c("metabolite_id", "record_type", "mode", "representative_feature", "member_features",
      "compound_name", "annotation_level", "representative_adduct", "member_adducts",
      "member_annotation_levels", "confidence_class", "mz", "rt", "mean_area", "n_features",
      "compound_key", "keep", "drop_reason", "recurrent_status", "suspected_interference",
      "interference_reason", "network_roles", "parent_feature_ids",
      "variable_id", "Compound.name", "Adduct", "Level", "Total.score", "HMDB.ID", "KEGG.ID", "CAS.ID", "Lab.ID"),
    colnames(tbl)
  )
  utils::head(tbl[, keep_cols, drop = FALSE], max_rows)
}

metminer_ai_chat <- function(provider,
                             model,
                             api_key,
                             messages,
                             temperature = 0.3,
                             base_url = NULL,
                             timeout_sec = 120) {
  provider <- provider %||% "openai"
  defaults <- metminer_ai_provider_defaults(provider)
  model <- trimws(model %||% defaults$model)
  base_url <- trimws(base_url %||% defaults$base_url)
  api_style <- defaults$api_style
  if (!has_text(api_key)) {
    stop("API key is required.", call. = FALSE)
  }
  if (identical(api_style, "gemini")) {
    return(metminer_ai_chat_gemini(model, api_key, messages, temperature, base_url, timeout_sec))
  }
  metminer_ai_chat_openai_compatible(model, api_key, messages, temperature, base_url, timeout_sec)
}

metminer_ai_chat_openai_compatible <- function(model, api_key, messages, temperature, base_url, timeout_sec) {
  req <- httr2::request(base_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = messages,
      temperature = as.numeric(temperature)
    ), auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_sec)
  resp <- httr2::req_perform(req)
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- parsed$choices[[1]]$message$content %||% ""
  if (!has_text(content)) stop("LLM response was empty.", call. = FALSE)
  content
}

metminer_ai_chat_gemini <- function(model, api_key, messages, temperature, base_url, timeout_sec) {
  endpoint <- paste0(sub("/$", "", base_url), "/models/", model, ":generateContent")
  system_text <- paste(vapply(messages[vapply(messages, `[[`, character(1), "role") == "system"], `[[`, character(1), "content"), collapse = "\n")
  user_text <- paste(vapply(messages[vapply(messages, `[[`, character(1), "role") != "system"], `[[`, character(1), "content"), collapse = "\n\n")
  body <- list(
    systemInstruction = list(parts = list(list(text = system_text))),
    contents = list(list(role = "user", parts = list(list(text = user_text)))),
    generationConfig = list(temperature = as.numeric(temperature))
  )
  req <- httr2::request(endpoint) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_sec)
  resp <- httr2::req_perform(req)
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- parsed$candidates[[1]]$content$parts[[1]]$text %||% ""
  if (!has_text(content)) stop("Gemini response was empty.", call. = FALSE)
  content
}

metminer_ai_test_connection <- function(provider, model, api_key, base_url = NULL) {
  messages <- list(
    list(role = "system", content = "You are a connectivity test. Reply with OK only."),
    list(role = "user", content = "Reply OK.")
  )
  metminer_ai_chat(provider, model, api_key, messages, temperature = 0, base_url = base_url, timeout_sec = 45)
}
