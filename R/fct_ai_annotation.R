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
      model = "deepseek-v4-flash",
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

metminer_ai_provider_model_choices <- function(provider) {
  provider <- provider %||% "openai"
  choices <- list(
    openai = c(
      "GPT-5.4 mini" = "gpt-5.4-mini",
      "GPT-5.4" = "gpt-5.4",
      "GPT-5.3 Codex" = "gpt-5.3-codex",
      "GPT-4o mini (legacy)" = "gpt-4o-mini"
    ),
    gemini = c(
      "Gemini 3.0 Pro" = "gemini-3.0-pro",
      "Gemini 3.0 Flash" = "gemini-3.0-flash",
      "Gemini 2.5 Pro" = "gemini-2.5-pro",
      "Gemini 2.5 Flash" = "gemini-2.5-flash"
    ),
    deepseek = c(
      "DeepSeek V4 Flash" = "deepseek-v4-flash",
      "DeepSeek V4 Pro" = "deepseek-v4-pro",
      "DeepSeek Chat (legacy, deprecated 2026-07-24)" = "deepseek-chat",
      "DeepSeek Reasoner (legacy, deprecated 2026-07-24)" = "deepseek-reasoner"
    ),
    qwen = c(
      "Qwen Plus" = "qwen-plus",
      "Qwen Max" = "qwen-max",
      "Qwen Turbo" = "qwen-turbo"
    ),
    kimi = c(
      "Moonshot v1 8K" = "moonshot-v1-8k",
      "Moonshot v1 32K" = "moonshot-v1-32k",
      "Moonshot v1 128K" = "moonshot-v1-128k"
    ),
    grok = c(
      "Grok 2 Latest" = "grok-2-latest",
      "Grok 2 Vision Latest" = "grok-2-vision-latest"
    )
  )
  choices[[provider]] %||% choices$openai
}

metminer_ai_language_choices <- function() {
  c(
    "简体中文" = "zh-CN",
    "English" = "en",
    "日本語" = "ja",
    "한국어" = "ko",
    "Deutsch" = "de",
    "Français" = "fr",
    "Español" = "es",
    "Português" = "pt",
    "Italiano" = "it"
  )
}

metminer_ai_language_label <- function(language) {
  choices <- metminer_ai_language_choices()
  language <- language %||% "en"
  label <- names(choices)[match(language, unname(choices))]
  if (is.null(label) || length(label) == 0 || is.na(label) || !nzchar(label)) {
    return("English")
  }
  label
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
                                    temperature = 0.3,
                                    language = "en") {
  if (!isTRUE(metminer_ai_config_persistence_available())) {
    stop("Local AI configuration persistence is disabled in server environments.", call. = FALSE)
  }
  cfg <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = base_url,
    temperature = as.numeric(temperature),
    language = language %||% "en",
    os = Sys.info()[["sysname"]],
    updated_at = as.character(Sys.time())
  )
  path <- metminer_ai_config_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cfg, path)
  path
}

metminer_ai_system_prompt <- function(language = "en", mode = c("review", "chat")) {
  mode <- match.arg(mode)
  language_label <- metminer_ai_language_label(language)
  base_prompt <- c(
    "You are an LC-MS plant metabolomics annotation reviewer.",
    paste0("Write the final answer in ", language_label, ". All explanatory prose and section headings must use this language. Keep compound names, feature IDs, adducts, database IDs, DOIs, and literature references unchanged."),
    "You must judge metabolite annotation credibility from the provided evidence bundle, not from compound names alone.",
    "Interpret annotations with the MetMiner layered strategy: Layer 1 is genome-informed KEGG/PlantCyc reaction candidates; Layer 2 is spectral evidence from public MS2 libraries and optional local/custom standard libraries.",
    "For Layer 1 KEGG/PlantCyc candidates, require strict core-adduct support before treating the candidate as credible. Do not upgrade a Layer 1-only candidate beyond putative Level 3 without spectral or orthogonal evidence.",
    "Layer 2 public MS2 matches can validate Layer 1 candidates or independently supplement metabolites absent from reaction databases. Optional local/custom standard libraries can support Level 1 only when RT, precursor, and MS2 evidence are present in the bundle.",
    "Prioritize annotation level, MetMiner layered confidence level, metID adduct evidence, MS1 m/z/RT, MS2 fragments, feature-network role, recurrent-ion status, and non-redundancy audit fields.",
    "Treat metID adducts as database/adduct-dictionary annotation evidence, not as feature-network relationships.",
    "If literature_evidence is provided, use it only as contextual support for known biology, plant occurrence, metabolism, and analytical reports; do not let literature override weak spectral evidence.",
    "Never invent papers, authors, years, journals, or DOIs. You may cite only papers present in literature_evidence.papers.",
    "When a user asks for literature via @agent, @paper, @mcp, or @literature, literature claims must be based only on literature_evidence.papers. If paper search failed or returned no papers, say that no literature evidence was retrieved and do not cite or imply unsourced papers.",
    "When citing literature in the review body, use author-year style such as Wang et al., 2024. If author metadata is unavailable, cite the source and title briefly.",
    "Every cited paper must appear in a final References section. Format references as: FirstAuthor., SecondAuthor., et al., Paper title, Year, Journal/source, DOI. Use the journal field when present; otherwise use source. If DOI is missing, write DOI not available in paper-search result.",
    "Before using a DOI, verify it is exactly present in the literature_evidence entry. Do not infer or fabricate missing DOI values.",
    "A same compound name appearing at multiple RTs is suspicious unless explained by isomers or explicit chromatographic evidence.",
    "A feature marked as resolved_recurrent_isf or suspected_interference is likely a fragment/interference candidate.",
    "If evidence is insufficient, say so clearly."
  )
  mode_prompt <- if (identical(mode, "chat")) {
    c(
      "This is a follow-up chat turn. Answer the user's current question directly.",
      "Do not repeat the previous full annotation review unless the user explicitly asks for a full review.",
      "Use prior chat context only to avoid re-explaining already established points."
    )
  } else {
    c(
    "Return a concise structured review with these headings: Verdict, Confidence score 0-100, Best-supported feature, Likely interference features, Key supporting evidence, Key conflicts, Literature context, Suggested next checks, References.",
      "If no literature_evidence is provided or paper search failed, write Literature context: not used and References: not used."
    )
  }
  paste(
    c(base_prompt, mode_prompt),
    collapse = "\n"
  )
}

metminer_ai_build_review_user_prompt <- function(compound_name,
                                                 evidence_bundle,
                                                 lc_conditions = "",
                                                 user_question = "") {
  evidence_json <- metminer_ai_safe_json(evidence_bundle)
  paste0(
    "Review the LC-MS annotation credibility for compound query: ", compound_name, "\n\n",
    "User LC-MS conditions:\n", coerce_text(lc_conditions, "not provided"), "\n\n",
    "User question:\n", coerce_text(user_question, "Please judge annotation credibility."), "\n\n",
    "Evidence bundle JSON:\n", evidence_json
  )
}

#' Build cache-optimised messages for DeepSeek / OpenAI-compatible prefix caching
#'
#' DeepSeek (and most OpenAI-compatible providers) use automatic prefix
#' caching: identical token prefixes across requests are cached in the KV
#' store so subsequent requests with the same prefix are much faster and
#' cheaper.  This function structures messages so the long, static evidence
#' block sits directly after the system prompt — forming the cacheable
#' prefix — while the variable chat context and user question come last.
#'
#' @param system_prompt  System-level instruction (cached).
#' @param evidence_json  Pre-serialised evidence JSON string (cached).
#' @param compound_name  Compound name for the evidence label.
#' @param lc_conditions  LC-MS conditions text.
#' @param user_question  The new user question (NOT cached — must differ).
#' @param chat_context   Optional brief summary of prior turns (NOT cached).
#'
#' @return A list of messages ready for `metminer_ai_chat`.
#' @noRd
metminer_ai_build_cacheable_messages <- function(system_prompt,
                                                  evidence_json,
                                                  compound_name,
                                                  lc_conditions = "",
                                                  user_question = "",
                                                  chat_context = NULL,
                                                  language = "en",
                                                  mode = c("review", "chat")) {
  mode <- match.arg(mode)
  language_label <- metminer_ai_language_label(language)
  # Cacheable prefix block — identical across follow-ups
  evidence_block <- paste0(
    "Compound: ", compound_name, "\n",
    "LC-MS conditions: ", coerce_text(lc_conditions, "not provided"), "\n",
    "Evidence:\n", evidence_json
  )

  messages <- list(
    list(role = "system", content = system_prompt),
    list(role = "user",   content = evidence_block),
    list(role = "assistant", content = "Evidence loaded. I will answer based on this evidence.")
  )

  # Variable suffix — only this part changes between requests
  if (!is.null(chat_context) && nzchar(chat_context)) {
    messages[[length(messages) + 1L]] <- list(role = "user", content = chat_context)
    messages[[length(messages) + 1L]] <- list(role = "assistant", content = "Understood. What is your question?")
  }

  messages[[length(messages) + 1L]] <- list(
    role = "user",
    content = paste(
      paste0("Answer language: ", language_label, "."),
      if (identical(mode, "chat")) {
        "This is a follow-up question. Answer only this question and do not repeat the full previous review unless explicitly requested."
      } else {
        "Please review this annotation."
      },
      coerce_text(user_question, "Please review this annotation."),
      sep = "\n"
    )
  )

  messages
}

#' Extract a brief summary of the prior conversation for context reuse
#'
#' Strips long review text down to the Verdict line + confidence score;
#' keeps user questions verbatim (they are short) and assistant messages
#' are truncated to their first meaningful heading.
#'
#' @noRd
metminer_ai_summarize_chat_context <- function(chat_history, max_summary_chars = 1500) {
  if (length(chat_history) == 0) return("")
  parts <- character()
  total <- 0L
  for (msg in rev(chat_history)) {
    role <- msg$role %||% "user"
    content <- msg$content %||% ""
    if (role == "user") {
      snippet <- paste0("[User asked]: ", metminer_ai_extract_first_sentence(content))
    } else if (role == "assistant") {
      snippet <- metminer_ai_extract_verdict_summary(content)
    } else {
      next
    }
    if (!nzchar(snippet)) next
    total <- total + nchar(snippet)
    if (total > max_summary_chars) break
    parts <- c(parts, snippet)
  }
  paste(rev(parts), collapse = "\n")
}

metminer_ai_extract_first_sentence <- function(text) {
  text <- trimws(text %||% "")
  first <- sub("^([^.?!]+[.?!]).*", "\\1", text)
  if (nchar(first) < nchar(text) * 0.9 && nchar(first) > 5) return(first)
  if (nchar(text) > 500) paste0(substr(text, 1, 500), "...") else text
}

metminer_ai_extract_verdict_summary <- function(text) {
  text <- trimws(text %||% "")
  # Grab Verdict + Confidence score lines
  verdict <- regmatches(text, regexpr("Verdict\\s*\\n.*?(?=\\n\\n|Confidence)", text, perl = TRUE))
  verdict <- verdict %||% ""
  confidence <- regmatches(text, regexpr("Confidence score:\\s*\\d+/100", text, perl = TRUE))
  confidence <- confidence %||% ""
  summary <- paste(c(verdict, confidence), collapse = "\n")
  if (nzchar(trimws(summary))) return(trimws(summary))
  # Fallback: first 400 chars
  if (nchar(text) > 400) paste0(substr(text, 1, 400), "...") else text
}

metminer_ai_safe_json <- function(x) {
  sanitize <- function(obj) {
    if (is.data.frame(obj)) {
      obj[] <- lapply(obj, sanitize)
      return(obj)
    }
    if (is.list(obj) && !is.null(names(obj))) {
      return(lapply(obj, sanitize))
    }
    if (is.numeric(obj)) {
      obj[!is.finite(obj)] <- NA_real_
    }
    obj
  }
  jsonlite::toJSON(sanitize(x), auto_unbox = TRUE, pretty = TRUE, na = "null")
}

#' Trim an evidence bundle for chat follow-ups (strip heavy fields the LLM already saw)
#'
#' @noRd
metminer_ai_compact_evidence_for_chat <- function(evidence) {
  if (is.null(evidence)) return(evidence)
  out <- evidence
  # Keep feature_evidence slim: drop MS2 peak strings, keep id/mz/rt/role/status
  if (!is.null(out$feature_evidence) && nrow(out$feature_evidence) > 0) {
    fe <- out$feature_evidence
    keep_cols <- intersect(
      c("feature_id", "mode", "mz", "rt", "mean_area",
        "network_role", "parent_feature_id", "relation_to_parent",
        "recurrent_ion_group", "local_network_edge_count",
        "ms2_precursor_mz", "ms2_precursor_rt"),
      colnames(fe)
    )
    out$feature_evidence <- fe[, keep_cols, drop = FALSE]
    # Truncate edge summary and MS2 peak strings
    for (col in c("local_network_edges", "ms2_top_peaks")) {
      if (col %in% colnames(fe)) out$feature_evidence[[col]] <- NA_character_
    }
  }
  # Slim raw_annotation_candidates to essential columns
  if (!is.null(out$raw_annotation_candidates) && nrow(out$raw_annotation_candidates) > 0) {
    raw <- out$raw_annotation_candidates
    slim_cols <- intersect(
      c("variable_id", "Compound.name", "Adduct", "Level", "Total.score", "Database",
        "annotation_layer", "evidence_scope", "core_adduct_match",
        "strict_genome_adduct_pass", "metminer_confidence_level", "mode"),
      colnames(raw)
    )
    out$raw_annotation_candidates <- raw[, slim_cols, drop = FALSE]
  }
  # Trim paper abstracts shorter for chat
  if (!is.null(out$literature_evidence$papers)) {
    out$literature_evidence$papers <- lapply(out$literature_evidence$papers, function(p) {
      p$abstract <- metminer_ai_truncate_text(p$abstract, 300)
      p
    })
  }
  out
}

#' Truncate a chat message to a max character length
#'
#' @noRd
metminer_ai_trim_chat_message <- function(msg, max_chars = 3000) {
  if (is.null(msg$content) || nchar(msg$content) <= max_chars) return(msg)
  msg$content <- paste0(substr(msg$content, 1L, max_chars), "\n\n... [truncated for length]")
  msg
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
    title <- metminer_ai_first_text(paper$title, paper$name)
    authors <- metminer_ai_author_text(paper$authors %||% paper$creators %||% paper$author)
    year <- metminer_ai_first_text(
      paper$year,
      paper$published,
      paper$published_date,
      paper$publicationDate,
      paper$publication_date,
      paper$date
    )
    journal <- metminer_ai_first_text(
      paper$journal,
      paper$journal_name,
      paper$venue,
      paper$publicationVenue$name,
      paper$publicationVenue,
      paper$container_title,
      paper$containerTitle,
      paper$publisher,
      paper$categories,
      paper$source
    )
    source <- metminer_ai_first_text(paper$source, paper$database, paper$provider)
    doi <- metminer_ai_first_text(
      paper$doi,
      paper$DOI,
      paper$externalIds$DOI,
      paper$external_ids$DOI,
      paper$ids$doi
    )
    url <- metminer_ai_first_text(
      paper$url,
      paper$pdf_url,
      paper$pdfUrl,
      paper$openAccessPdf$url,
      paper$link
    )
    list(
      title = title,
      authors = authors,
      year = year,
      journal = journal,
      source = source,
      doi = doi,
      paper_id = metminer_ai_first_text(paper$paper_id, paper$paperId, paper$id, paper$pmid, paper$pmcid),
      url = url,
      abstract = metminer_ai_truncate_text(metminer_ai_first_text(paper$abstract, paper$summary), 900)
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

metminer_ai_first_text <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value) || length(value) == 0) {
      next
    }
    if (is.data.frame(value)) {
      value <- as.list(value)
    }
    if (is.list(value)) {
      if (!is.null(value$name)) {
        value <- value$name
      } else if (!is.null(value$title)) {
        value <- value$title
      } else if (!is.null(value$text)) {
        value <- value$text
      } else {
        value <- unlist(value, use.names = FALSE)
      }
    }
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(trimws(value))]
    if (length(value) > 0) {
      return(paste(unique(trimws(value)), collapse = "; "))
    }
  }
  NA_character_
}

metminer_ai_author_text <- function(authors) {
  if (is.null(authors) || length(authors) == 0) {
    return(NA_character_)
  }
  if (is.data.frame(authors)) {
    if ("name" %in% colnames(authors)) {
      authors <- authors$name
    } else {
      authors <- apply(authors, 1, function(x) paste(stats::na.omit(as.character(x)), collapse = " "))
    }
  } else if (is.list(authors)) {
    authors <- vapply(authors, function(author) {
      if (is.null(author)) {
        return("")
      }
      if (is.list(author)) {
        name <- author$name %||% author$full_name %||% author$fullName
        if (!is.null(name) && has_text(name)) {
          return(as.character(name)[1])
        }
        given <- author$given %||% author$first %||% author$firstName
        family <- author$family %||% author$last %||% author$lastName
        return(trimws(paste(given %||% "", family %||% "")))
      }
      as.character(author)[1]
    }, character(1))
  }
  authors <- as.character(authors)
  authors <- authors[!is.na(authors) & nzchar(trimws(authors))]
  if (length(authors) == 0) {
    return(NA_character_)
  }
  paste(unique(trimws(authors)), collapse = "; ")
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
  ann <- metminer_add_annotation_layer_columns(ann, mode, metminer_default_adduct_advice())
  keep_cols <- intersect(c("variable_id", "Compound.name", "Adduct", "Level", "Total.score",
                           "SS", "RT.error", "CE", "Database", "HMDB.ID", "KEGG.ID", "PlantCyc.ID",
                           "CAS.ID", "Lab.ID", "annotation_layer", "evidence_scope",
                           "core_adduct_match", "strict_genome_adduct_pass", "metminer_confidence_level"),
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
      "member_annotation_levels", "annotation_layer", "evidence_scope", "core_adduct_match",
      "strict_genome_adduct_pass", "metminer_confidence_level", "confidence_class",
      "mz", "rt", "mean_area", "n_features",
      "compound_key", "keep", "drop_reason", "recurrent_status", "suspected_interference",
      "interference_reason", "network_roles", "parent_feature_ids",
      "variable_id", "Compound.name", "Adduct", "Level", "Total.score", "Database",
      "HMDB.ID", "KEGG.ID", "PlantCyc.ID", "CAS.ID", "Lab.ID"),
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
                             timeout_sec = 300) {
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
  body <- list(
    model = model,
    messages = messages,
    temperature = as.numeric(temperature)
  )
  body_json <- tryCatch(
    jsonlite::toJSON(body, auto_unbox = TRUE, na = "null"),
    error = function(e) stop("Failed to serialise request body: ", e$message, call. = FALSE)
  )
  req <- httr2::request(base_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_raw(body_json, "application/json") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(max_tries = 2, max_seconds = timeout_sec)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      msg <- e$message %||% "unknown HTTP error"
      parent_msg <- conditionMessage(e$parent) %||% ""
      detail <- if (nzchar(parent_msg)) paste(msg, "-", parent_msg) else msg
      stop("LLM API request failed: ", detail, call. = FALSE)
    }
  )
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
