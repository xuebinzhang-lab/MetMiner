# ---- LC-MS method-aware parameter advisor helpers ----

#' Parse LC-MS method text into coarse instrument and chromatography context
#'
#' @noRd
metminer_parse_lcms_method <- function(method_text = "", instrument_type = "auto", chromatography = "auto") {
  text <- paste(method_text %||% "", collapse = "\n")
  text_lower <- tolower(text)

  instrument <- instrument_type %||% "auto"
  if (identical(instrument, "auto")) {
    instrument <- if (grepl("orbitrap|exploris|q exactive|qe\\b", text_lower, perl = TRUE)) {
      "Orbitrap"
    } else if (grepl("qtof|q-tof|tof|xevo|synapt", text_lower, perl = TRUE)) {
      "QTOF"
    } else {
      "Other"
    }
  }

  chrom <- chromatography %||% "auto"
  if (identical(chrom, "auto")) {
    chrom <- if (grepl("c18|hypersil gold|beh c18|reverse phase|reversed phase|rp-", text_lower, perl = TRUE)) {
      "C18 reversed phase"
    } else if (grepl("hilic|amide|zic", text_lower, perl = TRUE)) {
      "HILIC"
    } else {
      "Other"
    }
  }

  list(
    instrument = instrument,
    chromatography = chrom,
    has_formic_acid = grepl("formic acid|甲酸|\\bfa\\b", text_lower, perl = TRUE),
    has_acetonitrile = grepl("acetonitrile|乙腈|\\bacn\\b", text_lower, perl = TRUE),
    has_methanol = grepl("methanol|甲醇|\\bmeoh\\b", text_lower, perl = TRUE),
    has_ammonium_formate = grepl("ammonium formate|甲酸铵", text_lower, perl = TRUE),
    has_ammonium_acetate = grepl("ammonium acetate|乙酸铵", text_lower, perl = TRUE),
    has_acetate = grepl("acetate|乙酸盐|乙酸", text_lower, perl = TRUE),
    positive_mode = grepl("positive|正离子|\\+3\\.|\\+\\s*kv", text_lower, perl = TRUE),
    negative_mode = grepl("negative|负离子|-3\\.|-\\s*kv", text_lower, perl = TRUE),
    dda = grepl("data dependent|dd-ms|dda", text_lower, perl = TRUE),
    scan_range = regmatches(text, regexpr("m/z\\s*[0-9,.-]+\\s*[–-]\\s*[0-9,.-]+", text, ignore.case = TRUE, perl = TRUE))
  )
}

#' Extract project-level LC-MS condition fields from free text
#'
#' @noRd
metminer_extract_lcms_items <- function(method_text = "",
                                        instrument_type = "auto",
                                        chromatography = "auto") {
  text <- paste(method_text %||% "", collapse = "\n")
  ctx <- metminer_parse_lcms_method(text, instrument_type, chromatography)
  pick_first <- function(pattern, default = "") {
    hit <- regmatches(text, regexpr(pattern, text, ignore.case = TRUE, perl = TRUE))
    if (length(hit) == 0 || !nzchar(hit[1])) default else trimws(hit[1])
  }
  pick_line <- function(pattern, default = "") {
    lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
    hit <- lines[grepl(pattern, lines, ignore.case = TRUE, perl = TRUE)]
    if (length(hit) == 0) default else trimws(hit[1])
  }
  strip_label <- function(x) {
    x <- trimws(x %||% "")
    trimws(sub("^[^:：]{1,40}[:：]\\s*", "", x, perl = TRUE))
  }
  ion_mode <- paste(c(
    if (isTRUE(ctx$positive_mode)) "positive" else NULL,
    if (isTRUE(ctx$negative_mode)) "negative" else NULL
  ), collapse = "; ")
  if (!nzchar(ion_mode)) ion_mode <- if (grepl("positive|negative|正离子|负离子", tolower(text), perl = TRUE)) "positive; negative" else ""

  list(
    method_text = text,
    instrument = ctx$instrument,
    chromatography = ctx$chromatography,
    column = strip_label(pick_line("column|色谱柱|stationary", "")),
    mobile_phase_a = strip_label(pick_line("mobile phase a|phase a|流动相\\s*a|A相", "")),
    mobile_phase_b = strip_label(pick_line("mobile phase b|phase b|流动相\\s*b|B相", "")),
    ion_source = if (grepl("\\besi\\b|electrospray|电喷雾", text, ignore.case = TRUE, perl = TRUE)) "ESI" else "",
    ion_mode = ion_mode,
    acquisition = if (ctx$dda) "Full scan DDA MS/MS" else pick_line("dda|data dependent|full scan|ms/ms|ms2|采集", ""),
    scan_range = if (length(ctx$scan_range) > 0) ctx$scan_range[1] else pick_first("m/z\\s*[0-9,.-]+\\s*[–-]\\s*[0-9,.-]+", ""),
    collision_energy = pick_first("(NCE|nce|collision energy|碰撞能量)\\s*[:=]?\\s*[0-9,./ ;+-]+", pick_line("collision|nce|碰撞|能量", ""))
  )
}

#' Format project-level LC-MS condition fields for prompts
#'
#' @noRd
metminer_format_lcms_conditions <- function(items = list()) {
  items <- items %||% list()
  fields <- c(
    instrument = "Instrument",
    chromatography = "Chromatography",
    column = "Column/stationary phase",
    mobile_phase_a = "Mobile phase A",
    mobile_phase_b = "Mobile phase B",
    ion_source = "Ion source",
    ion_mode = "Ion mode",
    acquisition = "Acquisition",
    scan_range = "Scan range",
    collision_energy = "Collision energy"
  )
  lines <- vapply(names(fields), function(key) {
    value <- trimws(as.character(items[[key]] %||% ""))
    paste0(fields[[key]], ": ", value)
  }, character(1))
  notes <- trimws(as.character(items$method_text %||% ""))
  paste(c(lines, if (nzchar(notes)) paste0("Original method text:\n", notes) else NULL), collapse = "\n")
}

#' Build LC-MS annotation/filter parameter advice from parsed method context
#'
#' @noRd
metminer_lcms_parameter_advice <- function(method_text = "",
                                           instrument_type = "auto",
                                           chromatography = "auto") {
  ctx <- metminer_parse_lcms_method(method_text, instrument_type, chromatography)

  pos_core <- c("[M+H]+", "[M+Na]+")
  neg_core <- c("[M-H]-")
  if (isTRUE(ctx$has_formic_acid) || isTRUE(ctx$has_ammonium_formate)) {
    neg_core <- unique(c(neg_core, "[M+HCOO]-"))
  }

  pos_optional <- c("[M+K]+", "[M+H-H2O]+", "[2M+H]+", "[2M+Na]+")
  neg_optional <- c("[M+Cl]-", "[M-H2O-H]-", "[2M-H]-")
  if (isTRUE(ctx$has_ammonium_formate) || isTRUE(ctx$has_ammonium_acetate)) {
    pos_optional <- unique(c("[M+NH4]+", pos_optional))
  }
  if (isTRUE(ctx$has_ammonium_acetate) || (isTRUE(ctx$has_acetate) && !isTRUE(ctx$has_formic_acid))) {
    neg_optional <- unique(c("[M+CH3COO]-", neg_optional))
  }

  ms1_tolerance <- switch(
    ctx$instrument,
    Orbitrap = "5-10 ppm; use 5 ppm when mass calibration is stable",
    QTOF = "10-20 ppm; tighten when lock-mass calibration is stable",
    "10-20 ppm; adjust after checking mass error distribution"
  )
  ms2_tolerance <- switch(
    ctx$instrument,
    Orbitrap = "10-20 ppm or 0.01-0.02 Da for high-resolution MS2",
    QTOF = "20-30 ppm or 0.02-0.05 Da",
    "0.02-0.05 Da or instrument-specific ppm tolerance"
  )

  rows <- list(
    data.frame(
      section = "Detected method context",
      parameter = c("Instrument class", "Chromatography", "Mobile phase clue", "Acquisition"),
      recommendation = c(
        ctx$instrument,
        ctx$chromatography,
        paste(c(
          if (ctx$has_formic_acid) "formic acid" else NULL,
          if (ctx$has_acetonitrile) "acetonitrile" else NULL,
          if (ctx$has_methanol) "methanol" else NULL,
          if (ctx$has_ammonium_formate) "ammonium formate" else NULL,
          if (ctx$has_ammonium_acetate) "ammonium acetate" else NULL
        ), collapse = "; "),
        if (ctx$dda) "Full scan with data-dependent MS/MS" else "Not detected from text"
      ),
      priority = "context",
      rationale = "Parsed from the LC-MS method text.",
      caution = "Use this as a parameter advisory summary; verify against the raw method and instrument report.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Adduct search",
      parameter = c("Positive core adducts", "Positive optional adducts", "Negative core adducts", "Negative optional adducts"),
      recommendation = c(
        paste(pos_core, collapse = ", "),
        paste(pos_optional, collapse = ", "),
        paste(neg_core, collapse = ", "),
        paste(neg_optional, collapse = ", ")
      ),
      priority = c("core", "optional", "core", "optional"),
      rationale = c(
        "[M+H]+ and [M+Na]+ are common in ESI positive mode for plant metabolites under RP-LC conditions.",
        "K adducts, dehydration ions, and dimers are useful but can expand the candidate space.",
        "[M-H]- is the dominant negative-mode ion; formate adducts are plausible when formic acid/formate is present.",
        "Cl adducts, dehydration ions, acetate adducts, and dimers should be secondary evidence."
      ),
      caution = c(
        "Core adducts can be used for Level 1 candidate generation after species-database filtering.",
        "Dehydration ions are common for glycosides, alcohol-rich metabolites, terpenoids, and some lipids, but they are less specific.",
        "Treat [M+HCOO]- as lower priority than [M-H]- unless it is recurrent and supported by feature-network evidence.",
        "Do not let optional adducts dominate annotation unless RT, isotope, feature-network, or MS/MS evidence supports them."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Mass tolerance",
      parameter = c("MS1 tolerance", "MS2 tolerance"),
      recommendation = c(ms1_tolerance, ms2_tolerance),
      priority = "core",
      rationale = c(
        "High-resolution full scan data can usually use ppm-level precursor tolerance.",
        "MS2 tolerance should reflect fragment mass accuracy and centroiding behavior."
      ),
      caution = c(
        "Check empirical mass error distribution before final annotation filtering.",
        "Do not use overly broad MS2 tolerance for spectral matching."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Level 1 strategy",
      parameter = c("MS1/adduct-only candidates", "Optional adduct confidence"),
      recommendation = c(
        "Retain as candidate evidence after species-specific database filtering; do not treat as structural confirmation.",
        "Mark dehydration, dimer, K, Cl, acetate, and unusual adduct hits as secondary evidence."
      ),
      priority = c("core", "optional"),
      rationale = c(
        "A filtered species database reduces false positives, so Level 1 candidates can guide downstream review and pathway background construction.",
        "Optional adducts are useful for coverage but increase ambiguity."
      ),
      caution = c(
        "Prefer MS/MS, feature-network, isotope, RT, and blank/QC evidence for final confidence.",
        "Use optional adducts to prioritize review rather than to automatically accept annotations."
      ),
      stringsAsFactors = FALSE
    )
  )

  advice <- do.call(rbind, rows)
  rownames(advice) <- NULL
  attr(advice, "context") <- ctx
  advice
}
