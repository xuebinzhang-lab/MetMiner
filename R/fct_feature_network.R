#' Feature relationship network helpers
#'
#' Store, extract, and build a feature relationship network for a
#' `mass_dataset` object. The network is stored in `object@other_files` so the
#' implementation stays compatible with the upstream `massdataset` S4 class.
#'
#' @param object A `mass_dataset` object.
#' @param feature_network A data frame with edge columns.
#'
#' @return `set_feature_network()` returns the updated object.
#' `extract_feature_network()` returns an edge table.
#' @export
set_feature_network <- function(object, feature_network) {
  check_mass_dataset_object(object)
  feature_network <- normalize_feature_network(feature_network)
  object@other_files$feature_network <- feature_network
  object
}

#' @rdname set_feature_network
#' @export
extract_feature_network <- function(object) {
  check_mass_dataset_object(object)
  network <- object@other_files$feature_network
  if (is.null(network)) {
    return(empty_feature_network())
  }
  normalize_feature_network(network)
}

#' Merge positive and negative mode feature networks
#'
#' Builds a final cross-polarity network by prefixing within-mode feature ids
#' with `pos::` and `neg::`, then adding cross-polarity edges for features that
#' likely represent the same neutral compound or a cross-polarity in-source
#' fragment relationship.
#'
#' @param positive_object Optional positive-mode `mass_dataset`.
#' @param negative_object Optional negative-mode `mass_dataset`.
#' @param ppm Neutral mass tolerance in ppm.
#' @param rt_tolerance Retention time tolerance in seconds.
#' @param cor_cutoff Minimum abundance correlation across shared samples. Set
#'   to `NULL` to skip this filter.
#' @param neutral_loss_table Optional neutral loss dictionary with columns
#'   `annotation` and `mass`.
#' @param include_within_mode Include stored within-mode networks.
#'
#' @return A normalized feature network with polarity-prefixed feature ids.
#' @export
merge_polarity_feature_networks <- function(positive_object = NULL,
                                            negative_object = NULL,
                                            ppm = 10,
                                            rt_tolerance = 5,
                                            cor_cutoff = 0.7,
                                            neutral_loss_table = NULL,
                                            include_within_mode = TRUE) {
  edges <- list()

  if (isTRUE(include_within_mode) && !is.null(positive_object)) {
    check_mass_dataset_object(positive_object)
    edges$positive <- prefix_feature_network_ids(extract_feature_network(positive_object), "pos")
  }
  if (isTRUE(include_within_mode) && !is.null(negative_object)) {
    check_mass_dataset_object(negative_object)
    edges$negative <- prefix_feature_network_ids(extract_feature_network(negative_object), "neg")
  }
  if (!is.null(positive_object) && !is.null(negative_object)) {
    edges$cross <- detect_cross_polarity_edges(
      positive_object = positive_object,
      negative_object = negative_object,
      ppm = ppm,
      rt_tolerance = rt_tolerance,
      cor_cutoff = cor_cutoff,
      neutral_loss_table = neutral_loss_table
    )
  }

  normalize_feature_network(do.call(rbind, edges))
}

#' Convert a feature network edge table to an igraph object
#'
#' @param object A `mass_dataset` object.
#' @param feature_network Optional edge table. If `NULL`, the network stored in
#'   `object` is used.
#'
#' @return An `igraph` object.
#' @export
as_feature_igraph <- function(object, feature_network = NULL) {
  check_mass_dataset_object(object)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required to build a graph object.", call. = FALSE)
  }

  variable_info <- massdataset::extract_variable_info(object)
  if (!"variable_id" %in% colnames(variable_info)) {
    stop("variable_info must contain a 'variable_id' column.", call. = FALSE)
  }

  if (is.null(feature_network)) {
    feature_network <- extract_feature_network(object)
  } else {
    feature_network <- normalize_feature_network(feature_network)
  }

  vertices <- variable_info
  vertices$name <- as.character(vertices$variable_id)
  igraph::graph_from_data_frame(feature_network, directed = TRUE, vertices = vertices)
}

#' Plant-focused neutral loss and fragment ion dictionaries
#'
#' @return A data frame.
#' @export
default_plant_neutral_loss_table <- function() {
  data.frame(
    annotation = c(
      "H2O",
      "2H2O",
      "NH3",
      "CO",
      "CO2",
      "CH2O2",
      "CH4O",
      "C2H4O2",
      "SO3",
      "H2SO4",
      "H3PO4",
      "HPO3",
      "Hexose",
      "Pentose",
      "Deoxyhexose",
      "Hexuronic acid",
      "HexNAc",
      "Acetylhexose",
      "Dihexose",
      "Hexose+Pentose",
      "Rutinoside/Neohesperidose",
      "Malonylhexose",
      "Galloylhexose",
      "Caffeoylhexose",
      "p-Coumaroylhexose",
      "Feruloylhexose",
      "Sinapoylhexose",
      "Malonyl",
      "Acetyl",
      "Galloyl",
      "Caffeoyl",
      "p-Coumaroyl",
      "Feruloyl",
      "Sinapoyl",
      "Quinic acid",
      "Quinic acid-H2O/Shikimic acid",
      "C-glycoside cross-ring 120",
      "C-glycoside cross-ring 90",
      "C-glycoside cross-ring 60",
      "Glycine",
      "Taurine",
      "Trimethylamine",
      "Phosphocholine headgroup",
      "FA ketene C16:0",
      "FA C16:0",
      "FA C18:2",
      "FA C18:1",
      "FA C18:0"
    ),
    mass = c(
      18.010565,
      36.021130,
      17.026549,
      27.994915,
      43.989829,
      46.005479,
      32.026215,
      60.021129,
      79.956815,
      97.967380,
      97.976896,
      79.966331,
      162.052824,
      132.042259,
      146.057909,
      176.032088,
      203.079373,
      204.063388,
      324.105648,
      294.095083,
      308.110732,
      248.053218,
      314.063783,
      324.084518,
      308.089603,
      338.100168,
      368.110732,
      86.000394,
      42.010565,
      152.010959,
      162.031694,
      146.036779,
      176.047344,
      206.057909,
      192.063388,
      174.052824,
      120.042259,
      90.031694,
      60.021129,
      75.032028,
      125.014664,
      59.073499,
      183.066044,
      238.229666,
      256.240230,
      280.240230,
      282.255880,
      284.271530
    ),
    class_hint = c(
      "hydroxyl-rich compounds; sugars; phenolics; saponins",
      "hydroxyl-rich compounds; saponins",
      "amines; amino acids; alkaloids",
      "phenolics; flavonoids; carbonyl compounds",
      "organic acids; phenolic acids; fatty acids",
      "formate adducts; organic acids",
      "methoxy compounds; methyl esters",
      "acetylated metabolites; acetate adducts",
      "sulfated metabolites",
      "sulfated metabolites",
      "phosphorylated metabolites; phospholipids",
      "phosphorylated metabolites",
      "O-glycosides; flavonoids; phenylpropanoids; saponins",
      "O-glycosides; arabinosides; xylosides",
      "O-glycosides; rhamnosides; fucosides",
      "glucuronides; saponins",
      "glycans; glycosides",
      "acetylated glycosides",
      "dihexosides; sophorosides",
      "mixed pentosyl-hexosyl glycosides",
      "flavonoid rutinosides; neohesperidosides",
      "malonylated glycosides",
      "galloylated glycosides; hydrolysable tannins",
      "caffeoyl glycosides; phenylpropanoid glycosides",
      "coumaroyl glycosides; phenylpropanoid glycosides",
      "feruloyl glycosides; phenylpropanoid glycosides",
      "sinapoyl glycosides; phenylpropanoid glycosides",
      "malonylated glycosides; acylated metabolites",
      "acetylated metabolites",
      "gallotannins; galloylated phenolics",
      "caffeoyl esters; caffeoylquinic acids",
      "p-coumaroyl esters",
      "feruloyl esters",
      "sinapoyl esters; hydroxycinnamates",
      "quinic acid esters; chlorogenic acids",
      "chlorogenic acids; shikimates",
      "flavonoid C-glycosides",
      "flavonoid C-glycosides",
      "flavonoid C-glycosides",
      "glycine conjugates",
      "taurine conjugates",
      "choline/carnitine/quaternary amines",
      "PC; SM lipids",
      "lipids",
      "lipids",
      "linoleic acid-containing lipids",
      "oleic acid-containing lipids",
      "stearic acid-containing lipids"
    ),
    specificity = c(
      "low", "low", "medium", "low", "medium", "low", "medium", "medium",
      "high", "high", "high", "high", "medium", "medium", "medium", "high",
      "medium", "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "high", "medium",
      "medium", "medium", "medium", "medium"
    ),
    confidence = c(
      "low", "low", "medium", "low", "medium", "low", "medium", "medium",
      "high", "high", "high", "medium", "high", "high", "high", "high",
      "medium", "medium", "high", "medium", "high", "high", "medium",
      "medium", "medium", "medium", "medium", "high", "medium", "high",
      "medium", "medium", "medium", "medium", "medium", "medium", "high",
      "high", "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium"
    ),
    use_in_isf = c(
      FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname default_plant_neutral_loss_table
#' @export
default_plant_fragment_ion_table <- function() {
  data.frame(
    fragment_mz = c(
      96.960103, 79.956815, 78.959053, 96.969619, 98.984744,
      59.013853, 44.998201, 191.019728, 179.034982, 135.045153,
      193.050632, 163.039522, 169.013702, 125.023867,
      301.034828, 285.039914, 269.045000, 315.050478,
      271.060650, 287.055565, 317.030000, 303.050500,
      184.073321, 104.107539, 86.096426, 60.080775,
      136.061772, 152.056686, 112.050538, 126.066188,
      70.065125, 84.080775, 110.071275, 120.080775,
      136.075690, 159.091674, 255.233000, 279.233000,
      281.248600, 277.217300, 241.011880, 153.019000
    ),
    assignment = c(
      "HSO4-", "SO3-", "PO3-", "H2PO4-", "phosphate-related product",
      "CH3COO-", "HCOO-", "quinic acid related ion",
      "caffeic acid related ion", "decarboxylated caffeic acid ion",
      "ferulic acid related ion", "p-coumaric acid related ion",
      "gallate/gallic acid ion", "decarboxylated gallate ion",
      "quercetin aglycone [Y0-H]-",
      "kaempferol/luteolin aglycone [Y0-H]-",
      "apigenin/genistein aglycone [Y0-H]-",
      "isorhamnetin aglycone [Y0-H]-",
      "naringenin aglycone [Y0-H]-",
      "eriodictyol/cyanidin-related aglycone",
      "myricetin aglycone [Y0-H]-",
      "taxifolin/dihydroquercetin aglycone [Y0-H]-",
      "phosphocholine headgroup",
      "choline-related ion",
      "acylcarnitine/amine ion",
      "choline/quaternary amine ion",
      "adenine ion",
      "guanine ion",
      "cytosine ion",
      "thymine/uracil-related ion",
      "proline immonium ion",
      "lysine immonium ion",
      "histidine immonium ion",
      "phenylalanine immonium ion",
      "tyrosine immonium ion",
      "tryptophan immonium ion",
      "C16:0 fatty acid anion",
      "C18:2 fatty acid anion",
      "C18:1 fatty acid anion",
      "C18:3 fatty acid anion",
      "inositol phosphate-related ion",
      "glycerophosphate-related ion"
    ),
    ion_mode = c(
      "ESI-", "ESI-", "ESI-", "ESI-", "ESI+",
      "ESI-", "ESI-", "ESI-", "ESI-", "ESI-",
      "ESI-", "ESI-", "ESI-", "ESI-",
      "ESI-", "ESI-", "ESI-", "ESI-",
      "ESI-", "ESI-", "ESI-", "ESI-",
      "ESI+", "ESI+", "ESI+", "ESI+",
      "ESI+", "ESI+", "ESI+", "ESI+",
      "ESI+", "ESI+", "ESI+", "ESI+",
      "ESI+", "ESI+", "ESI-", "ESI-",
      "ESI-", "ESI-", "ESI-", "ESI-"
    ),
    class_hint = c(
      "sulfated metabolites", "sulfated metabolites",
      "phosphorylated metabolites", "phosphorylated metabolites",
      "organophosphates; phosphates", "acetate/adducts",
      "formate/adducts", "quinic acid; chlorogenic acids",
      "caffeic acid / phenolics", "caffeic acid derivatives",
      "ferulic acid derivatives", "p-coumaric acid derivatives",
      "gallic acid / galloyl phenolics", "gallic acid derivatives",
      "flavonol glycosides", "flavonoid glycosides",
      "flavonoid glycosides", "methylated flavonols",
      "flavanone glycosides", "flavonoids/anthocyanins",
      "flavonol glycosides", "dihydroflavonols",
      "PC; SM lipids", "choline compounds", "acylcarnitines; amines",
      "choline; quaternary amines", "adenine; nucleosides",
      "guanine; nucleosides", "cytosine; nucleosides",
      "pyrimidine nucleosides", "proline; immonium ions",
      "lysine; immonium ions", "histidine; immonium ions",
      "phenylalanine; immonium ions", "tyrosine; immonium ions",
      "tryptophan; immonium ions", "lipids; fatty acids",
      "lipids; fatty acids", "lipids; fatty acids", "lipids; fatty acids",
      "PI; PIP lipids", "phospholipids"
    ),
    specificity = c(
      "high", "medium", "high", "high", "high", "medium", "low",
      "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "high", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "medium", "medium", "medium", "high",
      "high", "high", "high", "high", "medium"
    ),
    stringsAsFactors = FALSE
  )
}

#' Detect isotope, adduct, and in-source fragment relationships
#'
#' @param object A `mass_dataset` object.
#' @param mode Ion mode. One of `"positive"` or `"negative"`.
#' @param detect Relationship classes to detect.
#' @param ppm Mass tolerance in ppm.
#' @param rt_tolerance Retention time tolerance in seconds.
#' @param cor_cutoff Minimum abundance correlation across samples.
#' @param cor_method Correlation method passed to `stats::cor()`.
#' @param max_charge Maximum charge state checked for isotopes and neutral loss.
#' @param max_neutral_loss_charge Maximum charge state checked for neutral
#'   losses. For plant small-molecule LC-MS data, `1` is a conservative default
#'   that avoids ambiguous edges such as dihexose/z2 versus hexose/z1.
#' @param isotope_intensity_ratio_max Maximum mean abundance ratio of isotope to
#'   monoisotopic peak.
#' @param adduct_table Optional adduct dictionary with columns `annotation`,
#'   `mass_offset`, and optional `charge`.
#' @param neutral_loss_table Optional neutral loss dictionary with columns
#'   `annotation` and `mass`.
#' @param use_ms2 Logical. If `TRUE`, add MS2 assignment-aware evidence for
#'   ISF edges when spectra are available in `object@ms2_data`.
#' @param ms2_mz_tol_ppm Strict MS1-to-MS2 precursor m/z tolerance used to
#'   re-audit `mutate_ms2()` assignments before using MS2 evidence.
#' @param ms2_rt_tol Strict MS1-to-MS2 precursor RT tolerance in seconds used
#'   to re-audit `mutate_ms2()` assignments before using MS2 evidence.
#' @param ms2_fragment_mz_tol Absolute m/z tolerance used when checking whether
#'   an ISF feature m/z appears in a parent MS2 spectrum.
#' @param store Logical. If `TRUE`, attach the network to `object@other_files`.
#'
#' @return If `store = TRUE`, an updated `mass_dataset`; otherwise an edge table.
#' @export
detect_feature_relationships <- function(object,
                                         mode = c("positive", "negative"),
                                         detect = c("isotope", "adduct", "isf"),
                                         ppm = 10,
                                         rt_tolerance = 1,
                                         cor_cutoff = 0.7,
                                         cor_method = "pearson",
                                         max_charge = 2,
                                         max_neutral_loss_charge = 1,
                                         isotope_intensity_ratio_max = 0.8,
                                         adduct_table = NULL,
                                         neutral_loss_table = NULL,
                                         use_ms2 = TRUE,
                                         ms2_mz_tol_ppm = 5,
                                         ms2_rt_tol = 10,
                                         ms2_fragment_mz_tol = 0.02,
                                         store = TRUE) {
  check_mass_dataset_object(object)
  mode <- match.arg(mode)
  detect <- match.arg(detect, several.ok = TRUE)

  feature_data <- prepare_feature_network_data(object)
  variable_info <- feature_data$variable_info
  expr <- feature_data$expression_data
  mean_area <- rowMeans(expr, na.rm = TRUE)
  mean_area[!is.finite(mean_area)] <- NA_real_

  edges <- list()

  if ("isotope" %in% detect) {
    edges$isotope <- detect_isotope_edges(
      variable_info = variable_info,
      expression_data = expr,
      mean_area = mean_area,
      ppm = ppm,
      rt_tolerance = rt_tolerance,
      cor_cutoff = cor_cutoff,
      cor_method = cor_method,
      max_charge = max_charge,
      isotope_intensity_ratio_max = isotope_intensity_ratio_max
    )
  }

  if ("adduct" %in% detect) {
    if (is.null(adduct_table)) {
      adduct_table <- default_adduct_table(mode)
    }
    edges$adduct <- detect_mass_difference_edges(
      variable_info = variable_info,
      expression_data = expr,
      dictionary = adduct_pair_dictionary(adduct_table),
      type = "Adduct",
      ppm = ppm,
      rt_tolerance = rt_tolerance,
      cor_cutoff = cor_cutoff,
      cor_method = cor_method
    )
  }

  if ("isf" %in% detect) {
    if (is.null(neutral_loss_table)) {
      neutral_loss_table <- default_neutral_loss_table()
    }
    edges$isf <- detect_mass_difference_edges(
      variable_info = variable_info,
      expression_data = expr,
      dictionary = neutral_loss_dictionary(neutral_loss_table, max_neutral_loss_charge),
      type = "ISF",
      ppm = ppm,
      rt_tolerance = rt_tolerance,
      cor_cutoff = cor_cutoff,
      cor_method = cor_method,
      direction = "high_to_low"
    )
    if (nrow(edges$isf) > 0) {
      edges$isf$evidence_level <- "Rule"
      edges$isf$evidence <- "neutral_loss_or_mass_difference"
    }

    if (isTRUE(use_ms2) && nrow(edges$isf) > 0) {
      ms2_index <- prepare_ms2_feature_index(
        object = object,
        variable_info = variable_info,
        mz_tol_ppm = ms2_mz_tol_ppm,
        rt_tol_sec = ms2_rt_tol
      )
      edges$isf <- add_ms2_isf_evidence(
        edges = edges$isf,
        variable_info = variable_info,
        ms2_index = ms2_index,
        fragment_mz_tol = ms2_fragment_mz_tol
      )
    }
  }

  network <- normalize_feature_network(do.call(rbind, edges))
  network <- add_qc_ratio_stability(network, object, expr)

  if (store) {
    return(set_feature_network(object, network))
  }
  network
}

#' Collapse feature subnetworks to empirical compound pseudo-areas
#'
#' @param object A `mass_dataset` object.
#' @param feature_network Optional edge table. If `NULL`, the stored network is
#'   used.
#' @param include_singletons Keep features that have no network edges.
#' @param min_component_size Minimum number of features required for PC1
#'   collapsing.
#' @param scale_features Logical. Scale each feature before PCA.
#'
#' @return A list with `expression_data`, `compound_info`, and
#'   `feature_mapping`.
#' @export
collapse_to_pseudo_area <- function(object,
                                    feature_network = NULL,
                                    include_singletons = TRUE,
                                    min_component_size = 2,
                                    scale_features = TRUE) {
  check_mass_dataset_object(object)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required to collapse subnetworks.", call. = FALSE)
  }

  feature_data <- prepare_feature_network_data(object)
  expr <- feature_data$expression_data
  variable_info <- feature_data$variable_info
  feature_ids <- as.character(variable_info$variable_id)

  graph <- as_feature_igraph(object, feature_network)
  undirected <- igraph::as_undirected(graph, mode = "collapse")
  membership <- igraph::components(undirected)$membership
  membership <- membership[feature_ids]
  membership[is.na(membership)] <- seq_len(sum(is.na(membership))) + max(membership, na.rm = TRUE)

  split_features <- split(feature_ids, membership)
  compound_rows <- list()
  mapping_rows <- list()
  compound_info <- list()

  for (component_id in names(split_features)) {
    ids <- split_features[[component_id]]
    if (length(ids) < min_component_size && !include_singletons) {
      next
    }

    sub_expr <- expr[ids, , drop = FALSE]
    mean_area <- rowMeans(sub_expr, na.rm = TRUE)
    base_feature <- names(which.max(mean_area))
    compound_id <- paste0("EC_", sprintf("%05d", as.integer(component_id)))

    if (length(ids) >= min_component_size) {
      pseudo_area <- pc1_pseudo_area(sub_expr, base_feature, scale_features)
      method <- "PC1_rescaled_to_base_peak"
    } else {
      pseudo_area <- as.numeric(sub_expr[base_feature, ])
      method <- "singleton"
    }

    compound_rows[[compound_id]] <- pseudo_area
    mapping_rows[[compound_id]] <- data.frame(
      compound_id = compound_id,
      variable_id = ids,
      base_feature = base_feature,
      stringsAsFactors = FALSE
    )
    compound_info[[compound_id]] <- data.frame(
      compound_id = compound_id,
      base_feature = base_feature,
      n_features = length(ids),
      mz = variable_info$mz[match(base_feature, feature_ids)],
      rt = variable_info$rt[match(base_feature, feature_ids)],
      collapse_method = method,
      stringsAsFactors = FALSE
    )
  }

  expression_data <- as.data.frame(do.call(rbind, compound_rows), check.names = FALSE)
  colnames(expression_data) <- colnames(expr)

  list(
    expression_data = expression_data,
    compound_info = do.call(rbind, compound_info),
    feature_mapping = do.call(rbind, mapping_rows)
  )
}

check_mass_dataset_object <- function(object) {
  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' object.", call. = FALSE)
  }
  invisible(TRUE)
}

prepare_feature_network_data <- function(object) {
  variable_info <- massdataset::extract_variable_info(object)
  expression_data <- massdataset::extract_expression_data(object)

  required_cols <- c("variable_id", "mz", "rt")
  missing_cols <- setdiff(required_cols, colnames(variable_info))
  if (length(missing_cols) > 0) {
    stop("variable_info is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  variable_info$variable_id <- as.character(variable_info$variable_id)
  variable_info$mz <- suppressWarnings(as.numeric(variable_info$mz))
  variable_info$rt <- suppressWarnings(as.numeric(variable_info$rt))
  if (any(!is.finite(variable_info$mz)) || any(!is.finite(variable_info$rt))) {
    stop("variable_info columns 'mz' and 'rt' must contain finite numeric values.", call. = FALSE)
  }
  rownames(variable_info) <- variable_info$variable_id

  if (is.null(rownames(expression_data))) {
    stop("expression_data must use variable_id as row names.", call. = FALSE)
  }
  expression_data <- expression_data[variable_info$variable_id, , drop = FALSE]
  original_na <- sum(is.na(expression_data))
  expression_data <- suppressWarnings(as.data.frame(
    lapply(expression_data, function(x) as.numeric(as.character(x))),
    check.names = FALSE
  ))
  rownames(expression_data) <- variable_info$variable_id
  coerced_na <- sum(is.na(expression_data))

  if (coerced_na > original_na) {
    stop(
      "expression_data contains non-numeric values that cannot be coerced safely. ",
      "Please check the quantitative matrix before building the feature network.",
      call. = FALSE
    )
  }

  list(variable_info = variable_info, expression_data = as.matrix(expression_data))
}

#' Audit MS2 assignments stored in a mass_dataset object
#'
#' `mutate_ms2()` can intentionally be run with broad MS1-to-MS2 matching
#' tolerances to increase coverage. This helper re-checks those assignments
#' against stricter m/z and RT tolerances and flags shared spectra before the
#' spectra are used as ISF evidence.
#'
#' @param object A `mass_dataset` object.
#' @param mz_tol_ppm Strict precursor m/z tolerance in ppm.
#' @param rt_tol_sec Strict precursor RT tolerance in seconds.
#'
#' @return A data frame with one row per feature-MS2 assignment.
#' @export
audit_ms2_assignment <- function(object, mz_tol_ppm = 5, rt_tol_sec = 10) {
  check_mass_dataset_object(object)
  variable_info <- massdataset::extract_variable_info(object)
  records <- collect_ms2_assignment_records(object, variable_info)
  if (nrow(records$meta) == 0) {
    return(empty_ms2_assignment_audit())
  }
  score_ms2_assignment_records(records$meta, mz_tol_ppm, rt_tol_sec)
}

empty_ms2_assignment_audit <- function() {
  data.frame(
    ms2_set = character(),
    variable_id = character(),
    feature_mz = numeric(),
    feature_rt = numeric(),
    ms2_spectrum_id = character(),
    ms2_mz = numeric(),
    ms2_rt = numeric(),
    ms2_file = character(),
    original_mz_tol = numeric(),
    original_rt_tol = numeric(),
    mz_error_ppm = numeric(),
    rt_error_sec = numeric(),
    within_strict_tol = logical(),
    ms2_assignment_count = integer(),
    shared_ms2 = logical(),
    assignment_score = numeric(),
    best_owner = logical(),
    ms2_assignment_quality = character(),
    stringsAsFactors = FALSE
  )
}

collect_ms2_assignment_records <- function(object, variable_info) {
  ms2_sets <- massdataset::extract_ms2_data(object)
  if (length(ms2_sets) == 0) {
    return(list(meta = empty_ms2_assignment_audit()[, 1:10], spectra = list()))
  }

  variable_info$variable_id <- as.character(variable_info$variable_id)
  records <- list()
  spectra <- list()
  record_idx <- 1L

  for (set_idx in seq_along(ms2_sets)) {
    ms2_obj <- ms2_sets[[set_idx]]
    set_name <- names(ms2_sets)[set_idx]
    if (is.null(set_name) || is.na(set_name) || !nzchar(set_name)) {
      set_name <- paste0("ms2_set_", set_idx)
    }

    variable_ids <- as.character(ms2_obj@variable_id)
    if (length(variable_ids) == 0) {
      next
    }

    vi_idx <- match(variable_ids, variable_info$variable_id)
    n <- length(variable_ids)
    record_key <- paste(set_name, seq_len(n), sep = "||")
    spectra[record_key] <- ms2_obj@ms2_spectra

    records[[record_idx]] <- data.frame(
      ms2_set = rep(set_name, n),
      variable_id = variable_ids,
      feature_mz = variable_info$mz[vi_idx],
      feature_rt = variable_info$rt[vi_idx],
      ms2_spectrum_id = as.character(ms2_obj@ms2_spectrum_id),
      ms2_mz = as.numeric(ms2_obj@ms2_mz),
      ms2_rt = as.numeric(ms2_obj@ms2_rt),
      ms2_file = as.character(ms2_obj@ms2_file),
      original_mz_tol = rep(as.numeric(ms2_obj@mz_tol)[1], n),
      original_rt_tol = rep(as.numeric(ms2_obj@rt_tol)[1], n),
      record_key = record_key,
      stringsAsFactors = FALSE
    )
    record_idx <- record_idx + 1L
  }

  if (length(records) == 0) {
    return(list(meta = empty_ms2_assignment_audit()[, 1:10], spectra = list()))
  }

  list(meta = do.call(rbind, records), spectra = spectra)
}

score_ms2_assignment_records <- function(meta, mz_tol_ppm, rt_tol_sec) {
  meta$mz_error_ppm <- abs(meta$feature_mz - meta$ms2_mz) / meta$ms2_mz * 1e6
  meta$rt_error_sec <- abs(meta$feature_rt - meta$ms2_rt)
  meta$within_strict_tol <- is.finite(meta$mz_error_ppm) &
    is.finite(meta$rt_error_sec) &
    meta$mz_error_ppm <= mz_tol_ppm &
    meta$rt_error_sec <= rt_tol_sec

  spectrum_key <- paste(meta$ms2_set, meta$ms2_file, meta$ms2_spectrum_id, sep = "||")
  meta$ms2_assignment_count <- as.integer(ave(spectrum_key, spectrum_key, FUN = length))
  meta$shared_ms2 <- meta$ms2_assignment_count > 1

  meta$assignment_score <- (meta$mz_error_ppm / mz_tol_ppm)^2 +
    (meta$rt_error_sec / rt_tol_sec)^2
  meta$assignment_score[!is.finite(meta$assignment_score)] <- Inf

  meta$best_owner <- FALSE
  split_idx <- split(seq_len(nrow(meta)), spectrum_key)
  for (idx in split_idx) {
    valid_idx <- idx[meta$within_strict_tol[idx]]
    if (length(valid_idx) == 0) {
      next
    }
    best <- valid_idx[which.min(meta$assignment_score[valid_idx])]
    meta$best_owner[best] <- TRUE
  }

  meta$ms2_assignment_quality <- ifelse(
    !meta$within_strict_tol, "bad_match",
    ifelse(meta$shared_ms2 & !meta$best_owner, "shared_non_owner",
           ifelse(meta$shared_ms2, "shared_best_owner", "unique"))
  )

  meta[, colnames(empty_ms2_assignment_audit()), drop = FALSE]
}

prepare_ms2_feature_index <- function(object, variable_info, mz_tol_ppm, rt_tol_sec) {
  records <- collect_ms2_assignment_records(object, variable_info)
  if (nrow(records$meta) == 0) {
    return(list(meta = empty_ms2_assignment_audit(), spectra = list()))
  }

  meta <- score_ms2_assignment_records(records$meta, mz_tol_ppm, rt_tol_sec)
  meta$record_key <- records$meta$record_key
  keep <- meta$ms2_assignment_quality != "bad_match"
  if (!any(keep)) {
    return(list(meta = empty_ms2_assignment_audit(), spectra = list()))
  }

  meta <- meta[keep, , drop = FALSE]
  quality_rank <- c(unique = 1, shared_best_owner = 2, shared_non_owner = 3)
  meta$quality_rank <- unname(quality_rank[meta$ms2_assignment_quality])
  meta$quality_rank[is.na(meta$quality_rank)] <- 99

  split_idx <- split(seq_len(nrow(meta)), meta$variable_id)
  selected_idx <- vapply(split_idx, function(idx) {
    idx[order(meta$quality_rank[idx], meta$assignment_score[idx], na.last = TRUE)][1]
  }, integer(1))
  meta <- meta[selected_idx, , drop = FALSE]
  rownames(meta) <- meta$variable_id

  spectra <- records$spectra[meta$record_key]
  names(spectra) <- meta$variable_id
  meta$record_key <- NULL
  meta$quality_rank <- NULL

  list(meta = meta, spectra = spectra)
}

empty_feature_network <- function() {
  data.frame(
    from = character(),
    to = character(),
    type = character(),
    annotation = character(),
    confidence = numeric(),
    mz_error_ppm = numeric(),
    rt_diff = numeric(),
    abundance_cor = numeric(),
    qc_ratio_rsd = numeric(),
    evidence_level = character(),
    evidence = character(),
    from_ms2_spectrum_id = character(),
    to_ms2_spectrum_id = character(),
    from_ms2_quality = character(),
    to_ms2_quality = character(),
    from_ms2_shared_n = integer(),
    to_ms2_shared_n = integer(),
    same_ms2_spectrum = logical(),
    ms2_fragment_match = logical(),
    ms2_similarity_score = numeric(),
    ms2_matched_peaks = integer(),
    ms2_matched_ratio = numeric(),
    stringsAsFactors = FALSE
  )
}

normalize_feature_network <- function(feature_network) {
  if (is.null(feature_network) || nrow(feature_network) == 0) {
    return(empty_feature_network())
  }
  feature_network <- as.data.frame(feature_network, stringsAsFactors = FALSE)
  template <- empty_feature_network()
  missing_cols <- setdiff(colnames(template), colnames(feature_network))
  for (col in missing_cols) {
    feature_network[[col]] <- template[[col]][NA]
  }
  feature_network <- feature_network[, colnames(template), drop = FALSE]
  feature_network$from <- as.character(feature_network$from)
  feature_network$to <- as.character(feature_network$to)
  feature_network$type <- as.character(feature_network$type)
  feature_network$annotation <- as.character(feature_network$annotation)
  feature_network$evidence_level <- as.character(feature_network$evidence_level)
  feature_network$evidence <- as.character(feature_network$evidence)
  feature_network$from_ms2_spectrum_id <- as.character(feature_network$from_ms2_spectrum_id)
  feature_network$to_ms2_spectrum_id <- as.character(feature_network$to_ms2_spectrum_id)
  feature_network$from_ms2_quality <- as.character(feature_network$from_ms2_quality)
  feature_network$to_ms2_quality <- as.character(feature_network$to_ms2_quality)
  feature_network
}

default_adduct_table <- function(mode) {
  if (identical(mode, "positive")) {
    return(data.frame(
      annotation = c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+", "[M-H2O+H]+"),
      mass_offset = c(1.007276, 22.989218, 38.963158, 18.033823, -17.003289),
      charge = 1,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    annotation = c("[M-H]-", "[M+Cl]-", "[M+FA-H]-", "[M+Hac-H]-"),
    mass_offset = c(-1.007276, 34.969402, 44.998201, 59.013851),
    charge = 1,
    stringsAsFactors = FALSE
  )
}

default_neutral_loss_table <- function() {
  table <- default_plant_neutral_loss_table()
  table[table$use_in_isf, , drop = FALSE]
}

adduct_pair_dictionary <- function(adduct_table) {
  required_cols <- c("annotation", "mass_offset")
  if (!all(required_cols %in% colnames(adduct_table))) {
    stop("adduct_table must contain columns: annotation, mass_offset", call. = FALSE)
  }

  pairs <- utils::combn(seq_len(nrow(adduct_table)), 2)
  out <- lapply(seq_len(ncol(pairs)), function(i) {
    idx <- pairs[, i]
    delta <- abs(adduct_table$mass_offset[idx[2]] - adduct_table$mass_offset[idx[1]])
    data.frame(
      annotation = paste(adduct_table$annotation[idx], collapse = " <-> "),
      mass_difference = delta,
      charge = 1,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

neutral_loss_dictionary <- function(neutral_loss_table, max_charge) {
  if (!all(c("annotation", "mass") %in% colnames(neutral_loss_table))) {
    stop("neutral_loss_table must contain columns: annotation, mass", call. = FALSE)
  }

  out <- list()
  for (charge in seq_len(max_charge)) {
    temp <- neutral_loss_table
    temp$mass_difference <- temp$mass / charge
    temp$charge <- charge
    temp$annotation <- paste0("-", temp$annotation, "/z", charge)
    out[[charge]] <- temp[, c("annotation", "mass_difference", "charge")]
  }
  do.call(rbind, out)
}

detect_isotope_edges <- function(variable_info,
                                 expression_data,
                                 mean_area,
                                 ppm,
                                 rt_tolerance,
                                 cor_cutoff,
                                 cor_method,
                                 max_charge,
                                 isotope_intensity_ratio_max) {
  dictionary <- data.frame(
    annotation = paste0("[M+1] isotope z=", seq_len(max_charge)),
    mass_difference = 1.0033548378 / seq_len(max_charge),
    charge = seq_len(max_charge),
    stringsAsFactors = FALSE
  )

  edges <- detect_mass_difference_edges(
    variable_info = variable_info,
    expression_data = expression_data,
    dictionary = dictionary,
    type = "Isotope",
    ppm = ppm,
    rt_tolerance = rt_tolerance,
    cor_cutoff = cor_cutoff,
    cor_method = cor_method,
    direction = "low_to_high"
  )

  if (nrow(edges) == 0) {
    return(edges)
  }

  denominator <- mean_area[edges$from]
  ratio <- ifelse(is.finite(denominator) & denominator > 0,
                  mean_area[edges$to] / denominator,
                  NA_real_)
  edges <- edges[is.finite(ratio) & ratio <= isotope_intensity_ratio_max, , drop = FALSE]
  edges
}

detect_mass_difference_edges <- function(variable_info,
                                         expression_data,
                                         dictionary,
                                         type,
                                         ppm,
                                         rt_tolerance,
                                         cor_cutoff,
                                         cor_method,
                                         direction = c("low_to_high", "high_to_low")) {
  direction <- match.arg(direction)
  variable_info <- variable_info[order(variable_info$rt, variable_info$mz), , drop = FALSE]
  feature_ids <- as.character(variable_info$variable_id)
  rt_values <- variable_info$rt
  cor_cache <- build_feature_correlation_cache(expression_data, cor_method)
  dictionary_mass_difference <- dictionary$mass_difference
  dictionary_annotation <- dictionary$annotation
  edges <- list()
  edge_idx <- 1L

  for (i in seq_len(nrow(variable_info))) {
    low_id <- feature_ids[i]
    rt_left <- findInterval(rt_values[i] - rt_tolerance, rt_values, left.open = TRUE) + 1L
    rt_right <- findInterval(rt_values[i] + rt_tolerance, rt_values)
    if (rt_left > rt_right) {
      next
    }

    candidates <- seq.int(rt_left, rt_right)
    candidates <- candidates[candidates != i & variable_info$mz[candidates] > variable_info$mz[i]]
    if (length(candidates) == 0) {
      next
    }

    observed_delta <- variable_info$mz[candidates] - variable_info$mz[i]
    for (d in seq_along(dictionary_mass_difference)) {
      target_delta <- dictionary_mass_difference[d]
      if (!is.finite(target_delta) || target_delta <= 0) {
        next
      }

      mass_window <- target_delta * ppm / 1e6
      hit_idx <- which(
        observed_delta >= target_delta - mass_window &
          observed_delta <= target_delta + mass_window
      )
      if (length(hit_idx) == 0) {
        next
      }

      hits <- candidates[hit_idx]
      high_ids <- feature_ids[hits]
      hit_errors <- abs(observed_delta[hit_idx] - target_delta) / target_delta * 1e6
      abundance_cor <- pairwise_feature_correlations(cor_cache, low_id, high_ids)
      keep <- is.finite(abundance_cor) & abundance_cor >= cor_cutoff
      if (!any(keep)) {
        next
      }

      hits <- hits[keep]
      high_ids <- high_ids[keep]
      hit_errors <- hit_errors[keep]
      abundance_cor <- abundance_cor[keep]

      if (identical(direction, "high_to_low")) {
        from <- high_ids
        to <- rep(low_id, length(high_ids))
      } else {
        from <- rep(low_id, length(high_ids))
        to <- high_ids
      }

      rt_delta <- abs(variable_info$rt[hits] - variable_info$rt[i])
      confidence <- edge_confidence(
        mz_error_ppm = hit_errors,
        ppm = ppm,
        rt_diff = rt_delta,
        rt_tolerance = rt_tolerance,
        abundance_cor = abundance_cor
      )

      edges[[edge_idx]] <- data.frame(
        from = from,
        to = to,
        type = type,
        annotation = dictionary_annotation[d],
        confidence = confidence,
        mz_error_ppm = hit_errors,
        rt_diff = rt_delta,
        abundance_cor = abundance_cor,
        qc_ratio_rsd = NA_real_,
        stringsAsFactors = FALSE
      )
      edge_idx <- edge_idx + 1L
    }
  }

  normalize_feature_network(do.call(rbind, edges))
}

build_feature_correlation_cache <- function(expression_data, cor_method) {
  expression_data <- as.matrix(expression_data)
  fast_pearson <- identical(cor_method, "pearson") && !anyNA(expression_data)
  if (!fast_pearson) {
    return(list(
      fast_pearson = FALSE,
      expression_data = expression_data,
      cor_method = cor_method
    ))
  }

  row_centered <- sweep(expression_data, 1, rowMeans(expression_data), "-")
  row_norm <- sqrt(rowSums(row_centered^2))
  row_norm[!is.finite(row_norm) | row_norm == 0] <- NA_real_
  scaled_expression <- sweep(row_centered, 1, row_norm, "/")

  list(
    fast_pearson = TRUE,
    scaled_expression = scaled_expression,
    expression_data = expression_data,
    cor_method = cor_method
  )
}

pairwise_feature_correlations <- function(cor_cache, feature_id, candidate_ids) {
  if (length(candidate_ids) == 0) {
    return(numeric())
  }

  if (isTRUE(cor_cache$fast_pearson)) {
    source_row <- cor_cache$scaled_expression[feature_id, , drop = FALSE]
    candidate_rows <- cor_cache$scaled_expression[candidate_ids, , drop = FALSE]
    return(as.numeric(source_row %*% t(candidate_rows)))
  }

  vapply(candidate_ids, function(candidate_id) {
    suppressWarnings(stats::cor(
      cor_cache$expression_data[feature_id, ],
      cor_cache$expression_data[candidate_id, ],
      use = "pairwise.complete.obs",
      method = cor_cache$cor_method
    ))
  }, numeric(1))
}

add_ms2_isf_evidence <- function(edges,
                                 variable_info,
                                 ms2_index,
                                 fragment_mz_tol) {
  if (nrow(edges) == 0 || nrow(ms2_index$meta) == 0) {
    return(edges)
  }

  variable_info <- variable_info[match(unique(c(edges$from, edges$to)),
                                      variable_info$variable_id), , drop = FALSE]
  feature_mz <- stats::setNames(variable_info$mz, variable_info$variable_id)
  meta <- ms2_index$meta
  spectra <- ms2_index$spectra

  for (i in seq_len(nrow(edges))) {
    parent_id <- edges$from[i]
    fragment_id <- edges$to[i]
    parent_meta <- if (parent_id %in% rownames(meta)) meta[parent_id, , drop = FALSE] else meta[0, , drop = FALSE]
    fragment_meta <- if (fragment_id %in% rownames(meta)) meta[fragment_id, , drop = FALSE] else meta[0, , drop = FALSE]

    edges$evidence_level[i] <- "Rule"
    edges$evidence[i] <- "neutral_loss_or_mass_difference"

    if (nrow(parent_meta) > 0) {
      edges$from_ms2_spectrum_id[i] <- parent_meta$ms2_spectrum_id
      edges$from_ms2_quality[i] <- parent_meta$ms2_assignment_quality
      edges$from_ms2_shared_n[i] <- parent_meta$ms2_assignment_count
    }
    if (nrow(fragment_meta) > 0) {
      edges$to_ms2_spectrum_id[i] <- fragment_meta$ms2_spectrum_id
      edges$to_ms2_quality[i] <- fragment_meta$ms2_assignment_quality
      edges$to_ms2_shared_n[i] <- fragment_meta$ms2_assignment_count
    }

    parent_spectrum <- spectra[[parent_id]]
    fragment_spectrum <- spectra[[fragment_id]]
    has_parent_ms2 <- !is.null(parent_spectrum) && nrow(as_ms2_peak_matrix(parent_spectrum)) > 0
    has_fragment_ms2 <- !is.null(fragment_spectrum) && nrow(as_ms2_peak_matrix(fragment_spectrum)) > 0

    if (!has_parent_ms2) {
      next
    }

    if (nrow(parent_meta) > 0 && nrow(fragment_meta) > 0) {
      same_spectrum <- identical(
        paste(parent_meta$ms2_set, parent_meta$ms2_file, parent_meta$ms2_spectrum_id, sep = "||"),
        paste(fragment_meta$ms2_set, fragment_meta$ms2_file, fragment_meta$ms2_spectrum_id, sep = "||")
      )
      edges$same_ms2_spectrum[i] <- same_spectrum
    } else {
      same_spectrum <- FALSE
    }

    fragment_mz <- feature_mz[fragment_id]
    fragment_match <- is.finite(fragment_mz) &&
      ms2_contains_mz(parent_spectrum, fragment_mz, fragment_mz_tol)
    edges$ms2_fragment_match[i] <- fragment_match
    if (!fragment_match) {
      next
    }

    parent_quality <- parent_meta$ms2_assignment_quality
    parent_reliable <- parent_quality %in% c("unique", "shared_best_owner")
    if (parent_reliable) {
      edges$evidence_level[i] <- "L2"
      edges$evidence[i] <- paste(edges$evidence[i], "fragment_mz_in_parent_ms2", sep = ";")
      edges$confidence[i] <- pmin(1, edges$confidence[i] + 0.05)
    } else {
      edges$evidence_level[i] <- "L2_ambiguous"
      edges$evidence[i] <- paste(edges$evidence[i], "fragment_mz_in_shared_parent_ms2", sep = ";")
      next
    }

    if (!has_fragment_ms2 || isTRUE(same_spectrum)) {
      if (isTRUE(same_spectrum)) {
        edges$evidence[i] <- paste(edges$evidence[i], "same_ms2_spectrum_no_l1", sep = ";")
      }
      next
    }

    fragment_quality <- fragment_meta$ms2_assignment_quality
    fragment_reliable <- fragment_quality %in% c("unique", "shared_best_owner")
    if (!fragment_reliable) {
      edges$evidence[i] <- paste(edges$evidence[i], "fragment_ms2_shared_non_owner_no_l1", sep = ";")
      next
    }

    sim <- reverse_ms2_similarity(parent_spectrum, fragment_spectrum, mz_tol = fragment_mz_tol)
    edges$ms2_similarity_score[i] <- sim$score
    edges$ms2_matched_peaks[i] <- sim$matched_peaks
    edges$ms2_matched_ratio[i] <- sim$matched_ratio
    if (is.finite(sim$score) && (sim$score > 0.5 || sim$matched_ratio > 0.7)) {
      edges$evidence_level[i] <- "L1"
      edges$evidence[i] <- paste(edges$evidence[i], "ms2_spectral_similarity", sep = ";")
      edges$confidence[i] <- pmin(1, edges$confidence[i] + 0.10)
    }
  }

  edges
}

as_ms2_peak_matrix <- function(spectrum) {
  if (is.null(spectrum)) {
    return(matrix(numeric(), ncol = 2))
  }
  spectrum <- as.matrix(spectrum)
  if (ncol(spectrum) < 2 || nrow(spectrum) == 0) {
    return(matrix(numeric(), ncol = 2))
  }
  spectrum <- spectrum[, 1:2, drop = FALSE]
  suppressWarnings(storage.mode(spectrum) <- "numeric")
  spectrum <- spectrum[is.finite(spectrum[, 1]) & is.finite(spectrum[, 2]) & spectrum[, 2] > 0, ,
                       drop = FALSE]
  colnames(spectrum) <- c("mz", "intensity")
  spectrum
}

ms2_contains_mz <- function(spectrum, mz, mz_tol) {
  spectrum <- as_ms2_peak_matrix(spectrum)
  if (nrow(spectrum) == 0 || !is.finite(mz)) {
    return(FALSE)
  }
  any(abs(spectrum[, 1] - mz) <= mz_tol)
}

reverse_ms2_similarity <- function(parent_spectrum, fragment_spectrum, mz_tol) {
  x <- as_ms2_peak_matrix(parent_spectrum)
  y <- as_ms2_peak_matrix(fragment_spectrum)
  empty <- list(score = NA_real_, matched_peaks = 0L, matched_ratio = NA_real_)
  if (nrow(x) == 0 || nrow(y) == 0) {
    return(empty)
  }

  x[, 2] <- 100 * x[, 2] / max(x[, 2])
  y[, 2] <- 100 * y[, 2] / max(y[, 2])
  y_available <- rep(TRUE, nrow(y))
  matched_parent <- numeric()
  matched_fragment <- numeric()

  for (i in seq_len(nrow(x))) {
    candidate_idx <- which(y_available & abs(y[, 1] - x[i, 1]) <= mz_tol)
    if (length(candidate_idx) == 0) {
      next
    }
    best_idx <- candidate_idx[which.min(abs(y[candidate_idx, 1] - x[i, 1]))]
    matched_parent <- c(matched_parent, x[i, 2])
    matched_fragment <- c(matched_fragment, y[best_idx, 2])
    y_available[best_idx] <- FALSE
  }

  if (length(matched_parent) == 0) {
    return(list(score = 0, matched_peaks = 0L, matched_ratio = 0))
  }

  score <- sum(matched_parent * matched_fragment) /
    sqrt(sum(matched_parent^2) * sum(y[, 2]^2))
  list(
    score = as.numeric(score),
    matched_peaks = length(matched_parent),
    matched_ratio = length(matched_parent) / nrow(y)
  )
}

edge_confidence <- function(mz_error_ppm, ppm, rt_diff, rt_tolerance, abundance_cor) {
  mass_score <- pmax(0, 1 - mz_error_ppm / ppm)
  rt_score <- pmax(0, 1 - rt_diff / rt_tolerance)
  cor_score <- pmax(0, abundance_cor)
  round((0.45 * mass_score) + (0.20 * rt_score) + (0.35 * cor_score), 4)
}

add_qc_ratio_stability <- function(network, object, expression_data) {
  if (nrow(network) == 0) {
    return(network)
  }

  sample_info <- massdataset::extract_sample_info(object)
  qc_ids <- character()
  if ("class" %in% colnames(sample_info)) {
    qc_ids <- as.character(sample_info$sample_id[toupper(sample_info$class) == "QC"])
  }
  if (length(qc_ids) == 0) {
    qc_ids <- as.character(sample_info$sample_id[grepl("QC", sample_info$sample_id, ignore.case = TRUE)])
  }
  qc_ids <- intersect(qc_ids, colnames(expression_data))
  if (length(qc_ids) < 3) {
    return(network)
  }

  network$qc_ratio_rsd <- vapply(seq_len(nrow(network)), function(i) {
    ratio <- expression_data[network$to[i], qc_ids] / expression_data[network$from[i], qc_ids]
    ratio <- ratio[is.finite(ratio)]
    if (length(ratio) < 3 || mean(ratio) == 0) {
      return(NA_real_)
    }
    stats::sd(ratio) / abs(mean(ratio))
  }, numeric(1))

  qc_score <- ifelse(is.na(network$qc_ratio_rsd), 0, pmax(0, 1 - network$qc_ratio_rsd))
  network$confidence <- round((0.85 * network$confidence) + (0.15 * qc_score), 4)
  network
}

prefix_feature_network_ids <- function(network, prefix) {
  network <- normalize_feature_network(network)
  if (nrow(network) == 0) {
    return(network)
  }
  network$from <- paste(prefix, network$from, sep = "::")
  network$to <- paste(prefix, network$to, sep = "::")
  network
}

detect_cross_polarity_edges <- function(positive_object,
                                        negative_object,
                                        ppm,
                                        rt_tolerance,
                                        cor_cutoff,
                                        neutral_loss_table = NULL) {
  pos <- prepare_feature_network_data(positive_object)
  neg <- prepare_feature_network_data(negative_object)
  pos_adducts <- default_cross_polarity_adduct_table("positive")
  neg_adducts <- default_cross_polarity_adduct_table("negative")
  same_compound <- detect_cross_neutral_mass_edges(
    pos = pos,
    neg = neg,
    pos_adducts = pos_adducts,
    neg_adducts = neg_adducts,
    ppm = ppm,
    rt_tolerance = rt_tolerance,
    cor_cutoff = cor_cutoff
  )

  if (is.null(neutral_loss_table)) {
    neutral_loss_table <- default_neutral_loss_table()
  }
  cross_isf <- detect_cross_polarity_isf_edges(
    pos = pos,
    neg = neg,
    pos_adducts = pos_adducts,
    neg_adducts = neg_adducts,
    neutral_loss_table = neutral_loss_table,
    ppm = ppm,
    rt_tolerance = rt_tolerance,
    cor_cutoff = cor_cutoff
  )

  normalize_feature_network(rbind(same_compound, cross_isf))
}

default_cross_polarity_adduct_table <- function(mode) {
  table <- default_adduct_table(mode)
  table[!grepl("-H2O", table$annotation, fixed = TRUE), , drop = FALSE]
}

detect_cross_neutral_mass_edges <- function(pos,
                                            neg,
                                            pos_adducts,
                                            neg_adducts,
                                            ppm,
                                            rt_tolerance,
                                            cor_cutoff) {
  edges <- list()
  edge_idx <- 1L
  cor_data <- prepare_cross_polarity_correlation(pos$expression_data, neg$expression_data)

  for (p_adduct in seq_len(nrow(pos_adducts))) {
    pos_neutral <- pos$variable_info$mz - pos_adducts$mass_offset[p_adduct]
    for (n_adduct in seq_len(nrow(neg_adducts))) {
      neg_neutral <- neg$variable_info$mz - neg_adducts$mass_offset[n_adduct]
      matches <- match_cross_neutral_features(
        pos_neutral = pos_neutral,
        neg_neutral = neg_neutral,
        pos_rt = pos$variable_info$rt,
        neg_rt = neg$variable_info$rt,
        ppm = ppm,
        rt_tolerance = rt_tolerance
      )
      if (nrow(matches) == 0) {
        next
      }
      matches <- add_cross_correlation(matches, pos, neg, cor_data)
      if (!is.null(cor_cutoff)) {
        matches <- matches[is.finite(matches$abundance_cor) & matches$abundance_cor >= cor_cutoff, ,
                           drop = FALSE]
      }
      if (nrow(matches) == 0) {
        next
      }

      edges[[edge_idx]] <- data.frame(
        from = paste("pos", pos$variable_info$variable_id[matches$pos_idx], sep = "::"),
        to = paste("neg", neg$variable_info$variable_id[matches$neg_idx], sep = "::"),
        type = "Cross-polarity",
        annotation = paste0(pos_adducts$annotation[p_adduct], " <-> ", neg_adducts$annotation[n_adduct]),
        confidence = edge_confidence(matches$mz_error_ppm, ppm, matches$rt_diff, rt_tolerance,
                                     matches$abundance_cor),
        mz_error_ppm = matches$mz_error_ppm,
        rt_diff = matches$rt_diff,
        abundance_cor = matches$abundance_cor,
        qc_ratio_rsd = NA_real_,
        evidence_level = "Cross",
        evidence = "same_neutral_mass_across_polarity",
        stringsAsFactors = FALSE
      )
      edge_idx <- edge_idx + 1L
    }
  }

  normalize_feature_network(do.call(rbind, edges))
}

detect_cross_polarity_isf_edges <- function(pos,
                                            neg,
                                            pos_adducts,
                                            neg_adducts,
                                            neutral_loss_table,
                                            ppm,
                                            rt_tolerance,
                                            cor_cutoff) {
  if (!all(c("annotation", "mass") %in% colnames(neutral_loss_table))) {
    stop("neutral_loss_table must contain columns: annotation, mass", call. = FALSE)
  }

  edges <- list()
  edge_idx <- 1L
  cor_data <- prepare_cross_polarity_correlation(pos$expression_data, neg$expression_data)

  for (p_adduct in seq_len(nrow(pos_adducts))) {
    pos_neutral <- pos$variable_info$mz - pos_adducts$mass_offset[p_adduct]
    for (n_adduct in seq_len(nrow(neg_adducts))) {
      neg_neutral <- neg$variable_info$mz - neg_adducts$mass_offset[n_adduct]
      for (loss_idx in seq_len(nrow(neutral_loss_table))) {
        loss_mass <- neutral_loss_table$mass[loss_idx]
        if (!is.finite(loss_mass) || loss_mass <= 0) {
          next
        }

        pos_parent_matches <- match_cross_neutral_features(
          pos_neutral = pos_neutral - loss_mass,
          neg_neutral = neg_neutral,
          pos_rt = pos$variable_info$rt,
          neg_rt = neg$variable_info$rt,
          ppm = ppm,
          rt_tolerance = rt_tolerance
        )
        if (nrow(pos_parent_matches) > 0) {
          pos_parent_matches <- add_cross_correlation(pos_parent_matches, pos, neg, cor_data)
          if (!is.null(cor_cutoff)) {
            pos_parent_matches <- pos_parent_matches[
              is.finite(pos_parent_matches$abundance_cor) &
                pos_parent_matches$abundance_cor >= cor_cutoff, , drop = FALSE]
          }
          if (nrow(pos_parent_matches) > 0) {
            edges[[edge_idx]] <- make_cross_isf_edge(
              matches = pos_parent_matches,
              parent_ids = paste("pos", pos$variable_info$variable_id[pos_parent_matches$pos_idx], sep = "::"),
              fragment_ids = paste("neg", neg$variable_info$variable_id[pos_parent_matches$neg_idx], sep = "::"),
              annotation = paste0("pos ", pos_adducts$annotation[p_adduct], " -> neg ",
                                  neg_adducts$annotation[n_adduct], " -", neutral_loss_table$annotation[loss_idx]),
              ppm = ppm,
              rt_tolerance = rt_tolerance
            )
            edge_idx <- edge_idx + 1L
          }
        }

        neg_parent_matches <- match_cross_neutral_features(
          pos_neutral = pos_neutral,
          neg_neutral = neg_neutral - loss_mass,
          pos_rt = pos$variable_info$rt,
          neg_rt = neg$variable_info$rt,
          ppm = ppm,
          rt_tolerance = rt_tolerance
        )
        if (nrow(neg_parent_matches) > 0) {
          neg_parent_matches <- add_cross_correlation(neg_parent_matches, pos, neg, cor_data)
          if (!is.null(cor_cutoff)) {
            neg_parent_matches <- neg_parent_matches[
              is.finite(neg_parent_matches$abundance_cor) &
                neg_parent_matches$abundance_cor >= cor_cutoff, , drop = FALSE]
          }
          if (nrow(neg_parent_matches) > 0) {
            edges[[edge_idx]] <- make_cross_isf_edge(
              matches = neg_parent_matches,
              parent_ids = paste("neg", neg$variable_info$variable_id[neg_parent_matches$neg_idx], sep = "::"),
              fragment_ids = paste("pos", pos$variable_info$variable_id[neg_parent_matches$pos_idx], sep = "::"),
              annotation = paste0("neg ", neg_adducts$annotation[n_adduct], " -> pos ",
                                  pos_adducts$annotation[p_adduct], " -", neutral_loss_table$annotation[loss_idx]),
              ppm = ppm,
              rt_tolerance = rt_tolerance
            )
            edge_idx <- edge_idx + 1L
          }
        }
      }
    }
  }

  normalize_feature_network(do.call(rbind, edges))
}

make_cross_isf_edge <- function(matches, parent_ids, fragment_ids, annotation, ppm, rt_tolerance) {
  data.frame(
    from = parent_ids,
    to = fragment_ids,
    type = "Cross-polarity ISF",
    annotation = annotation,
    confidence = edge_confidence(matches$mz_error_ppm, ppm, matches$rt_diff, rt_tolerance,
                                 matches$abundance_cor),
    mz_error_ppm = matches$mz_error_ppm,
    rt_diff = matches$rt_diff,
    abundance_cor = matches$abundance_cor,
    qc_ratio_rsd = NA_real_,
    evidence_level = "Cross_ISF",
    evidence = "neutral_loss_across_polarity",
    stringsAsFactors = FALSE
  )
}

match_cross_neutral_features <- function(pos_neutral, neg_neutral, pos_rt, neg_rt, ppm, rt_tolerance) {
  matches <- list()
  idx <- 1L
  neg_order <- order(neg_rt)
  neg_rt_sorted <- neg_rt[neg_order]

  for (i in seq_along(pos_neutral)) {
    if (!is.finite(pos_neutral[i])) {
      next
    }
    left <- findInterval(pos_rt[i] - rt_tolerance, neg_rt_sorted, left.open = TRUE) + 1L
    right <- findInterval(pos_rt[i] + rt_tolerance, neg_rt_sorted)
    if (left > right) {
      next
    }
    candidates <- neg_order[seq.int(left, right)]
    mass_window <- pos_neutral[i] * ppm / 1e6
    delta <- abs(neg_neutral[candidates] - pos_neutral[i])
    hit <- candidates[is.finite(delta) & delta <= mass_window]
    if (length(hit) == 0) {
      next
    }
    matches[[idx]] <- data.frame(
      pos_idx = rep(i, length(hit)),
      neg_idx = hit,
      mz_error_ppm = abs(neg_neutral[hit] - pos_neutral[i]) / pos_neutral[i] * 1e6,
      rt_diff = abs(neg_rt[hit] - pos_rt[i]),
      abundance_cor = NA_real_,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (length(matches) == 0) {
    return(data.frame(pos_idx = integer(), neg_idx = integer(), mz_error_ppm = numeric(),
                      rt_diff = numeric(), abundance_cor = numeric()))
  }
  do.call(rbind, matches)
}

prepare_cross_polarity_correlation <- function(pos_expr, neg_expr) {
  shared_samples <- intersect(colnames(pos_expr), colnames(neg_expr))
  if (length(shared_samples) < 3) {
    return(list(shared_samples = shared_samples, enabled = FALSE))
  }
  list(
    shared_samples = shared_samples,
    enabled = TRUE,
    pos_expr = pos_expr[, shared_samples, drop = FALSE],
    neg_expr = neg_expr[, shared_samples, drop = FALSE]
  )
}

add_cross_correlation <- function(matches, pos, neg, cor_data) {
  if (nrow(matches) == 0) {
    return(matches)
  }
  if (!isTRUE(cor_data$enabled)) {
    matches$abundance_cor <- NA_real_
    return(matches)
  }
  pos_ids <- pos$variable_info$variable_id[matches$pos_idx]
  neg_ids <- neg$variable_info$variable_id[matches$neg_idx]
  matches$abundance_cor <- vapply(seq_len(nrow(matches)), function(i) {
    suppressWarnings(stats::cor(
      cor_data$pos_expr[pos_ids[i], ],
      cor_data$neg_expr[neg_ids[i], ],
      use = "pairwise.complete.obs",
      method = "pearson"
    ))
  }, numeric(1))
  matches
}

pc1_pseudo_area <- function(sub_expr, base_feature, scale_features) {
  x <- t(log1p(sub_expr))
  keep <- apply(x, 2, stats::sd, na.rm = TRUE) > 0
  x <- x[, keep, drop = FALSE]

  if (ncol(x) < 2 || !base_feature %in% colnames(x)) {
    return(as.numeric(sub_expr[base_feature, ]))
  }

  pc <- stats::prcomp(x, center = TRUE, scale. = scale_features)
  score <- pc$x[, 1]
  base_area <- as.numeric(sub_expr[base_feature, ])
  if (stats::cor(score, base_area, use = "pairwise.complete.obs") < 0) {
    score <- -score
  }

  fit <- stats::lm(base_area ~ score)
  pseudo_area <- as.numeric(stats::predict(fit, newdata = data.frame(score = score)))
  pmax(pseudo_area, 0)
}
