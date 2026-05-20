#' Curated local RAG corpus for the global MetMiner assistant
#'
#' The corpus intentionally stores short, source-attributed summaries rather
#' than long copied passages. This keeps the assistant grounded while avoiding
#' broad claims beyond verified sources and the current MetMiner2 UI.
#'
#' @noRd
metminer_global_ai_rag_corpus <- function() {
  core_corpus <- data.frame(
    id = c(
      "metminer2_current_input",
      "metminer2_current_features",
      "metminer2_feature_network",
      "metminer2_ai_review",
      "metminer2_species_database",
      "tidymass_website_scope",
      "tidymass_package_ecosystem",
      "tidymass_2022_framework",
      "tidymass_2022_mass_dataset",
      "tidymass2_2026_innovations",
      "metminer_2024_scope",
      "metminer_2024_design"
    ),
    title = c(
      "MetMiner2 current input requirements",
      "MetMiner2 core enhancements",
      "MetMiner2 Feature Network",
      "MetMiner2 AI-assisted review",
      "MetMiner2 species-specific databases",
      "TidyMass website scope",
      "TidyMass package ecosystem",
      "TidyMass 2022 framework",
      "TidyMass 2022 mass_dataset design",
      "TidyMass2 2026 innovations",
      "MetMiner 2024 scope",
      "MetMiner 2024 design"
    ),
    source = c(
      "MetMiner2 local code",
      "MetMiner2 local code and homepage",
      "MetMiner2 local code and Feature Network principles",
      "MetMiner2 local code",
      "MetMiner2 local code",
      "TidyMass official documentation",
      "TidyMass official package page",
      "Shen et al., 2022, Nature Communications",
      "Shen et al., 2022, Nature Communications",
      "Wang et al., 2026, Nature Communications",
      "Wang et al., 2024, Journal of Integrative Plant Biology",
      "Wang et al., 2024, Journal of Integrative Plant Biology"
    ),
    year = c(2026, 2026, 2026, 2026, 2026, 2025, 2026, 2022, 2022, 2026, 2024, 2024),
    doi = c(
      "", "", "", "", "", "", "",
      "10.1038/s41467-022-32155-w",
      "10.1038/s41467-022-32155-w",
      "10.1038/s41467-026-68464-7",
      "10.1111/jipb.13774",
      "10.1111/jipb.13774"
    ),
    url = c(
      "",
      "",
      "",
      "",
      "",
      "https://www.tidymass.org/docs/",
      "https://tidymass.tidymass.org/articles/packages",
      "https://www.nature.com/articles/s41467-022-32155-w",
      "https://www.nature.com/articles/s41467-022-32155-w",
      "https://www.nature.com/articles/s41467-026-68464-7",
      "https://www.jipb.net/EN/abstract/article/1672-9072/99158",
      "https://www.jipb.net/EN/abstract/article/1672-9072/99158"
    ),
    text = c(
      paste(
        "Current MetMiner2 Project Init accepts sample metadata as CSV. Data Import accepts raw MS data as an MS1 ZIP, peak picking table as CSV, and existing mass_dataset objects as RDA.",
        "The raw ZIP is expected to contain POS and/or NEG folders. Parameter optimization currently scans mzXML files.",
        "Do not claim that the current complete workflow fully supports NetCDF CDF or Excel sample metadata."
      ),
      paste(
        "MetMiner2 extends the original MetMiner workflow toward annotation-centered evidence integration.",
        "Important upgrades include parameter optimization, blank-informed noise handling, QC/report output, imputation, normalization, Feature Network, AI-assisted annotation review, species-specific PlantCyc and KEGG databases, ID mapping, annotation filtering, differential analysis, and pathway enrichment.",
        "Do not describe MetMiner2 as inheriting TidyMass2 metabolite-origin inference, MetOriginDB, or metabolic feature-based functional module analysis; those are TidyMass2-specific literature background unless separately implemented in MetMiner2."
      ),
      paste(
        "Feature Network is designed for LC-MS feature redundancy and metabolite-origin evidence inside one experiment.",
        "It keeps isotope, adduct, source-fragment, neutral-loss, recurrent ion, co-elution, MS1, and MS2 evidence visible instead of forcing a one-feature one-metabolite assumption.",
        "Outputs include feature network tables, recurrent ion network, sub-network roles, pseudo-area matrix, non-redundant annotation table, and annotation audit table."
      ),
      paste(
        "The AI-assisted review module treats LLM output as evidence review rather than automatic table overwrite.",
        "Evidence bundles may include non-redundant annotations, raw metID candidates, MS1 features, MS2 summaries, Feature Network roles, recurrent ions, PlantCyc and KEGG evidence, LC-MS conditions, and optional paper-search records.",
        "Human review remains the final decision gate."
      ),
      paste(
        "MetMiner2 builds plant-focused annotation and enrichment resources using PlantCyc and KEGG.",
        "The PlantCyc local PGDB builder parses licensed PMN flat files to construct compound metadata, MS1 databases, pathway-compound links, reactions, and pathway databases.",
        "The KEGG workflow uses organism reaction evidence and can flag weak pathways for AI or manual curation before enrichment."
      ),
      paste(
        "The TidyMass documentation teaches LC-MS metabolomics data processing and analysis.",
        "Its documented topics include raw data processing, data exploration, data cleaning, metabolite annotation, statistical analysis, pathway enrichment, whole workflow examples, and a Shiny application."
      ),
      paste(
        "TidyMass is organized as an ecosystem of packages.",
        "massconverter handles raw data conversion, massdataset organizes data into mass_dataset objects, massprocesser handles LC-MS untargeted raw data processing, masscleaner supports cleaning, massqc supports quality control, metID supports database construction and annotation, massstat supports statistics, and metpath supports pathway enrichment."
      ),
      paste(
        "The 2022 TidyMass paper presents an R-based, object-oriented, reproducible framework for LC-MS untargeted metabolomics.",
        "The framework emphasizes traceability, shareability, reproducibility, shared data structures, and a modular package ecosystem."
      ),
      paste(
        "In TidyMass, mass_dataset is the central class for storing metabolomics data and metadata.",
        "The design keeps expression data, sample information, variable information, MS2 spectra, and analysis parameters synchronized across workflow steps."
      ),
      paste(
        "The 2026 TidyMass2 paper highlights three major innovations: metabolite origin inference, metabolic feature-based functional module analysis, and an intuitive web interface.",
        "It also describes MetOriginDB and a stronger focus on extracting biological insight from unannotated or weakly annotated LC-MS features.",
        "This source is background for the broader TidyMass ecosystem; it should not be used to claim that MetMiner2 implements those TidyMass2-specific modules."
      ),
      paste(
        "The 2024 MetMiner paper presents MetMiner as a user-friendly, full-functionality pipeline for large-scale plant metabolomics analysis.",
        "It was built with R Shiny, targets plant metabolomics users, and emphasizes transparency, traceability, reproducibility, interactive graphics, and a plant-specific mass spectrometry database."
      ),
      paste(
        "The original MetMiner workflow covers plant metabolomics data processing, metabolite annotation, statistical analysis, classification, enrichment, and biomarker mining.",
        "It was designed to help users without extensive programming experience analyze large-scale plant metabolomics datasets."
      )
    ),
    stringsAsFactors = FALSE
  )
  rbind(core_corpus, metminer_global_ai_parameter_corpus())
}

#' Curated explanations for Shiny-exposed analysis parameters
#'
#' These chunks summarize the parameters users can actually see in the app,
#' plus the internal function comments for the raw-data parameter optimizer.
#' Keep the text practical: what the parameter controls, how to interpret it,
#' and when a user might tune it.
#'
#' @noRd
metminer_global_ai_parameter_corpus <- function() {
  data.frame(
    id = c(
      "parameters_project_init",
      "parameters_data_import_xcms",
      "parameters_paramounter",
      "parameters_noise_filtering",
      "parameters_outlier_detection",
      "parameters_imputation",
      "parameters_normalization",
      "parameters_annotation_metid",
      "parameters_feature_network",
      "parameters_feature_annotation_review",
      "parameters_annotation_filtering",
      "parameters_differential_analysis",
      "parameters_pathway_enrichment",
      "parameters_kegg_database_builder",
      "parameters_plantcyc_database_builder",
      "parameters_id_mapping"
    ),
    title = c(
      "Project Init parameters",
      "Raw data import and XCMS parameters",
      "Paramounter parameter optimization",
      "Noise filtering parameters",
      "Outlier detection parameters",
      "Missing value imputation parameters",
      "Normalization parameters",
      "Metabolite annotation parameters",
      "Feature Network parameters",
      "Feature annotation review parameters",
      "Annotation filtering parameters",
      "Differential analysis parameters",
      "Pathway enrichment parameters",
      "KEGG database builder parameters",
      "PlantCyc database builder parameters",
      "Compound ID mapping parameters"
    ),
    source = rep("MetMiner2 local Shiny UI and function comments", 16),
    year = rep(2026, 16),
    doi = rep("", 16),
    url = rep("", 16),
    text = c(
      paste(
        "Project Init accepts sample information as CSV.",
        "The user maps columns to Sample ID, Injection Order, Class, Group, and Batch.",
        "Sample ID links metadata to data files; injection order is useful for QC and drift checks; class usually separates biological samples from QC or blank categories; group is the default biological comparison variable; batch records technical batches."
      ),
      paste(
        "Raw Data Import exposes XCMS-style peak detection and grouping parameters.",
        "ppm is the mass accuracy tolerance for chromatographic peak detection; smaller values are stricter and require better mass calibration.",
        "snthresh is the signal-to-noise threshold; increasing it removes weak peaks but can lose low-abundance metabolites.",
        "noise is the intensity baseline below which signals are ignored.",
        "peakwidth min and max define the expected chromatographic peak duration in seconds.",
        "prefilter peaks and prefilter intensity require a minimum number of scans above a minimum intensity before a peak is considered.",
        "fitgauss controls Gaussian peak fitting; integrate controls peak area integration mode.",
        "mzdiff is the minimum m/z difference for overlapping peaks.",
        "binSize, bw, and min_fraction affect peak grouping across samples; fill_peaks controls whether missing peak areas are filled after grouping.",
        "column records LC mode such as RP or HILIC for downstream interpretation."
      ),
      paste(
        "Paramounter optimizes XCMS parameters from mzXML files.",
        "paramounter_part1 estimates a ppm cutoff using a binned EIC approach.",
        "directory is the mzXML data path; massSDrange is the range of standard deviations used for mass-difference stability; smooth controls chromatographic smoothing; cutoff is the quantile or percentage used to select the ppm cutoff; thread controls parallel workers; filenum chooses 3, 5, or all files for optimization.",
        "paramounter_part2 optimizes remaining parameters and uses ppmCut from part 1.",
        "For slow runs, use fewer files or fewer threads; for final datasets, all files gives more representative parameter estimates."
      ),
      paste(
        "Noise filtering includes low-confidence intensity masking, blank-ratio filtering, missing-value thresholds, and QC RSD filtering.",
        "Blank SD Multiplier and Percentile Fallback define conservative intensity floors.",
        "Blank Ratio Threshold removes features whose sample signal is not sufficiently higher than blank signal.",
        "QC Missing Ratio Threshold and Group Missing Ratio Threshold control how much missingness is tolerated in QC and biological groups.",
        "QC RSD Threshold removes unstable features across QC injections; 30 percent is a common starting point for untargeted LC-MS."
      ),
      paste(
        "Outlier detection supports automatic and manual review.",
        "NA percent cutoff flags samples with high missingness.",
        "SD fold change and MAD fold change flag samples with abnormal total signal or dispersion.",
        "PCA distance p-value flags samples far from the multivariate sample cloud.",
        "Manual outlier selectors let users remove known problematic injections after inspecting PCA and QC plots."
      ),
      paste(
        "Missing value imputation offers KNN, random forest, PCA, BPCA, and other methods depending on available packages.",
        "KNN k controls neighbor count; rowmax and colmax limit features or samples with too much missingness; maxp controls block size for high-dimensional data; seed improves reproducibility.",
        "Random forest max iterations and number of trees trade speed for stability.",
        "PCA/BPCA nPcs controls latent components; BPCA max steps and threshold control convergence."
      ),
      paste(
        "Normalization parameters control signal correction across samples.",
        "SVR normalization can optimize parameters over Begin, End, Step, and Multiple values.",
        "PQN reference chooses median or mean reference spectrum.",
        "Keep Scale controls whether normalized intensities preserve the original scale.",
        "Integration method controls how positive and negative modes are combined or retained for downstream summaries."
      ),
      paste(
        "Annotation parameters control metID-style database matching.",
        "Database organism selection chooses the local plant database resource.",
        "Local database folder points to .rda annotation databases.",
        "LC Column should match RP or HILIC chromatography.",
        "MS1 Match ppm is the precursor m/z tolerance; stricter values reduce false positives but require calibrated data.",
        "MS2 Match ppm is the fragment tolerance for MS/MS matching.",
        "RT Match seconds controls retention-time tolerance when database RT is available.",
        "Candidates per feature limits how many ranked annotations are retained.",
        "Threads controls parallel execution."
      ),
      paste(
        "Feature Network parameters define relationships among LC-MS features.",
        "Mass Tolerance ppm is used for isotope, adduct, neutral loss, and cross-feature mass-difference matching.",
        "RT Window seconds defines co-elution tolerance; too wide connects unrelated peaks, too narrow misses broad or shifted peaks.",
        "Correlation Full-score Reference controls intensity-profile similarity scoring.",
        "Max Isotope Charge and Max Neutral Loss Charge define charge states considered.",
        "MS2 evidence options attach MGF spectra and audit source-fragment evidence using MS1-MS2 m/z tolerance, RT tolerance, strict MS2 m/z and RT tolerance, fragment m/z tolerance, and top N MS2 peaks."
      ),
      paste(
        "Feature annotation review parameters control inspection of a selected feature or sub-network.",
        "EIC RT Window sets the chromatogram window around a feature.",
        "EIC m/z Window sets extracted-ion chromatogram mass tolerance in Da.",
        "Max Traces limits how many chromatogram traces are drawn.",
        "MS1-MS2 Match m/z ppm and RT seconds connect MS2 scans to feature precursors.",
        "Fragment m/z tolerance and Annotate Top N Peaks control how MS2 fragments and neutral-loss pairs are labeled."
      ),
      paste(
        "Annotation filtering ranks and reduces redundant metID annotations.",
        "Top annotations per feature keeps the top N candidates ranked by Level, Total score, and LC-MS adduct priority.",
        "High confidence Level defines which metID annotation levels are considered high confidence.",
        "Positive/Negative RT Tolerance pairs opposite-polarity annotations that likely represent the same metabolite.",
        "Use feature-network validation adds isotope, adduct, source-fragment, recurrent ion, and co-elution evidence.",
        "Exclude suspected recurrent/background ions can mark or remove recurrent low-quality ion signals from final annotation tables."
      ),
      paste(
        "Differential analysis parameters define case-control comparisons.",
        "Group column selects the sample metadata column used to form groups.",
        "Comparison chooses case versus control.",
        "Summary statistic chooses mean or median intensity per group.",
        "Group test selects t test or Wilcoxon test.",
        "P adjustment controls multiple testing correction.",
        "Fold-change cutoff and P/FDR cutoff define significant DAMs.",
        "Use FDR switches volcano significance from raw p-value to adjusted p-value.",
        "Lightweight OPLS-DA is only released when biological replicates are sufficient."
      ),
      paste(
        "Pathway enrichment parameters control over-representation analysis.",
        "Pathway database .rda supplies the background pathway-compound mapping.",
        "ID type should match compound identifiers in the query and database.",
        "Test method chooses hypergeometric or Fisher exact testing.",
        "P adjustment controls multiple testing correction.",
        "Minimum pathway size removes pathways with too few matched background compounds."
      ),
      paste(
        "KEGG database builder parameters construct organism-specific metabolite and pathway resources.",
        "Organism code and organism name select the KEGG species.",
        "Minimum and maximum MW filter compounds for LC-MS relevance.",
        "Review thresholds flag weak pathways by reaction coverage, supported reaction count, pathway-specific compound count, and hub compound frequency.",
        "Max pathways in prompt controls AI review prompt size.",
        "Sleep between KEGG requests throttles REST calls to avoid overloading KEGG."
      ),
      paste(
        "PlantCyc database builder parameters construct local resources from licensed PMN PGDB files.",
        "PGDB archive or folder points to the downloaded species PGDB.",
        "Output prefix and organism label name the generated resources.",
        "Minimum and maximum MW filter compounds for LC-MS MS1 databases.",
        "MS2 validation ppm and Da control matching between PlantCyc compounds and public MS2 records.",
        "ClassyFire options add chemical classification with sleep and retry limits for polite remote requests."
      ),
      paste(
        "Compound ID mapping parameters merge PlantCyc and KEGG identifiers.",
        "PlantCyc and KEGG database inputs may be spectra_info or database .rda files.",
        "Mass tolerance ppm is used as fallback when exact identifiers or names are insufficient.",
        "The four-database merge accepts PlantCyc MS1, KEGG MS1, PlantCyc MS2, and KEGG MS2 resources.",
        "New Lab.ID prefix creates stable merged identifiers while preserving plantcyc_id and kegg_id fields."
      )
    ),
    stringsAsFactors = FALSE
  )
}

#' Expand common Chinese MetMiner Bot queries for lexical retrieval
#' @noRd
metminer_global_ai_rag_expand_query <- function(query) {
  q <- tolower(query %||% "")
  extra <- character()
  add_if <- function(pattern, words) {
    if (grepl(pattern, q, perl = TRUE)) extra <<- c(extra, words)
  }
  add_if("新特性|升级|亮点|改进|特点", "features enhancements innovations upgrade MetMiner2 MetMiner v1 original")
  add_if("输入|格式|上传|导入|数据准备|netcdf|cdf|excel|xlsx|csv|zip", "input format upload import CSV ZIP mzXML NetCDF CDF Excel sample metadata")
  add_if("参数|阈值|设置|调参|优化|ppm|snthresh|noise|peakwidth|prefilter|fitgauss|integrate|mzdiff|binsize|bw|min_fraction|fill_peaks|masssdrange|smooth|cutoff|rsd|pqn|svr|opls|fold.change|fdr", "parameter threshold tuning optimization ppm snthresh noise peakwidth prefilter fitgauss integrate mzdiff binSize bw min_fraction fill_peaks massSDrange smooth cutoff RSD PQN SVR OPLS fold change FDR")
  add_if("注释|annotation|metid|ms1|ms2|rt|候选|candidate|adduct|level|score", "annotation metID MS1 MS2 RT tolerance candidate adduct level total score database matching")
  add_if("差异|dam|火山|volcano|t.test|wilcoxon|fold|fdr|opls", "differential analysis DAM volcano t test Wilcoxon fold change p value FDR OPLS-DA")
  add_if("去噪|空白|blank|缺失|missing|qc|rsd|离群|outlier|pca", "noise filtering blank ratio missing value QC RSD outlier PCA")
  add_if("feature network|特征网络|网络|源内裂解|isf|同位素|加合|recurrent", "Feature Network isotope adduct ISF source fragment recurrent ion neutral loss")
  add_if("ai|llm|人工智能|大模型|审阅|审核", "AI LLM annotation review evidence human review")
  add_if("plantcyc|pmn|kegg|数据库|通路|富集", "PlantCyc KEGG database pathway enrichment species specific PGDB")
  add_if("tidymass2|origin|来源|功能模块", "TidyMass2 metabolite origin inference metabolic feature-based functional module web interface")
  add_if("tidymass|mass_dataset|massdataset", "TidyMass mass_dataset massprocesser metID metpath massdataset")
  add_if("metminer|metminer2", "MetMiner MetMiner2 plant metabolomics Shiny pipeline annotation")
  paste(c(query, extra), collapse = " ")
}

#' Tokenize English-like terms and selected identifiers for local RAG
#' @noRd
metminer_global_ai_rag_tokens <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  x <- gsub("[^a-z0-9_.+-]+", " ", x, perl = TRUE)
  tokens <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  tokens[nchar(tokens) > 1]
}

#' Retrieve source-attributed passages using lightweight TF-IDF vectors
#' @noRd
metminer_global_ai_rag_retrieve <- function(query, top_n = 6) {
  corpus <- metminer_global_ai_rag_corpus()
  raw_query <- query %||% ""
  query <- metminer_global_ai_rag_expand_query(query)
  docs <- paste(corpus$title, corpus$text, corpus$source, corpus$doi, corpus$url)
  doc_tokens <- lapply(docs, metminer_global_ai_rag_tokens)
  query_tokens <- metminer_global_ai_rag_tokens(query)
  vocab <- sort(unique(c(unlist(doc_tokens, use.names = FALSE), query_tokens)))
  if (length(vocab) == 0 || length(query_tokens) == 0) {
    corpus$score <- 0
    return(utils::head(corpus, top_n))
  }

  make_tf <- function(tokens) {
    tab <- table(tokens)
    vec <- numeric(length(vocab))
    idx <- match(names(tab), vocab)
    vec[idx] <- as.numeric(tab)
    vec
  }

  doc_tf <- do.call(rbind, lapply(doc_tokens, make_tf))
  query_tf <- make_tf(query_tokens)
  df <- colSums(doc_tf > 0)
  idf <- log((nrow(doc_tf) + 1) / (df + 1)) + 1
  doc_mat <- sweep(doc_tf, 2, idf, `*`)
  query_vec <- query_tf * idf
  denom <- sqrt(rowSums(doc_mat^2)) * sqrt(sum(query_vec^2))
  score <- as.numeric(doc_mat %*% query_vec)
  score <- ifelse(denom > 0, score / denom, 0)
  if (!grepl("tidymass|mass_dataset|massdataset|metorigin|origin|来源|功能模块|functional", raw_query, ignore.case = TRUE, perl = TRUE)) {
    score[grepl("^tidymass", corpus$id)] <- score[grepl("^tidymass", corpus$id)] * 0.2
  }
  corpus$score <- score
  corpus <- corpus[order(corpus$score, decreasing = TRUE), , drop = FALSE]
  utils::head(corpus, top_n)
}
