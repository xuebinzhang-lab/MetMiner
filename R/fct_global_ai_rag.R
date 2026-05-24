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
      "parameters_data_overview",
      "parameters_parameter_advisor",
      "parameters_data_import_xcms",
      "parameters_paramounter",
      "parameters_noise_filtering",
      "parameters_outlier_detection",
      "parameters_imputation",
      "parameters_normalization",
      "parameters_annotation_metid",
      "parameters_ai_annotation",
      "parameters_feature_network",
      "parameters_feature_annotation_review",
      "parameters_annotation_filtering",
      "parameters_differential_analysis",
      "parameters_pathway_enrichment",
      "parameters_kegg_database_builder",
      "parameters_global_bot",
      "parameters_plantcyc_pgdb_builder",
      "parameters_plantcyc_database_builder",
      "parameters_id_mapping"
    ),
    title = c(
      "Project Init parameters",
      "Data Overview parameters",
      "LC-MS Parameter Advisor parameters",
      "Raw data import and XCMS parameters",
      "Paramounter parameter optimization",
      "Noise filtering parameters",
      "Outlier detection parameters",
      "Missing value imputation parameters",
      "Normalization parameters",
      "Metabolite annotation parameters",
      "AI annotation review parameters",
      "Feature Network parameters",
      "Feature annotation review parameters",
      "Annotation filtering parameters",
      "Differential analysis parameters",
      "Pathway enrichment parameters",
      "KEGG database builder parameters",
      "Global MetMiner Bot settings",
      "PlantCyc PGDB parser parameters",
      "PlantCyc database builder parameters",
      "Compound ID mapping parameters"
    ),
    source = rep("MetMiner2 local Shiny UI and function comments", 21),
    year = rep(2026, 21),
    doi = rep("", 21),
    url = rep("", 21),
    text = c(
      paste(
        "Project Init accepts sample information as CSV.",
        "The user maps columns to Sample ID, Injection Order, Class, Group, and Batch.",
        "Sample ID links metadata to data files; injection order is useful for QC and drift checks; class usually separates biological samples from QC or blank categories; group is the default biological comparison variable; batch records technical batches.",
        "Project Init also has optional LC-MS Conditions. Users can paste method text, auto-fill key items such as instrument, chromatography, column, mobile phases, ion mode, scan range, and collision energy, then save these as project-level context for AI review and MetMiner Bot."
      ),
      paste(
        "Data Overview parameters are visualization controls, not data-processing thresholds.",
        "mz-RT uses a hexbin toggle to switch between binned density and point-style plotting.",
        "Missing-value plots expose Color By, Order By, Show Percentage, Show X Text, and Descending Order.",
        "PCA plot controls include Log2 + Scale, Color By, Point Alpha, and Point Size.",
        "QC RSD plots expose Color By, RSD Cutoff percent, and Descending; the cutoff changes the reference line or display emphasis, not the cleaned dataset.",
        "Boxplots expose Color By, Fill By, and Show Points. Correlation plots expose correlation Method, spearman, pearson, or kendall, and Order By."
      ),
      paste(
        "LC-MS Parameter Advisor parameters generate annotation and adduct advice from method text.",
        "Paste LC-MS method is free text describing instrument, column, mobile phases, ion source, scan range, resolution, and collision energy.",
        "Instrument class can be Auto detect, Orbitrap, QTOF, or Other. Chromatography can be Auto detect, C18 reversed phase, HILIC, or Other.",
        "The advisor contextualizes annotation parameters and adduct priorities; it does not confirm metabolite identities or run peak picking.",
        "Downloaded JSON or TSV advice can be reused in annotation filtering as LC-MS parameter advice."
      ),
      paste(
        "Raw Data Import exposes XCMS-style peak detection and grouping parameters.",
        "Data source chooses raw MS1 ZIP, peak-picking table CSV, or existing mass_dataset RDA workflows. Table import maps Variable ID, m/z, RT, Polarity, and RT Unit; minute RT values are converted to seconds.",
        "ppm POS and ppm NEG are polarity-specific mass accuracy tolerances for chromatographic peak detection; smaller values are stricter and require better mass calibration.",
        "snthresh is the signal-to-noise threshold; increasing it removes weak peaks but can lose low-abundance metabolites.",
        "noise is the intensity baseline below which signals are ignored.",
        "threads controls massprocesser parallel workers; the Shiny UI currently offers 1, 2, or 4.",
        "peakwidth min and max define the expected chromatographic peak duration in seconds and are passed as peakwidth = c(min, max).",
        "prefilter peaks and prefilter intensity are passed as prefilter = c(peaks, intensity), requiring a minimum number of scans above a minimum intensity before a peak is considered.",
        "fitgauss controls Gaussian peak fitting; integrate controls peak area integration mode.",
        "mzdiff is the minimum m/z difference for overlapping peaks.",
        "binSize and bw affect peak grouping or retention-time correction inside massprocesser; min_fraction is the minimum sample fraction required for a grouped feature.",
        "fill_peaks controls whether missing peak areas are filled after grouping. All listed raw-import parameters are now passed to massprocesser::process_data."
      ),
      paste(
        "Paramounter optimizes XCMS parameters from mzXML files.",
        "paramounter_part1 estimates a ppm cutoff using a binned EIC approach.",
        "directory is the mzXML data path; massSDrange is a single numeric standard-deviation multiplier, not a lower-upper range. The current implementation calculates each peak-level ppm estimate as massSDrange * sd(peak_mzs) / apex_mz * 1e6; larger values make the suggested ppm cutoff more permissive and smaller values make it stricter.",
        "smooth is a numeric EIC smoothing-window level and defaults to 0, which disables smoothing; cutoff is the quantile of peak-level ppm estimates used to select the ppm cutoff and defaults to 0.95; thread controls parallel workers.",
        "filenum chooses 3, 5, or all mzXML files. Numeric choices are sampled evenly across the file list rather than taking only the first files.",
        "paramounter_part2 optimizes remaining XCMS-style parameters and uses polarity-specific ppmCut values from part 1. POS and NEG ppmCut can differ and are passed separately to the POS and NEG Step 2 runs.",
        "In the current implementation, part 2 uses smooth, ppmCut, filenum, and thread; massSDrange and cutoff are retained in the function signature only for backward compatibility and are not shown in the Step 2 UI.",
        "For high-resolution Orbitrap data, the default massSDrange = 2 is a reasonable starting point. Do not suggest vector values such as c(0, 15), because the Shiny UI expects one number.",
        "For slow runs, use fewer files or fewer threads; for final datasets, all files gives more representative parameter estimates."
      ),
      paste(
        "Noise filtering includes low-confidence intensity masking, blank-ratio filtering, missing-value thresholds, and QC RSD filtering.",
        "Group Samples By chooses the sample_info column used for biological missingness groups; QC samples remain the QC group when class == QC.",
        "Mask low-confidence intensities is optional. The blank method uses blank_mean + Blank SD Multiplier * blank_SD per feature and requires at least two blank samples; the distribution method estimates sample-level cutoffs and should be inspected carefully.",
        "Percentile Fallback is used only by the distribution masking method when a clear intensity antimode is not found.",
        "Apply Blank Ratio Filter is optional. Blank Sample Label and Reference Sample Label are class values; Ratio Threshold keeps features with sample_mean / blank_mean >= threshold, while blank_mean = 0 or undefined ratios are kept.",
        "QC Missing Ratio Threshold keeps features with QC missingness at or below the threshold. Group Missing Ratio Threshold keeps features present enough in at least one non-QC group.",
        "Apply RSD Filter controls the QC RSD step. QC RSD Threshold percent removes features with rsd >= threshold when more than one QC sample is available."
      ),
      paste(
        "Outlier detection supports automatic and manual review.",
        "Detection Method switches between Automatic and Manual modes.",
        "Automatic mode exposes NA percent cutoff, SD fold change, MAD fold change, and PCA distance p-value. Higher NA, SD, or MAD thresholds are more permissive; lower PCA distance p-values are stricter for multivariate outliers.",
        "Manual mode exposes separate positive and negative outlier sample selectors.",
        "PCA controls such as Color By, Label By, Scale Data, Point Size, Point Alpha, Width, and Height affect plots and downloads only; they do not change which samples are removed unless the user selects samples manually."
      ),
      paste(
        "Missing value imputation offers KNN, random forest, PCA, BPCA, and other methods depending on available packages.",
        "The method selector calls masscleaner::impute_mv with method values knn, rf, bpca, ppca, svdImpute, mean, median, zero, or minimum.",
        "KNN k controls neighbor count; rowmax and colmax limit rows or columns with too much missingness; maxp controls block size for high-dimensional KNN imputation; RNG Seed is passed as rng.seed.",
        "Random Forest parameters are maxiter, ntree, and decreasing order.",
        "BPCA, PPCA, and SVD Impute share nPcs. BPCA additionally uses maxSteps and threshold.",
        "Mean, median, zero, and minimum imputation have no additional Shiny parameters."
      ),
      paste(
        "Normalization parameters control signal correction across samples.",
        "Method chooses masscleaner::normalize_data methods: svr, pqn, total, median, mean, or loess.",
        "Keep Scale is passed to normalize_data for every normalization method.",
        "SVR Parameter Optimization toggles optimization. When enabled, Begin, End, Step, and Multiple are passed as SVR optimization settings.",
        "PQN reference chooses median or mean reference spectrum.",
        "Integration Method chooses qc_mean, qc_median, subject_mean, or subject_median for masscleaner::integrate_data.",
        "Integration is conditional: it runs only when sample_info has a batch column with more than one unique batch. It is skipped for single-batch data or missing batch metadata.",
        "PCA and RSD controls in this module, including Log2 + Scale, Color By, Point Alpha, Point Size, RSD Cutoff, Descending, Width, and Height, are plot/download controls."
      ),
      paste(
        "Annotation parameters control metID-style matching across layered databases.",
        "Layer 1 KEGG DB folder points to a locally built KEGG organism database folder containing .rda resources.",
        "Layer 1 PlantCyc DB chooses one built-in PlantCyc species database.",
        "Layer 2 public MS2 DB chooses optional public spectral databases. Customized DB folder is optional and can be empty.",
        "The four database inputs are independent; absence of a customized database is normal.",
        "LC Column should match RP or HILIC chromatography.",
        "MS1 Match ppm is the precursor m/z tolerance; stricter values reduce false positives but require calibrated data.",
        "MS2 Match ppm is the fragment tolerance for MS/MS matching.",
        "RT Match seconds controls retention-time tolerance when database RT is available.",
        "Candidates per feature limits how many ranked annotations are retained.",
        "Threads controls parallel execution. Genome-derived KEGG and PlantCyc annotation is later filtered with stricter core-adduct evidence in the annotation-filtering layer."
      ),
      paste(
        "AI annotation review parameters configure evidence packaging and optional LLM calls for feature-level review.",
        "Provider, model, API endpoint, API key, and temperature affect external LLM calls only; local evidence tables can still be prepared without changing analysis results.",
        "Compound or Feature Query focuses the review on a compound name, feature, or biological question.",
        "Max Features in Evidence limits how many feature records are included in the prompt. MS2 Top Peaks limits fragment peaks included per spectrum.",
        "Use paper-search for this request enables optional literature context. Paper Sources chooses which literature connectors are queried. Papers per Source limits returned records per literature source. paper-search CLI points to the local command or MCP wrapper used by the app.",
        "LC-MS Conditions is user-provided method context included in the review prompt.",
        "LLM output is treated as review evidence; it does not overwrite annotation tables without user action."
      ),
      paste(
        "Feature Network parameters define relationships among LC-MS features.",
        "Relationship Types chooses which edge classes to detect: natural isotopes, adducts, and in-source fragments.",
        "Ion Mode Auto runs available positive and negative objects; explicit Positive or Negative limits detection to one polarity.",
        "Mass Tolerance ppm is used for isotope, adduct, neutral-loss, and related mass-difference matching.",
        "RT Window seconds defines co-elution tolerance and recurrent-ion grouping; too wide connects unrelated peaks, too narrow misses broad or shifted peaks.",
        "Correlation Full-score Reference controls intensity-profile similarity scoring; it is a scoring reference, not a hard filter for relationship detection.",
        "Max Isotope Charge and Max Neutral Loss Charge define charge states considered.",
        "Use audited MS2 evidence for ISF enables optional MS2 ZIP attachment and source-fragment auditing.",
        "MS2 attachment uses LC Column, MS1-MS2 Match m/z ppm, and MS1-MS2 Match RT seconds to attach MGF spectra to normalized data.",
        "Strict MS2 Audit m/z ppm, Strict MS2 Audit RT seconds, and MS2 Fragment m/z Tolerance are used for stricter MS2 audit evidence.",
        "Visualization controls include Network View, Ion Mode, Sub-network, Interactive network, Min Edge Confidence, Max Rendered Edges, and Max Sub-network Choices.",
        "Cross-polarity Merge uses m/z Tolerance ppm, RT Window seconds, and Correlation to merge positive and negative networks. MS2 Annotate Top N Peaks affects spectrum labels in network views."
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
        "Exclude suspected recurrent/background ions can mark or remove recurrent low-quality ion signals from final annotation tables.",
        "LC-MS parameter advice JSON/TSV can provide adduct priority or instrument advice. Compound ID mapping TSV, CSV, or XLSX can add cross-database identifiers before redundancy reduction."
      ),
      paste(
        "Differential analysis parameters define case-control comparisons.",
        "Ion mode chooses positive or negative analysis object.",
        "Group column selects the sample metadata column used to form groups.",
        "Comparison chooses case versus control; fold change is calculated as case/control.",
        "Summary statistic chooses mean or median intensity per group.",
        "Group test selects t test or Wilcoxon test.",
        "P adjustment controls multiple testing correction.",
        "Fold-change cutoff and P/FDR cutoff define significant DAMs.",
        "Use FDR switches volcano significance from raw p-value to adjusted p-value.",
        "Interactive volcano controls whether the volcano is rendered interactively.",
        "Run lightweight OPLS-DA when available attempts an optional model only when biological replicates are sufficient. OPLS-DA scaling choices are pareto, standard, center, or none."
      ),
      paste(
        "Pathway enrichment parameters control over-representation analysis.",
        "Annotation/enrichment database chooses KEGG or PlantCyc and determines whether KEGG.ID or PlantCyc.ID is used.",
        "Pathway database .rda supplies the background pathway-compound mapping.",
        "Query source uses either the annotation filtering result or an uploaded ID list.",
        "Annotation table chooses collapse review, expand review, or final non-redundant when the query source is annotation filtering result.",
        "Uploaded Query ID file can be TSV, TXT, or CSV and should contain identifiers matching the selected database type.",
        "Test method chooses hypergeometric or Fisher exact testing.",
        "P adjustment controls multiple testing correction.",
        "Minimum pathway size removes pathways with too few matched background compounds."
      ),
      paste(
        "KEGG database builder parameters construct organism-specific metabolite and pathway resources.",
        "Mode switches between Database builder and AI review curation.",
        "Refresh KEGG plant list fetches KEGG organism entries filtered to Eukaryotes;Plants.",
        "Green plant organism is the KEGG organism code, for example zma. Organism name is the human-readable label stored in outputs.",
        "Minimum and maximum MW filter compounds for LC-MS relevance.",
        "Review thresholds flag weak pathways when reaction coverage is below the threshold, supported reaction count is below the threshold, pathway-specific compound count is below the threshold, or hub compound frequency indicates generic precursor support.",
        "Max pathways in prompt controls review prompt size. Sleep between KEGG requests throttles REST calls to avoid overloading KEGG.",
        "Output folder stores kegg organism MS1, MS2, pathway, review prompt, and curation outputs. Existing complete databases can be loaded instead of overwritten.",
        "AI review curation accepts uploaded JSON or JSONL review files, can load multiple reviews for voting, and can generate a curated pathway database. LLM provider controls are intentionally handled by MetMiner Bot or external tools, not this curation panel."
      ),
      paste(
        "Global MetMiner Bot settings configure the chat assistant, not core LC-MS processing.",
        "Provider, model, endpoint, API key, language, and temperature control LLM calls.",
        "Language sets the preferred response language. Temperature controls response variability; lower values are more deterministic.",
        "The Bot can read current project state, use the local RAG corpus, and for KEGG pathway review can call the generated review prompt and optionally paper-search context when available.",
        "The @lc-condition command lets users paste LC-MS method text and asks the Bot to extract project-level LC-MS condition fields into Project Init.",
        "If a user declines a suggested agent action such as KEGG review, the Bot suppresses repeat prompts for that same action during the current session."
      ),
      paste(
        "PlantCyc PGDB parser parameters build a local PlantCyc resource from licensed PMN PGDB files.",
        "PGDB archive ZIP or extracted PGDB folder path identifies the source files.",
        "Output prefix controls generated file names and object names. Organism label records the species label in metadata.",
        "Output directory is where parsed compound, reaction, pathway, and database resources are written."
      ),
      paste(
        "PlantCyc database builder parameters construct local resources from licensed PMN PGDB files.",
        "Compound SmartTable and Pathway SmartTable are PlantCyc SmartTable exports used as source inputs.",
        "Minimum and maximum MW filter compounds for LC-MS MS1 databases.",
        "MS2 validation ppm and Da control matching between PlantCyc compounds and public MS2 records.",
        "Add ClassyFire classification via Fiehn CFB optionally enriches chemical classes. Sleep between requests and Max retries control polite remote lookup and retry behavior.",
        "Output file prefix controls generated .rda object and file names. Output folder controls where database files are written."
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
  add_if("总览|overview|可视化|plot|绘图|pca|boxplot|correlation|hexbin|透明度|alpha|点大小", "Data Overview visualization plot PCA boxplot correlation hexbin alpha point size RSD")
  add_if("advisor|建议|方法文本|仪器|色谱|orbitrap|qtof|adduct advice|加合建议", "Parameter Advisor LC-MS method instrument chromatography Orbitrap QTOF adduct advice JSON TSV")
  add_if("注释|annotation|metid|ms1|ms2|rt|候选|candidate|adduct|level|score", "annotation metID MS1 MS2 RT tolerance candidate adduct level total score database matching layered KEGG PlantCyc public customized")
  add_if("差异|dam|火山|volcano|t.test|wilcoxon|fold|fdr|opls", "differential analysis DAM volcano t test Wilcoxon fold change p value FDR OPLS-DA")
  add_if("去噪|空白|blank|缺失|missing|qc|rsd|离群|outlier|pca", "noise filtering blank ratio missing value QC RSD outlier PCA")
  add_if("feature network|特征网络|网络|源内裂解|isf|同位素|加合|recurrent", "Feature Network isotope adduct ISF source fragment recurrent ion neutral loss")
  add_if("ai|llm|人工智能|大模型|审阅|审核|paper|文献|mcp|bot", "AI LLM annotation review evidence human review paper-search MCP MetMiner Bot provider model endpoint temperature")
  add_if("plantcyc|pmn|kegg|数据库|通路|富集|pgdb|classyfire", "PlantCyc KEGG database pathway enrichment species specific PGDB SmartTable ClassyFire")
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
  boost_if <- function(pattern, ids, multiplier = 1.6) {
    if (grepl(pattern, raw_query, ignore.case = TRUE, perl = TRUE)) {
      score[corpus$id %in% ids] <<- score[corpus$id %in% ids] * multiplier
    }
  }
  boost_if("总览|overview|可视化|plot|绘图|pca|boxplot|correlation|hexbin|透明度|alpha|点大小",
           "parameters_data_overview")
  boost_if("advisor|建议|方法文本|仪器|色谱|orbitrap|qtof|adduct advice|加合建议",
           "parameters_parameter_advisor")
  boost_if("paper|文献|mcp|大模型|llm|审阅|审核",
           c("parameters_ai_annotation", "parameters_global_bot", "metminer2_ai_review"))
  boost_if("pgdb|classyfire|smarttable",
           c("parameters_plantcyc_pgdb_builder", "parameters_plantcyc_database_builder"))
  corpus$score <- score
  corpus <- corpus[order(corpus$score, decreasing = TRUE), , drop = FALSE]
  utils::head(corpus, top_n)
}
