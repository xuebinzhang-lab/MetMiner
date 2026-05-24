# MetMiner v2 — Changelog

## v0.1.0.9005 / app 2.0.2 (development)

### 2026-05-24 — MetMiner Bot module advisors and downstream workflow polish

- **Added module-aware MetMiner Bot advisors** for data import, noise filtering, outlier review, missing-value imputation, normalization, feature networking, annotation, annotation filtering, differential analysis, enrichment, and database construction.
- **Captured live Shiny module state for AI guidance**, including current parameters, available POS/NEG objects, object summaries, selected features/pathways, and key result tables so Bot recommendations are grounded in the active project.
- **Added conservative advisor prompts** that separate observed evidence from recommendations and warn when missing plots, sample metadata, annotations, or pathway IDs make a decision under-supported.
- **Improved downstream interactive review support** for differential analysis, pathway enrichment, feature-network inspection, plot downloads, and PlantCyc/KEGG enrichment handling.
- **Connected PlantCyc database results to global project context**, allowing database and enrichment advisors to reason over both KEGG and PlantCyc resources.

*Co-authored with Codex (OpenAI GPT-5) and Shawn*

---

### 2026-05-21 — Layered annotation evidence strategy

- **Refined the annotation workflow into two evidence layers**: Layer 1 genome-informed KEGG/PlantCyc reaction candidates and Layer 2 spectral evidence from public MS2 libraries plus optional local/custom standard libraries.
- **Added strict core-adduct gating for Layer 1 candidates** during annotation filtering, so KEGG/PlantCyc reaction-derived candidates must pass high-confidence adduct rules before entering review and non-redundant outputs.
- **Added layered evidence fields** to expand, collapse, final non-redundant, redundancy audit, and AI evidence bundles: `annotation_layer`, `evidence_scope`, `core_adduct_match`, `strict_genome_adduct_pass`, and `metminer_confidence_level`.
- **Updated AI annotation review prompts and documentation** so the reviewer can validate Layer 1 Level 3 candidates with MS2 evidence, while also allowing public/local spectral evidence to supplement metabolites missing from reaction databases.

*Co-authored with Codex (OpenAI GPT-5) and Shawn*

---

### 2026-05-08 — PlantCyc/KEGG organism database construction

- **Added a PlantCyc database construction toolkit** under `Toolkits`, including SmartTable upload guidance, LC-MS-oriented compound filtering, metID-compatible MS1/MS2 database generation, and metpath pathway database generation.
- **Added optional Fiehn CFB/ClassyFire classification** with a default-off Shiny option, user warning, throttled requests, retry handling, and a persistent local PlantCyc classification cache reused by PlantCyc ID and InChIKey.
- **Separated PlantCyc MS1 and MS2 compound backgrounds for CoA handling**: CoA/acyl-CoA derivatives are excluded from the MS1-only library, retained as MS2-eligible candidates, and accompanied by a CoA diagnostic-fragment rule table.
- **Added conservative KEGG organism database helpers** that infer organism metabolite backgrounds from gene/KO/EC-supported reactions rather than full reference pathway compound lists.
- **Added a KEGG organism database Shiny toolkit** with a cached green-plant organism selector populated from KEGG organism entries.
- **Extended KEGG database construction to build MS2 databases from massdbbuildin**, using KEGG compound IDs as the primary join key and exporting bare KEGG IDs without `cpd:` or `path:` prefixes.
- **Added KEGG pathway review QC and an exportable AI prompt** for weakly supported pathways, allowing external LLM curation without integrating AI into the KEGG toolkit.

*Co-authored with Codex (OpenAI GPT-5) and Shawn*

---

## v0.1.0.9004 / app 2.0.1 (development)

### 2026-05-05 — AI-assisted annotation review and literature-aware chat

- **Added the AI Annotation Reviewer module** for evidence-based metabolite annotation review using user-configured LLM providers.
- **Built a chat-style Shiny interface** with a collapsible settings sidebar, provider/model/API configuration, local desktop config persistence, language selection, and in-chat asynchronous progress indicators.
- **Supported OpenAI-compatible providers, Gemini, DeepSeek, Qwen, Kimi, and Grok**, with provider-specific model choices and editable custom model IDs. DeepSeek defaults now use `deepseek-v4-flash`, with `deepseek-v4-pro` available and legacy `deepseek-chat` / `deepseek-reasoner` retained as compatibility options.
- **Collected structured annotation evidence bundles** from non-redundant annotation tables, redundancy audit rows, raw metID candidates, feature-level MS1/MS2 summaries, feature-network roles, recurrent-ion status, and user-supplied LC-MS conditions.
- **Added optional paper-search MCP integration** triggered by a checkbox or `@agent`, `@paper`, `@mcp`, and `@literature` chat tags. Literature citations are restricted to returned paper-search results and missing DOI values must be reported explicitly.
- **Separated full Review prompts from follow-up Chat prompts**, so follow-up questions answer only the current request rather than repeating the complete annotation review.
- **Added output language control** with Simplified Chinese listed first and English as the default; language instructions are enforced in both system and user prompts.
- **Documented the module** in `inst/app/ai_annotation_principles.md`.

*Co-authored with Codex (OpenAI GPT-5) and Claude Code*

---

## v0.1.0.9003 (development)

### 2026-05-03 — Codex-assisted: feature network interpretation and parameter optimization UX

- **Refined noise removal Step 0 as blank-informed intensity masking** rather than a general per-peak intensity cutoff. The method now requires sufficient blank samples and no longer silently falls back to sample-wide intensity distribution filtering when blanks are unavailable.
- **Kept blank filtering as the recommended post-picking background control** because Tidymass/massprocesser carries blank files through peak picking as samples but does not automatically subtract or remove blank-like biological sample intensities.
- **Split raw-data parameter optimization into two explicit actions**:
  - Step 1 estimates `ppmCut` and produces the PPM cutoff plot.
  - Step 2 optimizes the remaining peak-picking parameters using the reviewed `ppmCut`.
- **Preserved decimal ppm values** in `paramounter_part1()` and `paramounter_part2()`. XCMS accepts numeric `ppm`, so MetMiner no longer forces recommended ppm values to integers.
- **Connected the optimization UI `cutoff` controls to the underlying `paramounter_part1()` and `paramounter_part2()` calls**, so changing the UI value now affects the optimization.
- **Refactored Feature Network into analysis and visualization views**. Analysis focuses on producing network tables; visualization focuses on interactive graph, sub-network inspection, and spectra interpretation.
- **Added click-driven spectra interpretation for feature sub-networks**:
  - node selection shows an MS1 lollipop spectrum for all nodes in the selected sub-network;
  - ISF/neutral-loss relationships are shown as hoverable horizontal arrows between peaks;
  - clicking an MS1 peak displays the corresponding MS2 lollipop spectrum;
  - diagnostic fragments are annotated as peak-level hover information;
  - neutral losses in MS2 are annotated as fragment-pair differences rather than labels on single fragments.
- **Added raw XCMS chromatogram support** when Tidymass/massprocesser intermediate `xdata` objects are available under `POS/Result/intermediate_data` or `NEG/Result/intermediate_data`.
- **Removed the System Console module** from the app shell and data-import server because it did not provide useful workflow feedback.
- **UI polish**: fixed low-contrast title styling in noise removal and missing-value imputation pages.

*Co-authored with Codex (OpenAI GPT-5)*

---

## v0.1.0.9002 (development)

### 2026-05-02 — Per-peak intensity noise filter

- **New function `find_noise_intensity()`** — marks individual peak intensities as `NA` when they fall below a noise threshold estimated from the data, rather than a hard cutoff.
  - **Blank-based method**: estimates per-feature noise floor as `blank_mean + k × blank_SD` from blank samples. The gold standard when blanks are available.
  - **Distribution-based method**: finds the log-intensity antimode between noise and signal peaks within each sample, with percentile fallback when no clear bimodal structure exists.
  - Auto-falls back from blank to distribution when fewer than 2 blank samples are present.
- **Integrated into noise-removal pipeline** (`mod_data_rm_noise.R`) as Step 0, upstream of blank ratio, MV, and RSD filters. UI controls added with method-dependent parameter panels.
- **Documentation** — `inst/app/intensity_filter_principles.md` covering algorithm details, parameter guidance, and comparison with existing filters.

*Co-authored with Claude Code*

---

## v0.1.0.9001 (development)

### 2026-04-29 — Codex-assisted: MS2-audited ISF network workflow

- Added audited MS2 evidence for ISF detection, including stricter post-`mutate_ms2()` assignment checks to reduce false feature-to-spectrum matches when broad MS1/MS2 tolerances are used.
- Added optional MS2 ZIP upload and matching inside the Feature Network workflow after normalization, so discarded raw features are not needlessly matched to spectra.
- Added positive/negative final merged feature networks with cross-polarity redundancy edges.
- Refined the Feature Network UI: the sidebar stays open by default, parameter groups are collapsed, MS2 controls are conditional, and network rendering controls now live beside the relationship network panel.

*Co-authored with Codex (OpenAI GPT-5)*

---

## v0.1.0.9000

### 2026-04-28 — Code refactoring & standardization

- **Refactor `find_noise_multiple()`** — removed dead computation (unused `mutate_variable_na_freq` loop, fragile `key` column manipulation), added optional RSD-based filtering (`do_rsd`, `rsd_cutoff` params), return `mass_dataset` directly instead of a list. `do_cleaning_pipeline()` in `mod_data_rm_noise.R` now delegates to `find_noise_multiple()` (was: ~60 lines of duplicated logic, now: 15-line wrapper).
- **Extract `create_progress_handlers()` factory** — a single definition in `utils_ui.R` replaces three copy-pasted progress modal trios in `mod_data_import.R`, `mod_data_normalize.R`, and `mod_feature_network.R`. All Shiny functions use explicit `shiny::` namespace for cross-file closure safety.
- **Extract `.paramounter_setup()` helper** — deduplicates ~50 lines of file enumeration, parallel plan setup, and chunk calculation shared between `paramounter_part1()` and `paramounter_part2()`. Added guard for `cut(breaks <= 1)` edge case.
- **POS/NEG execution blocks refactored to for-loops** — 5 pipeline modules (`mod_data_rm_noise`, `mod_data_rm_outlier`, `mod_data_mv_impute`, `mod_data_normalize`, `mod_feature_network`) now iterate over a `polarities` list instead of copy-pasting positive/negative blocks.
- **12 empty placeholder files** now contain `# TODO` stubs describing their planned content.
- **All comments standardized to English** — 27 Chinese comments across `fct_qc_report.R`, `mod_data_overview.R`, and `report_template.qmd` translated.
- **Net**: 23 files, +366/−425 lines.

*Co-authored with Claude Opus 4.7*

---

### 2026-04-28 — Codex-assisted: feature network structure fix

- Fixed the structural layout of `fct_feature_network.R` and `mod_feature_network.R` — clarified function boundaries, improved documentation, and corrected edge-case handling in the network extraction and visualization pipeline.

*Co-authored with Codex (OpenAI)*

---

### 2026-04-27 — Feature relationship network (Codex-assisted)

- **New module**: `mod_feature_network` (520 lines) — full Shiny UI and server for interactive feature relationship network detection and visualization. Supports isotope, adduct, and in-source fragment (ISF) detection; interactive (visNetwork) and static (igraph) views; sub-network filtering; edge confidence scoring; and empirical compound collapse via `collapse_to_pseudo_area()`.
- **New engine**: `fct_feature_network.R` (919 lines) — core algorithms for feature relationship detection in LC-MS metabolomics data:
  - `detect_feature_relationships()` — orchestrates isotope, adduct, and ISF edge detection
  - `detect_isotope_edges()` / `detect_mass_difference_edges()` — sliding-window mass difference search with RT filtering via `findInterval`
  - `build_feature_correlation_cache()` — optimized Pearson correlation fast path (matrix multiplication, no NAs)
  - `edge_confidence()` — composite score from mass error, RT difference, and abundance correlation
  - `add_qc_ratio_stability()` — QC-sample ratio stability annotation
  - `collapse_to_pseudo_area()` — PCA-driven (PC1) feature sub-network collapse into empirical compound pseudo-areas
  - Plant-specific dictionaries: `default_plant_neutral_loss_table()`, `default_plant_fragment_ion_table()`, plus adduct and neutral loss tables
  - Graph conversion: `as_feature_igraph()`, `set_feature_network()`, `extract_feature_network()`

*Co-authored with Codex (OpenAI)*

---

### 2026-03-30 — Project documentation

- Added `README.md` with architecture diagram, feature list, and Phase 1–4 roadmap.

### 2026-03-21 — Project initialization

- Initial golem scaffold, package structure, `DESCRIPTION`, `NAMESPACE`, and `LICENSE`.
