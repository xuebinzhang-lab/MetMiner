# MetMiner v2 — Changelog

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
