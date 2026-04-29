# Feature Relationship Network — Principles & Algorithm

## Overview

The feature network engine detects relationships among LC-MS features (peaks) and groups them into **empirical compounds** for downstream metabolomics analysis. It is plant-metabolomics focused, with dictionaries tailored to plant secondary metabolites and optional MS2 evidence auditing for in-source fragment (ISF) relationships.

A **feature** here is a chromatographic peak characterized by (m/z, retention time, abundance). The engine detects three relationship classes:

1. **Isotope edges** — same compound, different isotopic composition (e.g., [M] and [M+1])
2. **Adduct edges** — same compound, different adduct forms (e.g., [M+H]+ and [M+Na]+)
3. **ISF edges** (In-Source Fragments) — fragments of the same compound produced during ionization (e.g., neutral loss of H₂O, CO₂, sugar moieties)

When positive and negative ion modes are both available, the engine can also build a final cross-polarity network that connects redundant signals across acquisition modes.

---

## Algorithm Pipeline

### Step 1 — Candidate search via sliding mass-difference window

Features are sorted by RT, then m/z. For each feature `i`:

1. **RT window**: `findInterval()` (O(log n)) selects candidates within `rt[i] ± rt_tolerance`.
2. **Mass filter**: only features with `m/z > m/z[i]` are kept.
3. **Dictionary match**: for each mass difference in the dictionary (isotope / adduct / ISF), the observed delta `m/z[high] − m/z[low]` is tested against the expected delta ± ppm tolerance.

This yields candidate pairs `(low_mz_feature, high_mz_feature)`.

### Step 2 — Abundance correlation filter

For each candidate pair, the Pearson correlation of abundance profiles across all samples is computed:

- **Fast path**: if no missing values exist, uses matrix multiplication `source_row %*% t(candidate_rows)` after row-centering and scaling — O(1) per pair after cache.
- **Fallback**: `stats::cor(use = "pairwise.complete.obs")` for data with NAs.

Pairs with correlation below `cor_cutoff` (default 0.7) are discarded.

### Step 3 — Edge confidence scoring

A composite score (0–1) from three components:

```
confidence = 0.45 × mass_score + 0.20 × rt_score + 0.35 × cor_score
```

- `mass_score = max(0, 1 − mz_error_ppm / ppm_tolerance)`
- `rt_score = max(0, 1 − rt_diff / rt_tolerance)`
- `cor_score = max(0, abundance_correlation)`

Weights prioritize mass accuracy (45%) and abundance correlation (35%) over RT matching (20%), since RT in LC-MS is less reproducible than mass accuracy.

### Step 4 — QC ratio stability annotation

For each edge, the ratio `abundance[high] / abundance[low]` is computed in QC samples. The RSD (relative standard deviation) of this ratio across QC injections measures consistency. A stable ratio suggests a genuine physicochemical relationship rather than a spurious correlation.

### Step 5 — Isotope-specific intensity filter

Isotope edges are additionally filtered: the mean abundance ratio `[M+1] / [M]` must be ≤ `isotope_intensity_ratio_max` (default 0.8). This prevents mislabeling of near-equal-abundance features as isotopes.

### Step 6 — Optional MS2-audited ISF evidence

ISF relationships are first proposed from MS1-level evidence: mass difference, RT proximity, and abundance correlation. If MS2 spectra are available, the algorithm adds an evidence audit layer rather than trusting every MS1-MS2 match directly.

This is important because users may run `mutate_ms2()` with broad matching windows, for example `ms1.ms2.match.mz.tol = 15` ppm and `ms1.ms2.match.rt.tol = 30` sec, to increase MS2 coverage. Broad matching can cause multiple features to point to the same spectrum or assign a spectrum to a nearby but incorrect feature.

The audit therefore:

1. Re-checks each feature-MS2 assignment with stricter precursor m/z and RT tolerances (`ms2_mz_tol_ppm`, `ms2_rt_tol`).
2. Flags shared spectra as `unique`, `shared_best_owner`, `shared_non_owner`, or `bad_match`.
3. Uses only reliable spectrum ownership as strong evidence.
4. Checks whether the proposed ISF feature m/z appears in the parent feature's MS2 spectrum (`ms2_fragment_mz_tol`).
5. Optionally compares parent/fragment spectra by reverse MS2 similarity when both features have reliable spectra.

The resulting edge table keeps the original MS1 relationship but annotates it with evidence columns:

- `evidence_level`: MS1-only, MS2 fragment-supported, MS2 spectrum-supported, or cross-polarity evidence.
- `evidence`: a compact explanation of the supporting rule.
- `from_ms2_quality` / `to_ms2_quality`: audited spectrum assignment quality.
- `same_ms2_spectrum`: whether both features were assigned to the same spectrum, which is treated cautiously.
- `ms2_similarity_score`, `ms2_matched_peaks`, and `ms2_matched_ratio`: optional spectrum similarity evidence.

MS2 evidence upgrades confidence in an ISF edge; it does not replace the MS1 co-elution and correlation filters.

### Step 7 — Final cross-polarity network

If both positive and negative mode normalized objects are available, MetMiner can merge their within-mode networks into a final network:

1. Within-mode feature IDs are prefixed as `pos::feature_id` and `neg::feature_id`.
2. Positive and negative features are converted to candidate neutral masses using conservative adduct rules.
3. Cross-polarity edges are added when neutral mass, RT, and abundance correlation agree.
4. Cross-polarity ISF edges are added when a neutral-loss relationship is plausible across modes.

This is designed for cases where a parent compound is more easily detected in one polarity while an ISF or redundant signal is more easily detected in the other polarity. The final network is therefore a redundancy map across acquisition modes, not a replacement for polarity-specific empirical compound quantification.

---

## Empirical Compound Collapse (`collapse_to_pseudo_area`)

After edge detection, connected components in the network graph represent putative **empirical compounds** (a parent metabolite + its isotopologues + adducts + fragments). For each sub-network:

1. The feature with the **highest mean abundance** is selected as the **base feature**.
2. **PC1 pseudo-area**: expression data of all features in the sub-network are standardized (center + scale) and projected onto the first principal component. This captures the shared abundance pattern while suppressing noise from individual features. The PC1 scores become the **pseudo-area** of the empirical compound.

This produces a single abundance vector per empirical compound, suitable for downstream statistical analysis.

Cross-polarity merged networks are used for redundancy inspection and relationship visualization. Pseudo-area tables remain polarity-specific because positive and negative modes can have different ionization efficiencies, missingness patterns, and sample-level preprocessing histories.

---

## Plant-Specific Dictionaries

Two built-in dictionaries support plant metabolomics:

- **Neutral loss table**: H₂O, CO₂, NH₃, CH₂O₂, hexose (−162 Da), pentose (−132 Da), malonyl (−86 Da), SO₃, H₂SO₄, H₃PO₄, etc.
- **Fragment ion table**: diagnostic fragments for flavonoids (quercetin, kaempferol, apigenin aglycones), phenolic acids (caffeic, ferulic, coumaric, gallic acid fragments), phospholipids, fatty acids, amino acid immonium ions.

Both can be overridden with custom tables via `adduct_table` and `neutral_loss_table` parameters.

---

## Implementation Notes

- **Storage**: The network edge table is stored in `object@other_files$feature_network`, keeping compatibility with the `massdataset` S4 class without modifying its slot definition.
- **MS2 timing**: MS2 ZIP upload and `mutate_ms2()` matching are performed in the Feature Network workflow after normalization. This avoids matching spectra to raw features that may later be removed during cleaning or normalization.
- **MS2 audit helper**: `audit_ms2_assignment()` exposes the stricter post-`mutate_ms2()` assignment audit as a standalone helper.
- **Cross-polarity merge helper**: `merge_polarity_feature_networks()` builds the final positive/negative merged network with prefixed feature IDs.
- **Graph conversion**: `as_feature_igraph()` converts the edge table to an igraph directed graph, with vertex attributes from `variable_info`.
- **Performance**: The RT window search uses `findInterval()` (O(log n)) rather than scanning all features. The Pearson fast path avoids the overhead of `stats::cor()` per pair (which would be O(n²) calls without the cache).

---

## References

- The ISF detection strategy is adapted from the MetMiner v1 pipeline (Wang et al., 2024, JIPB).
- Neutral loss and fragment ion dictionaries are curated from plant metabolomics literature and MS/MS spectral libraries.
