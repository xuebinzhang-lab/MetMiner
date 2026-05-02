# Per-Peak Intensity Noise Filter — Principles & Algorithm

## Motivation

In untargeted LC-MS metabolomics, not every detected peak represents a genuine signal. Low-intensity peaks often arise from electrical noise, chemical background, or contaminants rather than from biological analytes. Common practice in tools like XCMS and MZmine is to apply a hard intensity cutoff (e.g., `> 10⁴` counts) during peak picking. However, a fixed threshold risks:

- **False negatives**: genuine low-abundance features (e.g., trace secondary metabolites) may fall below the cutoff.
- **Batch insensitivity**: signal intensity varies across instruments, columns, and runs — a threshold calibrated on one batch may not transfer.

MetMiner's approach defers this filtering to the post-import stage, where the noise floor can be estimated **from the data itself** rather than from an arbitrary constant.

## Where It Fits

The intensity filter operates as **Step 0** in the noise-removal pipeline, before feature-level filtering:

```
Step 0: Per-peak intensity filter  ← marks individual low-intensity cells as NA
Step 1: Blank ratio filter         ← removes features dominated by blank signal
Step 2: MV filter (QC / group)     ← removes features with too many NAs
Step 3: RSD filter (QC)            ← removes features with high QC variability
```

After Step 0 sets low-intensity peaks to `NA`, the downstream MV filter (Step 2) naturally removes features that accumulate too many missing values across samples. This two-stage design decouples the *measurement-level* decision ("is this peak above noise?") from the *feature-level* decision ("is this feature sufficiently present to analyze?").

---

## Algorithm

Two methods are available, selected by the `method` parameter. **Blank-based** is the preferred approach when blank samples exist; **distribution-based** is a fallback for experiments without blanks.

### Method 1 — Blank-based noise estimation (`method = "blank"`)

This is the gold standard. Blank samples contain only background signal, so their intensities directly measure the noise distribution for each feature.

For each feature `i`:

1. Compute the mean (`μ_blank`) and standard deviation (`σ_blank`) of its intensity across all blank samples.
2. If `σ_blank = 0` (e.g., a single blank or all identical values), substitute the median `σ_blank` across all features to avoid a zero-width threshold.
3. Compute the noise cutoff:

```
cutoff_i = μ_blank,i + k × σ_blank,i
```

where `k` is `blank_sd_multiplier` (default 3).

4. For every **non-blank** sample `j`, set:

```
if intensity_i,j < cutoff_i → NA
```

Blanks themselves are not filtered — they serve only as the noise reference.

**Rationale for `k = 3`**: Under the assumption of approximately normal noise, `μ + 3σ` corresponds to a false-positive rate of ~0.15%. In practice, LC-MS noise is right-skewed, so the actual FPR is somewhat higher, but the downstream MV filter provides a second safeguard.

**Prerequisite**: at least 2 blank samples. With a single blank, `σ` cannot be estimated per feature, and the method auto-falls back to distribution-based.

### Method 2 — Distribution-based antimode detection (`method = "distribution"`)

When no blank samples are available, the noise floor is estimated from each sample's own intensity distribution. In untargeted metabolomics data, the log-intensity distribution across features within a single sample is typically **bimodal**:

- **Noise mode** (low intensity): thousands of low-abundance features from background, artifacts, and near-baseline peaks.
- **Signal mode** (higher intensity): genuine metabolite features.

The valley (antimode) between these two modes provides a natural, sample-specific noise cutoff.

#### Algorithm per sample `j`:

1. Take all non-NA, positive intensity values in sample `j`.
2. If fewer than 20 valid intensities remain, skip this sample (insufficient data for reliable density estimation).
3. Log₁₀-transform the intensities.
4. Estimate the probability density via kernel density estimation (`stats::density()`, Gaussian kernel, `n = 512`).
5. Locate all local minima in the density curve.
6. Find the **global maximum** (main signal peak) of the density.
7. Search for a local minimum **to the left** of the main peak (i.e., at lower log-intensity). If multiple valleys exist left of the main peak, take the rightmost one — the valley closest to the signal peak, which represents the noise-signal boundary.
8. If no valley is found left of the main peak, the distribution is likely unimodal (very clean data or very few noise features). In this case, fall back to:

```
cutoff_log_j = Q(p_fallback)
```

where `p_fallback` is `percentile_fallback` (default 0.01, i.e., bottom 1% of intensities).

9. Mark all peaks in sample `j` with `intensity < 10^cutoff_log_j` as `NA`.

#### Why log-transform?

LC-MS intensities span 4–6 orders of magnitude. On the linear scale, density estimation is dominated by the highest-intensity features and fails to resolve the noise region. Log transformation compresses the dynamic range and makes the noise mode detectable.

#### When the distribution method can fail

| Scenario | Behavior |
|---|---|
| Highly clean data (few noise features) | Distribution is nearly unimodal → falls back to percentile |
| Extremely noisy data (no clear signal peak) | Multiple ambiguous valleys → picks the last valley left of the highest peak; may be too conservative |
| Very few features (<20 valid intensities) | Sample skipped entirely — not enough data for density estimation |
| All features have similar intensity | No clear bimodal structure → falls back to percentile |

In all edge cases, the fallback percentile ensures the method degrades gracefully rather than producing spurious results.

---

## Parameters

| Parameter | Method | Default | Description |
|---|---|---|---|
| `method` | both | `"blank"` | Noise estimation strategy. Auto-falls back to `"distribution"` if < 2 blank samples exist. |
| `blank_sd_multiplier` | blank | `3` | Multiplier `k` in `μ_blank + k × σ_blank`. Higher = more conservative (fewer peaks marked NA). |
| `percentile_fallback` | distribution | `0.01` | Percentile used as cutoff when the antimode cannot be found. 0.01 = bottom 1%. Lower = more conservative. |
| `blank_label` | both | `"Blank"` | The class label that identifies blank samples in `sample_info$class`. |

---

## Comparison with Existing Filters

| Filter | Level | What it removes | Threshold source |
|---|---|---|---|
| **Intensity filter** (this) | Per-peak (cell) | Individual low-intensity measurements → NA | Data-driven: blank noise or per-sample distribution |
| **Blank ratio filter** | Per-feature (row) | Features where mean(sample)/mean(blank) < cutoff | User-specified ratio (default 3) |
| **MV filter** | Per-feature (row) | Features with excessive missing values in QC or sample groups | User-specified NA frequency |
| **RSD filter** | Per-feature (row) | Features with high RSD in QC injections | User-specified RSD % |

The intensity filter is the only one that operates at the *measurement* (cell) level. The other three operate at the *feature* (row) level. Together they form a layered defense: weak signals are first masked, then features that lose too many measurements are discarded.

---

## Practical Guidance

1. **If you have blank samples**: always use `method = "blank"`. This is the most defensible approach and directly measures the noise floor per feature.

2. **Tuning `blank_sd_multiplier`**: start with 3. If too many genuine low-abundance features are lost (check the message output for % matrix marked NA), reduce to 2. If noise features persist, increase to 4–5.

3. **If you have no blanks**: `method = "distribution"` is a reasonable fallback, but inspect the results. In the Shiny app, compare the MV distribution plots before and after — if a large fraction of features suddenly gain many NAs, the cutoff may be too aggressive (reduce `percentile_fallback`).

4. **Combined with blank ratio filter**: these two filters complement each other. The intensity filter catches per-peak noise within features that might still pass the feature-level blank ratio test (e.g., a feature with adequate mean sample/blank ratio but sporadic low-intensity measurements in individual samples).
