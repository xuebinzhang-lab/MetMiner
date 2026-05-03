# Blank-Informed Intensity Masking - Principles & Algorithm

## Motivation

MetMiner uses `massprocesser::process_data()`, which in turn uses XCMS centWave peak picking. During peak picking, XCMS already applies intensity-related thresholds such as `noise`, `prefilter`, and `snthresh`. Those parameters decide which raw centroids, ROIs, and chromatographic peaks are eligible to enter the feature table.

The masking step described here does **not** replace those XCMS thresholds. It is a post-picking quality-control step on the final feature table. Its purpose is narrower:

- use blank samples to estimate feature-specific background;
- mask non-blank sample intensities that do not clearly exceed that background;
- let the downstream missing-value (MV) filter remove features that become mostly missing after masking.

This is useful because blank samples are carried through the Tidymass/massprocesser workflow as ordinary samples and class labels. They are not automatically used to subtract background or remove blank-dominated features during peak picking.

## Where It Fits

The recommended noise-removal sequence is:

```
Step 0: Optional blank-informed intensity masking
Step 1: Blank ratio filter
Step 2: MV filter (QC / biological groups)
Step 3: RSD filter (QC)
```

Step 0 operates at the measurement level: one feature by one sample. Step 1 and later operate at the feature level.

This distinction matters. A feature can pass XCMS peak picking and still contain weak, blank-like values in some biological samples. Masking those values as `NA` makes the MV filter more informative.

## Recommended Method: Blank-Informed Masking

Use this method when at least two blank samples are present.

For each feature `i`:

1. Extract intensities across blank samples.
2. Compute the blank mean and standard deviation:

```
blank_mean_i
blank_sd_i
```

3. If `blank_sd_i` is zero or missing, replace it with the median blank SD across features.
4. Compute a feature-specific background threshold:

```
cutoff_i = blank_mean_i + k * blank_sd_i
```

where `k` is `blank_sd_multiplier` (default `3`).

5. For every non-blank sample `j`, mask values below the cutoff:

```
if intensity_i,j < cutoff_i -> NA
```

Blank samples themselves are not masked by this step. They are used as the background reference.

### Why This Is Not Redundant With XCMS

XCMS `noise` and `prefilter` are applied to raw centroid/ROI detection. They are global or semi-global peak-picking constraints.

Blank-informed masking is applied after feature table construction. It asks a different question:

> Does this sample's integrated abundance for this feature exceed the observed blank background for the same feature?

That question cannot be answered by a single XCMS intensity threshold such as `noise = 500`.

## Advanced Fallback: Distribution-Based Masking

The `distribution` method remains available only as an advanced fallback when no blank samples are available. It estimates a sample-level cutoff from the log-intensity distribution of all features in each sample.

This method is intentionally not the recommended workflow because it relies on a stronger assumption: that the low-intensity portion of a sample's global feature distribution mostly represents noise. In metabolomics, genuine low-abundance metabolites can also live in that region, especially trace secondary metabolites or group-specific compounds.

Use it only when:

- no blank samples exist;
- XCMS/paramounter thresholds were already reviewed;
- the result is manually inspected with missing-value plots before and after masking.

If the distribution is not clearly bimodal, the method falls back to a low percentile cutoff.

## Parameters

| Parameter | Method | Default | Description |
|---|---|---|---|
| `method` | both | `"blank"` | `"blank"` is recommended. `"distribution"` is an advanced no-blank fallback. |
| `blank_sd_multiplier` | blank | `3` | Multiplier `k` in `blank_mean + k * blank_sd`. Higher values mask fewer intensities. |
| `percentile_fallback` | distribution | `0.01` | Percentile used when a distribution cutoff cannot be found. |
| `blank_label` | blank | `"Blank"` | Class label identifying blank samples in `sample_info$class`. |

## Relationship To Other Filters

| Filter | Level | Main purpose |
|---|---|---|
| XCMS `noise` / `prefilter` / `snthresh` | Raw peak picking | Prevent weak raw signals from becoming peaks |
| Blank-informed intensity masking | Measurement | Convert blank-like sample intensities to `NA` |
| Blank ratio filter | Feature | Remove features dominated by blank abundance |
| MV filter | Feature | Remove features with excessive missingness |
| RSD filter | Feature | Remove features with unstable QC abundance |

## Practical Guidance

1. Keep intensity masking optional and off by default.
2. Prefer `method = "blank"` whenever blank samples are available.
3. Use the blank ratio filter together with blank-informed masking. The ratio filter removes globally blank-dominated features; masking helps expose features with sporadic blank-like measurements.
4. Do not present distribution-based masking as a core noise-removal step. It is a fallback for no-blank datasets and should be used conservatively.
5. For suspected solvent or contaminant peaks that are strong and reproducible, blank ratio filtering, m/z/RT exclusion lists, and contaminant annotation are more appropriate than low-intensity masking.
