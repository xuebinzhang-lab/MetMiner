# Global MetMiner Bot Advisors

## Purpose

The Global MetMiner Bot is a workflow-aware assistant embedded in the Shiny
app. It helps users reason about parameters and intermediate results across
the active MetMiner project. The Bot is intended for decision support, not for
automatic data curation.

Advisor answers are grounded in the current project state whenever possible:
available positive and negative `mass_dataset` objects, module parameters,
selected features or pathways, annotation and filtering tables, differential
analysis results, enrichment outputs, KEGG or PlantCyc database build results,
and project-level `sample_info`.

## Advisor Commands

Users can select an advisor from the Bot command menu or type advisor tags in
the chat composer:

- `@data-import-advisor`
- `@noise-filter-advisor`
- `@outlier-advisor`
- `@missing-value-advisor`
- `@normalization-advisor`
- `@feature-network-advisor`
- `@annotation-advisor`
- `@annotation-filter-advisor`
- `@differential-advisor`
- `@enrichment-advisor`
- `@database-advisor`

Each advisor receives only the context relevant to that workflow stage. For
example, the missing-value advisor receives missingness summaries, sample
metadata, and available POS/NEG objects, while the enrichment advisor receives
differential results, annotation-filter outputs, chosen ID type, and
KEGG/PlantCyc pathway-database status.

## Sample Design Awareness

The Bot treats `sample_info` as an extensible experimental-design table rather
than a fixed set of columns. It profiles user-provided columns, including
custom attributes such as tissue, genotype, treatment, dose, time point,
location, batch, or injection order.

The sample-design summary includes:

- row and column counts;
- core columns present, such as `sample_id`, `class`, `group`, `batch`, and
  `injection.order`;
- user attribute columns beyond the core fields;
- missing-value fraction and unique-level count for profiled columns;
- top levels for categorical or low-cardinality fields;
- numeric summaries for numeric fields;
- inferred design columns and common level combinations;
- warnings for heterogeneous designs, many sample strata, or singleton
  combinations.

This allows the Bot to warn when global missing-rate summaries may hide
group-specific absence, when imputation should be checked within relevant
strata, or when a single ion mode may have an injection/acquisition problem
even if the opposite mode looks normal.

## Guidance Rules

Advisor prompts are deliberately conservative:

- separate observed evidence from recommendations;
- state when a recommendation is under-supported because plots, annotations,
  sample metadata, or pathway IDs are missing;
- avoid treating mass matching alone as annotation confirmation;
- distinguish technical missingness from biologically plausible sparsity;
- treat PlantCyc and KEGG identifiers as different enrichment namespaces;
- explain practical next checks instead of making irreversible decisions.

The Bot can help users choose reasonable next actions, but final decisions
should still be based on chromatograms, spectra, sample metadata, biological
design, and experiment-specific QC knowledge.
