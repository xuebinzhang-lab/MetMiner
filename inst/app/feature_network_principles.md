# Feature Relationship Network: Principles and Implementation

## Purpose

LC-MS does not observe a metabolite as a single perfectly isolated signal. A
true chromatographic compound can generate several related features, including
natural isotopes, adduct ions, dimers, neutral-loss ions, and in-source
fragments (ISFs). These signals are often annotated independently by spectral
search engines, which creates redundant or misleading metabolite tables.

MetMiner's feature network is designed to separate three questions:

1. Which features are likely co-eluting ion forms of the same local compound?
2. Which repeated ions at different retention times are likely recurrent
   fragments or unresolved isomer-like signals?
3. Which annotation rows should be retained, removed, or flagged for review in
   the final non-redundant table?

The network therefore has two biological layers and one downstream annotation
filter layer:

- RT-local feature relationship network.
- Cross-RT recurrent ion network.
- Annotation-level redundancy and interference audit.

---

## Theoretical Basis

### 1. RT-local co-elution relationships

Natural isotopes, adducts, and ISFs produced from the same precursor compound
should usually have nearly identical retention times. They also tend to have
positively correlated abundance profiles across biological samples because they
are different ion forms of the same chromatographic source.

For this reason, the first feature-network layer is built only inside a
retention-time tolerance window. Candidate edges are evaluated with:

- m/z difference or neutral-loss rules;
- RT proximity;
- abundance correlation across samples;
- optional MS2 fragment or spectrum evidence;
- isotope intensity plausibility for isotope edges.

This layer answers a local question: within one RT window, which feature is the
putative parent and which features are isotope, adduct, or ISF signals?

### 2. Same m/z at different retention times

A single metabolite should not normally produce the same intact feature at many
unrelated retention times. If an extremely narrow m/z appears repeatedly at
different RTs, and the MS2 spectra are highly similar, these features are often
not multiple independent copies of the same metabolite. More likely
explanations include:

- different parent metabolites generating the same diagnostic ISF;
- structurally related isomers sharing the same fragment ion;
- low-abundance parent ions that are not confidently detected;
- annotation leakage, where a fragment is repeatedly matched to the same
  compound name;
- chromatographic or peak-picking artifacts.

Therefore, MetMiner does not merge cross-RT same-m/z features into one ordinary
feature sub-network. Instead it builds a separate recurrent ion layer. Each
recurrent group has a center m/z node and multiple RT-local feature instances.
When the RT-local network can resolve an instance as an ISF of a local parent,
that parent is connected to the instance.

This design keeps examples such as m/z 195 and m/z 177 separate. If m/z 177 can
be explained by a virtual neutral loss from a higher recurrent ion such as
`m/z 195 -> -H2O`, the explanation is represented as a virtual node in the
m/z 177 recurrent network. It does not force 195 and 177 into the same local
feature sub-network.

### 3. Annotation redundancy and interference

Annotation search results are evaluated after both network layers are built.
Rows with the same compound name or compound key are no longer collapsed only
by highest intensity. MetMiner now uses network context:

- RT-local parent candidates are preferred as representative rows.
- Resolved isotope, adduct, and ISF rows can be merged or removed when they are
  explained by a local parent.
- Cross-RT same-m/z recurrent ions are flagged as possible interference.
- Recurrent ions resolved as ISF in their local RT window are removed from the
  final non-redundant table when an alternative parent or unresolved candidate
  explains the same compound/mz group.
- Unresolved recurrent ions are retained but marked for review rather than
  being deleted automatically.

The final table therefore preserves evidence while reducing obvious redundant
annotations.

---

## Practical Workflow

### Step 1: Build the RT-local feature network

For each polarity, features are sorted by RT and m/z. The algorithm uses an RT
window search to find nearby candidates, then evaluates known mass-difference
rules:

- isotope mass differences;
- adduct mass offsets;
- neutral losses and ISF rules;
- optional cross-polarity neutral-mass rules during the merge step.

For each candidate edge, MetMiner records score components:

- `mz_score`;
- `rt_score`;
- `cor_score`;
- `ms2_score`;
- `rule_score`;
- `intensity_score` for isotope plausibility.

These components are combined into `relation_score` and
`confidence_class`. Abundance correlation is used as a scoring component rather
than a hard within-mode gate, because weakly correlated edges can still be
useful exploratory evidence when mass, RT, and MS2 information agree.

### Step 2: Assign local feature roles

Connected components in the RT-local network are interpreted as local
empirical-compound candidates. MetMiner scores parent candidates using edge
direction, relation type, intensity, and annotation support, then labels
features with roles such as:

- `putative_parent`;
- `isotope_of_parent`;
- `adduct_of_parent`;
- `isf_fragment_of_parent`;
- `network_neighbor`;
- `unresolved_feature`.

These roles are stored on the mass_dataset object and reused by annotation
validation and filtering.

### Step 3: Build the recurrent ion network

After local roles are available, MetMiner scans all features for very narrow
m/z groups that occur at multiple RTs. A group is retained when it has at least
two instances and spans more than the local RT tolerance.

The recurrent network contains:

- a central recurrent m/z node;
- one node for each RT-local feature instance;
- resolved parent nodes when local evidence identifies an instance as ISF;
- optional virtual neutral-loss explanation nodes such as `m/z 195 -> -H2O`.

The recurrent view is an interpretation layer, not a replacement for the
RT-local network. Selecting a recurrent feature instance in Shiny links back to
the MS1 spectrum, MS2 spectrum, and XCMS chromatogram for that real feature.
Selecting the recurrent center node summarizes all RT instances in the group.

### Step 4: Validate annotations with the network

Annotation candidates are re-ranked by combining metID scores with network
support and network conflict evidence. For each feature, MetMiner records:

- the selected compound candidate;
- network support and conflict scores;
- feature role interpretation;
- sub-network parent hypothesis;
- evidence that explains whether the annotation is locally consistent.

This step reduces cases where a fragment receives the same confident compound
name as its parent or as other repeated fragments.

### Step 5: Build the non-redundant annotation table

The final filtering stage creates two outputs:

- `annotation_nonredundant_table.csv`: rows kept for downstream use;
- `annotation_redundancy_audit.csv`: all candidate rows with keep/drop status
  and explanations.

The audit table includes recurrent-ion fields:

- `network_roles`;
- `parent_feature_ids`;
- `recurrent_ion_groups`;
- `recurrent_center_mz`;
- `recurrent_instance_count`;
- `recurrent_parent_resolved_n`;
- `recurrent_status`;
- `suspected_interference`;
- `interference_reason`.

Important recurrent statuses include:

- `resolved_recurrent_isf`: the repeated m/z instance is locally resolved as
  an ISF and can be removed when a better representative exists.
- `recurrent_network_contains_isf`: the retained network representative
  contains recurrent ISF evidence in its members.
- `recurrent_parent_candidate`: the repeated m/z instance is itself the best
  local parent candidate.
- `unresolved_recurrent_ion`: repeated m/z across RTs with no local parent
  explanation; retained for review.

---

## Shiny Interpretation Views

The Feature Network module provides three graph views:

1. **Single ion mode**: RT-local feature network for positive or negative mode.
2. **Recurrent ion network**: cross-RT repeated m/z interpretation layer.
3. **Final merged polarity network**: cross-polarity redundancy map.

In the single-mode view, edge color reflects abundance correlation, edge width
reflects confidence, and dashed edges indicate weak or exploratory relations.

In the recurrent view:

- green/orange instance nodes are real feature instances;
- blue parent nodes are RT-local parent hypotheses;
- grey neutral-loss nodes are virtual explanations;
- the MS1 panel shows all RT instances in the recurrent group;
- clicking a real feature instance drives MS2 and raw EIC panels.

---

## Implementation Notes

- The RT-local feature network is stored in
  `object@other_files$feature_network`.
- Local role interpretation is stored in
  `object@other_files$feature_network_roles`.
- Recurrent ion groups, nodes, and edges are stored in
  `object@other_files$recurrent_ion_network`.
- Annotation validation is stored in
  `object@other_files$annotation_validation`.
- Cross-polarity merged networks are used for redundancy inspection and
  visualization. Pseudo-area tables remain polarity-specific because positive
  and negative modes have different ionization efficiencies and missingness
  structures.
- MS2 evidence is used as an audit layer. It upgrades confidence when precursor
  ownership and fragment evidence are reliable, but it does not override the
  requirement for RT-local reasoning.

---

## Recommended Interpretation

The non-redundant table should not be read as a simple deletion list. It is a
ranked, network-audited annotation table. Rows marked as recurrent unresolved
ions are not necessarily false positives; they may represent isomers, shared
diagnostic fragments, or parent ions below detection. Rows marked as resolved
recurrent ISF are stronger interference candidates because the local RT network
has already found a plausible source feature.

This is why MetMiner keeps both the final table and the full audit table.
