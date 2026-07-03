# AI-Assisted Annotation Review: Principles and Implementation

## Purpose

Metabolite annotation in untargeted LC-MS is rarely decided by one signal.
The same compound name can appear across multiple features because of
isotopes, adducts, in-source fragments, recurrent same-m/z ions, polarity
differences, and database-level mass matches. The AI Annotation Reviewer is
designed to help users inspect those competing explanations, not to replace
spectral evidence or manual judgement.

The module turns MetMiner's evidence tables into a compact review bundle and
asks a user-selected large language model (LLM) to reason over that bundle.
The LLM is instructed to judge annotation credibility from the provided
evidence, to separate metID annotation evidence from feature-network
relationships, and to avoid fabricating literature or DOI information.

## Evidence Used by the Reviewer

For a queried compound name or feature ID, MetMiner collects:

- the annotation evidence layer: genome-informed KEGG/PlantCyc reaction
  candidate, public MS2 spectral evidence, or optional local/custom spectral
  evidence;
- strict core-adduct pass/fail status for genome-informed KEGG/PlantCyc
  candidates;
- non-redundant annotation rows from the annotation filter module;
- the full redundancy audit table, including keep/drop status and reasons;
- raw metID annotation candidates, including adduct and annotation level;
- feature-level MS1 information such as m/z, RT, abundance, and mode;
- top MS2 fragment summaries when spectra are available;
- feature-network roles, local edges, parent candidates, and recurrent-ion
  status;
- database source labels and source type, such as KEGG, PlantCyc, public MS2,
  or custom library;
- user-supplied LC-MS conditions, including instrument, column, mobile phases,
  gradient, ion source, and collision energy;
- optional literature evidence from paper-search MCP.

This evidence bundle is serialized as JSON and placed before the user's
question in the LLM message sequence. The current user request is kept as the
final variable part of the prompt so that follow-up questions can reuse the
same static evidence prefix.

## Review Logic

The reviewer follows the same logic expected from a human LC-MS analyst:

1. Separate Layer 1 genome-informed KEGG/PlantCyc reaction candidates from
   Layer 2 spectral evidence. Layer 1 candidates require strict core-adduct
   support and should remain putative unless MS2, standards, or orthogonal
   evidence supports them.
2. Use public MS2 libraries as the default Layer 2 evidence for validating
   Layer 1 candidates and for supplementing real metabolites that may be absent
   from reaction databases.
3. Treat PlantCyc and KEGG as distinct identifier namespaces. A PlantCyc
   pathway or reaction candidate is not automatically interchangeable with a
   KEGG candidate unless an explicit mapping or shared spectral evidence is
   present.
4. Treat local/custom standard libraries as optional enhancement evidence, not
   as a required input. When present, they can support the highest confidence
   only if RT, precursor, and MS2 evidence are available.
5. Prioritize higher annotation levels and stronger metID scores, but do not
   treat mass matching alone as confirmation.
6. Check whether the selected feature is locally supported by co-eluting
   isotope, adduct, or ISF relationships.
7. Inspect whether the same compound name or same m/z appears at multiple
   distant RTs.
8. Treat resolved recurrent ISFs and suspected interference features as
   low-confidence annotation candidates unless a local parent explains them.
9. Compare positive and negative mode evidence when both are present.
10. Use MS2 fragments as supporting or conflicting evidence, especially when
   diagnostic fragments or neutral-loss patterns are available.
11. Use literature only as biological context; literature cannot override weak
   spectral or chromatographic evidence.

The first Review action asks for a structured report with these sections:

- Verdict;
- Confidence score;
- Best-supported feature;
- Likely interference features;
- Key supporting evidence;
- Key conflicts;
- Literature context;
- Suggested next checks;
- References.

Follow-up Send messages use a different prompt mode. They answer only the
current user question and should not repeat the full annotation review unless
the user explicitly asks for it.

## LLM Provider and Language Handling

The UI supports OpenAI-compatible providers, Gemini, DeepSeek, Qwen, Kimi, and
Grok. Provider defaults and model choices are exposed in the sidebar, while
the model field remains editable for custom or newly released model IDs.

The chat language is selected independently from the provider. Simplified
Chinese is listed first, but English is the default. The selected language is
written into both the system prompt and the final user message so that review
answers and follow-up chat turns remain consistent. Compound names, feature
IDs, adduct labels, database IDs, DOI strings, and reference metadata are kept
unchanged.

Local configuration persistence is available only for local desktop sessions.
When enabled, provider, model, API key, endpoint, temperature, and language are
stored under the user's R configuration directory and loaded automatically in
future MetMiner sessions. This option is hidden in server deployments.

## Optional Paper Search

The reviewer can call paper-search MCP when either:

- the user checks "Use paper-search for this request"; or
- the chat message contains `@agent`, `@paper`, `@mcp`, or `@literature`.

MetMiner currently focuses paper-search on sources relevant to biology,
biochemistry, medicine, and plant science, such as PubMed, PubMed Central,
Europe PMC, bioRxiv, medRxiv, Semantic Scholar, Crossref, CORE, arXiv, and
Google Scholar when available.

The returned papers are compacted to title, authors, year, journal/source,
DOI, URL, and abstract. The system prompt explicitly requires that cited
papers must come from `literature_evidence.papers`. If a DOI is absent from
the paper-search result, the model must state that the DOI is not available
instead of inventing one. If paper-search fails or returns no papers, the
reviewer must say that no literature evidence was retrieved and must not cite
unsourced references.

## Interface Behavior

The module is implemented as a chat-like Shiny interface:

- the sidebar contains provider, model, API, language, paper-search, and LC-MS
  condition settings;
- the main panel contains the conversation history and composer;
- Review builds a full annotation evidence bundle;
- Send asks follow-up questions using the latest evidence bundle;
- progress is shown as an animated assistant message inside the chat stream;
- long LLM calls are handled asynchronously with `future` and `later`;
- buttons remain visually available, while an internal busy guard prevents
  duplicate concurrent requests.

The chat view automatically scrolls to the newest message. If required fields
such as API key, compound query, or message text are missing, MetMiner writes a
system notice into the chat instead of failing silently.

## Limitations

The AI reviewer is an evidence interpreter. It does not turn Level 3 mass
matches into confirmed annotations, and it cannot prove biological occurrence
without experimental or literature evidence. Final confidence still depends on
standard validation principles: authentic standards, RT match, high-quality
MS2 library match, orthogonal chromatography, and biological plausibility.
