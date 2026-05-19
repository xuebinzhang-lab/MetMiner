#' Homepage UI Module
#'
#' @param id Module id.
#' @noRd
mod_homepage_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .home-page {
        min-height: calc(100vh - 72px);
        background: #f8f9fa;
        color: #26343d;
      }
      .home-hero {
        background: linear-gradient(135deg, #008080 0%, #20c997 100%);
        color: #ffffff;
        padding: 64px 0 96px;
        position: relative;
        overflow: hidden;
      }
      .home-hero-grid {
        display: grid;
        grid-template-columns: minmax(0, 0.95fr) minmax(420px, 1.05fr);
        gap: 34px;
        align-items: center;
      }
      .home-kicker {
        color: rgba(255,255,255,0.88);
        font-weight: 800;
        font-size: 0.92rem;
        margin-bottom: 0.65rem;
      }
      .home-title {
        font-size: 2.85rem;
        line-height: 1.17;
        font-weight: 850;
        margin: 0 0 1rem;
        letter-spacing: 0;
      }
      .home-lead {
        color: rgba(255,255,255,0.92);
        font-size: 1.08rem;
        line-height: 1.72;
        margin-bottom: 1.25rem;
      }
      .home-actions .btn {
        border-radius: 7px;
        font-weight: 800;
        padding: 0.68rem 1.02rem;
      }
      .btn-home-primary {
        background: #ffffff;
        color: #008080;
        border: 1px solid #ffffff;
        box-shadow: 0 8px 20px rgba(0,0,0,0.16);
      }
      .btn-home-primary:hover {
        background: #f4fbfb;
        color: #006f6f;
      }
      .btn-home-secondary {
        color: #ffffff;
        background: transparent;
        border: 1px solid rgba(255,255,255,0.55);
      }
      .btn-home-secondary:hover {
        color: #ffffff;
        background: rgba(255,255,255,0.12);
      }
      .home-hero-visual {
        background: #ffffff;
        border: 1px solid rgba(255,255,255,0.35);
        border-radius: 20px;
        padding: 18px;
        box-shadow: 0 20px 50px rgba(0,0,0,0.26);
        transform: rotate(-2deg);
      }
      .home-hero-visual img {
        width: 100%;
        max-width: 760px;
        display: block;
        border-radius: 14px;
      }
      .home-quick-stats {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-top: 18px;
      }
      .home-stat {
        background: rgba(255,255,255,0.1);
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 8px;
        padding: 12px;
      }
      .home-stat-value {
        color: #ffffff;
        font-size: 1.25rem;
        font-weight: 850;
      }
      .home-stat-label {
        color: #c7dbdc;
        font-size: 0.82rem;
        line-height: 1.35;
      }
      .home-section {
        padding: 28px 0;
      }
      .home-features-floating {
        margin-top: -66px;
        position: relative;
        z-index: 2;
        padding-bottom: 32px;
      }
      .home-section-title {
        font-size: 1.35rem;
        font-weight: 850;
        color: #12383b;
        margin: 0 0 14px;
      }
      .home-feature-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 14px;
      }
      .home-feature {
        background: #ffffff;
        border: 1px solid rgba(0,0,0,0.04);
        border-top: 5px solid transparent;
        border-radius: 12px;
        padding: 18px;
        min-height: 168px;
        box-shadow: 0 12px 30px rgba(0,0,0,0.08);
        transition: transform 0.24s ease, border-top-color 0.24s ease, box-shadow 0.24s ease;
      }
      .home-feature:hover {
        transform: translateY(-5px);
        border-top-color: #008080;
        box-shadow: 0 16px 34px rgba(0,0,0,0.11);
      }
      .home-feature-icon {
        width: 38px;
        height: 38px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: 8px;
        background: #e5f5f1;
        color: #008080;
        font-size: 1.1rem;
        margin-bottom: 0.9rem;
      }
      .home-feature-title {
        font-weight: 850;
        color: #24313a;
        margin-bottom: 0.55rem;
      }
      .home-feature-text {
        color: #5f6d76;
        line-height: 1.62;
        font-size: 0.94rem;
        margin: 0;
      }
      .home-showcase {
        background: #ffffff;
        border: 1px solid #dbe3e6;
        border-radius: 12px;
        padding: 18px;
        box-shadow: 0 10px 24px rgba(29,48,54,0.07);
      }
      .home-showcase .nav-tabs {
        border-bottom-color: #dbe3e6;
        margin-bottom: 18px;
      }
      .home-showcase .nav-link {
        color: #55636c;
        font-weight: 800;
        border-radius: 6px 6px 0 0;
      }
      .home-showcase .nav-link.active {
        color: #008080;
        border-color: #dbe3e6 #dbe3e6 #ffffff;
      }
      .home-figure-layout {
        display: grid;
        grid-template-columns: minmax(0, 1.35fr) minmax(320px, 0.65fr);
        gap: 22px;
        align-items: start;
      }
      .home-figure-frame {
        background: #f9fbfb;
        border: 1px solid #e0e7ea;
        border-radius: 8px;
        padding: 10px;
      }
      .home-figure {
        width: 100%;
        display: block;
        border-radius: 6px;
        background: #ffffff;
      }
      .home-figure-copy {
        border-left: 4px solid #008080;
        padding-left: 16px;
      }
      .home-figure-kicker {
        color: #008080;
        font-size: 0.86rem;
        font-weight: 850;
        margin-bottom: 0.55rem;
      }
      .home-figure-title {
        color: #15383c;
        font-weight: 850;
        font-size: 1.22rem;
        line-height: 1.32;
        margin-bottom: 0.8rem;
      }
      .home-figure-text {
        color: #5b6871;
        line-height: 1.7;
        margin-bottom: 1rem;
      }
      .home-bullet-list {
        padding-left: 1rem;
        margin: 0 0 1rem;
        color: #52616a;
        line-height: 1.65;
      }
      .home-output-box {
        background: #f2f7f7;
        border: 1px solid #d8e6e6;
        border-radius: 8px;
        padding: 12px;
        color: #4d5d65;
        line-height: 1.55;
      }
      .home-output-title {
        font-weight: 850;
        color: #1e4448;
        margin-bottom: 0.35rem;
      }
      .home-meta-band {
        background: #ffffff;
        border-top: 1px solid #dbe3e6;
        padding: 26px 0;
      }
      .home-two-col {
        display: grid;
        grid-template-columns: minmax(0, 0.95fr) minmax(320px, 1.05fr);
        gap: 26px;
      }
      .home-panel {
        background: #f8fbfb;
        border: 1px solid #dbe3e6;
        border-radius: 8px;
        padding: 18px;
      }
      .home-cite {
        border-left: 4px solid #2fb28f;
        padding-left: 14px;
        color: #56636c;
        line-height: 1.65;
        margin-bottom: 12px;
      }
      @media (max-width: 1180px) {
        .home-hero-grid,
        .home-figure-layout,
        .home-two-col {
          grid-template-columns: 1fr;
        }
        .home-feature-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 720px) {
        .home-title {
          font-size: 1.82rem;
        }
        .home-hero {
          padding-bottom: 70px;
        }
        .home-features-floating {
          margin-top: -44px;
        }
        .home-feature-grid,
        .home-quick-stats {
          grid-template-columns: 1fr;
        }
      }
    ")),
    div(
      class = "home-page",
      div(
        class = "home-hero",
        div(
          class = "container-fluid px-4 px-xl-5",
          div(
            class = "home-hero-grid",
            div(
              div(class = "home-kicker", "MetMiner2 | Plant LC-MS untargeted metabolomics"),
              h1(class = "home-title", "A practical workspace for evidence-integrated metabolite annotation"),
              p(
                class = "home-lead",
                "MetMiner2 keeps the original end-to-end plant metabolomics workflow, but shifts the center of gravity toward annotation confidence: feature-network evidence, species-specific PlantCyc/KEGG databases, AI-assisted review, ID mapping, and pathway interpretation."
              ),
              div(
                class = "home-actions d-flex flex-wrap gap-2",
                actionButton(ns("go_init"), "Start Analysis", icon = icon("play"), class = "btn-home-primary"),
                a(
                  href = "https://shawnwx2019.github.io/metminer-cookbook/",
                  target = "_blank",
                  class = "btn btn-home-secondary",
                  icon("book"),
                  "Documentation"
                ),
                a(
                  href = "https://github.com/ShawnWx2019/MetMiner",
                  target = "_blank",
                  class = "btn btn-home-secondary",
                  icon("github"),
                  "GitHub"
                )
              ),
              div(
                class = "home-quick-stats",
                div(class = "home-stat", div(class = "home-stat-value", "10"), div(class = "home-stat-label", "PlantCyc species databases")),
                div(class = "home-stat", div(class = "home-stat-value", "3"), div(class = "home-stat-label", "Evidence-centered figures")),
                div(class = "home-stat", div(class = "home-stat-value", "MS1"), div(class = "home-stat-label", "LC-MS filtered plant libraries")),
                div(class = "home-stat", div(class = "home-stat-value", "AI"), div(class = "home-stat-label", "Auditable annotation review"))
              )
            ),
            div(
              class = "home-hero-visual",
              img(src = "www/MetMiner.jpg", alt = "MetMiner logo")
            )
          )
        )
      ),
      div(
        class = "home-section home-features-floating",
        div(
          class = "container-fluid px-4 px-xl-5",
          h2(class = "home-section-title", "What changed in MetMiner2"),
          div(
            class = "home-feature-grid",
            div(
              class = "home-feature",
              div(class = "home-feature-icon", icon("network-wired")),
              div(class = "home-feature-title", "Feature Network"),
              p(class = "home-feature-text", "Links isotopes, adducts, in-source fragments, recurrent ions, MS/MS similarity, retention time, and abundance correlation to identify the most plausible core feature.")
            ),
            div(
              class = "home-feature",
              div(class = "home-feature-icon", icon("robot")),
              div(class = "home-feature-title", "AI-assisted review"),
              p(class = "home-feature-text", "Sends structured evidence, system prompts, and optional literature search results to LLMs while keeping the final annotation under human control.")
            ),
            div(
              class = "home-feature",
              div(class = "home-feature-icon", icon("database")),
              div(class = "home-feature-title", "PlantCyc/KEGG resources"),
              p(class = "home-feature-text", "Builds LC-MS-oriented MS1 databases and pathway backgrounds from licensed PlantCyc PGDB files or KEGG reaction evidence.")
            ),
            div(
              class = "home-feature",
              div(class = "home-feature-icon", icon("chart-line")),
              div(class = "home-feature-title", "Downstream interpretation"),
              p(class = "home-feature-text", "Connects annotation, differential analysis, compound ID mapping, and pathway enrichment so case-study results can move directly into reports.")
            )
          )
        )
      ),
      div(
        class = "home-section pt-0",
        div(
          class = "container-fluid px-4 px-xl-5",
          h2(class = "home-section-title", "Module overview"),
          div(
            class = "home-showcase",
            bslib::navset_tab(
              bslib::nav_panel(
                "Figure 1 | Platform",
                div(
                  class = "home-figure-layout",
                  div(class = "home-figure-frame", img(src = "www/metminer2_figure1.png", class = "home-figure", alt = "MetMiner2 platform overview")),
                  div(
                    class = "home-figure-copy",
                    div(class = "home-figure-kicker", "Development and enhancements"),
                    div(class = "home-figure-title", "From routine processing to annotation-centered evidence integration"),
                    p(class = "home-figure-text", "The first figure is the executive overview of MetMiner2. It separates routine preprocessing improvements from the modules that directly improve metabolite annotation precision."),
                    tags$ul(
                      class = "home-bullet-list",
                      tags$li("Basic upgrades: parameter optimization, blank-informed masking, QC reports, imputation, and normalization."),
                      tags$li("Core modules: Feature Network, AI-assisted annotation review, and species-specific PlantCyc/KEGG database construction."),
                      tags$li("Main outputs: non-redundant annotation tables, audit tables, MS1/MS2 databases, and pathway resources.")
                    ),
                    div(class = "home-output-box",
                        div(class = "home-output-title", "Use this figure for"),
                        "Software overview, method introduction, and explaining how separate modules contribute to more reliable annotation.")
                  )
                )
              ),
              bslib::nav_panel(
                "Figure 2 | Feature Network",
                div(
                  class = "home-figure-layout",
                  div(class = "home-figure-frame", img(src = "www/metminer2_figure2.png", class = "home-figure", alt = "Feature Network workflow")),
                  div(
                    class = "home-figure-copy",
                    div(class = "home-figure-kicker", "Feature-level evidence graph"),
                    div(class = "home-figure-title", "Use co-elution and spectral evidence to decide which feature should carry the annotation"),
                    p(class = "home-figure-text", "Feature Network is designed for the common LC-MS situation where one metabolite produces several related signals. The module keeps these signals visible instead of forcing premature one-feature-one-metabolite assumptions."),
                    tags$ul(
                      class = "home-bullet-list",
                      tags$li("Edges describe isotope, adduct, ISF, recurrent ion, MS/MS similarity, RT, and correlation relationships."),
                      tags$li("Subnetworks help identify core metabolites and explain why nearby features may be redundant or interfering."),
                      tags$li("The resulting roles are carried into annotation filtering and AI review.")
                    ),
                    div(class = "home-output-box",
                        div(class = "home-output-title", "Expected outputs"),
                        "Feature network table, feature role table, non-redundant annotation candidates, and subnetwork-level evidence for manual review.")
                  )
                )
              ),
              bslib::nav_panel(
                "Figure 3 | Databases",
                div(
                  class = "home-figure-layout",
                  div(class = "home-figure-frame", img(src = "www/metminer2_figure3.png", class = "home-figure", alt = "PlantCyc and KEGG database construction")),
                  div(
                    class = "home-figure-copy",
                    div(class = "home-figure-kicker", "Species-specific knowledge base"),
                    div(class = "home-figure-title", "Build plant-focused annotation and enrichment resources instead of relying only on generic databases"),
                    p(class = "home-figure-text", "MetMiner2 can construct PlantCyc databases from licensed local PGDB archives and KEGG databases from organism reaction evidence. The goal is to make annotation and enrichment backgrounds closer to the species under study."),
                    tags$ul(
                      class = "home-bullet-list",
                      tags$li("LC-MS filters remove compounds that are too small, lack formula/mass, or are unsuitable for MS1 annotation."),
                      tags$li("CoA-related compounds are excluded from MS1 but retained for MS2-oriented audit rules."),
                      tags$li("KEGG pathways can be curated by reaction evidence and AI review before enrichment.")
                    ),
                    div(class = "home-output-box",
                        div(class = "home-output-title", "Built-in plant resources"),
                        "Current PlantCyc MS1 libraries include maize, Arabidopsis, wheat, rice, soybean, cotton, tomato, tartary buckwheat, Brassica napus, and PlantCyc reference.")
                  )
                )
              )
            )
          )
        )
      ),
      div(
        class = "home-meta-band",
        div(
          class = "container-fluid px-4 px-xl-5",
          div(
            class = "home-two-col",
            div(
              class = "home-panel",
              h4(class = "fw-bold text-primary", "Recommended workflow"),
              p(class = "home-figure-text",
                "Initialize a project, import raw LC-MS data and sample metadata, run cleaning and normalization, annotate with species-specific PlantCyc resources, build Feature Network evidence, review ambiguous annotations with AI support, and then perform differential analysis and enrichment.")
            ),
            div(
              class = "home-panel",
              h4(class = "fw-bold text-primary", "Citation"),
              div(
                class = "home-cite",
                HTML("Wang, X. et al. <strong>2024</strong>. MetMiner: A user-friendly pipeline for large-scale plant metabolomics data analysis. <em>Journal of Integrative Plant Biology</em>. DOI: <a href='https://doi.org/10.1111/jipb.13774' target='_blank'>10.1111/jipb.13774</a>.")
              ),
              div(
                class = "home-cite",
                HTML("Wang, X. et al. <strong>2026</strong>. TidyMass2: advancing LC-MS untargeted metabolomics through metabolite origin inference and metabolic feature-based functional module analysis. <em>Nature Communications</em> 17, 1755. DOI: <a href='https://doi.org/10.1038/s41467-026-68464-7' target='_blank'>10.1038/s41467-026-68464-7</a>.")
              ),
              div(
                class = "home-cite",
                HTML("Shen, X. et al. <strong>2022</strong>. TidyMass: an object-oriented reproducible analysis framework for LC-MS data. <em>Nature Communications</em>. DOI: <a href='https://doi.org/10.1038/s41467-022-32155-w' target='_blank'>10.1038/s41467-022-32155-w</a>.")
              )
            )
          )
        )
      )
    )
  )
}

#' Homepage Server Module
#'
#' @noRd
mod_homepage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_init, {
      showModal(modalDialog(
        title = "Start a MetMiner2 project",
        "Please open the Project Init tab to set up a workspace and project directories.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
}
