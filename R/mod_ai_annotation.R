#' AI-assisted metabolite annotation reviewer UI
#'
#' @noRd
mod_ai_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML(sprintf("
      #%s .ai-chat-page {
        min-height: clamp(520px, 72vh, 820px);
        display: flex;
        flex-direction: column;
        background: #ffffff;
      }
      #%s .ai-chat-header {
        padding: 0.65rem 0 0.85rem;
        border-bottom: 1px solid #e2e8f0;
      }
      #%s .ai-chat-scroll {
        flex: 1 1 auto;
        min-height: 320px;
        max-height: none;
        overflow-y: auto;
        padding: 1.25rem 0;
        background: #ffffff;
      }
      #%s .ai-message {
        max-width: 920px;
        margin-bottom: 1rem;
        padding: 0.9rem 1rem;
        border-radius: 8px;
        border: 1px solid #e5e7eb;
        line-height: 1.5;
      }
      #%s .ai-message h1,
      #%s .ai-message h2,
      #%s .ai-message h3 {
        font-size: 1.05rem;
        margin: 0.65rem 0 0.35rem;
        font-weight: 700;
      }
      #%s .ai-message p {
        margin-bottom: 0.55rem;
      }
      #%s .ai-message ul,
      #%s .ai-message ol {
        padding-left: 1.3rem;
        margin-bottom: 0.55rem;
      }
      #%s .ai-message table {
        width: 100%%;
        border-collapse: collapse;
        margin: 0.75rem 0;
        font-size: 0.92rem;
      }
      #%s .ai-message th,
      #%s .ai-message td {
        border: 1px solid #dbe3ea;
        padding: 0.35rem 0.45rem;
        vertical-align: top;
      }
      #%s .ai-message th {
        background: #f1f5f9;
      }
      #%s .ai-message pre {
        background: #0f172a;
        color: #e2e8f0;
        padding: 0.75rem;
        border-radius: 6px;
        overflow-x: auto;
      }
      #%s .ai-user {
        margin-left: auto;
        background: #eef6f6;
        border-color: #cce3e3;
      }
      #%s .ai-assistant {
        margin-right: auto;
        background: #ffffff;
      }
      #%s .ai-system {
        margin-left: auto;
        margin-right: auto;
        background: #f8fafc;
        color: #64748b;
        font-size: 0.9rem;
      }
      #%s .ai-composer {
        border-top: 1px solid #e2e8f0;
        padding: 1rem;
        background: #f8fafc;
      }
      #%s .ai-busy {
        border-top: 1px solid #dbeafe;
        background: #eff6ff;
        color: #1d4ed8;
        padding: 0.55rem 1rem;
        font-size: 0.92rem;
      }
      #%s .ai-thinking-dots {
        display: inline-flex;
        gap: 0.28rem;
        margin-left: 0.25rem;
      }
      #%s .ai-thinking-dots span {
        width: 0.45rem;
        height: 0.45rem;
        border-radius: 999px;
        background: #2563eb;
        opacity: 0.3;
        animation: aiBlink 1.2s infinite ease-in-out;
      }
      #%s .ai-thinking-dots span:nth-child(2) {
        animation-delay: 0.18s;
      }
      #%s .ai-thinking-dots span:nth-child(3) {
        animation-delay: 0.36s;
      }
      #%s .ai-progress {
        position: relative;
        height: 3px;
        margin-top: 0.45rem;
        overflow: hidden;
        border-radius: 999px;
        background: #bfdbfe;
      }
      #%s .ai-progress::after {
        content: '';
        position: absolute;
        top: 0;
        left: -35%%;
        width: 35%%;
        height: 100%%;
        background: linear-gradient(90deg, transparent, #2563eb, transparent);
        animation: aiProgress 1.4s infinite linear;
      }
      @keyframes aiBlink {
        0%%, 80%%, 100%% { opacity: 0.25; transform: translateY(0); }
        40%% { opacity: 1; transform: translateY(-2px); }
      }
      @keyframes aiProgress {
        from { left: -35%%; }
        to { left: 100%%; }
      }
    ", ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"),
       ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"),
       ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"),
       ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"),
       ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"), ns("ai_root"),
       ns("ai_root")))),
    bslib::page_fluid(
      id = ns("ai_root"),
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,
        sidebar = bslib::sidebar(
          title = tagList(bsicons::bs_icon("robot"), " AI Reviewer"),
          width = 360,
          bg = "#f8f9fa",
          open = TRUE,
          tags$h6(class = "fw-bold text-primary", "LLM Settings"),
          selectInput(
            ns("provider"),
            "LLM Provider",
            choices = c(
              "OpenAI" = "openai",
              "Gemini" = "gemini",
              "DeepSeek" = "deepseek",
              "Qwen" = "qwen",
              "Kimi" = "kimi",
              "Grok" = "grok"
            ),
            selected = "openai"
          ),
          textInput(ns("model"), "Model", value = metminer_ai_provider_defaults("openai")$model),
          passwordInput(ns("api_key"), "API Key"),
          textInput(ns("base_url"), "API Endpoint", value = metminer_ai_provider_defaults("openai")$base_url),
          sliderInput(ns("temperature"), "Temperature", min = 0, max = 1.5, value = 0.3, step = 0.05),
          div(
            class = "d-grid gap-2 mb-3",
            actionButton(ns("test_api"), "Test API", icon = icon("plug"), class = "btn-outline-primary"),
            uiOutput(ns("save_config_ui")),
            actionButton(ns("clear_chat"), "Clear Chat", icon = icon("trash"), class = "btn-outline-secondary")
          ),
          tags$hr(),
          tags$h6(class = "fw-bold text-primary", "Evidence Scope"),
          textInput(ns("compound_query"), "Compound / Feature Query", placeholder = "e.g. (-)-jasmonoyl-L-isoleucine"),
          numericInput(ns("max_features"), "Max Features in Evidence", value = 12, min = 1, step = 1),
          numericInput(ns("ms2_top_n"), "MS2 Top Peaks", value = 12, min = 3, step = 1),
          checkboxInput(ns("include_paper_search"), "Use paper-search for this request", value = FALSE),
          tags$small(class = "text-muted d-block mb-2", "Also triggered by @agent, @paper, or @mcp in the message."),
          shinyWidgets::pickerInput(
            ns("paper_sources"),
            "Paper Sources",
            choices = metminer_ai_paper_sources(),
            selected = metminer_ai_default_paper_sources(),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              selectedTextFormat = "count > 3"
            )
          ),
          numericInput(ns("paper_max_results"), "Papers per Source", value = 3, min = 1, max = 10, step = 1),
          textInput(ns("paper_search_cli"), "paper-search CLI", value = Sys.getenv("PAPER_SEARCH_MCP_CLI", "paper-search")),
          actionButton(ns("test_paper_search"), "Test Paper Search", icon = icon("book-open"), class = "btn-outline-primary w-100"),
          tags$hr(),
          textAreaInput(
            ns("lc_conditions"),
            "LC-MS Conditions",
            value = "Instrument: \nColumn/stationary phase: \nMobile phase A/B: \nGradient: \nIon source: ESI\nCollision energy: \nNotes: ",
            rows = 8
          ),
          tags$hr(),
          verbatimTextOutput(ns("connection_status"), placeholder = TRUE)
        ),
        div(
          class = "p-3 ai-chat-page",
          div(class = "ai-chat-header", tags$strong("Annotation Review Chat")),
            div(class = "ai-chat-scroll", uiOutput(ns("chat_history"))),
            uiOutput(ns("busy_indicator")),
            div(
              class = "ai-composer",
              div(
                class = "d-flex gap-2 align-items-end",
                div(
                  class = "flex-grow-1",
                  textAreaInput(
                    ns("user_message"),
                    NULL,
                    placeholder = "Ask the AI to review this annotation, compare candidate features, or explain conflicts...",
                    rows = 3,
                    width = "100%"
                  )
                ),
                div(
                  class = "d-grid gap-2",
                  actionButton(ns("review_compound"), "Review", icon = icon("wand-magic-sparkles"), class = "btn-primary"),
                  actionButton(ns("send_message"), "Send", icon = icon("paper-plane"), class = "btn-outline-primary")
                )
              )
            )
        )
      )
    )
  )
}

#' AI-assisted metabolite annotation reviewer server
#'
#' @noRd
mod_ai_annotation_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    chat <- reactiveVal(list(
      list(role = "system", content = "Ready. Configure an LLM provider, enter a compound name, then click Review.")
    ))
    last_evidence <- reactiveVal(NULL)
    connection_status <- reactiveVal("No API test yet.")
    ai_busy <- reactiveVal(FALSE)
    api_test_ok <- reactiveVal(FALSE)
    loaded_config_provider <- reactiveVal(NULL)

    session$onFlushed(function() {
      cfg <- metminer_ai_load_config()
      if (is.null(cfg)) {
        return()
      }
      loaded_config_provider(cfg$provider %||% "openai")
      updateSelectInput(session, "provider", selected = cfg$provider %||% "openai")
      updateTextInput(session, "model", value = cfg$model %||% metminer_ai_provider_defaults(cfg$provider)$model)
      updateTextInput(session, "api_key", value = cfg$api_key %||% "")
      updateTextInput(session, "base_url", value = cfg$base_url %||% metminer_ai_provider_defaults(cfg$provider)$base_url)
      updateSliderInput(session, "temperature", value = cfg$temperature %||% 0.3)
      connection_status(sprintf("Loaded local LLM config: %s / %s", cfg$provider %||% "openai", cfg$model %||% ""))
    }, once = TRUE)

    observeEvent(input$provider, {
      if (identical(input$provider, loaded_config_provider())) {
        loaded_config_provider(NULL)
        return()
      }
      api_test_ok(FALSE)
      defaults <- metminer_ai_provider_defaults(input$provider)
      updateTextInput(session, "model", value = defaults$model)
      updateTextInput(session, "base_url", value = defaults$base_url)
    }, ignoreInit = TRUE)

    observeEvent(list(input$model, input$base_url, input$api_key), {
      api_test_ok(FALSE)
    }, ignoreInit = TRUE)

    output$save_config_ui <- renderUI({
      if (!isTRUE(metminer_ai_config_persistence_available())) {
        return(tags$small(class = "text-muted", "Local config persistence is hidden on server deployments."))
      }
      actionButton(
        session$ns("save_config"),
        "Save Local Config",
        icon = icon("save"),
        class = "btn-outline-success"
      )
    })

    output$connection_status <- renderText(connection_status())

    output$busy_indicator <- renderUI({
      if (!isTRUE(ai_busy())) return(NULL)
      div(
        class = "ai-busy",
        div(
          class = "d-flex align-items-center gap-2",
          tags$div(class = "spinner-border spinner-border-sm", role = "status"),
          tags$span("AI is reasoning over MS1/MS2, feature-network, recurrent-ion and annotation evidence"),
          tags$span(class = "ai-thinking-dots", tags$span(), tags$span(), tags$span())
        ),
        div(class = "ai-progress")
      )
    })

    output$chat_history <- renderUI({
      msgs <- chat()
      if (length(msgs) == 0) return(NULL)
      tags$div(
        lapply(msgs, function(msg) {
          role <- msg$role %||% "assistant"
          css <- switch(role,
                        user = "ai-message ai-user",
                        system = "ai-message ai-system",
                        "ai-message ai-assistant")
          tags$div(class = css, markdown_to_html(msg$content %||% ""))
        })
      )
    })

    observeEvent(input$clear_chat, {
      chat(list(list(role = "system", content = "Chat cleared.")))
      last_evidence(NULL)
    })

    observeEvent(input$test_api, {
      req(input$api_key)
      connection_status("Testing API connection...")
      tryCatch({
        ans <- metminer_ai_test_connection(
          provider = input$provider,
          model = input$model,
          api_key = input$api_key,
          base_url = input$base_url
        )
        connection_status(paste("Connection OK:", trimws(ans)))
        api_test_ok(TRUE)
        shinyalert::shinyalert("API Test", "Connection succeeded.", type = "success")
      }, error = function(e) {
        api_test_ok(FALSE)
        connection_status(paste("Connection failed:", e$message))
        shinyalert::shinyalert("API Test Failed", e$message, type = "error")
      })
    })

    observeEvent(input$test_paper_search, {
      req(input$compound_query)
      connection_status("Testing paper-search MCP CLI...")
      tryCatch({
        papers <- metminer_ai_search_papers(
          query = input$compound_query,
          sources = input$paper_sources,
          max_results_per_source = 1,
          cli = input$paper_search_cli
        )
        connection_status(sprintf(
          "Paper search OK: %d paper(s), sources: %s",
          papers$total %||% length(papers$papers %||% list()),
          paste(papers$sources_used %||% character(), collapse = ",")
        ))
        shinyalert::shinyalert("Paper Search", "paper-search MCP CLI is available.", type = "success")
      }, error = function(e) {
        connection_status(paste("Paper search failed:", e$message))
        shinyalert::shinyalert("Paper Search Failed", e$message, type = "error")
      })
    })

    observeEvent(input$save_config, {
      if (!isTRUE(metminer_ai_config_persistence_available())) {
        return()
      }
      req(input$provider, input$model, input$api_key, input$base_url)
      if (!isTRUE(api_test_ok())) {
        shinyalert::shinyalert(
          "Test API First",
          "Please run Test API successfully before saving this local LLM configuration.",
          type = "warning"
        )
        return()
      }
      tryCatch({
        path <- metminer_ai_save_config(
          provider = input$provider,
          model = input$model,
          api_key = input$api_key,
          base_url = input$base_url,
          temperature = input$temperature
        )
        connection_status(paste("Saved local LLM config:", path))
        shinyalert::shinyalert("Config Saved", paste("Local configuration saved to:", path), type = "success")
      }, error = function(e) {
        shinyalert::shinyalert("Save Failed", e$message, type = "error")
      })
    })

    observeEvent(input$review_compound, {
      req(input$compound_query, input$api_key)
      if (isTRUE(ai_busy())) {
        return()
      }
      ai_busy(TRUE)
      shinyjs::disable("review_compound")
      shinyjs::disable("send_message")
      shinyjs::disable("test_api")
      on.exit({
        ai_busy(FALSE)
        shinyjs::enable("review_compound")
        shinyjs::enable("send_message")
        shinyjs::enable("test_api")
      }, add = TRUE)
      append_chat(chat, "user", paste("Review annotation:", input$compound_query, "\n", input$user_message %||% ""))
      append_chat(chat, "system", "Preparing evidence bundle and waiting for the LLM response...")
      tryCatch({
        paper_search <- run_paper_search_for_request(
          input = input,
          chat = chat,
          query = input$compound_query,
          trigger_text = input$user_message
        )
        updateCheckboxInput(session, "include_paper_search", value = FALSE)
        evidence <- metminer_ai_collect_evidence(
          compound_name = input$compound_query,
          positive_object = global_data$object_pos_annotated %||% global_data$object_pos_network,
          negative_object = global_data$object_neg_annotated %||% global_data$object_neg_network,
          filter_result = global_data$annotation_filter_result,
          max_features = input$max_features,
          ms2_top_n = input$ms2_top_n,
          paper_search = paper_search
        )
        last_evidence(evidence)
        prompt <- metminer_ai_build_review_user_prompt(
          compound_name = input$compound_query,
          evidence_bundle = evidence,
          lc_conditions = input$lc_conditions,
          user_question = input$user_message
        )
        append_chat(chat, "system", sprintf("Evidence bundle prepared: %d feature(s), %d final row(s), %d audit row(s).",
                                            evidence$feature_count,
                                            nrow(evidence$nonredundant_rows),
                                            nrow(evidence$redundancy_audit_rows)))
        answer <- call_ai_reviewer(input, prompt)
        append_chat(chat, "assistant", answer)
      }, error = function(e) {
        append_chat(chat, "assistant", paste("Review failed:", e$message))
        shinyalert::shinyalert("AI Review Failed", e$message, type = "error")
      })
    })

    observeEvent(input$send_message, {
      req(input$user_message, input$api_key)
      if (isTRUE(ai_busy())) {
        return()
      }
      ai_busy(TRUE)
      shinyjs::disable("review_compound")
      shinyjs::disable("send_message")
      shinyjs::disable("test_api")
      on.exit({
        ai_busy(FALSE)
        shinyjs::enable("review_compound")
        shinyjs::enable("send_message")
        shinyjs::enable("test_api")
      }, add = TRUE)
      append_chat(chat, "user", input$user_message)
      append_chat(chat, "system", "Waiting for the LLM response...")
      tryCatch({
        evidence <- last_evidence()
        if (!is.null(evidence) && metminer_ai_should_use_paper_search(input, input$user_message)) {
          evidence$literature_evidence <- run_paper_search_for_request(
            input = input,
            chat = chat,
            query = evidence$query %||% input$compound_query %||% input$user_message,
            trigger_text = input$user_message
          )
          updateCheckboxInput(session, "include_paper_search", value = FALSE)
          last_evidence(evidence)
        }
        prompt <- if (!is.null(evidence)) {
          metminer_ai_build_review_user_prompt(
            compound_name = evidence$query,
            evidence_bundle = evidence,
            lc_conditions = input$lc_conditions,
            user_question = input$user_message
          )
        } else {
          input$user_message
        }
        answer <- call_ai_reviewer(input, prompt)
        append_chat(chat, "assistant", answer)
      }, error = function(e) {
        append_chat(chat, "assistant", paste("Message failed:", e$message))
      })
    })
  })
}

metminer_ai_should_use_paper_search <- function(input, trigger_text = "") {
  trigger_text <- tolower(trigger_text %||% "")
  isTRUE(input$include_paper_search) ||
    grepl("(^|\\s)@(agent|paper|mcp|literature)\\b", trigger_text, perl = TRUE)
}

run_paper_search_for_request <- function(input, chat, query, trigger_text = "") {
  if (!metminer_ai_should_use_paper_search(input, trigger_text)) {
    return(list(status = "not_requested", trigger = "not_requested"))
  }
  append_chat(chat, "system", sprintf(
    "Searching literature sources: %s...",
    paste(input$paper_sources %||% character(), collapse = ", ")
  ))
  paper_search <- tryCatch(
    metminer_ai_search_papers(
      query = query,
      sources = input$paper_sources,
      max_results_per_source = input$paper_max_results,
      cli = input$paper_search_cli
    ),
    error = function(e) list(status = "error", message = e$message)
  )
  if (identical(paper_search$status %||% "", "error")) {
    append_chat(chat, "system", paste("Paper search failed:", paper_search$message))
  } else {
    append_chat(chat, "system", sprintf(
      "Paper search prepared: %d deduplicated paper(s) from %s.",
      paper_search$total %||% length(paper_search$papers %||% list()),
      paste(paper_search$sources_used %||% character(), collapse = ",")
    ))
  }
  paper_search
}

append_chat <- function(chat, role, content) {
  msgs <- chat()
  msgs[[length(msgs) + 1L]] <- list(role = role, content = content)
  chat(msgs)
}

call_ai_reviewer <- function(input, user_prompt) {
  messages <- list(
    list(role = "system", content = metminer_ai_system_prompt()),
    list(role = "user", content = user_prompt)
  )
  metminer_ai_chat(
    provider = input$provider,
    model = input$model,
    api_key = input$api_key,
    messages = messages,
    temperature = input$temperature,
    base_url = input$base_url
  )
}

markdown_to_html <- function(x) {
  x <- x %||% ""
  if (requireNamespace("markdown", quietly = TRUE)) {
    html <- markdown::markdownToHTML(
      text = x,
      fragment.only = TRUE,
      options = c("use_xhtml", "smartypants", "tables", "fenced_code")
    )
    return(HTML(html))
  }
  x <- htmltools::htmlEscape(x)
  x <- gsub("\\n", "<br>", x, fixed = FALSE)
  HTML(x)
}
