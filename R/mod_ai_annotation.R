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
          selectizeInput(
            ns("model"),
            "Model",
            choices = metminer_ai_provider_model_choices("openai"),
            selected = metminer_ai_provider_defaults("openai")$model,
            options = list(create = TRUE)
          ),
          passwordInput(ns("api_key"), "API Key"),
          textInput(ns("base_url"), "API Endpoint", value = metminer_ai_provider_defaults("openai")$base_url),
          selectInput(
            ns("language"),
            "Chat Language",
            choices = metminer_ai_language_choices(),
            selected = "en"
          ),
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
            div(id = ns("chat_scroll"), class = "ai-chat-scroll", uiOutput(ns("chat_history"))),
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
    ns <- session$ns
    chat <- reactiveVal(list(
      list(role = "system", content = "Ready. Configure an LLM provider, enter a compound name, then click Review.")
    ))
    last_evidence <- reactiveVal(NULL)
    connection_status <- reactiveVal("No API test yet.")
    ai_busy <- reactiveVal(FALSE)
    ai_status <- reactiveVal(NULL)
    api_test_ok <- reactiveVal(FALSE)
    loaded_config_provider <- reactiveVal(NULL)

    start_ai_request <- function(title = "AI Annotation Review", message = "Preparing evidence...", value = 5) {
      ai_busy(TRUE)
      ai_status(list(title = title, message = message, value = value))
    }

    update_ai_progress <- function(value, message = NULL) {
      current_status <- shiny::isolate(ai_status())
      ai_status(list(
        title = current_status$title %||% "AI Annotation Review",
        message = message %||% current_status$message %||% "Working...",
        value = value
      ))
    }

    finish_ai_request <- function(answer = NULL, value = 100, message = "Done.") {
      if (!is.null(answer)) {
        append_chat(chat, "assistant", answer)
      }
      update_ai_progress(value, message)
      ai_status(NULL)
      ai_busy(FALSE)
    }

    scroll_chat_to_bottom <- function() {
      shiny::withReactiveDomain(session, {
        shinyjs::runjs(sprintf(
          "setTimeout(function(){var el=document.getElementById('%s'); if(el){el.scrollTop=el.scrollHeight;}}, 40);",
          ns("chat_scroll")
        ))
      })
    }

    session$onFlushed(function() {
      cfg <- metminer_ai_load_config()
      if (is.null(cfg)) {
        return()
      }
      loaded_config_provider(cfg$provider %||% "openai")
      updateSelectInput(session, "provider", selected = cfg$provider %||% "openai")
      updateSelectizeInput(
        session,
        "model",
        choices = metminer_ai_provider_model_choices(cfg$provider %||% "openai"),
        selected = cfg$model %||% metminer_ai_provider_defaults(cfg$provider)$model,
        server = TRUE
      )
      updateTextInput(session, "api_key", value = cfg$api_key %||% "")
      updateTextInput(session, "base_url", value = cfg$base_url %||% metminer_ai_provider_defaults(cfg$provider)$base_url)
      updateSelectInput(session, "language", selected = cfg$language %||% "en")
      updateSliderInput(session, "temperature", value = cfg$temperature %||% 0.3)
      connection_status(sprintf("Loaded local LLM config: %s / %s", cfg$provider %||% "openai", cfg$model %||% ""))
    }, once = TRUE)

    observeEvent(prj_init$lcms_conditions_text, {
      txt <- trimws(prj_init$lcms_conditions_text %||% "")
      if (!nzchar(txt)) {
        return()
      }
      current <- trimws(input$lc_conditions %||% "")
      default_txt <- trimws("Instrument: \nColumn/stationary phase: \nMobile phase A/B: \nGradient: \nIon source: ESI\nCollision energy: \nNotes:")
      if (!nzchar(current) || identical(current, default_txt)) {
        updateTextAreaInput(session, "lc_conditions", value = txt)
      }
    }, ignoreInit = FALSE)

    observeEvent(input$provider, {
      if (identical(input$provider, loaded_config_provider())) {
        loaded_config_provider(NULL)
        return()
      }
      api_test_ok(FALSE)
      defaults <- metminer_ai_provider_defaults(input$provider)
      updateSelectizeInput(
        session,
        "model",
        choices = metminer_ai_provider_model_choices(input$provider),
        selected = defaults$model,
        server = TRUE
      )
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

    output$chat_history <- renderUI({
      msgs <- chat()
      if (length(msgs) == 0) return(NULL)
      rendered_msgs <- lapply(msgs, function(msg) {
        role <- msg$role %||% "assistant"
        css <- switch(role,
                      user = "ai-message ai-user",
                      system = "ai-message ai-system",
                      "ai-message ai-assistant")
        tags$div(class = css, markdown_to_html(msg$content %||% ""))
      })
      status <- ai_status()
      if (isTRUE(ai_busy()) && !is.null(status)) {
        rendered_msgs[[length(rendered_msgs) + 1L]] <- tags$div(
          class = "ai-message ai-assistant",
          tags$div(class = "fw-semibold", status$title %||% "AI Annotation Review"),
          tags$div(class = "text-muted", status$message %||% "Working..."),
          tags$div(class = "ai-thinking-dots", tags$span(), tags$span(), tags$span()),
          tags$div(class = "ai-progress mt-2")
        )
      }
      tags$div(
        rendered_msgs
      )
    })

    observeEvent(list(chat(), ai_status()), {
      scroll_chat_to_bottom()
    }, ignoreInit = TRUE)

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
          temperature = input$temperature,
          language = input$language
        )
        connection_status(paste("Saved local LLM config:", path))
        shinyalert::shinyalert("Config Saved", paste("Local configuration saved to:", path), type = "success")
      }, error = function(e) {
        shinyalert::shinyalert("Save Failed", e$message, type = "error")
      })
    })

    observeEvent(input$review_compound, {
      if (isTRUE(ai_busy())) {
        return()
      }

      # Capture inputs for the async future (Shiny inputs are not safe inside futures)
      provider   <- input$provider
      model      <- input$model
      api_key    <- input$api_key
      base_url   <- input$base_url
      temperature <- input$temperature
      lc_cond    <- input$lc_conditions
      language   <- input$language
      user_msg   <- input$user_message
      compound_q <- input$compound_query
      max_feat   <- input$max_features
      ms2_n      <- input$ms2_top_n
      positive_object <- global_data$object_pos_annotated %||% global_data$object_pos_network
      negative_object <- global_data$object_neg_annotated %||% global_data$object_neg_network
      filter_result <- global_data$annotation_filter_result
      if (!nzchar(trimws(api_key %||% ""))) {
        append_chat(chat, "system", "Please enter an API key before sending the request.")
        return()
      }
      if (!nzchar(trimws(compound_q %||% ""))) {
        append_chat(chat, "system", "Please enter a compound or feature query before clicking Review.")
        return()
      }
      start_ai_request("AI Annotation Review", "Preparing evidence bundle...", 8)

      append_chat(chat, "user", paste("Review annotation:", compound_q, "\n", user_msg %||% ""))
      append_chat(chat, "system", "Preparing evidence bundle...")
      later::later(function() {

      # Gather evidence synchronously (needs Shiny reactives)
      paper_search <- tryCatch(
        run_paper_search_for_request(input = input, chat = chat, query = compound_q, trigger_text = user_msg),
        error = function(e) list(status = "error", message = e$message)
      )
      update_ai_progress(25, "Collecting annotation, MS1/MS2 and feature-network evidence...")
      if (metminer_ai_should_use_paper_search(input, user_msg)) {
        updateCheckboxInput(session, "include_paper_search", value = FALSE)
      }

      evidence <- tryCatch(
        metminer_ai_collect_evidence(
          compound_name = compound_q,
          positive_object = positive_object,
          negative_object = negative_object,
          filter_result = filter_result,
          max_features = max_feat,
          ms2_top_n = ms2_n,
          paper_search = paper_search
        ),
        error = function(e) list(feature_count = 0, nonredundant_rows = data.frame(), redundancy_audit_rows = data.frame(), error = e$message)
      )
      last_evidence(evidence)

      append_chat(chat, "system", sprintf("Evidence bundle prepared: %d feature(s), %d final row(s), %d audit row(s).",
                                          evidence$feature_count,
                                          nrow(evidence$nonredundant_rows %||% data.frame()),
                                          nrow(evidence$redundancy_audit_rows %||% data.frame())))
      update_ai_progress(45, "Building cacheable LLM prompt...")

      # Build cache-optimised messages: evidence as static prefix, question as variable suffix
      evidence_json <- metminer_ai_safe_json(evidence)
      messages <- metminer_ai_build_cacheable_messages(
        system_prompt = metminer_ai_system_prompt(language, mode = "review"),
        evidence_json = evidence_json,
        compound_name = compound_q,
        lc_conditions = lc_cond,
        user_question = user_msg,
        language = language,
        mode = "review"
      )
      llm_caller <- metminer_ai_llm_caller()

      # Async LLM call
      fut <- tryCatch(future::future(
        llm_caller(provider, model, api_key, base_url, temperature, messages),
        packages = c("httr2", "jsonlite"),
        globals = list(
          llm_caller = llm_caller,
          provider = provider,
          model = model,
          api_key = api_key,
          base_url = base_url,
          temperature = temperature,
          language = language,
          messages = messages
        ),
        seed = TRUE
      ), error = function(e) e)

      if (inherits(fut, "error")) {
        finish_ai_request(paste("Review failed:", fut$message), value = 100, message = "Failed.")
        shinyalert::shinyalert("AI Review Failed", fut$message, type = "error")
        return()
      }
      update_ai_progress(65, "Waiting for LLM response...")

      request_started_at <- Sys.time()
      max_wait_sec <- 360
      poll_result <- function() {
        if (future::resolved(fut)) {
          answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
          finish_ai_request(answer, value = 100, message = "AI response received.")
        } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > max_wait_sec) {
          finish_ai_request(
            sprintf("LLM call timed out after %d seconds. Please try a smaller evidence scope, a faster model, or a higher provider timeout.", max_wait_sec),
            value = 100,
            message = "Timed out."
          )
        } else {
          update_ai_progress(80, "LLM is reasoning. This may take a while for thinking models...")
          later::later(poll_result, 0.5)
        }
      }
      later::later(poll_result, 0.2)
      }, 0.05)
    })

    observeEvent(input$send_message, {
      if (isTRUE(ai_busy())) {
        return()
      }

      # Capture all inputs for the async future
      provider   <- input$provider
      model      <- input$model
      api_key    <- input$api_key
      base_url   <- input$base_url
      temperature <- input$temperature
      lc_cond    <- input$lc_conditions
      language   <- input$language
      user_msg   <- input$user_message
      compound_q <- input$compound_query
      if (!nzchar(trimws(api_key %||% ""))) {
        append_chat(chat, "system", "Please enter an API key before sending the request.")
        return()
      }
      if (!nzchar(trimws(user_msg %||% ""))) {
        append_chat(chat, "system", "Please enter a message before clicking Send.")
        return()
      }
      start_ai_request("AI Chat", "Preparing follow-up question...", 10)

      append_chat(chat, "user", user_msg)
      append_chat(chat, "system", "Reasoning...")
      later::later(function() {

      # Gather evidence + optional paper search synchronously
      evidence <- shiny::isolate(last_evidence())
      paper_requested <- metminer_ai_should_use_paper_search(input, user_msg)
      if (isTRUE(paper_requested)) {
        if (is.null(evidence)) {
          evidence <- list(
            query = metminer_ai_clean_paper_query(compound_q %||% user_msg),
            literature_evidence = list(status = "not_requested"),
            feature_count = 0,
            nonredundant_rows = data.frame(),
            redundancy_audit_rows = data.frame(),
            raw_annotation_candidates = data.frame(),
            feature_evidence = data.frame()
          )
        }
        prev_lit <- evidence$literature_evidence
        if (is.null(prev_lit) || !isTRUE(prev_lit$status == "ok") || length(prev_lit$papers %||% list()) == 0) {
          evidence$literature_evidence <- tryCatch(
            run_paper_search_for_request(input = input, chat = chat,
                                         query = metminer_ai_clean_paper_query(evidence$query %||% compound_q %||% user_msg),
                                         trigger_text = user_msg),
            error = function(e) list(status = "error", message = e$message)
          )
          last_evidence(evidence)
        }
        updateCheckboxInput(session, "include_paper_search", value = FALSE)
      }
      update_ai_progress(40, "Building chat context and evidence prefix...")

      # Build cache-optimised messages: identical evidence prefix → DeepSeek cache hit
      # Chat context is summarised to a short snippet so the variable suffix stays small
      ev <- shiny::isolate(last_evidence())
      evidence_json <- if (!is.null(ev)) {
        metminer_ai_safe_json(ev)
      } else {
        "{}"
      }
      current_chat <- shiny::isolate(chat())
      chat_context <- if (length(current_chat) > 0) {
        metminer_ai_summarize_chat_context(current_chat)
      } else {
        NULL
      }

      messages <- metminer_ai_build_cacheable_messages(
        system_prompt = metminer_ai_system_prompt(language, mode = "chat"),
        evidence_json = evidence_json,
        compound_name = ev$query %||% compound_q,
        lc_conditions = lc_cond,
        user_question = user_msg,
        chat_context = chat_context,
        language = language,
        mode = "chat"
      )
      llm_caller <- metminer_ai_llm_caller()

      # Async LLM call
      fut <- tryCatch(future::future(
        llm_caller(provider, model, api_key, base_url, temperature, messages),
        packages = c("httr2", "jsonlite"),
        globals = list(
          llm_caller = llm_caller,
          provider = provider,
          model = model,
          api_key = api_key,
          base_url = base_url,
          temperature = temperature,
          language = language,
          messages = messages
        ),
        seed = TRUE
      ), error = function(e) e)

      if (inherits(fut, "error")) {
        finish_ai_request(paste("Message failed:", fut$message), value = 100, message = "Failed.")
        return()
      }
      update_ai_progress(70, "Waiting for LLM response...")

      request_started_at <- Sys.time()
      max_wait_sec <- 360
      poll_result <- function() {
        if (future::resolved(fut)) {
          answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
          finish_ai_request(answer, value = 100, message = "AI response received.")
        } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > max_wait_sec) {
          finish_ai_request(
            sprintf("LLM call timed out after %d seconds. Please try a shorter question, a faster model, or a higher provider timeout.", max_wait_sec),
            value = 100,
            message = "Timed out."
          )
        } else {
          update_ai_progress(85, "LLM is reasoning. This may take a while for thinking models...")
          later::later(poll_result, 0.5)
        }
      }
      later::later(poll_result, 0.2)
      }, 0.05)
    })
  })
}

metminer_ai_should_use_paper_search <- function(input, trigger_text = "") {
  trigger_text <- tolower(trigger_text %||% "")
  isTRUE(shiny::isolate(input$include_paper_search)) ||
    grepl("@(agent|paper|mcp|literature)", trigger_text, perl = TRUE)
}

metminer_ai_clean_paper_query <- function(x) {
  x <- trimws(x %||% "")
  x <- gsub("@(agent|paper|mcp|literature)", "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

run_paper_search_for_request <- function(input, chat, query, trigger_text = "") {
  if (!metminer_ai_should_use_paper_search(input, trigger_text)) {
    return(list(status = "not_requested", trigger = "not_requested"))
  }
  append_chat(chat, "system", sprintf(
    "Searching literature sources: %s...",
    paste(shiny::isolate(input$paper_sources) %||% character(), collapse = ", ")
  ))
  paper_search <- tryCatch(
    metminer_ai_search_papers(
      query = query,
      sources = shiny::isolate(input$paper_sources),
      max_results_per_source = shiny::isolate(input$paper_max_results),
      cli = shiny::isolate(input$paper_search_cli)
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

metminer_ai_llm_caller <- function() {
  envs <- list(
    parent.frame(),
    environment(),
    globalenv()
  )
  for (env in envs) {
    fun <- tryCatch(get("call_llm_async", envir = env, mode = "function", inherits = TRUE), error = function(e) NULL)
    if (is.function(fun)) {
      return(fun)
    }
  }
  stop("Internal LLM caller is not available. Please restart the MetMiner app after updating the code.", call. = FALSE)
}

call_llm_async <- function(provider, model, api_key, base_url, temperature, messages) {
  first_non_empty <- function(x, y) {
    if (is.null(x) || length(x) == 0 || !nzchar(trimws(as.character(x)[1]))) y else as.character(x)[1]
  }
  msg_field <- function(x, name) {
    value <- x[[name]]
    if (is.null(value) || length(value) == 0) "" else as.character(value)[1]
  }
  provider <- first_non_empty(provider, "openai")
  defaults <- list(
    openai = list(model = "gpt-4o-mini", style = "openai"),
    deepseek = list(model = "deepseek-v4-flash", style = "openai"),
    qwen = list(model = "qwen-plus", style = "openai"),
    kimi = list(model = "moonshot-v1-8k", style = "openai"),
    grok = list(model = "grok-2-latest", style = "openai"),
    gemini = list(model = "gemini-1.5-flash", style = "gemini")
  )
  default <- defaults[[provider]]
  if (is.null(default)) default <- defaults$openai
  model <- first_non_empty(model, default$model)
  base_url <- first_non_empty(base_url, "")
  api_key <- first_non_empty(api_key, "")
  if (!nzchar(api_key)) stop("API key is required.", call. = FALSE)

  if (identical(default$style, "gemini")) {
    endpoint <- paste0(sub("/+$", "", base_url), "/models/", model, ":generateContent")
    roles <- vapply(messages, msg_field, character(1), name = "role")
    system_text <- paste(vapply(messages[roles == "system"], msg_field, character(1), name = "content"), collapse = "\n")
    user_text <- paste(vapply(messages[roles != "system"], msg_field, character(1), name = "content"), collapse = "\n\n")
    body <- list(
      systemInstruction = list(parts = list(list(text = system_text))),
      contents = list(list(role = "user", parts = list(list(text = user_text)))),
      generationConfig = list(temperature = as.numeric(temperature))
    )
    resp <- tryCatch(
      httr2::req_perform(
        httr2::req_timeout(
          httr2::req_body_json(
            httr2::req_method(
              httr2::req_url_query(httr2::request(endpoint), key = api_key),
              "POST"
            ),
            body,
            auto_unbox = TRUE
          ),
          300
        )
      ),
      error = function(e) stop("HTTP error: ", e$message, call. = FALSE)
    )
    parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    content <- parsed$candidates[[1]]$content$parts[[1]]$text
    if (is.null(content)) content <- ""
    if (!nzchar(trimws(content))) stop("LLM returned empty response.", call. = FALSE)
    return(content)
  }

  body_json <- jsonlite::toJSON(
    list(model = model, messages = messages, temperature = as.numeric(temperature)),
    auto_unbox = TRUE,
    na = "null"
  )
  resp <- tryCatch(
    httr2::req_perform(
      httr2::req_retry(
        httr2::req_timeout(
          httr2::req_body_raw(
            httr2::req_headers(
              httr2::req_method(httr2::request(base_url), "POST"),
              Authorization = paste("Bearer", api_key),
              `Content-Type` = "application/json"
            ),
            body_json,
            "application/json"
          ),
          300
        ),
        max_tries = 2,
        max_seconds = 300
      )
    ),
    error = function(e) stop("HTTP error: ", e$message, call. = FALSE)
  )
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- parsed$choices[[1]]$message$content
  if (is.null(content)) content <- ""
  if (!nzchar(trimws(content))) stop("LLM returned empty response.", call. = FALSE)
  content
}

append_chat <- function(chat, role, content) {
  msgs <- shiny::isolate(chat())
  msgs[[length(msgs) + 1L]] <- list(role = role, content = content)
  chat(msgs)
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
