#' Global expandable AI assistant UI
#'
#' @noRd
mod_global_ai_bot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML(sprintf("
      #%s { position: fixed; right: 1.35rem; bottom: 1.35rem; z-index: 1080; font-family: inherit; }
      #%s .global-ai-launcher {
        width: 3.4rem; height: 3.4rem; border-radius: 999px; border: 1px solid rgba(0,128,128,.35);
        background: #008080; color: #fff; box-shadow: 0 16px 36px rgba(15,23,42,.24);
        display: inline-flex; align-items: center; justify-content: center; cursor: pointer;
      }
      #%s .global-ai-panel {
        width: min(440px, calc(100vw - 2rem)); height: min(680px, calc(100vh - 2rem));
        min-height: 420px; max-height: calc(100vh - 2rem);
        background: #fff; border: 1px solid #d7e5e5; border-radius: 8px;
        box-shadow: 0 24px 60px rgba(15,23,42,.26); display: none; overflow: hidden;
      }
      #%s .global-ai-panel.is-open { display: flex; flex-direction: column; }
      #%s .global-ai-panel > * { flex-shrink: 0; }
      #%s .global-ai-resize-handle {
        height: 12px;
        cursor: ns-resize;
        flex: 0 0 12px;
        background: linear-gradient(180deg, #f8fafc, #eef7f7);
        border-bottom: 1px solid #d7e5e5;
        position: relative;
      }
      #%s .global-ai-resize-handle::after {
        content: '';
        position: absolute;
        left: 50%%;
        top: 4px;
        width: 42px;
        height: 3px;
        transform: translateX(-50%%);
        border-radius: 999px;
        background: #9fcaca;
      }
      #%s .global-ai-header {
        display: flex; align-items: center; justify-content: space-between; padding: .85rem 1rem;
        background: #f4fbfb; border-bottom: 1px solid #d7e5e5;
      }
      #%s .global-ai-title { display: flex; align-items: center; gap: .5rem; font-weight: 700; color: #006b6b; }
      #%s .global-ai-body { flex: 1 1 auto; min-height: 0; overflow-y: auto; padding: 1rem; background: #fff; }
      #%s .global-ai-message {
        border: 1px solid #e2e8f0; border-radius: 8px; padding: .75rem .85rem;
        margin-bottom: .75rem; line-height: 1.45; font-size: .94rem;
      }
      #%s .global-ai-user { margin-left: 2.4rem; background: #eef7f7; border-color: #cfe7e7; }
      #%s .global-ai-assistant { margin-right: 2.4rem; background: #fff; }
      #%s .global-ai-system { background: #f8fafc; color: #64748b; font-size: .88rem; }
      #%s .global-ai-settings { border-bottom: 1px solid #e2e8f0; background: #f8fafc; padding: .75rem 1rem; }
      #%s .global-ai-settings summary { cursor: pointer; font-weight: 600; color: #006b6b; }
      #%s .global-ai-typing-bar {
        border-top: 1px solid #e2e8f0;
        background: #f4fbfb;
        padding: .55rem .85rem;
        color: #006b6b;
        font-size: .88rem;
      }
      #%s .global-ai-typing-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: .75rem;
      }
      #%s .global-ai-composer { border-top: 1px solid #e2e8f0; background: #f8fafc; padding: .85rem; }
      #%s .global-ai-thinking-dots {
        display: inline-flex;
        gap: 0.28rem;
        margin-top: 0.35rem;
      }
      #%s .global-ai-thinking-dots span {
        width: 0.45rem;
        height: 0.45rem;
        border-radius: 999px;
        background: #008080;
        opacity: 0.3;
        animation: globalAiBlink 1.2s infinite ease-in-out;
      }
      #%s .global-ai-thinking-dots span:nth-child(2) {
        animation-delay: 0.18s;
      }
      #%s .global-ai-thinking-dots span:nth-child(3) {
        animation-delay: 0.36s;
      }
      #%s .global-ai-progress {
        position: relative;
        height: 3px;
        margin-top: 0.45rem;
        overflow: hidden;
        border-radius: 999px;
        background: #cfe7e7;
      }
      #%s .global-ai-progress::after {
        content: '';
        position: absolute;
        top: 0;
        left: -35%%;
        width: 35%%;
        height: 100%%;
        background: linear-gradient(90deg, transparent, #008080, transparent);
        animation: globalAiProgress 1.4s infinite linear;
      }
      @keyframes globalAiBlink {
        0%%, 80%%, 100%% { opacity: .25; transform: translateY(0); }
        40%% { opacity: 1; transform: translateY(-2px); }
      }
      @keyframes globalAiProgress {
        from { left: -35%%; }
        to { left: 100%%; }
      }
      @media (max-width: 576px) {
        #%s { right: .75rem; bottom: .75rem; }
        #%s .global-ai-panel { width: calc(100vw - 1.5rem); height: calc(100vh - 1.5rem); }
        #%s .global-ai-resize-handle { display: none; }
      }
    ",
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root")
    ))),
    tags$script(HTML(sprintf("
      (function() {
        var panelId = '%s';
        var handleId = '%s';
        var storageKey = 'metminer_global_ai_bot_height';
        function initResize() {
          var panel = document.getElementById(panelId);
          var handle = document.getElementById(handleId);
          if (!panel || !handle || handle.dataset.bound === '1') return;
          handle.dataset.bound = '1';
          var saved = window.localStorage.getItem(storageKey);
          if (saved) panel.style.height = saved;
          var startY = 0;
          var startHeight = 0;
          function onMove(e) {
            var dy = startY - e.clientY;
            var maxH = Math.max(420, window.innerHeight - 32);
            var next = Math.min(maxH, Math.max(420, startHeight + dy));
            panel.style.height = next + 'px';
          }
          function onUp() {
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
            document.body.style.userSelect = '';
            window.localStorage.setItem(storageKey, panel.style.height);
          }
          handle.addEventListener('mousedown', function(e) {
            e.preventDefault();
            startY = e.clientY;
            startHeight = panel.getBoundingClientRect().height;
            document.body.style.userSelect = 'none';
            document.addEventListener('mousemove', onMove);
            document.addEventListener('mouseup', onUp);
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', initResize);
        } else {
          initResize();
        }
        document.addEventListener('shiny:connected', initResize);
      })();
    ", ns("panel"), ns("resize_handle")))),
    div(
      id = ns("global_ai_root"),
      actionButton(ns("open_bot"), label = NULL, icon = bsicons::bs_icon("robot", size = "1.45rem"),
                   class = "global-ai-launcher", title = "Open MetMiner Bot"),
      div(
        id = ns("panel"),
        class = "global-ai-panel",
        div(id = ns("resize_handle"), class = "global-ai-resize-handle", title = "Drag to resize"),
        div(
          class = "global-ai-header",
          div(class = "global-ai-title", bsicons::bs_icon("robot"), span("MetMiner Bot")),
          div(
            class = "d-flex gap-2",
            actionButton(ns("clear_chat"), NULL, icon = icon("trash"), class = "btn btn-sm btn-outline-secondary", title = "Clear chat"),
            actionButton(ns("close_bot"), NULL, icon = icon("xmark"), class = "btn btn-sm btn-outline-secondary", title = "Close")
          )
        ),
        tags$details(
          class = "global-ai-settings",
          tags$summary("LLM settings"),
          div(
            class = "row g-2 mt-2",
            div(class = "col-6", selectInput(ns("provider"), "Provider", choices = c(
              "OpenAI" = "openai", "Gemini" = "gemini", "DeepSeek" = "deepseek",
              "Qwen" = "qwen", "Kimi" = "kimi", "Grok" = "grok"
            ), selected = "openai")),
            div(class = "col-6", selectizeInput(ns("model"), "Model",
              choices = metminer_ai_provider_model_choices("openai"),
              selected = metminer_ai_provider_defaults("openai")$model,
              options = list(create = TRUE)
            )),
            div(class = "col-12", passwordInput(ns("api_key"), "API Key")),
            div(class = "col-12", textInput(ns("base_url"), "Endpoint", value = metminer_ai_provider_defaults("openai")$base_url)),
            div(class = "col-6", selectInput(ns("language"), "Language", choices = metminer_ai_language_choices(), selected = "zh-CN")),
            div(class = "col-6", sliderInput(ns("temperature"), "Temperature", min = 0, max = 1.5, value = 0.3, step = 0.05)),
            div(class = "col-12 d-flex gap-2",
                actionButton(ns("test_api"), "Test", icon = icon("plug"), class = "btn-sm btn-outline-primary"),
                uiOutput(ns("save_config_ui")))
          )
        ),
        div(id = ns("chat_scroll"), class = "global-ai-body", uiOutput(ns("chat_history"))),
        uiOutput(ns("typing_indicator")),
        div(
          class = "global-ai-composer",
          textAreaInput(ns("user_message"), NULL,
            placeholder = "Ask about the current project, parameters, annotation results, feature network, or next steps...",
            rows = 3, width = "100%"
          ),
          div(
            class = "d-flex justify-content-between gap-2 align-items-center",
            div(class = "text-muted small", textOutput(ns("status"), inline = TRUE)),
            div(class = "d-flex gap-2",
                actionButton(ns("summarize_project"), "Summarize", class = "btn-sm btn-outline-secondary"),
                actionButton(ns("send_message"), "Send", icon = icon("paper-plane"), class = "btn-sm btn-primary"))
          )
        )
      )
    )
  )
}

#' Global expandable AI assistant server
#'
#' @noRd
mod_global_ai_bot_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    chat <- reactiveVal(list(
      list(role = "system", content = "MetMiner Bot is ready. Open settings, configure an LLM provider, then ask about the current project.")
    ))
    bot_busy <- reactiveVal(FALSE)
    ai_status <- reactiveVal(NULL)
    status_text <- reactiveVal("Ready.")
    loaded_config_provider <- reactiveVal(NULL)
    api_test_ok <- reactiveVal(FALSE)

    start_ai_request <- function(title = "MetMiner Bot", message = "Preparing request...") {
      bot_busy(TRUE)
      ai_status(list(title = title, message = message))
      status_text(message)
    }

    update_ai_progress <- function(message = "Waiting for LLM response...") {
      status <- shiny::isolate(ai_status())
      ai_status(list(
        title = status$title %||% "MetMiner Bot",
        message = message
      ))
      status_text(message)
    }

    finish_ai_request <- function(answer = NULL, message = "Ready.") {
      if (!is.null(answer)) {
        append_chat(chat, "assistant", answer)
      }
      ai_status(NULL)
      bot_busy(FALSE)
      status_text(message)
    }

    open_panel <- function() {
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').classList.add('is-open'); document.getElementById('%s').style.display='none';",
        session$ns("panel"), session$ns("open_bot")
      ))
    }
    close_panel <- function() {
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').classList.remove('is-open'); document.getElementById('%s').style.display='inline-flex';",
        session$ns("panel"), session$ns("open_bot")
      ))
    }
    scroll_chat_to_bottom <- function() {
      shinyjs::runjs(sprintf(
        "setTimeout(function(){var el=document.getElementById('%s'); if(el){el.scrollTop=el.scrollHeight;}}, 40);",
        session$ns("chat_scroll")
      ))
    }

    observeEvent(input$open_bot, {
      open_panel()
      scroll_chat_to_bottom()
    })
    observeEvent(input$close_bot, close_panel())

    session$onFlushed(function() {
      cfg <- metminer_ai_load_config()
      if (is.null(cfg)) return()
      loaded_config_provider(cfg$provider %||% "openai")
      updateSelectInput(session, "provider", selected = cfg$provider %||% "openai")
      updateSelectizeInput(session, "model",
        choices = metminer_ai_provider_model_choices(cfg$provider %||% "openai"),
        selected = cfg$model %||% metminer_ai_provider_defaults(cfg$provider %||% "openai")$model,
        server = TRUE
      )
      updateTextInput(session, "api_key", value = cfg$api_key %||% "")
      updateTextInput(session, "base_url", value = cfg$base_url %||% metminer_ai_provider_defaults(cfg$provider %||% "openai")$base_url)
      updateSelectInput(session, "language", selected = cfg$language %||% "zh-CN")
      updateSliderInput(session, "temperature", value = cfg$temperature %||% 0.3)
      status_text(sprintf("Loaded config: %s / %s", cfg$provider %||% "openai", cfg$model %||% ""))
    }, once = TRUE)

    observeEvent(input$provider, {
      if (identical(input$provider, loaded_config_provider())) {
        loaded_config_provider(NULL)
        return()
      }
      api_test_ok(FALSE)
      defaults <- metminer_ai_provider_defaults(input$provider)
      updateSelectizeInput(session, "model", choices = metminer_ai_provider_model_choices(input$provider),
                           selected = defaults$model, server = TRUE)
      updateTextInput(session, "base_url", value = defaults$base_url)
    }, ignoreInit = TRUE)

    observeEvent(list(input$model, input$base_url, input$api_key), api_test_ok(FALSE), ignoreInit = TRUE)

    output$save_config_ui <- renderUI({
      if (!isTRUE(metminer_ai_config_persistence_available())) {
        return(tags$small(class = "text-muted", "Config persistence disabled."))
      }
      actionButton(session$ns("save_config"), "Save", icon = icon("save"), class = "btn-sm btn-outline-success")
    })
    output$status <- renderText(status_text())

    output$typing_indicator <- renderUI({
      req(isTRUE(bot_busy()))
      status <- ai_status()
      tags$div(
        class = "global-ai-typing-bar",
        tags$div(
          class = "global-ai-typing-row",
          tags$span(status$message %||% "Waiting for LLM response..."),
          tags$div(class = "global-ai-thinking-dots", tags$span(), tags$span(), tags$span())
        ),
        tags$div(class = "global-ai-progress")
      )
    })

    output$chat_history <- renderUI({
      rendered <- lapply(chat(), function(msg) {
        role <- msg$role %||% "assistant"
        css <- switch(role,
          user = "global-ai-message global-ai-user",
          system = "global-ai-message global-ai-system",
          "global-ai-message global-ai-assistant"
        )
        tags$div(class = css, markdown_to_html(msg$content %||% ""))
      })
      if (isTRUE(bot_busy())) {
        status <- ai_status()
        rendered[[length(rendered) + 1L]] <- tags$div(
          class = "global-ai-message global-ai-assistant",
          tags$div(class = "fw-semibold", status$title %||% "MetMiner Bot"),
          tags$div(class = "text-muted", status$message %||% "Waiting for LLM response..."),
          tags$div(class = "global-ai-thinking-dots", tags$span(), tags$span(), tags$span()),
          tags$div(class = "global-ai-progress")
        )
      }
      tags$div(rendered)
    })

    outputOptions(output, "save_config_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "status", suspendWhenHidden = FALSE)
    outputOptions(output, "typing_indicator", suspendWhenHidden = FALSE)
    outputOptions(output, "chat_history", suspendWhenHidden = FALSE)

    observeEvent(list(chat(), bot_busy(), ai_status()), scroll_chat_to_bottom(), ignoreInit = TRUE)
    observeEvent(input$clear_chat, chat(list(list(role = "system", content = "Chat cleared."))))

    observeEvent(input$test_api, {
      req(input$api_key)
      status_text("Testing API...")
      tryCatch({
        ans <- metminer_ai_test_connection(input$provider, input$model, input$api_key, input$base_url)
        api_test_ok(TRUE)
        status_text(paste("Connection OK:", trimws(ans)))
      }, error = function(e) {
        api_test_ok(FALSE)
        status_text(paste("Connection failed:", e$message))
      })
    })

    observeEvent(input$save_config, {
      if (!isTRUE(metminer_ai_config_persistence_available())) return()
      req(input$provider, input$model, input$api_key, input$base_url)
      if (!isTRUE(api_test_ok())) {
        status_text("Run Test successfully before saving.")
        return()
      }
      path <- tryCatch(
        metminer_ai_save_config(input$provider, input$model, input$api_key, input$base_url, input$temperature, input$language),
        error = function(e) e
      )
      status_text(if (inherits(path, "error")) paste("Save failed:", path$message) else paste("Config saved:", path))
    })

    observeEvent(input$summarize_project, {
      updateTextAreaInput(session, "user_message", value = "请总结当前 MetMiner 项目的数据状态、已经完成的步骤、主要结果和建议的下一步。")
      shinyjs::runjs(sprintf("document.getElementById('%s').click();", session$ns("send_message")))
    })

    observeEvent(input$send_message, {
      if (isTRUE(bot_busy())) return()
      provider <- input$provider
      model <- input$model
      api_key <- input$api_key
      base_url <- input$base_url
      temperature <- input$temperature
      language <- input$language
      user_msg <- trimws(input$user_message %||% "")
      chat_snapshot <- shiny::isolate(chat())
      if (!has_text(api_key)) {
        append_chat(chat, "system", "Please enter an API key in LLM settings first.")
        status_text("API key required.")
        open_panel()
        scroll_chat_to_bottom()
        return()
      }
      if (!has_text(user_msg)) {
        append_chat(chat, "system", "Please enter a question first.")
        status_text("Question is empty.")
        scroll_chat_to_bottom()
        return()
      }

      append_chat(chat, "user", user_msg)
      updateTextAreaInput(session, "user_message", value = "")
      start_ai_request("MetMiner Bot", "Preparing project context and MetMiner2 knowledge base...")
      scroll_chat_to_bottom()

      later::later(function() {
        shiny::withReactiveDomain(session, {
          messages <- tryCatch({
            project_context <- metminer_global_ai_project_context(global_data, prj_init)
            app_knowledge <- metminer_global_ai_knowledge_base()
            retrieved_knowledge <- metminer_global_ai_rag_retrieve(user_msg, top_n = 6)
            chat_context <- metminer_ai_summarize_chat_context(chat_snapshot, max_summary_chars = 1200)
            metminer_global_ai_messages(user_msg, project_context, app_knowledge, retrieved_knowledge, chat_context, language)
          }, error = function(e) e)

          if (inherits(messages, "error")) {
            finish_ai_request(paste("MetMiner Bot context preparation failed:", messages$message), "Failed.")
            return()
          }

          update_ai_progress("Sending request to the selected LLM provider...")

          future::plan(future::multisession, workers = 1)
          fut <- tryCatch(future::future(
            {
              call_llm_async(provider, model, api_key, base_url, temperature, messages)
            },
            packages = c("httr2", "jsonlite"),
            globals = list(
              call_llm_async = call_llm_async,
              provider = provider, model = model, api_key = api_key, base_url = base_url,
              temperature = temperature, messages = messages
            ),
            seed = TRUE
          ), error = function(e) e)

          if (inherits(fut, "error")) {
            finish_ai_request(paste("Request failed:", fut$message), "Failed.")
            return()
          }

          request_started_at <- Sys.time()
          poll_result <- function() {
            shiny::withReactiveDomain(session, {
              if (future::resolved(fut)) {
                update_ai_progress("Receiving and formatting the answer...")
                answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
                finish_ai_request(answer, "Ready.")
              } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 360) {
                finish_ai_request("LLM call timed out after 360 seconds.", "Timed out.")
              } else {
                update_ai_progress("LLM is reasoning. This may take a while for thinking models...")
                later::later(poll_result, 0.5)
              }
            })
          }
          later::later(poll_result, 0.2)
        })
      }, 0.1)
    })
  })
}

metminer_global_ai_messages <- function(user_question,
                                        project_context,
                                        app_knowledge,
                                        retrieved_knowledge = data.frame(),
                                        chat_context = "",
                                        language = "zh-CN") {
  language_label <- metminer_ai_language_label(language)
  system_prompt <- paste(
    "You are MetMiner Bot, a global assistant embedded in a Shiny LC-MS plant metabolomics application.",
    paste0("Answer in ", language_label, ". Keep feature IDs, file names, database IDs, adducts, m/z and RT values unchanged."),
    "Use app_knowledge for general questions about MetMiner2 features, workflow, modules, and design intent.",
    "Use retrieved_knowledge as the highest-priority source for questions about TidyMass, TidyMass2, MetMiner, MetMiner2, supported inputs, package relationships, and literature-backed software background.",
    "For questions about Shiny parameters, thresholds, or tuning, answer from retrieved_knowledge parameter chunks first and explain the practical effect of increasing or decreasing the parameter.",
    "Do not claim that MetMiner2 inherits or implements TidyMass2 metabolite-origin inference, MetOriginDB, or metabolic feature-based functional module analysis unless retrieved_knowledge explicitly says MetMiner2 implements that feature. Treat TidyMass2 as related literature/ecosystem background only.",
    "Use project_context and chat_context for claims about the current user's data, current project state, tables, results, and next actions.",
    "Do not say that project data are missing when the user asks a general question about MetMiner2 itself; answer from app_knowledge instead.",
    "If the current app state does not contain enough evidence for a project-specific question, say what is missing and suggest concrete next actions inside MetMiner.",
    "For questions about supported input formats, follow app_knowledge$current_ui_input_requirements exactly. Do not broaden support based on general LC-MS software knowledge.",
    "Do not claim that the current complete raw-data workflow supports NetCDF/CDF or Excel sample metadata. In the current UI, sample metadata upload is CSV, raw import upload is a ZIP, and parameter optimisation scans mzXML files.",
    "When retrieved_knowledge contains source, DOI, or URL fields relevant to the answer, cite them compactly in prose.",
    "Be concise and practical. Avoid unsupported biological or bibliographic claims.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("retrieved_knowledge JSON:\n", metminer_ai_safe_json(retrieved_knowledge))),
    list(role = "assistant", content = "Retrieved RAG knowledge loaded."),
    list(role = "user", content = paste0("app_knowledge JSON:\n", metminer_ai_safe_json(app_knowledge))),
    list(role = "assistant", content = "MetMiner2 application knowledge loaded."),
    list(role = "user", content = paste0("project_context JSON:\n", metminer_ai_safe_json(project_context))),
    list(role = "assistant", content = "Project context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_knowledge_base <- function() {
  list(
    product = list(
      name = "MetMiner2",
      scope = "Shiny-based plant LC-MS untargeted metabolomics platform",
      core_goal = "Improve metabolite annotation precision by integrating LC-MS evidence, species-specific databases, feature-network context, and human/AI review.",
      positioning = "MetMiner2 keeps the original end-to-end MetMiner workflow, but shifts the center of gravity toward annotation-centered evidence integration.",
      raw_data_format_note = "The current raw-data workflow is designed around converted LC-MS files in POS/NEG folders. Parameter optimisation currently scans .mzXML files. Do not describe NetCDF/.cdf as fully supported unless the import and optimisation code are extended and tested."
    ),
    current_ui_input_requirements = list(
      project_initialization = list(
        sample_metadata = "Upload Sample Information as .csv only in the current Project Init UI.",
        required_mapping = c("sample_id", "injection.order", "class", "group", "batch"),
        note = "The sample_id column should match the key part of raw file names or existing mass_dataset sample IDs. Avoid claiming .xlsx/.xls support for this UI unless it is implemented."
      ),
      data_import_sources = list(
        raw_ms_data = "Upload one MS1 ZIP file. After extraction, MetMiner expects POS and/or NEG folders under the detected MS1 root.",
        peak_picking_table = "Upload peak table as .csv.",
        mass_dataset = "Upload existing positive and/or negative mass_dataset objects as .rda."
      ),
      raw_folder_structure = list(
        expected = "MS1 ZIP should contain POS and/or NEG directories. Each polarity directory may contain Subject and/or QC sample folders as used by the import logic.",
        polarity = "Positive and negative ionization modes are processed separately.",
        parameter_optimization = "Current parameter optimization functions scan .mzXML files. NetCDF/.cdf is not a guaranteed complete workflow format."
      ),
      unsupported_or_not_guaranteed = c(
        "Do not state that NetCDF/.cdf is supported by the full current workflow.",
        "Do not state that Excel sample metadata is supported by Project Init.",
        "Do not state that raw vendor formats are directly supported unless the user provides converted files and the workflow has been tested."
      )
    ),
    major_enhancements = list(
      basic_processing = c(
        "Parameter optimization for raw LC-MS data import and peak picking",
        "Blank-informed noise masking",
        "QC and report output",
        "Imputation and normalization",
        "Improved sample metadata handling during data import"
      ),
      core_modules = c(
        "Feature Network and ISF audit",
        "AI-assisted annotation review",
        "Species-specific PlantCyc and KEGG database construction",
        "PlantCyc/KEGG compound ID mapping and merged annotation databases",
        "Annotation filtering with expand/collapse review tables",
        "Differential abundance and pathway enrichment workflows"
      )
    ),
    feature_network = list(
      purpose = "Represent multiple LC-MS features that may arise from the same metabolite or related ion chemistry, rather than assuming one feature equals one metabolite.",
      evidence_types = c("isotope-like relationships", "adduct relationships", "source-fragment/ISF relationships", "neutral-loss relationships", "recurrent ion patterns", "co-elution and retention-time context", "MS2 support when available"),
      outputs = c("feature network tables", "recurrent ion network", "sub-network roles", "pseudo-area matrix", "non-redundant annotation table", "annotation audit table"),
      interpretation = "The module helps distinguish likely parent features, redundant adduct/isotope/source-fragment features, recurrent ions, and unresolved network instances for later annotation filtering."
    ),
    ai_annotation = list(
      purpose = "Use LLMs as an evidence reviewer, not as an automatic overwriter of annotation results.",
      evidence_bundle = c("non-redundant annotation table", "redundancy audit table", "raw metID candidates", "MS1 feature table", "MS/MS fragment summary", "Feature Network roles and edges", "recurrent ion status", "PlantCyc/KEGG database evidence", "user LC-MS conditions"),
      constraints = c("evidence-only reasoning", "do not fabricate DOI or literature", "cite only verified paper-search or user-provided sources", "report conflicts and uncertainty", "human review remains the final gate"),
      supported_providers = c("OpenAI-compatible APIs", "Gemini", "DeepSeek", "Qwen", "Kimi", "Grok")
    ),
    database_construction = list(
      plantcyc = c(
        "Builds LC-MS-oriented species-specific databases from licensed PlantCyc/PMN PGDB archives or PlantCyc SmartTable exports.",
        "Local PGDB builder parses compounds, reactions, pathways, cross references, formulas, masses, and pathway-compound links.",
        "MS1 database construction filters entries unsuitable for LC-MS annotation, such as missing formula or m/z and very low mass compounds.",
        "CoA-related compounds are handled carefully because intact CoA species can be poorly suited for routine LC-MS MS1 annotation but may still provide useful MS2/pathway context.",
        "Pathway database construction supports PlantCyc compound IDs for enrichment."
      ),
      kegg = c(
        "Builds organism-specific KEGG resources from gene, KO, EC, reaction, compound, and pathway links.",
        "Uses reaction-supported pathway extraction rather than broad pathway inclusion.",
        "Flags weakly supported pathways, common-precursor-only pathways, or upstream-only pathways for AI/manual curation before enrichment background construction."
      ),
      id_mapping = "PlantCyc and KEGG compound IDs can be mapped and merged so downstream annotation and enrichment can retain both identifier systems."
    ),
    homepage_figures = list(
      figure1 = "Development and Enhancements in MetMiner2: platform overview separating basic preprocessing upgrades from core annotation-focused modules.",
      figure2 = "Feature Network workflow: relationship discovery, recurrent ion analysis, network roles, and annotation filtering support.",
      figure3 = "PlantCyc/KEGG database construction: species-specific database generation and pathway background curation."
    ),
    recommended_workflow = c(
      "Initialize a project and import raw LC-MS data with sample metadata.",
      "Run cleaning, blank/QC checks, imputation, and normalization.",
      "Annotate using species-specific PlantCyc/KEGG databases when available.",
      "Build Feature Network evidence and inspect redundant or recurrent features.",
      "Use AI-assisted review for ambiguous annotation or pathway curation, keeping human review as the final decision.",
      "Run differential analysis and pathway enrichment with curated annotation/background databases."
    )
  )
}

metminer_global_ai_project_context <- function(global_data, prj_init) {
  shiny::isolate({
    g <- shiny::reactiveValuesToList(global_data, all.names = TRUE)
    object_keys <- names(g)[grepl("^object_", names(g))]
    objects <- lapply(object_keys, function(key) metminer_global_ai_object_summary(key, g[[key]]))
    names(objects) <- object_keys

    sample_info <- as.data.frame(prj_init$sample_info %||% data.frame())
    list(
      project = list(
        job_id = prj_init$job_id %||% NA_character_,
        working_dir = prj_init$wd %||% NA_character_,
        mass_dataset_dir = prj_init$mass_dataset_dir %||% NA_character_,
        sample_info_rows = nrow(sample_info),
        sample_info_columns = colnames(sample_info)
      ),
      global_data_keys = names(g),
      objects = objects,
      annotation_filter = metminer_global_ai_filter_summary(g$annotation_filter_result),
      differential_analysis = metminer_global_ai_result_summary(g$dam_result %||% g$differential_result),
      enrichment = metminer_global_ai_result_summary(g$enrichment_result),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_object_summary <- function(key, obj) {
  if (is.null(obj)) return(list(key = key, available = FALSE))
  variable_info <- tryCatch(metminer_safe_extract_variable_info(obj), error = function(e) data.frame())
  sample_info <- tryCatch(massdataset::extract_sample_info(obj), error = function(e) data.frame())
  annotation_table <- tryCatch(metminer_safe_extract_annotation_table(obj), error = function(e) data.frame())
  network <- tryCatch(extract_feature_network(obj), error = function(e) data.frame())
  recurrent <- tryCatch(extract_recurrent_ion_network(obj), error = function(e) empty_recurrent_ion_network())
  list(
    key = key,
    available = TRUE,
    class = paste(class(obj), collapse = "/"),
    variables = nrow(variable_info),
    samples = nrow(sample_info),
    annotation_rows = nrow(annotation_table),
    feature_network_edges = nrow(network),
    recurrent_ion_groups = nrow(recurrent$groups %||% data.frame()),
    variable_columns = utils::head(colnames(variable_info), 30),
    sample_columns = utils::head(colnames(sample_info), 30)
  )
}

metminer_global_ai_filter_summary <- function(filter_result) {
  if (is.null(filter_result)) return(list(available = FALSE))
  final <- as.data.frame(filter_result$final_table %||% data.frame())
  audit <- as.data.frame(filter_result$redundancy_table %||% data.frame())
  list(
    available = TRUE,
    final_rows = nrow(final),
    audit_rows = nrow(audit),
    record_types = if ("record_type" %in% colnames(final)) as.list(table(final$record_type)) else list(),
    recurrent_flagged = if ("recurrent_status" %in% colnames(audit)) sum(audit$recurrent_status != "none", na.rm = TRUE) else NA_integer_,
    suspected_interference = if ("suspected_interference" %in% colnames(audit)) sum(audit$suspected_interference, na.rm = TRUE) else NA_integer_
  )
}

metminer_global_ai_result_summary <- function(result) {
  if (is.null(result)) return(list(available = FALSE))
  if (is.data.frame(result)) return(list(available = TRUE, rows = nrow(result), columns = colnames(result)))
  if (is.list(result)) {
    return(list(
      available = TRUE,
      names = names(result),
      table_sizes = lapply(result, function(x) if (is.data.frame(x)) nrow(x) else NA_integer_)
    ))
  }
  list(available = TRUE, class = paste(class(result), collapse = "/"))
}
