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
      .global-ai-header-main {
        min-width: 0;
        display: flex;
        flex-direction: column;
        gap: .18rem;
      }
      #%s .global-ai-title { display: flex; align-items: center; gap: .5rem; font-weight: 700; color: #006b6b; }
      .global-ai-header-status {
        color: #64748b;
        font-size: .78rem;
        line-height: 1.25;
        max-width: 17rem;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
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
        border: 1px solid #cfe7e7;
        border-radius: 8px;
        background: #f4fbfb;
        padding: .55rem .65rem;
        margin-bottom: .65rem;
        color: #006b6b;
        font-size: .88rem;
        box-shadow: 0 6px 16px rgba(0,128,128,.08);
      }
      #%s .global-ai-typing-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: .75rem;
      }
      #%s .global-ai-composer { border-top: 1px solid #e2e8f0; background: #f8fafc; padding: .85rem; }
      #%s .global-ai-action-bar {
        border-top: 1px solid #d7e5e5;
        background: #eef7f7;
        padding: .75rem .85rem;
      }
      #%s .global-ai-action-title { font-weight: 700; color: #006b6b; margin-bottom: .35rem; }
      #%s .global-ai-action-text { color: #334155; font-size: .9rem; margin-bottom: .55rem; }
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
      .global-ai-agent-select .form-group,
      .global-ai-agent-select .shiny-input-container {
        margin-bottom: 0;
        width: 8.4rem;
      }
      .global-ai-agent-select .selectize-control {
        margin-bottom: 0;
      }
      .global-ai-agent-select .selectize-input {
        min-height: 31px;
        padding: .25rem 1.65rem .25rem .75rem;
        font-size: .86rem;
        border-radius: 999px;
        border-color: #d7e5e5;
        background: #fff;
        box-shadow: none;
        line-height: 1.35;
        color: #006b6b;
        font-weight: 600;
      }
      .global-ai-agent-select .selectize-input.dropdown-active {
        border-radius: 999px;
        border-color: #9fcaca;
      }
      .global-ai-agent-select .selectize-control.single .selectize-input::after {
        right: .72rem;
        border-color: transparent transparent #334155 transparent;
        border-width: 0 5px 5px 5px;
      }
      .global-ai-agent-select .selectize-dropdown {
        top: auto !important;
        bottom: calc(100%% + .35rem) !important;
        left: 0 !important;
        width: 17rem !important;
        min-width: 17rem !important;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        box-shadow: 0 14px 34px rgba(15,23,42,.16);
        overflow: hidden;
        z-index: 1100;
      }
      .global-ai-agent-select .selectize-dropdown-content {
        max-height: 14rem;
      }
      .global-ai-agent-select .selectize-dropdown .option {
        padding: .48rem .72rem;
        font-size: .9rem;
        white-space: nowrap;
      }
      .global-ai-agent-select .selectize-dropdown .active {
        background: #eef7f7;
        color: #006b6b;
      }
      @media (max-width: 576px) {
        #%s { right: .75rem; bottom: .75rem; }
        #%s .global-ai-panel { width: calc(100vw - 1.5rem); height: calc(100vh - 1.5rem); }
        #%s .global-ai-resize-handle { display: none; }
        .global-ai-header-status { max-width: 11rem; }
        .global-ai-agent-select .shiny-input-container { width: 7.8rem; }
        .global-ai-agent-select .selectize-dropdown {
          width: 14.5rem !important;
          min-width: 14.5rem !important;
        }
      }
    ",
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root"),
      ns("global_ai_root"),
      ns("global_ai_root"), ns("global_ai_root"), ns("global_ai_root")
    ))),
    tags$script(HTML(sprintf("
      (function() {
        var panelId = '%s';
        var handleId = '%s';
        var openButtonId = '%s';
        var closeButtonId = '%s';
        var storageKey = 'metminer_global_ai_bot_height';
        function openBotPanel() {
          var panel = document.getElementById(panelId);
          var openButton = document.getElementById(openButtonId);
          if (panel) panel.classList.add('is-open');
          if (openButton) openButton.style.display = 'none';
        }
        function closeBotPanel() {
          var panel = document.getElementById(panelId);
          var openButton = document.getElementById(openButtonId);
          if (panel) panel.classList.remove('is-open');
          if (openButton) openButton.style.display = 'inline-flex';
        }
        function initToggle() {
          var openButton = document.getElementById(openButtonId);
          var closeButton = document.getElementById(closeButtonId);
          if (openButton && openButton.dataset.globalAiToggleBound !== '1') {
            openButton.dataset.globalAiToggleBound = '1';
            openButton.addEventListener('click', openBotPanel);
          }
          if (closeButton && closeButton.dataset.globalAiToggleBound !== '1') {
            closeButton.dataset.globalAiToggleBound = '1';
            closeButton.addEventListener('click', closeBotPanel);
          }
        }
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
        function initGlobalAiBot() {
          initToggle();
          initResize();
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', initGlobalAiBot);
        } else {
          initGlobalAiBot();
        }
        document.addEventListener('shiny:connected', initGlobalAiBot);
      })();
    ", ns("panel"), ns("resize_handle"), ns("open_bot"), ns("close_bot")))),
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
          div(
            class = "global-ai-header-main",
            div(class = "global-ai-title", bsicons::bs_icon("robot"), span("MetMiner Bot")),
            div(class = "global-ai-header-status", textOutput(ns("status"), inline = TRUE))
          ),
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
        uiOutput(ns("pending_action_ui")),
        div(
          class = "global-ai-composer",
          uiOutput(ns("typing_indicator")),
          textAreaInput(ns("user_message"), NULL,
            placeholder = "Ask about the current project, parameters, annotation results, feature network, or next steps...",
            rows = 3, width = "100%"
          ),
          div(
            class = "d-flex justify-content-between gap-2 align-items-center",
            div(
              class = "global-ai-agent-select",
              selectizeInput(ns("agent_command"), NULL,
                choices = c(
                  "@agent" = "",
                  "@lc-condition" = "lc-condition",
                  "@data-import-advisor" = "data-import-advisor",
                  "@noise-filter-advisor" = "noise-filter-advisor",
                  "@outlier-advisor" = "outlier-advisor",
                  "@missing-value-advisor" = "missing-value-advisor",
                  "@normalization-advisor" = "normalization-advisor",
                  "@feature-network-advisor" = "feature-network-advisor",
                  "@annotation-advisor" = "annotation-advisor",
                  "@annotation-filter-advisor" = "annotation-filter-advisor",
                  "@differential-advisor" = "differential-advisor",
                  "@enrichment-advisor" = "enrichment-advisor",
                  "@database-advisor" = "database-advisor",
                  "@paper-search-mcp" = "paper-search-mcp",
                  "@kegg-review" = "kegg-review",
                  "Project summary" = "project-summary"
                ),
                selected = "",
                width = "100%",
                options = list(
                  allowEmptyOption = TRUE,
                  maxOptions = 20
                )
              )
            ),
            div(class = "d-flex gap-2 align-items-center",
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
    pending_action <- reactiveVal(NULL)
    recent_action <- reactiveVal(NULL)
    rejected_actions <- reactiveVal(character())
    active_agent_command <- reactiveVal(NULL)

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
        "var panel=document.getElementById('%s'); var openButton=document.getElementById('%s'); if(panel){panel.classList.add('is-open');} if(openButton){openButton.style.display='none';}",
        session$ns("panel"), session$ns("open_bot")
      ))
    }
    close_panel <- function() {
      shinyjs::runjs(sprintf(
        "var panel=document.getElementById('%s'); var openButton=document.getElementById('%s'); if(panel){panel.classList.remove('is-open');} if(openButton){openButton.style.display='inline-flex';}",
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

    output$pending_action_ui <- renderUI({
      action <- pending_action()
      if (is.null(action)) return(NULL)
      tags$div(
        class = "global-ai-action-bar",
        tags$div(class = "global-ai-action-title", action$title %||% "Confirm action"),
        tags$div(class = "global-ai-action-text", action$message %||% ""),
        tags$div(
          class = "d-flex gap-2",
          actionButton(session$ns("confirm_pending_action"), "确定", icon = icon("check"), class = "btn-sm btn-success"),
          actionButton(session$ns("cancel_pending_action"), "取消", icon = icon("xmark"), class = "btn-sm btn-outline-secondary")
        )
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
    outputOptions(output, "pending_action_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "chat_history", suspendWhenHidden = FALSE)

    observeEvent(list(chat(), bot_busy(), ai_status()), scroll_chat_to_bottom(), ignoreInit = TRUE)
    observeEvent(input$clear_chat, {
      chat(list(list(role = "system", content = "Chat cleared.")))
      pending_action(NULL)
      recent_action(NULL)
      active_agent_command(NULL)
      updateSelectizeInput(session, "agent_command", selected = "")
      status_text("Ready.")
    })

    observeEvent(input$user_message, {
      msg <- input$user_message %||% ""
      if (identical(active_agent_command(), "lc-condition") &&
          !metminer_global_ai_is_lcms_condition_request(msg)) {
        active_agent_command(NULL)
      }
    }, ignoreInit = TRUE)

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

    observeEvent(input$agent_command, {
      command <- input$agent_command %||% ""
      if (!has_text(command)) return()
      active_agent_command(command)
      template <- switch(command,
        "lc-condition" = paste(
          "@lc-condition",
          "请从下面的液质方法描述中提取 Project Init 的 LC-MS Conditions 关键字段：",
          "",
          sep = "\n"
        ),
        "data-import-advisor" = paste(
          "@data-import-advisor",
          "请根据当前项目的 data import 状态，帮我判断导入方式、文件结构或峰提取参数是否合适：",
          "",
          sep = "\n"
        ),
        "noise-filter-advisor" = paste(
          "@noise-filter-advisor",
          "请根据当前项目的 blank/QC/missing-value/RSD 去噪参数和结果，帮我判断过滤是否合适：",
          "",
          sep = "\n"
        ),
        "outlier-advisor" = paste(
          "@outlier-advisor",
          "请根据当前项目的 NA frequency、PCA 和 outlier detection 参数，帮我判断是否需要移除样本：",
          "",
          sep = "\n"
        ),
        "normalization-advisor" = paste(
          "@normalization-advisor",
          "请根据当前项目的 normalization/integration 参数和 QC 状态，帮我判断归一化策略是否合适：",
          "",
          sep = "\n"
        ),
        "feature-network-advisor" = paste(
          "@feature-network-advisor",
          "请根据当前项目的 feature network 参数和网络结果，帮我判断关系识别、recurrent ion 或 MS2 audit 是否需要调整：",
          "",
          sep = "\n"
        ),
        "annotation-advisor" = paste(
          "@annotation-advisor",
          "请根据当前项目的注释数据库、ppm/RT/candidate 参数和注释结果，帮我判断注释策略是否合适：",
          "",
          sep = "\n"
        ),
        "annotation-filter-advisor" = paste(
          "@annotation-filter-advisor",
          "请根据当前项目的 annotation filtering 参数和 redundancy/recurrent ion 结果，帮我判断过滤策略是否合适：",
          "",
          sep = "\n"
        ),
        "differential-advisor" = paste(
          "@differential-advisor",
          "请根据当前项目的差异分析分组、阈值、火山图和 selected feature 信息，帮我判断参数是否合适：",
          "",
          sep = "\n"
        ),
        "enrichment-advisor" = paste(
          "@enrichment-advisor",
          "请根据当前项目的差异代谢物、ID mapping 和 KEGG/PlantCyc 富集结果，帮我判断富集分析是否合适：",
          "",
          sep = "\n"
        ),
        "database-advisor" = paste(
          "@database-advisor",
          "请根据当前项目的 KEGG/PlantCyc database 构建和 ID mapping 状态，帮我判断数据库是否适合后续注释和富集：",
          "",
          sep = "\n"
        ),
        "missing-value-advisor" = paste(
          "@missing-value-advisor",
          "请根据当前项目的缺失值分布和 imputation 参数，帮我判断填补方法和参数是否合适：",
          "",
          sep = "\n"
        ),
        "paper-search-mcp" = "@paper-search-mcp 请检索并总结与当前问题相关的论文证据：",
        "kegg-review" = "@kegg-review review 现在这个 KEGG 数据库的存疑通路",
        "project-summary" = "请总结当前 MetMiner 项目的数据状态、已经完成的步骤、主要结果和建议的下一步。",
        ""
      )
      current <- input$user_message %||% ""
      next_value <- if (has_text(current)) paste(trimws(current), template, sep = "\n\n") else template
      updateTextAreaInput(session, "user_message", value = next_value)
      updateSelectizeInput(session, "agent_command", selected = "")
      open_panel()
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_pending_action, {
      action <- pending_action()
      if (!is.null(action$key)) {
        rejected_actions(unique(c(rejected_actions(), action$key)))
      }
      pending_action(NULL)
      append_chat(chat, "system", "已取消当前待执行任务。这个 KEGG review 请求本次会话中不会再自动弹出。")
      status_text("Ready.")
    })

    observeEvent(input$confirm_pending_action, {
      action <- pending_action()
      if (is.null(action)) return()
      if (identical(action$type, "kegg_pathway_review")) {
        recent_action(action)
        pending_action(NULL)
        metminer_global_ai_run_kegg_review_action(
          action = action,
          chat = chat,
          start_ai_request = start_ai_request,
          update_ai_progress = update_ai_progress,
          finish_ai_request = finish_ai_request,
          provider = input$provider,
          model = input$model,
          api_key = input$api_key,
          base_url = input$base_url,
          temperature = input$temperature,
          session = session,
          global_data = global_data
        )
      }
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

      pending <- pending_action()
      if (!is.null(pending) && metminer_global_ai_has_agent_token(user_msg) && metminer_global_ai_is_negative(user_msg)) {
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        if (!is.null(pending$key)) {
          rejected_actions(unique(c(rejected_actions(), pending$key)))
        }
        pending_action(NULL)
        append_chat(chat, "system", "已记录：本次会话中不再自动弹出这个 KEGG pathway review 请求。")
        status_text("Ready.")
        return()
      }

      if (!is.null(pending) && metminer_global_ai_has_agent_token(user_msg) && metminer_global_ai_is_affirmative(user_msg)) {
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        recent_action(pending)
        pending_action(NULL)
        if (identical(pending$type, "kegg_pathway_review")) {
          metminer_global_ai_run_kegg_review_action(
            action = pending,
            chat = chat,
            start_ai_request = start_ai_request,
            update_ai_progress = update_ai_progress,
            finish_ai_request = finish_ai_request,
            provider = provider,
            model = model,
            api_key = api_key,
            base_url = base_url,
            temperature = temperature,
            session = session,
            global_data = global_data
          )
        }
        return()
      }

      retry <- recent_action()
      if (!is.null(retry) &&
          identical(retry$type, "kegg_pathway_review") &&
          metminer_global_ai_has_agent_token(user_msg) &&
          metminer_global_ai_is_affirmative_or_retry(user_msg) &&
          is.null(shiny::isolate(global_data$kegg_ai_review))) {
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        metminer_global_ai_run_kegg_review_action(
          action = retry,
          chat = chat,
          start_ai_request = start_ai_request,
          update_ai_progress = update_ai_progress,
          finish_ai_request = finish_ai_request,
          provider = provider,
          model = model,
          api_key = api_key,
          base_url = base_url,
          temperature = temperature,
          session = session,
          global_data = global_data
        )
        return()
      }

      if (metminer_global_ai_is_lcms_condition_request(user_msg)) {
        active_agent_command(NULL)
        start_ai_request("LC-MS Conditions", "Preparing LC-MS condition extraction...")
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        open_panel()
        scroll_chat_to_bottom()
        later::later(function() {
          shiny::withReactiveDomain(session, {
            metminer_global_ai_run_lcms_condition_action(
              user_msg = user_msg,
              chat = chat,
              start_ai_request = start_ai_request,
              update_ai_progress = update_ai_progress,
              finish_ai_request = finish_ai_request,
              provider = provider,
              model = model,
              api_key = api_key,
              base_url = base_url,
              temperature = temperature,
              session = session,
              prj_init = prj_init
            )
          })
        }, 0.15)
        return()
      }

      if (metminer_global_ai_is_data_import_advisor_request(user_msg, active_agent_command())) {
        active_agent_command(NULL)
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        start_ai_request("Data Import Advisor", "Collecting data-import context...")
        open_panel()
        scroll_chat_to_bottom()
        later::later(function() {
          shiny::withReactiveDomain(session, {
            messages <- tryCatch({
              advisor_context <- metminer_global_ai_data_import_context(global_data, prj_init)
              retrieved_knowledge <- metminer_global_ai_rag_retrieve(
                paste(user_msg, "data import raw MS peak picking xcms massprocesser parameters sample metadata"),
                top_n = 6
              )
              chat_context <- metminer_ai_summarize_chat_context(chat_snapshot, max_summary_chars = 900)
              metminer_global_ai_data_import_advisor_messages(
                user_question = user_msg,
                advisor_context = advisor_context,
                retrieved_knowledge = retrieved_knowledge,
                chat_context = chat_context,
                language = language
              )
            }, error = function(e) e)

            if (inherits(messages, "error")) {
              finish_ai_request(paste("Data Import Advisor context preparation failed:", messages$message), "Failed.")
              return()
            }

            update_ai_progress("Sending Data Import Advisor request to the selected LLM provider...")
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
              finish_ai_request(paste("Data Import Advisor request failed:", fut$message), "Failed.")
              return()
            }

            request_started_at <- Sys.time()
            poll_result <- function() {
              shiny::withReactiveDomain(session, {
                if (future::resolved(fut)) {
                  update_ai_progress("Receiving Data Import Advisor answer...")
                  answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
                  finish_ai_request(answer, "Ready.")
                } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 360) {
                  finish_ai_request("Data Import Advisor call timed out after 360 seconds.", "Timed out.")
                } else {
                  update_ai_progress("Data Import Advisor is reasoning...")
                  later::later(poll_result, 0.5)
                }
              })
            }
            later::later(poll_result, 0.2)
          })
        }, 0.1)
        return()
      }

      if (metminer_global_ai_is_missing_value_advisor_request(user_msg, active_agent_command())) {
        active_agent_command(NULL)
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        start_ai_request("Missing Value Advisor", "Collecting missing-value/imputation context...")
        open_panel()
        scroll_chat_to_bottom()
        later::later(function() {
          shiny::withReactiveDomain(session, {
            messages <- tryCatch({
              advisor_context <- metminer_global_ai_missing_value_context(global_data, prj_init)
              retrieved_knowledge <- metminer_global_ai_rag_retrieve(
                paste(user_msg, "missing value imputation KNN missForest BPCA PPCA SVD rowmax colmax maxp nPcs metabolomics"),
                top_n = 6
              )
              chat_context <- metminer_ai_summarize_chat_context(chat_snapshot, max_summary_chars = 900)
              metminer_global_ai_missing_value_advisor_messages(
                user_question = user_msg,
                advisor_context = advisor_context,
                retrieved_knowledge = retrieved_knowledge,
                chat_context = chat_context,
                language = language
              )
            }, error = function(e) e)

            if (inherits(messages, "error")) {
              finish_ai_request(paste("Missing Value Advisor context preparation failed:", messages$message), "Failed.")
              return()
            }

            update_ai_progress("Sending Missing Value Advisor request to the selected LLM provider...")
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
              finish_ai_request(paste("Missing Value Advisor request failed:", fut$message), "Failed.")
              return()
            }

            request_started_at <- Sys.time()
            poll_result <- function() {
              shiny::withReactiveDomain(session, {
                if (future::resolved(fut)) {
                  update_ai_progress("Receiving Missing Value Advisor answer...")
                  answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
                  finish_ai_request(answer, "Ready.")
                } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 360) {
                  finish_ai_request("Missing Value Advisor call timed out after 360 seconds.", "Timed out.")
                } else {
                  update_ai_progress("Missing Value Advisor is reasoning...")
                  later::later(poll_result, 0.5)
                }
              })
            }
            later::later(poll_result, 0.2)
          })
        }, 0.1)
        return()
      }

      if (metminer_global_ai_is_outlier_advisor_request(user_msg, active_agent_command())) {
        active_agent_command(NULL)
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        start_ai_request("Outlier Advisor", "Collecting outlier-detection context...")
        open_panel()
        scroll_chat_to_bottom()
        later::later(function() {
          shiny::withReactiveDomain(session, {
            messages <- tryCatch({
              advisor_context <- metminer_global_ai_outlier_context(global_data, prj_init)
              retrieved_knowledge <- metminer_global_ai_rag_retrieve(
                paste(user_msg, "outlier PCA missing values sample NA frequency Mahalanobis SD MAD sample removal"),
                top_n = 6
              )
              chat_context <- metminer_ai_summarize_chat_context(chat_snapshot, max_summary_chars = 900)
              metminer_global_ai_outlier_advisor_messages(
                user_question = user_msg,
                advisor_context = advisor_context,
                retrieved_knowledge = retrieved_knowledge,
                chat_context = chat_context,
                language = language
              )
            }, error = function(e) e)

            if (inherits(messages, "error")) {
              finish_ai_request(paste("Outlier Advisor context preparation failed:", messages$message), "Failed.")
              return()
            }

            update_ai_progress("Sending Outlier Advisor request to the selected LLM provider...")
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
              finish_ai_request(paste("Outlier Advisor request failed:", fut$message), "Failed.")
              return()
            }

            request_started_at <- Sys.time()
            poll_result <- function() {
              shiny::withReactiveDomain(session, {
                if (future::resolved(fut)) {
                  update_ai_progress("Receiving Outlier Advisor answer...")
                  answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
                  finish_ai_request(answer, "Ready.")
                } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 360) {
                  finish_ai_request("Outlier Advisor call timed out after 360 seconds.", "Timed out.")
                } else {
                  update_ai_progress("Outlier Advisor is reasoning...")
                  later::later(poll_result, 0.5)
                }
              })
            }
            later::later(poll_result, 0.2)
          })
        }, 0.1)
        return()
      }

      module_advisor <- metminer_global_ai_detect_module_advisor_request(user_msg, active_agent_command())
      if (!is.null(module_advisor)) {
        active_agent_command(NULL)
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        start_ai_request(module_advisor$title, paste0("Collecting ", module_advisor$label, " context..."))
        open_panel()
        scroll_chat_to_bottom()
        later::later(function() {
          shiny::withReactiveDomain(session, {
            messages <- tryCatch({
              advisor_context <- metminer_global_ai_module_advisor_context(
                global_data = global_data,
                prj_init = prj_init,
                advisor_key = module_advisor$key
              )
              retrieved_knowledge <- metminer_global_ai_rag_retrieve(
                paste(user_msg, module_advisor$query_terms),
                top_n = 6
              )
              chat_context <- metminer_ai_summarize_chat_context(chat_snapshot, max_summary_chars = 900)
              metminer_global_ai_module_advisor_messages(
                user_question = user_msg,
                advisor_context = advisor_context,
                retrieved_knowledge = retrieved_knowledge,
                chat_context = chat_context,
                language = language
              )
            }, error = function(e) e)

            if (inherits(messages, "error")) {
              finish_ai_request(paste(module_advisor$title, "context preparation failed:", messages$message), "Failed.")
              return()
            }

            update_ai_progress(paste("Sending", module_advisor$title, "request to the selected LLM provider..."))
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
              finish_ai_request(paste(module_advisor$title, "request failed:", fut$message), "Failed.")
              return()
            }

            request_started_at <- Sys.time()
            poll_result <- function() {
              shiny::withReactiveDomain(session, {
                if (future::resolved(fut)) {
                  update_ai_progress(paste("Receiving", module_advisor$title, "answer..."))
                  answer <- tryCatch(future::value(fut), error = function(e) paste("LLM call failed:", e$message))
                  finish_ai_request(answer, "Ready.")
                } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 360) {
                  finish_ai_request(paste(module_advisor$title, "call timed out after 360 seconds."), "Timed out.")
                } else {
                  update_ai_progress(paste(module_advisor$title, "is reasoning..."))
                  later::later(poll_result, 0.5)
                }
              })
            }
            later::later(poll_result, 0.2)
          })
        }, 0.1)
        return()
      }

      kegg_review_action <- metminer_global_ai_detect_kegg_review_request(user_msg, global_data, rejected_keys = rejected_actions())
      if (!is.null(kegg_review_action)) {
        append_chat(chat, "user", user_msg)
        updateTextAreaInput(session, "user_message", value = "")
        pending_action(kegg_review_action)
        recent_action(kegg_review_action)
        append_chat(
          chat,
          "assistant",
          paste0(
            "检测到已经构建的 **", kegg_review_action$organism_name, " (", kegg_review_action$organism_code, ") KEGG 数据库**，",
            "其中有 **", kegg_review_action$review_n, "** 个 pathway 需要 review。\n\n",
            "是否需要我调用当前生成的 review prompt 进行 AI review，并在输出目录生成 JSON/TSV 审查文件？"
          )
        )
        status_text("Waiting for confirmation.")
        open_panel()
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
            paper_context <- NULL
            if (metminer_global_ai_should_use_paper_search(user_msg)) {
              update_ai_progress("Searching literature with paper-search MCP...")
              paper_context <- metminer_global_ai_general_paper_search_context(user_msg)
              if (identical(paper_context$status %||% "", "ok")) {
                update_ai_progress("Literature evidence retrieved. Preparing answer...")
              } else {
                update_ai_progress("Paper search was requested but no usable literature evidence was retrieved.")
              }
            }
            metminer_global_ai_messages(
              user_msg,
              project_context,
              app_knowledge,
              retrieved_knowledge,
              chat_context,
              language,
              paper_context
            )
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
                                        language = "zh-CN",
                                        paper_context = NULL) {
  language_label <- metminer_ai_language_label(language)
  system_prompt <- paste(
    "You are MetMiner Bot, a global assistant embedded in a Shiny LC-MS plant metabolomics application.",
    paste0("Answer in ", language_label, ". Keep feature IDs, file names, database IDs, adducts, m/z and RT values unchanged."),
    "Use app_knowledge for general questions about MetMiner2 features, workflow, modules, and design intent.",
    "Use retrieved_knowledge as the highest-priority source for questions about TidyMass, TidyMass2, MetMiner, MetMiner2, supported inputs, package relationships, and literature-backed software background.",
    "For questions about Shiny parameters, thresholds, or tuning, answer from retrieved_knowledge parameter chunks first and explain the practical effect of increasing or decreasing the parameter.",
    "Do not claim that MetMiner2 inherits or implements TidyMass2 metabolite-origin inference, MetOriginDB, or metabolic feature-based functional module analysis unless retrieved_knowledge explicitly says MetMiner2 implements that feature. Treat TidyMass2 as related literature/ecosystem background only.",
    "Use project_context and chat_context for claims about the current user's data, current project state, tables, results, and next actions.",
    "If paper_search_context is provided, use it only as traceable literature support. Do not invent papers, authors, journals, years, or DOI values. If it failed or has no papers, say no usable literature evidence was retrieved.",
    "When citing paper_search_context, include a short References section and cite only papers present in that context.",
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
    list(role = "user", content = paste0("paper_search_context JSON:\n", metminer_ai_safe_json(paper_context %||% list(status = "not_requested")))),
    list(role = "assistant", content = "Paper-search context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_is_lcms_condition_request <- function(text) {
  grepl("@lc-condition|@lc_condition|@lcms", text %||% "", ignore.case = TRUE, perl = TRUE)
}

metminer_global_ai_has_agent_token <- function(text) {
  grepl("@[[:alnum:]_-]+", text %||% "", ignore.case = TRUE, perl = TRUE)
}

metminer_global_ai_should_use_paper_search <- function(text) {
  text <- tolower(text %||% "")
  grepl(
    paste(
      "@paper-search-mcp", "@paper", "@mcp", "@literature",
      "查找文献", "检索文献", "文献支撑", "文献支持", "论文", "pubmed",
      "google scholar", "scholar", "literature", "paper", "papers",
      "citation", "doi", "evidence from literature",
      sep = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
}

metminer_global_ai_is_data_import_advisor_request <- function(text, active_command = NULL) {
  if (identical(active_command, "data-import-advisor")) return(TRUE)
  text <- tolower(text %||% "")
  grepl(
    paste(
      "@data-import-advisor", "@data_import_advisor",
      "data import", "导入", "数据导入", "raw ms", "ms1 zip", "mzxml",
      "peak picking", "峰提取", "峰识别", "massprocesser", "xcms",
      "ppm", "snthresh", "noise", "peakwidth", "prefilter", "mzdiff",
      "binsize", "min_fraction", "fill_peaks", "sample metadata", "样本信息",
      sep = "|"
    ),
    text,
    perl = TRUE
  )
}

metminer_global_ai_is_outlier_advisor_request <- function(text, active_command = NULL) {
  if (identical(active_command, "outlier-advisor")) return(TRUE)
  text <- tolower(text %||% "")
  grepl(
    paste(
      "@outlier-advisor", "@outlier_advisor",
      "outlier", "离群", "异常样本", "删除样本", "移除样本",
      "pca", "mahalanobis", "distance", "na freq", "na frequency",
      "missing value", "缺失", "sd fold", "mad fold", "th_na", "th_dist",
      sep = "|"
    ),
    text,
    perl = TRUE
  )
}

metminer_global_ai_is_missing_value_advisor_request <- function(text, active_command = NULL) {
  if (identical(active_command, "missing-value-advisor")) return(TRUE)
  text <- tolower(text %||% "")
  grepl(
    paste(
      "@missing-value-advisor", "@missing_value_advisor", "@mv-advisor", "@imputation-advisor",
      "missing value advisor", "mv advisor", "imputation advisor",
      "impute", "imputation", "missforest", "bpca", "ppca", "svdimpute",
      "knn", "rowmax", "colmax", "maxp", "npcs",
      "缺失值填补", "缺失填补", "填补方法", "补值", "插补",
      sep = "|"
    ),
    text,
    perl = TRUE
  )
}

metminer_global_ai_module_advisor_specs <- function() {
  list(
    "noise-filter-advisor" = list(
      key = "noise_filter",
      command = "noise-filter-advisor",
      title = "Noise Filter Advisor",
      label = "noise-filter",
      token = "@noise-filter-advisor",
      query_terms = "blank filter QC missing ratio RSD noise filtering intensity blank ratio sample group",
      state_key = "noise_filter_advisor_state",
      object_keys = c("object_pos_raw", "object_neg_raw", "object_pos_clean", "object_neg_clean"),
      result_keys = character(),
      scope = c("blank-informed masking", "blank/sample ratio filtering", "QC and group missing-value thresholds", "QC RSD filtering")
    ),
    "normalization-advisor" = list(
      key = "normalization",
      command = "normalization-advisor",
      title = "Normalization Advisor",
      label = "normalization",
      token = "@normalization-advisor",
      query_terms = "normalization PQN SVR keep_scale batch integration QC RSD PCA",
      state_key = "normalization_advisor_state",
      object_keys = c("object_pos_impute", "object_neg_impute", "object_pos_norm", "object_neg_norm"),
      result_keys = character(),
      scope = c("normalization method choice", "batch integration", "PCA/RSD checks before and after normalization")
    ),
    "feature-network-advisor" = list(
      key = "feature_network",
      command = "feature-network-advisor",
      title = "Feature Network Advisor",
      label = "feature-network",
      token = "@feature-network-advisor",
      query_terms = "feature network isotope adduct source fragment neutral loss recurrent ion MS2 audit pseudo area",
      state_key = "feature_network_advisor_state",
      object_keys = c("object_pos_norm", "object_neg_norm", "object_pos_network", "object_neg_network"),
      result_keys = c("pseudo_area_pos", "pseudo_area_neg", "merged_feature_network"),
      scope = c("relationship detection parameters", "MS2 audit", "recurrent ion interpretation", "pseudo-area network collapse")
    ),
    "annotation-advisor" = list(
      key = "annotation",
      command = "annotation-advisor",
      title = "Annotation Advisor",
      label = "annotation",
      token = "@annotation-advisor",
      query_terms = "metabolite annotation MS1 ppm MS2 ppm RT tolerance candidate number KEGG PlantCyc database",
      state_key = "annotation_advisor_state",
      object_keys = c("object_pos_network", "object_neg_network", "object_pos_annotated", "object_neg_annotated"),
      result_keys = c("kegg_database_result", "plantcyc_database_result"),
      scope = c("annotation database selection", "MS1/MS2/RT tolerances", "candidate count", "annotation status by polarity")
    ),
    "annotation-filter-advisor" = list(
      key = "annotation_filter",
      command = "annotation-filter-advisor",
      title = "Annotation Filter Advisor",
      label = "annotation-filter",
      token = "@annotation-filter-advisor",
      query_terms = "annotation filtering redundancy recurrent ion adduct priority strict core adduct cross polarity collapse expand review",
      state_key = "annotation_filter_advisor_state",
      object_keys = c("object_pos_annotated", "object_neg_annotated"),
      result_keys = c("annotation_filter_result"),
      scope = c("redundant annotation filtering", "feature-network validation", "recurrent/background ion removal", "expand/collapse review")
    ),
    "differential-advisor" = list(
      key = "differential",
      command = "differential-advisor",
      title = "Differential Advisor",
      label = "differential-analysis",
      token = "@differential-advisor",
      query_terms = "differential abundance volcano plot fold change p value FDR OPLS selected feature annotation only",
      state_key = "differential_advisor_state",
      object_keys = c("object_pos_norm", "object_neg_norm", "differential_object"),
      result_keys = c("differential_result", "dam_result"),
      scope = c("group comparison", "statistical test and FDR", "volcano thresholds", "selected feature evidence")
    ),
    "enrichment-advisor" = list(
      key = "enrichment",
      command = "enrichment-advisor",
      title = "Enrichment Advisor",
      label = "pathway-enrichment",
      token = "@enrichment-advisor",
      query_terms = "KEGG PlantCyc pathway enrichment differential metabolites query IDs ID mapping Fisher hypergeometric bubble barplot",
      state_key = "enrichment_advisor_state",
      object_keys = c("object_pos_annotated", "object_neg_annotated"),
      result_keys = c("enrichment_result", "differential_result", "annotation_filter_result"),
      scope = c("query ID source", "KEGG versus PlantCyc identifiers", "pathway database compatibility", "bubble/bar plot interpretation")
    ),
    "database-advisor" = list(
      key = "database",
      command = "database-advisor",
      title = "Database Advisor",
      label = "database",
      token = "@database-advisor",
      query_terms = "KEGG database PlantCyc database species-specific pathway database ID mapping organism PGDB SmartTable",
      state_key = "database_advisor_state",
      object_keys = character(),
      result_keys = c("kegg_database_result", "plantcyc_database_result", "id_mapping_result"),
      scope = c("species-specific database construction", "KEGG/PlantCyc pathway background", "compound ID mapping", "database suitability for annotation/enrichment")
    )
  )
}

metminer_global_ai_detect_module_advisor_request <- function(text, active_command = NULL) {
  specs <- metminer_global_ai_module_advisor_specs()
  if (!is.null(active_command) && active_command %in% names(specs)) {
    return(specs[[active_command]])
  }
  text_lower <- tolower(text %||% "")
  for (spec in specs) {
    patterns <- c(spec$token, paste0("@", spec$command), paste0("@", spec$key, "-advisor"), paste0(spec$label, " advisor"))
    if (any(vapply(patterns, function(pattern) grepl(pattern, text_lower, fixed = TRUE), logical(1)))) {
      return(spec)
    }
  }
  NULL
}

metminer_global_ai_clean_paper_query <- function(text) {
  query <- gsub("@paper-search-mcp|@paper|@mcp|@literature", "", text %||% "", ignore.case = TRUE, perl = TRUE)
  query <- gsub("请检索并总结与当前问题相关的论文证据：", "", query, fixed = TRUE)
  trimws(query)
}

metminer_global_ai_general_paper_search_context <- function(user_msg,
                                                           max_results_per_source = 2,
                                                           sources = NULL) {
  query <- metminer_global_ai_clean_paper_query(user_msg)
  if (!has_text(query)) {
    query <- user_msg %||% ""
  }
  if (is.null(sources)) {
    sources <- c("pubmed", "pmc", "europepmc", "semantic", "crossref")
    if (grepl("google scholar|scholar", user_msg %||% "", ignore.case = TRUE, perl = TRUE)) {
      sources <- unique(c("google_scholar", sources))
    }
  }
  found <- tryCatch(
    metminer_ai_search_papers(
      query = query,
      sources = sources,
      max_results_per_source = max_results_per_source
    ),
    error = function(e) list(status = "error", message = e$message, papers = list())
  )
  papers <- found$papers %||% list()
  list(
    status = if (length(papers) > 0) "ok" else (found$status %||% "empty"),
    query = query,
    sources = sources,
    total_papers = length(papers),
    message = found$message %||% NA_character_,
    papers = papers
  )
}

metminer_global_ai_extract_json_object <- function(text) {
  text <- trimws(text %||% "")
  text <- sub("^```[a-zA-Z0-9_-]*\\s*", "", text, perl = TRUE)
  text <- sub("\\s*```$", "", text, perl = TRUE)
  start <- regexpr("\\{", text, perl = TRUE)[1]
  end_matches <- gregexpr("\\}", text, perl = TRUE)[[1]]
  if (start < 0 || length(end_matches) == 0 || end_matches[1] < 0) {
    stop("No JSON object found in LLM response.", call. = FALSE)
  }
  substr(text, start, max(end_matches))
}

metminer_global_ai_normalize_lcms_conditions <- function(x, fallback_text = "") {
  x <- x %||% list()
  allowed <- c(
    "method_text", "instrument", "chromatography", "column",
    "mobile_phase_a", "mobile_phase_b", "ion_source", "ion_mode",
    "acquisition", "scan_range", "collision_energy", "notes"
  )
  out <- stats::setNames(vector("list", length(allowed)), allowed)
  for (key in allowed) {
    value <- x[[key]] %||% ""
    if (length(value) > 1) value <- paste(value, collapse = "; ")
    out[[key]] <- trimws(as.character(value %||% ""))
  }
  if (!nzchar(out$method_text)) {
    out$method_text <- fallback_text %||% ""
  }
  if (!nzchar(out$ion_source) && grepl("\\besi\\b|electrospray|电喷雾", out$method_text, ignore.case = TRUE, perl = TRUE)) {
    out$ion_source <- "ESI"
  }
  if (!nzchar(out$instrument) || !nzchar(out$chromatography)) {
    fallback <- metminer_extract_lcms_items(out$method_text)
    if (!nzchar(out$instrument)) out$instrument <- fallback$instrument %||% ""
    if (!nzchar(out$chromatography)) out$chromatography <- fallback$chromatography %||% ""
  }
  instrument_lower <- tolower(out$instrument)
  out$instrument <- if (grepl("orbitrap|exploris|q exactive|qe\\b", instrument_lower, perl = TRUE)) {
    "Orbitrap"
  } else if (grepl("qtof|q-tof|tof|xevo|synapt", instrument_lower, perl = TRUE)) {
    "QTOF"
  } else if (nzchar(out$instrument) && !identical(out$instrument, "auto")) {
    "Other"
  } else {
    out$instrument
  }
  chrom_lower <- tolower(out$chromatography)
  out$chromatography <- if (grepl("c18|hypersil gold|reverse|reversed|rp\\b", chrom_lower, perl = TRUE)) {
    "C18 reversed phase"
  } else if (grepl("hilic|amide|zic", chrom_lower, perl = TRUE)) {
    "HILIC"
  } else if (nzchar(out$chromatography) && !identical(out$chromatography, "auto")) {
    "Other"
  } else {
    out$chromatography
  }
  out
}

metminer_global_ai_lcms_condition_messages <- function(user_msg, language = "zh-CN") {
  system_prompt <- paste(
    "You extract LC-MS method conditions for a Shiny metabolomics project.",
    "Return exactly one JSON object and no prose.",
    "Use empty strings for unknown fields. Preserve units and vendor/model names.",
    "Fields: method_text, instrument, chromatography, column, mobile_phase_a, mobile_phase_b, ion_source, ion_mode, acquisition, scan_range, collision_energy, notes.",
    "instrument should be a concise class/model such as Orbitrap Exploris 240, QTOF, TripleTOF, or Other when known.",
    "chromatography should be C18 reversed phase, HILIC, RP, or Other when known.",
    "ion_mode should mention positive, negative, or both when present.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("Extract LC-MS conditions from this user message:\n", user_msg))
  )
}

metminer_global_ai_run_lcms_condition_action <- function(user_msg,
                                                        chat,
                                                        start_ai_request,
                                                        update_ai_progress,
                                                        finish_ai_request,
                                                        provider,
                                                        model,
                                                        api_key,
                                                        base_url,
                                                        temperature,
                                                        session,
                                                        prj_init) {
  method_text <- trimws(gsub("@lc-condition|@lc_condition|@lcms", "", user_msg %||% "", ignore.case = TRUE, perl = TRUE))
  method_text <- trimws(gsub("^请从下面的液质方法描述中提取 Project Init 的 LC-MS Conditions 关键字段：", "", method_text, perl = TRUE))
  if (!nzchar(method_text)) {
    finish_ai_request("请在 `@lc-condition` 后面粘贴液质方法描述，例如仪器、色谱柱、流动相、离子模式、扫描范围和碰撞能量。", "Ready.")
    return(invisible(NULL))
  }

  start_ai_request("LC-MS Conditions", "Calling LLM to extract structured method fields...")
  messages <- metminer_global_ai_lcms_condition_messages(user_msg)
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
    finish_ai_request(paste("LC-MS condition extraction failed:", fut$message), "Failed.")
    return(invisible(NULL))
  }

  request_started_at <- Sys.time()
  poll_result <- function() {
    shiny::withReactiveDomain(session, {
      tryCatch({
      if (future::resolved(fut)) {
        update_ai_progress("Parsing LC-MS condition JSON...")
        raw <- tryCatch(future::value(fut), error = function(e) e)
        if (inherits(raw, "error")) {
          finish_ai_request(paste("LLM call failed:", raw$message), "Failed.")
          return()
        }
        parsed <- tryCatch({
          jsonlite::fromJSON(metminer_global_ai_extract_json_object(raw), simplifyVector = FALSE)
        }, error = function(e) e)
        if (inherits(parsed, "error")) {
          finish_ai_request(paste("Could not parse LC-MS condition JSON:", parsed$message, "\n\nRaw response:\n", raw), "Failed.")
          return()
        }
        items <- metminer_global_ai_normalize_lcms_conditions(parsed, fallback_text = method_text)
        formatted <- metminer_format_lcms_conditions(items)
        prj_init$lcms_conditions <- items
        prj_init$lcms_conditions_text <- formatted
        wd <- shiny::isolate(prj_init$wd)
        if (!is.null(wd) && dir.exists(wd)) {
          saveRDS(items, file.path(wd, "project_lcms_conditions.rds"))
          project_info_file <- file.path(wd, "project_info.rds")
          if (file.exists(project_info_file)) {
            info <- tryCatch(readRDS(project_info_file), error = function(e) list())
            info$lcms_conditions <- items
            saveRDS(info, project_info_file)
          }
        }
        finish_ai_request(
          paste0(
            "已生成并写入 Project Init 的 LC-MS conditions：\n\n```text\n",
            formatted,
            "\n```\n\n你可以在 Project Init 的 LC-MS Conditions 面板继续手动微调。"
          )
        )
      } else if (as.numeric(difftime(Sys.time(), request_started_at, units = "secs")) > 180) {
        finish_ai_request("LC-MS condition extraction timed out after 180 seconds.", "Timed out.")
      } else {
        update_ai_progress("LLM is extracting method fields...")
        later::later(poll_result, 0.5)
      }
      }, error = function(e) {
        finish_ai_request(paste("LC-MS condition extraction crashed:", e$message), "Failed.")
      })
    })
  }
  later::later(poll_result, 0.2)
  invisible(NULL)
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
        sample_info_columns = colnames(sample_info),
        lcms_conditions = prj_init$lcms_conditions %||% list(),
        lcms_conditions_text = prj_init$lcms_conditions_text %||% NA_character_
      ),
      global_data_keys = names(g),
      objects = objects,
      annotation_filter = metminer_global_ai_filter_summary(g$annotation_filter_result),
      kegg_database = metminer_global_ai_kegg_summary(g$kegg_database_result, g$kegg_ai_review, g$kegg_ai_review_curation),
      differential_analysis = metminer_global_ai_result_summary(g$dam_result %||% g$differential_result),
      enrichment = metminer_global_ai_result_summary(g$enrichment_result),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_module_advisor_context <- function(global_data, prj_init, advisor_key) {
  specs <- metminer_global_ai_module_advisor_specs()
  spec <- NULL
  for (candidate in specs) {
    if (identical(candidate$key, advisor_key) || identical(candidate$command, advisor_key)) {
      spec <- candidate
      break
    }
  }
  if (is.null(spec)) stop("Unknown module advisor: ", advisor_key, call. = FALSE)

  shiny::isolate({
    g <- shiny::reactiveValuesToList(global_data, all.names = TRUE)
    sample_info <- as.data.frame(prj_init$sample_info %||% data.frame(), stringsAsFactors = FALSE)
    object_summaries <- stats::setNames(
      lapply(spec$object_keys, function(key) metminer_global_ai_object_detail_summary(key, g[[key]])),
      spec$object_keys
    )
    result_summaries <- stats::setNames(
      lapply(spec$result_keys, function(key) metminer_global_ai_module_result_summary(key, g[[key]])),
      spec$result_keys
    )
    list(
      advisor = list(
        name = spec$key,
        title = spec$title,
        scope = spec$scope,
        limits = c(
          "It gives recommendations only; it should not mutate project data.",
          "It should separate evidence present in context from suggested next checks.",
          "It should be conservative when a plot/result/object is unavailable."
        )
      ),
      project = list(
        job_id = prj_init$job_id %||% NA_character_,
        working_dir = prj_init$wd %||% NA_character_,
        mass_dataset_dir = prj_init$mass_dataset_dir %||% NA_character_,
        sample_info_rows = nrow(sample_info),
        sample_info_columns = colnames(sample_info),
        sample_group_counts = metminer_global_ai_sample_group_counts(sample_info),
        lcms_conditions = prj_init$lcms_conditions %||% list(),
        lcms_conditions_text = prj_init$lcms_conditions_text %||% NA_character_
      ),
      module_ui_state = g[[spec$state_key]] %||% list(available = FALSE, note = paste("No", spec$state_key, "has been captured yet.")),
      relevant_objects = object_summaries,
      relevant_results = result_summaries,
      global_data_keys = names(g),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_module_result_summary <- function(key, result) {
  if (is.null(result)) return(list(available = FALSE, key = key))
  if (identical(key, "annotation_filter_result")) {
    return(c(list(key = key), metminer_global_ai_filter_summary(result)))
  }
  if (key %in% c("differential_result", "dam_result")) {
    return(c(list(key = key), metminer_global_ai_differential_summary(result)))
  }
  if (identical(key, "enrichment_result")) {
    return(c(list(key = key), metminer_global_ai_enrichment_summary(result)))
  }
  if (identical(key, "kegg_database_result")) {
    return(c(list(key = key), metminer_global_ai_kegg_summary(result)))
  }
  if (identical(key, "plantcyc_database_result")) {
    return(c(list(key = key), metminer_global_ai_plantcyc_summary(result)))
  }
  if (is.data.frame(result)) {
    return(list(available = TRUE, key = key, rows = nrow(result), columns = colnames(result), preview = utils::head(result, 8)))
  }
  if (is.list(result)) {
    return(list(
      available = TRUE,
      key = key,
      class = paste(class(result), collapse = "/"),
      names = names(result),
      table_sizes = lapply(result, function(x) if (is.data.frame(x)) nrow(x) else NA_integer_)
    ))
  }
  list(available = TRUE, key = key, class = paste(class(result), collapse = "/"))
}

metminer_global_ai_module_advisor_messages <- function(user_question,
                                                       advisor_context,
                                                       retrieved_knowledge = data.frame(),
                                                       chat_context = "",
                                                       language = "zh-CN") {
  language_label <- metminer_ai_language_label(language)
  advisor_name <- advisor_context$advisor$title %||% "Module Advisor"
  system_prompt <- paste(
    paste0("You are ", advisor_name, ", a module-aware assistant inside MetMiner Bot."),
    paste0("Answer in ", language_label, ". Keep sample IDs, group labels, method names, thresholds, file paths, m/z and RT values unchanged."),
    "Use advisor_context as the source of truth for current project state, UI parameters, available objects, and results.",
    "Use retrieved_knowledge only for MetMiner behavior and parameter interpretation.",
    "Do not invent plots, uploaded files, sample metadata, database contents, or results not present in advisor_context.",
    "Give practical, conservative parameter advice and name the exact evidence that supports it.",
    "If the current context is insufficient, say exactly which MetMiner panel/result/plot should be checked next.",
    "When giving advice, prefer this structure: conclusion, evidence in current context, parameter interpretation, next action.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("retrieved_knowledge JSON:\n", metminer_ai_safe_json(retrieved_knowledge))),
    list(role = "assistant", content = "Retrieved module knowledge loaded."),
    list(role = "user", content = paste0("advisor_context JSON:\n", metminer_ai_safe_json(advisor_context))),
    list(role = "assistant", content = "Module advisor context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_data_import_context <- function(global_data, prj_init) {
  shiny::isolate({
    g <- shiny::reactiveValuesToList(global_data, all.names = TRUE)
    sample_info <- as.data.frame(prj_init$sample_info %||% data.frame(), stringsAsFactors = FALSE)
    state <- g$data_import_advisor_state %||% list(available = FALSE)
    raw_dir <- state$raw_ms1_dir %||% file.path(prj_init$wd %||% "", "MS1")
    raw_structure <- metminer_global_ai_raw_ms_structure(raw_dir)
    list(
      advisor = list(
        name = "data_import_advisor",
        scope = c(
          "raw MS1 ZIP structure",
          "peak-picking parameter sanity checks",
          "table CSV column mapping",
          "existing mass_dataset loading",
          "sample metadata consistency"
        ),
        limits = c(
          "It cannot inspect chromatographic peak shapes unless plots or objects are available in context.",
          "It should suggest parameter changes conservatively and ask the user to rerun/import before claiming final quality."
        )
      ),
      project = list(
        job_id = prj_init$job_id %||% NA_character_,
        working_dir = prj_init$wd %||% NA_character_,
        mass_dataset_dir = prj_init$mass_dataset_dir %||% NA_character_,
        sample_info_rows = nrow(sample_info),
        sample_info_columns = colnames(sample_info),
        sample_info_preview = utils::head(sample_info, 8),
        lcms_conditions = prj_init$lcms_conditions %||% list(),
        lcms_conditions_text = prj_init$lcms_conditions_text %||% NA_character_
      ),
      data_import_ui_state = state,
      raw_ms_structure = raw_structure,
      imported_objects = list(
        positive_raw = metminer_global_ai_object_detail_summary("object_pos_raw", g$object_pos_raw),
        negative_raw = metminer_global_ai_object_detail_summary("object_neg_raw", g$object_neg_raw)
      ),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_data_import_advisor_messages <- function(user_question,
                                                            advisor_context,
                                                            retrieved_knowledge = data.frame(),
                                                            chat_context = "",
                                                            language = "zh-CN") {
  language_label <- metminer_ai_language_label(language)
  system_prompt <- paste(
    "You are Data Import Advisor, a module-aware assistant inside MetMiner Bot.",
    paste0("Answer in ", language_label, ". Keep sample IDs, file paths, parameters, m/z and RT values unchanged."),
    "Use advisor_context as the source of truth for the current project and data-import state.",
    "Use retrieved_knowledge for MetMiner data-import behavior and parameter meanings.",
    "Do not invent uploaded files, sample groups, object dimensions, or optimization results.",
    "For parameter advice, explain the practical effect of increasing or decreasing each parameter.",
    "Separate current evidence from recommendations. If evidence is missing, say exactly what should be checked in the Data Import UI.",
    "When giving advice, prefer this structure: conclusion, evidence from current context, parameter/file checks, next action.",
    "Be conservative: recommend rerunning optimization or checking preview tables rather than asserting data quality from absent evidence.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("retrieved_knowledge JSON:\n", metminer_ai_safe_json(retrieved_knowledge))),
    list(role = "assistant", content = "Retrieved data-import knowledge loaded."),
    list(role = "user", content = paste0("advisor_context JSON:\n", metminer_ai_safe_json(advisor_context))),
    list(role = "assistant", content = "Data-import advisor context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_outlier_context <- function(global_data, prj_init) {
  shiny::isolate({
    g <- shiny::reactiveValuesToList(global_data, all.names = TRUE)
    state <- g$outlier_advisor_state %||% list(
      available = FALSE,
      outlier_method = "By tidymass",
      auto_criteria = c("na", "distance"),
      thresholds = list(th_na = 0.5, th_sd = 6, th_mad = 6, th_dist = 0.05)
    )
    thresholds <- state$thresholds %||% list(th_na = 0.5, th_sd = 6, th_mad = 6, th_dist = 0.05)
    input_pos <- g$object_pos_clean %||% prj_init$object_positive.init
    input_neg <- g$object_neg_clean %||% prj_init$object_negative.init
    list(
      advisor = list(
        name = "outlier_advisor",
        scope = c(
          "sample-level missing-value frequency",
          "PCA distance and separation",
          "total-intensity SD/MAD fold heuristics",
          "automatic versus manual outlier-removal settings",
          "cross-polarity consistency before removing samples"
        ),
        limits = c(
          "It should not remove samples automatically.",
          "It should treat single-mode evidence as weaker than POS+NEG concordant evidence.",
          "It should warn when biological group separation may be mistaken for technical outliers."
        )
      ),
      project = list(
        job_id = prj_init$job_id %||% NA_character_,
        working_dir = prj_init$wd %||% NA_character_,
        mass_dataset_dir = prj_init$mass_dataset_dir %||% NA_character_
      ),
      outlier_ui_state = state,
      input_objects = list(
        positive = metminer_global_ai_outlier_object_context(
          object = input_pos,
          mode = "positive",
          thresholds = thresholds,
          criteria = state$auto_criteria %||% character(),
          removed_ids = state$removed$positive %||% character()
        ),
        negative = metminer_global_ai_outlier_object_context(
          object = input_neg,
          mode = "negative",
          thresholds = thresholds,
          criteria = state$auto_criteria %||% character(),
          removed_ids = state$removed$negative %||% character()
        )
      ),
      output_objects = list(
        positive = metminer_global_ai_object_detail_summary("object_pos_outlier", g$object_pos_outlier),
        negative = metminer_global_ai_object_detail_summary("object_neg_outlier", g$object_neg_outlier)
      ),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_outlier_advisor_messages <- function(user_question,
                                                        advisor_context,
                                                        retrieved_knowledge = data.frame(),
                                                        chat_context = "",
                                                        language = "zh-CN") {
  language_label <- metminer_ai_language_label(language)
  system_prompt <- paste(
    "You are Outlier Advisor, a module-aware assistant inside MetMiner Bot.",
    paste0("Answer in ", language_label, ". Keep sample IDs, group labels, thresholds, file paths, m/z and RT values unchanged."),
    "Use advisor_context as the source of truth for the current project and outlier-detection state.",
    "Use retrieved_knowledge for general MetMiner or LC-MS QC behavior, but do not override current project evidence.",
    "Do not invent PCA plots, removed samples, or sample metadata not present in advisor_context.",
    "Give conservative sample-removal advice. Removing a sample should require convergent evidence such as high NA frequency, extreme PCA distance, abnormal total intensity, and/or consistency across POS and NEG.",
    "Warn when PCA separation may reflect biology, treatment group, batch, or injection order rather than technical failure.",
    "Explain the current auto-removal rule: in this module, automatic removal requires a sample to satisfy ALL selected criteria.",
    "When giving advice, prefer this structure: conclusion, evidence by polarity, threshold/parameter interpretation, next action.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("retrieved_knowledge JSON:\n", metminer_ai_safe_json(retrieved_knowledge))),
    list(role = "assistant", content = "Retrieved outlier/QC knowledge loaded."),
    list(role = "user", content = paste0("advisor_context JSON:\n", metminer_ai_safe_json(advisor_context))),
    list(role = "assistant", content = "Outlier advisor context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_missing_value_context <- function(global_data, prj_init) {
  shiny::isolate({
    g <- shiny::reactiveValuesToList(global_data, all.names = TRUE)
    state <- g$missing_value_advisor_state %||% list(
      available = FALSE,
      impute_method = "knn",
      parameters = list(
        knn = list(k = 10, rowmax = 0.5, colmax = 0.8, maxp = 1500, rng_seed = 362436069),
        rf = list(maxiter = 10, ntree = 100, decreasing = "FALSE"),
        pca = list(nPcs = 2, bpca_maxSteps = 100, bpca_threshold = 1e-04)
      )
    )
    input_pos <- g$object_pos_outlier %||% g$object_pos_clean %||% prj_init$object_positive.init
    input_neg <- g$object_neg_outlier %||% g$object_neg_clean %||% prj_init$object_negative.init
    list(
      advisor = list(
        name = "missing_value_advisor",
        scope = c(
          "missing-value rate by sample and feature",
          "imputation method and parameter sanity checks",
          "comparison of pre-imputation and post-imputation missingness",
          "deciding whether to impute now or return to noise filtering/sample QC",
          "POS/NEG consistency for missing-value behavior"
        ),
        limits = c(
          "It should not run imputation automatically.",
          "It should not recommend aggressive imputation for features or samples dominated by missing values.",
          "It should separate technical missingness from biologically meaningful absence when evidence is insufficient."
        )
      ),
      project = list(
        job_id = prj_init$job_id %||% NA_character_,
        working_dir = prj_init$wd %||% NA_character_,
        mass_dataset_dir = prj_init$mass_dataset_dir %||% NA_character_
      ),
      imputation_ui_state = state,
      objects = list(
        positive_before_imputation = metminer_global_ai_missing_value_object_context(input_pos, "positive", "before_imputation"),
        negative_before_imputation = metminer_global_ai_missing_value_object_context(input_neg, "negative", "before_imputation"),
        positive_after_imputation = metminer_global_ai_missing_value_object_context(g$object_pos_impute, "positive", "after_imputation"),
        negative_after_imputation = metminer_global_ai_missing_value_object_context(g$object_neg_impute, "negative", "after_imputation")
      ),
      generated_at = as.character(Sys.time())
    )
  })
}

metminer_global_ai_missing_value_advisor_messages <- function(user_question,
                                                              advisor_context,
                                                              retrieved_knowledge = data.frame(),
                                                              chat_context = "",
                                                              language = "zh-CN") {
  language_label <- metminer_ai_language_label(language)
  system_prompt <- paste(
    "You are Missing Value Advisor, a module-aware assistant inside MetMiner Bot.",
    paste0("Answer in ", language_label, ". Keep sample IDs, group labels, method names, thresholds, file paths, m/z and RT values unchanged."),
    "Use advisor_context as the source of truth for the current project and missing-value imputation state.",
    "Use retrieved_knowledge for MetMiner imputation behavior and parameter meanings, but do not override current project evidence.",
    "Do not invent missing-value plots, sample metadata, or imputation results not present in advisor_context.",
    "Give conservative imputation advice. If missingness is very high in specific samples or variables, recommend checking noise filtering/outlier handling before imputation.",
    "Explain practical parameter effects: KNN k, rowmax, colmax, maxp, RF maxiter/ntree, PCA nPcs, and simple mean/median/zero/minimum methods when relevant.",
    "Warn that zero/minimum imputation can distort downstream fold changes if used broadly without a left-censoring rationale.",
    "When giving advice, prefer this structure: conclusion, evidence by polarity, method/parameter interpretation, next action.",
    sep = "\n"
  )
  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0("retrieved_knowledge JSON:\n", metminer_ai_safe_json(retrieved_knowledge))),
    list(role = "assistant", content = "Retrieved missing-value/imputation knowledge loaded."),
    list(role = "user", content = paste0("advisor_context JSON:\n", metminer_ai_safe_json(advisor_context))),
    list(role = "assistant", content = "Missing-value advisor context loaded."),
    list(role = "user", content = paste0("chat_context:\n", coerce_text(chat_context, "none"))),
    list(role = "user", content = paste0("User question:\n", user_question))
  )
}

metminer_global_ai_missing_value_object_context <- function(object,
                                                            mode = "positive",
                                                            stage = "before_imputation") {
  if (is.null(object)) return(list(available = FALSE, mode = mode, stage = stage))
  sample_info <- tryCatch(as.data.frame(massdataset::extract_sample_info(object), stringsAsFactors = FALSE), error = function(e) data.frame())
  variable_info <- tryCatch(as.data.frame(massdataset::extract_variable_info(object), stringsAsFactors = FALSE), error = function(e) data.frame())
  expr <- tryCatch(as.matrix(massdataset::extract_expression_data(object)), error = function(e) NULL)
  if (is.null(expr) || length(expr) == 0) {
    return(list(
      available = TRUE,
      mode = mode,
      stage = stage,
      samples = nrow(sample_info),
      variables = nrow(variable_info),
      expression_available = FALSE
    ))
  }

  sample_metrics <- metminer_global_ai_missing_value_sample_metrics(expr, sample_info)
  variable_metrics <- metminer_global_ai_missing_value_variable_metrics(expr, variable_info)
  list(
    available = TRUE,
    mode = mode,
    stage = stage,
    samples = ncol(expr),
    variables = nrow(expr),
    sample_columns = colnames(sample_info),
    variable_columns = colnames(variable_info),
    overall_na_rate = signif(mean(is.na(expr)), 4),
    overall_zero_rate = signif(mean(expr == 0, na.rm = TRUE), 4),
    samples_with_na_over_20pct = sum(sample_metrics$na_fraction > 0.2, na.rm = TRUE),
    samples_with_na_over_50pct = sum(sample_metrics$na_fraction > 0.5, na.rm = TRUE),
    variables_with_na_over_20pct = sum(variable_metrics$na_fraction > 0.2, na.rm = TRUE),
    variables_with_na_over_50pct = sum(variable_metrics$na_fraction > 0.5, na.rm = TRUE),
    sample_missing_top = utils::head(sample_metrics[order(-sample_metrics$na_fraction, na.last = TRUE), , drop = FALSE], 20),
    variable_missing_top = utils::head(variable_metrics[order(-variable_metrics$na_fraction, na.last = TRUE), , drop = FALSE], 20)
  )
}

metminer_global_ai_missing_value_sample_metrics <- function(expr, sample_info = data.frame()) {
  sample_ids <- colnames(expr) %||% character()
  sample_info <- as.data.frame(sample_info %||% data.frame(), stringsAsFactors = FALSE)
  sample_index <- if ("sample_id" %in% colnames(sample_info)) match(sample_ids, sample_info$sample_id) else rep(NA_integer_, length(sample_ids))
  data.frame(
    sample_id = sample_ids,
    class = if ("class" %in% colnames(sample_info)) as.character(sample_info$class[sample_index]) else NA_character_,
    group = if ("group" %in% colnames(sample_info)) as.character(sample_info$group[sample_index]) else NA_character_,
    batch = if ("batch" %in% colnames(sample_info)) as.character(sample_info$batch[sample_index]) else NA_character_,
    injection_order = if ("injection.order" %in% colnames(sample_info)) as.character(sample_info$injection.order[sample_index]) else NA_character_,
    na_fraction = signif(colMeans(is.na(expr)), 4),
    zero_fraction = signif(colMeans(expr == 0, na.rm = TRUE), 4),
    total_intensity = signif(colSums(expr, na.rm = TRUE), 5),
    stringsAsFactors = FALSE
  )
}

metminer_global_ai_missing_value_variable_metrics <- function(expr, variable_info = data.frame()) {
  variable_info <- as.data.frame(variable_info %||% data.frame(), stringsAsFactors = FALSE)
  variable_ids <- rownames(expr) %||% seq_len(nrow(expr))
  if ("variable_id" %in% colnames(variable_info)) {
    variable_ids_from_info <- as.character(variable_info$variable_id[seq_len(min(nrow(variable_info), nrow(expr)))])
    variable_ids[seq_along(variable_ids_from_info)] <- variable_ids_from_info
  }
  data.frame(
    variable_id = as.character(variable_ids),
    mz = if ("mz" %in% colnames(variable_info)) suppressWarnings(as.numeric(variable_info$mz[seq_len(nrow(expr))])) else NA_real_,
    rt = if ("rt" %in% colnames(variable_info)) suppressWarnings(as.numeric(variable_info$rt[seq_len(nrow(expr))])) else NA_real_,
    na_fraction = signif(rowMeans(is.na(expr)), 4),
    zero_fraction = signif(rowMeans(expr == 0, na.rm = TRUE), 4),
    mean_intensity = signif(rowMeans(expr, na.rm = TRUE), 5),
    stringsAsFactors = FALSE
  )
}

metminer_global_ai_outlier_object_context <- function(object,
                                                      mode = "positive",
                                                      thresholds = list(),
                                                      criteria = character(),
                                                      removed_ids = character()) {
  if (is.null(object)) return(list(available = FALSE, mode = mode))
  sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
  expression_data <- tryCatch(massdataset::extract_expression_data(object), error = function(e) NULL)
  outlier_table <- tryCatch(
    calc_outlier_table(
      object,
      th_na = as.numeric(thresholds$th_na %||% 0.5),
      th_sd = as.numeric(thresholds$th_sd %||% 6),
      th_mad = as.numeric(thresholds$th_mad %||% 6),
      th_dist = as.numeric(thresholds$th_dist %||% 0.05)
    ),
    error = function(e) data.frame()
  )
  sample_metrics <- metminer_global_ai_sample_qc_metrics(object, outlier_table, criteria)
  pca_flag_rank <- if (nrow(sample_metrics) > 0) as.integer(sample_metrics$pca_distance_flag %in% TRUE) else integer()
  list(
    available = TRUE,
    mode = mode,
    samples = nrow(sample_info),
    variables = if (!is.null(expression_data)) nrow(expression_data) else NA_integer_,
    sample_columns = colnames(sample_info),
    group_columns = intersect(c("class", "group", "batch", "injection.order", "sample_group"), colnames(sample_info)),
    group_counts = metminer_global_ai_sample_group_counts(sample_info),
    thresholds = thresholds,
    selected_criteria = criteria,
    current_removed_ids = as.character(removed_ids %||% character()),
    outlier_counts = if (nrow(outlier_table) > 0) as.list(colSums(outlier_table, na.rm = TRUE)) else list(),
    automatic_rule_candidate_ids = metminer_global_ai_auto_outlier_ids(outlier_table, criteria),
    sample_qc_top = utils::head(sample_metrics[order(-sample_metrics$flag_count, -sample_metrics$na_fraction, -pca_flag_rank, na.last = TRUE), , drop = FALSE], 20),
    pca_scores_top = utils::head(metminer_global_ai_pca_scores(object), 20)
  )
}

metminer_global_ai_sample_qc_metrics <- function(object, outlier_table = data.frame(), criteria = character()) {
  sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
  expr <- tryCatch(as.matrix(massdataset::extract_expression_data(object)), error = function(e) NULL)
  sample_ids <- as.character(sample_info$sample_id %||% colnames(expr) %||% character())
  if (is.null(expr) || length(sample_ids) == 0) {
    return(data.frame())
  }
  sample_ids <- intersect(sample_ids, colnames(expr))
  expr <- expr[, sample_ids, drop = FALSE]
  outlier_table <- as.data.frame(outlier_table %||% data.frame())
  flags <- if (nrow(outlier_table) > 0) outlier_table[sample_ids, , drop = FALSE] else data.frame(row.names = sample_ids)
  data.frame(
    sample_id = sample_ids,
    class = if ("class" %in% colnames(sample_info)) as.character(sample_info$class[match(sample_ids, sample_info$sample_id)]) else NA_character_,
    group = if ("group" %in% colnames(sample_info)) as.character(sample_info$group[match(sample_ids, sample_info$sample_id)]) else NA_character_,
    batch = if ("batch" %in% colnames(sample_info)) as.character(sample_info$batch[match(sample_ids, sample_info$sample_id)]) else NA_character_,
    injection_order = if ("injection.order" %in% colnames(sample_info)) as.character(sample_info$injection.order[match(sample_ids, sample_info$sample_id)]) else NA_character_,
    na_fraction = signif(colMeans(is.na(expr)), 4),
    total_intensity = signif(colSums(expr, na.rm = TRUE), 5),
    flag_count = if (ncol(flags) > 0) rowSums(flags, na.rm = TRUE) else 0,
    na_flag = if ("according_to_na" %in% colnames(flags)) flags$according_to_na else NA,
    sd_flag = if ("pc_sd" %in% colnames(flags)) flags$pc_sd else NA,
    mad_flag = if ("pc_mad" %in% colnames(flags)) flags$pc_mad else NA,
    pca_distance_flag = if ("according_to_distance" %in% colnames(flags)) flags$according_to_distance else NA,
    auto_remove_candidate = sample_ids %in% metminer_global_ai_auto_outlier_ids(outlier_table, criteria),
    stringsAsFactors = FALSE
  )
}

metminer_global_ai_auto_outlier_ids <- function(outlier_table, criteria = character()) {
  outlier_table <- as.data.frame(outlier_table %||% data.frame())
  if (nrow(outlier_table) == 0 || length(criteria) == 0) return(character())
  keep_cols <- unique(unlist(lapply(criteria, function(pattern) grep(pattern, colnames(outlier_table), value = TRUE))))
  if (length(keep_cols) == 0) return(character())
  flags <- as.matrix(outlier_table[, keep_cols, drop = FALSE])
  rownames(outlier_table)[rowSums(flags == TRUE, na.rm = TRUE) == length(keep_cols)]
}

metminer_global_ai_pca_scores <- function(object) {
  expr <- tryCatch(as.matrix(massdataset::extract_expression_data(object)), error = function(e) NULL)
  sample_info <- tryCatch(massdataset::extract_sample_info(object), error = function(e) data.frame())
  if (is.null(expr) || ncol(expr) < 3 || nrow(expr) < 2) return(data.frame())
  row_means <- rowMeans(expr, na.rm = TRUE)
  keep <- is.finite(row_means)
  expr <- expr[keep, , drop = FALSE]
  row_means <- row_means[keep]
  na_idx <- which(is.na(expr), arr.ind = TRUE)
  if (nrow(na_idx) > 0) expr[na_idx] <- row_means[na_idx[, 1]]
  row_vars <- apply(expr, 1, stats::var)
  expr <- expr[is.finite(row_vars) & row_vars > 1e-12, , drop = FALSE]
  if (nrow(expr) < 2) return(data.frame())
  pca <- tryCatch(stats::prcomp(t(expr), scale. = TRUE, center = TRUE), error = function(e) NULL)
  if (is.null(pca) || ncol(pca$x) < 1) return(data.frame())
  pc1 <- pca$x[, 1]
  pc2 <- if (ncol(pca$x) >= 2) pca$x[, 2] else rep(NA_real_, length(pc1))
  data.frame(
    sample_id = rownames(pca$x),
    class = if ("class" %in% colnames(sample_info)) as.character(sample_info$class[match(rownames(pca$x), sample_info$sample_id)]) else NA_character_,
    group = if ("group" %in% colnames(sample_info)) as.character(sample_info$group[match(rownames(pca$x), sample_info$sample_id)]) else NA_character_,
    PC1 = signif(pc1, 5),
    PC2 = signif(pc2, 5),
    stringsAsFactors = FALSE
  )
}

metminer_global_ai_sample_group_counts <- function(sample_info) {
  sample_info <- as.data.frame(sample_info %||% data.frame(), stringsAsFactors = FALSE)
  cols <- intersect(c("class", "group", "batch"), colnames(sample_info))
  stats::setNames(lapply(cols, function(col) as.list(table(sample_info[[col]], useNA = "ifany"))), cols)
}

metminer_global_ai_raw_ms_structure <- function(raw_dir) {
  raw_dir <- normalizePath(raw_dir %||% "", winslash = "/", mustWork = FALSE)
  if (!has_text(raw_dir) || !dir.exists(raw_dir)) {
    return(list(available = FALSE, raw_ms1_dir = raw_dir))
  }
  mode_summary <- lapply(c("POS", "NEG"), function(mode) {
    mode_dir <- file.path(raw_dir, mode)
    if (!dir.exists(mode_dir)) {
      return(list(available = FALSE, mode_dir = mode_dir))
    }
    subdirs <- list.dirs(mode_dir, recursive = FALSE, full.names = TRUE)
    files <- list.files(mode_dir, recursive = TRUE, full.names = FALSE)
    mzxml <- grep("\\.mzxml$", files, ignore.case = TRUE, value = TRUE)
    mzml <- grep("\\.mzml$", files, ignore.case = TRUE, value = TRUE)
    list(
      available = TRUE,
      mode_dir = normalizePath(mode_dir, winslash = "/", mustWork = FALSE),
      subdirectories = basename(subdirs),
      mzxml_files = length(mzxml),
      mzml_files = length(mzml),
      example_files = utils::head(files, 10),
      result_object_exists = file.exists(file.path(mode_dir, "Result", "object"))
    )
  })
  names(mode_summary) <- c("POS", "NEG")
  list(
    available = TRUE,
    raw_ms1_dir = raw_dir,
    modes = mode_summary
  )
}

metminer_global_ai_object_detail_summary <- function(key, obj) {
  base <- metminer_global_ai_object_summary(key, obj)
  if (!isTRUE(base$available)) return(base)
  expression_data <- tryCatch(massdataset::extract_expression_data(obj), error = function(e) NULL)
  sample_info <- tryCatch(massdataset::extract_sample_info(obj), error = function(e) data.frame())
  variable_info <- tryCatch(metminer_safe_extract_variable_info(obj), error = function(e) data.frame())
  zero_rate <- NA_real_
  na_rate <- NA_real_
  intensity_quantiles <- list()
  if (!is.null(expression_data)) {
    values <- suppressWarnings(as.numeric(as.matrix(expression_data)))
    na_rate <- mean(is.na(values))
    zero_rate <- mean(values == 0, na.rm = TRUE)
    qs <- stats::quantile(values[is.finite(values) & values > 0], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    intensity_quantiles <- as.list(signif(qs, 4))
  }
  c(
    base,
    list(
      sample_id_preview = utils::head(as.character(sample_info$sample_id %||% rownames(sample_info)), 12),
      group_columns = intersect(c("group", "class", "sample_group", "batch", "injection_order"), colnames(sample_info)),
      mz_range = if ("mz" %in% colnames(variable_info)) as.list(range(suppressWarnings(as.numeric(variable_info$mz)), na.rm = TRUE)) else list(),
      rt_range = if ("rt" %in% colnames(variable_info)) as.list(range(suppressWarnings(as.numeric(variable_info$rt)), na.rm = TRUE)) else list(),
      expression_na_rate = signif(na_rate, 4),
      expression_zero_rate = signif(zero_rate, 4),
      intensity_quantiles = intensity_quantiles
    )
  )
}

metminer_global_ai_kegg_summary <- function(result, ai_review = NULL, curation = NULL) {
  if (is.null(result)) return(list(available = FALSE))
  qc <- as.data.frame(result$pathway_qc %||% data.frame())
  review_n <- if ("review_flag" %in% colnames(qc)) sum(qc$review_flag, na.rm = TRUE) else NA_integer_
  list(
    available = TRUE,
    organism_code = result$organism_code %||% NA_character_,
    organism_name = result$organism_name %||% NA_character_,
    output_dir = result$output_dir %||% NA_character_,
    compounds = nrow(result$clean_compounds %||% data.frame()),
    pathways = if (!is.null(result$pathway_database)) length(result$pathway_database@pathway_id) else NA_integer_,
    review_flagged_pathways = review_n,
    review_prompt_available = has_text(result$pathway_review_prompt %||% ""),
    ai_review_available = !is.null(ai_review),
    ai_review_rows = nrow(as.data.frame(ai_review %||% data.frame())),
    curation_rows = nrow(as.data.frame(curation %||% data.frame()))
  )
}

metminer_global_ai_plantcyc_summary <- function(result) {
  if (is.null(result)) return(list(available = FALSE))
  summary <- as.data.frame(result$summary %||% data.frame(), stringsAsFactors = FALSE)
  metric_value <- function(metric) {
    if (!all(c("metric", "value") %in% colnames(summary))) return(NA)
    value <- summary$value[summary$metric == metric]
    if (length(value) == 0) NA else value[1]
  }
  list(
    available = TRUE,
    output_dir = result$output_dir %||% NA_character_,
    output_prefix = result$output_prefix %||% NA_character_,
    summary_rows = nrow(summary),
    ms1_compounds = metric_value("ms1_compounds"),
    ms2_compounds = metric_value("ms2_compounds"),
    pathway_count = metric_value("pathway_count"),
    pathway_compound_links = metric_value("pathway_compound_links"),
    removed_compounds = nrow(result$clean_result$removed_compounds %||% data.frame()),
    ms2_match_log_rows = nrow(result$ms2_match_log %||% data.frame())
  )
}

metminer_global_ai_differential_summary <- function(result) {
  if (is.null(result)) return(list(available = FALSE))
  table <- as.data.frame(result$result %||% result %||% data.frame(), stringsAsFactors = FALSE)
  p_col <- result$p_column %||% if ("p_adjust" %in% colnames(table)) "p_adjust" else if ("p_value" %in% colnames(table)) "p_value" else NA_character_
  list(
    available = TRUE,
    rows = nrow(table),
    columns = colnames(table),
    mode = result$mode %||% NA_character_,
    control_samples = length(result$control_ids %||% character()),
    case_samples = length(result$case_ids %||% character()),
    p_column = p_col,
    up = if ("change" %in% colnames(table)) sum(table$change == "Up", na.rm = TRUE) else NA_integer_,
    down = if ("change" %in% colnames(table)) sum(table$change == "Down", na.rm = TRUE) else NA_integer_,
    unchanged = if ("change" %in% colnames(table)) sum(table$change == "No sig", na.rm = TRUE) else NA_integer_,
    annotated_rows = if (any(c("Compound.name", "compound_name", "annotation") %in% colnames(table))) {
      ann_cols <- intersect(c("Compound.name", "compound_name", "annotation"), colnames(table))
      sum(has_text(table[[ann_cols[1]]]), na.rm = TRUE)
    } else NA_integer_,
    top_rows = utils::head(table, 10)
  )
}

metminer_global_ai_enrichment_summary <- function(result) {
  if (is.null(result)) return(list(available = FALSE))
  table <- tryCatch(metminer_extract_enrich_result_table(result), error = function(e) data.frame())
  list(
    available = TRUE,
    class = paste(class(result), collapse = "/"),
    rows = nrow(table),
    columns = colnames(table),
    mapped_pathways = if ("mapped_number" %in% colnames(table)) sum(table$mapped_number > 0, na.rm = TRUE) else NA_integer_,
    fdr_0_05 = if ("p_value_adjust" %in% colnames(table)) sum(table$p_value_adjust < 0.05, na.rm = TRUE) else NA_integer_,
    top_pathways = utils::head(table, 10)
  )
}

metminer_global_ai_detect_kegg_review_request <- function(user_msg, global_data, rejected_keys = character()) {
  msg <- tolower(trimws(user_msg %||% ""))
  if (!grepl("@kegg-review|@kegg_review", msg, perl = TRUE)) return(NULL)
  wants_kegg <- grepl("kegg|通路|pathway|database|数据库", msg)
  wants_review <- grepl("review|审查|审核|检查|curat|json|存疑|weak", msg)
  if (!wants_kegg || !wants_review) return(NULL)
  result <- shiny::isolate(global_data$kegg_database_result)
  if (is.null(result)) return(NULL)
  action_key <- metminer_global_ai_kegg_review_key(result)
  if (action_key %in% rejected_keys) return(NULL)
  qc <- as.data.frame(result$pathway_qc %||% data.frame())
  review_n <- if ("review_flag" %in% colnames(qc)) sum(qc$review_flag, na.rm = TRUE) else 0L
  prompt <- result$pathway_review_prompt %||% ""
  if (review_n <= 0 || !has_text(prompt)) return(NULL)
  list(
    type = "kegg_pathway_review",
    title = "KEGG pathway AI review",
    message = paste0(
      "检测到 ", result$organism_name %||% "organism", " (", result$organism_code %||% "NA",
      ") KEGG 数据库已有 ", review_n, " 个需要 review 的 pathway。确认后将调用当前 LLM 配置生成 JSON 审查文件。"
    ),
    organism_code = result$organism_code %||% "organism",
    organism_name = result$organism_name %||% "organism",
    output_dir = result$output_dir %||% getwd(),
    review_n = review_n,
    key = action_key
  )
}

metminer_global_ai_kegg_review_key <- function(result) {
  paste(
    result$organism_code %||% "organism",
    normalizePath(result$output_dir %||% getwd(), winslash = "/", mustWork = FALSE),
    sep = "::"
  )
}

metminer_global_ai_is_affirmative <- function(user_msg) {
  msg <- tolower(trimws(user_msg %||% ""))
  msg <- gsub("[[:punct:]，。！？；：、\\s]+", "", msg)
  msg %in% c("yes", "y", "ok", "okay", "confirm", "sure", "go", "run", "doit",
             "是", "是的", "好的", "确定", "确认", "可以", "行", "执行", "开始", "运行")
}

metminer_global_ai_is_affirmative_or_retry <- function(user_msg) {
  msg <- tolower(trimws(user_msg %||% ""))
  metminer_global_ai_is_affirmative(msg) ||
    grepl("retry|rerun|again|继续|重试|再试|重新|再跑|继续执行|继续review|继续审查", msg)
}

metminer_global_ai_is_negative <- function(user_msg) {
  msg <- tolower(trimws(user_msg %||% ""))
  compact <- gsub("[[:punct:]，。！？；：、\\s]+", "", msg)
  compact %in% c("no", "n", "nope", "cancel", "stop", "skip",
                 "否", "不", "不用", "不需要", "取消", "停止", "先不用", "不要") ||
    grepl("不要|不用|不需要|取消|先不|暂不|别弹|不要再提示|dont|do not|don't", msg)
}

metminer_global_ai_run_kegg_review_action <- function(action,
                                                      chat,
                                                      start_ai_request,
                                                      update_ai_progress,
                                                      finish_ai_request,
                                                      provider,
                                                      model,
                                                      api_key,
                                                      base_url,
                                                      temperature,
                                                      session,
                                                      global_data) {
  if (!has_text(api_key)) {
    append_chat(chat, "system", "请先在 LLM settings 中填写 API key，然后再确认执行 KEGG pathway review。")
    return()
  }
  result <- shiny::isolate(global_data$kegg_database_result)
  if (is.null(result) || !has_text(result$pathway_review_prompt %||% "")) {
    append_chat(chat, "system", "没有检测到可用的 KEGG pathway review prompt。请先完成 KEGG database 构建。")
    return()
  }

  start_ai_request("KEGG pathway review", "正在准备 KEGG review prompt 和文献检索上下文...")
  prompt <- result$pathway_review_prompt
  update_ai_progress("正在尝试调用 paper-search-mcp 检索 pathway 文献证据...")
  paper_context <- metminer_global_ai_kegg_paper_search_context(result)
  paper_context_json <- metminer_ai_safe_json(paper_context)
  if (identical(paper_context$status %||% "", "ok")) {
    update_ai_progress("paper-search 文献证据已加入 prompt，正在发送给 LLM...")
  } else {
    update_ai_progress("paper-search 不可用或检索失败，将继续进行无外部文献的保守审查...")
  }
  messages <- list(
    list(
      role = "system",
      content = paste(
        "You are a strict KEGG organism-specific pathway reviewer.",
        "Return only the JSON requested by the user prompt.",
        "Use paper_search_context only when it is provided with status ok and contains traceable papers.",
        "Do not invent literature, genes, reactions, or organism evidence not present in the prompt or paper_search_context.",
        sep = "\n"
      )
    ),
    list(role = "user", content = paste0(
      prompt,
      "\n\n# paper_search_context JSON\n",
      paper_context_json,
      "\n\nIf paper_search_context.status is not ok, do not cite external literature and keep literature-dependent decisions conservative."
    ))
  )

  future::plan(future::multisession, workers = 1)
  fut <- tryCatch(future::future(
    call_llm_async(provider, model, api_key, base_url, temperature, messages),
    packages = c("httr2", "jsonlite"),
    globals = list(
      call_llm_async = call_llm_async,
      provider = provider,
      model = model,
      api_key = api_key,
      base_url = base_url,
      temperature = temperature,
      messages = messages
    ),
    seed = TRUE
  ), error = function(e) e)

  if (inherits(fut, "error")) {
    finish_ai_request(paste("KEGG pathway review request failed:", fut$message), "Failed.")
    return()
  }

  started <- Sys.time()
  poll <- function() {
    shiny::withReactiveDomain(session, {
      if (future::resolved(fut)) {
        update_ai_progress("正在解析 JSON 并写入 KEGG 输出目录...")
        response <- tryCatch(future::value(fut), error = function(e) e)
        if (inherits(response, "error")) {
          finish_ai_request(paste("LLM call failed:", response$message), "Failed.")
          return()
        }
        output_dir <- result$output_dir %||% action$output_dir %||% getwd()
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        raw_file <- file.path(output_dir, paste0("kegg_", action$organism_code, "_ai_review_raw_response.md"))
        writeLines(response, raw_file, useBytes = TRUE)
        review_file <- tempfile(fileext = ".json")
        writeLines(response, review_file, useBytes = TRUE)
        parsed <- tryCatch(metminer_kegg_parse_ai_review_json(review_file), error = function(e) e)
        if (inherits(parsed, "error")) {
          finish_ai_request(
            paste0("AI 已返回内容，但 JSON 解析失败：", parsed$message, "\n\n原始响应已保存到：`", raw_file, "`"),
            "JSON parse failed."
          )
          return()
        }
        parsed$model <- model %||% provider %||% "AI"
        curation <- metminer_kegg_prepare_multi_review_curation(result$pathway_qc, parsed)
        curation$final_status <- curation$consensus_decision
        global_data$kegg_ai_review <- parsed
        global_data$kegg_ai_review_curation <- curation
        json_file <- file.path(output_dir, paste0("kegg_", action$organism_code, "_ai_review_merged.json"))
        tsv_file <- file.path(output_dir, paste0("kegg_", action$organism_code, "_ai_review_merged.tsv"))
        summary_file <- file.path(output_dir, paste0("kegg_", action$organism_code, "_ai_review_merged_summary.json"))
        jsonlite::write_json(parsed, json_file, pretty = TRUE, na = "null")
        parsed_export <- parsed
        for (col in colnames(parsed_export)) {
          if (is.list(parsed_export[[col]])) {
            parsed_export[[col]] <- vapply(parsed_export[[col]], function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null"), character(1))
          }
        }
        utils::write.table(parsed_export, tsv_file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
        jsonlite::write_json(curation[, setdiff(colnames(curation), "ai_detail_html"), drop = FALSE], summary_file, pretty = TRUE, na = "null")
        answer <- paste0(
          "KEGG pathway AI review 已完成。\n\n",
          "- 数据库：", action$organism_name, " (", action$organism_code, ")\n",
          "- Review pathways：", nrow(parsed), "\n",
          "- Keep：", sum(curation$final_status == "keep", na.rm = TRUE), "\n",
          "- Remove：", sum(curation$final_status == "remove", na.rm = TRUE), "\n",
          "- Review/undecided：", sum(curation$final_status == "review", na.rm = TRUE), "\n\n",
          "已生成文件：\n",
          "- `", raw_file, "`\n",
          "- `", json_file, "`\n",
          "- `", tsv_file, "`\n",
          "- `", summary_file, "`"
        )
        finish_ai_request(answer, "Ready.")
      } else if (as.numeric(difftime(Sys.time(), started, units = "secs")) > 600) {
        finish_ai_request("KEGG pathway AI review timed out after 600 seconds.", "Timed out.")
      } else {
        update_ai_progress("LLM 正在审查 weak KEGG pathways...")
        later::later(poll, 0.7)
      }
    })
  }
  later::later(poll, 0.3)
}

metminer_global_ai_kegg_paper_search_context <- function(result,
                                                         max_pathways = 10,
                                                         max_results_per_source = 1,
                                                         sources = c("pubmed", "pmc", "europepmc", "semantic", "crossref")) {
  if (!isTRUE(metminer_ai_paper_search_available())) {
    return(list(
      status = "not_available",
      message = "paper-search MCP CLI was not found; no external literature evidence was retrieved.",
      searched_pathways = 0L,
      papers = list()
    ))
  }
  qc <- as.data.frame(result$pathway_qc %||% data.frame())
  if (nrow(qc) == 0 || !"review_flag" %in% colnames(qc)) {
    return(list(status = "no_pathway_qc", message = "No KEGG pathway QC table was available.", papers = list()))
  }
  review <- qc[qc$review_flag %in% TRUE, , drop = FALSE]
  if (nrow(review) == 0) {
    return(list(status = "no_review_pathways", message = "No KEGG pathways were flagged for review.", papers = list()))
  }
  review <- review[order(review$reaction_coverage, review$supported_reactions, na.last = TRUE), , drop = FALSE]
  review <- utils::head(review, max_pathways)
  organism <- result$organism_name %||% result$organism_code %||% "organism"

  searches <- lapply(seq_len(nrow(review)), function(i) {
    pathway_id <- review$pathway_id[i]
    pathway_name <- review$pathway_name[i] %||% pathway_id
    query <- paste(organism, pathway_name, "metabolism pathway")
    found <- tryCatch(
      metminer_ai_search_papers(
        query = query,
        sources = sources,
        max_results_per_source = max_results_per_source
      ),
      error = function(e) list(status = "error", message = e$message, papers = list())
    )
    papers <- found$papers %||% list()
    if (length(papers) > 0) {
      papers <- lapply(papers, function(paper) {
        paper$pathway_id <- pathway_id
        paper$pathway_name <- pathway_name
        paper$query <- query
        paper
      })
    }
    list(
      pathway_id = pathway_id,
      pathway_name = pathway_name,
      query = query,
      status = found$status %||% "ok",
      message = found$message %||% NA_character_,
      total = found$total %||% length(papers),
      papers = papers
    )
  })
  papers <- unlist(lapply(searches, `[[`, "papers"), recursive = FALSE)
  list(
    status = "ok",
    note = paste0(
      "paper-search MCP was queried for the first ", nrow(review),
      " weakest KEGG pathways. Use only these records as external literature evidence."
    ),
    organism = organism,
    searched_pathways = nrow(review),
    total_papers = length(papers),
    searches = searches,
    papers = papers
  )
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
