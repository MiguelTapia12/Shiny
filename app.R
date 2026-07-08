# ==============================================================================
# APP.R — Aplicación Principal
# CR Breeding System — Central Romana Corp. v3.0
# ==============================================================================

source("global.R")

# --- TEMAS CORPORATIVOS CR (Light y Dark) ---
cr_theme_light <- bs_theme(
  version      = 5,
  primary      = "#0b5c2e",
  secondary    = "#15803d",
  success      = "#16a34a",
  warning      = "#d97706",
  info         = "#0369a1",
  danger       = "#dc2626",
  bg           = "#f8faf8",
  fg           = "#1a2e22",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

cr_theme_dark <- bs_theme(
  version      = 5,
  primary      = "#22c55e",
  secondary    = "#16a34a",
  success      = "#4ade80",
  warning      = "#fbbf24",
  info         = "#38bdf8",
  danger       = "#f87171",
  bg           = "#0f1a13",
  fg           = "#e2f0e8",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

# --- INTERFAZ DE USUARIO ---
ui <- page_navbar(
  title = tags$span(
    useShinyjs(),
    tags$img(
      src    = "logo_cr.png",
      height = "34px",
      style  = "margin-right:8px; border-radius:4px; vertical-align:middle;",
      onerror = "this.style.display='none'"
    ),
    tags$b("CR Breeding", style = "color:#fff; letter-spacing:.4px; vertical-align:middle;"),
    tags$span(`data-i18n` = "nav_subtitle", " \u2014 Sistema de Mejoramiento",
              style = "color:#a7f3d0; font-size:.82em; font-weight:400; margin-left:4px; vertical-align:middle;")
  ),
  window_title = "CR Breeding \u2014 Central Romana",
  navbar_options = navbar_options(bg = "#0b5c2e", theme = "dark"),
  theme = cr_theme_light,
  
  # CSS externo + JS dark mode toggle + i18n
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "i18n.js"),
    tags$script(HTML("
      // Dark mode toggle: alterna clase en body y guarda preferencia
      function toggleDarkMode() {
        var body  = document.body;
        var isDark = body.classList.toggle('dark-mode');
        localStorage.setItem('cr_dark_mode', isDark ? '1' : '0');
        var btn = document.getElementById('dark_toggle_label');
        if (btn) btn.innerText = isDark ? '\u2600 Modo Claro' : '\uD83C\uDF19 Modo Oscuro';
        // Notificar a Shiny para cambiar el tema bslib
        Shiny.setInputValue('dark_mode_active', isDark, {priority: 'event'});
      }
      // Language toggle
      function toggleLang() {
        var current = i18n.lang || 'es';
        i18n.setLang(current === 'es' ? 'en' : 'es');
      }
      // Restaurar preferencia al cargar y enviar estado inicial al servidor
      $(document).on('shiny:connected', function() {
        var isDark = localStorage.getItem('cr_dark_mode') === '1';
        if (isDark) {
          document.body.classList.add('dark-mode');
          var btn = document.getElementById('dark_toggle_label');
          if (btn) btn.innerText = '\u2600 Modo Claro';
        }
        Shiny.setInputValue('dark_mode_active', isDark, {priority: 'event'});
        // Enviar idioma guardado a Shiny
        var lang = localStorage.getItem('cr_lang') || 'es';
        Shiny.setInputValue('global_lang', lang, {priority: 'event'});
      });
    "))
  ),

  # Sidebar Global
  sidebar = sidebar(
    width = 280,
    title = tags$span(icon("sliders-h"), tags$span(`data-i18n` = "nav_sidebar_title", " Panel de Control")),
    bg = "#f0f7f3",

    # Toggle Modo Oscuro
    tags$button(
      id      = "btn_dark_toggle",
      class   = "btn dark-toggle-btn",
      onclick = "toggleDarkMode()",
      tags$span(id = "dark_toggle_label", `data-i18n` = "btn_dark_mode", "\U0001F319 Modo Oscuro")
    ),

    # Toggle Idioma ES | EN
    tags$button(
      id      = "btn_lang_toggle",
      class   = "btn dark-toggle-btn",
      onclick = "toggleLang()",
      style   = "margin-top:4px;",
      tags$span(id = "lang_toggle_label", "\U0001F1EA\U0001F1F8 ES")
    ),

    actionButton(
      "btn_sync",
      tagList(icon("sync-alt"), tags$span(`data-i18n` = "sidebar_sync", " Sincronizar Sistema")),
      class = "btn-success w-100 mb-3",
      style = "font-weight:700; height:46px; font-size:.9rem;"
    ),
    hr(),
    tags$div(
      class = "cr-info-box",
      tags$p(icon("database"),   tags$b(tags$span(`data-i18n` = "sidebar_varieties", " Variedades: ")),  nrow(cat_var)),
      tags$p(icon("dna"),        tags$b(tags$span(`data-i18n` = "sidebar_parentage", " Parentesco: ")),  nrow(pedigree_var)),
      tags$p(icon("chart-line"), tags$b(tags$span(`data-i18n` = "sidebar_performance", " Rendimiento: ")), nrow(sim_rendimiento)),
      tags$p(icon("heartbeat"),  tags$b(tags$span(`data-i18n` = "sidebar_health", " Sanidad: ")),     nrow(sim_enfermedades)),
      hr(style = "margin:6px 0;"),
      tags$p(class = "text-center text-muted fst-italic mb-0",
             tags$small("Central Romana Corp.", tags$br(), "Programa de Mejoramiento"))
    )
  ),

  # ── ZONA 0: INICIO ────────────────────────────────────────────────────────
  nav_panel(
    title = tags$span(`data-i18n` = "nav_home", "Inicio"),
    icon  = icon("home"),
    mod_home_ui("home")
  ),

  # ── ZONA 1: GENÉTICA (primer paso del pipeline — cruces y semilla) ────────
  nav_menu(
    title = tags$span(`data-i18n` = "nav_genetics", "Gen\u00e9tica"),
    icon  = icon("dna"),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_cross_suggest", "Modo Cruzamientos"),
      icon  = icon("vials"),
      mod_cruzamientos_ui("cruzamientos")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_fuzz_bank", "Banco de Fuzz"),
      icon  = icon("box-archive"),
      mod_banco_fuzz_ui("banco_fuzz")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_floracion", "Gesti\u00f3n de Floraci\u00f3n"),
      icon  = icon("spa"),
      mod_floracion_ui("floracion")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_genealogy_viewer", "Visor de Genealog\u00eda"),
      icon  = icon("project-diagram"),
      mod_genealogia_ui("genealogia")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_variety_status", "Estado de Variedad"),
      icon  = icon("certificate"),
      mod_estado_variedad_ui("estado_var")
    )
  ),

  # ── ZONA 2: CAMPAÑA (evaluación en campo y análisis de resultados) ────────
  nav_menu(
    title = tags$span(`data-i18n` = "nav_campaign", "Campa\u00f1a"),
    icon  = icon("seedling"),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_capture_analytics", "Captura de Campo y Anal\u00edtica"),
      icon  = icon("mobile-alt"),
      mod_seleccion_ui("seguimiento")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_phytopathology", "Fitopatolog\u00eda y Sanidad"),
      icon  = icon("virus"),
      mod_fitopatologia_ui("fito")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_success_dashboard", "Dashboard de \u00c9xito"),
      icon  = icon("chart-bar"),
      mod_dashboard_ui("dashboard")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_mgmt_panel", "Panel Gerencial"),
      icon  = icon("chart-pie"),
      mod_gerencial_ui("gerencial")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_intelligence", "Inteligencia Anal\u00edtica"),
      icon  = icon("brain"),
      mod_inteligencia_ui("inteligencia")
    )
  ),

  # ── ZONA 3: ADMINISTRACIÓN ────────────────────────────────────────────────
  nav_menu(
    title = tags$span(`data-i18n` = "nav_admin", "Administraci\u00f3n"),
    icon  = icon("cogs"),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_field_assistant", "Asistente de Campo"),
      icon  = icon("magic"),
      mod_asistente_ui("asistente")
    ),
    nav_panel(
      title = tags$span(`data-i18n` = "nav_master_files", "Archivo Maestro y Descargas"),
      icon  = icon("folder-open"),
      mod_archivo_ui("archivo")
    )
  )
)

# --- SERVIDOR ---
server <- function(input, output, session) {
  
  # Guard: si la conexion global esta cerrada (ej. reload en RStudio), reconectar
  if (!DBI::dbIsValid(con)) {
    message(">> Reconectando a SQLite (conexion previa cerrada)...")
    con <<- db_connect("data/breeding_system.db")
    db_init_schema(con)
  }
  
  # Cerrar conexión a BD al terminar la sesión
  onStop(function() {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
      message(">> Conexion SQLite cerrada correctamente.")
    }
  })

  # Modo Oscuro — cambia el tema bslib completo al togglear
  observeEvent(input$dark_mode_active, {
    if (isTRUE(input$dark_mode_active)) {
      session$setCurrentTheme(cr_theme_dark)
    } else {
      session$setCurrentTheme(cr_theme_light)
    }
  }, ignoreNULL = TRUE)

  # Idioma reactivo global — accesible por todos los módulos
  lang <- reactiveVal("es")
  observeEvent(input$global_lang, {
    lang(input$global_lang)
  }, ignoreNULL = TRUE)

  # Estado reactivo global (compartido entre módulos)
  rv <- reactiveValues(
    df_familias   = suppressWarnings(dbReadTable(con, "familias_evf")) %>%
      rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
      mutate(ano = as.character(ano)),
    df_categorias = suppressWarnings(dbReadTable(con, "categorias")),
    df_st1        = suppressWarnings(dbReadTable(con, "clones_st1")),
    df_st2        = suppressWarnings(dbReadTable(con, "clones_st2")),
    df_st3        = suppressWarnings(dbReadTable(con, "clones_st3")),
    df_st4        = suppressWarnings(dbReadTable(con, "clones_st4")),
    df_st5        = suppressWarnings(dbReadTable(con, "clones_st5"))
  )
  
  # Auto-sincronización reactiva con la BD cada 2 segundos
  db_mtime <- reactivePoll(2000, session,
    checkFunc = function() {
      db_file <- "data/breeding_system.db"
      if (file.exists(db_file)) file.info(db_file)$mtime else Sys.time()
    },
    valueFunc = function() {
      file.info("data/breeding_system.db")$mtime
    }
  )
  
  observe({
    req(db_mtime())
    if (DBI::dbIsValid(con)) {
      message(">> [Auto-Sync] Cambios detectados en SQLite. Actualizando dashboards reactivos...")
      try({
        rv$df_familias   <- suppressWarnings(dbReadTable(con, "familias_evf")) %>%
          rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
          mutate(ano = as.character(ano))
        cat_temp <- suppressWarnings(dbReadTable(con, "categorias"))
        if (exists("ebvs_var") && nrow(ebvs_var) > 0) {
          # Remover tca, rend, taa si existen vacios, y traerlos de ebvs_var
          cat_temp <- cat_temp %>% 
            select(-any_of(c("tca", "rend", "taa"))) %>%
            full_join(ebvs_var %>% select(variedad, tca = ebv_tca, rend = ebv_rend, taa = ebv_tsh), by = "variedad") %>%
            # Llenar categoria con "Histórica/Avanzada" si es NA
            mutate(categoria = ifelse(is.na(categoria), "Avanzada/EBV", categoria))
        }
        rv$df_categorias <- cat_temp
        rv$df_st1        <- suppressWarnings(dbReadTable(con, "clones_st1"))
        rv$df_st2        <- suppressWarnings(dbReadTable(con, "clones_st2"))
        rv$df_st3        <- suppressWarnings(dbReadTable(con, "clones_st3"))
        rv$df_st4        <- suppressWarnings(dbReadTable(con, "clones_st4"))
        rv$df_st5        <- suppressWarnings(dbReadTable(con, "clones_st5"))
      }, silent = TRUE)
    }
  })
  
  # Botón de Sincronización con confirmación
  observeEvent(input$btn_sync, {
    ask_confirmation(
      inputId   = "confirm_sync",
      title     = tr("dlg_confirm_sync_title", lang()),
      text      = tr("dlg_confirm_sync_text", lang()),
      type      = "warning",
      btn_labels = c(tr("btn_cancel", lang()), tr("btn_sync", lang())),
      btn_colors = c("#d33", "#0b5c2e")
    )
  })
  
  observeEvent(input$confirm_sync, {
    req(isTRUE(input$confirm_sync))
    withProgress(message = tr("msg_syncing", lang()), value = 0, {
      resultado <- run_full_etl_sync(
        con = con,
        allact_file   = "AllAct2025.xls",
        families_file = "Evaluacion de Familias.xlsx",
        progress_callback = function(val, det) setProgress(val, detail = det)
      )
      if (resultado$ok) {
        rv$df_familias   <- dbReadTable(con, "familias_evf") %>%
          rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
          mutate(ano = as.character(ano))
        cat_temp_sync <- dbReadTable(con, "categorias")
        if (exists("ebvs_var") && nrow(ebvs_var) > 0) {
          cat_temp_sync <- cat_temp_sync %>% 
            select(-any_of(c("tca", "rend", "taa"))) %>%
            left_join(ebvs_var %>% select(variedad, tca = ebv_tca, rend = ebv_rend, taa = ebv_tsh), by = "variedad")
        }
        rv$df_categorias <- cat_temp_sync
        rv$df_st1        <- dbReadTable(con, "clones_st1")
        rv$df_st2        <- dbReadTable(con, "clones_st2")
        rv$df_st3        <- dbReadTable(con, "clones_st3")
        rv$df_st4        <- dbReadTable(con, "clones_st4")
        rv$df_st5        <- dbReadTable(con, "clones_st5")
      }
    })
    if (resultado$ok) {
      sendSweetAlert(session, title = tr("msg_success", lang()),  text = resultado$msg, type = "success")
    } else {
      sendSweetAlert(session, title = tr("msg_error", lang()),     text = resultado$msg, type = "error")
    }
  })
  
  # Variable reactiva de df_categorias para el dashboard
  df_categorias_r <- reactive({ rv$df_categorias })
  
  # --- Construir opciones parentales ---
  nombres_disp <- df_act2025 %>%
    left_join(cat_var %>% select(id_variedad, descripcion_variedad), by = c("variedad" = "id_variedad")) %>%
    mutate(label = ifelse(is.na(descripcion_variedad), variedad, descripcion_variedad)) %>%
    arrange(label)
  opciones_parentales <- setNames(nombres_disp$variedad, nombres_disp$label)
  
  # --- Módulos ---
  ped_data    <- mod_genealogia_server("genealogia", con, ebvs_var, df_categorias = reactive({ rv$df_categorias }))
  
  # Módulo Banco de Fuzz
  mod_banco_fuzz_server("banco_fuzz", con)
  
  cruces_data <- mod_cruzamientos_server(
    "cruzamientos", cat_var, pedigree_var,
    df_ped_wide, df_act2025,
    reactive({ rv$df_categorias }),
    ebvs_var,
    opciones_parentales
  )
  
  estado_data <- mod_estado_variedad_server(
    "estado_var", cat_var, pedigree_var,
    reactive({ rv$df_categorias }),
    reactive({ rv$df_familias }),
    con = con
  )
  
  mod_seleccion_server("seguimiento", con = con, db_trigger = reactive({ db_mtime() }))
  
  mod_trazabilidad_server(
    "trazabilidad", cat_var, pedigree_var,
    reactive({ rv$df_categorias }), con = con
  )
  
  mod_home_server("home", con)
  mod_dashboard_server("dashboard", con = con, df_categorias_rv = df_categorias_r)
  mod_gerencial_server("gerencial", con)
  
  mod_asistente_server(
    "asistente", cat_var, pedigree_var,
    df_ped_wide, con,
    reactive({ rv$df_categorias })
  )
  
  mod_archivo_server("archivo")
  mod_floracion_server("floracion", con = con)
  mod_fitopatologia_server("fito", con = con)
  mod_inteligencia_server("inteligencia", con = con)
}

# --- LANZAR APP ---
shinyApp(ui, server)