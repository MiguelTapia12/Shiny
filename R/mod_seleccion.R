# ==============================================================================
# MOD_SELECCION.R — Módulo Shiny: Seguimiento de Selecciones
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- Función Helper: Genera dinámicamente el sub-tabset de analítica por Etapa ---
make_stage_tab <- function(ns, stage_key, config) {
  is_evf <- stage_key == "EVF"
  
  # Preparar contenido de la sub-pestaña de Carga / Registro
  carga_ui <- if (is_evf) {
    layout_column_wrap(
      width = 1/2,

      # ── COLUMNA IZQUIERDA: Formularios ────────────────────────────────────────
      card(
        card_header(tagList(icon("archive"), " Evaluación de Familias (EVF)")),
        layout_column_wrap(
          width = 1/3,
          numericInput(ns("up_evf_year"), tags$span(`data-i18n`="lbl_harvest_year", tags$span(`data-i18n`="lbl_harvest_year", tags$span(`data-i18n`="lbl_harvest_year", "Año Zafra (Colecta):"))), value = 2026),
          numericInput(ns("up_evf_year_cruce"), tags$span(`data-i18n`="lbl_cross_year", tags$span(`data-i18n`="lbl_cross_year", tags$span(`data-i18n`="lbl_cross_year", "Año Cruce:"))), value = 2024),
          selectInput(ns("up_evf_prog"),  "Programa:", choices = c("CR", "BR"))
        ),
        fileInput(ns("file_evf"), tags$span(`data-i18n`="btn_upload_families_excel", tags$span(`data-i18n`="btn_upload_families_excel", tags$span(`data-i18n`="btn_upload_families_excel", "Cargar Excel de Familias"))), accept = ".xlsx"),
        layout_column_wrap(
          width = 1/3,
          numericInput(ns("threshold_r96"), "R96 (%):", value = 100),
          numericInput(ns("threshold_tca"), "TCA (%):", value = 100),
          numericInput(ns("threshold_tsa"), "TSA (%):", value = 110)
        ),
        layout_column_wrap(
          width = 1/2,
          actionButton(ns("btn_auto_select"), tags$span(`data-i18n`="btn_preselect", tags$span(`data-i18n`="btn_preselect", tags$span(`data-i18n`="btn_preselect", "Pre-seleccionar"))),   class = "btn-warning w-100"),
          actionButton(ns("btn_confirm_evf"), tags$span(`data-i18n`="btn_confirm_selection", tags$span(`data-i18n`="btn_confirm_selection", tags$span(`data-i18n`="btn_confirm_selection", "Confirmar Selección"))), class = "btn-success w-100")
        )
      ),

      # ── COLUMNA DERECHA: Vistas previas ───────────────────────────────────────
      uiOutput(ns("ui_card_evf"))
    )

  } else {
    # Etapas clonales ST1 a ST5
    stage_num <- as.integer(gsub("ST", "", stage_key))
    prev_stage_label <- paste0("ST", stage_num - 1)
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(tagList(icon("sync"), paste("Sincronización de Campo —", config$title))),
        layout_column_wrap(
          width = 1/4,
          numericInput(ns(paste0("up_", config$id, "_year")), tags$span(`data-i18n`="lbl_sel_year", tags$span(`data-i18n`="lbl_sel_year", tags$span(`data-i18n`="lbl_sel_year", "Año Sel:"))), 2026),
          numericInput(ns(paste0("up_", config$id, "_year_cruce")), tags$span(`data-i18n`="lbl_cross_year_short", tags$span(`data-i18n`="lbl_cross_year_short", tags$span(`data-i18n`="lbl_cross_year_short", "Año Cru:"))), 2026 - config$cruce_year_diff),
          selectInput(ns(paste0("up_", config$id, "_prog")), "Prog:", choices = if (stage_num == 1) c("CR", "BR", "EVF CR", "EVF BR") else c("CR", "BR")),
          selectInput(ns(paste0("up_", config$id, "_soil")), tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", "Suelo:"))), choices = c("BUENO", "MAL_DRENADO", "ROCOSO"))
        ),
        actionButton(ns(paste0("btn_api_", stage_num)), tags$span(`data-i18n`="btn_view_field_data", tags$span(`data-i18n`="btn_view_field_data", tags$span(`data-i18n`="btn_view_field_data", "Ver Datos de Campo (API)"))), class = "btn-primary w-100 mb-2", icon = icon("cloud-download-alt")),
        tags$details(
          tags$summary(tags$span(`data-i18n`="sel_manual_upload", tags$span(`data-i18n`="sel_manual_upload", tags$span(`data-i18n`="sel_manual_upload", "⚙️ Carga Manual / Excel (Admin)")))),
          fileInput(ns(paste0("f", stage_num)), tags$span(`data-i18n`="btn_upload_field_excel", tags$span(`data-i18n`="btn_upload_field_excel", tags$span(`data-i18n`="btn_upload_field_excel", "Subir Excel de Campo"))), accept=".xlsx"),
          actionButton(ns(paste0("btn_confirm_", config$id)), tags$span(`data-i18n`="btn_confirm_manual", tags$span(`data-i18n`="btn_confirm_manual", tags$span(`data-i18n`="btn_confirm_manual", "Confirmar Carga Manual"))), class = "btn-warning w-100")
        ),
        if (stage_num > 1) {
          tagList(
            hr(),
            card(card_header(paste(tags$span(`data-i18n`="sel_promoted_candidates", tags$span(`data-i18n`="sel_promoted_candidates", tags$span(`data-i18n`="sel_promoted_candidates", "Candidatos Promocionados desde"))), prev_stage_label)), DT::DTOutput(ns(paste0("promocionados_est", stage_num - 1))), height = "200px")
          )
        } else {
          NULL
        }
      ),
      card(card_header(tags$span(`data-i18n`="sel_excel_preview", tags$span(`data-i18n`="sel_excel_preview", tags$span(`data-i18n`="sel_excel_preview", "Vista Previa del Excel de Campo")))), shinycssloaders::withSpinner(DT::DTOutput(ns(paste0("t", stage_num))), type=4, color="#0b5c2e"))
    )
  }

  # Preparar contenido de la sub-pestaña de Analítica y Lista de Corte local
  analitica_ui <- tagList(
    accordion(
      open = FALSE,
      accordion_panel(
        title = paste("Filtros de Analítica y Reportes —", config$title),
        icon = icon("sliders-h"),
        fluidRow(
          column(width = 2, numericInput(ns(paste0("rpt_year_", config$id)), tags$span(`data-i18n`="lbl_selection_year", tags$span(`data-i18n`="lbl_selection_year", tags$span(`data-i18n`="lbl_selection_year", "Año de Selección:"))), 2026)),
          column(width = 2, selectInput(ns(paste0("rpt_prog_", config$id)), "Programa:", choices = c("Todos", "CR", "BR", "EVF CR", "EVF BR"))),
          column(width = 2,
            if (config$id == "evf") {
              NULL
            } else {
              selectInput(ns(paste0("rpt_soil_", config$id)), tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", "Suelo:"))), choices = c("Todos", "BUENO", "MAL_DRENADO", "ROCOSO"))
            }
          ),
          column(width = 2,
            if (config$id %in% c("st4", "st5")) {
              selectInput(ns(paste0("rpt_exp_", config$id)), tags$span(`data-i18n`="lbl_experiment", tags$span(`data-i18n`="lbl_experiment", tags$span(`data-i18n`="lbl_experiment", "Experimento:"))), choices = c("Todos"))
            } else {
              NULL
            }
          ),
          column(width = 2,
            if (config$id %in% c("st4", "st5")) {
              selectInput(ns(paste0("rpt_div_", config$id)), tags$span(`data-i18n`="lbl_division", tags$span(`data-i18n`="lbl_division", tags$span(`data-i18n`="lbl_division", "División:"))),
                choices = c("Todos", "HIGUERAL", "GUAYMATE", "LECHUGAS",
                            "CUYA", "BAIGUA", "CHAVON ABAJO", "LA HIGUERA"))
            } else {
              NULL
            }
          ),
          column(width = 2, style = "padding-top: 32px;",
            actionButton(ns(paste0("btn_gen_rpt_", config$id)), tags$span(`data-i18n`="btn_update_analytics", tags$span(`data-i18n`="btn_update_analytics", tags$span(`data-i18n`="btn_update_analytics", "Actualizar Analítica"))), class = "btn-primary", icon = icon("sync"))
          ),
          column(width = 2, style = "padding-top: 32px;",
            downloadButton(ns(paste0("btn_dw_rpt_", config$id)), if(config$id %in% c("st4", "st5")) tags$span(`data-i18n`="btn_download_variety_data", tags$span(`data-i18n`="btn_download_variety_data", tags$span(`data-i18n`="btn_download_variety_data", "Descargar Datos de Variedades"))) else tags$span(`data-i18n`="btn_download_list", tags$span(`data-i18n`="btn_download_list", tags$span(`data-i18n`="btn_download_list", "Descargar Lista"))), class = "btn-success float-end")
          )
        )
      )
    ),
    br(),
    
    # Value Boxes
    uiOutput(ns(paste0("ui_vbox_", config$id))),
    
    br(),
    # Layout de la tabla + gráficos
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      if (config$id %in% c("st4", "st5")) {
        layout_columns(
          col_widths = c(12),
          card(
            card_header(tagList(icon("chart-line"), tags$span(`data-i18n`="chart_isoproductivity", tags$span(`data-i18n`="chart_isoproductivity", tags$span(`data-i18n`="chart_isoproductivity", " Curvas de Isoproductividad (Arrastre para Zoom y Doble Clic para Alejar)"))))),
            layout_column_wrap(
              width = 1/4,
              selectInput(ns(paste0("iso_grad_", config$id)), tags$span(`data-i18n`="lbl_gradient_palette", tags$span(`data-i18n`="lbl_gradient_palette", tags$span(`data-i18n`="lbl_gradient_palette", "Paleta de Gradiente:"))), choices = c("Default", "Vice City", "Evening Night", "Ibiza Sunset", "Green Beach", "Flickr")),
              selectInput(ns(paste0("iso_testigo_", config$id)), tags$span(`data-i18n`="lbl_base_control", tags$span(`data-i18n`="lbl_base_control", tags$span(`data-i18n`="lbl_base_control", "Testigo Base:"))), choices = c("Todos", "BR0010", "CR87339", "Ninguno")),
              numericInput(ns(paste0("iso_bins_", config$id)), tags$span(`data-i18n`="lbl_contours", tags$span(`data-i18n`="lbl_contours", tags$span(`data-i18n`="lbl_contours", "Contornos:"))), value = 12, min = 5, max = 30),
              checkboxInput(ns(paste0("iso_fill_", config$id)), tags$span(`data-i18n`="lbl_thermal_fill", tags$span(`data-i18n`="lbl_thermal_fill", tags$span(`data-i18n`="lbl_thermal_fill", "Relleno Térmico"))), value = TRUE)
            ),
            shinycssloaders::withSpinner(plotOutput(ns(paste0("plot_iso_", config$id)), height = "500px",
                                                    dblclick = ns(paste0("iso_dblclick_", config$id)),
                                                    brush = brushOpts(id = ns(paste0("iso_brush_", config$id)), resetOnNew = TRUE)), type=4, color="#0b5c2e")
          )
        )
      } else {
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(tagList(icon("chart-bar"), tags$span(`data-i18n`="chart_brix_distribution", tags$span(`data-i18n`="chart_brix_distribution", tags$span(`data-i18n`="chart_brix_distribution", " Distribución de Brix / Calidad (Selección vs Testigos vs Rechazo)"))))),
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(paste0("plot_brix_comp_", config$id)), height = "320px"), type=4, color="#0b5c2e")
          ),
          card(
            card_header(tagList(icon("award"), tags$span(`data-i18n`="chart_top_families", tags$span(`data-i18n`="chart_top_families", tags$span(`data-i18n`="chart_top_families", " Top 10 Cruces / Familias con Más Selecciones"))))),
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(paste0("plot_top_cruces_", config$id)), height = "320px"), type=4, color="#0b5c2e")
          )
        )
      },
      
      # Tabla de Lista de Corte
      card(
        card_header(tagList(icon("list"), if(config$id %in% c("st4", "st5")) tags$span(`data-i18n`="sel_variety_analytics", tags$span(`data-i18n`="sel_variety_analytics", tags$span(`data-i18n`="sel_variety_analytics", "Analítica de Variedades"))) else tags$span(`data-i18n`="sel_cut_list", tags$span(`data-i18n`="sel_cut_list", tags$span(`data-i18n`="sel_cut_list", "Lista de Corte / Selección (Material Seleccionado)"))))),
        shinycssloaders::withSpinner(DT::DTOutput(ns(paste0("tabla_corte_", config$id))), type=4, color="#0b5c2e")
      )
    )
  )

  # Ensamblar en un hermoso sub-navegador de tarjetas tipo Pill (Mínimo Desorden UX)
  nav_panel(
    title = config$title,
    icon = icon(config$icon),
    navset_card_pill(
      nav_panel(
        value = "tab_register_sync",
        title = tags$span(`data-i18n`="tab_register_sync", tags$span(`data-i18n`="tab_register_sync", tags$span(`data-i18n`="tab_register_sync", "Registro y Sincronización"))),
        icon = icon("upload"),
        carga_ui
      ),
      nav_panel(
        value = "tab_analytics_cutlist",
        title = if (config$id %in% c("st4", "st5")) tags$span(`data-i18n`="tab_variety_study", tags$span(`data-i18n`="tab_variety_study", tags$span(`data-i18n`="tab_variety_study", "Estudio de Variedades"))) else tags$span(`data-i18n`="tab_analytics_cutlist", tags$span(`data-i18n`="tab_analytics_cutlist", tags$span(`data-i18n`="tab_analytics_cutlist", "Analítica y Lista de Corte"))),
        icon = icon(if (config$id %in% c("st4", "st5")) "microscope" else "chart-pie"),
        analitica_ui
      )
    )
  )
}

# --- UI del Módulo ---
mod_seleccion_ui <- function(id) {
  ns <- NS(id)
  
  stages_config <- list(
    EVF = list(id = "evf", title = "EVF: Familias", icon = "users", cruce_year_diff = 1),
    ST1 = list(id = "st1", title = "Estado 1", icon = "seedling", cruce_year_diff = 1),
    ST2 = list(id = "st2", title = "Estado 2", icon = "tachometer-alt", cruce_year_diff = 2),
    ST3 = list(id = "st3", title = "Estado 3", icon = "industry", cruce_year_diff = 3),
    ST4 = list(id = "st4", title = "Estado 4", icon = "flask", cruce_year_diff = 4),
    ST5 = list(id = "st5", title = "Estado 5", icon = "vial", cruce_year_diff = 5)
  )

  tagList(
    navset_card_tab(
      title = tagList(icon("dna"), " Gestión del Pipeline de Selección"),
      
      # Generar dinámicamente las pestañas de cada Estado con su analítica integrada
      make_stage_tab(ns, "EVF", stages_config$EVF),
      make_stage_tab(ns, "ST1", stages_config$ST1),
      make_stage_tab(ns, "ST2", stages_config$ST2),
      make_stage_tab(ns, "ST3", stages_config$ST3),
      make_stage_tab(ns, "ST4", stages_config$ST4),
      make_stage_tab(ns, "ST5", stages_config$ST5),

# --- CAPTURA DE CAMPO (App Móvil) ---
      nav_panel(
        value = "tab_field_capture_app",
        title = tags$span(`data-i18n`="tab_field_capture_app", tags$span(`data-i18n`="tab_field_capture_app", tags$span(`data-i18n`="tab_field_capture_app", "Captura de Campo (App)"))),
        icon  = icon("mobile-alt"),
        layout_column_wrap(
          width = 1,
          fill  = FALSE,
          uiOutput(ns("ui_fc_stats")),
          card(
            card_header(tagList(icon("filter"), " Filtros y Acciones")),
            layout_column_wrap(
              width = 1/4,
              selectInput(ns("fc_etapa"),  "Etapa:",    choices = c("Todas","ST1","ST2","ST3","ST4","ST5")),
              selectInput(ns("fc_prog"),   "Programa:", choices = c("Todos")),
              selectInput(ns("fc_accion"), "Decisión:", choices = c("Todas","S","T","R")),
              selectInput(ns("fc_suelo"),  tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", tags$span(`data-i18n`="lbl_soil", "Suelo:"))),    choices = c("Todos","BUENO","MAL_DRENADO","ROCOSO"))
            ),
            layout_column_wrap(
              width = 1/4,
              actionButton(ns("btn_fc_refresh"), tags$span(`data-i18n`="btn_update_table", tags$span(`data-i18n`="btn_update_table", tags$span(`data-i18n`="btn_update_table", "Actualizar Tabla"))),
                           icon = icon("sync-alt"), class = "btn-primary w-100"),
              actionButton(ns("btn_fc_promote"), tags$span(`data-i18n`="btn_promote_st1", tags$span(`data-i18n`="btn_promote_st1", tags$span(`data-i18n`="btn_promote_st1", "Promover a ST1"))),
                           icon = icon("arrow-up"), class = "btn-success w-100"),
              actionButton(ns("btn_fc_eliminar"), tags$span(`data-i18n`="btn_delete_records", tags$span(`data-i18n`="btn_delete_records", tags$span(`data-i18n`="btn_delete_records", "Eliminar Registros"))),
                           icon = icon("trash"), class = "btn-danger w-100"),
              downloadButton(ns("dl_fc"), tags$span(`data-i18n`="btn_export_csv", tags$span(`data-i18n`="btn_export_csv", tags$span(`data-i18n`="btn_export_csv", "Exportar CSV"))),
                             class = "btn-outline-secondary w-100")
            )
          ),
          card(
            card_header(tagList(icon("table"), " Registros de Campo — field_captures")),
            DT::DTOutput(ns("tabla_fc"))
          )
        )
      ),
      

    )
  )
}

# --- Server del Módulo ---
mod_seleccion_server <- function(id, con, db_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Función auxiliar para manejar columnas inexistentes
    if_exists <- function(df, col_name, default_val) {
      if (col_name %in% names(df)) return(df[[col_name]])
      return(rep(default_val, nrow(df)))
    }

    # Piloto ST1: helper reutilizable para estandarizar lectura/procesamiento por etapa.
    process_stage_upload <- function(file_path, anio_seleccion, anio_cruce_default, prog_default, suelo_default) {
      # 1. Leer excel y limpiar nombres a minúsculas seguras
      df <- readxl::read_excel(file_path) %>%
        janitor::clean_names()
      
      # 2. Transformaciones robustas alineadas con el formato del usuario
      df <- df %>%
        mutate(
          anio_seleccion = as.integer(anio_seleccion),
          
          # Extraer información del programa si viene codificado como "CR22 CONV Bueno"
          raw_prog_col = as.character(if_exists(., "programa", "")),
          extracted_yr = stringr::str_extract(raw_prog_col, "\\d{2}"),
          extracted_prog = stringr::str_extract(raw_prog_col, "CR|BR"),
          
          anio_cruce = ifelse(!is.na(extracted_yr), as.numeric(extracted_yr) + 2000, anio_cruce_default),
          programa = ifelse(!is.na(extracted_prog), extracted_prog, prog_default),
          
          # Cruce como carácter limpio
          cruce = trimws(as.character(cruce)),
          es_testigo = toupper(cruce) %in% LISTA_TESTIGOS | 
                       toupper(as.character(if_exists(., "accion", ""))) == "T",
          
          suelo = suelo_default,
          
          # Mapear "Sel." (que janitor convierte a "sel") a num_sel
          num_sel = {
            raw <- as.character(if_exists(., "sel", if_exists(., "numero_de_seleccion", "0")))
            raw[is.na(raw) | raw == "-" | raw == ""] <- "0"
            as.character(raw)
          },
          
          # Soportar Brix con decimales en comas (ej: 23,2 -> 23.2)
          brix = {
            raw_brix <- as.character(if_exists(., "brix", "0"))
            cleaned_brix <- gsub(",", ".", raw_brix)
            suppressWarnings(as.numeric(cleaned_brix))
          },
          
          # Mapear "Agro." (que janitor convierte a "agro") a vigor
          vigor = {
            raw_vigor <- as.character(if_exists(., "agro", if_exists(., "vigor", "3")))
            raw_vigor[is.na(raw_vigor) | raw_vigor == "-" | raw_vigor == ""] <- "3"
            as.integer(raw_vigor)
          },
          
          # Acción de campo robusta (S = Seleccionado, T = Testigo, R = Rechazado)
          raw_accion = toupper(trimws(as.character(if_exists(., "accion", "R")))),
          accion = case_when(
            raw_accion %in% c("S", "SELECCIONADO", "SELECCIONADA", "SELECCION") ~ "S",
            raw_accion %in% c("T", "TESTIGO") ~ "T",
            TRUE ~ "R"
          ),
          
          # Evaluador (Opcional, por defecto 'Excel')
          evaluador = as.character(if_exists(., "evaluador", "Excel"))
        )

      # Unir con familias de EVF para asociar genealogía si está disponible
      fam_db <- dbGetQuery(con, "SELECT anio, cruce, madre, padre FROM familias_evf")
      if (nrow(fam_db) > 0) {
        fam_db <- fam_db %>%
          mutate(
            anio = as.numeric(as.character(anio)),
            cruce = trimws(as.character(cruce))
          )
        
        df <- df %>%
          mutate(
            anio_cruce = as.numeric(anio_cruce),
            cruce = trimws(as.character(cruce))
          ) %>%
          left_join(fam_db, by = c("anio_cruce" = "anio", "cruce")) %>%
          mutate(
            madre = ifelse(es_testigo, "TESTIGO", ifelse(is.na(madre), "Desconocida", madre)),
            padre = ifelse(es_testigo, "TESTIGO", ifelse(is.na(padre), "Desconocido", padre))
          )
      } else {
        df <- df %>%
          mutate(
            madre = ifelse(es_testigo, "TESTIGO", "Desconocida"),
            padre = ifelse(es_testigo, "TESTIGO", "Desconocido")
          )
      }

      df
    }
    
    # --- FUNCIÓN FACTORY PARA ESTADOS (ST1 - ST5) ---
    setup_stage <- function(st_num, file_in, btn_conf, btn_api, up_y, up_y_c, up_p, up_s, db_save_func) {
      rv_st <- reactiveVal(NULL)
      
      # 1. Carga desde API (Directo de BD)
      observeEvent(input[[btn_api]], {
        anio_sel <- input[[up_y]]
        prog <- input[[up_p]]
        suelo <- input[[up_s]]
        
        tbl_name <- paste0("clones_st", st_num)
        q <- sprintf("SELECT * FROM %s WHERE anio_seleccion = ? AND programa = ? AND suelo = ? AND origen = 'API'", tbl_name)
        
        df_api <- tryCatch(dbGetQuery(con, q, params = list(anio_sel, prog, suelo)), error = function(e) data.frame())
        
        if (nrow(df_api) == 0) {
          showNotification("No hay datos sincronizados vía API para este contexto.", type = "warning")
          rv_st(NULL)
          return()
        }
        
        # Enriquecer con pedigrí de familias EVF para visualización
        fam_db <- tryCatch(dbGetQuery(con, "SELECT anio, cruce, madre, padre FROM familias_evf"), error = function(e) data.frame())
        if (nrow(fam_db) > 0) {
          fam_db <- fam_db %>% mutate(anio = as.numeric(as.character(anio)), cruce = trimws(as.character(cruce)))
          df_api <- df_api %>%
            mutate(anio_cruce = as.numeric(anio_cruce), cruce = trimws(as.character(cruce))) %>%
            left_join(fam_db, by = c("anio_cruce" = "anio", "cruce")) %>%
            mutate(
              madre = ifelse(is.na(madre), "Desconocida", madre),
              padre = ifelse(is.na(padre), "Desconocido", padre)
            )
        } else {
          df_api <- df_api %>% mutate(madre = "Desconocida", padre = "Desconocido")
        }
        
        rv_st(df_api)
        showNotification(paste("Cargados", nrow(df_api), "registros sincronizados desde la API."), type = "message")
      })
      
      # 2. Carga Manual (Admin Fallback)
      observeEvent(input[[file_in]], {
        req(input[[file_in]])
        target_dir = file.path("data/storage", input[[up_y]], paste0("ST", st_num))
        if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
        file_name <- paste0(input[[up_y]], "_ST", st_num, "_", input[[up_p]], "_", input[[up_s]], ".xlsx")
        file.copy(input[[file_in]]$datapath, file.path(target_dir, file_name), overwrite = TRUE)
        
        df <- process_stage_upload(
          file_path = input[[file_in]]$datapath,
          anio_seleccion = input[[up_y]],
          anio_cruce_default = input[[up_y_c]],
          prog_default = input[[up_p]],
          suelo_default = input[[up_s]]
        )
        rv_st(df)
      })
      
      observeEvent(input[[btn_conf]], {
        req(rv_st())
        # Aseguramos que la carga manual tenga origen 'Manual'
        df_save <- rv_st() %>% 
          select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion, evaluador) %>%
          mutate(
            origen = "Manual",
            fecha_evaluacion = as.character(Sys.time()),
            latitud = NA_real_,
            longitud = NA_real_
          )
        
        db_save_func(con, df_save)
        n_sel <- sum(df_save$accion == "S", na.rm = TRUE)
        
        # Link: Automáticamente actualizar registro_cruces a EN_EVALUACION para ST1
        if (st_num == 1) {
          cruces_promovidos <- unique(df_save$cruce)
          if (length(cruces_promovidos) > 0) {
            cruces_str <- paste(sprintf("'%s'", cruces_promovidos), collapse = ",")
            tryCatch({
              DBI::dbExecute(con, sprintf("UPDATE registro_cruces SET estado = 'EN_EVALUACION' WHERE cruce IN (%s) AND estado != 'EN_EVALUACION'", cruces_str))
            }, error = function(e) {
              message("Error actualizando registro_cruces desde ST1: ", e$message)
            })
          }
        }
        
        showNotification(paste("Carga manual confirmada:", nrow(df_save), "clones evaluados al Estado", st_num, "."), type = "message")
      })
      
      return(rv_st)
    }

    # Instanciar reactivos de estados
    rv_st1 <- setup_stage(1, "f1", "btn_confirm_st1", "btn_api_1", "up_st1_year", "up_st1_year_cruce", "up_st1_prog", "up_st1_soil", db_save_st1_selection)
    rv_st2 <- setup_stage(2, "f2", "btn_confirm_st2", "btn_api_2", "up_st2_year", "up_st2_year_cruce", "up_st2_prog", "up_st2_soil", db_save_st2_selection)
    rv_st3 <- setup_stage(3, "f3", "btn_confirm_st3", "btn_api_3", "up_st3_year", "up_st3_year_cruce", "up_st3_prog", "up_st3_soil", db_save_st3_selection)
    rv_st4 <- setup_stage(4, "f4", "btn_confirm_st4", "btn_api_4", "up_st4_year", "up_st4_year_cruce", "up_st4_prog", "up_st4_soil", db_save_st4_selection)
    rv_st5 <- setup_stage(5, "f5", "btn_confirm_st5", "btn_api_5", "up_st5_year", "up_st5_year_cruce", "up_st5_prog", "up_st5_soil", db_save_st5_selection)

    # Renderizar tablas de vista previa
    output$t1 <- DT::renderDT({ req(rv_st1()); DT::datatable(rv_st1(), options = list(pageLength = 10), editable = TRUE) })
    output$t2 <- DT::renderDT({ req(rv_st2()); DT::datatable(rv_st2(), options = list(pageLength = 10), editable = TRUE) })
    output$t3 <- DT::renderDT({ req(rv_st3()); DT::datatable(rv_st3(), editable = TRUE) })
    output$t4 <- DT::renderDT({ req(rv_st4()); DT::datatable(rv_st4(), editable = TRUE) })
    output$t5 <- DT::renderDT({ req(rv_st5()); DT::datatable(rv_st5(), editable = TRUE) })

    # Renderizar candidatos promocionados
    output$promocionados_est1 <- DT::renderDT({ input$btn_confirm_st1; df <- db_get_selected_clones(con, "st1"); req(nrow(df) > 0); df %>% select(cruce, num_sel, brix) }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$promocionados_est2 <- DT::renderDT({ input$btn_confirm_st2; df <- db_get_selected_clones(con, "st2"); req(nrow(df) > 0); df %>% select(cruce, num_sel, brix) }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$promocionados_est3 <- DT::renderDT({ input$btn_confirm_st3; df <- db_get_selected_clones(con, "st3"); req(nrow(df) > 0); df %>% select(cruce, num_sel, brix) }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$promocionados_est4 <- DT::renderDT({ input$btn_confirm_st4; df <- db_get_selected_clones(con, "st4"); req(nrow(df) > 0); df %>% select(cruce, num_sel, brix) }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    
    # --- Evaluación de Familias ---
    datos_evf <- reactive({
      req(input$file_evf)
      
      tryCatch({
        target_dir <- file.path("data/storage", input$up_evf_year, "EVF")
        if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
        
        file_name <- paste0(input$up_evf_year, "_EVF_", input$up_evf_prog, ".xlsx")
        file.copy(input$file_evf$datapath, file.path(target_dir, file_name), overwrite = TRUE)
        
        df <- readxl::read_excel(input$file_evf$datapath) %>% 
          janitor::clean_names()
          
        req_cols_evf <- c("cruce", "tca", "rend", "experimento")
        missing_cols <- setdiff(req_cols_evf, names(df))
        if (length(missing_cols) > 0) {
          showNotification(
            paste("El archivo EVF no tiene las columnas requeridas:", paste(missing_cols, collapse = ", ")),
            type = "error", duration = 10
          )
          return(NULL)
        }
        
        df <- df %>% filter(!is.na(cruce))
        cols_num <- c("tca", "rend", "tsa")
        df <- df %>%
          mutate(across(any_of(cols_num), ~ as.numeric(as.character(.x))))
        
        if (!"tsa" %in% names(df) && all(c("tca", "rend") %in% names(df))) {
          df <- df %>% mutate(tsa = (tca * rend) / 100)
        }
        
        testigos <- df %>%
          group_by(experimento) %>%
          summarise(
            tca_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(tca[cruce == "CR9303"], na.rm=T), mean(tca, na.rm=T)),
            rend_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(rend[cruce == "CR9303"], na.rm=T), mean(rend, na.rm=T)),
            tsa_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(tsa[cruce == "CR9303"], na.rm=T), mean(tsa, na.rm=T)),
            .groups = "drop"
          ) %>%
          mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | .x == 0, 1, .x)))
        
        df_procesado <- df %>%
          left_join(testigos, by = "experimento") %>%
          mutate(
            indice_y = (tca / tca_testigo) * 100,
            indice_q = (rend / rend_testigo) * 100,
            indice_tsa = (tsa / tsa_testigo) * 100
          ) %>%
          mutate(across(where(is.numeric), ~ round(as.numeric(.x), 2)))
        
        if (!"accion" %in% names(df_procesado)) {
          df_procesado$accion <- "Pendiente"
        }
        
        return(df_procesado)
        
      }, error = function(e) {
        showNotification(paste("Error procesando Excel:", e$message), type = "error")
        return(NULL)
      })
    })
    
    output$tabla_evf <- DT::renderDT({
      req(datos_evf())
      DT::datatable(datos_evf(), 
                    options = list(pageLength = 10, scrollX = TRUE),
                    filter = 'top',
                    selection = 'multiple') %>%
        DT::formatStyle(
          'indice_tsa',
          backgroundColor = DT::styleInterval(c(90, 110), c('#fadbd8', '#fdebd0', '#d5f5e3'))
        )
    })
    
    observeEvent(input$btn_auto_select, {
      req(datos_evf())
      df <- datos_evf()
      rows_to_select <- which(df$indice_tsa >= input$threshold_tsa & 
                              df$indice_q >= input$threshold_r96 & 
                              df$indice_y >= input$threshold_tca)
      
      if (length(rows_to_select) > 0) {
        DT::selectRows(DT::dataTableProxy("tabla_evf"), rows_to_select)
        showNotification(paste("Se han pre-seleccionado", length(rows_to_select), "familias Élite."), type = "warning")
      } else {
        showNotification("Ninguna familia cumple con los umbrales actuales.", type = "error")
      }
    })
    
    observeEvent(input$btn_confirm_evf, {
      req(datos_evf())
      df_completo <- datos_evf()
      idx_sel <- input$tabla_evf_rows_selected
      
      df_final <- df_completo %>%
        mutate(
          accion = ifelse(row_number() %in% idx_sel, "S", "R"),
          anio = input$up_evf_year,
          anio_cruce = input$up_evf_year_cruce,
          programa = input$up_evf_prog
        )
      
      db_save_evf_selection(con, df_final)
      n_s <- sum(df_final$accion == "S")
      n_r <- sum(df_final$accion == "R")
      
      showNotification(paste("Procesadas", nrow(df_final), "familias (S:", n_s, ", R:", n_r, "). Historial actualizado."), 
                       type = "message")
    })
    

    # ==========================================================================
    # --- Dinamización de Analítica por Estado (Bucle Automático lapply) ---
    # ==========================================================================
    stages_info <- list(
      list(key = "EVF", id = "evf", tbl = "familias_evf"),
      list(key = "ST1", id = "st1", tbl = "clones_st1"),
      list(key = "ST2", id = "st2", tbl = "clones_st2"),
      list(key = "ST3", id = "st3", tbl = "clones_st3"),
      list(key = "ST4", id = "st4", tbl = "clones_st4"),
      list(key = "ST5", id = "st5", tbl = "clones_st5")
    )

    lapply(stages_info, function(stg) {
      stage_key <- stg$key
      stage_id  <- stg$id
      tbl_name  <- stg$tbl
      
      # 0.5 Selectores en cascada para ST4/ST5: Año → División → Suelo → Experimento
      if (stage_key %in% c("ST4", "ST5")) {

        # Nivel 1: Año cambia → actualizar Divisiones disponibles
        observe({
          year_val <- input[[paste0("rpt_year_", stage_id)]]
          req(year_val)
          df <- tryCatch(
            dbGetQuery(con,
              "SELECT DISTINCT UPPER(ambiente) as amb FROM ensayos_avanzados WHERE etapa = ? AND ano_zafra = ?",
              params = list(stage_key, year_val)),
            error = function(e) NULL)
          divs <- if (!is.null(df) && nrow(df) > 0)
            c("Todos", sort(unique(df$amb[!is.na(df$amb)]))) else c("Todos")
          updateSelectInput(session, paste0("rpt_div_",  stage_id), choices = divs,  selected = "Todos")
          updateSelectInput(session, paste0("rpt_soil_", stage_id), choices = c("Todos", "BUENO", "MAL_DRENADO", "ROCOSO"), selected = "Todos")
          updateSelectInput(session, paste0("rpt_exp_",  stage_id), choices = c("Todos"), selected = "Todos")
        })

        # Nivel 2: Año + División cambian → actualizar Suelos disponibles
        observe({
          year_val <- input[[paste0("rpt_year_", stage_id)]]
          div_val  <- input[[paste0("rpt_div_",  stage_id)]]
          req(year_val)
          q_conds  <- c("etapa = ?", "ano_zafra = ?")
          q_params <- list(stage_key, year_val)
          if (!is.null(div_val) && div_val != "Todos") {
            q_conds  <- c(q_conds, "UPPER(ambiente) = ?")
            q_params <- c(q_params, list(toupper(div_val)))
          }
          df <- tryCatch(
            dbGetQuery(con,
              paste("SELECT DISTINCT UPPER(suelo) as suelo FROM ensayos_avanzados WHERE",
                    paste(q_conds, collapse = " AND ")),
              params = q_params),
            error = function(e) NULL)
          soils <- if (!is.null(df) && nrow(df) > 0)
            c("Todos", sort(unique(df$suelo[!is.na(df$suelo)]))) else c("Todos")
          updateSelectInput(session, paste0("rpt_soil_", stage_id), choices = soils, selected = "Todos")
          updateSelectInput(session, paste0("rpt_exp_",  stage_id), choices = c("Todos"), selected = "Todos")
        })

        # Nivel 3: Año + División + Suelo cambian → actualizar Experimentos disponibles
        observe({
          year_val <- input[[paste0("rpt_year_", stage_id)]]
          div_val  <- input[[paste0("rpt_div_",  stage_id)]]
          soil_val <- input[[paste0("rpt_soil_", stage_id)]]
          req(year_val)
          q_conds  <- c("etapa = ?", "ano_zafra = ?")
          q_params <- list(stage_key, year_val)
          if (!is.null(div_val)  && div_val  != "Todos") {
            q_conds  <- c(q_conds, "UPPER(ambiente) = ?")
            q_params <- c(q_params, list(toupper(div_val)))
          }
          if (!is.null(soil_val) && soil_val != "Todos") {
            q_conds  <- c(q_conds, "UPPER(suelo) = ?")
            q_params <- c(q_params, list(toupper(soil_val)))
          }
          df <- tryCatch(
            dbGetQuery(con,
              paste("SELECT DISTINCT num_experimento FROM ensayos_avanzados WHERE",
                    paste(q_conds, collapse = " AND ")),
              params = q_params),
            error = function(e) NULL)
          exps <- if (!is.null(df) && nrow(df) > 0)
            c("Todos", sort(unique(df$num_experimento[!is.na(df$num_experimento)]))) else c("Todos")
          updateSelectInput(session, paste0("rpt_exp_", stage_id), choices = exps, selected = "Todos")
        })
      }


      # 1. EventReactive local para esta etapa (Tabla y KPIs)
      rpt_data_local <- eventReactive(list(
        input[[paste0("btn_gen_rpt_", stage_id)]],
        if (stage_key %in% c("ST4","ST5")) input[[paste0("rpt_exp_", stage_id)]] else NULL,
        if (stage_key %in% c("ST4","ST5")) input[[paste0("rpt_div_", stage_id)]] else NULL,
        if (!is.null(db_trigger)) db_trigger() else NULL
      ), {
        year_val <- input[[paste0("rpt_year_", stage_id)]]
        req(year_val)
        
        prog_val <- input[[paste0("rpt_prog_", stage_id)]]
        soil_val <- if (stage_key != "EVF") input[[paste0("rpt_soil_", stage_id)]] else NULL
        
        if (stage_key == "EVF") {
          df <- tryCatch({
            dbGetQuery(con, "SELECT anio as anio_seleccion, cruce, madre, padre, tsa as brix, rend, tca, accion, programa FROM familias_evf WHERE anio = ?", params = list(year_val))
          }, error = function(e) NULL)
          
          if (!is.null(df) && nrow(df) > 0 && !is.null(prog_val) && prog_val != "Todos") {
            df <- df %>% filter(toupper(programa) == toupper(prog_val))
          }
        } else if (stage_key %in% c("ST4", "ST5")) {
          # Leer todos los valores de filtro al momento de ejecutar
          exp_val  <- input[[paste0("rpt_exp_",  stage_id)]]
          div_val  <- input[[paste0("rpt_div_",  stage_id)]]

          # Construir la query con todos los filtros en SQL (evita estado inconsistente)
          q_conds  <- c("etapa = ?", "ano_zafra = ?")
          q_params <- list(stage_key, year_val)

          if (!is.null(div_val)  && div_val  != "Todos") {
            q_conds  <- c(q_conds,  "UPPER(ambiente) = ?")
            q_params <- c(q_params, list(toupper(div_val)))
          }
          if (!is.null(exp_val)  && exp_val  != "Todos") {
            q_conds  <- c(q_conds,  "num_experimento = ?")
            q_params <- c(q_params, list(exp_val))
          }
          if (!is.null(soil_val) && soil_val != "Todos") {
            q_conds  <- c(q_conds,  "UPPER(suelo) = ?")
            q_params <- c(q_params, list(toupper(soil_val)))
          }

          query <- paste(
            "SELECT variedad as Variedad, tca as TCA, rendimiento as Rendimiento,",
            "tsh as TSH, num_experimento as origen, ambiente,",
            "corte_nombre, num_replica, fcosecha,",
            "ind_testigo, suelo as Suelo, brix as Brix,",
            "ano_zafra as anio_seleccion, substr(variedad, 1, 2) as programa",
            "FROM ensayos_avanzados WHERE",
            paste(q_conds, collapse = " AND ")
          )
          df <- tryCatch(
            dbGetQuery(con, query, params = q_params),
            error = function(e) { print(e); NULL }
          )
        } else {
          df <- tryCatch({
            dbGetQuery(con, sprintf("SELECT * FROM %s WHERE anio_seleccion = ?", tbl_name), params = list(year_val))
          }, error = function(e) NULL)
          
          if (!is.null(df) && nrow(df) > 0) {
            if (!is.null(prog_val) && prog_val != "Todos") {
              df <- df %>% filter(toupper(programa) == toupper(prog_val))
            }
            if (!is.null(soil_val) && soil_val != "Todos") {
              df <- df %>% filter(toupper(suelo) == toupper(soil_val))
            }
          }
        }
        
        shiny::validate(need(!is.null(df) && is.data.frame(df) && nrow(df) > 0, "No hay datos analíticos para los filtros seleccionados."))
        df
      })
      
      # 2. UI Resumen de Métricas (Value Boxes Locales)
      output[[paste0("ui_vbox_", stage_id)]] <- renderUI({
        df <- rpt_data_local()
        total <- nrow(df)
        
        if (stage_key %in% c("ST4", "ST5")) {
          df_testigos <- df %>% filter(ind_testigo == "S")
          total_testigos <- nrow(df_testigos)
          rend_avg <- mean(as.numeric(df$Rendimiento), na.rm=TRUE)
          taa_avg <- mean(as.numeric(df$TSH), na.rm=TRUE)
          
          layout_column_wrap(
            width = 1/4,
            value_box("Total Variedades", total, icon("flask"), theme="info"),
            value_box("Total Testigos", total_testigos, icon("shield-alt"), theme="primary"),
            value_box("Promedio Rend.", paste0(round(rend_avg, 2), "%"), icon("percent"), theme="success"),
            value_box("Promedio TAA/TSH", round(taa_avg, 2), icon("leaf"), theme="warning")
          )
        } else {
          sel   <- sum(df$accion == "S", na.rm=TRUE)
          tasa  <- if (total > 0) (sel / total) * 100 else 0
          df_sel <- df %>% filter(accion == "S")
          brix_avg <- if (nrow(df_sel) > 0) mean(as.numeric(df_sel$brix), na.rm = TRUE) else 0
          metric_label <- ifelse(stage_key == "EVF", "Promedio TSA", "Promedio Brix")
          
          layout_column_wrap(
            width = 1/4,
            value_box("Total Evaluación", total, icon("users"), theme="info"),
            value_box("Seleccionados", sel, icon("check-double"), theme="success"),
            value_box("% Selección", paste0(round(tasa, 1), "%"), icon("percentage"), theme="warning"),
            value_box(metric_label, round(brix_avg, 2), icon("tint"), theme="primary")
          )
        }
      })
      
      # 2.5 Data Especial para Curvas ISO (Solo ST4 y ST5) -> Eliminado, usamos rpt_data_local()
      
      # 2.6 Zoom variables
      ranges_iso <- reactiveValues(x = NULL, y = NULL)
      
      observeEvent(input[[paste0("iso_dblclick_", stage_id)]], {
        brush <- input[[paste0("iso_brush_", stage_id)]]
        if (!is.null(brush)) {
          ranges_iso$x <- c(brush$xmin, brush$xmax)
          ranges_iso$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges_iso$x <- NULL
          ranges_iso$y <- NULL
        }
      })

      # 3. Gráfico 1: Caja comparando distribución de brix o Curva ISO
      if (stage_key %in% c("ST4", "ST5")) {
        output[[paste0("plot_iso_", stage_id)]] <- renderPlot({
          df <- rpt_data_local()
          shiny::validate(need(!is.null(df) && nrow(df) > 0, "No hay datos de laboratorio analítico para los filtros seleccionados."))
          
          df$TCA <- suppressWarnings(as.numeric(df$TCA))
          df$Rendimiento <- suppressWarnings(as.numeric(df$Rendimiento))
          df$TSH <- suppressWarnings(as.numeric(df$TSH))
          
          df <- df %>% filter(!is.na(TCA) & !is.na(Rendimiento))
          shiny::validate(need(nrow(df) > 0, "Los registros no contienen valores numéricos válidos en TCA o Rendimiento."))
          
          testigo_base <- input[[paste0("iso_testigo_", stage_id)]]
          testigos_vec <- c()
          if (!is.null(testigo_base)) {
            if (testigo_base != "Ninguno" && testigo_base != "Todos") {
              testigos_vec <- c(testigos_vec, testigo_base)
            } else if (testigo_base == "Todos") {
              t_df <- df %>% filter(ind_testigo == "S")
              testigos_vec <- unique(t_df$Variedad)
            }
          }
          
          grad <- input[[paste0("iso_grad_", stage_id)]]
          if (is.null(grad) || grad == "Default") grad <- "Vice City"
          
          bins_val <- input[[paste0("iso_bins_", stage_id)]]
          if(is.null(bins_val)) bins_val <- 12
          
          fill_val <- input[[paste0("iso_fill_", stage_id)]]
          if(is.null(fill_val)) fill_val <- TRUE
          
          create_iso_plot(
            media_df = df,
            testigos = testigos_vec,
            colors_input = "", 
            x_min = if(!is.null(ranges_iso$x)) ranges_iso$x[1] else NA,
            x_max = if(!is.null(ranges_iso$x)) ranges_iso$x[2] else NA,
            y_min = if(!is.null(ranges_iso$y)) ranges_iso$y[1] else NA,
            y_max = if(!is.null(ranges_iso$y)) ranges_iso$y[2] else NA,
            use_filled_contour = fill_val,
            selected_gradient = grad,
            bins = bins_val,
            plot_title = paste("Isoproductividad -", stage_key)
          )
        })
      } else {
        output[[paste0("plot_brix_comp_", stage_id)]] <- plotly::renderPlotly({
        df <- rpt_data_local()
        shiny::validate(need(nrow(df) > 0, "Cargando distribución..."))
        
        df_plot <- df %>%
          mutate(
            Estado = case_when(
              accion == "S" ~ "Selección",
              accion == "T" ~ "Testigos",
              TRUE          ~ "Rechazos"
            )
          )
        
        metric_title <- ifelse(stage_key == "EVF", "TSA (Azúcar)", "Brix (%)")
        
        plotly::plot_ly(df_plot, y = ~brix, x = ~Estado, type = "box", color = ~Estado,
                        colors = c("#27ae60", "#f1c40f", "#c0392b")) %>%
          plotly::layout(
            yaxis = list(title = metric_title),
            xaxis = list(title = ""),
            showlegend = FALSE,
            margin = list(t = 20, b = 20, l = 40, r = 20)
          )
      })
      
      # 4. Gráfico 2: Top 10 Cruces con Mayor Selección
      output[[paste0("plot_top_cruces_", stage_id)]] <- plotly::renderPlotly({
        df <- rpt_data_local() %>% filter(accion == "S")
        shiny::validate(need(nrow(df) > 0, "No hay selecciones suficientes en este lote para graficar."))
        
        top_df <- df %>%
          group_by(cruce) %>%
          summarise(n = n(), .groups = 'drop') %>%
          arrange(desc(n)) %>%
          head(10)
        
        plotly::plot_ly(top_df, x = ~reorder(cruce, -n), y = ~n, type = "bar",
                        marker = list(color = "#3498db", line = list(color = "#2980b9", width = 1))) %>%
          plotly::layout(
            xaxis = list(title = "Cruce / Familia"),
            yaxis = list(title = "Seleccionados"),
            margin = list(t = 20, b = 20, l = 40, r = 20)
          )
      })
      }
      
      # 5. Tabla de Lista de Corte Local
      output[[paste0("tabla_corte_", stage_id)]] <- DT::renderDT({
        df <- rpt_data_local()
        if (stage_key %in% c("ST4", "ST5")) {
          df_show <- df %>% select(
            `Año`        = anio_seleccion,
            Variedad,
            Programa     = programa,
            División     = ambiente,
            Suelo,
            Experimento  = origen,
            Corte        = corte_nombre,
            Réplicas     = num_replica,
            `F. Cosecha` = fcosecha,
            Rendimiento,
            TCA,
            TSH,
            Brix,
            Testigo      = ind_testigo
          )
          DT::datatable(df_show, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
        } else if (stage_key == "EVF") {
          df <- df %>% filter(accion == "S")
          DT::datatable(df %>% select(Cruce = cruce, Madre = madre, Padre = padre, TSA = brix, Rendimiento = rend, TCA = tca), 
                        options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
        } else {
          df <- df %>% filter(accion == "S")
          df_show <- df %>% select(Cruce = cruce, Clon = num_sel, Programa = programa, Brix = brix, `Grado Agronómico` = vigor, Suelo = suelo,
                                    Origen = any_of("origen"))
          DT::datatable(df_show, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
        }
      })
      
      # 6. Descarga Excel Localizada
      output[[paste0("btn_dw_rpt_", stage_id)]] <- downloadHandler(
        filename = function() { paste0("Lista_Corte_", stage_key, "_", input[[paste0("rpt_year_", stage_id)]], ".xlsx") },
        content = function(file) {
          df <- rpt_data_local()
          if (stage_key %in% c("ST4", "ST5")) {
            df_dl <- df %>% select(Año = anio_seleccion, Variedad, Programa = programa, Suelo, Experimento = origen, Rendimiento, TCA, TSH, Brix)
          } else if (stage_key == "EVF") {
            df <- df %>% filter(accion == "S")
            df_dl <- df %>% select(Cruce = cruce, Madre = madre, Padre = padre, Programa = programa, TSA = brix, Rendimiento = rend, TCA = tca)
          } else {
            df <- df %>% filter(accion == "S")
            df_dl <- df %>% select(Cruce = cruce, Clon = num_sel, Programa = programa, Brix = brix, `Grado Agronómico` = vigor, Suelo = suelo,
                                    Origen = any_of("origen"))
          }
          openxlsx::write.xlsx(df_dl, file)
        }
      )
    })
    # Variable reactiva para forzar actualización de tabla field_captures
    rv_refresh_fc <- reactiveVal(0)

    # ==========================================================================
    # --- CAPTURA DE CAMPO (API) — field_captures ---
    # ==========================================================================
    fc_data <- eventReactive(
      list(input$btn_fc_refresh, if (!is.null(db_trigger)) db_trigger() else NULL, rv_refresh_fc()),
      ignoreNULL = FALSE, ignoreInit = FALSE,

      {
        df <- tryCatch(dbReadTable(con, "field_captures"), error = function(e) data.frame())
        if (nrow(df) == 0) return(df)
        df %>% mutate(
          brix  = suppressWarnings(as.numeric(brix)),
          vigor = suppressWarnings(as.integer(vigor)),
          ts_fmt = format(
            as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
            "%d/%m/%Y %H:%M", tz = "America/Santo_Domingo"
          )
        )
      }
    )
    
    fc_filtered <- reactive({
      df <- fc_data()
      if (nrow(df) == 0) return(df)
      if (!is.null(input$fc_etapa)  && input$fc_etapa  != "Todas")
        df <- df %>% filter(toupper(etapa)    == toupper(input$fc_etapa))
      if (!is.null(input$fc_prog)   && input$fc_prog   != "Todos")
        df <- df %>% filter(toupper(programa) == toupper(input$fc_prog))
      if (!is.null(input$fc_accion) && input$fc_accion != "Todas")
        df <- df %>% filter(accion == input$fc_accion)
      if (!is.null(input$fc_suelo)  && input$fc_suelo  != "Todos")
        df <- df %>% filter(toupper(suelo) == toupper(input$fc_suelo))
      df
    })
    
    observe({
      df <- fc_data()
      if (nrow(df) == 0) return()
      updateSelectInput(session, "fc_prog",
                        choices = c("Todos", sort(unique(df$programa))))
    })
    
    output$ui_fc_stats <- renderUI({
      df <- fc_filtered()
      total <- nrow(df)
      sel   <- sum(df$accion == "S", na.rm = TRUE)
      test  <- sum(df$accion == "T", na.rm = TRUE)
      rej   <- sum(df$accion == "R", na.rm = TRUE)
      b_avg <- if (sel > 0) round(mean(df$brix[df$accion == "S"], na.rm = TRUE), 1) else 0
      layout_column_wrap(
        width = 1/5,
        value_box("Total Registros",  total, icon("seedling"),     theme = "info"),
        value_box("Seleccionados",    sel,   icon("check-double"), theme = "success"),
        value_box("Testigos",         test,  icon("star"),         theme = "warning"),
        value_box("Rechazados",       rej,   icon("times-circle"), theme = "danger"),
        value_box("Brix Prom. (Sel)", b_avg, icon("tint"),         theme = "primary")
      )
    })
    
    output$tabla_fc <- DT::renderDT({
      df <- fc_filtered()
      shiny::validate(need(
        nrow(df) > 0,
        "No hay registros de campo. Sincroniza desde la app móvil."
      ))
      df_show <- df %>% select(
        Fecha     = ts_fmt,
        Etapa     = etapa,
        Programa  = programa,
        Suelo     = suelo,
        Cruce     = cruce,
        `No. Sel` = num_sel,
        Brix      = brix,
        Vigor     = vigor,
        Decision  = accion,
        Evaluador = evaluador,
        Lat       = if("latitud" %in% names(df)) latitud else NA,
        Lon       = if("longitud" %in% names(df)) longitud else NA
      )
      DT::datatable(
        df_show,
        selection = "multiple",
        filter    = "top",
        rownames  = FALSE,
        options   = list(pageLength = 20, scrollX = TRUE,
                         order = list(list(0, "desc")))
      ) %>%
        DT::formatStyle(
          "Decision",
          backgroundColor = DT::styleEqual(
            c("S",       "T",       "R"),
            c("#d5f5e3", "#fdebd0", "#fadbd8")
          ),
          fontWeight = "bold"
        ) %>%
        DT::formatStyle(
          "Brix",
          background = DT::styleColorBar(c(10, 30), "#aed6f1")
        )
    })
    
    observeEvent(input$btn_fc_promote, {
      rows <- input$tabla_fc_rows_selected
      shiny::req(length(rows) > 0)
      df_all  <- fc_filtered()
      df_sel  <- df_all[rows, ]
      df_save <- df_sel %>%
        transmute(
          anio_seleccion = as.integer(anio_seleccion),
          anio_cruce     = as.integer(anio_cruce),
          programa       = programa,
          suelo          = suelo,
          cruce          = cruce,
          num_sel        = as.character(num_sel),
          brix           = brix,
          vigor          = as.integer(vigor),
          accion         = accion,
          origen         = "CAMPO_APP",
          evaluador      = evaluador,
          fecha_evaluacion = if("ts" %in% names(df_sel)) ts else as.character(Sys.time()),
          latitud        = if("latitud" %in% names(df_sel)) latitud else NA_real_,
          longitud       = if("longitud" %in% names(df_sel)) longitud else NA_real_
        )
      tryCatch({
        dbWriteTable(con, "clones_st1", df_save, append = TRUE, row.names = FALSE)
        
        # Link: Automatically update cross status in registro_cruces
        cruces_promovidos <- unique(df_save$cruce)
        if (length(cruces_promovidos) > 0) {
          cruces_str <- paste(sprintf("'%s'", cruces_promovidos), collapse = ",")
          DBI::dbExecute(con, sprintf("UPDATE registro_cruces SET estado = 'EN_EVALUACION' WHERE cruce IN (%s) AND estado != 'EN_EVALUACION'", cruces_str))
        }
        
        showNotification(
          paste(nrow(df_save), "registro(s) promovidos a ST1 con origen CAMPO_APP."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error al promover:", e$message), type = "error")
      })
    })
    
    # --- Eliminación de Capturas de Campo ---
    observeEvent(input$btn_fc_eliminar, {
      rows <- input$tabla_fc_rows_selected
      if (length(rows) == 0) {
        showNotification("Seleccione al menos un registro para eliminar.", type = "warning")
        return()
      }
      
      ask_confirmation(
        inputId = ns("confirm_fc_delete"),
        title = "Eliminar Registros de Campo",
        text = sprintf("¿Está seguro de que desea eliminar permanentemente %d registro(s)? Esta acción no se puede deshacer.", length(rows)),
        type = "error",
        btn_labels = c("Cancelar", "Eliminar"),
        btn_colors = c("#bdc3c7", "#e74c3c")
      )
    })
    
    observeEvent(input$confirm_fc_delete, {
      req(isTRUE(input$confirm_fc_delete))
      rows <- input$tabla_fc_rows_selected
      req(length(rows) > 0)
      
      df_actual <- fc_filtered()
      ids_to_delete <- df_actual$id[rows]
      
      tryCatch({
        ids_str <- paste(ids_to_delete, collapse = ",")
        DBI::dbExecute(con, sprintf("DELETE FROM field_captures WHERE id IN (%s)", ids_str))
        
        showNotification(sprintf("%d registro(s) eliminados correctamente.", length(ids_to_delete)), type = "message")
        
        # Limpiar selección y refrescar tabla
        DT::dataTableProxy(ns("tabla_fc")) %>% DT::selectRows(NULL)
        rv_refresh_fc(rv_refresh_fc() + 1)
      }, error = function(e) {
        showNotification(paste("Error al eliminar registros:", e$message), type = "error")
      })
    })
    
    output$dl_fc <- downloadHandler(
      filename = function() paste0("Captura_Campo_", Sys.Date(), ".csv"),
      content  = function(file) {
        write.csv(fc_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}
