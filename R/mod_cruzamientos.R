# ==============================================================================
# MOD_CRUZAMIENTOS.R — Módulo Shiny: Recomendación de Cruzamientos
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Sugiere cruzamientos óptimos basado en:
#   1. Consanguinidad (F) — evitar depresión por consanguinidad
#   2. Rendimiento de los padres — TCH, TAH, scores agronómicos
#   3. Sanidad de los padres — resistencia a enfermedades
# ==============================================================================

# --- UI del Módulo ---
mod_cruzamientos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS para calendario de fechas
    tags$head(tags$style(HTML("
      .fecha-selector-wrap .air-datepicker {
        border: none !important;
        box-shadow: none !important;
        width: 100% !important;
      }
      .air-datepicker-cell.-has-data- {
        background: #bbf7d0 !important;
        color: #15803d !important;
        font-weight: 700 !important;
        border-radius: 50% !important;
      }
      .air-datepicker-cell.-has-data-.-selected- {
        background: #16a34a !important;
        color: white !important;
      }
      .fecha-info-box {
        display: flex;
        gap: 8px;
        margin-top: 8px;
        padding: 8px 10px;
        border-radius: 8px;
        background: #f0fdf4;
        border: 1px solid #bbf7d0;
        font-size: 12px;
        align-items: center;
      }
      .fecha-info-box .fi-date { font-weight: 700; color: #15803d; }
      .fecha-info-box .fi-badge {
        background: #dcfce7;
        color: #166534;
        padding: 1px 7px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 600;
      }
      .fecha-vacia {
        text-align: center;
        color: #94a3b8;
        font-size: 12px;
        padding: 10px;
        border: 1px dashed #cbd5e1;
        border-radius: 8px;
        margin-top: 6px;
      }
    "))),

    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tagList(icon("sliders-h"), " Configuración"),
        
        # Selector de Fechas - Calendario con fechas resaltadas
        tags$div(
          class = "fecha-selector-wrap mb-2",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$span(
              icon("calendar-check"), " ",
              tags$strong("Evaluaciones Disponibles"),
              class = "text-dark"
            ),
            actionButton(ns("btn_refresh_fechas"), NULL,
                         icon  = icon("rotate"),
                         class = "btn-outline-secondary btn-sm",
                         title = "Refrescar")
          ),
          # Calendario con fechas resaltadas
          uiOutput(ns("ui_calendar")),
          # Info de la fecha seleccionada
          uiOutput(ns("ui_fecha_info"))
        ),
        
        hr(),
        
        # El modo teórico ha sido eliminado por solicitud del usuario para simplificar la app
        hidden(
          selectizeInput(
            ns("pool_madres"), "Pool de Madres:",
            choices = NULL, multiple = TRUE
          ),
          radioButtons(ns("modo_plan"), "Modo de Planificación:",
                       choices = c("Operativo (Sincronización Floral)"),
                       selected = "Operativo (Sincronización Floral)")
        ),
        
        accordion(
          open = FALSE,
          accordion_panel(
            "Filtros de Adaptación",
            icon = icon("leaf"),
            selectInput(ns("filtro_suelo"), "Tipo de Suelo Objetivo:",
                        choices = c("Cualquiera", "BUENO", "MAL_DRENADO", "ROCOSO"),
                        selected = "Cualquiera")
          ),
          accordion_panel(
            "Pesos del Modelo",
            icon = icon("balance-scale"),
            sliderInput(ns("limit_f"), "Máxima Consanguinidad (F):",
                        min = 0, max = 0.25, value = 0.0625, step = 0.005),
            sliderInput(ns("w_genetic"), "Peso: Diversidad (1-F)",
                        min = 0, max = 1, value = 0.3, step = 0.05),
            sliderInput(ns("w_factor"), "Peso: Valor (FACTOR)",
                        min = 0, max = 1, value = 0.7, step = 0.05),
            uiOutput(ns("peso_feedback"))
          ),
          accordion_panel(
            "Seguridad",
            icon = icon("shield-alt"),
            checkboxInput(ns("solo_evitar_directos"), "Omitir cálculo de F", value = FALSE),
            numericInput(ns("top_n"), "Top N cruces:", value = 50, min = 10, max = 500)
          )
        ),
        
        hr(),
        actionButton(ns("btn_simular"), "Simular Cruzamientos",
                     class = "btn-primary w-100 mb-2",
                     icon = icon("vials")),
        
        downloadButton(ns("btn_export"), "Exportar Resumen",
                       class = "btn-outline-info w-100 mb-3"),
        
        hr(),
        tags$h6("Herramientas de Campo", class = "text-muted"),
        numericInput(ns("num_lanterna_start"), "Nº Inicial Policruce:", value = 1, min = 1),
        downloadButton(ns("btn_export_campo"), "Hoja de Montaje",
                       class = "btn-success w-100")
      ),
      
      # Panel de resultados con TABS para separar lógicas
      card(
        card_header(tagList(icon("vials"), " Recomendaciones de Mejoramiento")),
        navset_card_pill(
          id = ns("tabs_cruces"),
          nav_panel(
            title = "Biparentales",
            icon = icon("dna"),
            uiOutput(ns("vboxes_resultados")),
            hr(),
            layout_column_wrap(
              width = 1/2,
              actionButton(ns("btn_reg_biparental"), "Registrar Biparentales Seleccionados", class="btn-success mb-2", icon=icon("save")),
              actionButton(ns("btn_ver_radar"), "Ver Análisis (Radar)", class="btn-info mb-2 text-white", icon=icon("chart-pie"))
            ),
            DT::DTOutput(ns("tabla_cruces"))
          ),
          nav_panel(
            title = "Policruces",
            icon = icon("layer-group"),
            div(id = ns("panel_lanternas"),
                p(tags$b("Nota: "), "El sistema utiliza un ratio de 1:2 en FLORES (mínimo 2 flores por hembra)."),
                actionButton(ns("btn_reg_policruce"), "Registrar Policruces Seleccionados", class="btn-success mb-2", icon=icon("save")),
                DT::DTOutput(ns("tabla_lanternas"))
            )
          ),
          nav_panel(
            title = "Registro de Cruces",
            icon = icon("book"),
            layout_column_wrap(
              width = 1,
              fill  = FALSE,
              
              # ── Value boxes ──────────────────────────────────────────────────────────
              uiOutput(ns("ui_rc_stats")),
              
              # ── Actualizar germinación (FIJO arriba del formulario) ──────
              card(
                class = "border-0 shadow-sm",
                card_header(tagList(icon("seedling"), " Actualizar Germinación"), class = "bg-light"),
                layout_column_wrap(
                  width = 1/3,
                  numericInput(ns("rc_germ_id"),       "ID del cruce:", value = NA, min = 1),
                  numericInput(ns("rc_germ_cantidad"), "Semillas germinadas:", value = NA, min = 0),
                  div(style = "padding-top: 24px;",
                      actionButton(ns("btn_rc_germ"), "Actualizar",
                                   icon = icon("check"), class = "btn-primary w-100"))
                )
              ),
              
              # ── Formulario de registro ────────────────────────────────────────
              card(
                card_header(tagList(icon("plus-circle"), " Registrar Cruce Ejecutado")),
                layout_column_wrap(
                  width = 1/3,
                  div(
                    tags$label("Madre", class = "form-label fw-bold"),
                    selectizeInput(ns("rc_madre"), NULL, choices = NULL, multiple = TRUE,
                                   options = list(create = FALSE,
                                                  placeholder = "Buscar madre..."))
                  ),
                  div(
                    tags$label("Padre", class = "form-label fw-bold"),
                    selectizeInput(ns("rc_padre"), NULL, choices = NULL, multiple = TRUE,
                                   options = list(create = FALSE,
                                                  placeholder = "Buscar padre..."))
                  ),
                  div(
                    tags$label("Fecha del Cruce", class = "form-label fw-bold"),
                    dateInput(ns("rc_fecha"), NULL, value = Sys.Date(),
                              format = "dd/mm/yyyy", language = "es")
                  )
                ),
                layout_column_wrap(
                  width = 1/4,
                  selectInput(ns("rc_tipo"), "Tipo de Cruce:",
                              choices = c("Biparental", "Policruce")),
                  selectInput(ns("rc_suelo"), "Suelo:",
                              choices = c("BUENO","MAL_DRENADO","ROCOSO")),
                  numericInput(ns("rc_anio"), "Año Cruce:",
                               value = as.integer(format(Sys.Date(), "%Y")),
                               min = 2000, max = 2100),
                  numericInput(ns("rc_semillas"), "Semillas cosechadas:",
                               value = NA, min = 0)
                ),
                textAreaInput(ns("rc_notas"), "Notas (opcional):",
                              placeholder = "Observaciones del cruce...",
                              rows = 2),
                uiOutput(ns("ui_rc_alerta_repetido")),
                actionButton(ns("btn_rc_guardar"), "Guardar Cruce",
                             icon = icon("save"), class = "btn-success w-100 mt-2")
              ),
              
              # ── Tabla de cruces registrados ──────────────────────────────────
              card(
                card_header(
                  layout_column_wrap(
                    width = 1/2,
                    tags$div(
                      class = "d-flex align-items-center",
                      icon("table"), tags$span(" Cruces Registrados", class = "ms-2 me-3"),
                      actionButton(ns("btn_rc_eliminar"), "Eliminar Seleccionados", class="btn-danger btn-sm me-2", icon=icon("trash")),
                      actionButton(ns("btn_print_cruces"), "🖨️ Hoja de Cruces", class="btn-outline-primary btn-sm")
                    ),

                    layout_column_wrap(
                      width = 1/4,
                      selectInput(ns("rc_filter_anio"),   "Año:",    choices = c("Todos")),
                      selectInput(ns("rc_filter_tipo"),   "Tipo:",   choices = c("Todos","Biparental","Policruce")),
                      selectInput(ns("rc_filter_suelo"),  "Suelo:",  choices = c("Todos","BUENO","MAL_DRENADO","ROCOSO")),
                      selectInput(ns("rc_filter_estado"), "Estado:", choices = c("Todos","EJECUTADO","GERMINADO","EN_EVALUACION"))
                    )
                  )
                ),
                DT::DTOutput(ns("tabla_rc"))
              )
            )
          ),
           nav_panel(
            title = "Parámetros Genéticos",
            icon = icon("cogs"),
            card(
              card_header("Configuración de Índice de Selección (Smith-Hazel)"),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Heredabilidades (h²)"),
                  sliderInput(ns("h2_y"), "Tonelaje (Y):", min = 0.05, max = 0.8, value = 0.25, step = 0.05),
                  sliderInput(ns("h2_q"), "Calidad (Q):", min = 0.05, max = 0.8, value = 0.45, step = 0.05),
                  sliderInput(ns("h2_s"), "Sanidad (Resistencia):", min = 0.05, max = 0.8, value = 0.60, step = 0.05)
                ),
                card(
                  card_header("Pesos Económicos ($)"),
                  sliderInput(ns("w_y"), "Imp. Tonelaje:", min = 0, max = 1, value = 0.20, step = 0.05),
                  sliderInput(ns("w_q"), "Imp. Calidad:", min = 0, max = 1, value = 0.60, step = 0.05),
                  sliderInput(ns("w_s"), "Imp. Sanidad:", min = 0, max = 1, value = 0.20, step = 0.05)
                )
              ),
              hr(),
              helpText("Nota: Estos parámetros definen cómo se combinan Y y Q para formar el Valor Agroeconómico del cruce.")
            )
          )
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_cruzamientos_server <- function(id, cat_var, pedigree_var, df_ped_wide, df_act2025, df_categorias, ebvs_var = NULL, opciones_parentales = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- Feedback de Pesos ---
    output$peso_feedback <- renderUI({
      suma <- input$w_genetic + input$w_factor
      color <- if (round(suma, 2) == 1) "success" else "danger"
      tags$div(
        class = paste0("text-", color),
        style = "margin-top: -10px; margin-bottom: 15px; font-weight: bold; font-size: 0.9em;",
        paste("Suma actual de pesos:", suma)
      )
    })
    
    # --- Estado Reactivo ---
    hoja_campo_rv <- reactiveVal(NULL)
    full_res_scoring_rv <- reactiveVal(NULL)
    
    # --- Poblar selectize con TODAS las variedades que tienen parentesco ---
    # Obtenemos todos los IDs (numéricos) que aparecen en el pedigree
    ids_con_pedigree <- unique(c(
      as.character(df_ped_wide$id_variedad),
      as.character(df_ped_wide$id_variedad_ancestro)
    ))
    
    # Usamos cat_var para tener TODOS los nombres, y evitamos el df_act2025
    nombres_disponibles <- cat_var %>%
      filter(id_variedad %in% ids_con_pedigree, !is.na(descripcion_variedad), descripcion_variedad != "") %>%
      arrange(descripcion_variedad)
    
    # Queremos que Shiny envíe el NOMBRE (descripcion_variedad) al servidor
    opciones <- setNames(nombres_disponibles$descripcion_variedad,
                         nombres_disponibles$descripcion_variedad)
    
    updateSelectizeInput(session, "pool_madres",
                         choices = opciones, server = TRUE)
    updateSelectizeInput(session, "pool_padres",
                         choices = opciones, server = TRUE)
    
    # --- 1. Calcular Categorías de Mejoramiento (Ahora viene de global.R) ---
    # df_categorias ya está disponible como argumento
    
    
    # --- Matriz A Pre-calculada ---
    # Usamos la matriz calculada en global.R para que la app no se congele
    matriz_A <- reactive({
      GLOBAL_A_MATRIX
    })

    

    # --- Calendario de fechas con datos resaltados ---
    fechas_rv <- reactiveVal(NULL)
    
    cargar_fechas_disponibles <- function() {
      tryCatch({
        con_f <- db_connect("data/breeding_system.db")
        on.exit(dbDisconnect(con_f))
        fechas <- dbGetQuery(con_f,
          "SELECT fecha_chequeo, 
                  MAX(dia_semana) as dia,
                  COUNT(*) as n_vars, 
                  SUM(CASE WHEN grc_emergencia > 0 THEN 1 ELSE 0 END) as n_emerg
           FROM floracion_chequeos
           GROUP BY fecha_chequeo
           ORDER BY fecha_chequeo DESC"
        )
        fechas_rv(if (nrow(fechas) > 0) fechas else NULL)
      }, error = function(e) fechas_rv(NULL))
    }
    
    observe({ cargar_fechas_disponibles() }) |> bindEvent(TRUE, once = TRUE)
    observeEvent(input$btn_refresh_fechas, { cargar_fechas_disponibles() })
    
    # Renderizar el calendario con airDatepickerInput
    output$ui_calendar <- renderUI({
      df <- fechas_rv()
      
      if (is.null(df) || nrow(df) == 0) {
        return(tags$div(class = "fecha-vacia",
          icon("satellite-dish"), " Sin evaluaciones sincronizadas aún."
        ))
      }
      
      fechas_con_datos <- as.Date(df$fecha_chequeo)
      fecha_inicial    <- fechas_con_datos[1]  # La más reciente
      
      # Crear el picker con las fechas resaltadas
      airDatepickerInput(
        inputId          = session$ns("fecha_floracion"),
        label            = NULL,
        value            = fecha_inicial,
        minDate          = min(fechas_con_datos) - 7,
        maxDate          = max(fechas_con_datos) + 7,
        inline           = TRUE,
        language         = "es",
        width            = "100%",
        highlightedDates = fechas_con_datos,
        dateFormat       = "yyyy-MM-dd",
        autoClose        = TRUE
      )
    })
    
    # Info box debajo del calendario mostrando stats de la fecha seleccionada
    output$ui_fecha_info <- renderUI({
      req(input$fecha_floracion)
      df <- fechas_rv()
      if (is.null(df)) return(NULL)
      
      fecha_sel_str <- as.character(input$fecha_floracion)
      fila <- df[df$fecha_chequeo == fecha_sel_str, ]
      
      if (nrow(fila) == 0) {
        return(tags$div(class = "fecha-vacia",
          icon("circle-xmark"), " Sin datos para esta fecha."
        ))
      }
      
      tags$div(
        class = "fecha-info-box",
        icon("calendar-check", style = "color:#16a34a"),
        tags$span(class = "fi-date", fila$fecha_chequeo[1]),
        tags$span(class = "fi-badge", icon("seedling"), " ", fila$n_vars[1], " vars"),
        tags$span(class = "fi-badge", icon("leaf"),    " ", fila$n_emerg[1], " emerg")
      )
    })

    # --- Consulta de Datos de Floración desde SQLite ---

    datos_floracion <- reactive({
      req(input$fecha_floracion)
      
      con <- db_connect("data/breeding_system.db")
      on.exit(dbDisconnect(con))
      
      fecha_str <- as.character(input$fecha_floracion)
      
      q <- sprintf("
        SELECT 
          c.variedad AS VARIEDAD,
          m.sx AS SX,
          c.grc_emergencia AS EMF_ACTUAL,
          m.adapt AS ADAPT
        FROM floracion_chequeos c
        LEFT JOIN floracion_master m 
          ON c.num = m.num AND c.temporada = m.temporada
        WHERE c.fecha_chequeo = '%s'
          AND c.grc_emergencia > 0
      ", fecha_str)
      
      df_flor <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
      
      if (nrow(df_flor) == 0) {
        showNotification(paste("No hay variedades con emergencia > 0 en la fecha", fecha_str), type = "warning")
        return(NULL)
      }
      
      df_flor %>%
        mutate(
          VARIEDAD   = trimws(toupper(as.character(VARIEDAD))),
          SX         = as.numeric(SX),
          EMF_ACTUAL = as.numeric(EMF_ACTUAL),
          ADAPT      = if ("ADAPT" %in% colnames(.)) toupper(trimws(as.character(ADAPT))) else "UNKNOWN"
        )
    })
    
    # Categorías con limpieza de nombres
    df_categorias_clean <- reactive({
      req(df_categorias())
      df_categorias() %>%
        mutate(variedad = trimws(as.character(variedad)))
    })
    
    # --- Datos reactivos de resultados ---
    resultados <- eventReactive(input$btn_simular, {
      req(input$tabs_cruces == "Biparentales")
      
      A <- matriz_A()
      shiny::validate(need(!is.null(A), "Error al calcular la Matriz de Parentesco."))
      
      # Filtro estricto de variedades por suelo
      df_f <- datos_floracion()
      # Obtener variedades que cumplen el suelo si no es "Cualquiera"
      if (input$filtro_suelo != "Cualquiera") {
        suelo_req <- toupper(trimws(input$filtro_suelo))
        if ("ADAPT" %in% colnames(df_f)) {
          # USAR GREPL PARA ROBUSTEZ (Igual que en policruces)
          df_f <- df_f %>% filter(grepl(suelo_req, ADAPT))
        } else {
          # Fallback: buscar en categorias permitiendo valores combinados como "GOOD / CLAY"
          variedades_suelo <- df_categorias_clean() %>%
            filter(grepl(suelo_req, toupper(trimws(adapt)))) %>%
            pull(variedad)
          
          df_f <- df_f %>% filter(VARIEDAD %in% variedades_suelo)
        }
      }
      
      pool_m <- df_f %>% filter(SX == 3) %>% pull(VARIEDAD)
      pool_p <- df_f %>% filter(SX %in% c(1, 2)) %>% pull(VARIEDAD)
      
      shiny::validate(need(length(pool_m) > 0, "No hay MADRES disponibles para este suelo."))
      shiny::validate(need(length(pool_p) > 0, "No hay PADRES disponibles para este suelo."))

      # Determinar filtro de adaptación antes de llamar a la función
      suelo_objetivo <- if(input$filtro_suelo != "Cualquiera") input$filtro_suelo else NULL
      
      # Calculo base usando la funcion optimizada
      res <- sugerir_cruces(
        matriz_A      = A,
        ids_madres    = pool_m,
        ids_padres    = pool_p,
        max_f         = input$limit_f,
        cat_var       = cat_var,
        df_categorias = df_categorias(),
        filtro_adapt  = suelo_objetivo
      )
      
      shiny::validate(need(nrow(res) > 0, "No hay cruces que cumplan con los criterios."))
      
      # 1. Preparar tabla de categorías robusta (igual que en policruces)
      # Esto evita el error many-to-many y asegura la mejor categoría por variedad
      cats_top <- df_categorias_clean() %>%
        mutate(
          variedad  = trimws(toupper(variedad)),
          cat_peso  = case_when(
            grepl("C1", categoria) ~ 1,
            grepl("C2", categoria) ~ 2,
            grepl("C3", categoria) ~ 3,
            grepl("C4", categoria) ~ 4,
            TRUE                   ~ 5
          )
        ) %>%
        arrange(variedad, cat_peso) %>%
        distinct(variedad, .keep_all = TRUE)
      
      # 2. Renombrar y asociar metadatos usando la tabla robusta
      res <- res %>%
        rename(
          madre_id = Madre_ID,
          padre_id = Padre_ID,
          madre_nombre = Madre,
          padre_nombre = Padre,
          f_progenie = F_progenie
        ) %>%
        mutate(
          madre_join = trimws(toupper(madre_nombre)),
          padre_join = trimws(toupper(padre_nombre))
        ) %>%
        left_join(cats_top %>% select(variedad, cat_m = categoria, factor_m = factor, adapt_m = adapt,
                                      disease_m = disease, y_m = y, q_m = q, agro_m = agro, evf_m = evf_info),
                  by = c("madre_join" = "variedad")) %>%
        left_join(cats_top %>% select(variedad, cat_p = categoria, factor_p = factor, adapt_p = adapt,
                                      disease_p = disease, y_p = y, q_p = q, agro_p = agro, evf_p = evf_info),
                  by = c("padre_join" = "variedad"))
      
      # 3. Re-asociar flores (Operativo)
      # Normalizar nombres en df_f para asegurar el join
      df_f_clean <- datos_floracion() %>% 
        mutate(V_JOIN = trimws(toupper(VARIEDAD))) %>%
        distinct(V_JOIN, .keep_all = TRUE)

      res <- res %>%
        left_join(df_f_clean %>% select(V_JOIN, emf_m = EMF_ACTUAL), by = c("madre_join" = "V_JOIN")) %>%
        left_join(df_f_clean %>% select(V_JOIN, sx_p = SX, emf_p = EMF_ACTUAL), by = c("padre_join" = "V_JOIN"))
        
      # 4. Asociar EBVs reales si existen
      if (!is.null(ebvs_var) && nrow(ebvs_var) > 0) {
        res <- res %>%
          left_join(ebvs_var %>% select(variedad, ebv_tca_m = ebv_tca, ebv_rend_m = ebv_rend, ebv_pureza_m = ebv_pureza), by = c("madre_join" = "variedad")) %>%
          left_join(ebvs_var %>% select(variedad, ebv_tca_p = ebv_tca, ebv_rend_p = ebv_rend, ebv_pureza_p = ebv_pureza), by = c("padre_join" = "variedad"))
      } else {
        res <- res %>% mutate(ebv_tca_m = NA, ebv_rend_m = NA, ebv_pureza_m = NA, ebv_tca_p = NA, ebv_rend_p = NA, ebv_pureza_p = NA)
      }

      # ── Motor de Scoring Avanzado (Reintegrado) ──
      scored_df <- res %>%
        mutate(
          # Prevenir NAs en los cálculos
          factor_m = ifelse(is.na(factor_m), 0, factor_m),
          factor_p = ifelse(is.na(factor_p), 0, factor_p),
          disease_m = ifelse(is.na(disease_m), 0, disease_m),
          disease_p = ifelse(is.na(disease_p), 0, disease_p),
          
          # 1. Puntuación Genética (F)
          # F = 0 da puntaje completo, F = 0.25 reduce el puntaje
          score_gen_f = (1 - f_progenie) * input$w_genetic,
          
          # ── NUEVA LÓGICA DE TIERS (CASCADA) ──
          # Rango Élite: C1 x C1
          # Rango V.H.Q: (C1|C2) x (C1|C2|C3)
          # Rango Amplio: (C1|C2|C3) x (C1|C2|C3)
          # Rango Comercial: (C3|C4) x (C4|C1|C2)
          # Rango Exploratorio: C1 x C5 (Padre Probador)
          # Residual: C5 x C5 y otros
          
          rango = case_when(
            cat_m == "C1: Progeny Tested" & cat_p == "C1: Progeny Tested" ~ "Élite",
            (cat_m %in% c("C1: Progeny Tested", "C2: V.H.Q") & cat_p %in% c("C1: Progeny Tested", "C2: V.H.Q", "C3: Alto Y|Q")) |
            (cat_p %in% c("C1: Progeny Tested", "C2: V.H.Q") & cat_m %in% c("C1: Progeny Tested", "C2: V.H.Q", "C3: Alto Y|Q")) ~ "V.H.Q",
            (cat_m %in% c("C1: Progeny Tested", "C2: V.H.Q", "C3: Alto Y|Q") & cat_p %in% c("C1: Progeny Tested", "C2: V.H.Q", "C3: Alto Y|Q")) ~ "Amplio",
            (cat_m %in% c("C3: Alto Y|Q", "C4: Comercial") & cat_p %in% c("C4: Comercial", "C1: Progeny Tested", "C2: V.H.Q")) |
            (cat_p %in% c("C3: Alto Y|Q", "C4: Comercial") & cat_m %in% c("C4: Comercial", "C1: Progeny Tested", "C2: V.H.Q")) ~ "Comercial",
            (cat_m == "C1: Progeny Tested" & cat_p == "C5: Exploratorio") |
            (cat_p == "C1: Progeny Tested" & cat_m == "C5: Exploratorio") ~ "Exploratorio (Test)",
            TRUE ~ "Residual"
          ),
          
          rango_peso = case_when(
            rango == "Élite" ~ 10,
            rango == "V.H.Q" ~ 8,
            rango == "Amplio" ~ 6,
            rango == "Comercial" ~ 4,
            rango == "Exploratorio (Test)" ~ 2,
            TRUE ~ 0
          ),
          
          # 2. Puntuación por Categoría (Bono Dinámico por Tier)
          bono_cat = case_when(
            rango == "Élite" ~ 1.15,
            rango == "V.H.Q" ~ 1.12,
            rango == "Amplio" ~ 1.10,
            rango == "Comercial" ~ 1.08,
            rango == "Exploratorio (Test)" ~ 1.05,
            TRUE ~ 1.0
          ),
          
          # Penalización C5 x C5 (No tiene sentido cruzar dos incógnitas)
          penalty_c5 = ifelse(cat_m == "C5: Exploratorio" & cat_p == "C5: Exploratorio", -20.0, 0),
          
          # 3. Puntuación de Valor Cría (Índice Smith-Hazel v2 - 3 Rasgos)
          # Invertimos las escalas para que 9-10 sea lo mejor
          pheno_m_y = 10 - y_m,
          pheno_m_q = 10 - q_m,
          pheno_m_s = 10 - disease_m, # Sanidad = Resistencia
          
          pheno_p_y = 10 - y_p,
          pheno_p_q = 10 - q_p,
          pheno_p_s = 10 - disease_p,
          
          # Cálculo de Pesos del Índice (b)
          b_y = input$h2_y * input$w_y,
          b_q = input$h2_q * input$w_q,
          b_s = input$h2_s * input$w_s,
          
          # Indice Individual
          index_m = (pheno_m_y * b_y) + (pheno_m_q * b_q) + (pheno_m_s * b_s),
          index_p = (pheno_p_y * b_y) + (pheno_p_q * b_q) + (pheno_p_s * b_s),
          
          # Promedio del Índice de la Progenie
          score_agro = (((index_m + index_p) / 2) * bono_cat) * input$w_factor,
          
          # 4. Penalización por Sanidad Crítica (Si alguno es muy enfermo, bajamos mas)
          penalty_extreme_disease = ifelse(disease_m > 7 | disease_p > 7, -10, 0),
          
          # 4. Penalización por Sanidad (Disease alto resta puntos)
          penalty_disease = (disease_m + disease_p) * 0.05,
          
          # 4. Sincronización Floral (Operativo)
          # Si faltan datos de flores, no penalizamos (penalty=0)
          penalty_flor = ifelse(!is.na(emf_m) & !is.na(emf_p) & abs(emf_m - emf_p) > 1, 0.15, 0),
          
          # 5. Penalización/Bono por Desempeño Histórico (EVF)
          # Derivado de evf_info (formato "n_sel/n_cruces")
          tasa_m_calc = suppressWarnings({
            partes <- strsplit(as.character(evf_m), "/")
            sapply(partes, function(x) if (length(x) == 2 && !is.na(as.numeric(x[2])) && as.numeric(x[2]) > 0) as.numeric(x[1]) / as.numeric(x[2]) else NA_real_)
          }),
          n_m_calc = suppressWarnings({
            partes <- strsplit(as.character(evf_m), "/")
            sapply(partes, function(x) if (length(x) == 2) as.numeric(x[2]) else NA_real_)
          }),
          tasa_p_calc = suppressWarnings({
            partes <- strsplit(as.character(evf_p), "/")
            sapply(partes, function(x) if (length(x) == 2 && !is.na(as.numeric(x[2])) && as.numeric(x[2]) > 0) as.numeric(x[1]) / as.numeric(x[2]) else NA_real_)
          }),
          n_p_calc = suppressWarnings({
            partes <- strsplit(as.character(evf_p), "/")
            sapply(partes, function(x) if (length(x) == 2) as.numeric(x[2]) else NA_real_)
          }),
          penalty_perf = case_when(
            (!is.na(tasa_m_calc) & tasa_m_calc < 0.2 & !is.na(n_m_calc) & n_m_calc >= 5) |
              (!is.na(tasa_p_calc) & tasa_p_calc < 0.2 & !is.na(n_p_calc) & n_p_calc >= 5) ~ -3.0,
            (!is.na(tasa_m_calc) & tasa_m_calc > 0.6) &
              (!is.na(tasa_p_calc) & tasa_p_calc > 0.6) ~ 2.0,
            TRUE ~ 0.0
          ),
          
          # 6. Bono por EBV Real (Desviación vs Testigo)
          # Recompensamos cruces donde los padres son positivamente superiores en Rendimiento y TCA
          # Limitamos el bono a +/- 0.5 para que no abrume a la lógica categórica
          ebv_bonus_m = pmax(pmin(coalesce(ebv_tca_m, 0) * 0.05 + coalesce(ebv_rend_m, 0) * 0.1, 0.5), -0.5),
          ebv_bonus_p = pmax(pmin(coalesce(ebv_tca_p, 0) * 0.05 + coalesce(ebv_rend_p, 0) * 0.1, 0.5), -0.5),
          ebv_bonus_total = (ebv_bonus_m + ebv_bonus_p),
          
          # Score Final Compuesto (Protección contra NAs)
          score_total = round(
            coalesce(score_gen_f, 0) + 
            coalesce(score_agro, 0) +
            coalesce(ebv_bonus_total, 0) +
            penalty_perf + 
            penalty_c5 -
            coalesce(penalty_flor, 0), 
            3
          )
        ) %>%
        # SEGURIDAD FASE 3: Filtro estricto de Consanguinidad y Scores Negativos
        filter(f_progenie <= input$limit_f) %>%
        # BLOQUEO ÉLITE EN ROCKY: si es rocky, bajamos el rango_peso de los Élite a 0 o los filtramos
        mutate(
          rango_peso = if_else(input$filtro_suelo == "ROCOSO" & rango == "Élite", -1, as.numeric(rango_peso))
        ) %>%
        filter(rango_peso != -1)
      
      # Guardamos el pool completo para el generador de policruces y radar
      full_res_scoring_rv(scored_df)
      
      # Ordenar SOLO por Score Total (sin jerarquía de categorías)
      # Limitar la aparición de cada variedad a máximo 3 veces en el top N
      # para garantizar diversidad genética en las sugerencias
      top_df <- scored_df %>%
        arrange(desc(score_total))
      
      # Aplicar limitador de saturación por variedad
      conteo_m <- table(character(0))
      conteo_p <- table(character(0))
      filas_seleccionadas <- integer(0)
      max_por_variedad <- 5
      
      for (i in seq_len(nrow(top_df))) {
        m <- top_df$madre_nombre[i]
        p <- top_df$padre_nombre[i]
        cnt_m <- if (m %in% names(conteo_m)) conteo_m[[m]] else 0
        cnt_p <- if (p %in% names(conteo_p)) conteo_p[[p]] else 0
        if (cnt_m < max_por_variedad & cnt_p < max_por_variedad) {
          filas_seleccionadas <- c(filas_seleccionadas, i)
          conteo_m[m] <- cnt_m + 1
          conteo_p[p] <- cnt_p + 1
        }
        if (length(filas_seleccionadas) >= input$top_n) break
      }
      
      top_df[filas_seleccionadas, ]
    })
    
    # --- 3. Generador de Policruces (Motor por Categorías v2) ---
    # Lógica: Agrupa padres por suelo y categoría genética en cascada:
    # Poli Élite → Poli V.H.Q → Poli Amplio → Poli Comercial → Poli Exploratorio
    # Codificación SX: 1=macho, 2=bisexual(puede donar polen), 3=hembra
    lanternas_recomendadas <- eventReactive(input$btn_simular, {
      req(input$tabs_cruces == "Policruces")
      
      df_f <- datos_floracion()
      req(!is.null(df_f), nrow(df_f) > 0)
      
      # IMPORTANTE: deduplicar cats (una fila por variedad) antes del join
      # para evitar el error many-to-many que corrompe el motor de policruces
      cats <- df_categorias_clean() %>%
        mutate(
          variedad  = trimws(toupper(variedad)),
          # Asignar peso numérico para priorizar la categoría más alta
          cat_peso  = case_when(
            grepl("C1", categoria) ~ 1,
            grepl("C2", categoria) ~ 2,
            grepl("C3", categoria) ~ 3,
            grepl("C4", categoria) ~ 4,
            TRUE                   ~ 5
          )
        ) %>%
        arrange(variedad, cat_peso) %>%
        distinct(variedad, .keep_all = TRUE) %>%   # una sola fila por variedad (la de mayor jerarquía)
        select(variedad, categoria, adapt, q, y, factor)
      
      # Enriquecer floración con categoría genética (solo cat/q/y/factor, NO adapt)
      # ADAPT viene directamente del archivo de floración (más confiable)
      df_enr <- df_f %>%
        left_join(cats, by = c("VARIEDAD" = "variedad"), relationship = "many-to-one") %>%
        mutate(
          categoria = ifelse(is.na(categoria), "C5: Exploratorio", categoria),
          # Priorizar ADAPT del archivo; si no existe usar el de cats
          ADAPT     = case_when(
            !is.na(ADAPT) & ADAPT != "" & ADAPT != "UNKNOWN" ~ ADAPT,
            !is.na(adapt) & adapt != ""                       ~ toupper(trimws(adapt)),
            TRUE                                               ~ "UNKNOWN"
          ),
          q         = as.numeric(ifelse(is.na(q), 0, q)),
          y         = as.numeric(ifelse(is.na(y), 0, y)),
          factor    = as.numeric(ifelse(is.na(factor), 0, factor))
        ) %>%
        distinct(VARIEDAD, SX, .keep_all = TRUE)
      
      shiny::validate(need(nrow(df_enr) >= 5, "El archivo de floración no tiene datos suficientes."))
      
      # Expandir padres multi-suelo: BUENO/MAL_DRENADO → una fila por cada suelo
      df_exp <- df_enr %>%
        rowwise() %>%
        mutate(suelos_list = list(strsplit(ADAPT, "/")[[1]])) %>%
        ungroup() %>%
        unnest(suelos_list) %>%
        rename(suelo = suelos_list) %>%
        mutate(suelo = trimws(toupper(suelo)))
      
      # Aplicar filtro de suelo del usuario
      if (input$filtro_suelo != "Cualquiera") {
        df_exp <- df_exp %>% filter(suelo == toupper(trimws(input$filtro_suelo)))
      }
      
      suelos_activos <- unique(df_exp$suelo)
      shiny::validate(need(length(suelos_activos) > 0, "No hay padres disponibles para el suelo seleccionado."))
      
      # Tipos de policruce en orden de prioridad (cascade)
      # cat_h = categorías válidas para hembras, cat_m = para machos
      tipos_poli <- list(
        list(nombre="Poli Élite",       cat_h=c("C1"),          cat_m=c("C1","C2"),          skip_rocky=TRUE),
        list(nombre="Poli V.H.Q",       cat_h=c("C1","C2"),    cat_m=c("C1","C2","C3"),    skip_rocky=FALSE),
        list(nombre="Poli Amplio",      cat_h=c("C1","C2","C3"),cat_m=c("C1","C2","C3"),  skip_rocky=FALSE),
        list(nombre="Poli Comercial",   cat_h=c("C3","C4"),     cat_m=c("C4","C1","C2"),   skip_rocky=FALSE),
        list(nombre="Poli Exploratorio",cat_h=NULL,              cat_m=NULL,                  skip_rocky=FALSE)
      )
      
      lanternas    <- list()
      hoja_detalle <- list() # Para poblar hoja_campo_rv
      lanterna_num <- as.integer(input$num_lanterna_start)
      
      for (suelo_actual in suelos_activos) {
        df_suelo <- df_exp %>% filter(suelo == suelo_actual)
        es_rocky <- suelo_actual == "ROCOSO"
        
        # SX=3: hembra, SX=1 o 2: macho/bisexual que puede donar polen
        hembras_base <- df_suelo %>%
          filter(SX == 3) %>%
          arrange(desc(q), desc(y), desc(factor)) %>%
          mutate(flores_disp = as.numeric(EMF_ACTUAL))
        
        machos_base <- df_suelo %>%
          filter(SX %in% c(1, 2)) %>%
          arrange(desc(q), desc(y), desc(factor)) %>%
          mutate(flores_disp = as.numeric(EMF_ACTUAL))
        
        if (nrow(hembras_base) < 2 || nrow(machos_base) < 3) next
        
        hembras_usadas <- character(0)
        
        for (tipo in tipos_poli) {
          if (isTRUE(tipo$skip_rocky) && es_rocky) next
          
          # Hembras candidatas para este tipo
          if (!is.null(tipo$cat_h)) {
            patron_h <- paste(tipo$cat_h, collapse = "|")
            h_cand <- hembras_base %>%
              filter(!(VARIEDAD %in% hembras_usadas),
                     flores_disp >= 2,
                     grepl(patron_h, categoria))
          } else {
            h_cand <- hembras_base %>%
              filter(!(VARIEDAD %in% hembras_usadas), flores_disp >= 2)
          }
          
          if (nrow(h_cand) < 2) next
          
          # Machos candidatos para este tipo
          if (!is.null(tipo$cat_m)) {
            patron_m <- paste(tipo$cat_m, collapse = "|")
            m_cand <- machos_base %>%
              filter(flores_disp > 0, grepl(patron_m, categoria))
          } else {
            m_cand <- machos_base %>% filter(flores_disp > 0)
          }
          
          if (nrow(m_cand) < 3) next
          
          # Formar grupos mientras haya hembras disponibles
          repeat {
            h_disp <- h_cand %>% filter(!(VARIEDAD %in% hembras_usadas), flores_disp >= 2)
            if (nrow(h_disp) < 2) break
            
            h_grupo        <- h_disp %>% head(3)
            n_h            <- nrow(h_grupo)
            flores_h_total <- n_h * 2          # 2 flores por hembra
            objetivo_m     <- flores_h_total * 2  # ratio objetivo 1:2
            
            m_disp <- m_cand %>% filter(flores_disp > 0)
            if (nrow(m_disp) < 3) break
            
            # Acumular machos hasta alcanzar el objetivo de flores
            flores_m_acum <- 0
            machos_sel    <- character(0)
            for (j in seq_len(nrow(m_disp))) {
              machos_sel    <- c(machos_sel, m_disp$VARIEDAD[j])
              flores_m_acum <- flores_m_acum + m_disp$flores_disp[j]
              # Romper solo si ya alcanzamos las flores Y tenemos al menos 3 machos
              if (flores_m_acum >= objetivo_m && length(machos_sel) >= 3) break
            }
            
            # Mínimo 3 machos (si no había suficientes machos disponibles en total)
            if (length(machos_sel) < 3) break
            
            ratio_real <- round(flores_m_acum / flores_h_total, 1)
            status     <- ifelse(flores_m_acum >= objetivo_m, "Óptimo", "Aceptable")
            
            lanternas[[length(lanternas) + 1]] <- data.frame(
              ID          = lanterna_num,
              Suelo       = suelo_actual,
              Tipo        = tipo$nombre,
              Hembras     = paste(h_grupo$VARIEDAD, collapse = ", "),
              Machos      = paste(machos_sel, collapse = ", "),
              `Flores H`  = flores_h_total,
              `Flores M`  = flores_m_acum,
              `Ratio H:M` = paste0("1:", ratio_real),
              Status      = status,
              check.names = FALSE, stringsAsFactors = FALSE
            )
            
            # ── Poblar Hoja de Detalle (Para Hoja de Montaje) ──
            # Hembras
            for (hv in h_grupo$VARIEDAD) {
              h_info <- h_grupo[h_grupo$VARIEDAD == hv, ]
              hoja_detalle[[length(hoja_detalle) + 1]] <- data.frame(
                Lanterna = lanterna_num,
                Suelo    = suelo_actual,
                Tipo     = tipo$nombre,
                Rol      = "HEMBRA",
                Variedad = hv,
                Cat      = h_info$categoria,
                Flores   = h_info$flores_disp,
                SX       = 3,
                stringsAsFactors = FALSE
              )
            }
            # Machos
            for (mv in machos_sel) {
              m_info <- m_cand[m_cand$VARIEDAD == mv, ]
              hoja_detalle[[length(hoja_detalle) + 1]] <- data.frame(
                Lanterna = lanterna_num,
                Suelo    = suelo_actual,
                Tipo     = tipo$nombre,
                Rol      = "MACHO",
                Variedad = mv,
                Cat      = m_info$categoria,
                Flores   = 3, # Se usan 3 flores por lanterna
                SX       = m_info$SX,
                stringsAsFactors = FALSE
              )
            }
            
            lanterna_num   <- lanterna_num + 1
            hembras_usadas <- c(hembras_usadas, h_grupo$VARIEDAD)
            
            # Descontar flores de machos (3 flores usadas por lanterna)
            for (mv in machos_sel) {
              idx <- which(m_cand$VARIEDAD == mv)
              if (length(idx) > 0) m_cand$flores_disp[idx[1]] <- max(0, m_cand$flores_disp[idx[1]] - 3)
            }
          }
        }
      }
      
      # Actualizar el valor reactivo de la hoja de campo detalle
      if (length(hoja_detalle) > 0) {
        hoja_campo_rv(do.call(rbind, hoja_detalle))
      } else {
        hoja_campo_rv(NULL)
      }
      
      shiny::validate(need(
        length(lanternas) > 0,
        paste0("No se formaron grupos de policruces. Verifica que el archivo tenga ",
               "plantas con SX=3 (hembras) y SX=1/2 (machos) con flores disponibles.")
      ))
      
      do.call(rbind, lanternas)
    })
    
    output$tabla_lanternas <- DT::renderDT({
      req(lanternas_recomendadas())
      df_lt <- lanternas_recomendadas()
      
      DT::datatable(
        df_lt,
        rownames = FALSE,
        options  = list(
          pageLength = 20,
          scrollX    = TRUE,
          dom        = 'frtip'   # Sin extensión Buttons para evitar errores JS
        ),
        caption = "Policruces generados por tipo genético y suelo"
      ) %>%
        DT::formatStyle(
          "Tipo",
          backgroundColor = DT::styleEqual(
            c("Poli Élite", "Poli V.H.Q", "Poli Amplio", "Poli Comercial", "Poli Exploratorio"),
            c("#d5f5e3",   "#d6eaf8",    "#fdebd0",     "#f9ebea",         "#f4f6f7")
          )
        ) %>%
        DT::formatStyle(
          "Status",
          color = DT::styleEqual(c("Óptimo", "Aceptable"), c("#27ae60", "#f39c12"))
        )
    })
    
    # --- KPIs (bslib value_boxes) ---
    output$vboxes_resultados <- renderUI({
      res <- resultados()
      n <- if (!is.null(res)) nrow(res) else 0
      avg_f <- if (!is.null(res)) round(mean(res$f_progenie), 4) else 0
      best <- if (!is.null(res)) round(max(res$score_total), 3) else 0
      
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Cruces Sugeridos",
          value = n,
          showcase = icon("dna"),
          theme = "success"
        ),
        value_box(
          title = "Consanguinidad Promedio",
          value = avg_f,
          showcase = icon("chart-line"),
          theme = "warning"
        ),
        value_box(
          title = "Mejor Score Alcanzado",
          value = best,
          showcase = icon("trophy"),
          theme = "primary"
        )
      )
    })
    
    # --- Tabla de Cruces ---
    output$tabla_cruces <- DT::renderDT({
      shiny::validate(need(
        !is.null(input$fecha_floracion) && input$btn_simular > 0,
        "Seleccione una fecha en el calendario y presione 'Simular Cruzamientos' para ver las recomendaciones."
      ))
      req(resultados())
      
      tabla <- resultados() %>%
        mutate(
          Flores = paste0(ifelse(is.na(emf_m), "?", emf_m), " / ",
                          ifelse(is.na(emf_p), "?", emf_p))
        ) %>%
        select(
          Madre             = madre_nombre,
          `Cat M`           = cat_m,
          Padre             = padre_nombre,
          `Cat P`           = cat_p,
          `Suelo`           = adapt_m,
          `Sexo P`          = sx_p,
          `Flores (M/P)`    = Flores,
          `F Progenie`      = f_progenie,
          `Score Total`     = score_total
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
      
      DT::datatable(
        tabla,
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          order      = list(list(7, "desc")) # Ordenar por Score
        ),
        rownames  = FALSE,
        selection = "multiple",
        caption   = "Selecciona las filas que deseas registrar o analizar."
      ) %>%
        DT::formatStyle("Score Total",
                        background         = DT::styleColorBar(range(tabla$`Score Total`), "#27ae60"),
                        backgroundSize     = "98% 88%",
                        backgroundRepeat   = "no-repeat",
                        backgroundPosition = "center") %>%
        DT::formatStyle("F Progenie",
                        color = DT::styleInterval(c(0.03, 0.0625),
                                                   c("#27ae60", "#f39c12", "#e74c3c")))
    })
    
    # --- Detalle del cruce seleccionado (Ventana Única con Radar) ---
    observeEvent(input$btn_ver_radar, {
      req(resultados())
      sel <- input$tabla_cruces_rows_selected
      if (length(sel) == 0) {
        showNotification("Seleccione un cruce en la tabla para ver el radar.", type = "warning")
        return()
      }
      ns <- session$ns
      
      # Tomamos el primero si hay varios seleccionados
      cruce <- resultados()[sel[1], ]
      
      showModal(modalDialog(
        title = tagList(icon("flask"), paste("Análisis Detallado:", cruce$madre_nombre, "x", cruce$padre_nombre)),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Cerrar"),
        
        div(
          class = "container-fluid",
          # 1. Comparativa de Padres
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(class = "bg-danger text-white", tagList(icon("venus"), " Madre (Hembra)")),
              tags$div(
                style = "font-size: 1em;",
                tags$p(tags$b("Nombre: "), cruce$madre_nombre),
                tags$p(tags$b("Adaptación: "), tags$span(cruce$adapt_m, class="badge bg-warning text-dark")),
                tags$p(tags$b("Histórico: "), tags$span(cruce$evf_m, class="badge bg-info")),
                tags$p(tags$small(paste0("Y:", round(cruce$y_m,1), " | Q:", round(cruce$q_m,1), " | Sanidad:", round(cruce$disease_m,1))))
              )
            ),
            card(
              card_header(class = "bg-primary text-white", tagList(icon("mars"), " Padre (Macho)")),
              tags$div(
                style = "font-size: 1em;",
                tags$p(tags$b("Nombre: "), cruce$padre_nombre),
                tags$p(tags$b("Adaptación: "), tags$span(cruce$adapt_p, class="badge bg-warning text-dark")),
                tags$p(tags$b("Histórico: "), tags$span(cruce$evf_p, class="badge bg-info")),
                tags$p(tags$small(paste0("Y:", round(cruce$y_p,1), " | Q:", round(cruce$q_p,1), " | Sanidad:", round(cruce$disease_p,1))))
              )
            )
          ),
          
          # 2. Gráfico de Radar (El "Corazón" del Análisis)
          hr(),
          card(
            card_header(tagList(icon("chart-pie"), " Perfil Genético Predictivo")),
            plotlyOutput(ns("plot_radar_progenie_modal"), height = "400px")
          ),
          
          # 3. Resultados de la Progenie
          layout_column_wrap(
            width = 1/2,
            value_box(
              title = "Consanguinidad (F)",
              value = round(cruce$f_progenie, 4),
              showcase = icon("dna"),
              theme = ifelse(cruce$f_progenie > 0.0625, "danger", "success")
            ),
            value_box(
              title = "Score Total",
              value = round(cruce$score_total, 3),
              showcase = icon("chart-line"),
              theme = "info"
            )
          )
        )
      ))
      
      # Renderizar el gráfico de radar dentro del modal
      output$plot_radar_progenie_modal <- renderPlotly({
        get_axes <- function(y, q, factor, disease, f) {
          c(
            Tonelaje   = 10 - y,
            Calidad    = 10 - q,
            Agronomico = factor,
            Sanidad    = 10 - disease,
            Diversidad = (1 - f) * 10
          )
        }
        
        vals_m <- get_axes(cruce$y_m, cruce$q_m, cruce$factor_m, cruce$disease_m, 0)
        vals_p <- get_axes(cruce$y_p, cruce$q_p, cruce$factor_p, cruce$disease_p, 0)
        vals_h <- get_axes(
          (cruce$y_m + cruce$y_p) / 2,
          (cruce$q_m + cruce$q_p) / 2,
          (cruce$factor_m + cruce$factor_p) / 2,
          (cruce$disease_m + cruce$disease_p) / 2,
          cruce$f_progenie
        )
        
        ejes_n <- c(names(vals_m), names(vals_m)[1])
        r_m <- c(as.numeric(vals_m), as.numeric(vals_m)[1])
        r_p <- c(as.numeric(vals_p), as.numeric(vals_p)[1])
        r_h <- c(as.numeric(vals_h), as.numeric(vals_h)[1])
        
        plot_ly(type = 'scatterpolar', fill = 'toself', mode = 'lines') %>%
          add_trace(r = r_m, theta = ejes_n, name = paste("Madre:", cruce$madre_nombre), fillcolor = 'rgba(255, 0, 0, 0.1)') %>%
          add_trace(r = r_p, theta = ejes_n, name = paste("Padre:", cruce$padre_nombre), fillcolor = 'rgba(0, 0, 255, 0.1)') %>%
          add_trace(r = r_h, theta = ejes_n, name = "Hijo (Predicho)", fillcolor = 'rgba(0, 255, 0, 0.4)', 
                    line = list(width = 4)) %>%
          layout(
            polar = list(radialaxis = list(visible = T, range = c(0, 10))),
            margin = list(l = 50, r = 50, b = 20, t = 20)
          )
      })
    })
    
    # --- Registro directo desde Simulador ---
    observeEvent(input$btn_reg_biparental, {
      req(resultados())
      sel <- input$tabla_cruces_rows_selected
      if (length(sel) == 0) {
        showNotification("Seleccione al menos un cruce biparental en la tabla.", type = "warning")
        return()
      }
      
      df_sel <- resultados()[sel, ]
      
      tryCatch({
        n_reg <- 0
        for (i in seq_len(nrow(df_sel))) {
          db_save_cruce(
            con       = con,
            madre     = df_sel$madre_nombre[i],
            padre     = df_sel$padre_nombre[i],
            programa  = "CR",
            suelo     = input$filtro_suelo,
            anio_cruce = as.integer(format(Sys.Date(), "%Y")),
            fecha_cruce = as.character(Sys.Date()),
            semillas  = NA_integer_,
            notas     = "Registrado desde Simulador (Biparental automático)",
            tipo      = "Biparental",
            flores_m  = df_sel$emf_m[i],
            flores_p  = df_sel$emf_p[i]
          )
          n_reg <- n_reg + 1
        }
        showNotification(paste0("Se registraron exitosamente ", n_reg, " cruces biparentales."), type = "message")
      }, error = function(e) {
        showNotification(paste("Error guardando cruces:", e$message), type = "error")
      })
    })
    
    observeEvent(input$btn_reg_policruce, {
      req(lanternas_recomendadas())
      sel <- input$tabla_lanternas_rows_selected
      if (length(sel) == 0) {
        showNotification("Seleccione al menos un policruce en la tabla.", type = "warning")
        return()
      }
      
      df_sel <- lanternas_recomendadas()[sel, ]
      
      tryCatch({
        n_reg <- 0
        for (i in seq_len(nrow(df_sel))) {
          # Polycross formula based on minimums
          fm <- df_sel$N_Hembras[i] * 2
          fp <- df_sel$N_Machos[i] * 2
          
          db_save_cruce(
            con       = con,
            madre     = df_sel$Hembras[i],
            padre     = df_sel$Machos[i],
            programa  = "CR",
            suelo     = input$filtro_suelo,
            anio_cruce = as.integer(format(Sys.Date(), "%Y")),
            fecha_cruce = as.character(Sys.Date()),
            semillas  = NA_integer_,
            notas     = paste0("Registrado desde Simulador (Policruce ID: ", df_sel$ID[i], ")"),
            tipo      = "Policruce",
            flores_m  = fm,
            flores_p  = fp
          )
          n_reg <- n_reg + 1
        }
        showNotification(paste0("Se registraron exitosamente ", n_reg, " policruces."), type = "message")
      }, error = function(e) {
        showNotification(paste("Error guardando policruces:", e$message), type = "error")
      })
    })
    
    # --- Exportar Resumen Consolidado (Excel Multi-hoja) ---
    output$btn_export <- downloadHandler(
      filename = function() {
        paste0("REPORTE_CRUZAMIENTOS_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        # 1. Preparar Biparentales
        df_bip <- resultados()
        if (!is.null(df_bip) && nrow(df_bip) > 0) {
          df_bip <- df_bip %>%
            select(-contains("_join"), -contains("rango_peso")) %>%
            rename(
              `Rango Genético` = rango,
              `Madre` = madre_nombre,
              `Cat M` = cat_m,
              `Padre` = padre_nombre,
              `Cat P` = cat_p,
              `Suelo` = adapt_m,
              `Flores M` = emf_m,
              `Flores P` = emf_p,
              `Consanguinidad (F)` = f_progenie,
              `Score Total` = score_total
            )
        } else {
          df_bip <- data.frame(Mensaje = "No se simularon cruces biparentales")
        }
        
        # 2. Preparar Policruces
        df_pol <- lanternas_recomendadas()
        if (!is.null(df_pol) && nrow(df_pol) > 0) {
          df_pol <- df_pol %>%
            select(ID, Suelo, Tipo, Hembras, Machos, `Flores H`, `Flores M`, `Ratio H:M`, Status)
        } else {
          df_pol <- data.frame(Mensaje = "No se simularon policruces")
        }
        
        # 3. Preparar Detalle de Campo (Policruces)
                list_export <- list(
          "Biparentales" = df_bip,
          "Policruces (Resumen)" = df_pol,
          "Detalle Campo (Policruces)" = df_detalle
        )
        
        openxlsx::write.xlsx(list_export, file, asTable = TRUE, tableStyle = "TableStyleMedium2")
      }
    )
    
    # --- Exportar Hoja de Campo (Policruces) ---
    output$btn_export_campo <- downloadHandler(
      filename = function() {
        paste0("HOJA_MONTAJE_CAMPO_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(lanternas_recomendadas())
        req(!is.null(hoja_campo_rv()))
        
        write.csv(hoja_campo_rv(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # ==========================================================================
    # --- REGISTRO OPERATIVO DE CRUCES ---
    # ==========================================================================
    
    # Poblar madre/padre desde AllAct (mismas opciones que biparentales)
    observe({
      ops <- if (!is.null(opciones_parentales)) opciones_parentales else opciones
      updateSelectizeInput(session, "rc_madre",
                           choices = ops, selected = "",
                           server  = TRUE,
                           options = list(
                             create      = FALSE,
                             placeholder = "Buscar madre...",
                             maxOptions  = 579
                           ))
      updateSelectizeInput(session, "rc_padre",
                           choices = ops, selected = "",
                           server  = TRUE,
                           options = list(
                             create      = FALSE,
                             placeholder = "Buscar padre...",
                             maxOptions  = 579
                           ))
    })
    
    # Poblar filtro de año desde BD
    observe({
      cruces <- db_get_cruces(con)
      if (nrow(cruces) == 0) return()
      anios <- c("Todos", sort(unique(cruces$anio_cruce), decreasing = TRUE))
      updateSelectInput(session, "rc_filter_anio", choices = anios)
    })
    
    # Alerta si el cruce ya se hizo antes
    output$ui_rc_alerta_repetido <- renderUI({
      req(input$rc_madre, input$rc_padre)
      if (input$rc_madre == "" || input$rc_padre == "") return(NULL)
      n <- dbGetQuery(con,
                      "SELECT COUNT(*) as n FROM registro_cruces WHERE madre = ? AND padre = ?",
                      params = list(input$rc_madre, input$rc_padre))$n
      if (n > 0) {
        div(class = "alert alert-warning mt-2",
            icon("triangle-exclamation"),
            tags$b(" Cruce repetido: "),
            sprintf("Este par %s x %s ya fue cruzado %d vez(ces) anteriormente.",
                    input$rc_madre, input$rc_padre, n)
        )
      } else {
        div(class = "alert alert-success mt-2",
            icon("check-circle"),
            tags$b(" Cruce nuevo: "),
            sprintf("%s x %s no tiene antecedentes en el registro.",
                    input$rc_madre, input$rc_padre)
        )
      }
    })
    
    # Value boxes resumen
    output$ui_rc_stats <- renderUI({
      cruces <- db_get_cruces(con)
      anio_act <- as.integer(format(Sys.Date(), "%Y"))
      c_anio   <- cruces %>% filter(anio_cruce == anio_act)
      bip   <- sum(c_anio$tipo == "Biparental", na.rm = TRUE)
      poli  <- sum(c_anio$tipo == "Policruce",  na.rm = TRUE)
      nuevos <- sum(c_anio$cruce_previo == 0, na.rm = TRUE)
      rep    <- sum(c_anio$cruce_previo == 1, na.rm = TRUE)
      germ   <- cruces %>% filter(!is.na(pct_germinacion))
      pct_g  <- if (nrow(germ) > 0) paste0(round(mean(germ$pct_germinacion, na.rm=TRUE),1),"%") else "—"
      
      layout_column_wrap(
        width = 1/4,
        value_box(paste0("Biparentales ", anio_act), bip,  icon("dna"),      theme = "primary", height = "80px", showcase_layout = "left center"),
        value_box(paste0("Policruces ",  anio_act), poli, icon("layer-group"), theme = "success", height = "80px", showcase_layout = "left center"),
        value_box("Repetidos",           rep,   icon("rotate"),   theme = "warning", height = "80px", showcase_layout = "left center"),
        value_box("Germ. Promedio",      pct_g, icon("seedling"), theme = "info",    height = "80px", showcase_layout = "left center")
      )
    })
    
    # Guardar cruce
    observeEvent(input$btn_rc_guardar, {
      req(input$rc_madre, input$rc_padre)
      if (length(input$rc_madre) == 0 || length(input$rc_padre) == 0 || all(input$rc_madre == "") || all(input$rc_padre == "")) {
        showNotification("Selecciona madre y padre antes de guardar.", type = "warning")
        return()
      }
      tryCatch({
        tipo_cruce <- input$rc_tipo
        
        if (tipo_cruce == "Policruce") {
          # Un solo registro con todas las madres y padres concatenados
          madres_str <- paste(input$rc_madre, collapse = ", ")
          padres_str <- paste(input$rc_padre, collapse = ", ")
          
          db_save_cruce(
            con       = con,
            madre     = madres_str,
            padre     = padres_str,
            programa  = "CR",
            suelo     = input$rc_suelo,
            anio_cruce = input$rc_anio,
            fecha_cruce = as.character(input$rc_fecha),
            semillas  = {s <- input$rc_semillas; if (is.na(s)) NA_integer_ else as.integer(s)},
            notas     = input$rc_notas,
            tipo      = tipo_cruce
          )
          msg <- paste0("Policruce registrado: ", length(input$rc_madre), " madres y ", length(input$rc_padre), " padres.")
        } else {
          # Biparental: Crear registros individuales para todas las combinaciones
          combinaciones <- expand.grid(madre = input$rc_madre, padre = input$rc_padre, stringsAsFactors = FALSE)
          n_registros <- 0
          
          for (i in seq_len(nrow(combinaciones))) {
            db_save_cruce(
              con       = con,
              madre     = combinaciones$madre[i],
              padre     = combinaciones$padre[i],
              programa  = "CR",
              suelo     = input$rc_suelo,
              anio_cruce = input$rc_anio,
              fecha_cruce = as.character(input$rc_fecha),
              semillas  = {s <- input$rc_semillas; if (is.na(s)) NA_integer_ else as.integer(s)},
              notas     = input$rc_notas,
              tipo      = tipo_cruce
            )
            n_registros <- n_registros + 1
          }
          msg <- paste0(n_registros, " cruces biparentales individuales registrados.")
        }
        
        showNotification(msg, type = "message")
        # Limpiar formulario
        updateSelectizeInput(session, "rc_madre", selected = "")
        updateSelectizeInput(session, "rc_padre", selected = "")
        updateTextAreaInput(session, "rc_notas", value = "")
        updateNumericInput(session, "rc_semillas", value = NA)
      }, error = function(e) {
        showNotification(paste("Error al guardar:", e$message), type = "error")
      })
    })
    
    # Variable reactiva para forzar actualización de tabla
    rv_refresh_rc <- reactiveVal(0)
    
    # Datos reactivos para la tabla de cruces registrados
    rc_filtered_data <- reactive({
      input$btn_rc_guardar
      input$btn_rc_germ
      rv_refresh_rc() # Para refrescar luego de eliminar
      
      cruces <- db_get_cruces(con)
      if (input$rc_filter_anio != "Todos")
        cruces <- cruces %>% filter(anio_cruce == as.integer(input$rc_filter_anio))
      if (input$rc_filter_tipo != "Todos")
        cruces <- cruces %>% filter(tipo == input$rc_filter_tipo)
      if (input$rc_filter_suelo != "Todos")
        cruces <- cruces %>% filter(suelo == input$rc_filter_suelo)
      if (input$rc_filter_estado != "Todos")
        cruces <- cruces %>% filter(estado == input$rc_filter_estado)
        
      cruces
    })
    
    # --- Eliminación de Cruces Registrados ---
    observeEvent(input$btn_rc_eliminar, {
      rows <- input$tabla_rc_rows_selected
      if (length(rows) == 0) {
        showNotification("Seleccione al menos un cruce de la tabla para eliminar.", type = "warning")
        return()
      }
      
      ask_confirmation(
        inputId = ns("confirm_rc_delete"),
        title = "Eliminar Cruces",
        text = sprintf("¿Está seguro de que desea eliminar permanentemente %d cruce(s)? Esta acción no se puede deshacer.", length(rows)),
        type = "error",
        btn_labels = c("Cancelar", "Eliminar"),
        btn_colors = c("#bdc3c7", "#e74c3c")
      )
    })
    
    observeEvent(input$confirm_rc_delete, {
      req(isTRUE(input$confirm_rc_delete))
      rows <- input$tabla_rc_rows_selected
      req(length(rows) > 0)
      
      df_actual <- rc_filtered_data()
      ids_to_delete <- df_actual$id[rows]
      
      tryCatch({
        ids_str <- paste(ids_to_delete, collapse = ",")
        DBI::dbExecute(con, sprintf("DELETE FROM registro_cruces WHERE id IN (%s)", ids_str))
        
        showNotification(sprintf("%d cruce(s) eliminados correctamente.", length(ids_to_delete)), type = "message")
        
        # Limpiar selección y refrescar tabla
        DT::dataTableProxy(ns("tabla_rc")) %>% DT::selectRows(NULL)
        rv_refresh_rc(rv_refresh_rc() + 1)
      }, error = function(e) {
        showNotification(paste("Error al eliminar cruces:", e$message), type = "error")
      })
    })
    
    # Tabla de cruces registrados
    output$tabla_rc <- DT::renderDT({
      cruces <- rc_filtered_data()
      shiny::validate(need(nrow(cruces) > 0, "No hay cruces registrados aún."))
      
      cruces %>%
        select(
          ID        = id,
          Fecha     = fecha_cruce,
          Madre     = madre,
          Padre     = padre,
          Tipo      = tipo,
          Suelo     = suelo,
          Año       = anio_cruce,
          Semillas  = semillas,
          Germinadas = germinadas,
          `Germ %`  = pct_germinacion,
          Estado    = estado,
          Repetido  = cruce_previo,
          Notas     = notas
        ) %>%
        DT::datatable(
          rownames  = FALSE,
          filter    = "top",
          selection = "multiple",
          options   = list(pageLength = 15, scrollX = TRUE,
                           order = list(list(0, "desc")))
        ) %>%
        DT::formatStyle("Estado",
                        backgroundColor = DT::styleEqual(
                          c("EJECUTADO",  "GERMINADO",  "EN_EVALUACION"),
                          c("#fef9c3",    "#dcfce7",    "#dbeafe")
                        ), fontWeight = "bold"
        ) %>%
        DT::formatStyle("Repetido",
                        backgroundColor = DT::styleEqual(c(0, 1), c("#dcfce7", "#fef9c3")),
                        color = DT::styleEqual(c(0, 1), c("#15803d", "#d97706")),
                        fontWeight = "bold"
        )
    })
    
    # Actualizar germinación
    observeEvent(input$btn_rc_germ, {
      req(input$rc_germ_id, input$rc_germ_cantidad)
      tryCatch({
        db_update_germinacion(con,
                              id         = as.integer(input$rc_germ_id),
                              germinadas = as.integer(input$rc_germ_cantidad))
        showNotification(
          paste0("Germinación actualizada. ID ", input$rc_germ_id,
                 ": ", input$rc_germ_cantidad, " semillas germinadas."),
          type = "message")
        updateNumericInput(session, "rc_germ_id",       value = NA)
        updateNumericInput(session, "rc_germ_cantidad", value = NA)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    return(resultados)
  })
}
