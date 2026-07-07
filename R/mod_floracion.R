# ==============================================================================
# MOD_FLORACION.R — Módulo de Gestión de Floración
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readxl)
library(RSQLite)

# --- UI del Módulo ---
mod_floracion_ui <- function(id) {
  ns <- NS(id)
  
  navset_card_underline(
    title = "Gestión de Floración",
    id = ns("tabs_floracion"),
    
    # --------------------------------------------------------------------------
    # Pestaña 1: Configuración del Maestro de Floración
    # --------------------------------------------------------------------------
    nav_panel(
      title = "Maestro de Floración",
      icon = icon("table"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Carga de Datos",
          width = 300,
          numericInput(ns("up_temp"), "Temporada (Año):", value = as.numeric(format(Sys.Date(), "%Y"))),
          fileInput(ns("up_file"), "Subir Excel de Floración:", accept = c(".xlsx")),
          actionButton(ns("btn_load_master"), "Cargar a Base de Datos", 
                       icon = icon("upload"), class = "btn-success w-100"),
          hr(),
          helpText("El archivo Excel debe contener las columnas: temporada, variedad, activity, sec, num, calles, sx, adapt.")
        ),
        
        card(
          card_header(
            tags$div(
              class = "d-flex align-items-center",
              icon("list-ul"), tags$span(" Catálogo Activo de Floración", class = "ms-2")
            )
          ),
          DT::DTOutput(ns("tabla_master"))
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # Pestaña 2: Chequeos y Monitoreo
    # --------------------------------------------------------------------------
    nav_panel(
      title = "Monitoreo de Chequeos",
      icon = icon("eye"),
      card(
        card_header(
          layout_column_wrap(
            width = 1/2,
            tags$div(
              class = "d-flex align-items-center",
              icon("clipboard-check"), tags$span(" Historial de Chequeos", class = "ms-2 me-3"),
              actionButton(ns("btn_refresh_chequeos"), "Refrescar", class="btn-primary btn-sm", icon=icon("sync"))
            ),
            layout_column_wrap(
              width = 1/3,
              selectInput(ns("fl_filter_temp"), "Temporada:", choices = c("Todas")),
              selectInput(ns("fl_filter_sec"), "Sección:", choices = c("Todas", "A", "B")),
              selectInput(ns("fl_filter_sx"), "Sexo (SX):", choices = c("Todos", "1 (Macho Fuerte)", "2 (Macho Débil)", "3 (Hembra)"))
            )
          )
        ),
        DT::DTOutput(ns("tabla_chequeos"))
      )
    ),
    # --------------------------------------------------------------------------
    # Pestaña 3: Programa de Cartuchos
    # --------------------------------------------------------------------------
    nav_panel(
      title = "Programa de Cartuchos",
      icon  = icon("filter"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Parámetros",
          width = 280,
          helpText("Genera la lista de posiciones que mostraron actividad floral en el chequeo del lunes. Incluye variedades con CA, Emergencia o Adelantados > 0."),
          hr(),
          selectInput(ns("hm_fecha"), "Fecha del Chequeo (Lunes):", choices = c("(Cargando...)" = "")),
          selectInput(ns("hm_sec"),   "Sección:", choices = c("Todas", "A", "B")),
          hr(),
          actionButton(ns("btn_gen_hoja"), "Generar Programa", 
                       icon = icon("filter"), class = "btn-success w-100 mb-2"),
          actionButton(ns("btn_print_hoja"), "Imprimir / PDF",
                       icon = icon("print"), class = "btn-outline-secondary w-100",
                       onclick = "window.print()")
        ),
        card(
          card_header(
            tags$div(
              class = "d-flex align-items-center justify-content-between",
              tags$div(icon("filter"), tags$span(" Programa de Cartuchos — Posiciones con Actividad Floral", class = "ms-2 fw-bold")),
              uiOutput(ns("hm_header_info"))
            )
          ),
          tags$style(HTML("
            @media print {
              .sidebar, nav, .navbar, button, .btn { display: none !important; }
              .card { box-shadow: none !important; border: none !important; }
            }
          ")),
          DT::DTOutput(ns("tabla_hoja_montaje"))
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_floracion_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv_refresh_master <- reactiveVal(0)
    rv_refresh_chequeos <- reactiveVal(0)
    
    # ==========================================================================
    # LÓGICA: MAESTRO DE FLORACIÓN
    # ==========================================================================
    observeEvent(input$btn_load_master, {
      req(input$up_file, input$up_temp)
      
      tryCatch({
        df <- readxl::read_excel(input$up_file$datapath) %>% janitor::clean_names()
        
        # Validación básica de columnas
        req_cols <- c("variedad", "activity", "sec", "num", "calles", "sx", "adapt")
        missing <- setdiff(req_cols, names(df))
        if (length(missing) > 0) {
          stop(paste("Faltan columnas obligatorias:", paste(missing, collapse = ", ")))
        }
        
        df_save <- df %>%
          mutate(
            temporada = as.integer(input$up_temp),
            variedad  = toupper(trimws(as.character(variedad))),
            activity  = as.integer(activity),
            sec       = toupper(trimws(as.character(sec))),
            num       = as.integer(num),
            calles    = as.character(calles),
            sx        = as.integer(sx),
            # Normalizar adapt a español sin importar cómo venga el Excel
            adapt     = dplyr::case_when(
              toupper(trimws(as.character(adapt))) %in% c("GOOD", "BUENO")                  ~ "BUENO",
              toupper(trimws(as.character(adapt))) %in% c("CLAY", "MAL_DRENADO", "MD")      ~ "MAL_DRENADO",
              toupper(trimws(as.character(adapt))) %in% c("ROCKY", "ROCOSO")                ~ "ROCOSO",
              TRUE ~ toupper(trimws(as.character(adapt)))
            )
          ) %>%
          select(temporada, variedad, activity, sec, num, calles, sx, adapt)
        
        # Eliminar maestro anterior de la misma temporada si existía para reemplazarlo
        dbExecute(con, "DELETE FROM floracion_master WHERE temporada = ?", params = list(as.integer(input$up_temp)))
        
        dbWriteTable(con, "floracion_master", df_save, append = TRUE)
        rv_refresh_master(rv_refresh_master() + 1)
        
        showNotification(paste("Se cargaron", nrow(df_save), "variedades al maestro de floración."), type = "message")
      }, error = function(e) {
        showNotification(paste("Error cargando maestro:", e$message), type = "error")
      })
    })
    
    # Tabla de Maestro
    output$tabla_master <- DT::renderDT({
      rv_refresh_master()
      df <- tryCatch(dbReadTable(con, "floracion_master"), error = function(e) data.frame())
      
      shiny::validate(need(nrow(df) > 0, "No hay datos en el maestro de floración."))
      
      df %>%
        select(Temporada = temporada, Sección = sec, Número = num, Variedad = variedad, SX = sx, Activity = activity, Calles = calles, Adapt = adapt) %>%
        DT::datatable(
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 15, scrollX = TRUE)
        ) %>%
        DT::formatStyle("SX",
          backgroundColor = DT::styleEqual(c(1, 2, 3), c("#fce4ec", "#e3f2fd", "#f3e5f5"))
        )
    })
    
    # ==========================================================================
    # LÓGICA: CHEQUEOS Y MONITOREO
    # ==========================================================================
    observeEvent(input$btn_refresh_chequeos, {
      rv_refresh_chequeos(rv_refresh_chequeos() + 1)
    })
    
    chequeos_data <- reactive({
      rv_refresh_chequeos()
      df <- tryCatch(
        dbGetQuery(con,
          "SELECT 
             c.temporada, c.fecha_chequeo, c.dia_semana, c.sec, c.num, c.variedad,
             c.grc_atrasado, c.grc_emergencia, c.grc_adelantado,
             COALESCE(c.sx, m.sx) AS sx,
             c.pct_polen, c.evaluador, c.notas
           FROM floracion_chequeos c
           LEFT JOIN floracion_master m
             ON c.num = m.num AND c.temporada = m.temporada
           ORDER BY c.fecha_chequeo DESC, c.sec, c.num"
        ),
        error = function(e) data.frame()
      )
      df
    })
    
    # Actualizar filtros basados en los datos
    observe({
      df <- chequeos_data()
      if (nrow(df) > 0) {
        updateSelectInput(session, "fl_filter_temp", choices = c("Todas", sort(unique(df$temporada), decreasing = TRUE)))
      }
    })
    
    output$tabla_chequeos <- DT::renderDT({
      df <- chequeos_data()
      
      if (input$fl_filter_temp != "Todas") df <- df %>% filter(temporada == as.integer(input$fl_filter_temp))
      if (input$fl_filter_sec != "Todas") df <- df %>% filter(sec == input$fl_filter_sec)
      if (input$fl_filter_sx != "Todos") {
        sx_val <- as.integer(substr(input$fl_filter_sx, 1, 1))
        df <- df %>% filter(sx == sx_val)
      }
      
      shiny::validate(need(nrow(df) > 0, "No hay chequeos registrados que coincidan con los filtros."))
      
      df %>%
        select(
          Temporada = temporada,
          Fecha = fecha_chequeo,
          Día = dia_semana,
          Sección = sec,
          Número = num,
          Variedad = variedad,
          `GRC (Atrasado)` = grc_atrasado,
          `GRC (Emergencia)` = grc_emergencia,
          `GRC (Adelantado)` = grc_adelantado,
          SX = sx,
          Polen = pct_polen,
          Evaluador = evaluador,
          Notas = notas
        ) %>%
        DT::datatable(
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 20, scrollX = TRUE, order = list(list(1, "desc")))
        ) %>%
        DT::formatStyle("GRC (Emergencia)",
          backgroundColor = DT::styleInterval(c(0), c("transparent", "#dcfce7")),
          fontWeight = DT::styleInterval(c(0), c("normal", "bold"))
        ) %>%
        DT::formatPercentage("Polen", 1)
    })
    
    # ==========================================================================
    # LÓGICA: HOJA DE MONTAJE
    # ==========================================================================
    # Poblar selector de fechas disponibles
    observe({
      fechas <- tryCatch(
        dbGetQuery(con, "SELECT DISTINCT fecha_chequeo FROM floracion_chequeos ORDER BY fecha_chequeo DESC"),
        error = function(e) data.frame()
      )
      if (nrow(fechas) > 0) {
        updateSelectInput(session, "hm_fecha", choices = fechas$fecha_chequeo, selected = fechas$fecha_chequeo[1])
      }
    })
    
    hoja_data <- eventReactive(input$btn_gen_hoja, {
      req(input$hm_fecha, nzchar(input$hm_fecha))
      
      # Filtro: cualquier actividad floral (cartuchos atrasados, emergencia o adelantados > 0)
      q <- sprintf("
        SELECT 
          c.sec            AS Sección,
          c.num            AS Número,
          c.variedad       AS Variedad,
          COALESCE(c.sx, m.sx) AS SX,
          m.adapt          AS Suelo,
          c.grc_atrasado   AS CA,
          c.grc_emergencia AS Emerg,
          c.grc_adelantado AS AD,
          c.pct_polen      AS Polen,
          c.evaluador      AS Evaluador
        FROM floracion_chequeos c
        LEFT JOIN floracion_master m
          ON c.num = m.num AND c.temporada = m.temporada
        WHERE c.fecha_chequeo = '%s'
          AND (c.grc_atrasado > 0 OR c.grc_emergencia > 0 OR c.grc_adelantado > 0)
        ORDER BY c.sec, c.num
      ", input$hm_fecha)
      
      if (input$hm_sec != "Todas") {
        q <- sprintf("
          SELECT 
            c.sec AS Sección, c.num AS Número,
            c.variedad AS Variedad, COALESCE(c.sx, m.sx) AS SX, m.adapt AS Suelo,
            c.grc_atrasado AS CA, c.grc_emergencia AS Emerg,
            c.grc_adelantado AS AD, c.pct_polen AS Polen, c.evaluador AS Evaluador
          FROM floracion_chequeos c
          LEFT JOIN floracion_master m ON c.num = m.num AND c.temporada = m.temporada
          WHERE c.fecha_chequeo = '%s'
            AND (c.grc_atrasado > 0 OR c.grc_emergencia > 0 OR c.grc_adelantado > 0)
            AND c.sec = '%s'
          ORDER BY c.sec, c.num
        ", input$hm_fecha, input$hm_sec)
      }
      
      df <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
      df
    }, ignoreNULL = FALSE)
    
    output$hm_header_info <- renderUI({
      df <- hoja_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      n_h <- sum(df$SX == 3, na.rm = TRUE)
      n_m <- sum(df$SX %in% c(1, 2), na.rm = TRUE)
      tags$div(
        class = "d-flex gap-2",
        tags$span(class = "badge bg-success fs-6", icon("leaf"), " ", nrow(df), " posiciones"),
        tags$span(class = "badge bg-danger",  "♀ ", n_h, " hembras"),
        tags$span(class = "badge bg-primary", "♂ ", n_m, " machos")
      )
    })
    
    output$tabla_hoja_montaje <- DT::renderDT({
      df <- hoja_data()
      shiny::validate(need(!is.null(df) && nrow(df) > 0,
        "Selecciona la fecha del lunes y presiona 'Generar Programa'. Solo aparecen variedades con CA, Emergencia o AD > 0."))
      
      df %>%
        mutate(
          SX_label = case_when(SX == 1 ~ "♂ Fuerte", SX == 2 ~ "♂ Débil", SX == 3 ~ "♀ Hembra", TRUE ~ as.character(SX))
        ) %>%
        select(Sección, Número, Variedad, Sexo = SX_label, Suelo, CA, Emerg, AD, Polen, Evaluador) %>%
        DT::datatable(
          rownames  = FALSE,
          options   = list(
            pageLength = 100, scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = list(list(extend='excel', text='Excel', filename=paste0('programa_cartuchos_', Sys.Date())))
          ),
          extensions = 'Buttons'
        ) %>%
        DT::formatStyle("Emerg",
          backgroundColor = "#dcfce7", fontWeight = "bold", color = "#15803d"
        ) %>%
        DT::formatStyle("CA",
          backgroundColor = DT::styleInterval(0, c("white", "#fef9c3")),
          fontWeight = DT::styleInterval(0, c("normal", "bold"))
        ) %>%
        DT::formatStyle("AD",
          backgroundColor = DT::styleInterval(0, c("white", "#ede9fe")),
          fontWeight = DT::styleInterval(0, c("normal", "bold"))
        ) %>%
        DT::formatStyle("Sexo",
          backgroundColor = DT::styleEqual(
            c("♂ Fuerte", "♂ Débil", "♀ Hembra"),
            c("#dbeafe",  "#e0f2fe",  "#fce7f3")
          )
        )
    })
    
  })
}
