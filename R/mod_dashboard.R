# ==============================================================================
# MOD_DASHBOARD.R — Módulo Shiny: Dashboard de Éxito Genético
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- UI del Módulo ---
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        div(style = "background: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          h2("Análisis del Éxito Genético", style = "margin-top: 0; color: #1a472a; font-weight: bold;"),
          p("Monitoreo en tiempo real del pipeline de selección y desempeño de progenitores.")
        )
      )
    ),
    
    fluidRow(
      valueBoxOutput(ns("vbox_familias"), width = 3),
      valueBoxOutput(ns("vbox_clones"), width = 3),
      valueBoxOutput(ns("vbox_tasa"), width = 3),
      valueBoxOutput(ns("vbox_variedades"), width = 3)
    ),
    
    fluidRow(
      # Columna Izquierda: Embudo y Distribución
      column(7,
        box(
          width = 12, title = "Embudo de Selección (Pipeline)", 
          status = "success", solidHeader = TRUE,
          plotlyOutput(ns("plot_funnel_pipeline"), height = "400px"),
          footer = "Muestra la cantidad de clones únicos que han alcanzado cada etapa."
        ),
        box(
          width = 12, title = "Distribución de Calidad (Brix por Estado)", 
          status = "primary", solidHeader = TRUE,
          plotlyOutput(ns("plot_brix_dist"), height = "350px")
        )
      ),
      
      # Columna Derecha: Rankings
      column(5,
        box(
          width = 12, title = "Top 10 Padres por Éxito de Progenie", 
          status = "info", solidHeader = TRUE,
          plotlyOutput(ns("plot_top_parents"), height = "400px"),
          footer = "Padres con mayor % de hijos promocionados a estados avanzados (Est 3+)."
        ),
        box(
          width = 12, title = "Resumen de Selección por Suelo", 
          status = "warning", solidHeader = TRUE,
          plotlyOutput(ns("plot_soil_summary"), height = "350px")
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # --- Datos Reactivos (Lectura de BD) ---
    data_funnel <- reactive({
      # Conteo consolidado por etapa (solo seleccionados)
      query <- "
        SELECT 'ST1' as etapa, count(*) as n FROM clones_st1 WHERE accion = 'S'
        UNION ALL
        SELECT 'ST2' as etapa, count(*) as n FROM clones_st2 WHERE accion = 'S'
        UNION ALL
        SELECT 'ST3' as etapa, count(*) as n FROM clones_st3 WHERE accion = 'S'
        UNION ALL
        SELECT 'ST4' as etapa, count(*) as n FROM clones_st4 WHERE accion = 'S'
        UNION ALL
        SELECT 'ST5' as etapa, count(*) as n FROM clones_st5 WHERE accion = 'S'
      "
      dbGetQuery(con, query)
    })

    data_pipeline <- reactive({
      # 0. Familias (Base genetica)
      df_evf <- tryCatch(dbGetQuery(con, "SELECT cruce, anio, madre, padre, accion FROM familias_evf"), error = function(e) data.frame())
      
      # 1-5 Clones (Seguimiento)
      clones_all <- data.frame()
      for (st in 1:5) {
        table_name <- validate_stage_table(paste0("clones_st", st))
        res <- tryCatch(dbGetQuery(con, sprintf("SELECT 'ST%s' as etapa, anio_seleccion, anio_cruce, cruce, num_sel, brix, suelo, accion FROM %s", st, table_name)), error = function(e) NULL)
        if (!is.null(res) && nrow(res) > 0) clones_all <- rbind(clones_all, res)
      }
      
      # Promociones
      df_promo <- tryCatch(dbReadTable(con, "promociones"), error = function(e) data.frame())
      
      list(evf = df_evf, clones = clones_all, promo = df_promo)
    })
    
    # --- KPIs ---
    output$vbox_familias <- renderValueBox({
      n <- nrow(data_pipeline()$evf)
      valueBox(n, "Familias en Sistema", icon = icon("users"), color = "purple")
    })
    
    output$vbox_clones <- renderValueBox({
      n <- if(nrow(data_pipeline()$clones) > 0) n_distinct(paste0(data_pipeline()$clones$cruce, data_pipeline()$clones$num_sel)) else 0
      valueBox(n, "Clones Evaluados (Total)", icon = icon("dna"), color = "green")
    })
    
    output$vbox_tasa <- renderValueBox({
      df <- data_pipeline()$evf
      tasa <- if(nrow(df) > 0) round(sum(df$accion == "S") / nrow(df) * 100, 1) else 0
      valueBox(paste0(tasa, "%"), "Tasa de Éxito EVF", icon = icon("chart-line"), color = "blue")
    })
    
    output$vbox_variedades <- renderValueBox({
      n <- nrow(data_pipeline()$promo)
      valueBox(n, "Nuevas Variedades CR", icon = icon("certificate"), color = "yellow")
    })
    
    # --- Gráfico 1: Funnel Pipeline ---
    output$plot_funnel_pipeline <- renderPlotly({
      df <- data_funnel()
      validate(need(nrow(df) > 0, "No hay datos de selección para mostrar el embudo."))
      
      # Conteo por etapa
      counts <- df %>% 
        count(etapa) %>%
        # Asegurar orden
        mutate(etapa = factor(etapa, levels = c("ST1", "ST2", "ST3", "ST4", "ST5"))) %>%
        arrange(etapa)
        
      plot_ly(counts, x = ~n, y = ~etapa, type = 'bar', orientation = 'h',
              marker = list(color = '#27ae60', line = list(color = '#1a472a', width = 1))) %>%
        layout(title = "Embudo de Selección Consolidado",
               xaxis = list(title = "Número de Individuos"),
               yaxis = list(title = ""))
    })
    
    # --- Gráfico 2: Top Parents (Rankings) ---
    output$plot_top_parents <- renderPlotly({
      d <- data_pipeline()
      validate(
        need(nrow(d$evf) > 0, "Se requiere la base de datos de Familias para calcular rankings."),
        need(nrow(d$clones) > 0, "Se requieren datos de seguimiento de Clones.")
      )
      
      # Unir clones con sus padres
      df_join <- d$clones %>%
        left_join(d$evf %>% select(cruce, anio_seleccion, madre, padre), by = c("cruce", "anio_seleccion"))
      
      # Calcular éxito por progenitor (Madre o Padre)
      ranking <- df_join %>%
        pivot_longer(cols = c(madre, padre), names_to = "tipo", values_to = "progenitor") %>%
        filter(!is.na(progenitor) & progenitor != "" & progenitor != "TESTIGO") %>%
        group_by(progenitor) %>%
        summarise(
          puntos = sum(case_when(
            etapa == "CLONES_ST1" ~ 1,
            etapa == "CLONES_ST2" ~ 2,
            etapa == "CLONES_ST3" ~ 5,
            etapa == "CLONES_ST4" ~ 10,
            etapa == "CLONES_ST5" ~ 20,
            TRUE ~ 0
          )),
          n_hijos = n_distinct(paste0(cruce, num_sel))
        ) %>%
        arrange(desc(puntos)) %>%
        head(10)
      
      plot_ly(ranking, x = ~reorder(progenitor, puntos), y = ~puntos, type = 'bar',
              marker = list(color = '#3498db')) %>%
        layout(title = "Top 10 Progenitores (Puntaje Genético)",
               xaxis = list(title = "Progenitor"),
               yaxis = list(title = "Puntos de Éxito (Ponderado)"))
    })
    
    # --- Gráfico 3: Distribución de Brix ---
    output$plot_brix_dist <- renderPlotly({
      d <- data_pipeline()$clones
      validate(need(nrow(d) > 0, "No hay datos de brix registrados en los estados de selección."))
      
      plot_ly(d, y = ~brix, x = ~etapa, type = "box", color = ~etapa,
              boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
        layout(title = "Variabilidad de Brix por Etapa de Selección",
               yaxis = list(title = "Brix (%)"),
               xaxis = list(title = "Etapa"))
    })
    
    # --- Gráfico 4: Resumen Suelo ---
    output$plot_soil_summary <- renderPlotly({
      d <- data_pipeline()$clones
      validate(need(nrow(d) > 0, "Esperando datos por suelo..."))
      
      soil_df <- d %>%
        group_by(suelo, etapa) %>%
        summarise(n = n(), .groups = 'drop')
      
      plot_ly(soil_df, x = ~etapa, y = ~n, color = ~suelo, type = 'bar') %>%
        layout(barmode = 'stack', title = "Distribución de Clones por Suelo")
    })
    
  })
}
