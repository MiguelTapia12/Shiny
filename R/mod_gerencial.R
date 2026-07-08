# MOD_GERENCIAL.R — Panel Gerencial (Centro de Mando)
# Muestra KPIs globales y avance de la selección

mod_gerencial_ui <- function(id) {
  ns <- NS(id)
  
  div(class = "p-3",
    tagList(
      # Encabezado
      fluidRow(
      column(12,
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          h2(tags$span(`data-i18n`="mgr_title", "Panel Gerencial \u2014 Centro de Mando"), class = "m-0 fw-bold text-success"),
          downloadButton(ns("descargar_pdf"), tags$span(`data-i18n`="btn_export_pdf", "Exportar Reporte PDF"), class = "btn btn-outline-success")
        )
      )
    ),
    
    # KPIs (ValueBoxes)
    fluidRow(
      column(4, uiOutput(ns("kpi_semillas"))),
      column(4, uiOutput(ns("kpi_volumen"))),
      column(4, uiOutput(ns("kpi_cruces")))
    ),
    
    br(),
    
    # Filtros Globales
    fluidRow(
      column(12,
        card(
          card_header(
            tags$b(icon("filter"), tags$span(`data-i18n`="mgr_global_filters", " Filtros Globales")),
            class = "bg-light text-dark"
          ),
          card_body(
            fluidRow(
              column(3,
                selectInput(ns("filtro_anio"), tags$span(`data-i18n`="lbl_eval_year", "Año de Evaluación:"), 
                            choices = c("Todos", seq(as.integer(format(Sys.Date(), "%Y")), 2020, -1)), 
                            selected = "Todos")
              ),
              column(3,
                selectInput(ns("filtro_programa"), tags$span(`data-i18n`="lbl_program", "Programa:"), 
                            choices = c("Todos", "Central Romana (CR)", "Barbados Romana (BR)"), 
                            selected = "Todos")
              ),
              column(3,
                selectInput(ns("filtro_suelo"), tags$span(`data-i18n`="lbl_soil_type", "Tipo de Suelo:"), 
                            choices = c("Todos", "BUENO", "MAL_DRENADO", "ROCOSO"), 
                            selected = "Todos")
              ),
              column(3,
                actionButton(ns("btn_filtrar"), tags$span(`data-i18n`="btn_apply_filters", "Aplicar Filtros"), icon = icon("check"), class = "btn-success mt-4 w-100")
              )
            )
          )
        )
      )
    ),
    
    br(),
    
    # Gráficos — Fila 1: Embudo + Top Familias
    fluidRow(
      # Embudo de Selección (mejorado con tasa)
      column(6,
        card(
          card_header(
            tags$b(icon("filter"), tags$span(`data-i18n`="mgr_funnel_chart", " Embudo de Selección y Tasa de Promoción")),
            class = "bg-success text-white"
          ),
          card_body(
            plotlyOutput(ns("plot_funnel"), height = "400px") %>% withSpinner(color = "#16a34a")
          )
        )
      ),
      # Top Familias
      column(6,
        card(
          card_header(
            tags$b(icon("trophy"), tags$span(`data-i18n`="mgr_top_families", " Top 10 Familias Exitosas")),
            class = "bg-success text-white"
          ),
          card_body(
            plotlyOutput(ns("plot_familias"), height = "400px") %>% withSpinner(color = "#16a34a")
          )
        )
      )
    ),
    
    br(),
    
    # Gráficos — Fila 2: Tendencia Genética + Brix vs Vigor
    fluidRow(
      column(6,
        card(
          card_header(
            tags$b(icon("chart-line"), tags$span(`data-i18n`="mgr_genetic_trend", " Tendencia Genética Anual (Brix & Vigor)")),
            class = "bg-success text-white"
          ),
          card_body(
            plotlyOutput(ns("plot_tendencia"), height = "400px") %>% withSpinner(color = "#16a34a")
          )
        )
      ),
      column(6,
        card(
          card_header(
            tags$b(icon("crosshairs"), tags$span(`data-i18n`="mgr_brix_vigor_matrix", " Matriz Brix vs. Vigor — Selección Élite")),
            class = "bg-success text-white"
          ),
          card_body(
            plotlyOutput(ns("plot_brix_vigor"), height = "400px") %>% withSpinner(color = "#16a34a")
          )
        )
      )
    ),
    
    br(),
    
    # Gráficos — Fila 3: Incidencia de Enfermedades Global
    fluidRow(
      column(12,
        card(
          card_header(
            tags$b(icon("virus"), tags$span(`data-i18n`="mgr_disease_incidence", " Incidencia de Enfermedades a Nivel Programa")),
            class = "bg-success text-white"
          ),
          card_body(
            plotlyOutput(ns("plot_sanidad"), height = "360px") %>% withSpinner(color = "#16a34a")
          )
        )
      )
    )
  )
  )
}

mod_gerencial_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ─── DATOS REACTIVOS ───────────────────────────────────────────────────────
    
    # Obtener inventario de semillas
    total_semillas <- reactive({
      tryCatch({
        df <- dbGetQuery(con, "SELECT gramos_retirados FROM historial_fuzz")
        sum(as.numeric(df$gramos_retirados), na.rm = TRUE)
      }, error = function(e) 0)
    })
    
    # Obtener volumen total en selección (ST1 - ST5) — QUERY UNIFICADA
    # Una sola consulta trae todo; las derivadas filtran en memoria
    datos_all_clones <- reactive({
      input$btn_filtrar
      all_clones <- data.frame()
      for (st in 1:5) {
        tbl <- paste0("clones_st", st)
        if (dbExistsTable(con, tbl)) {
          q <- sprintf(
            "SELECT '%s' AS estado, programa, cruce, num_sel, brix, vigor,
                    suelo AS tipo_suelo, accion, anio_seleccion
             FROM %s",
            paste0("ST", st), tbl
          )
          res <- tryCatch(dbGetQuery(con, q), error = function(e) NULL)
          if (!is.null(res) && nrow(res) > 0) all_clones <- rbind(all_clones, res)
        }
      }
      all_clones
    }) %>% bindCache(input$btn_filtrar)
    
    # Derivada: datos filtrados para embudo y familias
    datos_estados <- reactive({
      df <- datos_all_clones()
      if (nrow(df) == 0) return(df)
      
      df <- df %>% filter(accion != "R")
      
      anio_f <- isolate(input$filtro_anio)
      if (!is.null(anio_f) && anio_f != "Todos")
        df <- df %>% filter(anio_seleccion == as.integer(anio_f))
      
      df <- df %>% mutate(programa = case_when(
        grepl("^CR", programa, ignore.case = TRUE) ~ "Central Romana (CR)",
        grepl("^BR", programa, ignore.case = TRUE) ~ "Barbados Romana (BR)",
        TRUE ~ "Otros"
      ))
      
      prog_f <- isolate(input$filtro_programa)
      if (!is.null(prog_f) && prog_f != "Todos")
        df <- df %>% filter(programa == prog_f)
      
      suelo_f <- isolate(input$filtro_suelo)
      if (!is.null(suelo_f) && suelo_f != "Todos")
        df <- df %>% filter(grepl(suelo_f, toupper(tipo_suelo)))
      
      df
    })
    
    # Derivada: tendencia genética (desde cache)
    datos_tendencia <- reactive({
      df <- datos_all_clones()
      if (nrow(df) == 0) return(data.frame())
      
      df %>%
        filter(accion == "S", !is.na(anio_seleccion), !is.na(brix)) %>%
        group_by(anio_seleccion) %>%
        summarise(
          brix_prom  = round(mean(brix, na.rm = TRUE), 2),
          vigor_prom = round(mean(vigor, na.rm = TRUE), 2),
          n_clones   = n(),
          .groups = "drop"
        ) %>%
        arrange(anio_seleccion)
    })
    
    # Derivada: brix vs vigor scatter (desde cache)
    datos_brix_vigor <- reactive({
      df <- datos_all_clones()
      if (nrow(df) == 0) return(data.frame())
      
      df %>%
        filter(accion == "S", !is.na(brix), !is.na(vigor)) %>%
        rename(etapa = estado) %>%
        mutate(clon_id = paste0(cruce, "-", num_sel))
    })
    
    # Obtener cruces totales
    total_cruces <- reactive({
      tryCatch({
        df <- dbGetQuery(con, "SELECT COUNT(*) as n FROM familias_evf")
        if(nrow(df) > 0) df$n[1] else 0
      }, error = function(e) 0)
    })
    
    # Top 10 Familias (derivada desde cache)
    datos_top_familias <- reactive({
      df <- datos_estados()
      if(is.null(df) || nrow(df) == 0) return(data.frame())
      
      df %>%
        filter(estado %in% c("ST2", "ST3", "ST4", "ST5")) %>%
        mutate(familia = cruce) %>%
        filter(!is.na(familia) & familia != "") %>%
        group_by(familia) %>%
        summarise(clones_vivos = n(), .groups = "drop") %>%
        arrange(desc(clones_vivos)) %>%
        head(10)
    })
    
    # ─── DATOS: Incidencia de Enfermedades ──────────────────────────────────
    datos_sanidad_global <- reactive({
      input$btn_filtrar
      tryCatch({
        q <- "SELECT programa, 
                     AVG(carbon_porcentaje) AS carbon_prom, 
                     AVG(escaldadura_porcentaje) AS escaldadura_prom, 
                     AVG(roya_porcentaje) AS roya_prom,
                     COUNT(*) AS n_eval
              FROM evaluacion_enfermedades
              GROUP BY programa"
        df <- dbGetQuery(con, q)
        if (nrow(df) == 0) return(data.frame())
        df %>% mutate(across(c(carbon_prom, escaldadura_prom, roya_prom), ~ round(.x, 2)))
      }, error = function(e) data.frame())
    })
    
    # ─── DATOS: Embudo con Tasa de Selección ────────────────────────────────
    datos_embudo <- reactive({
      df <- datos_estados()
      if (is.null(df) || nrow(df) == 0) return(data.frame())
      
      resumen <- df %>%
        mutate(estado = factor(estado, levels = c("ST1", "ST2", "ST3", "ST4", "ST5"))) %>%
        group_by(estado) %>%
        summarise(cantidad = n(), .groups = "drop") %>%
        arrange(estado)
      
      if (nrow(resumen) == 0) return(data.frame())
      
      # Calcular tasa de selección (% que pasa de una etapa a la siguiente)
      resumen <- resumen %>%
        mutate(
          tasa_sel = ifelse(
            lag(cantidad) > 0,
            round((cantidad / lag(cantidad)) * 100, 1),
            NA_real_
          ),
          label = ifelse(
            is.na(tasa_sel),
            paste0(estado, ": ", cantidad, " clones"),
            paste0(estado, ": ", cantidad, " (", tasa_sel, "% del anterior)")
          )
        )
      
      resumen
    })
    
    # ─── RENDER KPIs ─────────────────────────────────────────────────────────
    output$kpi_semillas <- renderUI({
      valueBox(
        value = format(total_semillas(), big.mark = ","),
        subtitle = "Semillas en Inventario (Fuzz)",
        icon = icon("seedling"),
        color = "aqua",
        width = 12
      )
    })
    
    output$kpi_volumen <- renderUI({
      df <- datos_estados()
      total <- if(is.null(df) || nrow(df) == 0) 0 else nrow(df)
      valueBox(
        value = format(total, big.mark = ","),
        subtitle = "Clones en Pipeline (ST1-ST5)",
        icon = icon("layer-group"),
        color = "green",
        width = 12
      )
    })
    
    output$kpi_cruces <- renderUI({
      valueBox(
        value = format(total_cruces(), big.mark = ","),
        subtitle = "Cruces Realizados (Histórico)",
        icon = icon("dna"),
        color = "yellow",
        width = 12
      )
    })
    
    # ─── RENDER: Embudo de Selección (mejorado con tasa) ─────────────────────
    output$plot_funnel <- renderPlotly({
      resumen <- datos_embudo()
      
      if(is.null(resumen) || nrow(resumen) == 0) {
        return(plot_ly() %>% layout(title = "No hay datos para el embudo"))
      }
      
      fig <- plot_ly(
        type = "funnel",
        y = resumen$estado,
        x = resumen$cantidad,
        text = resumen$label,
        textinfo = "text",
        hoverinfo = "text",
        marker = list(color = c("#0b5c2e", "#16a34a", "#22c55e", "#4ade80", "#86efac")[1:nrow(resumen)])
      )
      
      fig <- fig %>% layout(
        yaxis = list(categoryarray = c("ST1", "ST2", "ST3", "ST4", "ST5")),
        margin = list(l = 60, r = 20, t = 20, b = 20)
      )
      
      fig
    })
    
    # ─── RENDER: Top 10 Familias ─────────────────────────────────────────────
    output$plot_familias <- renderPlotly({
      df <- datos_top_familias()
      
      if(is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(title = "No hay datos de familias en etapas avanzadas"))
      }
      
      # Ordenar para Plotly (ascendente para barras horizontales)
      df <- df %>% arrange(clones_vivos)
      
      fig <- plot_ly(
        x = ~df$clones_vivos, 
        y = ~factor(df$familia, levels = df$familia), 
        type = 'bar', 
        orientation = 'h',
        marker = list(color = '#16a34a'),
        text = ~paste0(df$familia, "  (", df$clones_vivos, ")"),
        textposition = "inside",
        insidetextanchor = "start",
        textfont = list(color = "#fff", size = 12, family = "Inter"),
        hovertemplate = "Cruce: %{y}<br>Clones vivos: %{x}<extra></extra>"
      )
      
      fig <- fig %>% layout(
        xaxis = list(title = "Nº de Clones Vivos (ST2+)"),
        yaxis = list(
          title = "", 
          tickfont = list(size = 12),
          automargin = TRUE
        ),
        margin = list(l = 80, r = 20, t = 40, b = 40)
      )
      
      fig
    })
    
    # ─── RENDER: Tendencia Genética Anual ────────────────────────────────────
    output$plot_tendencia <- renderPlotly({
      df <- datos_tendencia()
      
      if (is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(title = "Sin datos de tendencia genética"))
      }
      
      fig <- plot_ly(df, x = ~anio_seleccion) %>%
        add_trace(
          y = ~brix_prom, name = "Brix Promedio",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#0b5c2e", width = 3),
          marker = list(color = "#0b5c2e", size = 9),
          hovertemplate = "Año: %{x}<br>Brix: %{y}<extra></extra>"
        ) %>%
        add_trace(
          y = ~vigor_prom, name = "Vigor Promedio",
          type = "scatter", mode = "lines+markers",
          yaxis = "y2",
          line = list(color = "#d97706", width = 3, dash = "dot"),
          marker = list(color = "#d97706", size = 9, symbol = "diamond"),
          hovertemplate = "Año: %{x}<br>Vigor: %{y}<extra></extra>"
        )
      
      fig <- fig %>% layout(
        xaxis = list(title = "Año de Selección", dtick = 1),
        yaxis = list(title = "Brix Promedio", titlefont = list(color = "#0b5c2e")),
        yaxis2 = list(
          title = "Vigor Promedio", overlaying = "y", side = "right",
          titlefont = list(color = "#d97706"), tickfont = list(color = "#d97706"),
          range = c(1, 9)
        ),
        legend = list(orientation = "h", x = 0.1, y = 1.12),
        margin = list(l = 60, r = 60, t = 30, b = 50),
        hovermode = "x unified"
      )
      
      fig
    })
    
    # ─── RENDER: Brix vs Vigor (Scatter) ─────────────────────────────────────
    output$plot_brix_vigor <- renderPlotly({
      df <- datos_brix_vigor()
      
      if (is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(title = "Sin datos de Brix/Vigor"))
      }
      
      # Paleta por etapa
      cols <- c("ST1" = "#86efac", "ST2" = "#4ade80", "ST3" = "#22c55e", "ST4" = "#16a34a", "ST5" = "#0b5c2e")
      
      # Medianas para cuadrantes
      med_brix  <- median(df$brix, na.rm = TRUE)
      med_vigor <- median(df$vigor, na.rm = TRUE)
      
      fig <- plot_ly(df,
        x = ~brix, y = ~vigor,
        color = ~etapa, colors = cols,
        type = "scatter", mode = "markers",
        marker = list(size = 8, opacity = 0.75, line = list(width = 1, color = "#333")),
        text = ~clon_id,
        hovertemplate = "Clon: %{text}<br>Brix: %{x}<br>Vigor: %{y}<extra>%{fullData.name}</extra>"
      )
      
      # Líneas de referencia (medianas) para crear cuadrantes
      fig <- fig %>% layout(
        xaxis = list(title = "Brix"),
        yaxis = list(title = "Vigor", range = c(0.5, 9.5), dtick = 1),
        legend = list(title = list(text = "Etapa"), orientation = "h", x = 0.1, y = 1.12),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        shapes = list(
          # Línea vertical: mediana de Brix
          list(type = "line", x0 = med_brix, x1 = med_brix, y0 = 0.5, y1 = 9.5,
               line = list(dash = "dash", color = "#94a3b8", width = 1.5)),
          # Línea horizontal: mediana de Vigor
          list(type = "line", x0 = min(df$brix, na.rm = TRUE) - 1, x1 = max(df$brix, na.rm = TRUE) + 1, 
               y0 = med_vigor, y1 = med_vigor,
               line = list(dash = "dash", color = "#94a3b8", width = 1.5))
        ),
        annotations = list(
          list(
            x = max(df$brix, na.rm = TRUE), y = 9, text = "\u2B50 Élite",
            showarrow = FALSE, font = list(color = "#0b5c2e", size = 13, weight = "bold"),
            xanchor = "right"
          ),
          list(
            x = min(df$brix, na.rm = TRUE), y = 1, text = "Descarte \u274C",
            showarrow = FALSE, font = list(color = "#dc2626", size = 12),
            xanchor = "left"
          )
        )
      )
      
      fig
    })
    
    # ─── RENDER: Incidencia de Enfermedades Global ───────────────────────────
    output$plot_sanidad <- renderPlotly({
      df <- datos_sanidad_global()
      
      if (is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(title = "Sin datos de evaluación de enfermedades"))
      }
      
      # Convertir a formato largo para graficar
      df_long <- df %>%
        tidyr::pivot_longer(
          cols = c(carbon_prom, escaldadura_prom, roya_prom),
          names_to = "enfermedad", values_to = "incidencia"
        ) %>%
        mutate(
          enfermedad = case_when(
            enfermedad == "carbon_prom"       ~ "Carbón",
            enfermedad == "escaldadura_prom"  ~ "Escaldadura",
            enfermedad == "roya_prom"         ~ "Roya",
            TRUE ~ enfermedad
          )
        )
      
      cols_enf <- c("Carbón" = "#1e293b", "Escaldadura" = "#d97706", "Roya" = "#b45309")
      
      fig <- plot_ly(df_long,
        x = ~programa, y = ~incidencia,
        color = ~enfermedad, colors = cols_enf,
        type = "bar",
        text = ~paste0(incidencia, "%"),
        textposition = "outside",
        hovertemplate = "%{x}<br>%{fullData.name}: %{y:.1f}%<extra></extra>"
      )
      
      fig <- fig %>% layout(
        barmode = "group",
        xaxis = list(title = ""),
        yaxis = list(title = "Incidencia Promedio (%)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0.2, y = 1.1),
        margin = list(l = 60, r = 20, t = 40, b = 40),
        # Líneas de umbral
        shapes = list(
          list(type = "line", x0 = -0.5, x1 = nrow(df) - 0.5, y0 = 5, y1 = 5,
               line = list(dash = "dash", color = "#d97706", width = 1.5)),
          list(type = "line", x0 = -0.5, x1 = nrow(df) - 0.5, y0 = 10, y1 = 10,
               line = list(dash = "dash", color = "#dc2626", width = 1.5))
        ),
        annotations = list(
          list(x = nrow(df) - 0.6, y = 5.5, text = "Alerta (5%)", showarrow = FALSE,
               font = list(color = "#d97706", size = 10)),
          list(x = nrow(df) - 0.6, y = 10.5, text = "Crítico (10%)", showarrow = FALSE,
               font = list(color = "#dc2626", size = 10))
        )
      )
      
      fig
    })
    
    # ─── GENERAR REPORTE PDF ─────────────────────────────────────────────────
    output$descargar_pdf <- downloadHandler(
      filename = function() {
        paste("Reporte_Gerencial_CR_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Mostrar notificación
        showNotification("Generando reporte PDF... esto puede tomar unos segundos.", type = "message", duration = 5)
        
        # Ruta a la plantilla Rmd
        tempReport <- file.path(tempdir(), "reporte_gerencial.Rmd")
        file.copy("reporte_gerencial.Rmd", tempReport, overwrite = TRUE)
        
        # Preparar parámetros
        params <- list(
          semillas = total_semillas(),
          volumen = nrow(datos_estados()),
          cruces = total_cruces(),
          programa = input$filtro_programa,
          suelo = input$filtro_suelo,
          anio = input$filtro_anio,
          datos_funnel = datos_estados(),
          datos_familias = datos_top_familias()
        )
        
        # Compilar reporte
        rmarkdown::render(
          tempReport, 
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          clean = TRUE
        )
      }
    )
    
  })
}
