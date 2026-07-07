# ==============================================================================
# MOD_DASHBOARD.R — Dashboard de Campaña Activa
# Pipeline de Selección Genética — Central Romana v3.0
# ==============================================================================

# --- UI ---
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  div(class = "p-3",
    tagList(
      # ── Encabezado ────────────────────────────────────────────────────────
      fluidRow(
        column(12,
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            div(
              h2("\U0001F3AF ", tags$span(`data-i18n`="dash_title", "Dashboard de Campaña"), class = "m-0 fw-bold text-success"),
              p(tags$span(`data-i18n`="dash_subtitle", "Progreso en tiempo real de la campaña de selección."), 
                class = "text-muted mb-0", style = "font-size:0.9em;")
            ),
            div(
              class = "d-flex gap-2",
              downloadButton(ns("descargar_csv"), tags$span(`data-i18n`="dash_export_csv", "Exportar CSV"), 
                             class = "btn btn-outline-success btn-sm")
            )
          )
        )
      ),
      
      # ── Filtros ────────────────────────────────────────────────────────────
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("filter"), tags$span(`data-i18n`="dash_filters", " Filtros de Campaña")),
              class = "bg-light text-dark"
            ),
            card_body(
              fluidRow(
                column(4,
                  selectInput(
                    ns("sel_anio"), tags$span(`data-i18n`="lbl_eval_year", "Año de Evaluación:"),
                    choices  = c(as.integer(format(Sys.Date(), "%Y")),
                                 seq(as.integer(format(Sys.Date(), "%Y")) - 1, 2020, -1)),
                    selected = as.integer(format(Sys.Date(), "%Y")),
                    width = "100%"
                  )
                ),
                column(4,
                  selectInput(
                    ns("sel_programa"), tags$span(`data-i18n`="lbl_program", "Programa:"),
                    choices = c("Todos"),
                    width = "100%"
                  )
                ),
                column(4,
                  selectInput(
                    ns("sel_suelo"), tags$span(`data-i18n`="lbl_soil_type", "Tipo de Suelo:"),
                    choices = c("Todos", "BUENO", "MAL_DRENADO", "ROCOSO"),
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      ),
      
      br(),
      
      # ── Value Boxes ────────────────────────────────────────────────────────
      uiOutput(ns("ui_vboxes")),
      
      br(),
      
      # ── Fila 1: Pipeline + Tasa por Cruce ──────────────────────────────────
      fluidRow(
        column(6,
          card(
            card_header(
              tags$b(icon("layer-group"), tags$span(`data-i18n`="dash_clones_stage_decision", " Clones por Etapa y Decisión")),
              class = "bg-success text-white"
            ),
            card_body(
              plotlyOutput(ns("plot_pipeline"), height = "380px") %>% 
                withSpinner(color = "#16a34a")
            ),
            card_footer(class = "text-muted small", 
              tags$span(`data-i18n`="dash_clones_stage_desc", "Barras agrupadas S/T/R por etapa en la campaña seleccionada.")
            )
          )
        ),
        column(6,
          card(
            card_header(
              tags$b(icon("ranking-star"), tags$span(`data-i18n`="dash_selection_rate", " Tasa de Selección por Cruce (Top 20)")),
              class = "bg-success text-white"
            ),
            card_body(
              plotlyOutput(ns("plot_tasa_cruce"), height = "380px") %>% 
                withSpinner(color = "#16a34a")
            ),
            card_footer(class = "text-muted small", 
              tags$span(`data-i18n`="dash_selection_rate_desc", "% de clones Seleccionados sobre el total evaluado por cruce.")
            )
          )
        )
      ),
      
      br(),
      
      # ── Fila 2: Tendencia Semanal + Distribución de Brix ───────────────────
      fluidRow(
        column(6,
          card(
            card_header(
              tags$b(icon("chart-area"), tags$span(`data-i18n`="dash_weekly_captures", " Capturas Semanales (App de Campo)")),
              class = "bg-success text-white"
            ),
            card_body(
              plotlyOutput(ns("plot_tendencia"), height = "350px") %>% 
                withSpinner(color = "#16a34a")
            ),
            card_footer(class = "text-muted small",
              tags$span(`data-i18n`="dash_weekly_captures_desc", "Clones registrados por semana vía app móvil durante la campaña.")
            )
          )
        ),
        column(6,
          card(
            card_header(
              tags$b(icon("chart-bar"), tags$span(`data-i18n`="dash_brix_dist", " Distribución de Brix en Seleccionados")),
              class = "bg-success text-white"
            ),
            card_body(
              plotlyOutput(ns("plot_brix_dist"), height = "350px") %>% 
                withSpinner(color = "#16a34a")
            ),
            card_footer(class = "text-muted small",
              tags$span(`data-i18n`="dash_brix_dist_desc", "Histograma de Brix para clones con acción 'S' en la campaña.")
            )
          )
        )
      ),
      
      br(),
      
      # ── Fila 3: Top Cruces por Brix ────────────────────────────────────────
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("droplet"), tags$span(`data-i18n`="dash_top_brix", " Top 10 Cruces por Brix Promedio (Seleccionados)")),
              class = "bg-success text-white"
            ),
            card_body(
              plotlyOutput(ns("plot_top_brix"), height = "360px") %>%
                withSpinner(color = "#16a34a")
            )
          )
        )
      )
    )
  )
}

# --- Server ---
mod_dashboard_server <- function(id, con, df_categorias_rv) {
  moduleServer(id, function(input, output, session) {
    
    # ── Datos base: query unificada y cacheada ─────────────────────────────
    data_campana <- reactive({
      anio <- as.integer(input$sel_anio)
      all_clones <- data.frame()
      
      for (st in 1:5) {
        tbl <- paste0("clones_st", st)
        if (dbExistsTable(con, tbl)) {
          q   <- sprintf(
            "SELECT '%s' AS etapa, programa, cruce, num_sel,
                    brix, vigor, suelo, accion, anio_seleccion
             FROM %s
             WHERE anio_seleccion = %d",
            paste0("ST", st), tbl, anio
          )
          res <- tryCatch(dbGetQuery(con, q), error = function(e) NULL)
          if (!is.null(res) && nrow(res) > 0)
            all_clones <- rbind(all_clones, res)
        }
      }
      all_clones
    })
    
    # Datos field_captures del año
    data_fc <- reactive({
      anio <- as.integer(input$sel_anio)
      tryCatch(
        dbGetQuery(con,
                   sprintf("SELECT * FROM field_captures WHERE anio_seleccion = %d", anio)),
        error = function(e) data.frame()
      )
    })
    
    # Filtrado por programa y suelo
    data_filtrado <- reactive({
      df <- data_campana()
      if (nrow(df) == 0) return(df)
      prog <- input$sel_programa
      if (!is.null(prog) && prog != "Todos")
        df <- df %>% filter(toupper(programa) == toupper(prog))
      suelo <- input$sel_suelo
      if (!is.null(suelo) && suelo != "Todos")
        df <- df %>% filter(grepl(suelo, toupper(suelo)))
      df
    })
    
    # Poblar dropdowns dinámicamente
    observe({
      df <- data_campana()
      if (nrow(df) == 0) return()
      progs <- c("Todos", sort(unique(df$programa[!is.na(df$programa)])))
      updateSelectInput(session, "sel_programa", choices = progs)
    })
    
    # ── Value Boxes ───────────────────────────────────────────────────────
    output$ui_vboxes <- renderUI({
      df  <- data_filtrado()
      fc  <- data_fc()
      
      total   <- nrow(df)
      sel     <- sum(df$accion == "S", na.rm = TRUE)
      test    <- sum(df$accion == "T", na.rm = TRUE)
      rej     <- sum(df$accion == "R", na.rm = TRUE)
      tasa    <- if (total > 0) paste0(round(sel / total * 100, 1), "%") else "\u2014"
      brix_prom <- if (sel > 0) round(mean(df$brix[df$accion == "S"], na.rm = TRUE), 1) else "\u2014"
      fc_tot  <- nrow(fc)
      
      fluidRow(
        column(2,
          value_box(
            title    = "Evaluados",
            value    = format(total, big.mark = ","),
            showcase = icon("dna"),
            theme    = "primary",
            showcase_layout = "left center"
          )
        ),
        column(2,
          value_box(
            title    = "Seleccionados",
            value    = format(sel, big.mark = ","),
            showcase = icon("check-double"),
            theme    = "success",
            showcase_layout = "left center"
          )
        ),
        column(2,
          value_box(
            title    = "Rechazados",
            value    = format(rej, big.mark = ","),
            showcase = icon("circle-xmark"),
            theme    = "danger",
            showcase_layout = "left center"
          )
        ),
        column(2,
          value_box(
            title    = "Tasa Selección",
            value    = tasa,
            showcase = icon("chart-line"),
            theme    = "info",
            showcase_layout = "left center"
          )
        ),
        column(2,
          value_box(
            title    = "Brix Prom (S)",
            value    = brix_prom,
            showcase = icon("droplet"),
            theme    = "warning",
            showcase_layout = "left center"
          )
        ),
        column(2,
          value_box(
            title    = "Capturas Móvil",
            value    = format(fc_tot, big.mark = ","),
            showcase = icon("mobile-screen"),
            theme    = "secondary",
            showcase_layout = "left center"
          )
        )
      )
    })
    
    # ── Gráfico 1: Barras agrupadas por etapa ─────────────────────────────
    output$plot_pipeline <- renderPlotly({
      df <- data_filtrado()
      shiny::validate(need(
        nrow(df) > 0,
        "No hay clones registrados para esta campaña."
      ))
      
      resumen <- df %>%
        mutate(
          etapa  = factor(etapa, levels = paste0("ST", 1:5)),
          accion = factor(accion, levels = c("S", "T", "R"),
                          labels = c("Seleccionado", "Testigo", "Rechazado"))
        ) %>%
        group_by(etapa, accion) %>%
        summarise(n = n(), .groups = "drop")
      
      colores <- c(
        "Seleccionado" = "#16a34a",
        "Testigo"      = "#d97706",
        "Rechazado"    = "#dc2626"
      )
      
      plot_ly(
        resumen,
        x     = ~etapa,
        y     = ~n,
        color = ~accion,
        colors = colores,
        type  = "bar",
        text  = ~n,
        textposition = "auto",
        hovertemplate = "%{x} · %{fullData.name}: %{y}<extra></extra>"
      ) %>%
        layout(
          barmode = "group",
          bargap = 0.2,
          xaxis   = list(title = "Etapa"),
          yaxis   = list(title = "Número de Clones"),
          legend  = list(orientation = "h", x = 0.15, y = 1.1),
          margin  = list(l = 60, r = 20, t = 30, b = 50)
        )
    })
    
    # ── Gráfico 2: Tasa de selección por cruce ────────────────────────────
    output$plot_tasa_cruce <- renderPlotly({
      df <- data_filtrado()
      shiny::validate(need(nrow(df) > 0, "No hay datos de cruces para esta campaña."))
      
      tasa_cruce <- df %>%
        group_by(cruce) %>%
        summarise(
          total = n(),
          sel   = sum(accion == "S", na.rm = TRUE),
          tasa  = round(sel / total * 100, 1),
          .groups = "drop"
        ) %>%
        filter(total >= 2) %>%
        arrange(desc(tasa), desc(total)) %>%
        head(20)
      
      shiny::validate(need(nrow(tasa_cruce) > 0, "No hay suficientes datos por cruce."))
      
      plot_ly(tasa_cruce, x = ~reorder(cruce, -tasa)) %>%
        add_bars(y = ~total, name = "Evaluados", 
                 marker = list(color = "#cbd5e1"),
                 hovertemplate = "Evaluados: %{y}<extra></extra>") %>%
        add_bars(y = ~sel, name = "Seleccionados", 
                 marker = list(color = "#16a34a"),
                 text = ~paste0(tasa, "%"),
                 textposition = "outside",
                 textfont = list(size = 10, color = "#0b5c2e"),
                 hovertemplate = "Sel: %{y} (%{text})<extra></extra>") %>%
        layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Clones"),
          legend = list(orientation = "h", x = 0.2, y = 1.1),
          margin = list(b = 80, t = 30)
        )
    })
    
    # ── Gráfico 3: Tendencia semanal (área) ──────────────────────────────
    output$plot_tendencia <- renderPlotly({
      fc <- data_fc()
      shiny::validate(need(nrow(fc) > 0, "No hay registros de la app móvil para este año."))
      
      fc_sem <- fc %>%
        mutate(
          fecha  = as.Date(substr(ts, 1, 10)),
          semana = format(fecha, "%Y-W%V")
        ) %>%
        group_by(semana) %>%
        summarise(
          capturas = n(),
          sel      = sum(accion == "S", na.rm = TRUE),
          fecha_min = min(fecha),
          .groups  = "drop"
        ) %>%
        arrange(fecha_min)
      
      plot_ly(fc_sem, x = ~semana) %>%
        add_trace(
          y    = ~capturas,
          name = "Total Capturas",
          type = "scatter", mode = "none",
          fill = "tozeroy",
          fillcolor = "rgba(134,239,172,0.35)",
          line = list(color = "#86efac")
        ) %>%
        add_trace(
          y    = ~sel,
          name = "Seleccionados",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#0b5c2e", width = 2.5),
          marker = list(color = "#0b5c2e", size = 7)
        ) %>%
        layout(
          xaxis  = list(title = "Semana", tickangle = -45, tickfont = list(size = 9)),
          yaxis  = list(title = "Clones"),
          legend = list(orientation = "h", x = 0.15, y = 1.1),
          margin = list(b = 80, t = 30),
          hovermode = "x unified"
        )
    })
    
    # ── Gráfico 4: Distribución de Brix (Histograma) ─────────────────────
    output$plot_brix_dist <- renderPlotly({
      df <- data_filtrado()
      shiny::validate(need(nrow(df) > 0, "No hay datos."))
      
      df_sel <- df %>% filter(accion == "S", !is.na(brix))
      shiny::validate(need(nrow(df_sel) > 0, "No hay clones seleccionados con Brix."))
      
      med <- median(df_sel$brix, na.rm = TRUE)
      
      plot_ly(df_sel, x = ~brix, type = "histogram",
              marker = list(
                color = "rgba(22,163,74,0.6)",
                line = list(color = "#0b5c2e", width = 1)
              ),
              nbinsx = 25,
              hovertemplate = "Brix: %{x}<br>Clones: %{y}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Brix"),
          yaxis = list(title = "Frecuencia"),
          margin = list(l = 50, r = 20, t = 20, b = 50),
          shapes = list(
            list(type = "line", x0 = med, x1 = med, y0 = 0, y1 = 1, yref = "paper",
                 line = list(dash = "dash", color = "#d97706", width = 2))
          ),
          annotations = list(
            list(x = med, y = 1.05, yref = "paper", text = paste0("Mediana: ", round(med, 1)),
                 showarrow = FALSE, font = list(color = "#d97706", size = 12))
          )
        )
    })
    
    # ── Gráfico 5: Top 10 cruces por Brix promedio (barras horizontales) ──
    output$plot_top_brix <- renderPlotly({
      df <- data_filtrado()
      shiny::validate(need(nrow(df) > 0, "No hay datos."))
      
      top_brix <- df %>%
        filter(accion == "S", !is.na(brix)) %>%
        group_by(cruce) %>%
        summarise(
          n_sel     = n(),
          brix_prom = round(mean(brix, na.rm = TRUE), 1),
          vigor_prom = round(mean(as.numeric(vigor), na.rm = TRUE), 1),
          brix_max  = round(max(brix, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        filter(n_sel >= 4) %>%
        arrange(desc(brix_prom)) %>%
        head(10) %>%
        arrange(brix_prom)
      
      shiny::validate(need(nrow(top_brix) > 0, "No hay cruces con suficientes seleccionados."))
      
      plot_ly(top_brix) %>%
        add_bars(
          y = ~factor(cruce, levels = cruce),
          x = ~brix_prom,
          orientation = "h",
          marker = list(
            color = ~brix_prom,
            colorscale = list(c(0, "#86efac"), c(0.5, "#22c55e"), c(1, "#0b5c2e")),
            line = list(width = 1, color = "#064e3b")
          ),
          text = ~paste0("Cruce ", cruce, " · Brix: ", brix_prom, " · Vigor: ", vigor_prom, " · N=", n_sel),
          textposition = "inside",
          insidetextanchor = "start",
          textfont = list(color = "#fff", size = 12, family = "Inter"),
          hovertemplate = "Cruce: %{y}<br>Brix Prom: %{x}<br>Máx: %{customdata[0]}<extra></extra>",
          customdata = ~cbind(brix_max)
        ) %>%
        layout(
          xaxis = list(title = "Brix Promedio", range = c(0, max(top_brix$brix_prom) * 1.15)),
          yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 12)),
          margin = list(l = 80, r = 20, t = 20, b = 50),
          showlegend = FALSE
        )
    })
    
    # ── Exportar CSV ────────────────────────────────────────────────────────
    output$descargar_csv <- downloadHandler(
      filename = function() {
        paste0("Campana_", input$sel_anio, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(data_filtrado(), file, row.names = FALSE)
      }
    )
    
  })
}