# ==============================================================================
# MOD_FITOPATOLOGIA.R — Módulo de Sanidad y Trazabilidad
# Analítica de enfermedades recolectadas desde las tablets en el campo
# ==============================================================================

mod_fitopatologia_ui <- function(id) {
  ns <- NS(id)

  div(class = "p-3",
    tagList(
      # Encabezado
      fluidRow(
        column(12,
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            h2("Fitopatología y Sanidad", class = "m-0 fw-bold text-success"),
            p("Trazabilidad histórica de enfermedades por familia y progenitor.", class = "text-muted m-0")
          )
        )
      ),

      # Filtros Globales
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("filter"), " Filtros de Búsqueda"),
              class = "bg-light text-dark"
            ),
            card_body(
              fluidRow(
                column(3,
                  selectInput(ns("filtro_anio"), "Año de Selección:",
                              choices = c("Todos", seq(as.integer(format(Sys.Date(), "%Y")), 2020, -1)),
                              selected = "Todos")
                ),
                column(3,
                  selectInput(ns("filtro_etapa"), "Etapa:",
                              choices = c("Todas", "EVF", "ST1", "ST2", "ST3", "ST4", "ST5"),
                              selected = "Todas")
                ),
                column(3,
                  selectInput(ns("filtro_programa"), "Programa:",
                              choices = c("Todos", "CR", "BR"),
                              selected = "Todos")
                ),
                column(2,
                  actionButton(ns("btn_filtrar"), "Aplicar Filtros", icon = icon("search"), class = "btn-success mt-4 w-100")
                ),
                column(1,
                  downloadButton(ns("btn_descarga"), "Excel", class = "btn-outline-success mt-4 w-100")
                )
              ),
              hr(style = "margin: 8px 0;"),
              fluidRow(
                column(12,
                  tags$small(tags$b(icon("exclamation-triangle"), " Umbrales de Alerta de Color")),
                ),
                column(3,
                  numericInput(ns("umbral_amarillo"), "Amarillo ≥ (%)", value = 5,  min = 0, max = 100, step = 1)
                ),
                column(3,
                  numericInput(ns("umbral_rojo"),    "Rojo ≥ (%)",    value = 10, min = 0, max = 100, step = 1)
                ),
                column(6,
                  tags$small(class = "text-muted",
                    "Las celdas de la tabla y las líneas de referencia en los gráficos se actualizarán al aplicar filtros."
                  )
                )
              )
            )
          )
        )
      ),

      br(),

      # Gráfico 1: Progenitores (Madres + Padres)
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("chart-bar"), " Progenitores con mayor incidencia (Carbón, Escaldadura y Roya)"),
              class = "bg-danger text-white"
            ),
            card_body(
              fluidRow(
                column(6, plotlyOutput(ns("plot_madres"), height = "380px") %>% withSpinner(color = "#dc3545")),
                column(6, plotlyOutput(ns("plot_padres"), height = "380px") %>% withSpinner(color = "#dc3545"))
              )
            )
          )
        )
      ),

      br(),

      # Gráfico 2: Tendencia temporal
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("chart-line"), " Tendencia Histórica de Enfermedades por Año"),
              class = "bg-warning text-dark"
            ),
            card_body(
              plotlyOutput(ns("plot_tendencia"), height = "350px") %>% withSpinner(color = "#d97706")
            )
          )
        )
      ),

      br(),

      # Tabla Histórica
      fluidRow(
        column(12,
          card(
            card_header(
              tags$b(icon("table"), " Historial Clínico de Clones / Cruces"),
              class = "bg-success text-white"
            ),
            card_body(
              DT::DTOutput(ns("tabla_sanidad")) %>% withSpinner(color = "#16a34a")
            )
          )
        )
      )
    )
  )
}

mod_fitopatologia_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─── DATOS REACTIVOS ───────────────────────────────────────────────────────

    datos_sanidad <- reactive({
      input$btn_filtrar

      # Consulta parametrizada (evita SQL injection)
      conds  <- c("1=1")
      params <- list()

      anio_val <- isolate(input$filtro_anio)
      etapa_val <- isolate(input$filtro_etapa)
      prog_val  <- isolate(input$filtro_programa)

      if (anio_val != "Todos") {
        conds  <- c(conds, "anio_seleccion = ?")
        params <- c(params, list(as.integer(anio_val)))
      }
      if (etapa_val != "Todas") {
        conds  <- c(conds, "etapa = ?")
        params <- c(params, list(etapa_val))
      }
      if (prog_val != "Todos") {
        conds  <- c(conds, "programa LIKE ?")
        params <- c(params, list(paste0("%", prog_val, "%")))
      }

      q  <- paste("SELECT * FROM evaluacion_enfermedades WHERE", paste(conds, collapse = " AND "))
      df <- tryCatch(
        if (length(params) > 0) dbGetQuery(con, q, params = params) else dbGetQuery(con, q),
        error = function(e) data.frame()
      )

      if (nrow(df) > 0) df$fecha <- as.Date(df$fecha)
      df
    })

    # ─── HELPER: Resumen por progenitor ───────────────────────────────────────

    resumen_progenitor <- function(df, col) {
      df %>%
        filter(.data[[col]] != "", !is.na(.data[[col]])) %>%
        group_by(progenitor = .data[[col]]) %>%
        summarise(
          n            = n(),
          carbon       = round(mean(carbon_porcentaje,     na.rm = TRUE), 2),
          escaldadura  = round(mean(escaldadura_porcentaje, na.rm = TRUE), 2),
          roya         = round(mean(roya_porcentaje,        na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        filter(carbon > 0 | escaldadura > 0 | roya > 0) %>%
        arrange(desc(carbon + escaldadura + roya)) %>%
        head(10)
    }

    grafico_progenitor <- function(df_res, titulo, umbral_a, umbral_r) {
      if (nrow(df_res) == 0)
        return(plot_ly() %>% layout(title = list(text = paste("Sin incidencia para", titulo))))

      plot_ly(df_res, x = ~progenitor) %>%
        add_bars(y = ~carbon,      name = "% Carbón",      marker = list(color = "#475569")) %>%
        add_bars(y = ~escaldadura, name = "% Escaldadura", marker = list(color = "#b45252")) %>%
        add_bars(y = ~roya,        name = "% Roya",        marker = list(color = "#c9913a")) %>%
        layout(
          barmode = "group",
          title   = list(text = titulo, font = list(size = 13)),
          xaxis   = list(title = ""),
          yaxis   = list(title = "Infección Promedio (%)"),
          legend  = list(orientation = "h", y = -0.2),
          shapes  = list(
            list(type = "line", x0 = -0.5, x1 = nrow(df_res) - 0.5,
                 y0 = umbral_a, y1 = umbral_a,
                 line = list(color = "#a16207", dash = "dash", width = 1.2)),
            list(type = "line", x0 = -0.5, x1 = nrow(df_res) - 0.5,
                 y0 = umbral_r, y1 = umbral_r,
                 line = list(color = "#9b1c1c", dash = "dash", width = 1.2))
          )
        )
    }

    # ─── GRÁFICO 1A: MADRES ───────────────────────────────────────────────────

    output$plot_madres <- renderPlotly({
      df <- datos_sanidad()
      ua <- input$umbral_amarillo %||% 5
      ur <- input$umbral_rojo    %||% 10
      if (nrow(df) == 0)
        return(plot_ly() %>% layout(title = "No hay datos para los filtros seleccionados."))
      grafico_progenitor(resumen_progenitor(df, "madre"), "Top 10 Madres", ua, ur)
    })

    # ─── GRÁFICO 1B: PADRES ───────────────────────────────────────────────────

    output$plot_padres <- renderPlotly({
      df <- datos_sanidad()
      ua <- input$umbral_amarillo %||% 5
      ur <- input$umbral_rojo    %||% 10
      if (nrow(df) == 0)
        return(plot_ly() %>% layout(title = "No hay datos para los filtros seleccionados."))
      grafico_progenitor(resumen_progenitor(df, "padre"), "Top 10 Padres", ua, ur)
    })

    # ─── GRÁFICO 2: TENDENCIA TEMPORAL ────────────────────────────────────────

    output$plot_tendencia <- renderPlotly({
      df <- datos_sanidad()
      if (nrow(df) == 0 || !"anio_seleccion" %in% names(df))
        return(plot_ly() %>% layout(title = "Sin datos suficientes para tendencia."))

      tendencia <- df %>%
        group_by(anio_seleccion) %>%
        summarise(
          carbon      = round(mean(carbon_porcentaje,      na.rm = TRUE), 2),
          escaldadura = round(mean(escaldadura_porcentaje,  na.rm = TRUE), 2),
          roya        = round(mean(roya_porcentaje,         na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        arrange(anio_seleccion)

      if (nrow(tendencia) < 2)
        return(plot_ly() %>% layout(title = "Se necesitan al menos 2 años para mostrar tendencia."))

        plot_ly(tendencia, x = ~anio_seleccion) %>%
        add_lines(y = ~carbon,      name = "% Carbón",      line = list(color = "#475569", width = 2)) %>%
        add_lines(y = ~escaldadura, name = "% Escaldadura", line = list(color = "#b45252", width = 2)) %>%
        add_lines(y = ~roya,        name = "% Roya",        line = list(color = "#c9913a", width = 2)) %>%
        add_markers(y = ~carbon,      marker = list(color = "#475569", size = 7)) %>%
        add_markers(y = ~escaldadura, marker = list(color = "#b45252", size = 7)) %>%
        add_markers(y = ~roya,        marker = list(color = "#c9913a", size = 7)) %>%
        layout(
          xaxis  = list(title = "Año", dtick = 1),
          yaxis  = list(title = "Promedio de Infección (%)"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          shapes = list(
            list(type = "line", x0 = min(tendencia$anio_seleccion), x1 = max(tendencia$anio_seleccion),
                 y0 = input$umbral_amarillo %||% 5,  y1 = input$umbral_amarillo %||% 5,
                 line = list(color = "#a16207", dash = "dot", width = 1.2)),
            list(type = "line", x0 = min(tendencia$anio_seleccion), x1 = max(tendencia$anio_seleccion),
                 y0 = input$umbral_rojo    %||% 10, y1 = input$umbral_rojo    %||% 10,
                 line = list(color = "#9b1c1c", dash = "dot", width = 1.2))
          )
        )
    })

    # ─── TABLA DE DATOS ───────────────────────────────────────────────────────

    output$tabla_sanidad <- DT::renderDT({
      df <- datos_sanidad()
      if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "Sin datos")))

      df_vista <- df %>%
        select(Fecha = fecha, Etapa = etapa, Cruce = cruce, Madre = madre, Padre = padre,
               `% Carbón`      = carbon_porcentaje,
               `% Escaldadura` = escaldadura_porcentaje,
               `% Roya`        = roya_porcentaje) %>%
        arrange(desc(Fecha))

      ua <- input$umbral_amarillo %||% 5
      ur <- input$umbral_rojo    %||% 10

      datatable(
        df_vista,
        rownames = FALSE,
        options  = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip")
      ) %>%
      formatStyle("% Carbón",
        backgroundColor = styleInterval(c(ua, ur), c("white", "#fefce8", "#fde8e8"))) %>%
      formatStyle("% Escaldadura",
        backgroundColor = styleInterval(c(ua, ur), c("white", "#fefce8", "#fde8e8"))) %>%
      formatStyle("% Roya",
        backgroundColor = styleInterval(c(ua, ur), c("white", "#fefce8", "#fde8e8")))
    })

    # ─── DESCARGA EXCEL ───────────────────────────────────────────────────────

    output$btn_descarga <- downloadHandler(
      filename = function() paste0("Sanidad_CR_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        df <- datos_sanidad()
        if (nrow(df) == 0) {
          df <- data.frame(Mensaje = "Sin datos para los filtros seleccionados.")
        } else {
          df <- df %>%
            select(Fecha = fecha, Año = anio_seleccion, Etapa = etapa,
                   Cruce = cruce, Madre = madre, Padre = padre, Programa = programa,
                   `% Carbón`      = carbon_porcentaje,
                   `% Escaldadura` = escaldadura_porcentaje,
                   `% Roya`        = roya_porcentaje)
        }
        openxlsx::write.xlsx(df, file)
      }
    )

  })
}
