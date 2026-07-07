# ==============================================================================
# MOD_ESTADO_VARIEDAD.R — Módulo Shiny: Estado de Variedad
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Muestra la "Cédula de Identidad" técnica de una variedad activa en el
# programa de mejoramiento, incluyendo sus valores reales de Rendimiento,
# Sanidad, Habilidad Combinatoria y Estatus.
# ==============================================================================

# --- UI del Módulo ---
mod_estado_variedad_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .bslib-value-box .value-box-grid { padding: 8px !important; gap: 5px !important; }
      .bslib-value-box .value-box-title { font-size: 0.75rem !important; margin-bottom: 2px !important; }
      .bslib-value-box .value-box-value { font-size: 1.1rem !important; font-weight: bold !important; }
      .bslib-value-box .value-box-showcase { font-size: 1.5rem !important; opacity: 0.4 !important; }
      .card-header { padding: 4px 10px !important; font-size: 0.85rem !important; font-weight: bold !important; }
      .card-body { padding: 8px !important; }
      hr { margin: 8px 0 !important; }
    ")),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      layout_column_wrap(
        width = 1/2,
        fill = FALSE,
        card(
          height = "220px",
          card_header(tagList(icon("address-card"), " Variedad y Notas")),
          layout_column_wrap(
            width = 1/2,
            tagList(
              selectizeInput(ns("var_estado"), NULL, choices = NULL, 
                             options = list(placeholder = "Buscar variedad...")),
              layout_column_wrap(
                width = 1/2,
                actionButton(ns("btn_estado"), "Ver Ficha", class = "btn-warning btn-sm"),
                uiOutput(ns("download_ui"))
              )
            ),
            tagList(
              textAreaInput(ns("txt_notas"), NULL, placeholder = "Notas de campo...", rows = 2),
              actionButton(ns("btn_save_note"), "Guardar Nota", class = "btn-success btn-sm w-100")
            )
          )
        ),
        card(
          height = "220px",
          card_header(tagList(icon("info-circle"), " Métricas Principales")),
          uiOutput(ns("vboxes_principales"))
        )
      )
    ),
    
    # Tabs de detalle técnicos
    navset_card_pill(
      title = "Detalles Técnicos y Performance",
      nav_panel(
        "Rendimiento y GCA", icon = icon("chart-bar"),
        layout_column_wrap(
          width = 1,
          uiOutput(ns("vboxes_rendimiento")),
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(tagList(icon("bullseye"), " Perfil Genético (Escala 0-10)")),
              plotly::plotlyOutput(ns("plot_radar"), height = "320px")
            ),
            card(
              card_header(tagList(icon("table"), " Métricas Completas (Raw)")),
              DT::DTOutput(ns("tabla_rendimiento_raw"))
            )
          )
        )
      ),
      nav_panel(
        "Sanidad (Enfermedades)", icon = icon("shield-virus"),
        card(
          card_header(tagList(icon("virus-slash"), " Perfil Fitopatológico")),
          tags$h5("Score Global DISEASE: ", textOutput(ns("txt_disease"), inline=TRUE), class = "mb-3"),
          plotOutput(ns("plot_sanidad"), height = "340px")
        )
      ),
      nav_panel(
        "Historial Reproductivo", icon = icon("sitemap"),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Resumen de Cruces"),
            uiOutput(ns("vboxes_familias")),
            hr(),
            plotly::plotlyOutput(ns("plot_familias_historico"), height = "300px")
          ),
          card(
            card_header("Registro de Familias Evaluadas"),
            DT::DTOutput(ns("tabla_familias"))
          )
        )
      ),
      nav_panel(
        "Trazabilidad de Clones", icon = icon("history"),
        card(
          card_header(tagList(icon("history"), " Explorador de Trazabilidad Genética")),
          layout_column_wrap(
            width = 1,
            layout_column_wrap(
              width = 1/2,
              selectizeInput(ns("clon_search"), "ID del Clon / Variedad:", 
                             choices = NULL, 
                             options = list(placeholder = "Ej: 630-1, CR261001...", 
                                            maxOptions = 100,
                                            searchField = c("value", "label"))),
              tags$div(
                class = "d-flex flex-column gap-2",
                actionButton(ns("btn_trace"), "Rastrear Historia", 
                             class = "btn-primary w-100", 
                             icon = icon("search-location")),
                uiOutput(ns("download_btn_ui_trace"))
              )
            )
          )
        ),
        uiOutput(ns("timeline_ui_trace"))
      )
    )
  )
}

# --- Server del Módulo ---
mod_estado_variedad_server <- function(id, cat_var, pedigree_var, df_categorias, df_familias, con) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Helper seguro para datos faltantes — siempre devuelve un escalar numérico
    if_exists <- function(df, col) {
      if (!(col %in% names(df))) return(NA_real_)
      val <- df[[col]]
      # Si es lista o vector múltiple, tomar solo el primer elemento
      if (is.list(val)) val <- val[[1]]
      if (length(val) != 1) val <- val[1]
      suppressWarnings(as.numeric(val))
    }
    
    # Poblar selectize SOLO con las variedades activas en df_categorias
    observe({
      opciones_var <- sort(unique(df_categorias()$variedad))
      updateSelectizeInput(session, "var_estado", choices = opciones_var, server = TRUE)
    })
    
    # --- Datos reactivos de la variedad seleccionada ---
    var_data <- eventReactive(input$btn_estado, {
      req(input$var_estado)
      
      target_info <- df_categorias() %>%
        filter(variedad == input$var_estado) %>%
        slice(1)
      
      shiny::validate(need(nrow(target_info) > 0, "Variedad no encontrada en los registros de 2025."))
      
      target_id <- as.character(target_info$variedad)
      
      # Historial de familias donde es madre o padre
      familias_historico <- df_familias() %>%
        filter(madre == target_id | padre == target_id) %>%
        mutate(
          rol = ifelse(madre == target_id, "MADRE", "PADRE"),
          accion_desc = case_when(
            accion == "S" ~ "Seleccionada",
            accion == "R" ~ "Rechazada",
            TRUE ~ "En Evaluación"
          )
        )
      
      n_como_padre <- nrow(familias_historico %>% filter(rol == "PADRE"))
      n_como_madre <- nrow(familias_historico %>% filter(rol == "MADRE"))
      
      list(
        target_id     = target_id,
        nombre        = target_id,
        info          = target_info,
        familias      = familias_historico,
        n_como_padre  = n_como_padre,
        n_como_madre  = n_como_madre,
        notas         = target_info$notas
      )
    })
    
    # Cargar nota en el textArea al cambiar de variedad
    observeEvent(var_data(), {
      updateTextAreaInput(session, "txt_notas", value = ifelse(is.na(var_data()$notas), "", var_data()$notas))
    })
    
    # Guardar nota en la BD
    observeEvent(input$btn_save_note, {
      req(var_data())
      db_save_note(con, var_data()$target_id, input$txt_notas)
      showNotification("Nota guardada con éxito en la Base de Datos.", type = "message")
    })
    
    # --- KPIs Principales (bslib value_boxes) ---
    output$vboxes_principales <- renderUI({
      req(var_data())
      d <- var_data()$info
      
      # Mapeo de colores bslib
      color_cat <- case_when(
        grepl("C1", d$categoria) ~ "success",
        grepl("C2", d$categoria) ~ "primary",
        grepl("C6", d$categoria) ~ "warning",
        TRUE ~ "info"
      )
      
      color_suelo <- case_when(
        toupper(d$adapt) == "BUENO"       ~ "success",
        toupper(d$adapt) == "MAL_DRENADO" ~ "warning",
        toupper(d$adapt) == "ROCOSO"      ~ "danger",
        TRUE ~ "secondary"
      )
      
      layout_column_wrap(
        width = 1/5,
        height = "120px", # Altura fija para evitar estiramiento
        value_box(
          title = "FACTOR",
          value = ifelse(is.na(d$factor), "N/A", as.character(round(d$factor, 2))),
          showcase = icon("star"),
          theme = "primary"
        ),
        value_box(
          title = "Categoría",
          value = ifelse(is.na(d$categoria), "Sin Cat.", as.character(d$categoria)),
          showcase = icon("dna"),
          theme = color_cat
        ),
        value_box(
          title = "Status",
          value = ifelse(is.na(d$status), "N/A", as.character(d$status)),
          showcase = icon("check-circle"),
          theme = "info"
        ),
        value_box(
          title = "Suelo",
          value = d$adapt,
          showcase = icon("leaf"),
          theme = color_suelo
        ),
        value_box(
          title = "MAXEST",
          value = d$maxest,
          showcase = icon("layer-group"),
          theme = "secondary"
        )
      )
    })
    
    # --- Tab: Rendimiento y Agronomía (bslib value_boxes) ---
    output$vboxes_rendimiento <- renderUI({
      req(var_data())
      d <- var_data()$info
      
      val_y <- if_exists(d, "y")
      val_q <- if_exists(d, "q")
      
      # Traducción del grado a índice para Y
      y_idx <- case_when(
        is.na(val_y) ~ "N/D",
        val_y == 8 ~ "< 80%",
        val_y == 7 ~ "80-90%",
        val_y == 6 ~ "90-100%",
        val_y == 5 ~ "100-110%",
        val_y == 4 ~ "110-120%",
        val_y == 3 ~ "120-130%",
        val_y == 2 ~ "> 130%",
        TRUE ~ as.character(val_y)
      )
      
      # Traducción del grado a índice para Q
      q_idx <- case_when(
        is.na(val_q) ~ "N/D",
        val_q == 8 ~ "< 90%",
        val_q == 7 ~ "90-95%",
        val_q == 6 ~ "95-100%",
        val_q == 5 ~ "100-110%",
        val_q == 4 ~ "105-110%",
        val_q == 3 ~ "110-115%",
        val_q == 2 ~ "> 115%",
        TRUE ~ as.character(val_q)
      )
      
      color_y <- if(is.na(val_y)) "secondary" else if(val_y <= 4) "success" else if(val_y <= 6) "warning" else "danger"
      color_q <- if(is.na(val_q)) "secondary" else if(val_q <= 4) "success" else if(val_q <= 6) "warning" else "danger"
      
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = paste("Yield (Y) - Testigo:", y_idx),
          value = paste("Grado", ifelse(is.na(val_y), "N/D", val_y)),
          showcase = icon("balance-scale"),
          theme = color_y
        ),
        value_box(
          title = paste("Quality (Q) - Testigo:", q_idx),
          value = paste("Grado", ifelse(is.na(val_q), "N/D", val_q)),
          showcase = icon("gem"),
          theme = color_q
        )
      )
    })
    
    # --- Tab: Historial Reproductivo (bslib value_boxes) ---
    output$vboxes_familias <- renderUI({
      req(var_data())
      d <- var_data()
      fam <- d$familias
      
      totales <- nrow(fam)
      if (totales == 0) {
        return(layout_column_wrap(
          width = 1,
          value_box(title = "Cruces Totales", value = 0, showcase = icon("users"), theme = "secondary")
        ))
      }
      
      seleccionadas <- sum(fam$accion == "S", na.rm = TRUE)
      tasa <- round((seleccionadas / totales) * 100, 1)
      color_tasa <- if(tasa >= 20) "success" else if(tasa > 0) "warning" else "danger"
      
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = paste0("♂:", d$n_como_padre, " ♀:", d$n_como_madre),
          value = paste(totales, "Cruces"),
          showcase = icon("users"),
          theme = "primary"
        ),
        value_box(
          title = "Tasa de Selección (S)",
          value = paste0(tasa, "%"),
          showcase = icon("trophy"),
          theme = color_tasa
        )
      )
    })
    
    # --- Tab: Rendimiento y GCA (RADAR CHART) ---
    output$plot_radar <- plotly::renderPlotly({
      req(var_data())
      info <- var_data()$info
      
      # Extraer valores y normalizar a escala 0-10 donde MAYOR es MEJOR
      val_factor <- if_exists(info, "factor")
      val_y <- if_exists(info, "y")
      val_q <- if_exists(info, "q")
      val_disease <- if_exists(info, "disease")
      
      # Transformaciones:
      # Factor: asume max ~1.5. Normalizado = (factor / 1.5) * 10
      f_norm <- ifelse(is.na(val_factor), 0, min((val_factor / 1.5) * 10, 10))
      # Y y Q: en la BD 1-2 es excelente, 8-9 es malo. Invertimos: 10 - val
      y_norm <- ifelse(is.na(val_y), 0, max(10 - val_y, 0))
      q_norm <- ifelse(is.na(val_q), 0, max(10 - val_q, 0))
      # Disease: 0 es excelente, 9 es malo. Invertimos: 10 - disease
      s_norm <- ifelse(is.na(val_disease), 0, max(10 - val_disease, 0))
      
      df_radar <- data.frame(
        Eje = c("Potencial Híbrido (Factor)", "Rendimiento Agrícola (Y)", 
                "Calidad Jugo (Q)", "Sanidad (Resistencia)"),
        Valor = c(f_norm, y_norm, q_norm, s_norm)
      )
      
      # Plotly Radar Chart
      plotly::plot_ly(
        type = 'scatterpolar',
        r = c(df_radar$Valor, df_radar$Valor[1]),
        theta = c(df_radar$Eje, df_radar$Eje[1]),
        fill = 'toself',
        fillcolor = 'rgba(39, 174, 96, 0.4)',  # Verde esmeralda transparente
        line = list(color = '#27ae60', width = 2),
        marker = list(color = '#1e8449', size = 8),
        hoverinfo = "text",
        text = c(paste(df_radar$Eje, ":", round(df_radar$Valor, 1), "/ 10"), "")
      ) %>%
        plotly::layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 10),
              tickvals = c(0, 2, 4, 6, 8, 10),
              ticktext = c("0", "2", "4", "6", "8", "10")
            )
          ),
          showlegend = FALSE,
          margin = list(t = 20, b = 20, l = 40, r = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$tabla_rendimiento_raw <- DT::renderDT({
      req(var_data())
      info <- var_data()$info
      
      df_raw <- data.frame(
        Métrica = c("TCA", "REND", "TAA", "AGRO", "GEN", "GCAM", "GCAP", "GCAT", "GCAMEAN"),
        Valor = c(if_exists(info, "tca"), if_exists(info, "rend"), if_exists(info, "taa"), 
                  if_exists(info, "agro"), if_exists(info, "gen"), 
                  if_exists(info, "gcam"), if_exists(info, "gcap"), if_exists(info, "gcat"), if_exists(info, "gcamean"))
      )
      
      DT::datatable(
        df_raw %>% mutate(Valor = round(Valor, 3)),
        options = list(pageLength = 10, dom = "t"),
        rownames = FALSE, selection = "none"
      )
    })
    
    # --- Tab: Sanidad ---
    output$txt_disease <- renderText({
      req(var_data())
      round(var_data()$info$disease, 3)
    })
    
    output$plot_sanidad <- renderPlot({
      req(var_data())
      info <- var_data()$info
      
      v_carbon <- if_exists(info, "carbon")
      v_roya   <- if_exists(info, "roya")
      v_es     <- if_exists(info, "es")
      
      if (all(is.na(c(v_carbon, v_roya, v_es)))) {
        plot.new()
        text(0.5, 0.6, "Datos detallados de sanidad no disponibles en la BD.", cex = 1.3, col = "#2c3e50", font = 2)
        text(0.5, 0.4, "(Presione 'SINCRONIZAR SISTEMA' para cargar Carbón, Roya y Escaldadura desde Excel)", cex = 1.0, col = "#7f8c8d")
        return()
      }
      
      df_enf <- data.frame(
        Enfermedad = c("Carbón", "Roya", "Escaldadura"),
        Score      = c(
          ifelse(is.na(v_carbon), 0, v_carbon),
          ifelse(is.na(v_roya),   0, v_roya),
          ifelse(is.na(v_es),     0, v_es)
        )
      ) %>%
        mutate(
          Color = case_when(
            Score <= 3 ~ "#27ae60",   # Verde: Resistente
            Score <= 6 ~ "#f39c12",   # Naranja: Intermedio
            TRUE       ~ "#c0392b"    # Rojo: Susceptible
          ),
          Label = case_when(
            Score <= 3 ~ paste0(Score, " — Resistente"),
            Score <= 6 ~ paste0(Score, " — Intermedio"),
            TRUE       ~ paste0(Score, " — Susceptible")
          ),
          Enfermedad = factor(Enfermedad, levels = c("Carbón", "Roya", "Escaldadura"))
        )
      
      ggplot(df_enf, aes(x = Enfermedad, y = Score, fill = Color)) +
        geom_col(width = 0.45, color = "white", show.legend = FALSE) +
        geom_text(aes(label = Label), vjust = -0.5, fontface = "bold", size = 4.2, color = "#2c3e50") +
        scale_fill_identity() +
        scale_y_continuous(limits = c(0, 11), breaks = c(0, 3, 6, 9)) +
        geom_hline(yintercept = 3, linetype = "dashed", color = "#27ae60", alpha = 0.6) +
        geom_hline(yintercept = 6, linetype = "dashed", color = "#f39c12", alpha = 0.6) +
        labs(
          title    = "Perfil Sanitario",
          subtitle = paste0("Score Global DISEASE: ", round(if_exists(info, "disease"), 2), "  |  Escala: 0 = Sin infección, 9 = Alta susceptibilidad"),
          x = "", y = "Grado de Reacción (0–9)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title    = element_text(face = "bold", color = "#2c3e50", size = 11),
          plot.subtitle = element_text(color = "#7f8c8d", size = 9),
          axis.text.x   = element_text(face = "bold", size = 10),
          panel.grid.major.x = element_blank(),
          plot.margin   = margin(2, 2, 2, 2)
        )
    })
    
    # --- Tab: Historial Reproductivo ---
    output$plot_familias_historico <- plotly::renderPlotly({
      req(var_data())
      fam <- var_data()$familias
      if (nrow(fam) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
                 plotly::layout(title = list(text = "Sin historial de familias", font = list(color = "#7f8c8d"))))
      }
      
      tryCatch({
        # Resumir por año y decisión para Plotly
        df_agg <- fam %>%
          count(ano, accion_desc) %>%
          arrange(ano)
        
        # Colores personalizados
        colores <- c(
          "Seleccionada" = "#27ae60",
          "Rechazada" = "#c0392b",
          "En Evaluación" = "#f39c12"
        )
        
        plotly::plot_ly(df_agg, x = ~ano, y = ~n, color = ~accion_desc, colors = colores,
                        type = "bar", text = ~n, textposition = 'auto',
                        hoverinfo = "text", hovertext = ~paste("Año:", ano, "<br>Estado:", accion_desc, "<br>Familias:", n)) %>%
          plotly::layout(
            title = list(text = "Evaluación de Familias por Año", x = 0.05, font = list(size = 14, color = "#2c3e50", family = "Arial-Bold")),
            xaxis = list(title = "", tickangle = -45, type = 'category'),
            yaxis = list(title = "Número de Familias"),
            barmode = "stack",
            legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),
            margin = list(t = 40, b = 40, l = 40, r = 20)
          ) %>%
          plotly::config(displayModeBar = FALSE)
      }, error = function(e) {
        plotly::plotly_empty() %>% 
          plotly::layout(title = list(text = paste("Error al graficar:", e$message), font = list(color = "#c0392b")))
      })
    })
    
    output$tabla_familias <- DT::renderDT({
      req(var_data())
      fam <- var_data()$familias
      
      if (nrow(fam) == 0) {
        return(DT::datatable(
          data.frame(Mensaje = "Esta variedad no tiene historial en Evaluación de Familias"),
          rownames = FALSE
        ))
      }
      
      DT::datatable(
        fam %>% select(
          Año = ano,
          Cruce = cruce,
          Madre = madre,
          Padre = padre,
          Rol = rol,
          TCA = t_c_a,
          `REND 96` = rend_96o,
          TAA = t_a_a,
          Decisión = accion_desc
        ) %>%
          mutate(across(where(is.numeric), ~ round(.x, 2))),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "Registro de Familias Evaluadas"
      ) %>%
        DT::formatStyle(
          "Decisión",
          backgroundColor = DT::styleEqual(
            c("Seleccionada", "Rechazada", "En Evaluación"),
            c("#d5f5e3", "#fadbd8", "#fdebd0")
          )
        )
    })
    
    # --- Botón de Exportación ---
    output$download_ui <- renderUI({
      req(var_data())
      downloadButton(ns("download_ficha"), "Excel", class = "btn-info btn-sm w-100")
    })
    
    output$download_ficha <- downloadHandler(
      filename = function() {
        paste("Ficha_Tecnica_", var_data()$target_id, "_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        d <- var_data()
        wb <- createWorkbook()
        
        # Hoja 1: Perfil General
        addWorksheet(wb, "Perfil_General")
        writeData(wb, "Perfil_General", d$info)
        
        # Hoja 2: Historial Reproductivo
        if(nrow(d$familias) > 0) {
          addWorksheet(wb, "Familias_EVF")
          writeData(wb, "Familias_EVF", d$familias)
        }
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # --- Lógica de Trazabilidad Integrada ---
    
    # Poblar búsqueda de trazabilidad
    observe({
      cat_ids <- if (!is.null(df_categorias())) df_categorias()$variedad else c()
      clones_ids <- c()
      for (st in c("st1", "st2", "st3", "st4", "st5")) {
        table_name <- paste0("clones_", st)
        df_st <- tryCatch(
          dbGetQuery(con, sprintf("SELECT DISTINCT anio_seleccion AS anio, cruce, num_sel FROM %s", table_name)),
          error = function(e) NULL
        )
        if (!is.null(df_st) && nrow(df_st) > 0) {
          clones_ids <- c(clones_ids, paste0(df_st$anio, " | ", df_st$cruce, "-", df_st$num_sel))
        }
      }
      all_ids <- sort(unique(c(cat_ids, clones_ids)))
      updateSelectizeInput(session, "clon_search", choices = all_ids, server = TRUE)
    })
    
    trace_data <- eventReactive(input$btn_trace, {
      req(input$clon_search)
      input_str <- input$clon_search
      search_year <- NULL
      id_target <- input_str
      
      if (grepl(" | ", input_str, fixed = TRUE)) {
        parts <- strsplit(input_str, " | ", fixed = TRUE)[[1]]
        search_year <- as.integer(parts[1])
        id_target <- parts[2]
      }
      
      # 0. ¿Es una variedad CR promocionada? Buscar su clon origen
      promo_info <- tryCatch(
        dbGetQuery(con, "SELECT * FROM promociones WHERE nombre_cr = ?", params = list(id_target)),
        error = function(e) data.frame()
      )
      id_query <- if(nrow(promo_info) > 0) promo_info$clon_origen[1] else id_target
      
      # Buscamos en todas las tablas ST
      hitos <- list()
      for (st in c("st1", "st2", "st3", "st4", "st5")) {
        df_h <- tryCatch({
          if (grepl("-", id_query)) {
            pts <- strsplit(id_query, "-")[[1]]
            q <- sprintf("SELECT * FROM clones_%s WHERE cruce = ? AND num_sel = ?", st)
            dbGetQuery(con, q, params = list(pts[1], as.integer(pts[2])))
          } else {
            q <- sprintf("SELECT * FROM clones_%s WHERE cruce = ? OR cruce || '-' || num_sel = ?", st)
            dbGetQuery(con, q, params = list(id_query, id_query))
          }
        }, error = function(e) NULL)
        
        if (!is.null(df_h) && nrow(df_h) > 0) {
          if (!is.null(search_year)) df_h <- df_h %>% filter(anio_seleccion == search_year)
          if (nrow(df_h) > 0) hitos[[st]] <- df_h %>% mutate(stage = toupper(st))
        }
      }
      
      list(id = id_target, year = search_year, hitos = hitos)
    })
    
    output$timeline_ui_trace <- renderUI({
      req(trace_data())
      d <- trace_data()
      if (length(d$hitos) == 0) return(p("No se encontró historial de selección para este ID.", class="text-muted p-4"))
      
      tagList(
        lapply(names(d$hitos), function(st) {
          h <- d$hitos[[st]]
          card(
            class = "mb-2 border-start border-4 border-info",
            card_header(paste("Hito en", st)),
            layout_column_wrap(
              width = 1/4,
              tags$div(tags$b("Año: "), h$anio_seleccion),
              tags$div(tags$b("Cruce: "), h$cruce),
              tags$div(tags$b("Brix: "), round(as.numeric(h$brix), 2)),
              tags$div(tags$b("Estado: "), ifelse(h$seleccionado == 1, "SELECCIONADO", "RECHAZADO"))
            )
          )
        })
      )
    })

    return(var_data)
  })
}
