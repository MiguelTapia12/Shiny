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
    fluidRow(
      # Panel de búsqueda
      box(
        width = 3, title = "Seleccionar Variedad",
        status = "warning", solidHeader = TRUE,
        selectizeInput(
          ns("var_estado"), "Variedad:",
          choices = NULL,
          options = list(maxOptions = 50,
                         placeholder = "Buscar variedad activa...")
        ),
        actionButton(ns("btn_estado"), "Ver Ficha Técnica",
                     class = "btn-warning btn-block",
                     icon = icon("address-card")),
        br(),
        uiOutput(ns("download_ui"))
      ),
      
      # KPIs Principales (Corazón del Valor Genético)
      box(
        width = 9, title = "Métricas Principales (AllAct2025)",
        status = "info", solidHeader = TRUE,
        fluidRow(
          valueBoxOutput(ns("vb_factor"), width = 2),
          valueBoxOutput(ns("vb_categoria"), width = 3),
          valueBoxOutput(ns("vb_maxest"), width = 2),
          valueBoxOutput(ns("vb_adapt"), width = 2),
          valueBoxOutput(ns("vb_status"), width = 3)
        ),
        hr(),
        fluidRow(
          column(9, textAreaInput(ns("txt_notas"), "Observaciones / Notas de Campo:", 
                                 placeholder = "Escribe aquí cualquier detalle relevante sobre este clon...",
                                 width = "100%", rows = 2)),
          column(3, actionButton(ns("btn_save_note"), "Guardar Nota", 
                                 class = "btn-success btn-block", 
                                 style = "margin-top: 25px;",
                                 icon = icon("save")))
        )
      )
    ),
    
    fluidRow(
      # Tabs de detalle
      tabBox(
        width = 12, title = "Detalles Técnicos",
        id = ns("tabs_detalle"),
        
        # Tab: Perfil Agronómico y de Rendimiento
        tabPanel(
          "Rendimiento y GCA", icon = icon("chart-bar"),
          fluidRow(
            column(12, 
                   tags$h4("Métricas Relativas al Testigo"),
                   tags$p("Basado en el índice de rendimiento (Y) y calidad (Q). Grados del 2 al 8, donde menor es mejor."),
                   fluidRow(
                     valueBoxOutput(ns("vb_y"), width = 6),
                     valueBoxOutput(ns("vb_q"), width = 6)
                   ),
                   hr()
            )
          ),
          fluidRow(
            column(7, plotOutput(ns("plot_rendimiento"), height = "350px")),
            column(5,
                   tags$h4("Datos Exactos"),
                   DT::DTOutput(ns("tabla_rendimiento_raw"))
            )
          )
        ),
        
        # Tab: Sanidad
        tabPanel(
          "Sanidad (Enfermedades)", icon = icon("shield-virus"),
          fluidRow(
            column(12, 
                   tags$h4("Score Global DISEASE: ", textOutput(ns("txt_disease"), inline=TRUE)),
                   hr()
            )
          ),
          fluidRow(
            column(12, plotOutput(ns("plot_sanidad"), height = "300px"))
          )
        ),
        
        # Tab: Historial Reproductivo (Familias)
        tabPanel(
          "Historial Reproductivo", icon = icon("sitemap"),
          fluidRow(
            column(4,
                   valueBoxOutput(ns("vb_fam_totales"), width = 12),
                   valueBoxOutput(ns("vb_tasa_exito"), width = 12)
            ),
            column(8,
                   plotOutput(ns("plot_familias_historico"), height = "250px")
            )
          ),
          fluidRow(
            column(12,
                   tags$h4("Registro de Familias Evaluadas"),
                   DT::DTOutput(ns("tabla_familias"))
            )
          )
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_estado_variedad_server <- function(id, cat_var, pedigree_var, df_categorias, df_familias) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
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
      
      validate(need(nrow(target_info) > 0, "Variedad no encontrada en los registros de 2025."))
      
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
    
    # --- KPIs Principales ---
    output$vb_factor <- renderValueBox({
      req(var_data())
      val <- round(var_data()$info$factor, 2)
      valueBox(val, "FACTOR (Valor Cría)", icon = icon("star"), color = "yellow")
    })
    
    output$vb_categoria <- renderValueBox({
      req(var_data())
      val <- var_data()$info$categoria
      sub <- paste("Éxito EVF:", var_data()$info$evf_info)
      color_cat <- case_when(
        grepl("C1", val) ~ "green",
        grepl("C2", val) ~ "blue",
        grepl("C3", val) ~ "purple",
        grepl("C6", val) ~ "orange",
        TRUE ~ "olive"
      )
      valueBox(val, sub, icon = icon("dna"), color = color_cat)
    })
    
    output$vb_maxest <- renderValueBox({
      req(var_data())
      val <- var_data()$info$maxest
      valueBox(val, "MAXEST (Max. Estado Alcanzado)", icon = icon("layer-group"), color = "blue")
    })
    
    output$vb_adapt <- renderValueBox({
      req(var_data())
      val <- var_data()$info$adapt
      color_suelo <- case_when(
        toupper(val) == "GOOD" ~ "green",
        toupper(val) == "CLAY" ~ "orange",
        toupper(val) == "ROCKY" ~ "maroon",
        TRUE ~ "aqua"
      )
      valueBox(val, "Adaptación de Suelo", icon = icon("leaf"), color = color_suelo)
    })
    
    output$vb_status <- renderValueBox({
      req(var_data())
      val <- var_data()$info$status
      desc <- ifelse(val == "E", "E (Exploratoria)", ifelse(val == "P", "P (Probada)", val))
      valueBox(desc, "Status / STA", icon = icon("check-circle"), color = "purple")
    })
    
    # --- Tab: Rendimiento y Agronomía ---
    output$plot_rendimiento <- renderPlot({
      req(var_data())
      info <- var_data()$info
      
      # Preparar datos para un gráfico de barras horizontal comparativo (sin TCA/REND/TAA ya que pueden faltar)
      df_plot <- data.frame(
        Metrica = c("AGRO (Agronómico)", "GEN (Genético)"),
        Valor = c(info$agro, info$gen)
      ) %>%
        # Ordenar barras
        mutate(Metrica = factor(Metrica, levels = rev(Metrica)))
      
      ggplot(df_plot, aes(x = Metrica, y = Valor, fill = Metrica)) +
        geom_col(color = "#2c3e50") +
        geom_text(aes(label = round(Valor, 2)), hjust = -0.2, fontface = "bold", size = 5) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        labs(
          title = "Perfil Agronómico y de Rendimiento",
          subtitle = paste("Variedad:", var_data()$nombre),
          x = "", y = "Valor"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.text.y = element_text(face = "bold", size = 12)
        )
    })
    
    output$tabla_rendimiento_raw <- DT::renderDT({
      req(var_data())
      info <- var_data()$info
      
      df_raw <- data.frame(
        Métrica = c("TCA", "REND", "TAA", "AGRO", "GEN", "GCAM", "GCAP", "GCAT", "GCAMEAN"),
        Valor = c(info$tca, info$rend, info$taa, info$agro, info$gen, 
                  info$gcam, info$gcap, info$gcat, info$gcamean)
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
      
      # Datos de las 3 enfermedades principales
      df_enf <- data.frame(
        Enfermedad = c("Carbón", "Roya", "Escaldadura"),
        Score = c(info$carbon, info$roya, info$es)
      ) %>%
        mutate(
          # Determinar la categoría (Resistente, Intermedio, Susceptible) basado en los umbrales
          Categoria = case_when(
            Score <= 2 ~ "R (Resistente)",
            Score <= 4 ~ "I (Intermedio)",
            TRUE ~ "S (Susceptible)"
          ),
          Enfermedad = factor(Enfermedad, levels = c("Escaldadura", "Roya", "Carbón"))
        )
      
      # Colores semáforo
      colores_semaforo <- c("R (Resistente)" = "#27ae60", 
                            "I (Intermedio)" = "#f1c40f", 
                            "S (Susceptible)" = "#c0392b")
      
      ggplot(df_enf, aes(x = Enfermedad, y = Score, fill = Categoria)) +
        geom_col(color = "#333333", width = 0.6) +
        geom_text(aes(label = paste(Score, "-", substr(Categoria, 1, 1))), 
                  hjust = -0.3, fontface = "bold", size = 5) +
        coord_flip(ylim = c(0, max(10, max(df_enf$Score) + 2))) +
        scale_fill_manual(values = colores_semaforo) +
        labs(
          title = "Niveles de Infección",
          subtitle = "Escala de Severidad",
          x = "", y = "Grado de Reacción"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "bottom"
        )
    })
    
    output$vb_y <- renderValueBox({
      req(var_data())
      y_val <- var_data()$info$y_score
      
      # Traducción del grado a índice
      y_idx <- case_when(
        is.na(y_val) ~ "N/D",
        y_val == 8 ~ "< 80%",
        y_val == 7 ~ "80-90%",
        y_val == 6 ~ "90-100%",
        y_val == 5 ~ "100-110%",
        y_val == 4 ~ "110-120%",
        y_val == 3 ~ "120-130%",
        y_val == 2 ~ "> 130%",
        TRUE ~ as.character(y_val)
      )
      
      color_y <- ifelse(is.na(y_val), "navy", ifelse(y_val <= 4, "green", ifelse(y_val <= 6, "yellow", "red")))
      valueBox(paste0("Grado ", y_val), paste("Yield (Y) - Testigo:", y_idx), icon = icon("balance-scale"), color = color_y)
    })
    
    output$vb_q <- renderValueBox({
      req(var_data())
      q_val <- var_data()$info$q_score
      
      # Traducción del grado a índice
      q_idx <- case_when(
        is.na(q_val) ~ "N/D",
        q_val == 8 ~ "< 90%",
        q_val == 7 ~ "90-95%",
        q_val == 6 ~ "95-100%",
        q_val == 5 ~ "100-110%",
        q_val == 4 ~ "105-110%",
        q_val == 3 ~ "110-115%",
        q_val == 2 ~ "> 115%",
        TRUE ~ as.character(q_val)
      )
      
      color_q <- ifelse(is.na(q_val), "navy", ifelse(q_val <= 4, "green", ifelse(q_val <= 6, "yellow", "red")))
      valueBox(paste0("Grado ", q_val), paste("Quality (Q) - Testigo:", q_idx), icon = icon("gem"), color = color_q)
    })
    
    # --- Tab: Historial Reproductivo ---
    output$vb_fam_totales <- renderValueBox({
      req(var_data())
      totales <- nrow(var_data()$familias)
      subtitle <- paste0("Cruces (♂:", var_data()$n_como_padre, " ♀:", var_data()$n_como_madre, ")")
      valueBox(totales, subtitle, icon = icon("users"), color = "purple")
    })
    
    output$vb_tasa_exito <- renderValueBox({
      req(var_data())
      fam <- var_data()$familias
      if (nrow(fam) == 0) return(valueBox("N/A", "Tasa de Selección", icon = icon("percentage"), color = "gray"))
      
      seleccionadas <- sum(fam$accion == "S", na.rm = TRUE)
      tasa <- round((seleccionadas / nrow(fam)) * 100, 1)
      
      color_tasa <- ifelse(tasa >= 20, "green", ifelse(tasa > 0, "yellow", "red"))
      valueBox(paste0(tasa, "%"), "Familias Seleccionadas (S)", icon = icon("trophy"), color = color_tasa)
    })
    
    output$plot_familias_historico <- renderPlot({
      req(var_data())
      fam <- var_data()$familias
      if (nrow(fam) == 0) {
        plot.new()
        text(0.5, 0.5, "Sin historial de familias", cex = 1.5, col = "#7f8c8d")
        return()
      }
      
      ggplot(fam, aes(x = ano, fill = accion_desc)) +
        geom_bar() +
        scale_fill_manual(values = c("Seleccionada" = "#27ae60", 
                                     "Rechazada" = "#c0392b", 
                                     "En Evaluación" = "#f39c12")) +
        labs(
          title = "Evaluación de Familias por Año",
          x = "Año del Cruce",
          y = "Cantidad de Familias",
          fill = "Decisión"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          legend.position = "bottom"
        )
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
      downloadButton(ns("download_ficha"), "Bajar Ficha (.xlsx)", class = "btn-info btn-block")
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
    
    return(var_data)
  })
}
