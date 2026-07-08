# ==============================================================================
# MOD_ASISTENTE.R — Módulo Shiny: Asistente de Campo (Quick Tools)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# NOTA: Las variedades activas provienen de floracion_master (KM 14).
# El catálogo histórico (categorias) usa nomenclatura diferente, no compatible.
# El asistente opera 100% sobre floracion_master.
# ==============================================================================

# --- UI del Módulo ---
mod_asistente_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        h2(tags$span(`data-i18n`="ast_title", "Asistente de Campo"), class = "m-0 text-primary fw-bold"),
        p(tags$span(`data-i18n`="ast_subtitle", "Herramientas rápidas para toma de decisiones en tiempo real."), class = "text-muted")
      ),
      
      navset_card_pill(
        id = ns("tabs_herramientas"),
        
        # HERRAMIENTA 1: Buscador de Mejores Parejas
        nav_panel(
          value = "tab_ast_matchmaker",
          title = tags$span(`data-i18n`="ast_tab_matchmaker", "Buscador de Mejores Parejas"),
          icon = icon("heart-circle-check"),
          
          # Fila de controles en la parte superior, resultados abajo
          layout_columns(
            col_widths = c(4, 8),
            div(class = "p-3 bg-light rounded me-2",
              h5(icon("sliders"), tags$span(`data-i18n`="ast_config_search", " Configurar búsqueda")),
              selectInput(ns("suelo_contexto"), tags$span(`data-i18n`="lbl_soil_type", "Tipo de Suelo:"),
                          choices = c("BUENO", "MAL_DRENADO", "ROCOSO"),
                          selected = "BUENO", width = "100%"),
              selectizeInput(ns("hembra_focal"), tags$span(`data-i18n`="ast_female_variety", "Variedad Hembra (madre):"),
                             choices = NULL,
                             options = list(placeholder = "Escriba o seleccione..."),
                             width = "100%"),
              p(class = "text-muted small",
                icon("info-circle"), tags$span(`data-i18n`="ast_select_soil_first", " Seleccione el suelo primero.")),
              actionButton(ns("btn_buscar"), tags$span(`data-i18n`="btn_find_matches", "Encontrar Mejores Parejas"),
                           class = "btn-primary w-100 mt-2",
                           icon = icon("bolt"))
            ),
            div(
              uiOutput(ns("ui_resultados_quick"))
            )
          )
        ),
        
        # HERRAMIENTA 2: Tabla completa de activas
        nav_panel(
          value = "tab_ast_inventory",
          title = tags$span(`data-i18n`="ast_tab_inventory", "Inventario KM 14"),
          icon = icon("list"),
          
          div(class = "p-3",
            fluidRow(
              column(4, selectInput(ns("inv_suelo"), tags$span(`data-i18n`="ast_filter_soil", "Filtrar por Suelo:"),
                                   choices = c("Todos", "BUENO", "MAL_DRENADO", "ROCOSO"))),
              column(4, numericInput(ns("inv_sx_min"), tags$span(`data-i18n`="ast_min_sex", "Sexo mínimo (sx ≥):"), value = 1, min = 1, max = 4, step = 1))
            ),
            withSpinner(DT::DTOutput(ns("tabla_inventario")), type = 4, color = "#15803d")
          )
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_asistente_server <- function(id, cat_var, pedigree_var, df_ped_wide, con, df_categorias_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Leer todas las variedades activas en campo (floracion_master) con su suelo y sx
    vars_activas <- reactive({
      tryCatch(
        dbGetQuery(con,
          "SELECT variedad, adapt, sx, sec, num, calles
           FROM floracion_master
           WHERE variedad IS NOT NULL
           ORDER BY variedad"),
        error = function(e) data.frame(variedad = character(), adapt = character(),
                                       sx = integer(), sec = character(),
                                       num = integer(), calles = integer())
      )
    })
    
    # --- 1. Actualizar dropdown según suelo ---
    observe({
      req(input$suelo_contexto)
      av <- vars_activas()
      if (nrow(av) == 0) return()
      
      vars_suelo <- av %>%
        filter(adapt == input$suelo_contexto) %>%
        pull(variedad) %>%
        unique() %>%
        sort()
      
      updateSelectizeInput(session, "hembra_focal",
                           choices = vars_suelo,
                           server = TRUE)
    })
    
    # --- 2. Lógica de Búsqueda Top 5 Machos ---
    sugerencias <- eventReactive(input$btn_buscar, {
      req(input$hembra_focal, input$suelo_contexto)
      
      av <- vars_activas()
      
      # Info de la hembra focal
      h_info <- av %>%
        filter(variedad == input$hembra_focal, adapt == input$suelo_contexto) %>%
        slice(1)
      
      if (nrow(h_info) == 0) return(data.frame())
      
      # Machos candidatos: mismo suelo, sx >= 3 (producen suficiente polen)
      # excluir la hembra focal
      machos <- av %>%
        filter(
          adapt == input$suelo_contexto,
          variedad != input$hembra_focal,
          sx >= 3
        ) %>%
        # Agrupar para obtener mejor sx y ubicacion
        group_by(variedad) %>%
        summarise(
          sx_max   = max(sx, na.rm = TRUE),
          sec_list = paste(unique(sec), collapse = "/"),
          num_list = paste(unique(num), collapse = "/"),
          adapt    = first(adapt),
          .groups  = "drop"
        ) %>%
        arrange(desc(sx_max)) %>%
        head(8) # candidatos antes del orden final
      
      if (nrow(machos) == 0) {
        # Si no hay machos con sx>=3, relajar a sx>=2
        machos <- av %>%
          filter(adapt == input$suelo_contexto,
                 variedad != input$hembra_focal,
                 sx >= 2) %>%
          group_by(variedad) %>%
          summarise(
            sx_max   = max(sx, na.rm = TRUE),
            sec_list = paste(unique(sec), collapse = "/"),
            num_list = paste(unique(num), collapse = "/"),
            adapt    = first(adapt),
            .groups  = "drop"
          ) %>%
          arrange(desc(sx_max)) %>%
          head(8)
      }
      
      machos %>% head(5)
    })
    
    # --- 3. Renderizar tarjetas de resultados ---
    output$ui_resultados_quick <- renderUI({
      
      # Estado antes de buscar
      if (is.null(input$btn_buscar) || input$btn_buscar == 0) {
        return(div(class = "p-5 text-center text-muted",
                   icon("seedling", class = "fa-3x mb-3 text-success"),
                   h4("Seleccione una hembra y presione buscar"),
                   p("El sistema encontrará los mejores machos disponibles en el suelo seleccionado.")))
      }
      
      req(sugerencias())
      df <- sugerencias()
      
      if (nrow(df) == 0) {
        return(div(class = "p-5 text-center text-muted",
                   icon("exclamation-triangle", class = "fa-2x mb-2 text-warning"),
                   h4("Sin machos disponibles"),
                   p("No hay variedades con suficiente sexo (sx ≥ 2) en ese suelo.",
                     "Verifique el inventario en la pestaña 'Inventario KM 14'.")))
      }
      
      tagList(
        div(class = "alert alert-success mb-3",
            icon("check-circle"),
            tags$b(" Hembra seleccionada: "), input$hembra_focal,
            tags$span(class = "ms-3", icon("map-pin"), " Suelo: ", input$suelo_contexto)
        ),
        layout_column_wrap(
          width = "250px",
          lapply(seq_len(nrow(df)), function(i) {
            row <- df[i, ]
            sx_col <- switch(as.character(min(4, max(1, row$sx_max))),
                             "4" = "bg-success text-white",
                             "3" = "bg-primary text-white",
                             "bg-info text-white")
            card(
              full_screen = FALSE,
              card_header(
                class = sx_col,
                tagList(icon("vial"), " Macho #", i)
              ),
              card_body(
                h4(tags$b(row$variedad), class = "text-center mb-2"),
                hr(class = "my-2"),
                tags$p(tags$b("Sexo (sx): "),
                       tags$span(class = paste0("badge ", ifelse(row$sx_max >= 3, "bg-success", "bg-warning")),
                                 paste("sx =", row$sx_max, ifelse(row$sx_max >= 3, "\u2713 Óptimo", "\u26a0 Moderado")))),
                tags$p(tags$b("Sector(es): "), row$sec_list),
                tags$p(tags$b("Número(s): "), row$num_list),
                tags$p(tags$b("Suelo: "), row$adapt)
              ),
              card_footer(class = "text-center text-muted",
                          tags$small(icon("star"), " Candidato por sexo disponible"))
            )
          })
        )
      )
    })
    
    # --- Tab 2: Inventario completo KM 14 ---
    output$tabla_inventario <- DT::renderDT({
      av <- vars_activas()
      if (nrow(av) == 0) {
        return(DT::datatable(data.frame(Mensaje = "No hay datos en floracion_master"),
                             options = list(dom = "t"), rownames = FALSE))
      }
      
      df <- av
      
      if (!is.null(input$inv_suelo) && input$inv_suelo != "Todos")
        df <- df %>% filter(adapt == input$inv_suelo)
      
      sx_min <- if (!is.null(input$inv_sx_min)) input$inv_sx_min else 1
      df <- df %>% filter(sx >= sx_min)
      
      df_show <- df %>%
        select(Variedad = variedad, Suelo = adapt,
               Sector = sec, Número = num, Calles = calles,
               Sexo = sx) %>%
        arrange(Suelo, desc(Sexo), Variedad)
      
      DT::datatable(df_show,
                    rownames = FALSE,
                    filter = "top",
                    options = list(pageLength = 20, scrollX = TRUE)) %>%
        DT::formatStyle("Sexo",
                        backgroundColor = DT::styleInterval(c(2, 3), c("#fee2e2", "#fef9c3", "#dcfce7")),
                        fontWeight = "bold")
    })
    
  })
}
