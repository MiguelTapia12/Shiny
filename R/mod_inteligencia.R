# ==============================================================================
# MOD_INTELIGENCIA.R — Módulo Shiny: Inteligencia Analítica (Módulo 7)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

mod_inteligencia_ui <- function(id) {
  ns <- NS(id)
  
  navset_card_pill(
    title = tagList(icon("brain"), " Inteligencia Analítica"),
    
    # --- TAB 1: Índice de Selección de Clones (Smith-Hazel) ---
    nav_panel(
      title = "Índice de Selección (Clones)",
      icon = icon("sort-amount-down"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Pesos del Índice (Smith-Hazel)",
          p(class = "text-muted", "Ajuste la importancia de cada rasgo para calcular el puntaje global de los clones."),
          sliderInput(ns("w_tca"), "Importancia TCA (Biomasa)", min = 0, max = 100, value = 40, post = "%"),
          sliderInput(ns("w_rend"), "Importancia REND (Azúcar %)", min = 0, max = 100, value = 40, post = "%"),
          sliderInput(ns("w_taa"), "Importancia TAA (Azúcar/Acre)", min = 0, max = 100, value = 20, post = "%"),
          hr(),
          uiOutput(ns("ui_filtro_ano_global")),
          helpText("Filtra el ranking para evaluar solo clones que hayan sido probados en las zafras seleccionadas."),
          hr(),
          p(class = "text-primary", tags$b("Suma Total: "), textOutput(ns("txt_suma_pesos"), inline = TRUE), "%"),
          uiOutput(ns("alerta_pesos"))
        ),
        card(
          full_screen = TRUE,
          class = "border-0 shadow-sm",
          card_header(
            class = "bg-light",
            tagList(icon("trophy"), " Ranking de Mejores Clones (EBVs Ajustados)")
          ),
          card_body(
            class = "p-0",
            div(style = "width: 100%; overflow-x: auto; padding: 15px;",
              DT::DTOutput(ns("tabla_ranking"))
            )
          )
        )
      )
    ),
    
    # --- TAB 2: Análisis de Estabilidad (Normas de Reacción) ---
    nav_panel(
      title = "Análisis de Estabilidad (GxE)",
      icon = icon("chart-line"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Filtros de Estabilidad",
          uiOutput(ns("badge_lsmeans")),
          p(strong("1. Rasgo y Escala"), style = "color: #15803d; border-bottom: 1px solid #15803d; padding-bottom: 5px;"),
          selectInput(ns("var_gxe"), "Rasgo a Evaluar:", 
                      choices = c("Rendimiento (% Azúcar)" = "rendimiento", "TCA (Ton/Acre)" = "tca", "TAA (Azúcar/Acre)" = "taa")),
          shinyWidgets::materialSwitch(ns("usar_relativo"), "Comparar vs Testigos Comerciales (%)", value = TRUE, status = "success"),
          
          tags$hr(class="my-2"),
          tags$h6("2. Contexto Ambiental", class = "text-primary", style="font-weight: bold;"),
          radioButtons(ns("modo_gxe"), "Modo de Agrupación:", 
                       choices = c("Separado por Suelo" = "separado", "Combinado (General)" = "combinado"),
                       selected = "separado", inline = FALSE),
          uiOutput(ns("ui_filtros_suelo")),
          uiOutput(ns("ui_filtros_exp")),
          
          tags$hr(class="my-2"),
          tags$h6("3. Selección de Variedades", class = "text-primary", style="font-weight: bold;"),
          uiOutput(ns("ui_filtros_vars")),
          sliderInput(ns("top_n_clones"), "O graficar Top N (si vacío):", min = 5, max = 50, value = 15),
          
          tags$hr(class="my-2"),
          helpText("El eje X muestra la evolución en los cortes (Plantilla, Retoños). Variedades estables mostrarán líneas horizontales altas.", style="font-size: 0.85em; color: #666;"),
          tags$hr(class="my-2"),
          downloadButton(ns("descargar_pdf_estabilidad"), "Reporte Ejecutivo (PDF)", class = "btn-danger w-100", style="color:white; font-weight:bold; border-radius: 20px;")
        ),
        card(
          full_screen = TRUE,
          class = "border-0 shadow-sm",
          card_header(
            class = "bg-light",
            tagList(icon("chart-line"), " Normas de Reacción: Genotipo × Ambiente × Corte")
          ),
          card_body(
            class = "p-0 d-flex flex-column",
            div(style = "width: 100%; overflow-x: auto; padding: 15px;",
              plotly::plotlyOutput(ns("plot_estabilidad"), height = "450px")
            ),
            div(style = "padding: 0 15px 15px 15px;",
              uiOutput(ns("interpretacion_gxe"))
            )
          )
        )
      )
    ),
    
    # --- TAB 3: Matriz de Correlaciones (Trade-offs) ---
    nav_panel(
      title = "Trade-offs (Correlaciones)",
      icon = icon("project-diagram"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Filtros de Correlación",
          selectInput(ns("fuente_cor"), "Fuente de Datos:", 
                      choices = c("Ensayos Avanzados" = "EA", "Evaluación de Familias" = "FAM")),
          uiOutput(ns("ui_ano_cor")),
          uiOutput(ns("ui_suelo_cor")),
          uiOutput(ns("ui_vars_cor")),
          helpText("Este gráfico muestra cómo interactúan los rasgos agronómicos. 
                   Valores cercanos a 1 (Verde) indican que cuando uno sube, el otro también.
                   Valores cercanos a -1 (Rojo) indican un sacrificio o trade-off genético.")
        ),
        card(
          full_screen = TRUE,
          class = "border-0 shadow-sm",
          card_header(
            class = "bg-light",
            tagList(icon("project-diagram"), " Red de Correlaciones (Pearson)")
          ),
          card_body(
            class = "p-0",
            div(style = "width: 100%; overflow-x: auto; padding: 15px; display: flex; justify-content: center;",
              plotly::plotlyOutput(ns("plot_correlacion"), height = "600px", width = "100%")
            ),
            uiOutput(ns("texto_correlacion"))
          )
        )
      )
    ),
    
    # --- TAB 4: GGE Biplot (Mega-Ambientes) ---
    nav_panel(
      title = "Mega-Ambientes (GGE)",
      icon = icon("compass"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Filtros GGE Biplot",
          selectInput(ns("var_gge"), "Rasgo a Evaluar:",
                      choices = c("Rendimiento (% Azúcar)" = "rendimiento",
                                  "TCA (Ton/Acre)" = "tca",
                                  "TAA (Azúcar/Acre)" = "taa")),
          uiOutput(ns("ui_filtro_ano_gge")),
          uiOutput(ns("ui_filtros_exp_gge")),
          uiOutput(ns("ui_filtros_vars_gge")),
          sliderInput(ns("top_n_gge"), "Mostrar Top N Clones (+ Testigos):", min = 5, max = 50, value = 15),
          helpText("Interpretación del GGE Biplot:
                   - Puntos: Variedades (Clones).
                   - Flechas: Suelos (Ambientes).
                   - Flechas muy juntas indican ambientes redundantes.
                   - El clon más lejos en la dirección de una flecha es el más adaptado a ese ambiente.")
        ),
        card(
          full_screen = TRUE,
          class = "border-0 shadow-sm",
          card_header(
            class = "bg-light",
            tagList(icon("compass"), " GGE Biplot (PCA de Suelos vs Variedades)")
          ),
          card_body(
            class = "p-0",
            div(style = "width: 100%; overflow-x: auto; padding: 15px; display: flex; justify-content: center;",
              plotly::plotlyOutput(ns("plot_gge"), height = "600px", width = "100%")
            ),
            uiOutput(ns("texto_gge"))
          )
        )
      )
    ),
    
    # --- TAB 5: DASHBOARD DE PARENTALES Y CRUZAMIENTOS ---
    nav_panel(
      title = "Población y Familias",
      icon = icon("users"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Filtros de Progenie",
          uiOutput(ns("ui_fam_ano")),
          uiOutput(ns("ui_fam_suelo")),
          hr(),
          selectInput(ns("prog_var"), "Variable Principal (GCA):",
                      choices = c("TCA (Ton/Acre)" = "tca", "Rendimiento" = "rend", "TAA (Azúcar/Acre)" = "tsa"),
                      multiple = FALSE, selected = "tca"),
          sliderInput(ns("prog_topn"), "Top N Parentales:", min = 5, max = 50, value = 15, step = 5),
          helpText("El GCA (Habilidad Combinatoria General) mide la ventaja genética promedio que un parental hereda a su progenie.")
        ),
        
        # Panel Principal: Dashboard de Parentales
        navset_card_tab(
          id = ns("tabs_familias"),
          
          # Pestaña 1: Madres
          nav_panel(
            title = "Análisis de Madres", icon = icon("venus"),
            layout_columns(
              col_widths = c(12),
              card(
                card_header(tagList(icon("chart-bar"), " Top Madres por Habilidad Combinatoria (GCA)")),
                plotly::plotlyOutput(ns("plot_madres"), height = "500px")
              ),
              card(
                card_header(tagList(icon("table"), " Datos de Desempeño (Madres)")),
                DT::DTOutput(ns("dt_madres"))
              )
            )
          ),
          
          # Pestaña 2: Padres
          nav_panel(
            title = "Análisis de Padres", icon = icon("mars"),
            layout_columns(
              col_widths = c(12),
              card(
                card_header(tagList(icon("chart-bar"), " Top Padres por Habilidad Combinatoria (GCA)")),
                plotly::plotlyOutput(ns("plot_padres"), height = "500px")
              ),
              card(
                card_header(tagList(icon("table"), " Datos de Desempeño (Padres)")),
                DT::DTOutput(ns("dt_padres"))
              )
            )
          ),
          
          # Pestaña 3: Simulador de Cruces
          nav_panel(
            title = "Simulador de Cruces (Élite)", icon = icon("magic"),
            card(
              card_header(tagList(icon("star"), " Cruces Élite Sugeridos")),
              p("Las siguientes combinaciones maximizan la ventaja genética esperada basándose en el GCA de los mejores parentales disponibles.", style="color: #666;"),
              uiOutput(ns("ui_cruces_elite"))
            )
          )
        )
      )
    ),
    
    # --- TAB 6: ANÁLISIS DE ISOPRODUCTIVIDAD (Curvas Iso) ---
    nav_panel(
      title = "Isoproductividad",
      icon = icon("bullseye"),
      layout_sidebar(
        border = FALSE, fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          title = "Filtros de Isoproductividad",
          uiOutput(ns("badge_lsmeans_iso")),
          
          p(strong("1. Contexto Ambiental"), style = "color: #15803d; border-bottom: 1px solid #15803d; padding-bottom: 5px;"),
          uiOutput(ns("ui_iso_ano")),
          uiOutput(ns("ui_iso_suelo")),
          uiOutput(ns("ui_iso_exp")),
          
          tags$hr(class="my-2"),
          tags$h6("2. Selección de Variedades", class = "text-primary", style="font-weight: bold;"),
          uiOutput(ns("ui_iso_vars")),
          sliderInput(ns("iso_top_n"), "O graficar Top N (si vacío):", min = 5, max = 50, value = 20),
          
          tags$hr(class="my-2"),
          tags$h6("3. Apariencia Visual", class = "text-primary", style="font-weight: bold;"),
          selectInput(ns("iso_gradient"), "Tema Visual (Gradiente):", 
                      choices = c("Vice City", "Evening Night", "Ibiza Sunset", "Purple Love", 
                                  "Green Beach", "Deep Blue", "Flickr", "Sublime Vivid", 
                                  "Blush", "Moonlit Asteroid", "MegaTron", "DG", 
                                  "Azure Pop", "Tranfile", "Timber", "Sky"), 
                      selected = "Vice City"),
          downloadButton(ns("reporte_iso"), "Generar Reporte Iso", class = "btn-success w-100 mt-3")
        ),
        card(
          card_header(tagList(icon("chart-area"), " Curvas de Isoproductividad (TCA vs Rendimiento)")),
          plotOutput(ns("plot_iso"), height = "600px")
        )
      )
    )
  )
}

mod_inteligencia_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================================
    # REACTIVE COMPARTIDA: Cachear lectura de ensayos_avanzados (Mejora #1)
    # ==========================================================================
    ea_data <- reactive({
      if (!DBI::dbExistsTable(con, "ensayos_avanzados")) return(NULL)
      dbReadTable(con, "ensayos_avanzados")
    })
    
    # ==========================================================================
    # REACTIVE: Medias Ajustadas (LSMeans) pre-calculadas en el ETL
    # ==========================================================================
    ea_adjusted <- reactive({
      if (!DBI::dbExistsTable(con, "ensayos_ajustados")) return(NULL)
      dbReadTable(con, "ensayos_ajustados")
    })
    
    # --------------------------------------------------------------------------
    # INDICADOR UI PARA MEDIAS AJUSTADAS
    # --------------------------------------------------------------------------
    output$badge_lsmeans <- renderUI({
      if (DBI::dbExistsTable(con, "ensayos_ajustados")) {
        tags$span(class = "badge bg-success mb-3", style = "font-size: 0.85em; width: 100%; white-space: normal;",
                  icon("bolt"), " Datos DBCA: Medias Ajustadas (LSMeans)")
      } else {
        tags$span(class = "badge bg-warning text-dark mb-3", style = "font-size: 0.85em; width: 100%; white-space: normal;",
                  icon("exclamation-triangle"), " Datos: Promedios Crudos")
      }
    })
    
    # --------------------------------------------------------------------------
    # 1. ÍNDICE DE SELECCIÓN (CLONES)
    # --------------------------------------------------------------------------
    
    output$ui_filtro_ano_global <- renderUI({
      ea <- ea_data()
      if (is.null(ea)) return(NULL)
      anos <- sort(unique(ea$ano_zafra[!is.na(ea$ano_zafra)]), decreasing = TRUE)
      if (length(anos) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("filtro_ano_global"), "Años de Zafra (Global):",
                  choices = anos, selected = anos, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} años seleccionados"
                  ))
    })
    
    output$txt_suma_pesos <- renderText({
      suma <- input$w_tca + input$w_rend + input$w_taa
      paste0(suma)
    })
    
    output$alerta_pesos <- renderUI({
      suma <- input$w_tca + input$w_rend + input$w_taa
      if (suma != 100) {
        tags$div(class = "alert alert-danger mt-2 p-2", style = "font-size: 0.85em;",
                 icon("exclamation-triangle"), " ¡Atención! Los pesos suman ", suma, "%. Deben sumar exactamente 100%.")
      } else {
        tags$div(class = "alert alert-success mt-2 p-2", style = "font-size: 0.85em;",
                 icon("check-circle"), " Pesos configurados correctamente al 100%.")
      }
    })
    
    ranking_data <- reactive({
      if (!exists("ebvs_var") || is.null(ebvs_var)) return(NULL)
      
      df <- ebvs_var
      
      # Filtro Global por Año
      if (!is.null(input$filtro_ano_global) && length(input$filtro_ano_global) > 0) {
        ea <- ea_data()
        if (!is.null(ea)) {
          clones_recientes <- unique(ea$variedad[ea$ano_zafra %in% input$filtro_ano_global])
          df <- df %>% filter(variedad %in% clones_recientes)
        }
      }
      
      suma_pesos <- input$w_tca + input$w_rend + input$w_taa
      if (suma_pesos == 0) suma_pesos <- 1
      
      w1 <- input$w_tca / suma_pesos
      w2 <- input$w_rend / suma_pesos
      w3 <- input$w_taa / suma_pesos
      
      df_calc <- df %>%
        filter(!is.na(ebv_tca) & !is.na(ebv_rend)) %>%
        mutate(
          z_tca = scale(ebv_tca)[,1],
          z_rend = scale(ebv_rend)[,1],
          z_taa = if ("ebv_tsh" %in% names(.) && sum(!is.na(ebv_tsh)) > 3) {
             v <- scale(ebv_tsh)[,1]
             ifelse(is.na(v), 0, v)
          } else {
             rep(0, n())
          }
        ) %>%
        mutate(
          Puntaje_SI = (z_tca * w1) + (z_rend * w2) + (z_taa * w3)
        ) %>%
        arrange(desc(Puntaje_SI))
        
      cat_df <- dbReadTable(con, "categorias") %>% select(variedad, categoria, status)
      
      df_final <- df_calc %>%
        left_join(cat_df, by = "variedad") %>%
        select(variedad, categoria, status, Puntaje_SI, ebv_tca, ebv_rend, ebv_tsh = any_of("ebv_tsh"), total_obs) %>%
        mutate(across(where(is.numeric), ~round(., 3)))
        
      df_final
    })
    
    output$tabla_ranking <- DT::renderDT({
      req(ranking_data())
      
      df_rank <- ranking_data() %>%
        mutate(Rank = row_number(),
               Medalla = case_when(
                 Rank == 1 ~ "\U0001f947",
                 Rank == 2 ~ "\U0001f948",
                 Rank == 3 ~ "\U0001f949",
                 TRUE ~ ""
               )) %>%
        select(Medalla, everything(), -Rank)
      
      DT::datatable(df_rank,
        rownames = FALSE,
        colnames = c("", "Variedad", "Cat.", "Estatus", "Puntaje SI", "TCA (Desv)", "REND (Desv)", "TSH (Desv)", "Ensayos (N)"),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          order = list(list(4, 'desc')),
          columnDefs = list(list(className = 'dt-center', targets = 0))
        )
      ) %>%
      DT::formatStyle(
        'Puntaje_SI',
        background = DT::styleColorBar(range(df_rank$Puntaje_SI, na.rm = TRUE), '#15803d'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        c('ebv_tca', 'ebv_rend', 'ebv_tsh'),
        backgroundColor = DT::styleInterval(c(-1.0, -0.2, 0.2, 1.0), c('#f8d7da', '#fdfdfe', '#ffffff', '#e8f5e9', '#d4edda')),
        color = DT::styleInterval(c(0), c('#721c24', '#155724'))
      ) %>%
      DT::formatStyle(
        'variedad',
        fontWeight = 'bold'
      )
    })
    
    # --------------------------------------------------------------------------
    # 2. ANÁLISIS DE ESTABILIDAD (GxE HEATMAP)
    # --------------------------------------------------------------------------
    
    # Renderizar checkbox dinámicamente con los suelos disponibles
    output$ui_filtros_suelo <- renderUI({
      ea <- ea_data()
      if (is.null(ea)) return(NULL)
      suelos_disp <- unique(ea$suelo)
      suelos_disp <- suelos_disp[!is.na(suelos_disp) & suelos_disp != ""]
      
      if (length(suelos_disp) == 0) return(NULL)
      
      shinyWidgets::checkboxGroupButtons(session$ns("filtros_suelo"), "Suelos a Analizar:",
                         choices = suelos_disp,
                         selected = suelos_disp,
                         status = "outline-primary",
                         size = "sm",
                         direction = "vertical",
                         width = "100%")
    })
    
    # Renderizar selector de variedades
    output$ui_filtros_vars <- renderUI({
      # Usar los datos ajustados si existen, sino fallback a crudos
      ea <- ea_adjusted()
      if (is.null(ea) || nrow(ea) == 0) {
        ea <- ea_data()
      }
      if (is.null(ea)) return(NULL)
      if (!"ind_testigo" %in% names(ea)) ea$ind_testigo <- "N"
      
      vars <- sort(unique(ea$variedad[!is.na(ea$variedad) & ea$ind_testigo != "S"]))
      
      shinyWidgets::pickerInput(session$ns("filtros_vars"), "Comparar Variedades vs Testigos:",
                  choices = vars, selected = NULL, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    title = "Seleccionar..."
                  ))
    })
    
    # Renderizar selector de experimentos
    output$ui_filtros_exp <- renderUI({
      # Preferir medias ajustadas; fallback a datos crudos si no existen
      ea <- ea_adjusted()
      if (is.null(ea)) ea <- ea_data()
      if (is.null(ea)) return(NULL)
      if (!"num_experimento" %in% names(ea)) return(NULL)
      
      exps <- sort(unique(ea$num_experimento[!is.na(ea$num_experimento)]))
      if (length(exps) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("filtros_exp"), "Filtrar Experimentos (opcional):",
                  choices = exps, selected = exps, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} exps seleccionados"
                  ))
    })
    
    estabilidad_data <- reactive({
      # Usar medias ajustadas DBCA (una fila por variedad × experimento × corte)
      ea <- ea_adjusted()
      if (is.null(ea) || nrow(ea) == 0) {
        # Fallback a datos crudos si el ajuste no generó resultados
        ea <- ea_data()
        if (is.null(ea)) return(NULL)
      }
      
      if (!"suelo" %in% names(ea)) {
         cat_suelo <- dbReadTable(con, "categorias") %>% select(variedad, suelo = adapt)
         ea <- ea %>% left_join(cat_suelo, by = "variedad")
      }
      if (!"corte_nombre" %in% names(ea)) ea$corte_nombre <- "Plantilla"
      
      # Estandarizar nombre de cortes a "Plantilla", "Retoño 1", "Retoño 2", etc.
      ea <- ea %>% mutate(
        corte_nombre = case_when(
          tolower(trimws(corte_nombre)) %in% c("plantilla", "pl", "p") ~ "Plantilla",
          grepl("^corte", tolower(corte_nombre)) ~ {
            n <- as.numeric(gsub("[^0-9]", "", corte_nombre))
            ifelse(is.na(n) | n <= 1, "Plantilla", paste("Retoño", n - 1))
          },
          grepl("^reto[nñ]o", tolower(corte_nombre)) ~ {
            n <- as.numeric(gsub("[^0-9]", "", corte_nombre))
            ifelse(is.na(n), "Retoño 1", paste("Retoño", n))
          },
          TRUE ~ as.character(corte_nombre)
        )
      )
      
      if (!"num_experimento" %in% names(ea)) ea$num_experimento <- "ALL"
      if (!"ind_testigo" %in% names(ea)) ea$ind_testigo <- "N"
      
      # Normalizar nombres de columna TAA: tsh/tsa → taa
      # Garantiza compatibilidad entre ensayos_avanzados (tsh) y ensayos_ajustados (taa)
      if (!"taa" %in% names(ea)) {
        if ("tsh" %in% names(ea)) ea <- dplyr::rename(ea, taa = tsh)
        else if ("tsa" %in% names(ea)) ea <- dplyr::rename(ea, taa = tsa)
      }
      
      req(input$filtros_suelo)
      
      rasgo <- input$var_gxe
      
      # Filtro de experimento
      gxe <- ea
      if (!is.null(input$filtros_exp) && length(input$filtros_exp) > 0) {
        gxe <- gxe %>% filter(num_experimento %in% input$filtros_exp)
      }
      
      # Calcular media del experimento para corrección ambiental
      medias_exp <- gxe %>%
        filter(!is.na(!!sym(rasgo))) %>%
        group_by(num_experimento, suelo, corte_nombre) %>%
        summarise(media_ambiente = mean(!!sym(rasgo), na.rm = TRUE), .groups = "drop")
        
      # Datos de testigos por ambiente
      testigos_env <- gxe %>%
        filter(ind_testigo == "S", !is.na(!!sym(rasgo))) %>%
        select(num_experimento, suelo, corte_nombre, variedad, valor = !!sym(rasgo))
        
      # 1. Agrupar por variedad para tener 1 fila por variedad por experimento
      testigos_por_var <- testigos_env %>%
        group_by(num_experimento, suelo, corte_nombre, variedad) %>%
        summarise(valor_mean = mean(valor, na.rm = TRUE), .groups = "drop")
        
      # 2. Resumir por experimento contando variedades únicas
      testigos_summary <- testigos_por_var %>%
        group_by(num_experimento, suelo, corte_nombre) %>%
        summarise(
          suma_testigos   = sum(valor_mean, na.rm = TRUE),
          n_testigos      = n(), # Ahora sí es el número de variedades comerciales distintas
          media_testigos  = mean(valor_mean, na.rm = TRUE),
          .groups = "drop"
        )
        
      gxe <- gxe %>%
        left_join(testigos_summary %>% select(num_experimento, suelo, corte_nombre, media_testigos),
                  by = c("num_experimento","suelo","corte_nombre")) %>%
        left_join(medias_exp, by = c("num_experimento","suelo","corte_nombre")) %>%
        mutate(
          base_ref = case_when(
            !is.na(media_testigos) ~ media_testigos,  # Siempre comparar contra el panel comercial
            TRUE                   ~ media_ambiente   # Fallback si no hay testigos vivos en el experimento
          ),
          # Piso matemático para evitar que la división por valores ínfimos dispare los relativos a >500%
          base_ref = case_when(
            rasgo == "taa" & base_ref < 1.0 ~ 1.0,
            rasgo == "tca" & base_ref < 5.0 ~ 5.0,
            rasgo == "rendimiento" & base_ref < 5.0 ~ 5.0,
            TRUE ~ base_ref
          ),
          valor_plot = if (isTRUE(input$usar_relativo)) {
            val_clamp <- ifelse(!!sym(rasgo) < 0, 0, !!sym(rasgo)) # Clamp negativos a 0
            (val_clamp / base_ref) * 100 
          } else {
            !!sym(rasgo)
          }
        )
        
      if (!is.null(input$filtros_vars) && length(input$filtros_vars) > 0) {
        sel_vars <- input$filtros_vars
      } else {
        sel_vars <- tryCatch(
          head(ranking_data()$variedad, input$top_n_clones),
          error = function(e) character(0)
        )
        if (length(sel_vars) == 0) return(NULL)
      }
      
      # Filtrar: Top Clones (o vars seleccionadas) o Testigos, y Suelo
      gxe_plot <- gxe %>%
        filter(variedad %in% sel_vars | ind_testigo == "S") %>%
        filter(suelo %in% input$filtros_suelo)
        
      agrupacion <- if(!is.null(input$modo_gxe) && input$modo_gxe == "combinado") {
        c("variedad", "corte_nombre", "ind_testigo")
      } else {
        c("variedad", "suelo", "corte_nombre", "ind_testigo")
      }
        
      gxe_plot <- gxe_plot %>%
        group_by(across(all_of(agrupacion))) %>%
        summarise(
          valor_medio = mean(valor_plot, na.rm = TRUE),
          valor_absoluto = mean(!!sym(rasgo), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(valor_medio))
        
      if(!is.null(input$modo_gxe) && input$modo_gxe == "combinado") {
        gxe_plot$suelo <- "General (Suelos Combinados)"
      }
        
      if (nrow(gxe_plot) == 0) return(NULL)
        
      # Ordenar cronológicamente los retoños
      niveles_esperados <- c("Plantilla", paste("Retoño", 1:20))
      gxe_plot <- gxe_plot %>%
        mutate(corte_nombre = factor(corte_nombre, levels = intersect(niveles_esperados, unique(corte_nombre)), ordered = TRUE)) %>%
        arrange(suelo, variedad, corte_nombre)
        
      gxe_plot
    })
    
    estabilidad_plot_base <- reactive({
      df_plot <- estabilidad_data()
      if (is.null(df_plot) || nrow(df_plot) == 0) return(NULL)
      
      titulo_y <- names(which(c("Rendimiento (% Azúcar)" = "rendimiento", "TCA (Ton/Acre)" = "tca", "TAA (Azúcar/Acre)" = "taa") == input$var_gxe))
      if(length(titulo_y) == 0) titulo_y <- "Valor"
      if(isTRUE(input$usar_relativo)) titulo_y <- paste("% Relativo de", titulo_y)
      
      # Paleta dinámica: genera exactamente N colores distintos (Mejora #5)
      n_vars <- length(unique(df_plot$variedad))
      paleta_semilla <- c("#E69F00", "#56B4E9", "#009E73", "#b45309",
                          "#0072B2", "#D55E00", "#CC79A7", "#000000",
                          "#999999", "#44AA99", "#332288", "#AA4499")
      paleta_dinamica <- if (n_vars <= length(paleta_semilla)) {
        paleta_semilla[1:n_vars]
      } else {
        grDevices::colorRampPalette(paleta_semilla)(n_vars)
      }
      
      texto_ref <- "vs Testigos"
      
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(
        x = corte_nombre, 
        y = valor_medio, 
        group = variedad, 
        color = variedad,
        text = paste0(
          "<b>", variedad, "</b>",
          ifelse(ind_testigo == "S", " \u2726 TESTIGO", ""),
          "<br>Corte: ", corte_nombre,
          "<br>Suelo: ", suelo,
          "<br>", titulo_y, ": <b>", round(valor_medio, 2), "</b>",
          ifelse(
            isTRUE(input$usar_relativo),
            paste0("<br>", texto_ref, ": <b>",
                   ifelse(valor_medio >= 100,
                          paste0("+", round(valor_medio - 100, 1)),
                          round(valor_medio - 100, 1)),
                   "%</b>"),
            ""
          )
        )
      )) +
        ggplot2::geom_line(ggplot2::aes(alpha = ind_testigo, size = ind_testigo, linetype = ind_testigo)) +
        ggplot2::geom_point(ggplot2::aes(shape = ind_testigo, size = ind_testigo), alpha = 0.9) +
        ggplot2::scale_alpha_manual(values = c("S" = 0.45, "N" = 0.95), guide = "none") +
        ggplot2::scale_size_manual(values  = c("S" = 0.7,  "N" = 1.1),  guide = "none") +
        ggplot2::scale_shape_manual(values = c("S" = 17,   "N" = 16),   guide = "none") +
        ggplot2::scale_linetype_manual(values = c("S" = "dashed", "N" = "solid"), guide = "none") +
        ggplot2::scale_color_manual(values = paleta_dinamica) +
        ggplot2::facet_wrap(~suelo, scales = "free_x") +
        ggplot2::theme_minimal(base_family = "sans") +
        ggplot2::labs(
          x = "Retoño",
          y = titulo_y,
          color = "Variedades"
        ) +
        ggplot2::theme(
          axis.text.x       = ggplot2::element_text(angle = 40, hjust = 1, size = 9, color = "#374151"),
          axis.text.y       = ggplot2::element_text(size = 9, color = "#374151"),
          axis.title        = ggplot2::element_text(face = "bold", size = 10, color = "#1f2937"),
          panel.spacing     = ggplot2::unit(2, "lines"),
          strip.text        = ggplot2::element_text(face = "bold", size = 12, color = "#fff"),
          strip.background  = ggplot2::element_rect(fill = "#15803d", color = NA),
          legend.position   = "right",
          legend.title      = ggplot2::element_text(face = "bold", size = 11),
          legend.text       = ggplot2::element_text(size = 9),
          legend.key.width  = ggplot2::unit(1.8, "cm"),
          panel.grid.major  = ggplot2::element_line(color = "#f3f4f6", linewidth = 0.5),
          panel.grid.minor  = ggplot2::element_blank(),
          panel.background  = ggplot2::element_rect(fill = "#fafafa", color = NA),
          panel.border      = ggplot2::element_rect(colour = "#e5e7eb", fill = NA, linewidth = 0.8),
          plot.background   = ggplot2::element_rect(fill = "#ffffff", color = NA)
        )
      
      if(isTRUE(input$usar_relativo)) {
        g <- g +
          ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "#6b7280", linewidth = 0.6) +
          ggplot2::annotate("text", x = Inf, y = 101.5, label = "── 100%", size = 2.8, color = "#6b7280", hjust = 1.1, fontface = "italic")
      }
      return(g)
    })
    
    output$plot_estabilidad <- plotly::renderPlotly({
      g <- estabilidad_plot_base()
      if (is.null(g)) {
        return(plotly::plot_ly() %>% layout(title = "Sin datos suficientes para GxE"))
      }
      
      p <- plotly::ggplotly(g, tooltip = "text") %>%
        plotly::layout(
          legend = list(title = list(text = "<b>Variedades</b>")),
          hovermode = "closest"
        )
        
      if(isTRUE(input$usar_relativo)) {
        p <- p %>% plotly::layout(
          shapes = list(
            list(type="rect", xref="paper", yref="y",
                 x0=0, x1=1, y0=100, y1=200,
                 fillcolor="#15803d", opacity=0.05, line=list(width=0)),
            list(type="rect", xref="paper", yref="y",
                 x0=0, x1=1, y0=0, y1=100,
                 fillcolor="#dc2626", opacity=0.05, line=list(width=0))
          )
        )
      }
      
      p %>% plotly::config(toImageButtonOptions = list(format = "svg", filename = "estabilidad_gxe"))
    })
    
    estabilidad_textos <- reactive({
      df <- estabilidad_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      vars_sel <- input$filtros_vars
      if (is.null(vars_sel) || length(vars_sel) == 0) return(NULL)
      
      titulo_rasgo <- names(which(c("Rendimiento (% Azúcar)" = "rendimiento", "TCA (Ton/Acre)" = "tca", "TAA (Azúcar/Acre)" = "taa") == input$var_gxe))
      if (length(titulo_rasgo) == 0) titulo_rasgo <- "el rasgo evaluado"
      
      res <- lapply(vars_sel, function(v) {
        df_v <- df %>% filter(variedad == v)
        if (nrow(df_v) == 0) return(NULL)
        
        cortes_v <- as.character(unique(df_v$corte_nombre))
        df_test <- df %>% filter(ind_testigo == "S", corte_nombre %in% cortes_v)
        
        media_v <- mean(df_v$valor_medio, na.rm = TRUE)
        media_v_abs <- mean(df_v$valor_absoluto, na.rm = TRUE)
        media_test_abs <- mean(df_test$valor_absoluto, na.rm = TRUE)
        diff_abs <- media_v_abs - media_test_abs
        
        unidad_texto <- case_when(
          input$var_gxe == "taa" ~ "Ton Azúcar/Acre",
          input$var_gxe == "tca" ~ "Ton Caña/Acre",
          TRUE ~ "% de Rendimiento"
        )
        
        cortes_v <- as.character(unique(df_v$corte_nombre))
        n_plantillas <- sum(cortes_v == "Plantilla")
        n_retonos <- sum(grepl("Retoño", cortes_v))
        
        str_cortes <- ""
        if (n_plantillas > 0 && n_retonos > 0) {
          str_cortes <- paste("a través de", n_plantillas, "plantilla y", n_retonos, ifelse(n_retonos == 1, "retoño", "retoños"))
        } else if (n_plantillas > 0) {
          str_cortes <- "evaluada solo en plantilla"
        } else if (n_retonos > 0) {
          str_cortes <- paste("a través de", n_retonos, ifelse(n_retonos == 1, "retoño", "retoños"))
        }

        # 1. Ignorar plantilla para calcular inestabilidad real
        df_slope <- df_v %>% filter(corte_nombre != "Plantilla")
        if (nrow(df_slope) >= 2) {
          y_vals <- df_slope$valor_medio
          x_vals <- seq_along(y_vals)
          slope <- tryCatch(coef(lm(y_vals ~ x_vals))[2], error = function(e) 0)
        } else {
          slope <- 0
        }
        
        # 2. Lenguaje cauteloso si N <= 3 retoños
        if (n_retonos <= 3) {
           if (slope > 1.5) tend <- paste0("una tendencia ascendente en los retoños disponibles (n=", n_retonos, ")")
           else if (slope < -1.5) tend <- paste0("una tendencia descendente en los retoños disponibles (n=", n_retonos, ")")
           else tend <- paste0("una tendencia estable en los retoños disponibles (n=", n_retonos, ")")
           
           frase_final <- " (Se requiere avanzar a más cortes para confirmar su estabilidad a largo plazo)."
        } else {
           if (slope > 2) {
             tend <- paste("un perfil marcadamente ascendente", str_cortes)
             frase_final <- " Convirtiéndola en una variedad altamente estable con gran potencial de retoñamiento en cortes avanzados."
           } else if (slope > 0.5) {
             tend <- paste("un perfil ligeramente ascendente", str_cortes)
             frase_final <- " Mostrando buena estabilidad y adaptabilidad a medida que avanza la edad del cultivo."
           } else if (slope < -2) {
             tend <- paste("un perfil marcadamente descendente", str_cortes)
             frase_final <- " Lo que indica inestabilidad y un deterioro significativo en el rendimiento en cortes avanzados."
           } else if (slope < -0.5) {
             tend <- paste("un perfil ligeramente descendente", str_cortes)
             frase_final <- " Indicando una estabilidad moderada pero con signos de pérdida de vigor."
           } else {
             tend <- paste("un perfil estable", str_cortes)
             frase_final <- " Destacando como una variedad altamente estable a lo largo del tiempo."
           }
           
            # Contextualize extreme values if using Testigos (LOO) as the baseline
            if (isTRUE(input$usar_relativo)) {
              diferencia_formateada <- paste0(sprintf("%.1f", abs(diff_abs)), " ", unidad_texto)
              if (media_v > 150) {
                frase_final <- paste0(" Al compararse frente a los testigos, su alto porcentaje relativo indica que las variedades comerciales sufrieron un deterioro severo en este ambiente, mientras la candidata mantuvo su vigor, superando al promedio comercial por un margen real de +", diferencia_formateada, " a lo largo de los cortes evaluados.")
              } else if (media_v >= 100) {
                frase_final <- paste0(" Evaluada bajo el ancla de testigos locales, logra un balance positivo frente al promedio comercial, con una ventaja real de +", diferencia_formateada, " a lo largo de los cortes evaluados.")
              } else {
                frase_final <- paste0(" Evaluada bajo el ancla de testigos locales, queda por debajo del promedio comercial, con un déficit real de -", diferencia_formateada, " a lo largo de los cortes evaluados.")
              }
            }
        }
        
        # 3. Comparación contra los testigos
        testigos_nombres <- unique(df_test$variedad)
        if (length(testigos_nombres) > 0) {
           test_medias <- df_test %>% group_by(variedad) %>% summarise(m = mean(valor_medio, na.rm = TRUE), .groups="drop")
           formato_num <- if(isTRUE(input$usar_relativo)) "%" else ""
           test_medias <- test_medias %>% mutate(etiqueta = paste0(variedad, " (", round(m, 1), formato_num, ")"))
           peores <- test_medias %>% filter(m < media_v) %>% pull(etiqueta)
           mejores <- test_medias %>% filter(m >= media_v) %>% pull(etiqueta)
           
           if (length(peores) > 0 && length(mejores) == 0) {
             txt_comp <- " superando a todos los testigos evaluados en este ambiente."
           } else if (length(peores) == 0 && length(mejores) > 0) {
             txt_comp <- " situándose por debajo de todos los testigos evaluados."
           } else if (length(peores) > 0 && length(mejores) > 0) {
             txt_comp <- paste0(" situándose por encima de ", paste(peores, collapse=", "), " pero por debajo de ", paste(mejores, collapse=", "), ".")
           } else {
             txt_comp <- "."
           }
        } else {
           txt_comp <- "."
        }
        
        rend_text <- if(isTRUE(input$usar_relativo)) paste0(round(media_v, 1), "% relativo") else round(media_v, 1)
        
        html_str <- paste0("La variedad <b>", v, "</b> muestra ", tend, ", con un promedio general de <b>", rend_text, " en ", titulo_rasgo, "</b>", txt_comp, frase_final)
        plain_str <- gsub("%", "\\\\%", paste0("La variedad ", v, " muestra ", tend, ", con un promedio general de ", rend_text, " en ", titulo_rasgo, txt_comp, frase_final))
                 
        list(html = HTML(html_str), plain = plain_str)
      })
      
      # Filtrar nulos
      res <- res[!sapply(res, is.null)]
      return(res)
    })
    
    output$interpretacion_gxe <- renderUI({
      textos_list <- estabilidad_textos()
      if (is.null(textos_list) || length(textos_list) == 0) {
        return(div(class = "alert alert-secondary mt-3", icon("lightbulb"), " Seleccione una o más variedades específicas en el panel lateral para generar una interpretación automática vs los testigos."))
      }
      
      html_items <- lapply(textos_list, function(t) {
        tags$li(style = "font-size: 0.95em; margin-bottom: 5px;", t$html)
      })
      
      # Alerta de Modo Combinado (Mejora #11)
      alerta_combinado <- NULL
      if (!is.null(input$modo_gxe) && input$modo_gxe == "combinado") {
        alerta_combinado <- div(class = "alert alert-info mt-2 mb-2 py-2", style = "font-size: 0.88em;",
          icon("info-circle"), " Modo Combinado activo: los promedios fusionan todos los suelos seleccionados en un solo análisis general."
        )
      }
      
      tagList(
        alerta_combinado,
        div(class = "alert alert-success mt-3 shadow-sm", style = "border-left: 5px solid #28a745;",
            tags$h6(icon("robot"), " Análisis de Estabilidad:", class="text-success", style="font-weight:bold;"),
            tags$ul(class = "mb-0", do.call(tagList, html_items))
        )
      )
    })
    
    output$descargar_pdf_estabilidad <- downloadHandler(
      filename = function() {
        paste0("Reporte_Estabilidad_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
      },
      content = function(file) {
        id <- showNotification("Generando reporte PDF, por favor espere...", duration = NULL, type = "message")
        on.exit(removeNotification(id), add = TRUE)
        
        tempReport <- file.path(tempdir(), paste0("reporte_", as.integer(Sys.time()), ".Rmd"))
        file.copy("R/reporte_inteligencia.Rmd", tempReport, overwrite = TRUE)
        
        g_plot <- estabilidad_plot_base()
        textos_list <- estabilidad_textos()
        textos_plain <- if(!is.null(textos_list) && length(textos_list) > 0) sapply(textos_list, function(x) x$plain) else c()
        
        titulo_rasgo <- names(which(c("Rendimiento (% Azúcar)" = "rendimiento", "TCA (Ton/Acre)" = "tca", "TAA (Azúcar/Acre)" = "taa") == input$var_gxe))
        if(length(titulo_rasgo) == 0) titulo_rasgo <- "Desconocido"
        
        variedades_sel <- input$filtros_vars
        if(is.null(variedades_sel) || length(variedades_sel) == 0) {
           variedades_sel <- head(ranking_data()$variedad, input$top_n_clones)
        }
        
        df_plot <- estabilidad_data()
        if(!is.null(df_plot) && nrow(df_plot) > 0) {
           variedades_y_testigos <- unique(c(variedades_sel, df_plot %>% filter(ind_testigo == "S") %>% pull(variedad)))
           
           df_resumen <- df_plot %>% 
             filter(variedad %in% variedades_y_testigos) %>%
             group_by(Variedad = variedad, Suelo = suelo) %>%
             summarise(
               Cortes = length(unique(corte_nombre)),
               Tipo = ifelse(any(ind_testigo == "S"), "Testigo", "Candidata"),
               Promedio = round(mean(valor_medio, na.rm=TRUE), 2),
               Promedio_Absoluto = round(mean(valor_absoluto, na.rm=TRUE), 2),
               .groups="drop"
             ) %>%
             arrange(Suelo, desc(Promedio))
        } else {
           df_resumen <- data.frame(Mensaje = "No hay datos")
        }
        
        params_list <- list(
          rasgo = titulo_rasgo,
          variedades = variedades_sel,
          suelos = input$filtros_suelo,
          df_plot = df_plot,
          interpretacion = textos_plain,
          dat_tabla = df_resumen,
          usar_relativo = isTRUE(input$usar_relativo)
        )
        
        out <- rmarkdown::render(
          input = tempReport,
          params = params_list,
          envir = new.env(parent = globalenv())
        )
        file.rename(out, file)
      }
    )
    
    # --------------------------------------------------------------------------
    # 3. MATRIZ DE CORRELACIONES (TRADE-OFFS)
    # --------------------------------------------------------------------------
    
    # Renderizar filtro de año según fuente
    output$ui_ano_cor <- renderUI({
      if (input$fuente_cor == "EA") {
        if (!DBI::dbExistsTable(con, "ensayos_avanzados")) return(NULL)
        ea <- dbReadTable(con, "ensayos_avanzados")
        anos <- sort(unique(ea$ano_zafra[!is.na(ea$ano_zafra)]), decreasing = TRUE)
      } else {
        if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
        fam <- dbReadTable(con, "familias_evf")
        anos <- sort(unique(fam$anio[!is.na(fam$anio)]), decreasing = TRUE)
      }
      
      if (length(anos) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("ano_cor"), "Serie / Año:",
                  choices = anos, selected = anos, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} años seleccionados"
                  ))
    })
    
    # Renderizar filtro de suelo según fuente
    output$ui_suelo_cor <- renderUI({
      if (input$fuente_cor == "EA") {
        if (!DBI::dbExistsTable(con, "ensayos_avanzados")) return(NULL)
        ea <- dbReadTable(con, "ensayos_avanzados")
        suelos <- unique(ea$suelo)
        suelos <- sort(suelos[!is.na(suelos) & suelos != ""])
      } else {
        if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
        fam <- dbReadTable(con, "familias_evf")
        suelos <- c()
        if ("suelo" %in% names(fam) && any(!is.na(fam$suelo))) {
          suelos <- sort(unique(fam$suelo[!is.na(fam$suelo)]))
        } else {
          codigos <- unique(c(substr(grep("^CR\\d{4}", fam$madre, value=TRUE), 5, 5), 
                              substr(grep("^CR\\d{4}", fam$padre, value=TRUE), 5, 5)))
          cod_map <- c("0"="Bueno", "1"="Rocoso", "2"="Mal_Drenado")
          suelos <- sort(unname(cod_map[codigos[codigos %in% names(cod_map)]]))
        }
      }
      
      if (length(suelos) > 0) {
        selectInput(session$ns("suelo_cor"), "Suelo (Ambiente):", choices = c("Todos" = "ALL", suelos), selected = "ALL")
      } else {
        shinyjs::disabled(selectInput(session$ns("suelo_cor"), "Suelo:", choices = c("No Disp." = "NA"), selected = "NA"))
      }
    })
    
    # Renderizar variables según fuente
    output$ui_vars_cor <- renderUI({
      if (input$fuente_cor == "EA") {
        choices_cor <- c("TCA (Biomasa)" = "tca",
                         "Rendimiento (%)" = "rendimiento",
                         "TAA (Azúcar/Acre)" = "taa",
                         "Brix" = "brix",
                         "Fibra" = "fibra",
                         "Pureza" = "pureza",
                         "Sacarosa" = "sacarosa")
        selected_cor <- c("tca", "rendimiento", "taa", "brix", "fibra")
      } else {
        choices_cor <- c("TCA (Ton/Acre)" = "tca",
                         "Rendimiento (%)" = "rend",
                         "TSA (Azúcar/Acre)" = "tsa")
        selected_cor <- c("tca", "rend", "tsa")
      }
      
      checkboxGroupInput(session$ns("vars_cor"), "Variables a correlacionar:",
                         choices = choices_cor, selected = selected_cor)
    })
    
    output$plot_correlacion <- plotly::renderPlotly({
      req(input$vars_cor, input$fuente_cor)
      if (length(input$vars_cor) < 2) {
        return(plotly::plot_ly() %>% layout(title = "Seleccione al menos 2 variables"))
      }
      
      if (input$fuente_cor == "EA") {
        if (!DBI::dbExistsTable(con, "ensayos_avanzados")) return(NULL)
        df_base <- dbReadTable(con, "ensayos_avanzados")
        
        if (!is.null(input$ano_cor) && length(input$ano_cor) > 0) {
          df_base <- df_base %>% filter(ano_zafra %in% input$ano_cor)
        }
        
        if (!is.null(input$suelo_cor) && input$suelo_cor != "ALL" && input$suelo_cor != "NA" && "suelo" %in% names(df_base)) {
          df_base <- df_base %>% filter(suelo == input$suelo_cor)
        }
      } else {
        if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
        df_base <- dbReadTable(con, "familias_evf")
        
        if (!is.null(input$ano_cor) && length(input$ano_cor) > 0) {
          df_base <- df_base %>% filter(anio %in% input$ano_cor)
        }
        
        if (!is.null(input$suelo_cor) && input$suelo_cor != "ALL" && input$suelo_cor != "NA") {
          if (!"suelo" %in% names(df_base) || all(is.na(df_base$suelo))) {
            df_base <- df_base %>% mutate(
              suelo_inferido = case_when(
                grepl("^CR\\d{4}", madre) ~ substr(madre, 5, 5),
                grepl("^CR\\d{4}", padre) ~ substr(padre, 5, 5),
                TRUE ~ NA_character_
              ),
              suelo = case_when(
                suelo_inferido == "0" ~ "Bueno",
                suelo_inferido == "1" ~ "Rocoso",
                suelo_inferido == "2" ~ "Mal_Drenado",
                TRUE ~ "Global"
              )
            )
          }
          df_base <- df_base %>% filter(suelo == input$suelo_cor)
        }
      }
      
      # Filtrar solo columnas seleccionadas y que existan en la tabla
      cols_validas <- intersect(input$vars_cor, names(df_base))
      if (length(cols_validas) < 2) {
        return(plotly::plot_ly() %>% layout(title = "Datos insuficientes para correlacionar en este ambiente"))
      }
      
      # Convertir a numérico por seguridad y agrupar por variedad para balancear pesos
      if ("variedad" %in% names(df_base)) {
        df_num <- df_base %>%
          select(variedad, all_of(cols_validas)) %>%
          group_by(variedad) %>%
          summarise(across(everything(), ~mean(as.numeric(as.character(.)), na.rm = TRUE)), .groups = "drop") %>%
          select(-variedad)
      } else {
        df_num <- df_base %>% select(all_of(cols_validas)) %>% mutate_all(~as.numeric(as.character(.)))
      }
      
      # Remover filas con demasiados NAs
      df_num <- df_num %>% filter(rowSums(!is.na(.)) >= length(cols_validas) - 1)
      
      # Calcular matriz de correlación
      mat_cor <- cor(df_num, use = "pairwise.complete.obs")
      mat_cor[is.na(mat_cor)] <- 0
      
      # Bloquear la diagonal para que no distorsione la escala de color (Mejora #7)
      diag(mat_cor) <- NA
      
      # Nombres para mostrar
      nombres_display <- c(
        "tca" = "TCA",
        "rendimiento" = "Rend",
        "rend" = "Rend",
        "taa" = "TAA",
        "tsa" = "TAA",
        "brix" = "Brix",
        "fibra" = "Fibra",
        "pureza" = "Pureza",
        "sacarosa" = "Sacarosa"
      )
      
      colnames(mat_cor) <- ifelse(colnames(mat_cor) %in% names(nombres_display), nombres_display[colnames(mat_cor)], colnames(mat_cor))
      rownames(mat_cor) <- colnames(mat_cor)
      
      # Matriz de texto para tooltip y visualización
      text_mat <- round(mat_cor, 2)
      
      plotly::plot_ly(
        x = colnames(mat_cor),
        y = rownames(mat_cor),
        z = mat_cor,
        type = "heatmap",
        colorscale = list(
          c(0, "#d73027"),
          c(0.5, "#ffffff"),
          c(1, "#1a9850")
        ),
        zmin = -1, zmax = 1,
        text = text_mat,
        texttemplate = "%{text}",
        hoverinfo = "text",
        hovertext = paste0("<b>Var X:</b> ", rep(colnames(mat_cor), each=nrow(mat_cor)), "<br>",
                           "<b>Var Y:</b> ", rep(rownames(mat_cor), times=ncol(mat_cor)), "<br>",
                           "<b>Correlación:</b> ", as.vector(text_mat)),
        xgap = 2, ygap = 2
      ) %>%
      plotly::layout(
        title = "Correlación Genética (Pearson)",
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(b = 50, l = 50),
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "#ffffff"
      ) %>%
      plotly::config(toImageButtonOptions = list(format = "svg", filename = "correlacion_tradeoffs"))
    })
    
    output$texto_correlacion <- renderUI({
      req(input$vars_cor, input$fuente_cor)
      
      if (input$fuente_cor == "EA") {
        if (!DBI::dbExistsTable(con, "ensayos_avanzados")) return(NULL)
        df_base <- dbReadTable(con, "ensayos_avanzados")
        if (!is.null(input$ano_cor) && length(input$ano_cor) > 0) df_base <- df_base %>% filter(ano_zafra %in% input$ano_cor)
        if (!is.null(input$suelo_cor) && input$suelo_cor != "ALL" && input$suelo_cor != "NA" && "suelo" %in% names(df_base)) {
          df_base <- df_base %>% filter(suelo == input$suelo_cor)
        }
      } else {
        if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
        df_base <- dbReadTable(con, "familias_evf")
        if (!is.null(input$ano_cor) && length(input$ano_cor) > 0) df_base <- df_base %>% filter(anio %in% input$ano_cor)
      }
      
      cols_validas <- intersect(input$vars_cor, names(df_base))
      if (length(cols_validas) < 2) return(NULL)
      
      if ("variedad" %in% names(df_base)) {
        df_num <- df_base %>%
          select(variedad, all_of(cols_validas)) %>%
          group_by(variedad) %>%
          summarise(across(everything(), ~mean(as.numeric(as.character(.)), na.rm = TRUE)), .groups = "drop") %>%
          select(-variedad)
      } else {
        df_num <- df_base %>% select(all_of(cols_validas)) %>% mutate_all(~as.numeric(as.character(.)))
      }
      df_num <- df_num %>% filter(rowSums(!is.na(.)) >= length(cols_validas) - 1)
      mat_cor <- cor(df_num, use = "pairwise.complete.obs")
      
      # Encontrar el peor trade-off (menor a -0.3)
      mat_lower <- mat_cor
      mat_lower[upper.tri(mat_lower, diag=TRUE)] <- NA
      min_val <- min(mat_lower, na.rm=TRUE)
      max_val <- max(mat_lower, na.rm=TRUE)
      
      nombres_display <- c("tca" = "TCA", "rendimiento" = "Rendimiento", "rend" = "Rendimiento", "taa" = "TAA", "tsh" = "TAA", "tsa" = "TAA", "brix" = "Brix", "fibra" = "Fibra", "pureza" = "Pureza", "sacarosa" = "Sacarosa")
      
      txt_tradeoff <- ""
      if (min_val < -0.4) {
         idx <- which(mat_lower == min_val, arr.ind=TRUE)[1,]
         v1 <- nombres_display[rownames(mat_lower)[idx[1]]]
         v2 <- nombres_display[colnames(mat_lower)[idx[2]]]
         if (min_val >= -0.5 && ((v1 == "TCA" && v2 == "Rendimiento") || (v2 == "TCA" && v1 == "Rendimiento") || (v1 == "TCA" && v2 == "Rend") || (v2 == "TCA" && v1 == "Rend"))) {
            txt_tradeoff <- paste0("Existe un trade-off natural entre <b>", v1, "</b> y <b>", v2, "</b> (r = ", round(min_val, 2), "). Este rango es históricamente normal en caña de azúcar.")
         } else {
            txt_tradeoff <- paste0("Existe una penalización genética severa (trade-off) entre <b>", v1, "</b> y <b>", v2, "</b> (r = ", round(min_val, 2), "). Se requiere cautela para no afectar un rasgo al seleccionar por el otro.")
         }
      } else if (min_val < -0.2) {
         idx <- which(mat_lower == min_val, arr.ind=TRUE)[1,]
         v1 <- nombres_display[rownames(mat_lower)[idx[1]]]
         v2 <- nombres_display[colnames(mat_lower)[idx[2]]]
         txt_tradeoff <- paste0("Existe un leve trade-off entre <b>", v1, "</b> y <b>", v2, "</b> (r = ", round(min_val, 2), "). Se sugiere utilizar índices de selección balanceados (Smith-Hazel).")
      }
      
      txt_sinergia <- ""
      if (max_val > 0.6) {
         idx <- which(mat_lower == max_val, arr.ind=TRUE)[1,]
         v1 <- nombres_display[rownames(mat_lower)[idx[1]]]
         v2 <- nombres_display[colnames(mat_lower)[idx[2]]]
         # Evitar la sinergia obvia TCA-TAA o Rend-TAA
         if (!(v1 %in% c("TCA", "Rendimiento") && v2 == "TAA") && !(v2 %in% c("TCA", "Rendimiento") && v1 == "TAA")) {
           txt_sinergia <- paste0(" Por otro lado, existe una fuerte sinergia genética entre <b>", v1, "</b> y <b>", v2, "</b> (r = ", round(max_val, 2), "), lo que facilita su mejora simultánea.")
         }
      }
      
      if (txt_tradeoff == "" && txt_sinergia == "") {
         txt_final <- "No se detectan penalizaciones genéticas severas ni sinergias fuertes entre las variables seleccionadas en este ambiente."
         icon_type <- "check-circle"
         color_class <- "text-success"
      } else {
         txt_final <- paste0(txt_tradeoff, txt_sinergia)
         icon_type <- if(min_val < -0.4) "exclamation-triangle" else "info-circle"
         color_class <- if(min_val < -0.4) "text-danger" else "text-warning"
      }
      
      div(class = "alert alert-secondary mt-3 shadow-sm", style = "border-left: 5px solid #6c757d;",
          tags$h6(icon(icon_type), " Interpretación Agronómica (Trade-offs):", class=color_class, style="font-weight:bold;"),
          tags$p(class = "mb-0", HTML(txt_final))
      )
    })
    
    # --------------------------------------------------------------------------
    # 4. MEGA-AMBIENTES (GGE BIPLOT)
    # --------------------------------------------------------------------------
    output$ui_filtro_ano_gge <- renderUI({
      ea <- ea_adjusted()
      if (is.null(ea)) ea <- ea_data()
      if (is.null(ea)) return(NULL)
      anos <- sort(unique(ea$ano_zafra[!is.na(ea$ano_zafra)]), decreasing = TRUE)
      if (length(anos) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("filtro_ano_gge"), "Años de Zafra:",
                  choices = anos, selected = anos, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} años seleccionados"
                  ))
    })
    
    output$ui_filtros_exp_gge <- renderUI({
      ea <- ea_adjusted()
      if (is.null(ea)) ea <- ea_data()
      if (is.null(ea)) return(NULL)
      if (!"num_experimento" %in% names(ea)) return(NULL)
      
      exps <- sort(unique(ea$num_experimento[!is.na(ea$num_experimento)]))
      if (length(exps) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("filtros_exp_gge"), "Filtrar Experimentos (opcional):",
                  choices = exps, selected = exps, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} exps seleccionados"
                  ))
    })
    
    output$ui_filtros_vars_gge <- renderUI({
      ea <- ea_adjusted()
      if (is.null(ea)) ea <- ea_data()
      if (is.null(ea)) return(NULL)
      if (!"ind_testigo" %in% names(ea)) ea$ind_testigo <- "N"
      
      vars <- sort(unique(ea$variedad[!is.na(ea$variedad) & ea$ind_testigo != "S"]))
      
      shinyWidgets::pickerInput(session$ns("filtros_vars_gge"), "Seleccionar Variedades para Análisis:",
                  choices = vars, selected = NULL, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    title = "Seleccionar..."
                  ))
    })
    
    gge_data <- reactive({
      # Usar medias ajustadas para GGE (reduce ruido de bloques en el PCA)
      ea <- ea_adjusted()
      if (is.null(ea) || nrow(ea) == 0) {
        ea <- ea_data()
        if (is.null(ea)) return(NULL)
      }
      
      if (!"suelo" %in% names(ea)) {
         cat_suelo <- dbReadTable(con, "categorias") %>% select(variedad, suelo = adapt)
         ea <- ea %>% left_join(cat_suelo, by = "variedad")
      }
      if (!"num_experimento" %in% names(ea)) ea$num_experimento <- "ALL"
      if (!"ind_testigo" %in% names(ea)) ea$ind_testigo <- "N"
      
      # Normalizar nombres de columna TAA: tsh/tsa → taa
      if (!"taa" %in% names(ea)) {
        if ("tsh" %in% names(ea)) ea <- dplyr::rename(ea, taa = tsh)
        else if ("tsa" %in% names(ea)) ea <- dplyr::rename(ea, taa = tsa)
      }
      
      req(ranking_data())
      rasgo <- input$var_gge
      
      gxe <- ea
      if (!is.null(input$filtro_ano_gge) && length(input$filtro_ano_gge) > 0) gxe <- gxe %>% filter(ano_zafra %in% input$filtro_ano_gge)
      if (!is.null(input$filtros_exp_gge) && length(input$filtros_exp_gge) > 0) gxe <- gxe %>% filter(num_experimento %in% input$filtros_exp_gge)
      
      top_vars <- head(ranking_data()$variedad, input$top_n_gge)
      vars_extra <- if(!is.null(input$filtros_vars_gge)) input$filtros_vars_gge else character(0)
      
      gxe_resumen <- gxe %>%
        filter(variedad %in% top_vars | ind_testigo == "S" | variedad %in% vars_extra) %>%
        filter(!is.na(suelo) & !is.na(!!sym(rasgo))) %>%
        group_by(variedad, suelo) %>%
        summarise(valor_medio = mean(!!sym(rasgo), na.rm = TRUE), .groups = "drop")
      
      if (nrow(gxe_resumen) < 3) return(NULL)
      
      mat_original <- gxe_resumen %>% pivot_wider(names_from = suelo, values_from = valor_medio) %>% tibble::column_to_rownames("variedad")
      
      if (nrow(mat_original) < 5 || ncol(mat_original) < 3) return(NULL)
      
      mat <- mat_original
      for (i in 1:ncol(mat)) mat[is.na(mat[,i]), i] <- mean(mat[,i], na.rm = TRUE)
      
      pca <- tryCatch(prcomp(mat, center = TRUE, scale. = TRUE), error = function(e) NULL)
      if (is.null(pca)) return(NULL)
      
      df_scores <- as.data.frame(pca$x)
      if (!"PC2" %in% names(df_scores)) df_scores$PC2 <- 0
      df_scores$variedad <- rownames(df_scores)
      testigos_db <- unique(ea$variedad[ea$ind_testigo == "S"])
      df_scores$es_testigo <- df_scores$variedad %in% testigos_db
      
      df_loadings <- as.data.frame(pca$rotation)
      if (!"PC2" %in% names(df_loadings)) df_loadings$PC2 <- 0
      df_loadings$ambiente <- rownames(df_loadings)
      
      max_score <- max(abs(df_scores$PC1), abs(df_scores$PC2), na.rm=TRUE)
      max_load <- max(abs(df_loadings$PC1), abs(df_loadings$PC2), na.rm=TRUE)
      mult_factor <- if(is.finite(max_load) && max_load > 0) (max_score / max_load) * 0.8 else 1
      df_loadings$PC1 <- df_loadings$PC1 * mult_factor
      df_loadings$PC2 <- df_loadings$PC2 * mult_factor
      
      var_exp <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
      if (length(var_exp) < 2) var_exp <- c(100, 0)
      
      # Calcular Convex Hull
      hull_idx <- chull(df_scores$PC1, df_scores$PC2)
      hull_idx <- c(hull_idx, hull_idx[1])
      df_hull <- df_scores[hull_idx, ]
      
      list(scores = df_scores, loadings = df_loadings, var_exp = var_exp, hull = df_hull, rasgo = rasgo, mat_original = mat_original)
    })
    
    output$plot_gge <- plotly::renderPlotly({
      dat <- gge_data()
      if (is.null(dat)) return(plotly::plot_ly() %>% layout(title = "Datos insuficientes o muy singulares para GGE"))
      
      plotly::plot_ly() %>%
        # 0. Convex Hull (Polígono)
        plotly::add_polygons(
          data = dat$hull, x = ~PC1, y = ~PC2, 
          fillcolor = "rgba(230, 159, 0, 0.1)", line = list(color = "#E69F00", width = 1, dash = "dash"),
          hoverinfo = "none", name = "Polígono GGE"
        ) %>%
        # 1. Puntos (Variedades)
        plotly::add_trace(
          data = dat$scores, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers+text",
          text = ~variedad,
          textposition = "top center",
          textfont = list(color = ~ifelse(es_testigo, "#d55e00", "#56b4e9"), size = 11, family = "Arial", weight = "bold"),
          marker = list(
            size = ~ifelse(es_testigo, 12, 9),
            color = ~ifelse(es_testigo, "#d55e00", "#56b4e9"),
            symbol = ~ifelse(es_testigo, "square", "circle"),
            line = list(color = "white", width = 1)
          ),
          hoverinfo = "text",
          hovertext = ~paste0("<b>", variedad, "</b>", ifelse(es_testigo, " (TESTIGO)", ""), "<br>PC1: ", round(PC1, 2), "<br>PC2: ", round(PC2, 2)),
          name = "Clones"
        ) %>%
        # 2. Vectores (Ambientes)
        plotly::add_segments(
          data = dat$loadings, x = 0, y = 0, xend = ~PC1, yend = ~PC2,
          line = list(color = "#009e73", width = 2),
          hoverinfo = "none", name = "Ambientes"
        ) %>%
        # 3. Textos de Ambientes
        plotly::add_trace(
          data = dat$loadings, x = ~PC1 * 1.15, y = ~PC2 * 1.15, type = "scatter", mode = "text",
          text = ~ambiente, textfont = list(color = "#009e73", size = 13, family = "Arial Black"),
          hoverinfo = "text",
          hovertext = ~paste("<b>Ambiente:</b>", ambiente),
          name = "Ambientes (Etiquetas)"
        ) %>%
        plotly::layout(
          title = paste("GGE Biplot -", names(which(c("Rendimiento (% Azúcar)" = "rendimiento", "TCA (Ton/Acre)" = "tca", "TAA (Azúcar/Acre)" = "taa") == dat$rasgo))),
          xaxis = list(title = paste0("Componente Principal 1 (", dat$var_exp[1], "%)"), zeroline = TRUE, zerolinewidth=1, zerolinecolor="#cccccc"),
          yaxis = list(title = paste0("Componente Principal 2 (", dat$var_exp[2], "%)"), zeroline = TRUE, zerolinewidth=1, zerolinecolor="#cccccc"),
          showlegend = FALSE, hovermode = "closest", margin = list(t = 50),
          plot_bgcolor = "#f8f9fa", paper_bgcolor = "#ffffff"
        ) %>%
        plotly::config(toImageButtonOptions = list(format = "svg", filename = "mega_ambientes_gge"))
    })
    
    output$texto_gge <- renderUI({
      dat <- gge_data()
      if (is.null(dat)) return(NULL)
      
      vars_sel <- input$filtros_vars_gge
      if (is.null(vars_sel) || length(vars_sel) == 0) {
         return(div(class = "alert alert-secondary mt-3 shadow-sm", style = "border-left: 5px solid #6c757d;",
            tags$h6(icon("lightbulb"), " Seleccione una variedad en el panel lateral para ver su análisis de Mega-Ambientes.")
         ))
      }
      
      html_items <- lapply(vars_sel, function(v) {
         if (!v %in% dat$scores$variedad) return(tags$li(paste("La variedad", v, "no tiene datos suficientes para este gráfico.")))
         
         # Extraer vector del clon
         clon_pc1 <- dat$scores$PC1[dat$scores$variedad == v]
         clon_pc2 <- dat$scores$PC2[dat$scores$variedad == v]
         clon_dist <- sqrt(clon_pc1^2 + clon_pc2^2)
         
         # Calcular ángulo del clon
         clon_ang <- atan2(clon_pc2, clon_pc1)
         
         # Calcular ángulo de los ambientes y encontrar el más cercano
         dat$loadings$ang <- atan2(dat$loadings$PC2, dat$loadings$PC1)
         # Distancia angular
         dat$loadings$dist_ang <- abs(dat$loadings$ang - clon_ang)
         # Corregir por envoltura circular
         dat$loadings$dist_ang <- ifelse(dat$loadings$dist_ang > pi, 2*pi - dat$loadings$dist_ang, dat$loadings$dist_ang)
         
         mejor_amb <- dat$loadings$ambiente[which.min(dat$loadings$dist_ang)]
         peor_amb <- dat$loadings$ambiente[which.max(dat$loadings$dist_ang)]
         
         distancia_max <- max(sqrt(dat$scores$PC1^2 + dat$scores$PC2^2))
         rel_dist <- clon_dist / distancia_max
         
         # Detectar si hay falta de datos empíricos
         falta_peor <- is.na(dat$mat_original[v, peor_amb])
         txt_falta_peor <- if(falta_peor) paste0(" <i>(Nota: El modelo deduce esta penalización matemáticamente por su adaptación diametralmente opuesta, careciendo de datos empíricos en ", peor_amb, ")</i>.") else "."
         
         # Determinar si es testigo
         es_testigo <- dat$scores$es_testigo[dat$scores$variedad == v]
         
         # Interpretación de distancia al origen (estabilidad)
         if (es_testigo) {
            if (rel_dist < 0.2) {
               txt_est <- "Como <b>testigo de control</b>, esta variedad marca el estándar de estabilidad estática, situándose en el centro del polígono con un comportamiento predecible en todos los ambientes."
            } else {
               txt_est <- paste0("Como <b>testigo de control</b>, esta variedad define el estándar de adaptación específica hacia <b>", mejor_amb, "</b>.")
            }
         } else {
            if (rel_dist < 0.2) {
               txt_est <- "Se encuentra muy cerca del origen, indicando que posee una alta <b>estabilidad biológica</b> (baja interacción GxE). Su rendimiento es consistente a través de todos los ambientes."
            } else if (rel_dist > 0.8) {
               txt_est <- paste0("Es un material con alta interacción genotipo-ambiente (altamente reactivo), posicionado en el vértice del polígono. Muestra un fuerte <b>potencial de rendimiento</b> y adaptación específica hacia <b>", mejor_amb, "</b>.")
            } else {
               txt_est <- paste0("Muestra una interacción genotipo-ambiente moderada, con adaptación fisiológica orientada hacia <b>", mejor_amb, "</b>.")
            }
         }
         
         tags$li(HTML(paste0("<b>", v, ":</b> ", txt_est, " Su desempeño tiende a penalizarse si se expone a <b>", peor_amb, "</b>", txt_falta_peor)))
      })
      
      div(class = "alert alert-success mt-3 shadow-sm", style = "border-left: 5px solid #28a745;",
          tags$h6(icon("compass"), " Análisis de Mega-Ambientes (Dominancia):", class="text-success", style="font-weight:bold;"),
          tags$ul(class = "mb-0", do.call(tagList, html_items))
      )
    })
    # --------------------------------------------------------------------------
    # 5. EVALUACIÓN DE FAMILIAS (Campana de Gauss)
    # --------------------------------------------------------------------------
    output$ui_fam_ano <- renderUI({
      if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
      fam <- dbReadTable(con, "familias_evf")
      anos <- sort(unique(fam$anio[!is.na(fam$anio)]), decreasing = TRUE)
      if (length(anos) == 0) return(NULL)
      shinyWidgets::pickerInput(session$ns("fam_ano"), "Serie / Año:",
                  choices = anos, selected = anos, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 2",
                    `count-selected-text` = "{0} años seleccionados"
                  ))
    })
    
    # Extraemos la lista de cruces basada en el año seleccionado
    cruces_list <- reactive({
      req(input$fam_ano)
      if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
      fam <- dbReadTable(con, "familias_evf")
      fam_sub <- fam %>% filter(anio == input$fam_ano)
      fam_sub <- fam %>% filter(anio %in% input$fam_ano)
      sort(unique(fam_sub$cruce[!is.na(fam_sub$cruce)]))
    })
    
    output$ui_fam_suelo <- renderUI({
      if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
      fam <- dbReadTable(con, "familias_evf")
      
      suelos <- c()
      if ("suelo" %in% names(fam) && any(!is.na(fam$suelo))) {
        suelos <- sort(unique(fam$suelo[!is.na(fam$suelo)]))
        suelos <- suelos[suelos != "Rocoso"]
      } else {
        codigos <- unique(c(substr(grep("^CR\\d{4}", fam$madre, value=TRUE), 5, 5), 
                            substr(grep("^CR\\d{4}", fam$padre, value=TRUE), 5, 5)))
        cod_map <- c("0"="Bueno", "2"="Mal_Drenado")
        suelos <- unname(cod_map[codigos[codigos %in% names(cod_map)]])
      }
      
      if (length(suelos) > 0) {
        selectInput(session$ns("fam_suelo"), "Suelo (Ambiente):", choices = c("Todos" = "ALL", suelos), selected = "ALL")
      } else {
        shinyjs::disabled(selectInput(session$ns("fam_suelo"), "Suelo (Ambiente):", choices = c("No Disp. en DB" = "NA"), selected = "NA"))
      }
    })
    
    # Cálculos principales del Ranking
    madres_data <- reactive({
      req(input$fam_ano, input$prog_var)
      if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
      
      fam <- dbReadTable(con, "familias_evf")
      
      # Generar programa si no existe
      if (!"programa" %in% names(fam) || all(is.na(fam$programa))) {
        fam <- fam %>% mutate(
          programa = case_when(
            grepl("^CR", madre) | grepl("^CR", padre) ~ "CR",
            grepl("^B", madre) | grepl("^B", padre) ~ "BR",
            TRUE ~ "OTRO"
          )
        )
      }
      
      # Extraer suelo desde el nombre de la variedad si no existe o es todo NA
      if (!"suelo" %in% names(fam) || all(is.na(fam$suelo))) {
        # Ejemplo: CR202001 -> '2' es Mal_Drenado
        # Extraemos el 5to caracter de la madre o padre para inferir el suelo si empiezan con CR
        fam <- fam %>% mutate(
          suelo_inferido = case_when(
            grepl("^CR\\d{4}", madre) ~ substr(madre, 5, 5),
            grepl("^CR\\d{4}", padre) ~ substr(padre, 5, 5),
            TRUE ~ NA_character_
          ),
          suelo = case_when(
            suelo_inferido == "0" ~ "Bueno",
            suelo_inferido == "1" ~ "Rocoso",
            suelo_inferido == "2" ~ "Mal_Drenado",
            TRUE ~ "Global"
          )
        )
      }
      
      df_ano <- fam %>% 
        filter(anio %in% input$fam_ano) %>%
        filter(!is.na(tca) | !is.na(rend) | !is.na(tsa))
      
      if (!is.null(input$fam_suelo) && input$fam_suelo != "ALL" && input$fam_suelo != "NA" && "suelo" %in% names(df_ano)) {
        df_ano <- df_ano %>% filter(suelo == input$fam_suelo)
      }
      
      # Limpiar nombres
      df_ano <- df_ano %>% mutate(
        madre = trimws(madre),
        padre = trimws(padre)
      )
      
      df_m <- df_ano %>% select(progenitor = madre, tca, rend, tsa) %>% filter(!is.na(progenitor) & progenitor != "")
      
      K_SHRINKAGE <- 5
      res <- df_m %>%
        group_by(progenitor) %>%
        summarise(
          n_cruces = n(),
          prom_tca = round(mean(tca, na.rm=TRUE), 2),
          prom_rend = round(mean(rend, na.rm=TRUE), 2),
          prom_tsa = round(mean(tsa, na.rm=TRUE), 2),
          .groups = "drop"
        ) %>%
        mutate(
          gca_tca = round((prom_tca - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2),
          gca_rend = round((prom_rend - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2),
          gca_tsa = round((prom_tsa - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2)
        )
      
      var_sort <- if(length(input$prog_var) > 0) input$prog_var[1] else "tca"
      res %>% arrange(desc(!!sym(paste0("gca_", var_sort))))
    })
    
    padres_data <- reactive({
      req(input$fam_ano, input$prog_var)
      if (!DBI::dbExistsTable(con, "familias_evf")) return(NULL)
      fam <- dbReadTable(con, "familias_evf")
      
      df_ano <- fam %>% filter(anio %in% input$fam_ano) %>% filter(!is.na(tca) | !is.na(rend) | !is.na(tsa))
      
      if (!is.null(input$fam_suelo) && input$fam_suelo != "ALL" && input$fam_suelo != "NA" && "suelo" %in% names(df_ano)) {
        df_ano <- df_ano %>% filter(suelo == input$fam_suelo)
      }
      df_ano <- df_ano %>% mutate(padre = trimws(padre))
      df_p <- df_ano %>% select(progenitor = padre, tca, rend, tsa) %>% filter(!is.na(progenitor) & progenitor != "")
      
      K_SHRINKAGE <- 5
      res <- df_p %>%
        group_by(progenitor) %>%
        summarise(
          n_cruces = n(),
          prom_tca = round(mean(tca, na.rm=TRUE), 2),
          prom_rend = round(mean(rend, na.rm=TRUE), 2),
          prom_tsa = round(mean(tsa, na.rm=TRUE), 2),
          .groups = "drop"
        ) %>%
        mutate(
          gca_tca = round((prom_tca - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2),
          gca_rend = round((prom_rend - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2),
          gca_tsa = round((prom_tsa - 100) * (n_cruces / (n_cruces + K_SHRINKAGE)), 2)
        )
      
      var_sort <- if(length(input$prog_var) > 0) input$prog_var[1] else "tca"
      res %>% arrange(desc(!!sym(paste0("gca_", var_sort))))
    })
    
    # Gráfico de Barras de Madres
    output$plot_madres <- plotly::renderPlotly({
      dat <- madres_data()
      if (is.null(dat) || nrow(dat) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar"))
      
      dat <- head(dat, input$prog_topn)
      dat <- dat[nrow(dat):1, ]
      dat$progenitor <- factor(dat$progenitor, levels = dat$progenitor)
      
      var_sort <- if(length(input$prog_var) > 0) input$prog_var[1] else "tca"
      var_names <- c("tca" = "TCA", "rend" = "Rend", "tsa" = "TAA")
      
      col_name <- paste0("gca_", var_sort)
      vals <- dat[[col_name]]
      min_v <- min(vals, na.rm=TRUE)
      max_v <- max(vals, na.rm=TRUE)
      
      plotly::plot_ly() %>% plotly::add_bars(
        data = dat, y = ~progenitor, x = as.formula(paste0("~", col_name)),
        name = paste("GCA", var_names[var_sort]),
        marker = list(
           color = as.formula(paste0("~", col_name)),
           colorscale = "RdYlGn", cmin = min_v, cmax = max_v,
           line = list(color = "#333333", width = 0.5)
        ),
        orientation = 'h', text = ~paste("Cruces:", n_cruces), hoverinfo = "x+text+name"
      ) %>% plotly::layout(
        title = paste("Top Madres Élite - Habilidad Combinatoria (", var_names[var_sort], ")"),
        xaxis = list(title = paste("Ventaja Genética (GCA", var_names[var_sort], ")")),
        yaxis = list(title = ""), margin = list(l = 100)
      ) %>% plotly::config(displayModeBar = F)
    })
    
    # Gráfico de Barras de Padres
    output$plot_padres <- plotly::renderPlotly({
      dat <- padres_data()
      if (is.null(dat) || nrow(dat) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar"))
      
      dat <- head(dat, input$prog_topn)
      dat <- dat[nrow(dat):1, ]
      dat$progenitor <- factor(dat$progenitor, levels = dat$progenitor)
      
      var_sort <- if(length(input$prog_var) > 0) input$prog_var[1] else "tca"
      var_names <- c("tca" = "TCA", "rend" = "Rend", "tsa" = "TAA")
      
      col_name <- paste0("gca_", var_sort)
      vals <- dat[[col_name]]
      min_v <- min(vals, na.rm=TRUE)
      max_v <- max(vals, na.rm=TRUE)
      
      plotly::plot_ly() %>% plotly::add_bars(
        data = dat, y = ~progenitor, x = as.formula(paste0("~", col_name)),
        name = paste("GCA", var_names[var_sort]),
        marker = list(
           color = as.formula(paste0("~", col_name)),
           colorscale = "RdYlGn", cmin = min_v, cmax = max_v,
           line = list(color = "#333333", width = 0.5)
        ),
        orientation = 'h', text = ~paste("Cruces:", n_cruces), hoverinfo = "x+text+name"
      ) %>% plotly::layout(
        title = paste("Top Padres Élite - Habilidad Combinatoria (", var_names[var_sort], ")"),
        xaxis = list(title = paste("Ventaja Genética (GCA", var_names[var_sort], ")")),
        yaxis = list(title = ""), margin = list(l = 100)
      ) %>% plotly::config(displayModeBar = F)
    })
    
    output$dt_madres <- DT::renderDT({
      dat <- madres_data()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      dat_display <- dat %>% select(Progenitor = progenitor, Cruces = n_cruces, `GCA (TCA)` = gca_tca, `GCA (Rend)` = gca_rend, `GCA (TSA)` = gca_tsa)
      DT::datatable(dat_display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, class = "display compact") %>%
        DT::formatStyle(c("GCA (TCA)", "GCA (Rend)", "GCA (TSA)"),
                        color = DT::styleInterval(c(-1, 1), c('#d73027', '#333333', '#1a9850')),
                        fontWeight = "bold", backgroundColor = "#f8f9fa")
    })
    
    output$dt_padres <- DT::renderDT({
      dat <- padres_data()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      dat_display <- dat %>% select(Progenitor = progenitor, Cruces = n_cruces, `GCA (TCA)` = gca_tca, `GCA (Rend)` = gca_rend, `GCA (TSA)` = gca_tsa)
      DT::datatable(dat_display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, class = "display compact") %>%
        DT::formatStyle(c("GCA (TCA)", "GCA (Rend)", "GCA (TSA)"),
                        color = DT::styleInterval(c(-1, 1), c('#d73027', '#333333', '#1a9850')),
                        fontWeight = "bold", backgroundColor = "#f8f9fa")
    })
    
    # Sugerencia de Cruces Ideales (Tarjetas Élite)
    output$ui_cruces_elite <- renderUI({
      req(input$fam_ano, input$prog_var, input$prog_topn)
      
      dat_m <- head(madres_data(), input$prog_topn)
      dat_p <- head(padres_data(), input$prog_topn)
      if (is.null(dat_m) || nrow(dat_m) == 0 || is.null(dat_p) || nrow(dat_p) == 0) return(NULL)
      
      var_sort <- if(length(input$prog_var) > 0) input$prog_var[1] else "tca"
      col_val <- paste0("gca_", var_sort)
      var_names <- c("tca" = "TCA", "rend" = "Rend", "tsa" = "TAA")
      
      cruces <- expand.grid(Madre = dat_m$progenitor, Padre = dat_p$progenitor, stringsAsFactors = FALSE) %>% filter(Madre != Padre)
      
      if (DBI::dbExistsTable(con, "pedigree_maestro")) {
        pm <- dbReadTable(con, "pedigree_maestro") %>% select(variedad, madre, padre)
        cruces <- cruces %>%
          left_join(pm %>% rename(m_madre = madre, m_padre = padre), by = c("Madre" = "variedad")) %>%
          left_join(pm %>% rename(p_madre = madre, p_padre = padre), by = c("Padre" = "variedad")) %>%
          mutate(
            riesgo_inbreeding = case_when(
              Madre == p_madre | Madre == p_padre ~ "Alto",
              Padre == m_madre | Padre == m_padre ~ "Alto",
              (!is.na(m_madre) & m_madre != "" & (m_madre == p_madre | m_madre == p_padre)) |
              (!is.na(m_padre) & m_padre != "" & (m_padre == p_madre | m_padre == p_padre)) ~ "Alto",
              TRUE ~ "Bajo"
            )
          ) %>%
          select(-m_madre, -m_padre, -p_madre, -p_padre)
      } else {
        cruces$riesgo_inbreeding <- "Desconocido"
      }
      
      cruces_display <- cruces %>%
        left_join(dat_m %>% select(progenitor, gca_m = !!sym(col_val)), by = c("Madre" = "progenitor")) %>%
        left_join(dat_p %>% select(progenitor, gca_p = !!sym(col_val)), by = c("Padre" = "progenitor")) %>%
        mutate(
          gca_esperado = round((gca_m + gca_p)/2, 2)
        ) %>% arrange(desc(gca_esperado))
        
      # Mostrar top 12 cruces
      top_cruces <- head(cruces_display, 12)
      
      cards <- lapply(1:nrow(top_cruces), function(i) {
         cruce <- top_cruces[i,]
         color_badge <- if(cruce$gca_esperado > 1.5) "bg-success" else if(cruce$gca_esperado > 0) "bg-primary" else "bg-secondary"
         inb_badge <- if(cruce$riesgo_inbreeding == "Alto") "<span class='badge bg-danger ms-1'>Inbreeding</span>" else ""
         
         tags$div(class = "col-md-4 col-lg-3 mb-4",
           tags$div(class = "card h-100 shadow-sm border-0", style = "border-radius: 12px; overflow: hidden; background: linear-gradient(145deg, #ffffff, #f0f4f8);",
             tags$div(class = paste("card-header text-white text-center", color_badge), style = "font-weight: bold; font-size: 1.1em;", 
                      paste("GCA Promedio:", cruce$gca_esperado)),
             tags$div(class = "card-body text-center",
                tags$h5(class = "card-title mb-3", HTML(paste(cruce$Madre, "<i class='fa fa-times text-muted mx-1'></i>", cruce$Padre, inb_badge))),
                tags$div(class = "d-flex justify-content-around mb-2",
                   tags$div(tags$small(class="text-muted", "♀ GCA"), tags$br(), tags$span(class="badge bg-light text-dark border", style="font-size: 1em;", cruce$gca_m)),
                   tags$div(tags$small(class="text-muted", "♂ GCA"), tags$br(), tags$span(class="badge bg-light text-dark border", style="font-size: 1em;", cruce$gca_p))
                )
             )
           )
         )
      })
      
      tags$div(class = "row mt-3", cards)
    })
    
    # ==========================================================================
    # --- TAB 6: ANÁLISIS DE ISOPRODUCTIVIDAD (Curvas Iso) ---
    # ==========================================================================
    
    output$badge_lsmeans_iso <- renderUI({
      if (DBI::dbExistsTable(con, "ensayos_ajustados")) {
         return(div(class="alert alert-success p-2 text-center", style="font-size:0.9em; margin-bottom: 15px;", 
                icon("bolt"), tags$strong(" Datos DBCA: Medias Ajustadas (LSMeans)")))
      }
      return(NULL)
    })
    
    output$ui_iso_ano <- renderUI({
      dat <- ea_adjusted()
      if (is.null(dat)) dat <- ea_data()
      if (is.null(dat)) return(NULL)
      
      col_anio <- if ("ano_zafra" %in% names(dat)) "ano_zafra" else "anio"
      if (!(col_anio %in% names(dat))) return(NULL)
      
      anios <- sort(unique(dat[[col_anio]]), decreasing = TRUE)
      shinyWidgets::pickerInput(session$ns("iso_ano"), "Años de Zafra:", choices = anios, selected = anios, multiple = TRUE,
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "{0} años seleccionados"))
    })
    
    output$ui_iso_suelo <- renderUI({
      dat <- ea_adjusted()
      if (is.null(dat)) dat <- ea_data()
      if (is.null(dat)) return(NULL)
      
      col_anio <- if ("ano_zafra" %in% names(dat)) "ano_zafra" else "anio"
      if (col_anio %in% names(dat) && length(input$iso_ano) > 0) {
        dat <- dat[dat[[col_anio]] %in% input$iso_ano, ]
      }
      
      col_env <- if ("suelo" %in% names(dat)) "suelo" else if ("ambiente" %in% names(dat)) "ambiente" else NULL
      if (is.null(col_env)) return(NULL)
      
      suelos <- sort(unique(dat[[col_env]][!is.na(dat[[col_env]]) & dat[[col_env]] != "" & tolower(dat[[col_env]]) != "others"]))
      shinyWidgets::checkboxGroupButtons(session$ns("iso_suelo"), "Suelos a Analizar:", choices = suelos, selected = suelos, status = "outline-success", size = "sm", direction = "vertical", width = "100%")
    })
    
    output$ui_iso_exp <- renderUI({
      dat <- ea_adjusted()
      if (is.null(dat)) dat <- ea_data()
      if (is.null(dat)) return(NULL)
      if (!"num_experimento" %in% names(dat)) return(NULL)
      
      col_anio <- if ("ano_zafra" %in% names(dat)) "ano_zafra" else "anio"
      if (col_anio %in% names(dat) && length(input$iso_ano) > 0) {
        dat <- dat[dat[[col_anio]] %in% input$iso_ano, ]
      }
      exps <- sort(unique(dat$num_experimento[!is.na(dat$num_experimento)]))
      if (length(exps) == 0) return(NULL)
      
      shinyWidgets::pickerInput(session$ns("iso_exp"), "Filtrar Experimentos (opcional):", choices = exps, selected = exps, multiple = TRUE, options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2"))
    })
    
    output$ui_iso_vars <- renderUI({
      dat <- ea_adjusted()
      if (is.null(dat)) dat <- ea_data()
      if (is.null(dat)) return(NULL)
      if (!"ind_testigo" %in% names(dat)) dat$ind_testigo <- "N"
      
      variedades <- sort(unique(dat$variedad[!is.na(dat$variedad) & dat$ind_testigo != "S"]))
      shinyWidgets::pickerInput(session$ns("iso_vars"), "Comparar Variedades vs Testigos:", choices = variedades, selected = NULL, multiple = TRUE,
                                options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Seleccionar..."))
    })
    
    iso_data <- reactive({
      req(input$iso_ano)
      dat <- ea_adjusted()
      if (is.null(dat)) dat <- ea_data()
      if (is.null(dat)) return(NULL)
      if (!"ind_testigo" %in% names(dat)) dat$ind_testigo <- "N"
      
      col_anio <- if ("ano_zafra" %in% names(dat)) "ano_zafra" else "anio"
      if (col_anio %in% names(dat)) {
        dat <- dat[dat[[col_anio]] %in% input$iso_ano, ]
      }
      
      col_env <- if ("suelo" %in% names(dat)) "suelo" else if ("ambiente" %in% names(dat)) "ambiente" else NULL
      if (!is.null(col_env) && !is.null(input$iso_suelo)) {
        dat <- dat[dat[[col_env]] %in% input$iso_suelo, ]
      }
      
      if ("num_experimento" %in% names(dat) && !is.null(input$iso_exp)) {
        dat <- dat[dat$num_experimento %in% input$iso_exp, ]
      }
      
      tca_col <- if ("tca" %in% names(dat)) "tca" else if ("tch" %in% names(dat)) "tch" else NULL
      rend_col <- if ("rendimiento" %in% names(dat)) "rendimiento" else if ("rend" %in% names(dat)) "rend" else if ("sacarosa" %in% names(dat)) "sacarosa" else if ("sac" %in% names(dat)) "sac" else NULL
      tsa_col <- if ("taa" %in% names(dat)) "taa" else if ("tsa" %in% names(dat)) "tsa" else if ("tsh" %in% names(dat)) "tsh" else NULL
      
      if (is.null(tca_col) || is.null(rend_col)) return(NULL)
      
      res <- dat %>%
        group_by(variedad, ind_testigo) %>%
        summarise(
          TCA = mean(!!sym(tca_col), na.rm = TRUE),
          Rendimiento = mean(!!sym(rend_col), na.rm = TRUE),
          TSH = if (!is.null(tsa_col)) mean(!!sym(tsa_col), na.rm = TRUE) else mean(!!sym(tca_col)*!!sym(rend_col)/100, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(Variedad = variedad) %>% filter(!is.na(TCA) & !is.na(Rendimiento))
        
      res
    })
    
    output$plot_iso <- renderPlot({
      dat <- iso_data()
      if (is.null(dat) || nrow(dat) == 0) return(ggplot() + geom_text(aes(x=0,y=0,label="Sin datos para los filtros seleccionados")) + theme_void())
      
      # Si hay filtro de variedades manual, se usan esas + los testigos. Si no, las top N + testigos
      testigos <- dat$Variedad[dat$ind_testigo == "S"]
      if(!is.null(input$iso_vars) && length(input$iso_vars) > 0) {
         vars_sel <- unique(c(input$iso_vars, testigos))
         dat_plot <- dat %>% filter(Variedad %in% vars_sel)
      } else {
         n_top <- if(!is.null(input$iso_top_n)) input$iso_top_n else 20
         vars_top <- dat %>% filter(ind_testigo != "S") %>% arrange(desc(TSH)) %>% head(n_top) %>% pull(Variedad)
         vars_sel <- unique(c(vars_top, testigos))
         dat_plot <- dat %>% filter(Variedad %in% vars_sel)
      }
      
      if(nrow(dat_plot) == 0) return(ggplot() + geom_text(aes(x=0,y=0,label="Variedades no encontradas")) + theme_void())
      
      y_t <- "TCA (Ton/Acre)"
      y2_t <- "TAA (Ton/Acre)"
      
      create_iso_plot(
        media_df = dat_plot,
        testigos = testigos,
        selected_gradient = input$iso_gradient,
        plot_title = "Curvas Isoproductividad TAA",
        y_title = y_t,
        y2_title = y2_t,
        use_filled_contour = TRUE
      )
    })
    
    # Manejador del Reporte Iso
    output$reporte_iso <- downloadHandler(
      filename = function() {
        anio_str <- paste(input$iso_ano, collapse="-")
        paste0("Reporte_Isoproductividad_", anio_str, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        id <- showNotification("Generando reporte de Isoproductividad... Esto puede tomar unos segundos.", 
                               type = "message", duration = NULL)
        on.exit(removeNotification(id), add = TRUE)
        
        rmd_file <- "R/reporte_isoproductividad.Rmd"
        if (!file.exists(rmd_file)) {
          showNotification(paste("No se encontró el archivo:", rmd_file), type = "error")
          return(NULL)
        }
        
        dat <- iso_data()
        
        testigos <- dat$Variedad[dat$ind_testigo == "S"]
        if(!is.null(input$iso_vars) && length(input$iso_vars) > 0) {
           vars_sel <- unique(c(input$iso_vars, testigos))
           dat_rep <- dat %>% filter(Variedad %in% vars_sel)
        } else {
           n_top <- if(!is.null(input$iso_top_n)) input$iso_top_n else 20
           vars_top <- dat %>% filter(ind_testigo != "S") %>% arrange(desc(TSH)) %>% head(n_top) %>% pull(Variedad)
           vars_sel <- unique(c(vars_top, testigos))
           dat_rep <- dat %>% filter(Variedad %in% vars_sel)
        }
        
        params_list <- list(
          iso_data = dat_rep,
          vars_sel = vars_sel,
          gradient = input$iso_gradient,
          anio     = input$iso_ano,
          suelo    = input$iso_suelo
        )
        
        temp_dir <- tempdir()
        temp_rmd <- file.path(temp_dir, basename(rmd_file))
        file.copy(rmd_file, temp_rmd, overwrite = TRUE)
        
        out <- rmarkdown::render(
          input       = temp_rmd,
          params      = params_list,
          envir       = new.env(parent = globalenv()),
          quiet       = TRUE
        )
        file.rename(out, file)
      }
    )

    
  })
}
