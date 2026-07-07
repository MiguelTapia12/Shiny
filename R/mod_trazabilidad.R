# ============================================================
# MOD_TRAZABILIDAD.R - Modulo Shiny: Trazabilidad de Clones
# ============================================================

# --- UI del Modulo ---
mod_trazabilidad_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(tagList(icon("history"), tags$span(`data-i18n`="trz_title", " Explorador de Trazabilidad Genética"))),
      layout_column_wrap(
        width = 1,
        layout_column_wrap(
          width = 1/2,
          selectizeInput(ns("clon_search"), tags$span(`data-i18n`="lbl_clone_id", "ID del Clon / Variedad:"), 
                         choices = NULL, 
                         options = list(placeholder = "Ej: 630-1, CR261001...", 
                                        maxOptions = 100,
                                        searchField = c("value", "label"))),
          tags$div(
            class = "d-flex flex-column gap-2",
            actionButton(ns("btn_trace"), tags$span(`data-i18n`="btn_trace_history", "Rastrear Historia"), 
                         class = "btn-primary w-100", 
                         icon = icon("search-location")),
            uiOutput(ns("download_btn_ui"))
          )
        )
      )
    ),
    uiOutput(ns("timeline_ui"))
  )
}

# --- Server del Modulo ---
mod_trazabilidad_server <- function(id, cat_var, pedigree_var, df_categorias, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Poblar busqueda con Variedades + Clones de la BD
    observe({
      # 1. Variedades del catalogo e historico
      cat_ids <- if (!is.null(df_categorias())) df_categorias()$variedad else c()
      ped_ids <- if (!is.null(pedigree_var)) pedigree_var$id_variedad else c()
      
      # 2. Clones de la BD real (Estados 1-5)
      clones_ids <- c()
      for (st in c("st1", "st2", "st3", "st4", "st5")) {
        table_name <- validate_stage_table(paste0("clones_", st))
        df_st <- tryCatch(
          dbGetQuery(con, sprintf("SELECT DISTINCT anio_seleccion AS anio, cruce, num_sel FROM %s", table_name)),
          error = function(e) NULL
        )
        if (!is.null(df_st) && nrow(df_st) > 0) {
          # Formato unico: "Año | Cruce-NumSel"
          clones_ids <- c(clones_ids, paste0(df_st$anio, " | ", df_st$cruce, "-", df_st$num_sel))
        }
      }
      
      all_ids <- sort(unique(c(cat_ids, ped_ids, clones_ids)))
      updateSelectizeInput(session, "clon_search", choices = all_ids, server = TRUE)
    })
    
    # --- Datos de Trazabilidad Real e Interconectada ---
    trace_data <- eventReactive(input$btn_trace, {
      req(input$clon_search)
      input_str <- input$clon_search
      
      # Desglosar si viene en formato "Año | Cruce-NumSel"
      search_year <- NULL
      id_target <- input_str
      
      if (grepl(" | ", input_str, fixed = TRUE)) {
        parts <- strsplit(input_str, " | ", fixed = TRUE)[[1]]
        search_year <- as.integer(parts[1])
        id_target <- parts[2]
      }
      
      # 0. ¿Es una variedad CR promocionada? Buscar su clon origen
      promo_info <- dbGetQuery(
        con,
        "SELECT * FROM promociones WHERE nombre_cr = ?",
        params = list(id_target)
      )
      id_query <- if(nrow(promo_info) > 0) promo_info$clon_origen[1] else id_target
      
      # 1. Buscar Padres (Usando Año si está disponible)
      parents <- data.frame()
      if (grepl("-", id_query)) {
        cruce_id <- strsplit(id_query, "-")[[1]][1]
        # Query mas precisa usando el AÑO
        if (!is.null(search_year)) {
          fam_info <- dbGetQuery(
            con,
            "SELECT madre, padre FROM familias_evf WHERE cruce = ? AND anio = ?",
            params = list(cruce_id, search_year)
          )
        } else {
          fam_info <- dbGetQuery(
            con,
            "SELECT madre, padre FROM familias_evf WHERE cruce = ? ORDER BY anio DESC LIMIT 1",
            params = list(cruce_id)
          )
        }
        if (nrow(fam_info) > 0) {
          parents <- data.frame(
            id_variedad_ancestro = c(fam_info$madre[1], fam_info$padre[1]),
            tipo_ancestro = c("Madre", "Padre")
          )
        }
      }
 else {
        # Busqueda estandar en pedigri historico
        parents <- pedigree_var %>% 
          filter(id_variedad == id_target) %>%
          select(id_variedad_ancestro, tipo_ancestro)
      }
      
      # 2. Buscar Datos de Seleccion REAL (Estados 1-5)
      h_sel_real <- data.frame()
      for (st in c("st1", "st2", "st3", "st4", "st5")) {
        table_name <- validate_stage_table(paste0("clones_", st))
        # Desglosar id_query si es clon
        if (grepl("-", id_query)) {
          pts <- strsplit(id_query, "-")[[1]]
          q <- sprintf(
            "SELECT ? as etapa, anio_seleccion AS anio, suelo, brix, vigor FROM %s WHERE cruce = ? AND num_sel = ?",
            table_name
          )
          res <- tryCatch(
            dbGetQuery(con, q, params = list(toupper(st), pts[1], as.integer(pts[2]))),
            error = function(e) NULL
          )
        } else {
          # Búsqueda por nombre directo (ej. variedad testigo o variedad élite importada)
          # Tratamos de buscar donde el campo cruce sea igual al id_query
          q <- sprintf(
            "SELECT ? as etapa, anio_seleccion AS anio, suelo, brix, vigor FROM %s WHERE cruce = ?",
            table_name
          )
          res <- tryCatch(
            dbGetQuery(con, q, params = list(toupper(st), id_query)),
            error = function(e) NULL
          )
        }
        if (!is.null(res) && nrow(res) > 0) h_sel_real <- rbind(h_sel_real, res)
      }
      
      # 3. Buscar Descendencia (Hijos)
      hijos_df <- pedigree_var %>%
        filter(id_variedad_ancestro == id_target)
      
      list(
        id = id_target,
        id_clon = id_query,
        parents = parents,
        hist_sel = h_sel_real,
        hijos = hijos_df,
        current = df_categorias() %>% filter(variedad == id_target) %>% slice(1)
      )
    })
    
    # --- UI Dinamica del Timeline ---
    output$timeline_ui <- renderUI({
      req(trace_data())
      d <- trace_data()
      
      tagList(
      tagList(
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(tagList(icon("dna"), " Origen (Pedigrí)")),
            if(nrow(d$parents) > 0) {
              tags$ul(
                lapply(1:nrow(d$parents), function(i) {
                  tags$li(tags$b(d$parents$tipo_ancestro[i], ": "), d$parents$id_variedad_ancestro[i])
                })
              )
            } else {
              p("No se registran padres en la base de datos.", class = "text-muted")
            }
          ),
          card(
            card_header(tagList(icon("info-circle"), " Estatus Actual")),
            if(nrow(d$current) > 0) {
              layout_column_wrap(
                width = 1/2,
                value_box(
                  title = "Categoría",
                  value = d$current$categoria,
                  showcase = icon("dna"),
                  theme = "success"
                ),
                value_box(
                  title = "Éxito EVF",
                  value = d$current$evf_info,
                  showcase = icon("vial"),
                  theme = "primary"
                )
              )
            } else {
              p("Variedad no activa en el ciclo actual.", class = "text-muted")
            }
          )
        ),
        
        card(
          card_header(tagList(icon("chart-line"), " Progreso Real en el Pipeline de Selección")),
          if(nrow(d$hist_sel) > 0) {
            layout_column_wrap(
              width = 1/2,
              plotOutput(ns("plot_history_sel"), height = "350px"),
              card(
                card_header("Detalle por Etapa"),
                DT::DTOutput(ns("table_history_sel"))
              )
            )
          } else {
            p("No hay registros de avance real en la base de datos para este clon.", class = "text-muted")
          }
        ),
        
        card(
          card_header(tagList(icon("sitemap"), " Hijos (Cruces donde es progenitor)")),
          if(nrow(d$hijos) > 0) {
            DT::DTOutput(ns("table_hijos"))
          } else {
            p("No se registran hijos para este clon.", class = "text-muted")
          }
        )
      )
      )
    })
    
    # --- Graficos con Datos Reales ---
    output$plot_history_sel <- renderPlot({
      req(trace_data()$hist_sel)
      df <- trace_data()$hist_sel %>% arrange(anio)
      if (nrow(df) == 0) return(NULL)
      
      ggplot(df, aes(x = etapa, y = brix, group = 1)) +
        geom_line(linewidth = 1.2, color = "#27ae60") +
        geom_point(aes(size = vigor, color = suelo), alpha = 0.8) +
        geom_text(aes(label = paste0("B:", brix)), vjust = -1.5, fontface = "bold") +
        labs(subtitle = "Evolución de Brix y Vigor por Estado", x = "Estado de Selección", y = "Brix (%)") +
        theme_minimal(base_size = 14) +
        scale_size_continuous(range = c(3, 8))
    })
    
    output$table_history_sel <- DT::renderDT({
      req(trace_data()$hist_sel)
      DT::datatable(trace_data()$hist_sel, options = list(dom = 't'), rownames = FALSE)
    })
    

    
    output$table_hijos <- DT::renderDT({
      req(trace_data()$hijos)
      DT::datatable(trace_data()$hijos %>% select(id_variedad, tipo_ancestro, categoria, factor, disease),
                    options = list(pageLength = 5), rownames = FALSE)
    })
    # --- Boton de Descarga Dinamico ---
    output$download_btn_ui <- renderUI({
      req(trace_data())
      downloadButton(ns("download_trace"), "Descargar Reporte (.xlsx)", 
                     class = "btn-info btn-block", style = "margin-top: 10px;")
    })
    
    output$download_trace <- downloadHandler(
      filename = function() {
        paste("Reporte_Trazabilidad_", trace_data()$id, "_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        d <- trace_data()
        wb <- createWorkbook()
        
        # Hoja 1: Resumen y Pedigri
        addWorksheet(wb, "Resumen_Pedigri")
        writeData(wb, "Resumen_Pedigri", data.frame(Clon = d$id, Categoria = d$current$categoria, Exito_EVF = d$current$evf_info))
        writeData(wb, "Resumen_Pedigri", d$parents, startRow = 5)
        
        # Hoja 2: Historial de Seleccion Real
        if(nrow(d$hist_sel) > 0) {
          addWorksheet(wb, "Historial_Clon")
          writeData(wb, "Historial_Clon", d$hist_sel)
        }
        
        # Hoja 3: Descendencia
        if(nrow(d$hijos) > 0) {
          addWorksheet(wb, "Descendencia_Hijos")
          writeData(wb, "Descendencia_Hijos", d$hijos)
        }
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
