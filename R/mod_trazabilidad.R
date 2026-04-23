# ============================================================
# MOD_TRAZABILIDAD.R - Modulo Shiny: Trazabilidad de Clones
# ============================================================

# --- UI del Modulo ---
mod_trazabilidad_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = TRUE,
        title = "Explorador de Trazabilidad Genetica",
        icon = icon("history"),
        column(8, 
               selectizeInput(ns("clon_search"), "ID del Clon / Variedad:", 
                              choices = NULL, 
                              options = list(placeholder = "Ej: 630-1, CR261001...", 
                                             maxOptions = 50))),
        column(4, 
               actionButton(ns("btn_trace"), "Rastrear Historia", 
                            class = "btn-primary btn-block", 
                            style = "margin-top: 25px;",
                            icon = icon("search-location")),
               uiOutput(ns("download_btn_ui"))
        )
      )
    ),
    uiOutput(ns("timeline_ui"))
  )
}

# --- Server del Modulo ---
mod_trazabilidad_server <- function(id, cat_var, pedigree_var, df_categorias) {
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
          # Si buscamos por nombre de variedad CR, necesitamos mapear al clon primero (ya lo hicimos arriba)
          q <- sprintf(
            "SELECT ? as etapa, anio_seleccion AS anio, suelo, brix, vigor FROM %s WHERE cruce = ?",
            table_name
          )
          res <- tryCatch(
            dbGetQuery(con, q, params = list(toupper(st), "N/A")),
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
        fluidRow(
          box(
            width = 4, title = "Origen (Pedigri)", status = "info", solidHeader = TRUE,
            if(nrow(d$parents) > 0) {
              tags$ul(
                lapply(1:nrow(d$parents), function(i) {
                  tags$li(tags$b(d$parents$tipo_ancestro[i], ": "), d$parents$id_variedad_ancestro[i])
                })
              )
            } else {
              p("No se registran padres en la base de datos.")
            }
          ),
          box(
            width = 8, title = "Estatus Actual", status = "success", solidHeader = TRUE,
            if(nrow(d$current) > 0) {
              fluidRow(
                valueBox(d$current$categoria, "Categoria", icon = icon("dna"), width = 6, color = "green"),
                valueBox(d$current$evf_info, "Exito EVF", icon = icon("vial"), width = 6, color = "blue")
              )
            } else {
              p("Variedad no activa en el ciclo actual.")
            }
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Progreso Real en el Pipeline de Selección", status = "warning", solidHeader = TRUE,
            if(nrow(d$hist_sel) > 0) {
              fluidRow(
                column(7, plotOutput(ns("plot_history_sel"), height = "350px")),
                column(5, 
                       h5(tags$b("Detalle por Etapa y Suelo")),
                       DT::DTOutput(ns("table_history_sel")))
              )
            } else {
              p("No hay registros de avance real en la base de datos para este clon.")
            }
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Hijos (Cruces donde es progenitor)", status = "danger", solidHeader = TRUE,
            if(nrow(d$hijos) > 0) {
              DT::DTOutput(ns("table_hijos"))
            } else {
              p("No se registran hijos para este clon.")
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
    
    output$plot_history_rend <- renderPlot({
      req(trace_data()$hist_rend)
      df <- trace_data()$hist_rend
      if (nrow(df) == 0) return(NULL)
      
      df_long <- df %>%
        select(anio, tca, rend, taa) %>%
        pivot_longer(cols = -anio, names_to = "Metrica", values_to = "Valor")
      
      ggplot(df_long, aes(x = anio, y = Valor, color = Metrica, group = Metrica)) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        facet_wrap(~Metrica, scales = "free_y") +
        theme_minimal() +
        scale_color_brewer(palette = "Set1")
    })
    
    output$table_history_rend <- DT::renderDT({
      req(trace_data()$hist_rend)
      DT::datatable(trace_data()$hist_rend %>% select(anio, localidad, tca, rend, taa),
                    options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
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
