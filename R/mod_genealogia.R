# ==============================================================================
# MOD_GENEALOGIA.R — Módulo Shiny: Visor de Genealogía (v2.0 visNetwork)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- UI del Módulo ---
mod_genealogia_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .bslib-value-box .value-box-grid { padding: 8px !important; }
      .card-header { padding: 4px 10px !important; font-size: 0.85rem !important; font-weight: bold !important; }
      .card-body { padding: 8px !important; }
      #estado_var-txt_notas { font-size: 0.9rem; }
    ")),
    layout_columns(
      col_widths = c(4, 8),
      # Panel de búsqueda
      card(
        card_header(tagList(icon("search"), " Buscador de Variedades")),
        full_screen = TRUE,
        selectizeInput(
          ns("var_search"), "Variedad o ID:",
          choices = NULL,  # Se puebla en server
          options = list(
            maxOptions = 50,
            placeholder = "Busca una variedad..."
          )
        ),
        actionButton(ns("btn_plot"), "Visualizar Árbol",
                     class = "btn-success btn-sm w-100",
                     icon = icon("project-diagram")),
        uiOutput(ns("info_variedad"))
      ),
      
      # Panel del gráfico interactivo
      card(
        card_header(tagList(icon("project-diagram"), " Árbol Genealógico Interactivo")),
        full_screen = TRUE,
        div(style = "padding-bottom: 20px;",
            visNetworkOutput(ns("plot_pedigree_vis"), height = "1200px")
        )
      ),
      
      # Panel de la tabla de detalles
      card(
        card_header(tagList(icon("table"), " Detalle de Ancestros")),
        div(style = "padding: 10px;",
            DT::DTOutput(ns("tabla_pedigree"))
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_genealogia_server <- function(id, con, ebvs_var = NULL, df_categorias = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Leer la tabla unificada y limpia desde la base de datos
    ped_maestro <- reactive({
      tryCatch({
        dbGetQuery(con, "SELECT variedad, madre, padre, ano, suelo FROM pedigree_maestro WHERE variedad IS NOT NULL AND variedad != ''")
      }, error = function(e) data.frame(variedad=character()))
    })
    
    # Poblar selectize con las variedades del CSV maestro (no el catálogo DB)
    observe({
      pm <- ped_maestro()
      if (nrow(pm) == 0) return()
      
      # Variedades que tienen al menos padre o madre registrada son las más interesantes
      # Pero mostramos TODAS para que se pueda buscar cualquiera
      opciones <- sort(unique(pm$variedad))
      opciones <- opciones[nchar(opciones) > 0 & opciones != "NA"]
      updateSelectizeInput(session, "var_search", choices = opciones, server = TRUE)
    })
    
    # --- Datos reactivos ---
    ped_reactivo <- eventReactive(input$btn_plot, {
      req(input$var_search)
      pm <- ped_maestro()
      shiny::validate(need(nrow(pm) > 0, "No se pudo cargar el archivo de pedigrí maestro."))
      
      target_name <- trimws(input$var_search)
      
      # Verificar que existe en el maestro
      shiny::validate(
        need(target_name %in% pm$variedad, 
             paste0("'", target_name, "' no encontrada en el maestro de pedigree."))
      )
      
      # Convertir CSV formato ancho (variedad, madre, padre) 
      # al formato largo que espera get_full_ancestry_robust:
      # (id_variedad, id_variedad_ancestro, tipo_ancestro)
      ped_largo <- bind_rows(
        pm %>%
          filter(nchar(madre) > 0, madre != "NA", !is.na(madre)) %>%
          transmute(
            id_variedad          = variedad,
            id_variedad_ancestro = madre,
            tipo_ancestro        = "MADRE"
          ),
        pm %>%
          filter(nchar(padre) > 0, padre != "NA", !is.na(padre)) %>%
          transmute(
            id_variedad          = variedad,
            id_variedad_ancestro = padre,
            tipo_ancestro        = "PADRE"
          )
      )
      
      # Construir catálogo mínimo (id = nombre para que las etiquetas sean los nombres)
      cat_minimal <- data.frame(
        id_variedad          = unique(c(pm$variedad, pm$madre, pm$padre)),
        descripcion_variedad = unique(c(pm$variedad, pm$madre, pm$padre)),
        stringsAsFactors     = FALSE
      ) %>%
        filter(nchar(id_variedad) > 0, id_variedad != "NA", !is.na(id_variedad))
      
      # BFS recursivo de ancestría
      ped_data <- get_full_ancestry_robust(ped_largo, target_name, cat_minimal)
      
      # EBVs si aplica
      ebv_info <- NULL
      if (!is.null(ebvs_var) && nrow(ebvs_var) > 0) {
        ebv_info <- ebvs_var %>% filter(variedad == target_name) %>% slice(1)
      }
      
      # Info del maestro para la variedad objetivo
      info_var <- pm %>% filter(variedad == target_name) %>% slice(1)
      
      # 1. Buscar en EBV (ensayos_avanzados) directamente en la BD
      ebv_info_direct <- tryCatch({
        dbGetQuery(con, "SELECT COUNT(*) as total_obs, AVG(tca) as ebv_tca, AVG(rendimiento) as ebv_rend, AVG(pureza) as ebv_pureza, AVG(tsh) as ebv_tsh FROM ensayos_avanzados WHERE variedad = ?", params = list(target_name))
      }, error = function(e) NULL)
      
      if (!is.null(ebv_info_direct) && nrow(ebv_info_direct) > 0 && !is.na(ebv_info_direct$total_obs) && ebv_info_direct$total_obs > 0) {
        ebv_info_direct$confianza <- "Alta"
        ebv_info <- ebv_info_direct
      }
      
      # 2. Buscar en categorias (Cédula principal)
      cat_info <- tryCatch({
        res <- dbGetQuery(con, "SELECT * FROM categorias WHERE variedad = ?", params = list(target_name))
        if (nrow(res) > 0) res else NULL
      }, error = function(e) NULL)
      
      # 3. Buscar en ST4 y ST5 si no hay nada en EBV y Categorías
      st_info <- NULL
      if (is.null(ebv_info) && is.null(cat_info)) {
        st_data <- tryCatch({
          res4 <- dbGetQuery(con, "SELECT brix, vigor, suelo, 'ST4' as etapa FROM clones_st4 WHERE cruce = ? OR cruce || '-' || num_sel = ?", params=list(target_name, target_name))
          res5 <- dbGetQuery(con, "SELECT brix, vigor, suelo, 'ST5' as etapa FROM clones_st5 WHERE cruce = ? OR cruce || '-' || num_sel = ?", params=list(target_name, target_name))
          rbind(res4, res5)
        }, error = function(e) NULL)
        if (!is.null(st_data) && nrow(st_data) > 0) {
          st_info <- st_data %>% arrange(desc(etapa)) %>% slice(1)
        }
      }
      
      list(
        ped_data  = ped_data,
        target_id = target_name,
        nombre    = target_name,
        ebv_info  = ebv_info,
        cat_info  = cat_info,
        st_info   = st_info,
        info_var  = info_var
      )
    })
    
    # --- Info rápida ---
    output$info_variedad <- renderUI({
      req(ped_reactivo())
      datos <- ped_reactivo()
      ped <- datos$ped_data
      
      n_reales <- nrow(ped %>% filter(!grepl("^UNK_", id)))
      n_unk <- nrow(ped %>% filter(grepl("^UNK_", id)))
      
      # Construir tarjeta de EBVs si hay datos
      ebv_html <- ""
      if (!is.null(datos$ebv_info) && nrow(datos$ebv_info) > 0) {
        e <- datos$ebv_info
        
        # Helper para formatear (verde si positivo, rojo si negativo)
        fmt_val <- function(val, suffix="") {
          if (is.na(val)) return(span("N/A", style="color:#95a5a6;"))
          color <- ifelse(val >= 0, "#2ecc71", "#e74c3c")
          signo <- ifelse(val > 0, "+", "")
          span(paste0(signo, round(val, 2), suffix), style=paste0("color:", color, "; font-weight:bold;"))
        }
        
        ebv_html <- tags$div(
          style = "margin-top: 15px; padding-top: 10px; border-top: 1px dashed #bdc3c7;",
          tags$h5(icon("chart-line"), " Desempeño vs Testigo (EBV)", style = "color:#2c3e50; font-weight:bold;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 5px; font-size: 0.9em;",
            tags$div(tags$b("Rendimiento:"), fmt_val(e$ebv_rend)),
            tags$div(tags$b("TCA:"), fmt_val(e$ebv_tca)),
            tags$div(tags$b("Pureza:"), fmt_val(e$ebv_pureza, "%")),
            tags$div(tags$b("TSH:"), fmt_val(e$ebv_tsh))
          ),
          tags$div(
            style = "margin-top: 5px; font-size: 0.8em; color: #7f8c8d;",
            paste("Observaciones:", e$total_obs, "| Confiabilidad:", e$confianza)
          )
        )
      } else if (!is.null(datos$cat_info) && nrow(datos$cat_info) > 0) {
        c_i <- datos$cat_info
        
        # Helper para formatear básico
        fmt_val_cat <- function(val) {
          if (is.na(val)) return(span("N/A", style="color:#95a5a6;"))
          span(round(val, 2), style="color:#2c3e50; font-weight:bold;")
        }
        
        ebv_html <- tags$div(
          style = "margin-top: 15px; padding-top: 10px; border-top: 1px dashed #bdc3c7;",
          tags$h5(icon("seedling"), " Perfil de Cédula (Categorías)", style = "color:#27ae60; font-weight:bold;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 5px; font-size: 0.9em;",
            tags$div(tags$b("Factor:"), fmt_val_cat(c_i$factor)),
            tags$div(tags$b("Y (Rend):"), fmt_val_cat(c_i$y)),
            tags$div(tags$b("Q (Calidad):"), fmt_val_cat(c_i$q)),
            tags$div(tags$b("Sanidad:"), fmt_val_cat(c_i$disease))
          )
        )
      } else if (!is.null(datos$st_info) && nrow(datos$st_info) > 0) {
        s_i <- datos$st_info
        
        ebv_html <- tags$div(
          style = "margin-top: 15px; padding-top: 10px; border-top: 1px dashed #bdc3c7;",
          tags$h5(icon("flask"), paste(" Datos de Selección (", s_i$etapa, ")"), style = "color:#d35400; font-weight:bold;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 5px; font-size: 0.9em;",
            tags$div(tags$b("Brix:"), if(!is.na(s_i$brix)) span(round(s_i$brix, 2), style="color:#2c3e50; font-weight:bold;") else "N/A"),
            tags$div(tags$b("Vigor:"), if(!is.na(s_i$vigor)) span(s_i$vigor, style="color:#2c3e50; font-weight:bold;") else "N/A"),
            tags$div(tags$b("Suelo:"), if(!is.na(s_i$suelo)) span(s_i$suelo, style="color:#2c3e50; font-weight:bold;") else "N/A")
          )
        )
      } else {
         ebv_html <- tags$div(
          style = "margin-top: 15px; padding-top: 10px; border-top: 1px dashed #bdc3c7;",
          tags$small(icon("info-circle"), " Sin datos de evaluación registrados.", style = "color: #95a5a6;")
         )
      }
      
      iv <- if (!is.null(datos$info_var) && nrow(datos$info_var) > 0) datos$info_var else NULL
      
      tags$div(
        style = "background: #f8f9fa; padding: 10px; border-radius: 8px; border-left: 5px solid #27ae60;",
        tags$h4(datos$nombre, style = "margin: 0 0 8px 0; font-weight: 800; color: #1b4332;"),
        if (!is.null(iv)) tagList(
          tags$p(tags$b("Madre: "),
                 if (nchar(trimws(iv$madre)) > 0) iv$madre else tags$em("Desconocida"),
                 style = "margin-bottom:2px; font-size: 0.9rem;"),
          tags$p(tags$b("Padre: "),
                 if (nchar(trimws(iv$padre)) > 0) iv$padre else tags$em("Desconocido"),
                 style = "margin-bottom:2px; font-size: 0.9rem;"),
          if (!is.na(iv$ano) && nchar(as.character(iv$ano)) > 0)
            tags$p(tags$b("Año de cruce: "), iv$ano, style = "margin-bottom:2px; font-size: 0.9rem;"),
          if (!is.na(iv$suelo) && nchar(as.character(iv$suelo)) > 0)
            tags$p(tags$b("Suelo: "), iv$suelo, style = "margin-bottom:2px; font-size: 0.9rem;")
        ),
        tags$p(tags$b("Ancestros en árbol: "), n_reales, style="margin-bottom:2px; font-size: 0.9rem;"),
        ebv_html
      )
    })
    
    # --- Renderizado con visNetwork ---
    output$plot_pedigree_vis <- renderVisNetwork({
      req(ped_reactivo())
      
      datos <- ped_reactivo()
      df <- datos$ped_data
      target_id <- datos$target_id
      
      # 1. Crear NODOS
      nodes <- df %>%
        mutate(
          id = id,
          label = label,
          title = paste0("ID: ", id, "<br>Padre: ", dadid, "<br>Madre: ", momid),
          group = case_when(
            id == target_id ~ "Objetivo",
            grepl("^UNK_", id) ~ "Desconocido",
            sex == 1 ~ "Macho",
            sex == 2 ~ "Hembra",
            TRUE ~ "Ancestro"
          ),
          shape = case_when(
            group == "Objetivo" ~ "star",
            group == "Macho" ~ "square",
            group == "Hembra" ~ "dot",
            TRUE ~ "diamond"
          ),
          color = case_when(
            group == "Objetivo" ~ "#2ecc71",
            group == "Macho" ~ "#3498db",
            group == "Hembra" ~ "#e91e63",
            group == "Desconocido" ~ "#bdc3c7",
            TRUE ~ "#34495e"
          )
        )
      
      # 2. Crear EDGES (Aristas: de Padre/Madre a Hijo)
      edges_dad <- df %>%
        filter(!is.na(dadid) & dadid != "0") %>%
        select(from = dadid, to = id)
      
      edges_mom <- df %>%
        filter(!is.na(momid) & momid != "0") %>%
        select(from = momid, to = id)
      
      edges <- bind_rows(edges_dad, edges_mom)
      
      # 3. Construir Red
      visNetwork(nodes, edges, main = paste("Genealogía de", datos$nombre)) %>%
        visNodes(font = list(size = 18, face = "Arial")) %>%
        visEdges(arrows = "to", color = list(color = "#7f8c8d", highlight = "#2c3e50")) %>%
        visGroups(groupname = "Macho", color = "#3498db", shape = "square") %>%
        visGroups(groupname = "Hembra", color = "#e91e63", shape = "dot") %>%
        visGroups(groupname = "Objetivo", color = "#2ecc71", shape = "star") %>%
        visGroups(groupname = "Desconocido", color = "#bdc3c7", shape = "diamond") %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   nodesIdSelection = TRUE) %>%
        visHierarchicalLayout(direction = "UD", sortMethod = "directed", levelSeparation = 150) %>%
        visPhysics(hierarchicalRepulsion = list(nodeDistance = 200)) %>%
        visInteraction(navigationButtons = TRUE, zoomView = TRUE) %>%
        visExport(type = "png", name = paste0("Genealogia_", input$var_search), 
                  label = "Descargar PNG", style = "background-color: #0d6efd; color: white; border: none; padding: 5px 10px; border-radius: 4px; font-weight: bold; cursor: pointer;")
    })
    
    # --- Tabla de detalle ---
    output$tabla_pedigree <- DT::renderDT({
      req(ped_reactivo())
      datos <- ped_reactivo()
      
      ped <- datos$ped_data %>%
        filter(!grepl("^UNK_", id)) %>%
        mutate(
          Sexo = ifelse(sex == 1, "Macho (♂)", "Hembra (♀)"),
          Rol = case_when(
            id == datos$target_id ~ "OBJETIVO",
            id %in% datos$ped_data$dadid ~ "PADRE",
            id %in% datos$ped_data$momid ~ "MADRE",
            TRUE ~ "ANCESTRO"
          )
        ) %>%
        select(ID = id, Nombre = label, Padre = dadid, Madre = momid, Sexo, Rol)
      
      ped$Padre <- ifelse(grepl("^UNK_", ped$Padre), "---", ped$Padre)
      ped$Madre <- ifelse(grepl("^UNK_", ped$Madre), "---", ped$Madre)
      
      DT::datatable(ped,
                    options = list(pageLength = 8, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "none",
                    caption = "Ancestros registrados en la base de datos")
    })
    
    return(ped_reactivo)
  })
}
