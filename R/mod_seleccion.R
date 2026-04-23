# ==============================================================================
# MOD_SELECCION.R — Módulo Shiny: Seguimiento de Selecciones
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- UI del Módulo ---
mod_seleccion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12, title = "Gestión del Pipeline de Selección",
        status = "primary", solidHeader = TRUE,
        icon = icon("dna"),
        
        tabBox(
          width = 12, title = "Etapas de Evaluación",
          
          # --- EVALUACIÓN DE FAMILIAS ---
          tabPanel("EVF: Familias",
            fluidRow(
              column(4,
                wellPanel(
                  h4("Archivado en Repositorio"),
                  fluidRow(
                    column(6, numericInput(ns("up_evf_year"), "Año:", value = 2025)),
                    column(6, selectInput(ns("up_evf_prog"), "Programa:", choices = c("CR", "BR")))
                  ),
                  fileInput(ns("file_evf"), "Cargar Excel de Familias", accept = ".xlsx")
                )
              ),
              column(8,
                p("Relativización contra testigo y selección de progenies."),
                fluidRow(
                  column(3, numericInput(ns("threshold_tsa"), "Umbral TSA (%):", value = 110)),
                  column(3, numericInput(ns("threshold_q"), "Umbral Q (%):", value = 100)),
                  column(6, style="margin-top: 25px;",
                         actionButton(ns("btn_auto_select"), "Pre-seleccionar", class = "btn-warning"),
                         actionButton(ns("btn_confirm_evf"), "Confirmar S/R", class = "btn-success"))
                ),
                DT::DTOutput(ns("tabla_evf"))
              )
            )
          ),

          # --- ESTADOS INDIVIDUALES ---
          tabPanel("Estado 1 (Plántulas)", 
            fluidRow(
              column(4, 
                wellPanel(
                  h4("Sincronización Est 1"),
                  fluidRow(
                    column(3, numericInput(ns("up_st1_year"), "Año Selección:", 2025)),
                    column(3, numericInput(ns("up_st1_year_cruce"), "Año Cruce:", 2024)),
                    column(3, selectInput(ns("up_st1_prog"), "Prog:", choices = c("CR", "BR"))),
                    column(3, selectInput(ns("up_st1_soil"), "Suelo:", choices = c("GOOD", "CLAY", "ROCKY")))
                  ),
                  fileInput(ns("f1"), "Excel de Campo (Estado 1)", accept=".xlsx"),
                  hr(),
                  p(tags$i("Nota: La selección se toma directamente de la columna 'ACCION' del archivo.")),
                  actionButton(ns("btn_confirm_st1"), "Confirmar y Sincronizar", class = "btn-success btn-block")
                )
              ),
              column(8, 
                h4("Vista Previa de Datos de Campo"),
                DT::DTOutput(ns("t1"))
              )
            )
          ),
          tabPanel("Estado 2 (Vigor/Brix)", 
            fluidRow(
              column(4, 
                wellPanel(
                  h4("Sincronización Est 2"),
                  fluidRow(
                    column(3, numericInput(ns("up_st2_year"), "Año Selección:", 2025)),
                    column(3, numericInput(ns("up_st2_year_cruce"), "Año Cruce:", 2023)),
                    column(3, selectInput(ns("up_st2_prog"), "Prog:", choices = c("CR", "BR"))),
                    column(3, selectInput(ns("up_st2_soil"), "Suelo:", choices = c("GOOD", "CLAY", "ROCKY")))
                  ),
                  fileInput(ns("f2"), "Excel de Campo (Estado 2)", accept=".xlsx"),
                  hr(),
                  actionButton(ns("btn_confirm_st2"), "Confirmar y Sincronizar", class = "btn-success btn-block")
                ),
                wellPanel(
                  h4("Candidatos desde Est 1"),
                  DT::DTOutput(ns("promocionados_est1"))
                )
              ),
              column(8, 
                h4("Vista Previa de Datos de Campo"),
                DT::DTOutput(ns("t2"))
              )
            )
          ),
          tabPanel("Estado 3 (Molino)", 
            fluidRow(
              column(4, 
                wellPanel(
                  h4("Sincronización Est 3"),
                  fluidRow(
                    column(3, numericInput(ns("up_st3_year"), "Año Selección:", 2025)),
                    column(3, numericInput(ns("up_st3_year_cruce"), "Año Cruce:", 2022)),
                    column(3, selectInput(ns("up_st3_prog"), "Prog:", choices = c("CR", "BR"))),
                    column(3, selectInput(ns("up_st3_soil"), "Suelo:", choices = c("GOOD", "CLAY", "ROCKY")))
                  ),
                  fileInput(ns("f3"), "Excel de Campo (Estado 3)", accept=".xlsx"),
                  hr(),
                  actionButton(ns("btn_confirm_st3"), "Confirmar y Sincronizar", class = "btn-success btn-block")
                ),
                wellPanel(
                  h4("Candidatos desde Est 2"),
                  DT::DTOutput(ns("promocionados_est2"))
                )
              ),
              column(8, h4("Vista Previa Est 3"), DT::DTOutput(ns("t3")))
            )
          ),
          tabPanel("Estado 4 (Comercial)", 
            fluidRow(
              column(4, 
                wellPanel(
                  h4("Sincronización Est 4"),
                  fluidRow(
                    column(3, numericInput(ns("up_st4_year"), "Año Selección:", 2025)),
                    column(3, numericInput(ns("up_st4_year_cruce"), "Año Cruce:", 2021)),
                    column(3, selectInput(ns("up_st4_prog"), "Prog:", choices = c("CR", "BR"))),
                    column(3, selectInput(ns("up_st4_soil"), "Suelo:", choices = c("GOOD", "CLAY", "ROCKY")))
                  ),
                  fileInput(ns("f4"), "Excel de Campo (Estado 4)", accept=".xlsx"),
                  hr(),
                  actionButton(ns("btn_confirm_st4"), "Confirmar y Sincronizar", class = "btn-success btn-block")
                ),
                wellPanel(
                  h4("Candidatos desde Est 3"),
                  DT::DTOutput(ns("promocionados_est3"))
                )
              ),
              column(8, h4("Vista Previa Est 4"), DT::DTOutput(ns("t4")))
            )
          ),
          tabPanel("Estado 5 (Variedades)", 
            fluidRow(
              column(4, 
                wellPanel(
                  h4("Sincronización Est 5"),
                  fluidRow(
                    column(3, numericInput(ns("up_st5_year"), "Año Selección:", 2025)),
                    column(3, numericInput(ns("up_st5_year_cruce"), "Año Cruce:", 2020)),
                    column(3, selectInput(ns("up_st5_prog"), "Prog:", choices = c("CR", "BR"))),
                    column(3, selectInput(ns("up_st5_soil"), "Suelo:", choices = c("GOOD", "CLAY", "ROCKY")))
                  ),
                  fileInput(ns("f5"), "Excel de Campo (Estado 5)", accept=".xlsx"),
                  hr(),
                  actionButton(ns("btn_confirm_st5"), "Confirmar y Sincronizar", class = "btn-success btn-block")
                ),
                wellPanel(
                  h4("Candidatos desde Est 4"),
                  DT::DTOutput(ns("promocionados_est4"))
                )
              ),
              column(8, h4("Vista Previa Est 5"), DT::DTOutput(ns("t5")))
            )
          ),
          
          # --- PROMOCIÓN ---
          tabPanel("Promoción a Variedad",
            fluidRow(
              column(4,
                wellPanel(
                  h4("Registrar Nueva Variedad (Desde Est 3+)"),
                  selectizeInput(ns("clon_to_promote"), "Seleccionar Clon (Aprobado Est 3):", choices = NULL),
                  selectInput(ns("suelo_code"), "Tipo de Suelo:",
                              choices = c("Suelo Bueno" = "0", "Rocoso" = "1000", "Mal Drenado" = "2000")),
                  numericInput(ns("year_code"), "Año Selección:", value = 26),
                  radioButtons(ns("program_prefix"), "Programa:", choices = c("CR", "BR"), inline = TRUE),
                  actionButton(ns("btn_promote"), "Generar Nombre Variedad", class = "btn-success")
                )
              ),
              column(8,
                h4("Registro de Variedades"),
                downloadButton(ns("download_promociones"), "Exportar Lista (.xlsx)", class = "btn-info"),
                br(), br(),
                DT::DTOutput(ns("tabla_variedades_nuevas"))
              )
            )
          )
        )
      )
    )
  )
}

# --- Server del Módulo ---
mod_seleccion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Función auxiliar para manejar columnas inexistentes
    if_exists <- function(df, col_name, default_val) {
      if (col_name %in% names(df)) return(df[[col_name]])
      return(rep(default_val, nrow(df)))
    }

    # Piloto ST1: helper reutilizable para estandarizar lectura/procesamiento por etapa.
    process_stage_upload <- function(file_path, anio_seleccion, anio_cruce_default, prog_default, suelo_default) {
      df <- readxl::read_excel(file_path) %>%
        janitor::clean_names() %>%
        mutate(
          anio_seleccion = anio_seleccion,
          raw_prog_col = as.character(if_exists(., "programa", "")),
          extracted_yr = stringr::str_extract(raw_prog_col, "\\d{2}"),
          extracted_prog = stringr::str_extract(raw_prog_col, "CR|BR"),
          anio_cruce = ifelse(!is.na(extracted_yr), as.numeric(extracted_yr) + 2000, anio_cruce_default),
          programa = ifelse(!is.na(extracted_prog), extracted_prog, prog_default),
          es_testigo = toupper(cruce) %in% LISTA_TESTIGOS,
          suelo = suelo_default,
          num_sel = {
            raw <- as.character(if_exists(., "numero_de_seleccion", "0"))
            raw[is.na(raw) | raw == "-"] <- "0"
            as.integer(raw)
          },
          brix = as.numeric(as.character(if_exists(., "brix", 0))),
          vigor = as.integer(as.character(if_exists(., "vigor", 3))),
          accion_campo = tolower(as.character(if_exists(., "accion", "rechazado"))),
          accion = ifelse(grepl("selecc", accion_campo), "S", "R")
        )

      fam_db <- dbGetQuery(con, "SELECT anio, cruce, madre, padre FROM familias_evf")
      if (nrow(fam_db) > 0) {
        df <- df %>%
          left_join(fam_db, by = c("anio_cruce" = "anio", "cruce")) %>%
          mutate(
            madre = ifelse(es_testigo, "TESTIGO", ifelse(is.na(madre), "Desconocida", madre)),
            padre = ifelse(es_testigo, "TESTIGO", ifelse(is.na(padre), "Desconocido", padre))
          )
      }

      df
    }
    
    # --- ESTADO 1 (PLÁNTULAS) ---
    rv_st1 <- reactiveVal(NULL)
    
    observeEvent(input$f1, {
      req(input$f1)
      # 1. Archivado Estructurado
      target_dir <- file.path("data/storage", input$up_st1_year, "ST1")
      if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
      file_name <- paste0(input$up_st1_year, "_ST1_", input$up_st1_prog, "_", input$up_st1_soil, ".xlsx")
      file.copy(input$f1$datapath, file.path(target_dir, file_name), overwrite = TRUE)
      
      # 2. Cargar y Limpiar (piloto con helper para ST1)
      df <- process_stage_upload(
        file_path = input$f1$datapath,
        anio_seleccion = input$up_st1_year,
        anio_cruce_default = input$up_st1_year_cruce,
        prog_default = input$up_st1_prog,
        suelo_default = input$up_st1_soil
      )
      
      rv_st1(df)
    })
    
    # Confirmar Est 1 en BD
    observeEvent(input$btn_confirm_st1, {
      req(rv_st1())
      # Filtro: Solo guardamos los SELECCIONADOS en la BD de seguimiento
      df_save <- rv_st1() %>% 
        filter(accion == "S") %>%
        select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion)
      
      db_save_st1_selection(con, df_save)
      showNotification(paste("Sincronizados", nrow(df_save), "clones seleccionados al Estado 1."), type = "message")
    })
    
    output$t1 <- DT::renderDT({
      req(rv_st1())
      DT::datatable(rv_st1(), options = list(pageLength = 10), editable = TRUE)
    })
    
    # --- ESTADO 2 (VIGOR/BRIX) ---
    rv_st2 <- reactiveVal(NULL)
    
    observeEvent(input$f2, {
      req(input$f2)
      # 1. Archivado Estructurado
      target_dir <- file.path("data/storage", input$up_st2_year, "ST2")
      if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
      file_name <- paste0(input$up_st2_year, "_ST2_", input$up_st2_prog, "_", input$up_st2_soil, ".xlsx")
      file.copy(input$f2$datapath, file.path(target_dir, file_name), overwrite = TRUE)
      
      # 2. Cargar y Limpiar (piloto extendido con helper para ST2)
      df <- process_stage_upload(
        file_path = input$f2$datapath,
        anio_seleccion = input$up_st2_year,
        anio_cruce_default = input$up_st2_year_cruce,
        prog_default = input$up_st2_prog,
        suelo_default = input$up_st2_soil
      )
      rv_st2(df)
    })
    
    # Confirmar Est 2 en BD
    observeEvent(input$btn_confirm_st2, {
      req(rv_st2())
      df_save <- rv_st2() %>% 
        filter(accion == "S") %>%
        select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion)
      
      db_save_st2_selection(con, df_save)
      showNotification(paste("Sincronizados", nrow(df_save), "clones seleccionados al Estado 2."), type = "message")
    })
    
    # Mostrar Clones Promocionados desde Estado 1 (para el Estado 2)
    output$promocionados_est1 <- DT::renderDT({
      # Disparador reactivo: refrescar cuando se confirma el st1 o carga el st2
      input$btn_confirm_st1
      df <- db_get_selected_clones(con, "st1")
      req(nrow(df) > 0)
      df %>% select(cruce, num_sel, brix)
    }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    
    output$t2 <- DT::renderDT({
      req(rv_st2())
      DT::datatable(rv_st2(), options = list(pageLength = 10), editable = TRUE)
    })
    
    # --- ESTADO 3 (MOLINO) ---
    rv_st3 <- reactiveVal(NULL)
    observeEvent(input$f3, {
      req(input$f3)
      df <- process_stage_upload(
        file_path = input$f3$datapath,
        anio_seleccion = input$up_st3_year,
        anio_cruce_default = input$up_st3_year_cruce,
        prog_default = input$up_st3_prog,
        suelo_default = input$up_st3_soil
      )
      rv_st3(df)
    })
    observeEvent(input$btn_confirm_st3, {
      req(rv_st3())
      df_save <- rv_st3() %>% filter(accion == "S") %>% 
        select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion)
      db_save_st3_selection(con, df_save)
      showNotification(paste("Sincronizados", nrow(df_save), "clones seleccionados al Estado 3."), type = "message")
    })
    output$promocionados_est2 <- DT::renderDT({
      input$btn_confirm_st2; df <- db_get_selected_clones(con, "st2")
      req(nrow(df) > 0); df %>% select(cruce, num_sel, brix)
    }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$t3 <- DT::renderDT({ req(rv_st3()); DT::datatable(rv_st3(), editable = TRUE) })

    # --- ESTADO 4 (COMERCIAL) ---
    rv_st4 <- reactiveVal(NULL)
    observeEvent(input$f4, {
      req(input$f4)
      df <- process_stage_upload(
        file_path = input$f4$datapath,
        anio_seleccion = input$up_st4_year,
        anio_cruce_default = input$up_st4_year_cruce,
        prog_default = input$up_st4_prog,
        suelo_default = input$up_st4_soil
      )
      rv_st4(df)
    })
    observeEvent(input$btn_confirm_st4, {
      req(rv_st4())
      df_save <- rv_st4() %>% filter(accion == "S") %>% 
        select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion)
      db_save_st4_selection(con, df_save)
      showNotification(paste("Sincronizados", nrow(df_save), "clones seleccionados al Estado 4."), type = "message")
    })
    output$promocionados_est3 <- DT::renderDT({
      input$btn_confirm_st3; df <- db_get_selected_clones(con, "st3")
      req(nrow(df) > 0); df %>% select(cruce, num_sel, brix)
    }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$t4 <- DT::renderDT({ req(rv_st4()); DT::datatable(rv_st4(), editable = TRUE) })

    # --- ESTADO 5 (VARIEDADES) ---
    rv_st5 <- reactiveVal(NULL)
    observeEvent(input$f5, {
      req(input$f5)
      df <- process_stage_upload(
        file_path = input$f5$datapath,
        anio_seleccion = input$up_st5_year,
        anio_cruce_default = input$up_st5_year_cruce,
        prog_default = input$up_st5_prog,
        suelo_default = input$up_st5_soil
      )
      rv_st5(df)
    })
    observeEvent(input$btn_confirm_st5, {
      req(rv_st5())
      df_save <- rv_st5() %>% filter(accion == "S") %>% 
        select(anio_seleccion, anio_cruce, programa, suelo, cruce, num_sel, brix, vigor, accion)
      db_save_st5_selection(con, df_save)
      showNotification(paste("Sincronizados", nrow(df_save), "clones seleccionados al Estado 5."), type = "message")
    })
    output$promocionados_est4 <- DT::renderDT({
      input$btn_confirm_st4; df <- db_get_selected_clones(con, "st4")
      req(nrow(df) > 0); df %>% select(cruce, num_sel, brix)
    }, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    output$t5 <- DT::renderDT({ req(rv_st5()); DT::datatable(rv_st5(), editable = TRUE) })
    
    # --- Evaluación de Familias ---
    datos_evf <- reactive({
      req(input$file_evf)
      
      tryCatch({
        # Archivado Estructurado (Repositorio)
        target_dir <- file.path("data/storage", input$up_evf_year, "EVF")
        if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
        
        file_name <- paste0(input$up_evf_year, "_EVF_", input$up_evf_prog, ".xlsx")
        file.copy(input$file_evf$datapath, file.path(target_dir, file_name), overwrite = TRUE)
        
        # Leer y limpiar nombres
        df <- readxl::read_excel(input$file_evf$datapath) %>% 
          janitor::clean_names() %>%
          filter(!is.na(cruce)) # Eliminar filas vacias
        
        # Asegurar que las columnas de datos sean numericas
        cols_num <- c("tca", "rend", "tsa")
        df <- df %>%
          mutate(across(any_of(cols_num), ~ as.numeric(as.character(.x))))
        
        # Calcular TSA si falta
        if (!"tsa" %in% names(df) && all(c("tca", "rend") %in% names(df))) {
          df <- df %>% mutate(tsa = (tca * rend) / 100)
        }
        
        # Lógica de Relativización Blindada
        testigos <- df %>%
          group_by(experimento) %>%
          summarise(
            # Buscamos CR9303, si no existe usamos el promedio del experimento
            tca_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(tca[cruce == "CR9303"], na.rm=T), mean(tca, na.rm=T)),
            rend_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(rend[cruce == "CR9303"], na.rm=T), mean(rend, na.rm=T)),
            tsa_testigo = ifelse(any(cruce == "CR9303", na.rm=T), mean(tsa[cruce == "CR9303"], na.rm=T), mean(tsa, na.rm=T)),
            .groups = "drop"
          ) %>%
          # Evitar division por cero o NAs
          mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | .x == 0, 1, .x)))
        
        # Calcular Índices y añadir columna de Acción
        df_procesado <- df %>%
          left_join(testigos, by = "experimento") %>%
          mutate(
            indice_y = (tca / tca_testigo) * 100,
            indice_q = (rend / rend_testigo) * 100,
            indice_tsa = (tsa / tsa_testigo) * 100
          ) %>%
          mutate(across(where(is.numeric), ~ round(as.numeric(.x), 2)))
        
        if (!"accion" %in% names(df_procesado)) {
          df_procesado$accion <- "Pendiente"
        }
        
        return(df_procesado)
        
      }, error = function(e) {
        showNotification(paste("Error procesando Excel:", e$message), type = "error")
        return(NULL)
      })
    })
    
    output$tabla_evf <- DT::renderDT({
      req(datos_evf())
      DT::datatable(datos_evf(), 
                    options = list(pageLength = 10, scrollX = TRUE),
                    filter = 'top',
                    selection = 'multiple') %>%
        DT::formatStyle(
          'indice_tsa',
          backgroundColor = DT::styleInterval(c(90, 110), c('#fadbd8', '#fdebd0', '#d5f5e3'))
        )
    })
    
    # --- Acción: Auto-seleccionar según umbrales ---
    observeEvent(input$btn_auto_select, {
      req(datos_evf())
      df <- datos_evf()
      
      # Encontrar indices de filas que cumplen el criterio
      rows_to_select <- which(df$indice_tsa >= input$threshold_tsa & df$indice_q >= input$threshold_q)
      
      if (length(rows_to_select) > 0) {
        DT::selectRows(DT::dataTableProxy("tabla_evf"), rows_to_select)
        showNotification(paste("Se han pre-seleccionado", length(rows_to_select), "familias Élite."), type = "warning")
      } else {
        showNotification("Ninguna familia cumple con los umbrales actuales.", type = "error")
      }
    })
    
    # --- Acción: Confirmar Selección de Familias ---
    observeEvent(input$btn_confirm_evf, {
      req(datos_evf())
      
      # Tomamos TODO el dataset actual
      df_completo <- datos_evf()
      
      # Identificamos indices de seleccionados
      idx_sel <- input$tabla_evf_rows_selected
      
      # Creamos columna de accion final: S para seleccionados, R para el resto
      df_final <- df_completo %>%
        mutate(accion = ifelse(row_number() %in% idx_sel, "S", "R"))
      
      # Guardar en Base de Datos
      db_save_evf_selection(con, df_final)
      
      n_s <- sum(df_final$accion == "S")
      n_r <- sum(df_final$accion == "R")
      
      showNotification(paste("Procesadas", nrow(df_final), "familias (S:", n_s, ", R:", n_r, "). Historial actualizado."), 
                       type = "message")
    })
    
    # --- Persistencia de Promociones (SQLite) ---
    # Cargar datos iniciales desde la BD
    df_init <- db_load_promociones(con)
    variedades_cr <- reactiveVal(df_init)
    
    # Actualizar opciones del selector de promocion
    observe({
      # Se dispara cuando se confirma el Estado 3
      input$btn_confirm_st3
      df_st3 <- db_get_selected_clones(con, "st3")
      if (nrow(df_st3) > 0) {
        # Creamos un ID amigable: "Cruce-NumSel"
        ids <- paste0(df_st3$cruce, "-", df_st3$num_sel)
        updateSelectizeInput(session, "clon_to_promote", choices = ids, server = TRUE)
      }
    })
    
    observeEvent(input$btn_promote, {
      req(input$clon_to_promote)
      
      # Base del suelo (0, 1000, 2000)
      base_suelo <- as.numeric(input$suelo_code)
      # Secuencia (buscamos cuántas hay ya de ese suelo para dar el siguiente número)
      actual <- variedades_cr()
      secuencia <- nrow(actual %>% filter(Suelo == input$suelo_code)) + 1
      
      # Generar código de 4 dígitos: base + secuencia
      codigo_final <- sprintf("%04d", base_suelo + secuencia)
      
      # Prefijo seleccionado (CR o BR)
      prefijo <- input$program_prefix
      nuevo_nombre <- paste0(prefijo, input$year_code, codigo_final)
      
      nueva_fila <- data.frame(
        Clon_Origen = input$clon_to_promote,
        Nombre_CR = nuevo_nombre,
        Suelo = input$suelo_code,
        Fecha = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
      
      updated_df <- rbind(actual, nueva_fila)
      variedades_cr(updated_df)
      
      # Guardar en Base de Datos
      db_save_promotion(con, input$clon_to_promote, nuevo_nombre, input$suelo_code, as.character(Sys.Date()))
      
      showNotification(paste("Variedad", nuevo_nombre, "registrada con éxito en la BD."), type = "message")
    })
    
    output$tabla_variedades_nuevas <- DT::renderDT({
      DT::datatable(variedades_cr(), options = list(dom = 't'))
    })
    
    # --- Descarga Excel ---
    output$download_promociones <- downloadHandler(
      filename = function() {
        paste("Registro_Variedades_CR_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        openxlsx::write.xlsx(variedades_cr(), file)
      }
    )
    
  })
}
