# ==============================================================================
# MOD_CRUZAMIENTOS.R — Módulo Shiny: Recomendación de Cruzamientos
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Sugiere cruzamientos óptimos basado en:
#   1. Consanguinidad (F) — evitar depresión por consanguinidad
#   2. Rendimiento de los padres — TCH, TAH, scores agronómicos
#   3. Sanidad de los padres — resistencia a enfermedades
# ==============================================================================

# --- UI del Módulo ---
mod_cruzamientos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Panel de filtros
      box(
        width = 4, title = "Configuración de Cruzamientos",
        status = "success", solidHeader = TRUE,
        icon = icon("sliders-h"),
        
        # Modo de Planificación
        radioButtons(ns("modo_plan"), "Modo de Planificación:",
                     choices = c("Teórico (Catálogo Completo)", "Operativo (Sincronización Floral)"),
                     selected = "Teórico (Catálogo Completo)"),
        
        # UI Condicional: Modo Operativo
        conditionalPanel(
          condition = paste0("input['", ns("modo_plan"), "'] == 'Operativo (Sincronización Floral)'"),
          fileInput(ns("file_floracion"), "Subir Datos de Floración Semanal (CSV/Excel)", 
                    accept = c(".csv", ".xls", ".xlsx")),
          helpText("El archivo debe contener las columnas: VARIEDAD, SX, EMF_ACTUAL")
        ),
        
        hr(),
        
        # Pool de progenitores (Ocultar en modo operativo)
        conditionalPanel(
          condition = paste0("input['", ns("modo_plan"), "'] == 'Teórico (Catálogo Completo)'"),
          selectizeInput(
            ns("pool_madres"), "Pool de Madres:",
            choices = NULL, multiple = TRUE,
            options = list(maxOptions = 100, placeholder = "Todas las disponibles...")
          ),
          selectizeInput(
            ns("pool_padres"), "Pool de Padres:",
            choices = NULL, multiple = TRUE,
            options = list(maxOptions = 100, placeholder = "Todas las disponibles...")
          )
        ),
        
        hr(),
        tags$h5(icon("leaf"), "Adaptación Específica", style = "font-weight: bold;"),
        
        # Filtro de Suelo
        selectInput(ns("filtro_suelo"), "Tipo de Suelo Objetivo:",
                    choices = c("Cualquiera", "GOOD", "CLAY", "ROCKY"),
                    selected = "Cualquiera"),
        
        # Tipo de Cruce
        selectInput(ns("tipo_cruce"), "Tipo de Cruce Objetivo:",
                    choices = c("Biparental", "Policruce"),
                    selected = "Biparental"),
        helpText("Biparental: Prioriza éxito probado (C1/C6)."),
        helpText("Policruce: Prioriza variabilidad de categorías."),
        
        hr(),
        tags$h5(icon("balance-scale"), "Pesos del Modelo", style = "font-weight: bold;"),
        
        # Umbral de consanguinidad
        sliderInput(ns("limit_f"), "Máxima Consanguinidad (F):",
                    min = 0, max = 0.25, value = 0.0625, step = 0.005),
        
        # Pesos de criterios
        sliderInput(ns("w_genetic"), "Peso: Diversidad Genética (1-F)",
                    min = 0, max = 1, value = 0.3, step = 0.05),
        sliderInput(ns("w_factor"), "Peso: Valor Real (FACTOR)",
                    min = 0, max = 1, value = 0.7, step = 0.05),
        
        hr(),
        tags$h5(icon("shield-alt"), "Seguridad Genética", style = "font-weight: bold;"),
        checkboxInput(ns("solo_evitar_directos"), "Omitir cálculo de F (Solo evitar parientes directos)", value = FALSE),
        helpText("Útil si la base de datos de parentesco está incompleta."),
        
        # Número de sugerencias
        numericInput(ns("top_n"), "Top N cruces:", value = 50, min = 10, max = 500),
        
        hr(),
        actionButton(ns("btn_simular"), "Simular Cruzamientos",
                     class = "btn-primary btn-block",
                     icon = icon("vials")),
        
        downloadButton(ns("btn_export"), "Exportar CSV (Resumen)",
                       class = "btn-info btn-block"),
        
        hr(),
        tags$h5(icon("print"), "Herramientas de Campo", style = "font-weight: bold;"),
        numericInput(ns("num_lanterna_start"), "Número Inicial de Lanterna:", value = 1, min = 1),
        downloadButton(ns("btn_export_campo"), "Descargar Hoja de Montaje (Imprimible)",
                       class = "btn-success btn-block")
      ),
      
      # Panel de resultados
      box(
        width = 8, title = "Cruzamientos Recomendados",
        status = "primary", solidHeader = TRUE,
        
        # KPIs superiores
        fluidRow(
          valueBoxOutput(ns("vb_total_cruces"), width = 4),
          valueBoxOutput(ns("vb_avg_f"), width = 4),
          valueBoxOutput(ns("vb_best_score"), width = 4)
        ),
        
        # Tabla de resultados
        DT::DTOutput(ns("tabla_cruces")),
        
        # NUEVA SECCIÓN: Sugerencia de Lanternas (Solo Policruces)
        uiOutput(ns("ui_lanternas")),
        
        hr(),
        # Detalle del cruce seleccionado
        uiOutput(ns("detalle_cruce"))
      )
    )
  )
}

# --- Server del Módulo ---
mod_cruzamientos_server <- function(id, cat_var, pedigree_var, df_ped_wide, df_act2025, df_categorias) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- Poblar selectize SOLO con variedades del año (AllAct2025) que tienen parentesco ---
    ids_con_pedigree <- unique(as.character(df_ped_wide$id_variedad))
    
    nombres_disponibles <- df_act2025 %>%
      filter(variedad %in% ids_con_pedigree) %>%
      left_join(cat_var %>% select(id_variedad, descripcion_variedad), by = c("variedad" = "id_variedad")) %>%
      mutate(label = ifelse(is.na(descripcion_variedad), variedad, descripcion_variedad)) %>%
      arrange(label)
    
    opciones <- setNames(nombres_disponibles$variedad,
                         nombres_disponibles$label)
    
    updateSelectizeInput(session, "pool_madres",
                         choices = opciones, server = TRUE)
    updateSelectizeInput(session, "pool_padres",
                         choices = opciones, server = TRUE)
    
    # --- 1. Calcular Categorías de Mejoramiento (Ahora viene de global.R) ---
    # df_categorias ya está disponible como argumento
    
    
    # --- Calcular Matriz A (una sola vez) ---
    matriz_A <- reactive({
      # Formato requerido por AGHmatrix: data.frame con columnas Ind, Sire, Dam
      ped_for_matrix <- df_ped_wide %>%
        select(id_variedad, padre, madre)
      
      # Reemplazar NA por 0 para AGHmatrix
      ped_for_matrix$padre[is.na(ped_for_matrix$padre)] <- 0
      ped_for_matrix$madre[is.na(ped_for_matrix$madre)] <- 0
      
      tryCatch({
        # AGHmatrix usará "0" para indicar padres faltantes si lo convertimos previamente
        Amatrix(as.data.frame(ped_for_matrix))
      }, error = function(e) {
        showNotification(paste("Error en Matriz A:", e$message), type = "error")
        NULL
      })
    })
    

    # --- Lectura de Archivo de Floración ---
    datos_floracion <- reactive({
      req(input$modo_plan == "Operativo (Sincronización Floral)")
      req(input$file_floracion)
      
      ext <- tools::file_ext(input$file_floracion$name)
      df_flor <- tryCatch({
        if (ext == "csv") {
          read.csv(input$file_floracion$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xls", "xlsx")) {
          readxl::read_excel(input$file_floracion$datapath)
        } else {
          stop("Formato no soportado. Por favor suba un CSV o Excel.")
        }
      }, error = function(e) {
        showNotification(paste("Error leyendo archivo:", e$message), type = "error")
        return(NULL)
      })
      
      req(df_flor)
      
      # Estandarizar nombres a mayúsculas para facilitar detección
      colnames(df_flor) <- toupper(colnames(df_flor))
      
      # Mapeo flexible: Si existe "DATOS_FLORACION" pero no "EMF_ACTUAL", renombrar
      if (!("EMF_ACTUAL" %in% colnames(df_flor)) && ("DATOS_FLORACION" %in% colnames(df_flor))) {
        df_flor <- df_flor %>% rename(EMF_ACTUAL = DATOS_FLORACION)
      }
      
      # Validar columnas requeridas
      req_cols <- c("VARIEDAD", "SX", "EMF_ACTUAL")
      if (!all(req_cols %in% colnames(df_flor))) {
        msg <- paste("Faltan columnas. Requeridas:", paste(req_cols, collapse=", "), 
                     ". Detectadas:", paste(colnames(df_flor), collapse=", "))
        showNotification(msg, type = "error", duration = 10)
        return(NULL)
      }
      
      # Limpiar y castear
      df_flor %>%
        mutate(
          VARIEDAD = trimws(as.character(VARIEDAD)),
          SX = as.numeric(SX),
          EMF_ACTUAL = as.numeric(EMF_ACTUAL)
        ) %>%
        filter(!is.na(VARIEDAD), !is.na(SX), !is.na(EMF_ACTUAL), EMF_ACTUAL > 0)
    })
    
    # --- Datos reactivos de resultados ---
    resultados <- eventReactive(input$btn_simular, {
      req(input$modo_plan)
      
      A <- matriz_A()
      validate(need(!is.null(A), "Error al calcular la Matriz de Parentesco."))
      
      ids_matriz <- rownames(A)
      
      if (input$modo_plan == "Teórico (Catálogo Completo)") {
        # MODO TEÓRICO: Usar inputs manuales
        pool_m <- input$pool_madres
        pool_p <- input$pool_padres
        
        # Si no se seleccionan, asume "Todas las variedades disponibles"
        if (is.null(pool_m)) pool_m <- ids_matriz
        if (is.null(pool_p)) pool_p <- ids_matriz
        
      } else {
        # MODO OPERATIVO: Usar archivo de floración
        df_f <- datos_floracion()
        req(df_f)
        
        # SX 3 = Madres
        pool_m <- df_f %>% filter(SX == 3) %>% pull(VARIEDAD)
        # SX 1, 2 = Padres
        pool_p <- df_f %>% filter(SX %in% c(1, 2)) %>% pull(VARIEDAD)
        
        validate(need(length(pool_m) > 0, "No hay Hembras (SX=3) con EMF > 0 en el archivo."))
        validate(need(length(pool_p) > 0, "No hay Machos (SX=1 o 2) con EMF > 0 en el archivo."))
      }
      
      # Filtrar por IDs que estén en la matriz SOLO si vamos a calcular F
      if (!input$solo_evitar_directos) {
        # Diagnóstico: ¿Están las variedades en la base de datos de parentesco?
        m_en_base <- pool_m %in% ids_matriz
        p_en_base <- pool_p %in% ids_matriz
        
        # Si hay variedades en el Excel que no están en la base, avisar por notificación
        if (input$modo_plan == "Operativo (Sincronización Floral)") {
          missing_m <- pool_m[!m_en_base]
          missing_p <- pool_p[!p_en_base]
          
          if (length(missing_m) > 0 || length(missing_p) > 0) {
            showNotification(
              paste("Atención: Hay variedades en el Excel (ej:", 
                    head(c(missing_m, missing_p), 1), 
                    ") que no tienen registro de parentesco y serán omitidas para el cálculo de F."),
              type = "warning", duration = 10
            )
          }
        }
        
        pool_m <- intersect(pool_m, ids_matriz)
        pool_p <- intersect(pool_p, ids_matriz)
      }
      
      # --- FILTRO DE SUELO (Aplica a ambos modos) ---
      if (input$filtro_suelo != "Cualquiera") {
        # Obtener lista de variedades adaptadas al suelo elegido
        vars_adaptadas <- df_act2025 %>%
          filter(grepl(input$filtro_suelo, toupper(adapt))) %>%
          pull(variedad)
        
        # Si estamos en modo operativo, también podemos mirar la columna ADAPT del Excel si existe
        if (input$modo_plan == "Operativo (Sincronización Floral)") {
          df_f <- datos_floracion()
          if ("ADAPT" %in% colnames(df_f)) {
            vars_excel_adapt <- df_f %>%
              filter(grepl(input$filtro_suelo, toupper(ADAPT))) %>%
              pull(VARIEDAD)
            vars_adaptadas <- unique(c(vars_adaptadas, vars_excel_adapt))
          }
        }
        
        pool_m <- intersect(pool_m, vars_adaptadas)
        pool_p <- intersect(pool_p, vars_adaptadas)
      }
      
      # Validaciones finales
      validate(
        need(length(pool_m) >= 1, 
             paste("No hay suficientes madres (SX=3) para el suelo:", input$filtro_suelo)),
        need(length(pool_p) >= 1, 
             paste("No hay suficientes padres (SX=1 o 2) para el suelo:", input$filtro_suelo))
      )
      
      # Generar combinaciones
      comb <- expand.grid(
        madre_id = pool_m,
        padre_id = pool_p,
        stringsAsFactors = FALSE
      ) %>%
        filter(madre_id != padre_id)
      
      # Si solo queremos evitar parientes directos, saltar el cálculo pesado de F
      if (input$solo_evitar_directos) {
        comb <- comb %>%
          left_join(df_ped_wide %>% select(id_variedad, padre_m = padre, madre_m = madre), by = c("madre_id" = "id_variedad")) %>%
          mutate(
            es_pariente_directo = (padre_id == padre_m | padre_id == madre_m)
          ) %>%
          # IMPORTANTE: is.na(es_pariente_directo) significa que la variedad no está en la base,
          # por lo tanto permitimos el cruce en este modo de seguridad básica.
          filter(!es_pariente_directo | is.na(es_pariente_directo)) %>%
          mutate(f_progenie = 0.01) # Valor dummy bajo
      } else {
        # Calcular F real
        withProgress(message = "Calculando consanguinidad...", {
          comb$f_progenie <- apply(comb, 1, function(x) {
            tryCatch({
              A[x[1], x[2]] / 2
            }, error = function(e) NA_real_)
          })
        })
        
        # Filtrar por umbral de F
        comb <- comb %>%
          filter(!is.na(f_progenie), f_progenie <= input$limit_f)
      }
      
      validate(need(nrow(comb) > 0, 
                    "No hay cruces que cumplan con los criterios de consanguinidad/parentesco seleccionados."))
      
      # Agregar scores y CATEGORÍAS
      comb <- comb %>%
        # Cruces para Madres
        left_join(df_categorias() %>% select(variedad, cat_m = categoria, factor_m = factor, adapt_m = adapt, 
                                            disease_m = disease, y_m = y_score, q_m = q_score), 
                  by = c("madre_id" = "variedad")) %>%
        # Cruces para Padres
        left_join(df_categorias() %>% select(variedad, cat_p = categoria, factor_p = factor, adapt_p = adapt, 
                                            disease_p = disease, y_p = y_score, q_p = q_score), 
                  by = c("padre_id" = "variedad"))
      
      # Agregar datos florales si es modo operativo
      if (input$modo_plan == "Operativo (Sincronización Floral)") {
        df_f <- datos_floracion()
        comb <- comb %>%
          left_join(df_f %>% select(VARIEDAD, emf_m = EMF_ACTUAL), by = c("madre_id" = "VARIEDAD")) %>%
          left_join(df_f %>% select(VARIEDAD, sx_p = SX, emf_p = EMF_ACTUAL), by = c("padre_id" = "VARIEDAD")) %>%
          mutate(
            # Bonificación al score si es macho fuerte (SX = 1)
            bono_macho = ifelse(sx_p == 1, 0.1, 0) # 10% de boost
          )
        
        # Opcional: Eliminar combinaciones con 0 flores (ya deberían venir filtradas pero por seguridad)
        comb <- comb %>% filter(emf_m > 0, emf_p > 0)
          
        validate(need(nrow(comb) > 0, "No hay cruces posibles con las flores disponibles en el archivo."))
      }
      
      comb <- comb %>%
        mutate(
          # Promedio de FACTOR
          factor_avg = rowMeans(cbind(
            ifelse(is.na(factor_m), 0, factor_m),
            ifelse(is.na(factor_p), 0, factor_p)
          )),
          # F normalizado (0-1)
          f_norm = scales::rescale(f_progenie, to = c(0, 1)),
          # Normalizar FACTOR promedio para el score total
          factor_norm = scales::rescale(factor_avg, to = c(0, 1)),
          
          # --- LÓGICA DE BONOS DINÁMICA ---
          bono_cat = case_when(
            input$tipo_cruce == "Biparental" ~ case_when(
              (grepl("C1", cat_m) & grepl("C1", cat_p)) ~ 0.35, # C1 x C1 prioridad máxima
              (grepl("C1", cat_m) | grepl("C1", cat_p)) ~ 0.20,
              (grepl("C2", cat_m) & grepl("C2", cat_p)) ~ 0.20, # Calidad Élite x Calidad Élite
              (grepl("C6", cat_m) & grepl("C6", cat_p)) ~ 0.15, # Factor x Factor
              TRUE ~ 0
            ),
            input$tipo_cruce == "Policruce" ~ case_when(
              (cat_m != cat_p & !grepl("C5", cat_m) & !grepl("C5", cat_p)) ~ 0.25, # Mezcla de élites
              (grepl("C3", cat_m) & grepl("C4", cat_p)) ~ 0.20, # Calidad x Rendimiento
              (grepl("C2", cat_m) & grepl("C4", cat_p)) ~ 0.20, # VHQ x Rendimiento
              TRUE ~ 0.10 # Variabilidad base
            )
          ),
          
          # Score total ponderado
          score_total = input$w_genetic * (1 - f_norm) +
                        input$w_factor * factor_norm +
                        bono_cat
        )
      
      # Sumar el bono de macho si existe (SX=1)
      if ("bono_macho" %in% colnames(comb)) {
        comb$score_total <- comb$score_total + comb$bono_macho
      }
      
      # Nombres comerciales (Mapeo robusto por ID o por Nombre)
      # Primero intentamos por ID
      comb <- comb %>%
        left_join(cat_var %>% select(id_v = id_variedad, desc_v = descripcion_variedad),
                  by = c("madre_id" = "id_v")) %>%
        rename(madre_nombre = desc_v) %>%
        left_join(cat_var %>% select(id_v = id_variedad, desc_v = descripcion_variedad),
                  by = c("padre_id" = "id_v")) %>%
        rename(padre_nombre = desc_v)
      
      # Si los nombres quedaron vacíos (NA), es porque en el Excel venía el nombre en lugar del ID
      # Intentamos el mapeo inverso (por descripcion_variedad)
      comb <- comb %>%
        mutate(
          madre_nombre = ifelse(is.na(madre_nombre), as.character(madre_id), madre_nombre),
          padre_nombre = ifelse(is.na(padre_nombre), as.character(padre_id), padre_nombre)
        )
      
      comb <- comb %>%
        arrange(desc(score_total)) %>%
        head(input$top_n)
      
      comb
    })
    
    # --- 3. NUEVO: Generador de Lanternas (Policruces con Fraccionamiento) ---
    lanternas_recomendadas <- reactive({
      req(input$tipo_cruce == "Policruce")
      res <- resultados()
      req(nrow(res) > 0)
      
      # Inventario de hembras (usamos un dataframe reactivo local para ir restando flores)
      inv_h <- res %>%
        group_by(madre_id) %>%
        summarise(
          nombre = first(madre_nombre),
          flores_restantes = first(emf_m),
          score_m = max(score_total)
        ) %>%
        arrange(desc(score_m))
      
      # Inventario de machos
      inv_p <- res %>%
        group_by(padre_id) %>%
        summarise(
          nombre = first(padre_nombre),
          flores_restantes = first(emf_p),
          sx = first(sx_p)
        )
      
      lanternas <- list()
      cuota_h_por_lanterna <- 3 # Usamos 3 flores de cada hembra por lanterna
      
      # Intentar crear hasta 8 lanternas para dar opciones
      for (i in 1:8) {
        # Tomar las 3 hembras con más flores restantes
        h_pool <- inv_h %>% filter(flores_restantes >= cuota_h_por_lanterna) %>% head(3)
        if (nrow(h_pool) < 2) break # Necesitamos al menos 2 hembras para un poli
        
        h_ids <- h_pool$madre_id
        h_nombres <- paste(h_pool$nombre, collapse = ", ")
        total_flores_h_lanterna <- nrow(h_pool) * cuota_h_por_lanterna
        
        # Buscar machos compatibles con este grupo
        m_compatibles <- res %>%
          filter(madre_id %in% h_ids) %>%
          group_by(padre_id) %>%
          summarise(n_compat = n_distinct(madre_id)) %>%
          filter(n_compat == nrow(h_pool)) %>%
          left_join(inv_p, by = "padre_id") %>%
          filter(flores_restantes > 0) %>%
          arrange(desc(sx == 1), desc(flores_restantes)) # Priorizar machos fuertes y con stock
        
        if (nrow(m_compatibles) == 0) {
          # Si no hay machos para estas 3, restamos stock a la primera para probar otra combinación
          inv_h$flores_restantes[inv_h$madre_id == h_ids[1]] <- 0 
          next
        }
        
        # Seleccionar hasta 6 machos para cubrir el ratio 2:1
        m_seleccionados <- data.frame()
        flores_m_acum <- 0
        for (m_idx in 1:nrow(m_compatibles)) {
          m_seleccionados <- rbind(m_seleccionados, m_compatibles[m_idx, ])
          flores_m_acum <- flores_m_acum + m_compatibles$flores_restantes[m_idx]
          if (nrow(m_seleccionados) >= 6 | flores_m_acum >= (2 * total_flores_h_lanterna)) break
        }
        
        m_nombres <- paste(m_seleccionados$nombre, collapse = ", ")
        ratio_final <- round(flores_m_acum / total_flores_h_lanterna, 1)
        
        # Determinar el "Tipo de Poli"
        tipos <- c(h_pool$cat, m_seleccionados$cat)
        tipo_poli <- case_when(
          any(grepl("C1|C2", tipos)) & any(grepl("C4", tipos)) ~ "Calidad Élite x Rendimiento",
          any(grepl("C1|C2", tipos)) ~ "Élite Progenie/VHQ",
          any(grepl("C3", tipos)) & any(grepl("C4", tipos)) ~ "Combinatorio Y x Q",
          any(grepl("C6", tipos)) ~ "Enfoque Factor Campo",
          TRUE ~ "Exploración Comercial"
        )
        
        # 2. Crear desglose para la Hoja de Campo (Formato Largo)
        # Para cada madre: cuota fija
        det_h <- h_pool %>%
          mutate(
            Lanterna_ID = input$num_lanterna_start + i - 1,
            Rol = "HEMBRA (SX:3)",
            Variedad = nombre,
            Flores_a_Cortar = cuota_h_por_lanterna,
            Objetivo = tipo_poli
          ) %>%
          select(Lanterna_ID, Objetivo, Rol, Variedad, Flores_a_Cortar)
        
        # Para los machos: distribuimos las flores
        det_m <- m_seleccionados %>%
          mutate(
            Lanterna_ID = input$num_lanterna_start + i - 1,
            Rol = paste0("MACHO (SX:", sx, ")"),
            Variedad = nombre,
            # Indicamos cuántas flores tiene disponibles para que el técnico decida el máximo
            Flores_a_Cortar = flores_restantes, 
            Objetivo = tipo_poli
          ) %>%
          select(Lanterna_ID, Objetivo, Rol, Variedad, Flores_a_Cortar)
        
        # Guardar resumen para la tabla UI
        lanternas[[i]] <- data.frame(
          ID = input$num_lanterna_start + i - 1,
          Objetivo = tipo_poli,
          Hembras = h_nombres,
          Machos = m_nombres,
          `Uso Flores (H:M)` = paste0(total_flores_h_lanterna, " : ", flores_m_acum),
          `Ratio (M:H)` = ratio_final,
          Status = ifelse(ratio_final >= 2, "Óptimo (2:1)", "Aceptable"),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        # Guardar detalle para el archivo
        if (i == 1) {
          hoja_campo_data <<- rbind(det_h, det_m)
        } else {
          hoja_campo_data <<- rbind(hoja_campo_data, det_h, det_m)
        }
        
        # Restar stock usado
        inv_h$flores_restantes[inv_h$madre_id %in% h_ids] <- inv_h$flores_restantes[inv_h$madre_id %in% h_ids] - cuota_h_por_lanterna
      }
      
      if (length(lanternas) == 0) return(NULL)
      do.call(rbind, lanternas)
    })
    
    # Objeto para persistir el detalle fuera del reactive si es necesario (o usar una lista reactiva)
    hoja_campo_data <- NULL
    
    # --- UI Dinámica para Lanternas ---
    output$ui_lanternas <- renderUI({
      req(input$tipo_cruce == "Policruce")
      tagList(
        hr(),
        box(
          width = 12, title = "Sugerencia de Montaje de Lanternas (Policruces)",
          status = "warning", solidHeader = TRUE, icon = icon("layer-group"),
          p(tags$b("Nota: "), "El 'Ratio' indica cuántas flores de macho hay por cada hembra. Lo ideal es >= 2."),
          DT::DTOutput(ns("tabla_lanternas"))
        )
      )
    })
    
    output$tabla_lanternas <- DT::renderDT({
      req(lanternas_recomendadas())
      DT::datatable(lanternas_recomendadas(), rownames = FALSE, options = list(dom = 't'))
    })
    
    # --- KPIs ---
    output$vb_total_cruces <- renderValueBox({
      n <- if (!is.null(resultados())) nrow(resultados()) else 0
      valueBox(n, "Cruces Sugeridos", icon = icon("dna"), color = "green")
    })
    
    output$vb_avg_f <- renderValueBox({
      avg <- if (!is.null(resultados())) round(mean(resultados()$f_progenie), 4) else 0
      valueBox(avg, "F Promedio", icon = icon("chart-line"), color = "yellow")
    })
    
    output$vb_best_score <- renderValueBox({
      best <- if (!is.null(resultados())) round(max(resultados()$score_total), 3) else 0
      valueBox(best, "Mejor Score", icon = icon("trophy"), color = "blue")
    })
    
    # --- Tabla de resultados ---
    output$tabla_cruces <- DT::renderDT({
      req(resultados())
      
      # Si es modo operativo, añadir las flores y el sexo del padre
      if (input$modo_plan == "Operativo (Sincronización Floral)") {
        tabla <- resultados() %>%
          mutate(Flores = paste0(emf_m, " / ", emf_p)) %>%
          select(
            Madre = madre_nombre,
            `Cat M` = cat_m,
            Padre = padre_nombre,
            `Cat P` = cat_p,
            `Sexo P` = sx_p,
            `Inventario (M/P)` = Flores,
            `F Progenie` = f_progenie,
            `Score Total` = score_total
          )
      } else {
        # Modo teórico
        tabla <- resultados() %>%
          select(
            Madre = madre_nombre,
            `Cat M` = cat_m,
            Padre = padre_nombre,
            `Cat P` = cat_p,
            `Suelo M` = adapt_m,
            `F Progenie` = f_progenie,
            `FACTOR M` = factor_m,
            `FACTOR P` = factor_p,
            `Score Total` = score_total
          )
      }
      
      tabla <- tabla %>%
        mutate(
          across(where(is.numeric), ~ round(.x, 4))
        )
      
      DT::datatable(
        tabla,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(ncol(tabla) - 1, "desc"))  # Ordenar por la penúltima columna (Score Total) o última
        ),
        rownames = FALSE,
        selection = "single",
        caption = "Cruzamientos ordenados por Score Total (mayor = mejor)"
      ) %>%
        DT::formatStyle("Score Total",
                        background = DT::styleColorBar(range(tabla$`Score Total`), "#27ae60"),
                        backgroundSize = "98% 88%",
                        backgroundRepeat = "no-repeat",
                        backgroundPosition = "center") %>%
        DT::formatStyle("F Progenie",
                        color = DT::styleInterval(c(0.03, 0.0625), 
                                                   c("#27ae60", "#f39c12", "#e74c3c")))
    })
    
    # --- Detalle del cruce seleccionado ---
    output$detalle_cruce <- renderUI({
      req(resultados())
      sel <- input$tabla_cruces_rows_selected
      req(sel)
      
      cruce <- resultados()[sel, ]
      
      tags$div(
        style = "background: #ecf0f1; padding: 15px; border-radius: 8px; margin-top: 10px;",
        tags$h4(icon("flask"), "Detalle del Cruce Seleccionado",
                style = "color: #2c3e50; font-weight: bold;"),
        fluidRow(
          column(6,
                 tags$p(tags$b("Madre: "), cruce$madre_nombre, 
                        tags$span(paste0("(ID: ", cruce$madre_id, ")"), 
                                  style = "color: #7f8c8d;")),
                 tags$p(tags$b("Suelo: "), tags$span(cruce$adapt_m, style="color:#d35400; font-weight:bold;")),
                 tags$p(tags$b("Exito EVF: "), tags$span(cruce$evf_m, class="label label-info")),
                 tags$p(tags$b("Indices: "), paste0("Y: ", round(cruce$y_m, 1), " | Q: ", round(cruce$q_m, 1))),
                 tags$p(tags$b("FACTOR Madre: "), 
                        round(cruce$factor_m, 2)),
                 tags$p(tags$b("DISEASE Madre: "), 
                        round(cruce$disease_m, 2)),
                 tags$p(tags$b("AGRO Madre: "), 
                        round(cruce$agro_m, 2))
          ),
          column(6,
                 tags$p(tags$b("Padre: "), cruce$padre_nombre,
                        tags$span(paste0("(ID: ", cruce$padre_id, ")"), 
                                  style = "color: #7f8c8d;")),
                 tags$p(tags$b("Suelo: "), tags$span(cruce$adapt_p, style="color:#d35400; font-weight:bold;")),
                 tags$p(tags$b("Exito EVF: "), tags$span(cruce$evf_p, class="label label-info")),
                 tags$p(tags$b("Indices: "), paste0("Y: ", round(cruce$y_p, 1), " | Q: ", round(cruce$q_p, 1))),
                 tags$p(tags$b("FACTOR Padre: "), 
                        round(cruce$factor_p, 2)),
                 tags$p(tags$b("DISEASE Padre: "), 
                        round(cruce$disease_p, 2)),
                 tags$p(tags$b("AGRO Padre: "), 
                        round(cruce$agro_p, 2))
          )
        ),
        tags$hr(),
        tags$p(tags$b("Consanguinidad Predicha (F): "),
               tags$span(round(cruce$f_progenie, 4),
                         style = paste0("color: ", 
                                        ifelse(cruce$f_progenie > 0.0625, "#e74c3c", "#27ae60"),
                                        "; font-weight: bold; font-size: 1.2em;"))),
        tags$p(tags$b("Score Total: "),
               tags$span(round(cruce$score_total, 4),
                         style = "font-weight: bold; font-size: 1.2em; color: #2980b9;"))
      )
    })
    
    # --- Exportar CSV ---
    # --- Exportar CSV Resumen ---
    output$btn_export <- downloadHandler(
      filename = function() {
        paste0("Cruces_Sugeridos_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(resultados())
        write.csv(resultados(), file, row.names = FALSE)
      }
    )
    
    # --- Exportar Hoja de Campo (Policruces) ---
    output$btn_export_campo <- downloadHandler(
      filename = function() {
        paste0("HOJA_MONTAJE_CAMPO_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(lanternas_recomendadas())
        
        # Re-generamos el detalle para asegurar que coincida con lo que se ve en pantalla
        # (Aunque usamos la variable global, es más seguro extraerlo del reactive si lo estructuramos como lista)
        # Por ahora, reconstruimos el formato largo desde los resultados de lanternas_recomendadas() 
        # o usamos la lógica de guardado anterior.
        
        # Mejoramos la lógica de exportación para que sea limpia:
        res <- lanternas_recomendadas()
        
        # Como hemos guardado el detalle en la variable hoja_campo_data durante la ejecución de lanternas_recomendadas,
        # simplemente la usamos.
        
        write.csv(hoja_campo_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    return(resultados)
  })
}
