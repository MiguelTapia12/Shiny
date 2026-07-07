#' Módulo Banco de Fuzz (Semillas)
#' Manejo del inventario de gramos de semilla verdadera y actualización de germinación.

mod_banco_fuzz_ui <- function(id) {
  ns <- NS(id)
  div(class = "p-3",
    tagList(
    h3("Banco de Fuzz (Semilla Verdadera)", class="mb-1 mt-0"),
    p("Gestión de inventario de semilla almacenada, retiros para siembra y actualización de germinación.", class = "text-muted mb-3"),
    
    # KPIs
    layout_column_wrap(
      width = 1/3,
      height = "100px",
      fill = FALSE,
      value_box(
        title = "Gramos de Fuzz en Reserva",
        value = textOutput(ns("kpi_gramos_total")),
        theme = "primary",
        class = "py-1 px-3"
      ),
      value_box(
        title = "Cruces (Lotes) Activos",
        value = textOutput(ns("kpi_cruces_activos")),
        theme = "success",
        class = "py-1 px-3"
      ),
      value_box(
        title = "Plantas/Gramo Promedio",
        value = textOutput(ns("kpi_germinacion_prom")),
        theme = "info",
        class = "py-1 px-3"
      )
    ),
    
    br(),
    
    # Panel de Acción y Tablas
    layout_columns(
      col_widths = c(8, 4),
      # Tabla de Inventario
      card(
        card_header(tagList(icon("list"), " Inventario Fuzz (Con Balance Activo)")),
        fluidRow(
          column(6, selectInput(ns("filtro_anio"), "Año:", choices = c("Todos"), width="100%")),
          column(6, selectInput(ns("filtro_tipo"), "Tipo:", choices = c("Todos", "Biparental", "Policruce"), width="100%"))
        ),
        DT::DTOutput(ns("tabla_inventario"))
      ),
      
      # Acciones (Acordeón Desplegable)
      accordion(
        id = ns("acciones_acc"),
        multiple = FALSE,
        accordion_panel(
          title = "1. Prueba de Germinación (Test Inicial)",
          icon = icon("flask"),
          p("Calcula la viabilidad inicial (plantitas/gramo) usando una muestra pequeña.", class="text-muted mb-2"),
          uiOutput(ns("ui_germ_info")),
          numericInput(ns("rc_germ_gramos"), "Gramos de Fuzz usados en la prueba:", value = 0.5, min = 0.1, step = 0.1),
          numericInput(ns("rc_germ_cantidad"), "Plantitas germinadas:", value = NA, min = 0),
          actionButton(ns("btn_rc_germ"), "Registrar Prueba", class = "btn-primary w-100", icon=icon("flask"))
        ),
        accordion_panel(
          title = "2. Registrar Retiro para Siembra",
          icon = icon("minus-circle"),
          uiOutput(ns("ui_retiro_info")),
          numericInput(ns("retiro_gramos"), "Gramos a retirar:", value = 0, min = 0, step = 0.1),
          numericInput(ns("retiro_anio"), "Año de Siembra:", value = as.integer(format(Sys.Date(), "%Y"))),
          textInput(ns("retiro_notas"), "Notas/Observaciones:", placeholder = "Ej. Siembra en bandeja 5..."),
          actionButton(ns("btn_retirar"), "Registrar Retiro", class = "btn-warning w-100", icon=icon("save"))
        ),
        accordion_panel(
          title = "3. Actualizar Plantas Germinadas",
          icon = icon("leaf"),
          p("Suma plantitas (seedlings) que lograron germinar del total sembrado.", class="text-muted mb-2"),
          numericInput(ns("germ_cantidad"), "Total plantitas vivas (seedlings):", value = 0, min = 0),
          actionButton(ns("btn_germinar"), "Actualizar Germinación", class = "btn-success w-100", icon=icon("check"))
        )
      )
    ),
    
    # Historial
    card(
      class = "mt-3 p-3",
      card_header(tagList(icon("history"), " Historial de Retiros de Fuzz")),
      DT::DTOutput(ns("tabla_historial"))
    )
  )
  )
}

mod_banco_fuzz_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    # Reactivo para forzar refresco
    rv_refresh <- reactiveVal(0)
    
    # Filtros Dinámicos
    observe({
      df_anios <- tryCatch(dbGetQuery(con, "SELECT DISTINCT anio_cruce FROM registro_cruces ORDER BY anio_cruce DESC"), error = function(e) data.frame())
      if (nrow(df_anios) > 0) {
        updateSelectInput(session, "filtro_anio", choices = c("Todos", df_anios$anio_cruce))
      }
    })
    
    # 1. Cargar Datos del Inventario (Solo los que tienen gramos > 0 o son recientes)
    datos_inventario <- reactive({
      rv_refresh()
      
      q <- "SELECT id, anio_cruce, madre, padre, tipo, gramos_restantes, semillas as gramos_iniciales, 
                   germinadas, pct_germinacion 
            FROM registro_cruces 
            WHERE (gramos_restantes > 0 OR gramos_restantes IS NULL)"
            
      if (!is.null(input$filtro_anio) && input$filtro_anio != "Todos") {
        q <- paste0(q, " AND anio_cruce = ", input$filtro_anio)
      }
      if (!is.null(input$filtro_tipo) && input$filtro_tipo != "Todos") {
        q <- paste0(q, " AND tipo = '", input$filtro_tipo, "'")
      }
      
      q <- paste0(q, " ORDER BY anio_cruce DESC")
      
      df <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
      df
    })
    
    # Función segura para %||%
    `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
    
    # KPIs
    output$kpi_gramos_total <- renderText({
      df <- datos_inventario()
      if(nrow(df) == 0) return("0 g")
      paste0(round(sum(df$gramos_restantes, na.rm = TRUE), 1), " g")
    })
    
    output$kpi_cruces_activos <- renderText({
      df <- datos_inventario()
      nrow(df)
    })
    
    output$kpi_germinacion_prom <- renderText({
      df <- datos_inventario()
      if(nrow(df) == 0) return("N/A")
      prom <- mean(df$pct_germinacion, na.rm = TRUE)
      if(is.nan(prom)) return("N/A")
      paste0(round(prom, 1), " pl/g")
    })
    
    # Tabla de Inventario
    output$tabla_inventario <- DT::renderDT({
      df <- datos_inventario()
      if(nrow(df) == 0) return(DT::datatable(data.frame(Mensaje="Sin inventario")))
      
      df <- df %>%
        mutate(Seedlings_Potenciales = ifelse(!is.na(pct_germinacion) & !is.na(gramos_restantes), round(pct_germinacion * gramos_restantes), NA)) %>%
        select(
          ID = id,
          `Año` = anio_cruce,
          Madre = madre,
          Padre = padre,
          `Gramos Restantes` = gramos_restantes,
          `Gramos Inic.` = gramos_iniciales,
          `Plantas/g` = pct_germinacion,
          `Seedlings Potenciales` = Seedlings_Potenciales
        )
      
      DT::datatable(
        df,
        selection = "single",
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    # Info de Cruce Seleccionado (Compartido para Prueba de Germinación y Retiro)
    cruce_sel <- reactive({
      req(input$tabla_inventario_rows_selected)
      datos_inventario()[input$tabla_inventario_rows_selected, ]
    })
    
    # UI Info para Prueba de Germinación
    output$ui_germ_info <- renderUI({
      if (length(input$tabla_inventario_rows_selected) == 0) {
        return(p("Toca un cruce en la tabla de inventario a la izquierda.", class="text-danger fw-bold"))
      }
      c <- cruce_sel()
      tagList(
        p(tags$b("Cruce ID:"), c$id, tags$br(),
          tags$b("Familia:"), paste0(c$madre, " x ", c$padre))
      )
    })
    
    # UI Info para Retiro
    output$ui_retiro_info <- renderUI({
      if (length(input$tabla_inventario_rows_selected) == 0) {
        return(p("Toca un cruce en la tabla de inventario a la izquierda.", class="text-danger fw-bold"))
      }
      c <- cruce_sel()
      tagList(
        p(tags$b("Cruce ID:"), c$id, tags$br(),
          tags$b("Familia:"), paste0(c$madre, " x ", c$padre), tags$br(),
          tags$b("Balance Actual:"), paste0(c$gramos_restantes, " gramos"))
      )
    })
    # Registrar Retiro
    observeEvent(input$btn_retirar, {
      req(cruce_sel())
      c <- cruce_sel()
      
      ret <- as.numeric(input$retiro_gramos)
      if (is.na(ret) || ret <= 0) {
        showNotification("Ingrese una cantidad válida mayor a 0.", type = "warning")
        return()
      }
      
      balance_actual <- as.numeric(c$gramos_restantes)
      if (is.na(balance_actual)) balance_actual <- as.numeric(c$gramos_iniciales %||% 0)
      
      if (ret > balance_actual) {
        showNotification("No hay suficientes gramos en inventario.", type = "error")
        return()
      }
      
      tryCatch({
        # 1. Guardar Historial
        dbExecute(con, 
          "INSERT INTO historial_fuzz (cruce_id, fecha_retiro, gramos_retirados, anio_siembra, notas) VALUES (?, ?, ?, ?, ?)",
          params = list(c$id, as.character(Sys.Date()), ret, input$retiro_anio, input$retiro_notas)
        )
        # 2. Descontar Balance
        nuevo_balance <- max(0, balance_actual - ret)
        dbExecute(con, "UPDATE registro_cruces SET gramos_restantes = ? WHERE id = ?", params = list(nuevo_balance, c$id))
        
        showNotification("Retiro registrado y balance descontado.", type = "message")
        updateNumericInput(session, "retiro_gramos", value = 0)
        updateTextInput(session, "retiro_notas", value = "")
        rv_refresh(rv_refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Registrar Prueba Germinacion
    observeEvent(input$btn_rc_germ, {
      req(cruce_sel())
      c <- cruce_sel()
      
      gr <- as.numeric(input$rc_germ_gramos)
      pl <- as.integer(input$rc_germ_cantidad)
      
      if (is.na(gr) || gr <= 0 || is.na(pl) || pl < 0) {
        showNotification("Ingrese gramos y plantitas válidos.", type = "warning")
        return()
      }
      
      tryCatch({
        pct <- round(pl / gr, 1) # Calculamos Plantitas por Gramo
        dbExecute(con, "UPDATE registro_cruces SET pct_germinacion = ? WHERE id = ?", 
                  params = list(pct, c$id))
        showNotification(sprintf("Prueba completada: %.1f pl/g registradas para el cruce %s.", pct, c$id), type = "message")
        updateNumericInput(session, "rc_germ_gramos", value = 0.5)
        updateNumericInput(session, "rc_germ_cantidad", value = NA)
        rv_refresh(rv_refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # Actualizar Plantas Germinadas (Despues de Siembra Real)
    observeEvent(input$btn_germinar, {
      req(cruce_sel())
      c <- cruce_sel()
      plantitas <- as.integer(input$germ_cantidad)
      if (is.na(plantitas) || plantitas < 0) return()
      
      tryCatch({
        dbExecute(con, "UPDATE registro_cruces SET germinadas = ? WHERE id = ?", params = list(plantitas, c$id))
        showNotification("Germinación total actualizada en el lote.", type = "message")
        updateNumericInput(session, "germ_cantidad", value = 0)
        rv_refresh(rv_refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Registrar Prueba de Germinación
    observeEvent(input$btn_rc_germ, {
      req(input$rc_germ_id, input$rc_germ_cantidad)
      tryCatch({
        db_update_germinacion(con,
                              id         = as.integer(input$rc_germ_id),
                              germinadas = as.integer(input$rc_germ_cantidad))
        showNotification(
          paste0("Prueba de germinación actualizada. ID ", input$rc_germ_id,
                 ": ", input$rc_germ_cantidad, " semillas germinadas."),
          type = "message")
        updateNumericInput(session, "rc_germ_id",       value = NA)
        updateNumericInput(session, "rc_germ_cantidad", value = NA)
        rv_refresh(rv_refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Cargar Historial
    output$tabla_historial <- DT::renderDT({
      rv_refresh()
      q <- "SELECT h.id, h.cruce_id, h.fecha_retiro, h.gramos_retirados, h.anio_siembra, h.notas,
                   r.madre, r.padre 
            FROM historial_fuzz h
            JOIN registro_cruces r ON h.cruce_id = r.id
            ORDER BY h.id DESC"
      df <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
      
      if (nrow(df) > 0) {
        df <- df %>% select(
          `Retiro ID` = id,
          `Fecha` = fecha_retiro,
          `Cruce ID` = cruce_id,
          Madre = madre,
          Padre = padre,
          `Año Siembra` = anio_siembra,
          `Gramos Retirados` = gramos_retirados,
          Notas = notas
        )
      } else {
        df <- data.frame(Mensaje = "Sin retiros")
      }
      
      DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
    })
    
  })
}
