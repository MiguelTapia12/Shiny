# ==============================================================================
# MOD_ARCHIVO.R — Módulo Shiny: Repositorio Histórico (Gestión de Documentos)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- UI del Módulo ---
mod_archivo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(tagList(icon("archive"), tags$span(`data-i18n`="arc_title", " Repositorio Histórico de Selección"))),
      layout_column_wrap(
        width = 1/2,
        div(
          h2(tags$span(`data-i18n`="arc_repo_master", "Repositorio Maestros"), class = "m-0 text-primary"),
          p(tags$span(`data-i18n`="arc_repo_desc", "Gestión de archivos por año y programa."), class = "text-muted")
        ),
        layout_column_wrap(
          width = 1/3,
          selectInput(ns("year_filter"), tags$span(`data-i18n`="lbl_year", "Año:"), choices = c("2024", "2025", "2026"), selected = "2025"),
          selectInput(ns("prog_filter"), tags$span(`data-i18n`="lbl_program", "Programa:"), choices = c("Todos", "CR", "BR"), selected = "Todos"),
          actionButton(ns("btn_refresh"), tags$span(`data-i18n`="btn_refresh", "Actualizar"), icon = icon("sync"), class = "btn-info mt-4")
        )
      )
    ),
    
    layout_column_wrap(
      width = 1/3,
      card(
        card_header(tagList(icon("users-rays"), tags$span(`data-i18n`="stage_evf", " Evaluación de Familias"))),
        uiOutput(ns("list_evf"))
      ),
      card(
        card_header(tagList(icon("seedling"), tags$span(`data-i18n`="stage_st1", " Estado 1 (Plántulas)"))),
        uiOutput(ns("list_st1"))
      ),
      card(
        card_header(tagList(icon("leaf"), tags$span(`data-i18n`="stage_st2", " Estado 2 (Vigor/Brix)"))),
        uiOutput(ns("list_st2"))
      ),
      card(
        card_header(tagList(icon("industry"), tags$span(`data-i18n`="stage_st3", " Estado 3 (Molino)"))),
        uiOutput(ns("list_st3"))
      ),
      card(
        card_header(tagList(icon("flask-vial"), tags$span(`data-i18n`="stage_st4", " Estado 4 (Comercial)"))),
        uiOutput(ns("list_st4"))
      ),
      card(
        card_header(tagList(icon("certificate"), tags$span(`data-i18n`="stage_st5", " Estado 5 (Variedades)"))),
        uiOutput(ns("list_st5"))
      )
    )
  )
}

# --- Server del Módulo ---
mod_archivo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Función para listar y FILTRAR archivos
    get_file_list <- function(year, stage, prog) {
      path <- file.path("data/storage", year, stage)
      if (!dir.exists(path)) return(NULL)
      
      files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(files) == 0) return(NULL)
      
      # Filtrar por programa si no es "Todos"
      if (prog != "Todos") {
        files <- files[grep(paste0("_", prog, "\\.xlsx$"), files)]
      }
      
      return(files)
    }
    
    # Función Genérica para renderizar una gaveta
    render_box <- function(stage) {
      renderUI({
        # Dependencia del botón refresh
        input$btn_refresh
        
        files <- get_file_list(input$year_filter, stage, input$prog_filter)
        
        if (is.null(files) || length(files) == 0) {
          return(p("Sin archivos para este filtro.", style="color: gray; font-style: italic; padding: 10px;"))
        }
        
        tagList(
          lapply(files, function(f) {
            wellPanel(
              style = "padding: 8px; margin-bottom: 5px; background-color: #fcfcfc; border-left: 4px solid #27ae60;",
              fluidRow(
                column(9, strong(basename(f))),
                column(3, downloadButton(ns(paste0("dl_", digest::digest(f))), "", 
                                         class = "btn-xs btn-success", icon = icon("download")))
              )
            )
          })
        )
      })
    }
    
    # Asignar cada gaveta
    output$list_evf <- render_box("EVF")
    output$list_st1 <- render_box("ST1")
    output$list_st2 <- render_box("ST2")
    output$list_st3 <- render_box("ST3")
    output$list_st4 <- render_box("ST4")
    output$list_st5 <- render_box("ST5")
    
    # Lógica de descarga dinámica
    observe({
      # Esta parte es mas compleja en Shiny para descargas dinamicas en bucle
    })
    
  })
}
