mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
        div(style = "text-align: center; padding: 40px 20px; background: linear-gradient(135deg, #0b5c2e 0%, #16a34a 100%); color: white; border-radius: 12px; margin-bottom: 24px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
          h1(tags$b(tags$span(`data-i18n`="home_title", "Central Romana Corporation")), style = "margin-bottom: 10px; font-weight: 800;"),
          h3(tags$span(`data-i18n`="home_subtitle", "Sistema Avanzado de Mejoramiento Genético"), style = "font-weight: 300; opacity: 0.9; margin-bottom: 20px;"),
          p(tags$span(`data-i18n`="home_desc", "Plataforma integral para planificación de cruzamientos, gestión de campo y analítica clonal."), style = "font-size: 1.1rem;")
        )
      )
    ),
    fluidRow(
      column(3, bslib::value_box(title = tags$span(`data-i18n`="home_kpi_cat", "Variedades en Catálogo"), value = textOutput(ns("kpi_var")), showcase = icon("database"), theme = "primary")),
      column(3, bslib::value_box(title = tags$span(`data-i18n`="home_kpi_flores", "Parientes en KM 14"), value = textOutput(ns("kpi_flores")), showcase = icon("tree"), theme = "success")),
      column(3, bslib::value_box(title = tags$span(`data-i18n`="home_kpi_cruces", "Cruces Planificados"), value = textOutput(ns("kpi_cruces")), showcase = icon("project-diagram"), theme = "info")),
      column(3, bslib::value_box(title = tags$span(`data-i18n`="home_kpi_st1", "Clones en Campo (ST1)"), value = textOutput(ns("kpi_clones")), showcase = icon("leaf"), theme = "warning"))
    ),
    br(),
    fluidRow(
      column(6,
        card(
          card_header(icon("bolt"), tags$span(`data-i18n`="home_shortcuts", " Accesos Rápidos")),
          div(class = "d-grid gap-2",
            actionButton(ns("go_cruzamientos"), tags$span(`data-i18n`="home_btn_cross", "Planificar Cruzamientos"), class = "btn btn-outline-primary btn-lg", icon = icon("vials")),
            actionButton(ns("go_floracion"), tags$span(`data-i18n`="home_btn_flower", "Programa de Cartuchos"), class = "btn btn-outline-success btn-lg", icon = icon("spa")),
            actionButton(ns("go_analitica"), tags$span(`data-i18n`="home_btn_analytics", "Captura y Analítica ST"), class = "btn btn-outline-warning btn-lg", icon = icon("mobile-alt"))
          )
        )
      ),
      column(6,
        card(
          card_header(icon("info-circle"), tags$span(`data-i18n`="home_status_title", " Estado del Sistema")),
          p("✅ ", tags$span(`data-i18n`="home_status_db", "Base de datos conectada: "), tags$b("breeding_system.db")),
          p("✅ ", tags$span(`data-i18n`="home_status_api", "API de sincronización móvil en línea")),
          p("✅ ", tags$span(`data-i18n`="home_status_ebv", "Modelos genéticos EBV cargados"))
        )
      )
    )
  )
}

mod_home_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    output$kpi_var <- renderText({
      tryCatch(as.character(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM catalogo")[[1]]), error = function(e) "0")
    })
    output$kpi_flores <- renderText({
      tryCatch(as.character(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM floracion_master")[[1]]), error = function(e) "0")
    })
    output$kpi_cruces <- renderText({
      tryCatch({
        cruces_totales <- as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM registro_cruces")[[1]])
        if (is.na(cruces_totales) || cruces_totales == 0) return("0 (Bi: 0 | Poli: 0)")
        
        bip <- as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM registro_cruces WHERE tipo = 'Biparental'")[[1]])
        poli <- as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM registro_cruces WHERE tipo = 'Policruce'")[[1]])
        
        paste0(cruces_totales, " (Bi: ", ifelse(is.na(bip), 0, bip), " | Poli: ", ifelse(is.na(poli), 0, poli), ")")
      }, error = function(e) "0")
    })
    output$kpi_clones <- renderText({
      tryCatch(as.character(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM clones_st1")[[1]]), error = function(e) "0")
    })
    
    # Navegación (requiere manejo desde app.R para cambiar tabs)
    # Por ahora solo estético o podemos usar updateNavbarPage en app.R
  })
}
