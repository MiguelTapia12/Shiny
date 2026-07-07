# ==============================================================================
# UTILS_DB.R — Gestión de Base de Datos SQLite
# Pipeline de Selección Genética — Central Romana
# ==============================================================================

# --- Lista Oficial de Testigos CR ---
LISTA_TESTIGOS <- c("BR0402", "BR0010", "CR87339", "CR74250", "CR951007", "CR93003")

# Validación estricta de nombres de tabla para etapas ST.
validate_stage_table <- function(table_name) {
  allowed <- paste0("clones_st", 1:5)
  tbl <- tolower(as.character(table_name))
  if (!(tbl %in% allowed)) {
    stop("Nombre de tabla de etapa no permitido: ", table_name, call. = FALSE)
  }
  tbl
}

# ------------------------------------------------------------------------------
# db_connect()
# ------------------------------------------------------------------------------
db_connect <- function(db_path = "data/breeding_system.db") {
  db_type <- Sys.getenv("DB_TYPE", "sqlite")
  
  if (tolower(db_type) == "postgres") {
    # Conexión a PostgreSQL (Producción)
    require(RPostgres)
    dbConnect(RPostgres::Postgres(),
              dbname   = Sys.getenv("DB_NAME", "breeding_db"),
              host     = Sys.getenv("DB_HOST", "localhost"),
              port     = as.integer(Sys.getenv("DB_PORT", "5432")),
              user     = Sys.getenv("DB_USER", "postgres"),
              password = Sys.getenv("DB_PASS", ""))
  } else {
    # Conexión a SQLite (Desarrollo/Pruebas locales)
    if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
    dbConnect(RSQLite::SQLite(), db_path)
  }
}

# ------------------------------------------------------------------------------
# db_init_schema()
# ------------------------------------------------------------------------------
db_init_schema <- function(con) {
  # Tabla: Catálogo de Variedades
  dbExecute(con, "CREATE TABLE IF NOT EXISTS catalogo (
    id_variedad TEXT PRIMARY KEY,
    descripcion_variedad TEXT,
    especie TEXT,
    origen TEXT
  )")
  
  # Tabla: Parentesco
  dbExecute(con, "CREATE TABLE IF NOT EXISTS parentesco (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    id_parentesco INTEGER,
    id_compania INTEGER,
    id_variedad TEXT,
    id_variedad_ancestro TEXT,
    tipo_ancestro TEXT
  )")
  
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parentesco_var ON parentesco(id_variedad)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parentesco_anc ON parentesco(id_variedad_ancestro)")
  
  # Tabla: Categorías y Estatus Actual
  dbExecute(con, "CREATE TABLE IF NOT EXISTS categorias (
    variedad TEXT PRIMARY KEY,
    categoria TEXT,
    factor REAL,
    disease REAL,
    y REAL,
    q REAL,
    agro REAL,
    gen REAL,
    carbon REAL,
    roya REAL,
    es REAL,
    evf_info TEXT,
    adapt TEXT,
    status TEXT,
    maxest INTEGER,
    notas TEXT
  )")
  
  # Tabla: Registro de Promociones
  dbExecute(con, "CREATE TABLE IF NOT EXISTS promociones (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    clon_origen TEXT,
    nombre_cr TEXT,
    suelo TEXT,
    fecha TEXT
  )")
  
  # Tabla: Evaluacion de Familias
  dbExecute(con, "CREATE TABLE IF NOT EXISTS familias_evf (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    anio INTEGER, programa TEXT, experimento INTEGER, cruce TEXT,
    madre TEXT, padre TEXT, tca REAL, rend REAL, tsa REAL, indice_tsa REAL, accion TEXT
  )")
  
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_fam_anio_cruce ON familias_evf(anio, cruce)")

  # Tabla: Registro de Sincronización (Logs)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS sync_logs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    fecha_hora TEXT,
    archivo TEXT,
    operacion TEXT,
    resultado TEXT,
    registros_procesados INTEGER,
    usuario TEXT
  )")
  
# Tabla: Captura de Campo desde App Móvil
  dbExecute(con, "CREATE TABLE IF NOT EXISTS field_captures (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    etapa TEXT,
    suelo TEXT,
    anio_seleccion INTEGER,
    anio_cruce INTEGER,
    programa TEXT,
    cruce TEXT,
    num_sel INTEGER,
    brix REAL,
    vigor INTEGER,
    accion TEXT,
    evaluador TEXT,
    latitud REAL,
    longitud REAL,
    ts TEXT
  )")
  
  # Migración para field_captures (añadir evaluador si no existe)
  check_fc <- dbGetQuery(con, "PRAGMA table_info(field_captures)")
  if (nrow(check_fc) > 0 && !"evaluador" %in% check_fc$name) {
    dbExecute(con, "ALTER TABLE field_captures ADD COLUMN evaluador TEXT DEFAULT 'Desconocido'")
  }
  if (nrow(check_fc) > 0 && !"latitud" %in% check_fc$name) {
    dbExecute(con, "ALTER TABLE field_captures ADD COLUMN latitud REAL")
    dbExecute(con, "ALTER TABLE field_captures ADD COLUMN longitud REAL")
  }
  
  # Tablas de seguimiento de Clones (Estado 1 a 5)
  # Verificar si el esquema es viejo (si existe 'anio' en lugar de 'anio_seleccion')
  check_cols <- dbGetQuery(con, "PRAGMA table_info(clones_st1)")
  if (nrow(check_cols) > 0 && !"anio_seleccion" %in% check_cols$name) {
    message(">> Actualizando esquema de clones a multiaño...")
    for (st in c("st1", "st2", "st3", "st4", "st5")) {
      dbExecute(con, sprintf("DROP TABLE IF EXISTS clones_%s", st))
    }
  }

  for (st in c("st1", "st2", "st3", "st4", "st5")) {
    query <- sprintf("CREATE TABLE IF NOT EXISTS clones_%s (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      anio_seleccion INTEGER, 
      anio_cruce INTEGER, 
      programa TEXT, 
      suelo TEXT, 
      cruce TEXT, 
      num_sel INTEGER,
      brix REAL, 
      vigor INTEGER, 
      accion TEXT,
      origen TEXT DEFAULT 'Campo',
      evaluador TEXT,
      fecha_evaluacion TEXT,
      latitud REAL,
      longitud REAL
    )", st)
    dbExecute(con, query)
    dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_contexto ON clones_%s(anio_seleccion, suelo)", st, st))
    
    # Migración: añadir columna 'origen' y 'evaluador' si no existen en tablas pre-existentes
    cols_existentes <- dbGetQuery(con, sprintf("PRAGMA table_info(clones_%s)", st))$name
    if (!"origen" %in% cols_existentes) {
      dbExecute(con, sprintf("ALTER TABLE clones_%s ADD COLUMN origen TEXT DEFAULT 'Campo'", st))
      message(sprintf(">> Migración: columna 'origen' añadida a clones_%s", st))
    }
    if (!"evaluador" %in% cols_existentes) {
      dbExecute(con, sprintf("ALTER TABLE clones_%s ADD COLUMN evaluador TEXT DEFAULT 'Desconocido'", st))
    }
    if (!"fecha_evaluacion" %in% cols_existentes) {
      dbExecute(con, sprintf("ALTER TABLE clones_%s ADD COLUMN fecha_evaluacion TEXT", st))
      dbExecute(con, sprintf("ALTER TABLE clones_%s ADD COLUMN latitud REAL", st))
      dbExecute(con, sprintf("ALTER TABLE clones_%s ADD COLUMN longitud REAL", st))
    }
  }
  # Tabla: Registro Operativo de Cruces
  dbExecute(con, "CREATE TABLE IF NOT EXISTS registro_cruces (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    fecha_cruce TEXT,
    madre TEXT,
    padre TEXT,
    programa TEXT,
    suelo TEXT,
    anio_cruce INTEGER,
    semillas INTEGER,
    germinadas INTEGER,
    pct_germinacion REAL,
    estado TEXT DEFAULT 'EJECUTADO',
    tipo TEXT DEFAULT 'Biparental',
    anio_seleccion INTEGER,
    notas TEXT,
    cruce_previo INTEGER DEFAULT 0,
    flores_m TEXT,
    flores_p TEXT,
    ts_registro TEXT DEFAULT (datetime('now'))
  )")
  
  # Tabla: Historial de EBVs (Trazabilidad Genética)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS historial_ebvs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    variedad TEXT,
    ebv_tca REAL,
    ebv_rend REAL,
    ebv_pureza REAL,
    ebv_fibra REAL,
    ebv_pol REAL,
    total_obs INTEGER,
    confianza TEXT,
    timestamp_calculo TEXT
  )")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_hist_ebvs_var ON historial_ebvs(variedad)")
  
  # Migración para tabla existente
  try(dbExecute(con, "ALTER TABLE registro_cruces ADD COLUMN flores_m TEXT"), silent=TRUE)
  try(dbExecute(con, "ALTER TABLE registro_cruces ADD COLUMN flores_p TEXT"), silent=TRUE)
  try(dbExecute(con, "ALTER TABLE registro_cruces ADD COLUMN gramos_restantes REAL"), silent=TRUE)
  
  # Tabla: Historial de Fuzz (Retiros)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS historial_fuzz (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    cruce_id INTEGER,
    fecha_retiro TEXT,
    gramos_retirados REAL,
    plantas_germinadas INTEGER,
    anio_siembra INTEGER,
    notas TEXT,
    ts_registro TEXT DEFAULT (datetime('now')),
    FOREIGN KEY(cruce_id) REFERENCES registro_cruces(id)
  )")

  # Tabla: Floración Campo (movida desde api.R para garantizar existencia al arrancar)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS floracion_campo (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    fecha_registro TEXT,
    cruce TEXT,
    madre TEXT,
    padre TEXT,
    estado_floracion TEXT,
    cantidad_flores INTEGER,
    comentarios TEXT
  )")

  
  # Tabla: Configuracion de Testigos por Etapa
  dbExecute(con, "CREATE TABLE IF NOT EXISTS testigos_config (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    etapa TEXT,
    programa TEXT,
    variedad_testigo TEXT,
    activo INTEGER DEFAULT 1
  )")
  
  # Tabla: Master de floración (cargado desde Excel cada temporada)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS floracion_master (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    temporada INTEGER,
    variedad TEXT,
    activity INTEGER,
    sec TEXT,
    num INTEGER,
    calles TEXT,
    sx INTEGER,
    adapt TEXT,
    ts_carga TEXT DEFAULT (datetime('now'))
  )")

  # Tabla: Chequeos de floración (registros desde app tablet)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS floracion_chequeos (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    temporada INTEGER,
    fecha_chequeo TEXT,
    dia_semana TEXT,
    sec TEXT,
    num INTEGER,
    variedad TEXT,
    grc_atrasado INTEGER,
    grc_emergencia INTEGER,
    grc_adelantado INTEGER,
    sx INTEGER,
    pct_polen REAL,
    evaluador TEXT,
    notas TEXT,
    ts TEXT DEFAULT (datetime('now'))
  )")
  
  # Poblar testigos_config con valores por defecto si esta vacia
  n_test <- dbGetQuery(con, "SELECT COUNT(*) as n FROM testigos_config")$n
  if (n_test == 0) {
    testigos_default <- data.frame(
      etapa    = c("ST1","ST1","ST2","ST2","ST3","ST3","ST4","ST5"),
      programa = c("CR", "BR", "CR", "BR", "CR", "CR", "CR", "CR"),
      variedad_testigo = c("BR0010","BR0402","BR0010","BR0402",
                           "CR87339","BR0010","CR93003","CR93003"),
      activo   = 1
    )
    dbWriteTable(con, "testigos_config", testigos_default, append = TRUE, row.names = FALSE)
    message(">> Testigos de referencia inicializados.")
  } 
  message(">> Esquema de Base de Datos verificado.")
}

# ------------------------------------------------------------------------------
# db_save_evf_selection()
# ------------------------------------------------------------------------------
db_save_evf_selection <- function(con, df_selected) {
  df_to_save <- df_selected %>%
    select(any_of(c("anio", "programa", "experimento", "cruce", "madre", "padre", "tca", "rend", "tsa", "indice_tsa", "accion")))
  
  if (nrow(df_to_save) > 0) {
    anio_val <- unique(df_to_save$anio)[1]
    prog_val <- unique(df_to_save$programa)[1]
    exp_val  <- unique(df_to_save$experimento)[1]
    dbExecute(con, "DELETE FROM familias_evf WHERE anio = ? AND programa = ? AND experimento = ?", 
              params = list(anio_val, prog_val, exp_val))
  }
  dbWriteTable(con, "familias_evf", df_to_save, append = TRUE)
  
  padres <- unique(c(df_selected$madre, df_selected$padre))
  padres <- padres[padres != "TESTIGO" & !is.na(padres) & padres != ""]
  for (p in padres) {
    dbExecute(con, "UPDATE categorias SET categoria = 'C1: Progeny Tested', evf_info = 'S' WHERE variedad = ?", params = list(p))
  }
}

# ------------------------------------------------------------------------------
# FUNCIONES DE GUARDADO ESTADOS 1-5 (Sin restriccion de PK duplicada)
# ------------------------------------------------------------------------------
db_save_st_generic <- function(con, table_name, df_selected) {
  if (nrow(df_selected) == 0) return(NULL)
  table_name <- validate_stage_table(table_name)
  
  anio_sel  <- unique(df_selected$anio_seleccion)[1]
  prog_val  <- unique(df_selected$programa)[1]
  suelo_val <- unique(df_selected$suelo)[1]
  origen_val <- if ("origen" %in% names(df_selected)) unique(df_selected$origen)[1] else "Campo"
  
  # Limpiar solo el lote que estamos subiendo (Año/Programa/Suelo/Origen)
  dbExecute(con, sprintf("DELETE FROM %s WHERE anio_seleccion = ? AND programa = ? AND suelo = ? AND origen = ?", table_name),
            params = list(anio_sel, prog_val, suelo_val, origen_val))
  
  # Asegurar que la columna 'origen' esté presente
  if (!"origen" %in% names(df_selected)) {
    df_selected$origen <- "Campo"
  }
  
  # Guardar nuevos registros (el ID se genera solo)
  dbWriteTable(con, table_name, df_selected, append = TRUE)
}

db_save_st1_selection <- function(con, df) db_save_st_generic(con, "clones_st1", df)
db_save_st2_selection <- function(con, df) db_save_st_generic(con, "clones_st2", df)
db_save_st3_selection <- function(con, df) db_save_st_generic(con, "clones_st3", df)
db_save_st4_selection <- function(con, df) db_save_st_generic(con, "clones_st4", df)
db_save_st5_selection <- function(con, df) db_save_st_generic(con, "clones_st5", df)

# ------------------------------------------------------------------------------
# db_get_selected_clones()
# ------------------------------------------------------------------------------
db_get_selected_clones <- function(con, etapa) {
  table_name <- validate_stage_table(paste0("clones_", tolower(etapa)))
  if (!dbExistsTable(con, table_name)) return(data.frame())
  query <- sprintf("SELECT * FROM %s WHERE accion = 'S'", table_name)
  dbGetQuery(con, query)
}

# ------------------------------------------------------------------------------
# OTRAS UTILIDADES
# ------------------------------------------------------------------------------
db_load_promociones <- function(con) dbReadTable(con, "promociones")

# HOTFIX de estabilidad: evita error de símbolo ausente al guardar notas.
# Persistencia completa de notas se implementará en una fase posterior.
db_save_note <- function(con, target_id, note) {
  if (!DBI::dbExistsTable(con, "categorias")) return(invisible(FALSE))
  DBI::dbExecute(
    con,
    "UPDATE categorias SET notas = ? WHERE variedad = ?",
    params = list(as.character(note), as.character(target_id))
  )
  invisible(TRUE)
}

db_save_promotion <- function(con, clon_origen, nombre_cr, suelo, fecha) {
  DBI::dbExecute(
    con,
    "INSERT INTO promociones (clon_origen, nombre_cr, suelo, fecha) VALUES (?, ?, ?, ?)",
    params = list(clon_origen, nombre_cr, suelo, fecha)
  )
}

# ------------------------------------------------------------------------------
# db_sync_repository_to_db()
# Escaneo masivo del Repositorio Historico
# ------------------------------------------------------------------------------
db_sync_repository_to_db <- function(con) {
  base_path <- "data/storage"
  if (!dir.exists(base_path)) return(FALSE)
  
  message(">> Iniciando sincronizacion masiva desde Repositorio...")
  
  years <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  
  for (yr in years) {
    stages_dirs <- list.dirs(file.path(base_path, yr), full.names = FALSE, recursive = FALSE)
    
    for (st_dir in stages_dirs) {
      files <- list.files(file.path(base_path, yr, st_dir), pattern = "\\.xlsx$", full.names = TRUE)
      
      for (f in files) {
        tryCatch({
          # Ayudante para columnas faltantes
          if_exists <- function(d, col, def) if (col %in% names(d)) d[[col]] else def
          
          # Identificar Programa (CR o BR) desde el nombre
          prog_val <- if (grepl("_CR_", basename(f))) "CR" else if (grepl("_BR_", basename(f))) "BR" else "Desconocido"
          # Identificar Suelo desde el nombre de forma robusta y normalizada
          base_name_upper <- toupper(basename(f))
          suelo_val <- if (grepl("ROCKY|ROCOSO", base_name_upper)) {
            "ROCOSO"
          } else if (grepl("MAL_DRENADO|MAL DRENADO|MD", base_name_upper)) {
            "MAL_DRENADO"
          } else if (grepl("GOOD|BUENO", base_name_upper)) {
            "BUENO"
          } else {
            "BUENO"  # default fallback
          }
          
          df <- suppressWarnings(readxl::read_excel(f)) %>% janitor::clean_names()
          
          if (st_dir == "EVF") {
            # Procesar Familias con mapeo de accion
            df_proc <- df %>% mutate(
              anio = as.integer(yr), 
              programa = prog_val,
              accion_campo = tolower(as.character(if_exists(., "accion", "rechazado"))),
              accion = ifelse(grepl("selecc", accion_campo), "S", "R")
            )
            db_save_evf_selection(con, df_proc)
          } else {
            # Procesar Clones (ST1 a ST5)
            # El anio de la carpeta es el ANIO DE SELECCION
            anio_sel_val <- as.integer(yr)
            
            df_proc <- df %>% mutate(
              anio_seleccion = anio_sel_val,
              # --- EXTRACCIÓN INTELIGENTE ---
              raw_prog_col = as.character(if_exists(., "programa", "")),
              extracted_yr = stringr::str_extract(raw_prog_col, "\\d{2}"),
              extracted_prog = stringr::str_extract(raw_prog_col, "CR|BR"),
              
              anio_cruce = ifelse(!is.na(extracted_yr), as.numeric(extracted_yr) + 2000, anio_sel_val),
              programa = ifelse(!is.na(extracted_prog), extracted_prog, prog_val),
              
              # Identificar si es Testigo
              es_testigo = toupper(cruce) %in% LISTA_TESTIGOS | 
                           toupper(as.character(if_exists(., "accion", ""))) == "T",
              
              suelo = suelo_val,
              num_sel = {
                raw <- as.character(if_exists(., "sel", if_exists(., "numero_de_seleccion", "0")))
                raw[is.na(raw) | raw == "-" | raw == ""] <- "0"
                as.character(raw)
              },
              brix = {
                raw_brix <- as.character(if_exists(., "brix", "0"))
                cleaned_brix <- gsub(",", ".", raw_brix)
                suppressWarnings(as.numeric(cleaned_brix))
              },
              vigor = {
                raw_vigor <- as.character(if_exists(., "agro", if_exists(., "vigor", "3")))
                raw_vigor[is.na(raw_vigor) | raw_vigor == "-" | raw_vigor == ""] <- "3"
                as.integer(raw_vigor)
              },
              raw_accion = toupper(trimws(as.character(if_exists(., "accion", "R")))),
              accion = case_when(
                raw_accion %in% c("S", "SELECCIONADO", "SELECCIONADA", "SELECCION") ~ "S",
                raw_accion %in% c("T", "TESTIGO") ~ "T",
                TRUE ~ "R"
              )
            ) %>%
            select(any_of(c("anio_seleccion", "anio_cruce", "programa", "suelo", "cruce", "num_sel", "brix", "vigor", "accion")))
            
            table_name <- paste0("clones_", tolower(st_dir))
            db_save_st_generic(con, table_name, df_proc)
          }
          message(sprintf("   [OK] Sincronizado: %s", basename(f)))
        }, error = function(e) {
          message(sprintf("   [ERROR] En archivo %s: %s", basename(f), e$message))
        })
      }
    }
  }
  return(TRUE)
}
# ------------------------------------------------------------------------------
# db_log_sync()
# ------------------------------------------------------------------------------
db_log_sync <- function(con, archivo, operacion, resultado, n = 0) {
  dbExecute(con, "INSERT INTO sync_logs (fecha_hora, archivo, operacion, resultado, registros_procesados, usuario) 
            VALUES (?, ?, ?, ?, ?, ?)",
            list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), archivo, operacion, resultado, n, "System"))
}

  # ------------------------------------------------------------------------------
  # FUNCIONES: registro_cruces
  # ------------------------------------------------------------------------------
  db_save_cruce <- function(con, madre, padre, programa, suelo, anio_cruce,
                            fecha_cruce = as.character(Sys.Date()),
                            semillas = NA, notas = "", tipo = "Biparental",
                            flores_m = NA, flores_p = NA) {
    # Detectar si es cruce repetido
    previo <- dbGetQuery(con,
                         "SELECT COUNT(*) as n FROM registro_cruces WHERE madre = ? AND padre = ?",
                         params = list(madre, padre))$n
    cruce_previo <- as.integer(previo > 0)
    
    # Cuando se registra un nuevo cruce, los gramos_restantes iniciales son iguales a los gramos totales (semillas)
    dbExecute(con,
              "INSERT INTO registro_cruces
     (fecha_cruce, madre, padre, programa, suelo, anio_cruce,
      semillas, gramos_restantes, estado, notas, cruce_previo, tipo, flores_m, flores_p)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'EJECUTADO', ?, ?, ?, ?, ?)",
              params = list(fecha_cruce, madre, padre, programa, suelo,
                            anio_cruce, semillas, semillas, notas, cruce_previo, tipo, flores_m, flores_p))
    
    invisible(cruce_previo)
  }
  
  db_update_germinacion <- function(con, id, germinadas) {
    semillas <- dbGetQuery(con,
                           "SELECT semillas FROM registro_cruces WHERE id = ?",
                           params = list(id))$semillas
    pct <- if (!is.na(semillas) && semillas > 0) round(germinadas / semillas * 100, 1) else NA
    dbExecute(con,
              "UPDATE registro_cruces
     SET germinadas = ?, pct_germinacion = ?, estado = 'GERMINADO'
     WHERE id = ?",
              params = list(germinadas, pct, id))
  }
  
  db_get_cruces <- function(con, anio = NULL, programa = NULL) {
    # Construir condiciones y parámetros separados (evita SQL injection)
    conds  <- c("1=1")
    params <- list()
    if (!is.null(anio)) {
      conds  <- c(conds, "anio_cruce = ?")
      params <- c(params, list(anio))
    }
    if (!is.null(programa)) {
      conds  <- c(conds, "programa = ?")
      params <- c(params, list(programa))
    }
    q <- paste("SELECT * FROM registro_cruces WHERE",
               paste(conds, collapse = " AND "),
               "ORDER BY fecha_cruce DESC")
    if (length(params) > 0) {
      dbGetQuery(con, q, params = params)
    } else {
      dbGetQuery(con, q)
    }
  }  
