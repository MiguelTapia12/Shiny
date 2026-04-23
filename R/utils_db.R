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
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
  dbConnect(RSQLite::SQLite(), db_path)
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
      accion TEXT
    )", st)
    dbExecute(con, query)
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
  
  anio_sel <- unique(df_selected$anio_seleccion)[1]
  prog_val <- unique(df_selected$programa)[1]
  suelo_val <- unique(df_selected$suelo)[1]
  
  # Limpiar solo el lote que estamos subiendo (Año Seleccion/Programa/Suelo)
  dbExecute(con, sprintf("DELETE FROM %s WHERE anio_seleccion = ? AND programa = ? AND suelo = ?", table_name), 
            params = list(anio_sel, prog_val, suelo_val))
  
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
          # Identificar Suelo desde el nombre (ej. ROCKY, GOOD)
          suelo_val <- if (grepl("_ROCKY", basename(f), ignore.case = TRUE)) "Rocoso" else "Bueno"
          
          df <- readxl::read_excel(f) %>% janitor::clean_names()
          
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
              es_testigo = toupper(cruce) %in% LISTA_TESTIGOS,
              # ------------------------------
              suelo = suelo_val,
              num_sel = {
                raw <- as.character(if_exists(., "numero_de_seleccion", "0"))
                raw[is.na(raw) | raw == "-"] <- "0"
                as.integer(raw)
              },
              brix = as.numeric(as.character(if_exists(., "brix", 0))),
              vigor = as.integer(as.character(if_exists(., "vigor", 3))),
              accion_campo = tolower(as.character(if_exists(., "accion", "rechazado"))),
              accion = ifelse(grepl("selecc", accion_campo), "S", "R")
            ) %>%
            filter(accion == "S") %>% 
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
