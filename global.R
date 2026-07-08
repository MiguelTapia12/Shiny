# ==============================================================================
# GLOBAL.R — Configuración Global
# Pipeline de Selección Genética — Central Romana v2.0
# ==============================================================================
# Centraliza: librerías, carga de datos, generación de simulados,
# y source de módulos.
# ==============================================================================

# --- 0. DETECTAR CONTEXTO DE TESTING ---
.is_testing <- any(c(
  exists(".testing_mode", envir = .GlobalEnv, inherits = FALSE),
  isTRUE(getOption("testthat.running")),
  nzchar(Sys.getenv("TESTTHAT"))
))

options(shiny.autoload.r = FALSE)

if (!.is_testing) {
  # ── MODO PRODUCCIÓN (Carga completa e I/O) ────────────────────────────────
  
  # --- 1. LIBRERÍAS ---
  source("R/dependencies.R", local = FALSE, encoding = "UTF-8")
  load_dependencies()
  
  # --- 2. CARGAR MÓDULOS ---
  source("R/utils_pedigree.R",    local = FALSE, encoding = "UTF-8")
  source("R/utils_real_data.R",   local = FALSE, encoding = "UTF-8")
  source("R/utils_sim_data.R",    local = FALSE, encoding = "UTF-8")
  source("R/utils_selection.R",   local = FALSE, encoding = "UTF-8")
  source("R/utils_db.R",          local = FALSE, encoding = "UTF-8")
  source("R/utils_blup.R",        local = FALSE, encoding = "UTF-8")
  source("R/utils_lsmeans.R",     local = FALSE, encoding = "UTF-8")
  source("R/utils_iso.R",         local = FALSE, encoding = "UTF-8")
  source("R/utils_i18n.R",        local = FALSE, encoding = "UTF-8")
  source("R/etl_sync.R",          local = FALSE, encoding = "UTF-8")
  source("R/mod_genealogia.R",    local = FALSE, encoding = "UTF-8")
  source("R/mod_cruzamientos.R",  local = FALSE, encoding = "UTF-8")
  source("R/mod_banco_fuzz.R",      local = FALSE, encoding = "UTF-8")
  source("R/mod_estado_variedad.R",local = FALSE, encoding = "UTF-8")
  source("R/mod_seleccion.R",     local = FALSE, encoding = "UTF-8")
  source("R/mod_trazabilidad.R",  local = FALSE, encoding = "UTF-8")
  source("R/mod_archivo.R",       local = FALSE, encoding = "UTF-8")
  source("R/mod_dashboard.R",     local = FALSE, encoding = "UTF-8")
  source("R/mod_gerencial.R",     local = FALSE, encoding = "UTF-8")
  source("R/mod_floracion.R",     local = FALSE, encoding = "UTF-8")
  source("R/mod_asistente.R",     local = FALSE, encoding = "UTF-8")
  source("R/mod_fitopatologia.R", local = FALSE, encoding = "UTF-8")
  source("R/mod_inteligencia.R",  local = FALSE, encoding = "UTF-8")
  source("R/mod_home.R",          local = FALSE, encoding = "UTF-8")
  
  # --- 3. INICIALIZAR BASE DE DATOS ---
  backup_db <- function(db_path) {
    dir.create("backups", showWarnings = FALSE)
    dest <- paste0("backups/breeding_", Sys.Date(), ".db")
    if (!file.exists(dest)) {
      file.copy(db_path, dest)
      cat(">> Backup creado:", dest, "\n")
    } else {
      cat(">> Backup del dia ya existe:", dest, "\n")
    }
    old <- list.files("backups", full.names = TRUE, pattern = "\\.db$")
    if (length(old) > 30) file.remove(sort(old)[1:(length(old) - 30)])
  }
  
  backup_db("data/breeding_system.db")
  
  db_path <- "data/breeding_system.db"
  con <- db_connect(db_path)
  db_init_schema(con)
  
  # --- Cargar diccionario de traducciones en memoria (UNA sola vez) ---
  i18n_dict <- load_i18n("www/traducciones.csv")
  
  # Crear tabla de evaluación de enfermedades si no existe
  dbExecute(con, "CREATE TABLE IF NOT EXISTS evaluacion_enfermedades (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      ts DATETIME DEFAULT CURRENT_TIMESTAMP,
      fecha DATE,
      etapa TEXT,
      cruce TEXT,
      madre TEXT,
      padre TEXT,
      anio_seleccion INTEGER,
      programa TEXT,
      suelo TEXT,
      carbon_latigos INTEGER DEFAULT 0,
      carbon_herbaceas INTEGER DEFAULT 0,
      carbon_base_evaluacion INTEGER DEFAULT 0,
      carbon_porcentaje REAL DEFAULT 0.0,
      escaldadura_yemas_germinadas INTEGER DEFAULT 0,
      escaldadura_lineas_blancas INTEGER DEFAULT 0,
      escaldadura_base_evaluacion INTEGER DEFAULT 0,
      escaldadura_porcentaje REAL DEFAULT 0.0,
      roya_porcentaje REAL DEFAULT 0.0,
      usuario TEXT,
      observaciones TEXT
  )")
  
  # Migración: Agregar columnas de enfermedades si no existen (SQLite no soporta ADD COLUMN IF NOT EXISTS)
  try({
    dbExecute(con, "ALTER TABLE categorias ADD COLUMN carbon REAL")
    dbExecute(con, "ALTER TABLE categorias ADD COLUMN roya REAL")
    dbExecute(con, "ALTER TABLE categorias ADD COLUMN es REAL")
  }, silent = TRUE)
  
  # --- 4. CARGA Y OPTIMIZACIÓN DE DATOS (Fase 2) ---
  # Verificamos si ya hay datos procesados en la BD
  check_cat <- dbGetQuery(con, "SELECT COUNT(*) as n FROM catalogo")$n > 0
  check_ped <- dbGetQuery(con, "SELECT COUNT(*) as n FROM parentesco")$n > 0
  check_info <- dbGetQuery(con, "SELECT COUNT(*) as n FROM categorias")$n > 0
  
  db_has_data <- check_cat && check_ped && check_info
  
  if (db_has_data) {
    message(">> Cargando datos optimizados desde Base de Datos...")
    df_categorias <- dbReadTable(con, "categorias")
    df_familias   <- dbReadTable(con, "familias_evf") %>%
      # Renombrar de formato DB a formato Excel que esperan todos los módulos
      rename(
        ano      = anio,
        t_c_a    = tca,
        rend_96o = rend,
        t_a_a    = tsa
      ) %>%
      mutate(ano = as.character(ano))
    
    # Re-creamos df_act2025 minimal para compatibilidad con módulos legacy
    df_act2025 <- df_categorias %>% 
      select(variedad, factor, adapt, y, q, agro, disease, carbon, roya, es)
    
  } else {
    message(">> Datos incompletos. Ejecutando sincronizacion ETL automatica...")
    tryCatch({
      resultado <- run_full_etl_sync(
        con = con,
        allact_file   = "AllAct2025.xls",
        families_file = "Evaluacion de Familias.xlsx"
      )
      if (resultado$ok) {
        message(">> ETL completado: ", resultado$msg)
      } else {
        warning(">> ETL fallo, app iniciara con datos minimos: ", resultado$msg)
      }
    }, error = function(e) {
      warning(">> Error en carga inicial: ", e$message, ". La app iniciara en modo reducido.")
    })
    
    # Recargar datos desde BD (aunque esten vacios, permite que la app arranque)
    df_categorias <- tryCatch(dbReadTable(con, "categorias"), error = function(e) data.frame())
    df_familias   <- tryCatch({
      dbReadTable(con, "familias_evf") %>%
        rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
        mutate(ano = as.character(ano))
    }, error = function(e) data.frame())
    
    df_act2025 <- if (nrow(df_categorias) > 0) {
      df_categorias %>% select(any_of(c("variedad", "factor", "adapt", "y", "q", "agro", "disease", "carbon", "roya", "es")))
    } else {
      data.frame()
    }
  }
  
  # Cargar objetos globales desde la Base de Datos
  cat_var <- dbReadTable(con, "catalogo")
  pedigree_var <- dbReadTable(con, "parentesco")
  ebvs_var <- if(dbExistsTable(con, "ensayos_avanzados")) {
    dbGetQuery(con, "
      SELECT 
          v.variedad,
          COUNT(v.tca) as total_obs,
          AVG(v.tca - t.media_tca_exp) as ebv_tca,
          AVG(v.rendimiento - t.media_rend_exp) as ebv_rend,
          AVG(v.pureza - t.media_pureza_exp) as ebv_pureza,
          AVG(v.tsh - t.media_tsh_exp) as ebv_tsh,
          'Alta' as confianza
      FROM ensayos_avanzados v
      JOIN (
          SELECT num_experimento, 
                 AVG(tca) as media_tca_exp,
                 AVG(rendimiento) as media_rend_exp,
                 AVG(pureza) as media_pureza_exp,
                 AVG(tsh) as media_tsh_exp
          FROM ensayos_avanzados
          WHERE ind_testigo = 'S'
          GROUP BY num_experimento
      ) t ON v.num_experimento = t.num_experimento
      GROUP BY v.variedad
    ")
  } else {
    data.frame()
  }
  
  # Re-generar df_ped_wide para AGHmatrix
  df_ped_wide <- pedigree_var %>%
    filter(id_variedad != id_variedad_ancestro, 
           toupper(trimws(tipo_ancestro)) %in% c("PADRE", "MADRE")) %>%
    mutate(tipo_ancestro = toupper(trimws(tipo_ancestro))) %>%
    select(id_variedad, id_variedad_ancestro, tipo_ancestro) %>%
    group_by(id_variedad, tipo_ancestro) %>% slice(1) %>% ungroup() %>%
    # BUG 1 CORREGIDO: values_fill con nombre de columna evita NAs en padres/madres desconocidos
    pivot_wider(
      names_from  = tipo_ancestro,
      values_from = id_variedad_ancestro,
      values_fill = list(id_variedad_ancestro = "0")
    ) %>%
    clean_names()
  
  # BUG 2 CORREGIDO: reemplazar NAs con "0" (string) — AGHmatrix lo convertirá a entero
  if (!"padre" %in% names(df_ped_wide)) df_ped_wide$padre <- "0"
  if (!"madre" %in% names(df_ped_wide)) df_ped_wide$madre <- "0"
  
  df_ped_wide <- df_ped_wide %>%
    mutate(
      padre = if_else(is.na(padre), "0", padre),
      madre = if_else(is.na(madre), "0", madre)
    )
  
  # AGHmatrix requiere que todos los padres/madres existan como individuos en la tabla
  all_ids <- unique(c(df_ped_wide$id_variedad, df_ped_wide$padre, df_ped_wide$madre))
  all_ids <- all_ids[!is.na(all_ids) & all_ids != "0"]
  missing_ids <- setdiff(all_ids, df_ped_wide$id_variedad)
  
  if (length(missing_ids) > 0) {
    missing_df <- data.frame(
      id_variedad = missing_ids,
      padre = "0",
      madre = "0",
      stringsAsFactors = FALSE
    )
    # Fundadores van PRIMERO (requerimiento de AGHmatrix)
    df_ped_wide <- bind_rows(missing_df, df_ped_wide)
  }
  
  # NOTA: La Matriz A se construye aquí de forma GLOBAL para evitar que la app
  # se congele cada vez que el usuario presione "Simular Cruzamientos".
  # Se cachea en disco para evitar recálculo de 30s+ en cada inicio.
  a_matrix_file <- "data/a_matrix.rds"
  if (file.exists(a_matrix_file)) {
    message(">> Cargando Matriz de Parentesco desde caché (rápido)...")
    GLOBAL_A_MATRIX <- readRDS(a_matrix_file)
  } else {
    message(">> Calculando Matriz de Parentesco (AGHmatrix) globalmente... esto puede tomar un minuto...")
    ped_for_matrix <- df_ped_wide %>% select(id_variedad, padre, madre)
    ped_for_matrix$padre[is.na(ped_for_matrix$padre) | ped_for_matrix$padre == "0"] <- "0"
    ped_for_matrix$madre[is.na(ped_for_matrix$madre) | ped_for_matrix$madre == "0"] <- "0"
    
    GLOBAL_A_MATRIX <- tryCatch({
      mat <- AGHmatrix::Amatrix(data = as.data.frame(ped_for_matrix), ploidy = 2)
      saveRDS(mat, a_matrix_file)
      mat
    }, error = function(e) {
      message("Error en Matriz A: ", e$message)
      matrix(nrow = 0, ncol = 0)
    })
  }
  message(">> Matriz A lista.")
  # Cargar familias combinando Excel y Base de Datos
  df_familias_bd <- dbReadTable(con, "familias_evf")
  if (nrow(df_familias_bd) > 0) {
    df_familias_bd <- df_familias_bd %>%
      rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
      mutate(ano = as.character(ano))
  }
  
  # Usamos 'df_familias' que es el objeto cargado desde el Excel al inicio
  df_familias_final <- bind_rows(df_familias, df_familias_bd) %>%
    distinct(ano, experimento, cruce, .keep_all = TRUE)
  
  # Calcular categorias combinando Excel (metricas completas) y DB (notas/promociones)
  df_categorias_excel <- assign_genetic_categories(df_act2025, df_familias_final)
  categorias_bd <- dbReadTable(con, "categorias")
  
  df_categorias <- df_categorias_excel %>%
    left_join(categorias_bd %>% select(variedad, notas), by = "variedad")
  
  
  # Datos Simulados
  
  
  sim_rendimiento <- fread("data/sim/sim_rendimiento.csv")
  sim_enfermedades <- fread("data/sim/sim_enfermedades.csv")
  
  message(">> Sistema listo para iniciar en modo Producción.")
  
} else {
  # ── MODO TESTING (Solo funciones puras, sin I/O) ───────────────────────────
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(janitor)
    library(data.table)
  })
  
  source("R/utils_pedigree.R",    local = FALSE, encoding = "UTF-8")
  source("R/utils_real_data.R",   local = FALSE, encoding = "UTF-8")
  source("R/utils_selection.R",   local = FALSE, encoding = "UTF-8")
  
  message(">> Modo test: solo funciones puras y librerías base cargadas.")
}