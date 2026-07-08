# ==============================================================================
# Script de Verificación de Salud del Sistema (Health Check)
# ==============================================================================
# Este script verifica que las dependencias, la base de datos y la API
# estén configuradas correctamente para que el sistema funcione.
#
# Uso: Rscript health_check.R
# ==============================================================================

cat("\n=========================================================\n")
cat("Iniciando Verificacion de Salud (Health Check) del Sistema\n")
cat("=========================================================\n\n")

status_all <- TRUE

# 1. Verificar paquetes requeridos
cat("[1] Verificando dependencias de R...\n")
required_packages <- c("shiny", "RSQLite", "DBI", "dplyr", "plumber")
missing_packages <- c()

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("  [FAIL] Faltan los siguientes paquetes: ", paste(missing_packages, collapse = ", "), "\n")
  status_all <- FALSE
} else {
  cat("  [OK] Todos los paquetes principales estan instalados.\n")
}

# 2. Verificar base de datos
cat("\n[2] Verificando conexion a la base de datos...\n")
db_path <- "database.sqlite"
if (file.exists(db_path)) {
  cat("  [OK] Archivo database.sqlite encontrado.\n")
  
  con <- tryCatch({
    DBI::dbConnect(RSQLite::SQLite(), db_path)
  }, error = function(e) NULL)
  
  if (!is.null(con)) {
    cat("  [OK] Conexion a SQLite exitosa.\n")
    
    # Verificar tablas clave
    required_tables <- c("familias_evf", "ensayos_avanzados", "field_captures", "variedades")
    tables_in_db <- DBI::dbListTables(con)
    missing_tables <- setdiff(required_tables, tables_in_db)
    
    if (length(missing_tables) > 0) {
      cat("  [WARN] Faltan algunas tablas esperadas: ", paste(missing_tables, collapse = ", "), "\n")
    } else {
      cat("  [OK] Tablas principales verificadas.\n")
    }
    
    DBI::dbDisconnect(con)
  } else {
    cat("  [FAIL] No se pudo conectar a la base de datos.\n")
    status_all <- FALSE
  }
} else {
  cat("  [FAIL] Archivo database.sqlite no existe en la raiz.\n")
  status_all <- FALSE
}

# 3. Verificar API
cat("\n[3] Verificando archivos de la API...\n")
if (file.exists("plumber.R")) {
  cat("  [OK] Archivo plumber.R (API) encontrado.\n")
} else {
  cat("  [FAIL] Archivo plumber.R no encontrado.\n")
  status_all <- FALSE
}

# 4. Verificar modulos principales
cat("\n[4] Verificando modulos de la aplicacion...\n")
modules <- c("R/mod_home.R", "R/mod_gerencial.R", "R/mod_seleccion.R", "R/mod_cruzamientos.R")
missing_mods <- c()
for (mod in modules) {
  if (!file.exists(mod)) missing_mods <- c(missing_mods, mod)
}

if (length(missing_mods) > 0) {
  cat("  [FAIL] Faltan modulos: ", paste(missing_mods, collapse = ", "), "\n")
  status_all <- FALSE
} else {
  cat("  [OK] Archivos de modulos principales encontrados.\n")
}

cat("\n=========================================================\n")
if (status_all) {
  cat("RESULTADO: [SUCCESS] El sistema parece estar sano y listo.\n")
} else {
  cat("RESULTADO: [WARNING] El sistema tiene problemas que requieren atencion.\n")
}
cat("=========================================================\n")

