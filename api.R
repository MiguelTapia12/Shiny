# ==============================================================================
# API.R — API REST de Sincronización para Captura de Campo (Central Romana)
# Basado en el paquete Plumber (R REST Framework)
# ==============================================================================

#* @apiTitle Central Romana Breeding System REST API
#* @apiDescription Servicio web de sincronización en tiempo real para tablets de campo (Selecciones y Floración).
#* @apiVersion 1.0.0

library(RSQLite)
library(jsonlite)
library(dplyr)

# ==============================================================================
# CORS GLOBAL FILTER — CLAVE para que tablets y móviles en CUALQUIER red puedan
# comunicarse con este servidor. Sin esto el navegador bloquea silenciosamente
# todas las peticiones fetch() de la app de captura.
# ==============================================================================
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, ngrok-skip-browser-warning")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# Helper: Establecer conexión con la base de datos SQLite del sistema
get_db_conn <- function() {
  db_path <- "data/breeding_system.db"
  if (!file.exists(db_path)) {
    # Buscar en ruta alternativa si es ejecutado en subdirectorios
    db_path <- "../data/breeding_system.db"
  }
  dbConnect(SQLite(), db_path)
}

# Helper interno para campos inexistentes en el JSON
if_exists_api <- function(df, col_name, default_val) {
  if (col_name %in% names(df)) return(df[[col_name]])
  return(rep(default_val, nrow(df)))
}

# ==============================================================================
# AUTH FILTER — Protege todos los endpoints de escritura (POST).
# GET /api/ping y GET /api/pedigree quedan abiertos (las tablets los usan
# para iniciar sesión y cargar pedigríes sin necesitar la clave).
# ==============================================================================
#* @filter auth
function(req, res) {
  # Solo proteger métodos de escritura
  if (req$REQUEST_METHOD %in% c("POST", "PUT", "DELETE")) {
    expected_token <- Sys.getenv("BREEDING_API_TOKEN")
    if (nchar(expected_token) == 0) {
      expected_token <- "SugarCane2026Secure"
    }
    # Leer el header Authorization: Bearer <token>
    auth_header <- req$HTTP_AUTHORIZATION
    if (is.null(auth_header)) auth_header <- ""
    provided_token <- sub("(?i)^Bearer\\s+", "", auth_header)
    
    cat(sprintf("[DEBUG API] Ruta: %s | Auth Header: '%s' | Token: '%s' | Expected: '%s'\n", req$PATH_INFO, auth_header, provided_token, expected_token))
    
    if (!identical(provided_token, expected_token)) {
      res$status <- 401
      return(list(status = "error", message = "Acceso no autorizado. Token inválido o ausente."))
    }
  }
  plumber::forward()
}


#* ------------------------------------------------------------------------------
#* @get /api/ping
#* @serializer json
#* Verificar estado de la API y conectividad con la base de datos SQLite
#* ------------------------------------------------------------------------------
function() {
  con <- tryCatch({
    get_db_conn()
  }, error = function(e) NULL)
  
  if (is.null(con)) {
    return(list(
      status = "error", 
      message = "API en línea pero no se pudo establecer conexión con la base de datos SQLite."
    ))
  }
  
  dbDisconnect(con)
  return(list(
    status = "ok", 
    message = "API REST de Central Romana conectada a la Base de Datos con éxito.",
    timestamp = as.character(Sys.time())
  ))
}

#* ------------------------------------------------------------------------------
#* @get /api/pedigree
#* @serializer json
#* Obtener base genealógica de EVF para autocompletado Offline en tablets de campo
#* ------------------------------------------------------------------------------
function() {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))
  
  # Traer genealogía histórica para que la tablet valide los cruces sin internet
  df <- dbGetQuery(con, "SELECT DISTINCT anio, cruce, madre, padre FROM familias_evf")
  return(df)
}

#* ------------------------------------------------------------------------------
#* @post /api/selection/sync
#* @serializer json
#* Sincronizar un lote de evaluaciones de clones desde tablets (Formato JSON)
#* ------------------------------------------------------------------------------
function(req) {
  payload <- tryCatch({
    jsonlite::fromJSON(req$postBody)
  }, error = function(e) NULL)
  
  if (is.null(payload) || nrow(payload) == 0) {
    return(list(status = "error", message = "Payload vacío o JSON no válido."))
  }
  
  # Validar metadatos requeridos para ubicar la etapa de destino
  if (!"etapa" %in% names(payload)) {
    return(list(status = "error", message = "Falta especificar la clave 'etapa' (ej. ST1, ST2) en los registros."))
  }
  
  etapa <- unique(payload$etapa)[1]
  tbl_name <- tolower(paste0("clones_", etapa))
  
  # Estructurar columnas base requeridas para persistir en SQLite
  required_cols <- c("anio_seleccion", "anio_cruce", "programa", "suelo", "cruce", "num_sel", "brix", "vigor", "accion")
  missing <- setdiff(required_cols, names(payload))
  if (length(missing) > 0) {
    return(list(
      status = "error", 
      message = paste("Faltan campos obligatorios en el lote:", paste(missing, collapse = ", "))
    ))
  }
  
  # Cast y limpieza estricta de tipos de datos
  # Asegurar que existan para no romper mutate
  if (!"evaluador" %in% names(payload)) payload$evaluador <- "Desconocido"
  if (!"ts" %in% names(payload)) payload$ts <- as.character(Sys.time())
  if (!"latitud" %in% names(payload)) payload$latitud <- NA_real_
  if (!"longitud" %in% names(payload)) payload$longitud <- NA_real_
  
  df_to_save <- payload[, required_cols, drop = FALSE]
  df_to_save$evaluador <- payload$evaluador
  df_to_save$latitud <- payload$latitud
  df_to_save$longitud <- payload$longitud
  df_to_save$fecha_evaluacion <- payload$ts
  df_to_save$origen <- "API"

  df_to_save <- df_to_save %>%
    mutate(
      anio_seleccion = as.integer(anio_seleccion),
      anio_cruce = as.integer(anio_cruce),
      programa = toupper(trimws(as.character(programa))),
      programa = ifelse(grepl("^BR", programa), "BR", ifelse(grepl("^CR", programa), "CR", programa)),
      suelo = toupper(trimws(as.character(suelo))),
      cruce = trimws(as.character(cruce)),
      num_sel = as.integer(num_sel),
      brix = as.numeric(brix),
      vigor = as.integer(vigor),
      accion = toupper(trimws(as.character(accion))),
      evaluador = trimws(as.character(evaluador)),
      latitud = as.numeric(latitud),
      longitud = as.numeric(longitud),
      origen = "API"
    )
  
  con <- get_db_conn()
  on.exit(dbDisconnect(con))
  
  # Evitar duplicaciones en sincronización masiva: limpiar lote previo idéntico (Año/Suelo/Programa) solo de la API
  anio_sel <- unique(df_to_save$anio_seleccion)[1]
  suelo_sel <- unique(df_to_save$suelo)[1]
  prog_sel <- unique(df_to_save$programa)[1]
  
  dbExecute(con, sprintf("DELETE FROM %s WHERE anio_seleccion = ? AND suelo = ? AND programa = ? AND origen = 'API'", tbl_name),
            params = list(anio_sel, suelo_sel, prog_sel))
  
  # Inserción atómica en base de datos
  dbWriteTable(con, tbl_name, df_to_save, append = TRUE)
  
  # Escribir log en el historial de sincronizaciones
  dbExecute(con, "INSERT INTO sync_logs (fecha_hora, archivo, operacion, resultado, registros_procesados, usuario) VALUES (?, ?, ?, ?, ?, ?)",
            params = list(as.character(Sys.time()), paste0("API_SYNC_", etapa), "API_UPLOAD", "SUCCESS", nrow(df_to_save), "Tablet_Field"))
  
  return(list(
    status = "ok",
    message = sprintf("Sincronización exitosa. Guardados %d clones en %s.", nrow(df_to_save), tbl_name),
    registros_sincronizados = nrow(df_to_save)
  ))
}

#* ------------------------------------------------------------------------------
#* @post /api/crossings/sync
#* @serializer json
#* Capturar datos de floración y montaje de cruzamientos en tiempo real desde el campo
#* ------------------------------------------------------------------------------
function(req) {
  payload <- tryCatch({
    jsonlite::fromJSON(req$postBody)
  }, error = function(e) NULL)
  
  if (is.null(payload) || !is.data.frame(payload) || nrow(payload) == 0) {
    return(list(status = "error", message = "Payload vacío o JSON no válido."))
  }

  # Validar campos obligatorios
  required_cross <- c("cruce", "madre", "padre", "estado_floracion")
  missing_cross  <- setdiff(required_cross, names(payload))
  if (length(missing_cross) > 0) {
    return(list(
      status  = "error",
      message = paste("Faltan campos obligatorios en floración:", paste(missing_cross, collapse = ", "))
    ))
  }

  con <- get_db_conn()
  on.exit(dbDisconnect(con))

  # Estandarizar columnas y rellenar defaults
  df_save <- payload %>%
    mutate(
      fecha_registro    = as.character(if_exists_api(., "fecha_registro", as.character(Sys.Date()))),
      cruce             = trimws(as.character(cruce)),
      madre             = trimws(as.character(madre)),
      padre             = trimws(as.character(padre)),
      estado_floracion  = trimws(as.character(estado_floracion)),
      cantidad_flores   = as.integer(if_exists_api(., "cantidad_flores", 1)),
      comentarios       = as.character(if_exists_api(., "comentarios", ""))
    )

  # Escribir en base de datos
  dbWriteTable(con, "floracion_campo", df_save, append = TRUE)
  
  # Loguear evento
  dbExecute(con, "INSERT INTO sync_logs (fecha_hora, archivo, operacion, resultado, registros_procesados, usuario) VALUES (?, ?, ?, ?, ?, ?)",
            params = list(as.character(Sys.time()), "API_SYNC_FLORACION", "API_UPLOAD", "SUCCESS", nrow(df_save), "Tablet_Field"))
  
  return(list(
    status = "ok",
    message = sprintf("Sincronización de floración exitosa. Guardados %d registros de cruzamiento.", nrow(df_save)),
    registros_sincronizados = nrow(df_save)
  ))
}

#* ------------------------------------------------------------------------------
#* @post /api/sanidad/sync
#* @serializer json
#* Sincronizar evaluaciones fitopatológicas (Carbón, Escaldadura, Roya) desde tablets
#* ------------------------------------------------------------------------------
function(req) {
  payload <- tryCatch({
    jsonlite::fromJSON(req$postBody)
  }, error = function(e) NULL)
  
  if (is.null(payload) || !is.data.frame(payload) || nrow(payload) == 0) {
    return(list(status = "error", message = "Payload vacío o JSON no válido."))
  }

  # Validar campos obligatorios de fitopatología
  required_sanidad <- c("etapa", "cruce")
  missing_sanidad  <- setdiff(required_sanidad, names(payload))
  if (length(missing_sanidad) > 0) {
    return(list(
      status  = "error",
      message = paste("Faltan campos obligatorios en sanidad:", paste(missing_sanidad, collapse = ", "))
    ))
  }

  con <- get_db_conn()
  on.exit(dbDisconnect(con))

  
  # Preparar datos
  df_save <- payload %>%
    mutate(
      fecha = as.character(if_exists_api(., "fecha", as.character(Sys.Date()))),
      etapa = toupper(trimws(as.character(etapa))),
      cruce = trimws(as.character(cruce)),
      madre = trimws(as.character(if_exists_api(., "madre", ""))),
      padre = trimws(as.character(if_exists_api(., "padre", ""))),
      anio_seleccion = as.integer(if_exists_api(., "anio_seleccion", format(Sys.Date(), "%Y"))),
      programa = toupper(trimws(as.character(if_exists_api(., "programa", "")))),
      suelo = toupper(trimws(as.character(if_exists_api(., "suelo", "")))),
      
      carbon_latigos = as.integer(if_exists_api(., "carbon_latigos", 0)),
      carbon_herbaceas = as.integer(if_exists_api(., "carbon_herbaceas", 0)),
      carbon_base_evaluacion = as.integer(if_exists_api(., "carbon_base_evaluacion", 1)),
      
      escaldadura_yemas_germinadas = as.integer(if_exists_api(., "escaldadura_yemas_germinadas", 0)),
      escaldadura_lineas_blancas = as.integer(if_exists_api(., "escaldadura_lineas_blancas", 0)),
      escaldadura_base_evaluacion = as.integer(if_exists_api(., "escaldadura_base_evaluacion", 1)),
      
      roya_porcentaje = as.numeric(if_exists_api(., "roya_porcentaje", 0.0)),
      
      usuario = as.character(if_exists_api(., "usuario", "Tablet_Field")),
      observaciones = as.character(if_exists_api(., "observaciones", ""))
    ) %>%
    mutate(
      # Calcular porcentajes automáticamente
      carbon_porcentaje = round(((carbon_latigos + carbon_herbaceas) / ifelse(carbon_base_evaluacion == 0, 1, carbon_base_evaluacion)) * 100, 2),
      escaldadura_porcentaje = round(((escaldadura_yemas_germinadas + escaldadura_lineas_blancas) / ifelse(escaldadura_base_evaluacion == 0, 1, escaldadura_base_evaluacion)) * 100, 2)
    )
  
  # Escribir en base de datos
  dbWriteTable(con, "evaluacion_enfermedades", df_save, append = TRUE)
  
  # Loguear evento
  dbExecute(con, "INSERT INTO sync_logs (fecha_hora, archivo, operacion, resultado, registros_procesados, usuario) VALUES (?, ?, ?, ?, ?, ?)",
            params = list(as.character(Sys.time()), "API_SYNC_SANIDAD", "API_UPLOAD", "SUCCESS", nrow(df_save), "Tablet_Field"))
  
  return(list(
    status = "ok",
    message = sprintf("Sincronización de fitopatología exitosa. Guardados %d registros.", nrow(df_save)),
    registros_sincronizados = nrow(df_save)
  ))
}
