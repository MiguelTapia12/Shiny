library(plumber)
library(DBI)
library(RSQLite)
library(jsonlite)

source("R/utils_db.R")

get_conn <- function() {
  db_connect("C:/Proyectos/Shiny/data/breeding_system.db")
}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, ngrok-skip-browser-warning")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* @filter auth
function(req, res) {
  # Rutas públicas (no requieren token)
  rutas_publicas <- c("/api/ping", "/openapi.json")
  
  if (req$PATH_INFO %in% rutas_publicas || startsWith(req$PATH_INFO, "/__")) {
    plumber::forward()
  } else {
    # Validar el Token en el header Authorization
    auth_header <- req$HTTP_AUTHORIZATION
    expected_token <- Sys.getenv("BREEDING_API_TOKEN", "SugarCane2026Secure")
    
    if (is.null(auth_header)) {
      res$status <- 401
      return(list(status = "error", message = "Acceso denegado: Token de autorización no proporcionado. Envíe 'Authorization: Bearer <TOKEN>'"))
    }
    
    # Extraer el token (formato: "Bearer TOKEN")
    token_provided <- gsub("(?i)^Bearer\\s+", "", auth_header)
    
    if (token_provided == expected_token) {
      plumber::forward()
    } else {
      res$status <- 401
      return(list(status = "error", message = "Acceso denegado: Token inválido"))
    }
  }
}

#* @get /api/ping
function() {
  list(status = "ok", message = "Breeding API activa")
}

#* @post /api/selection/sync
#* @serializer json
#* @param req
function(req) {
  datos <- fromJSON(req$postBody, flatten = TRUE)
  con   <- get_conn()
  on.exit(dbDisconnect(con))
  
  if (nrow(datos) == 0) {
    return(list(status = "error", message = "No hay datos para sincronizar"))
  }
  
  # Añadimos marca de origen para saber que vino de la API (tablet)
  datos$origen <- "API"
  
  tryCatch({
    # Agrupar por etapa, porque en teoria podrian venir registros mezclados
    # aunque normalmente todo el lote es de la misma etapa
    etapas_presentes <- unique(datos$etapa)
    
    for (etp in etapas_presentes) {
      df_etp <- datos[datos$etapa == etp, ]
      
      # Si es ST1 a ST5, guardamos en clones_stX
      if (etp %in% paste0("ST", 1:5)) {
        table_name <- tolower(paste0("clones_", etp))
        
        # Eliminar registros previos de la API para este mismo contexto (evitar duplicados de sync)
        # El contexto es: anio_seleccion, programa, suelo, origen='API'
        # Asumimos que un lote entero comparte el mismo contexto (como viene configurado en la app)
        anio_sel <- df_etp$anio_seleccion[1]
        prog     <- df_etp$programa[1]
        suelo    <- df_etp$suelo[1]
        
        q_del <- sprintf(
          "DELETE FROM %s WHERE anio_seleccion = ? AND programa = ? AND suelo = ? AND origen = 'API'", 
          table_name
        )
        dbExecute(con, q_del, params = list(anio_sel, prog, suelo))
        
        # Remover columna 'ts' (timestamp de captura local) y la misma 'etapa' si no existen en la BD
        cols_to_save <- intersect(
          names(df_etp),
          c("anio_seleccion", "anio_cruce", "programa", "suelo", "cruce", "num_sel", "brix", "vigor", "accion", "origen")
        )
        df_save <- df_etp[, cols_to_save, drop = FALSE]
        
        dbWriteTable(con, table_name, df_save, append = TRUE, row.names = FALSE)
        
        # Link: Automáticamente actualizar registro_cruces a EN_EVALUACION para ST1
        if (etp == "ST1") {
          cruces_promovidos <- unique(df_save$cruce)
          if (length(cruces_promovidos) > 0) {
            cruces_str <- paste(sprintf("'%s'", cruces_promovidos), collapse = ",")
            tryCatch({
              dbExecute(con, sprintf("UPDATE registro_cruces SET estado = 'EN_EVALUACION' WHERE cruce IN (%s) AND estado != 'EN_EVALUACION'", cruces_str))
            }, error = function(e) NULL)
          }
        }
        
      } else {
        # Si es EVF u otra cosa, lo guardamos en field_captures (staging general)
        dbWriteTable(con, "field_captures", df_etp, append = TRUE, row.names = FALSE)
      }
    }
    
    list(
      status                  = "ok",
      message                 = "Datos procesados y guardados en la BD",
      registros_sincronizados = nrow(datos)
    )
  }, error = function(e) {
    list(status = "error", message = as.character(e))
  })
}

#* @get /api/field-status
#* @param etapa
#* @param suelo
#* @param anio
function(etapa = "ST1", suelo = "BUENO", anio = 2026) {
  con <- get_conn()
  on.exit(dbDisconnect(con))
  
  if (etapa %in% paste0("ST", 1:5)) {
    table_name <- tolower(paste0("clones_", etapa))
    q <- sprintf(
      "SELECT COUNT(*) as n FROM %s WHERE anio_seleccion = ? AND suelo = ? AND origen = 'API'",
      table_name
    )
    n_regs <- tryCatch(dbGetQuery(con, q, params = list(as.integer(anio), suelo))$n, error = function(e) 0)
    
    list(
      status = "ok",
      n_registros = n_regs,
      mensaje = sprintf("%d registros capturados via API", n_regs)
    )
  } else {
    list(status = "error", message = "Etapa no soportada para status de campo")
  }
}

#* @get /api/floracion-master
#* @param temporada
function(temporada = NULL) {
  con <- get_conn()
  on.exit(dbDisconnect(con))
  
  if (is.null(temporada)) {
    temporada <- as.integer(format(Sys.Date(), "%Y"))
  }
  
  q <- sprintf(
    "SELECT variedad, sec, num, calles, sx, adapt
     FROM floracion_master
     WHERE temporada = %d
     ORDER BY sec, num",
    as.integer(temporada)
  )
  
  datos <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
  
  if (nrow(datos) == 0) {
    list(status = "empty", message = "No hay datos de floración para esta temporada.", data = list())
  } else {
    list(status = "ok", total = nrow(datos), data = datos)
  }
}

#* @post /api/floracion-sync
#* @param req
function(req) {
  con <- get_conn()
  on.exit(dbDisconnect(con))
  
  datos <- tryCatch(
    jsonlite::fromJSON(req$postBody, flatten = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(datos) || nrow(datos) == 0) {
    return(list(status = "error", message = "No se recibieron datos válidos."))
  }
  
  tryCatch({
    dbWriteTable(con, "floracion_chequeos", datos,
                 append = TRUE, row.names = FALSE)
    list(
      status                  = "ok",
      message                 = "Chequeos recibidos correctamente.",
      registros_sincronizados = nrow(datos)
    )
  }, error = function(e) {
    list(status = "error", message = as.character(e))
  })
}

#* @get /api/floracion-activos
#* @param fecha Fecha del chequeo de referencia (YYYY-MM-DD). Si se omite, usa el lunes mas reciente.
#* @param sec   Seccion a filtrar (A, B, C...). Si se omite, devuelve todas.
function(fecha = NULL, sec = NULL) {
  con <- get_conn()
  on.exit(dbDisconnect(con))
  
  # Si no se pasa fecha, calcular el lunes mas reciente
  if (is.null(fecha) || !nzchar(fecha)) {
    hoy   <- Sys.Date()
    lunes <- hoy - (as.integer(format(hoy, "%u")) - 1)
    fecha <- as.character(lunes)
  }
  
  filtro_sec <- if (!is.null(sec) && nzchar(sec)) {
    sprintf("AND c.sec = '%s'", sec)
  } else ""
  
  q <- sprintf("
    SELECT
      c.fecha_chequeo,
      c.dia_semana,
      c.sec,
      c.num,
      c.variedad,
      COALESCE(c.sx, m.sx)   AS sx,
      m.calles,
      m.adapt,
      c.grc_atrasado,
      c.grc_emergencia,
      c.grc_adelantado,
      c.pct_polen,
      c.evaluador,
      c.temporada
    FROM floracion_chequeos c
    LEFT JOIN floracion_master m
      ON c.num = m.num AND c.temporada = m.temporada
    WHERE c.fecha_chequeo = '%s'
      AND (c.grc_atrasado > 0 OR c.grc_emergencia > 0 OR c.grc_adelantado > 0)
      %s
    ORDER BY c.sec, c.num
  ", fecha, filtro_sec)
  
  datos <- tryCatch(dbGetQuery(con, q), error = function(e) data.frame())
  
  if (nrow(datos) == 0) {
    list(
      status  = "empty",
      message = paste("No hay posiciones activas para la fecha", fecha),
      fecha   = fecha,
      data    = list()
    )
  } else {
    list(
      status  = "ok",
      fecha   = fecha,
      total   = nrow(datos),
      data    = datos
    )
  }
}