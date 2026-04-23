rm(list = ls())
source("global.R")

required <- list(
  familias_evf = c("anio", "cruce", "madre", "padre"),
  promociones = c("clon_origen", "nombre_cr", "suelo", "fecha"),
  clones_st1 = c("anio_seleccion", "cruce", "num_sel")
)

failed <- character()

for (tbl in names(required)) {
  info <- tryCatch(
    DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tbl)),
    error = function(e) NULL
  )
  if (is.null(info) || nrow(info) == 0) {
    failed <- c(failed, paste("Tabla no accesible:", tbl))
    next
  }
  cols <- info$name
  miss <- setdiff(required[[tbl]], cols)
  if (length(miss) > 0) {
    failed <- c(failed, paste(tbl, "faltan:", paste(miss, collapse = ", ")))
  }
}

if (length(failed) == 0) {
  cat("SMOKE_COLUMNS_OK\n")
  quit(status = 0)
} else {
  cat(paste(failed, collapse = "\n"), "\n")
  quit(status = 1)
}
