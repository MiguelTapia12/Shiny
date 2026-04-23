rm(list = ls())
source("global.R")

required_fns <- c("mod_seleccion_ui", "mod_seleccion_server")
missing <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]

if (length(missing) > 0) {
  cat("Faltan funciones:", paste(missing, collapse = ", "), "\n")
  quit(status = 1)
}

cat("SMOKE_MOD_SELECCION_OK\n")
quit(status = 0)
