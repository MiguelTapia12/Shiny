rm(list = ls())
ok <- TRUE
msg <- character()

tryCatch({
  source("global.R")
}, error = function(e) {
  ok <<- FALSE
  msg <<- c(msg, paste("ERROR source(global.R):", e$message))
})

if (!exists("con")) {
  ok <- FALSE
  msg <- c(msg, "ERROR: no existe objeto global 'con'")
}

if (ok) {
  cat("SMOKE_LOAD_OK\n")
  quit(status = 0)
} else {
  cat(paste(msg, collapse = "\n"), "\n")
  quit(status = 1)
}
