load_dependencies <- function() {
  pkgs <- c(
    "shiny",
    "shinydashboard",
    "kinship2",
    "dplyr",
    "data.table",
    "janitor",
    "tidyr",
    "DT",
    "AGHmatrix",
    "scales",
    "visNetwork",
    "readxl",
    "ggplot2",
    "openxlsx",
    "plotly",
    "RSQLite",
    "DBI",
    "stringr",
    "digest"
  )

  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Faltan paquetes requeridos: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}
