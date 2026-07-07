suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(plotly))
source("C:/Proyectos/Shiny/R/mod_seleccion.R")

cat("--- GENERATING UI ---\n")
ui <- mod_seleccion_ui("test")
cat("SUCCESS. LENGTH OF UI:", length(as.character(ui)), "\n")
