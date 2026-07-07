library(testthat)
library(dplyr)
library(tidyr)
library(janitor)

# Simula lo que construye global.R con datos controlados
build_ped_wide <- function(pedigree_df) {
  pedigree_df |>
    dplyr::filter(id_variedad != id_variedad_ancestro) |>
    dplyr::mutate(
      tipo_ancestro = toupper(trimws(tipo_ancestro))
    ) |>
    dplyr::filter(tipo_ancestro %in% c("PADRE", "MADRE")) |>
    dplyr::distinct(id_variedad, tipo_ancestro, .keep_all = TRUE) |>
    tidyr::pivot_wider(
      names_from   = tipo_ancestro,
      values_from  = id_variedad_ancestro,
      values_fill  = "0"
    ) |>
    janitor::clean_names()
}

test_that("pivot_wider produce columnas padre y madre con datos válidos", {
  ped <- data.frame(
    id_variedad          = c("100", "100"),
    id_variedad_ancestro = c("200", "300"),
    tipo_ancestro        = c("PADRE", "MADRE"),
    stringsAsFactors     = FALSE
  )
  result <- build_ped_wide(ped)
  expect_true("padre" %in% names(result))
  expect_true("madre" %in% names(result))
})

test_that("tipos con error tipográfico son filtrados silenciosamente", {
  ped <- data.frame(
    id_variedad          = c("100", "100", "100"),
    id_variedad_ancestro = c("200", "300", "400"),
    tipo_ancestro        = c("PADRE", "MADRE", "padre "),  # espacio y minúscula
    stringsAsFactors     = FALSE
  )
  result <- build_ped_wide(ped)
  # "padre " normalizado a "PADRE" debe unirse con el PADRE real → solo 1 fila
  expect_equal(nrow(result), 1L)
})

test_that("auto-referencias no aparecen en el pedigrí ancho", {
  ped <- data.frame(
    id_variedad          = c("100", "100"),
    id_variedad_ancestro = c("100", "300"),  # auto-referencia en padre
    tipo_ancestro        = c("PADRE", "MADRE"),
    stringsAsFactors     = FALSE
  )
  result <- build_ped_wide(ped)
  expect_false("100" %in% result$padre)
})

test_that("individuo sin padres registrados recibe '0' en ambas columnas", {
  ped <- data.frame(
    id_variedad          = character(),
    id_variedad_ancestro = character(),
    tipo_ancestro        = character(),
    stringsAsFactors     = FALSE
  )
  result <- build_ped_wide(ped)
  expect_equal(nrow(result), 0L)  # tabla vacía, no error
})
