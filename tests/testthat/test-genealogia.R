library(testthat)
library(dplyr)

# Nota: R/utils_pedigree.R ya es cargado por global.R en modo test, 
# pero lo cargamos aqui por claridad si se corre el test solo.
if (!exists("get_full_ancestry_robust")) source("../../R/utils_pedigree.R")

test_that("get_full_ancestry_robust retorna data.frame con columnas base", {
  result <- get_full_ancestry_robust(ped_minimal(), "100", cat_minimal())
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "dadid", "momid") %in% names(result)))
})

test_that("incluye individuo objetivo y ambos padres", {
  result <- get_full_ancestry_robust(ped_minimal(), "100", cat_minimal())
  expect_true("100" %in% result$id)
  expect_true("200" %in% result$id)
  expect_true("300" %in% result$id)
})

test_that("no produce filas duplicadas por id", {
  result <- get_full_ancestry_robust(ped_minimal(), "100", cat_minimal())
  expect_equal(nrow(result), dplyr::n_distinct(result$id))
})

test_that("maneja fundador sin padres conocidos", {
  result <- get_full_ancestry_robust(ped_minimal(), "200", cat_minimal())
  expect_true("200" %in% result$id)
})

test_that("ID inexistente no rompe y retorna estructura válida", {
  result <- get_full_ancestry_robust(ped_minimal(), "999", cat_minimal())
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)
})
