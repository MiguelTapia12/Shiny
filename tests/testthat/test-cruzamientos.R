library(testthat)
library(dplyr)

test_that("F_progenie se calcula correctamente desde matriz A", {
  A <- matrix(
    c(1.0, 0.5, 0.5,
      0.5, 1.0, 0.0,
      0.5, 0.0, 1.0),
    nrow = 3, ncol = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  comb <- data.frame(
    Madre_ID = c("A", "A"),
    Padre_ID = c("B", "C"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(F_progenie = A[cbind(Madre_ID, Padre_ID)] / 2)

  expect_equal(comb$F_progenie[1], 0.25)
  expect_equal(comb$F_progenie[2], 0.25)
})

test_that("expand.grid no genera auto-cruces tras filtrar", {
  ids <- c("VAR1", "VAR2", "VAR3")
  comb <- expand.grid(Madre_ID = ids, Padre_ID = ids, stringsAsFactors = FALSE) %>%
    filter(Madre_ID != Padre_ID)

  expect_false(any(comb$Madre_ID == comb$Padre_ID))
})

test_that("filtro por F máximo elimina cruces con alta consanguinidad", {
  comb <- data.frame(
    Madre = c("A", "B", "C"),
    Padre = c("B", "C", "A"),
    F_progenie = c(0.03, 0.10, 0.30),
    stringsAsFactors = FALSE
  )

  resultado <- comb %>% filter(F_progenie <= 0.0625)
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$Madre, "A")
})

