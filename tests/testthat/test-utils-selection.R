library(testthat)

# Matriz A de prueba
A_test <- matrix(
  c(1.00, 0.50, 0.10, 0.00,
    0.50, 1.00, 0.00, 0.05,
    0.10, 0.00, 1.00, 0.50,
    0.00, 0.05, 0.50, 1.00),
  nrow = 4, ncol = 4,
  dimnames = list(c("A","B","C","D"), c("A","B","C","D"))
)

cat_test <- data.frame(
  id_variedad          = c("A","B","C","D"),
  descripcion_variedad = c("Var-Alpha","Var-Beta","Var-Gamma","Var-Delta"),
  stringsAsFactors     = FALSE
)

test_that("sugerir_cruces retorna data.frame con columnas esperadas", {
  res <- sugerir_cruces(A_test, c("A","B","C","D"), max_f = 0.25, cat_var = cat_test)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Madre_ID","Padre_ID","F_progenie","Madre","Padre") %in% names(res)))
})

test_that("no incluye cruces de un individuo consigo mismo", {
  res <- sugerir_cruces(A_test, c("A","B","C","D"), max_f = 0.25)
  expect_false(any(res$Madre_ID == res$Padre_ID))
})

test_that("respeta el umbral max_f", {
  res <- sugerir_cruces(A_test, c("A","B","C","D"), max_f = 0.10)
  expect_true(all(res$F_progenie <= 0.10))
})

test_that("resultado está ordenado por F_progenie ascendente", {
  res <- sugerir_cruces(A_test, c("A","B","C","D"), max_f = 0.25)
  expect_equal(res$F_progenie, sort(res$F_progenie))
})

test_that("retorna tabla vacía si ningún cruce pasa el umbral", {
  res <- sugerir_cruces(A_test, c("A","B"), max_f = 0.001)
  expect_equal(nrow(res), 0L)
})

test_that("filtro de adaptación reduce candidatos correctamente", {
  cats <- data.frame(
    variedad = c("A","B","C","D"),
    adapt    = c("P","P","H","H"),
    stringsAsFactors = FALSE
  )
  res <- sugerir_cruces(A_test, c("A","B","C","D"),
                        max_f = 0.25,
                        df_categorias = cats,
                        filtro_adapt  = "P")
  # Solo A y B son de suelo P, por tanto solo puede aparecer A×B
  expect_true(all(res$Madre_ID %in% c("A","B")))
  expect_true(all(res$Padre_ID %in% c("A","B")))
})

test_that("emite warning si la matriz no es simétrica", {
  A_asim <- A_test
  A_asim[1, 2] <- 0.99   # romper simetría
  expect_warning(
    sugerir_cruces(A_asim, c("A","B","C","D"), max_f = 0.25),
    regexp = "simétrica"
  )
})
