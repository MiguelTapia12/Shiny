# tests/testthat/setup.R

.testing_mode <- TRUE
options(testthat.running = TRUE)

# Función helper que devuelve datos frescos en cada llamada
# (evita mutación accidental entre tests)
ped_minimal <- function() {
  data.frame(
    id_variedad          = c("100", "100", "200"),
    id_variedad_ancestro = c("200", "300", "400"),
    tipo_ancestro        = c("PADRE", "MADRE", "PADRE"),
    stringsAsFactors     = FALSE
  )
}

cat_minimal <- function() {
  data.frame(
    id_variedad          = c("100", "200", "300", "400"),
    descripcion_variedad = c("VAR-A", "VAR-B", "VAR-C", "VAR-D"),
    stringsAsFactors     = FALSE
  )
}

# Matriz A de prueba compartida (solo lectura, no modificar)
A_test <- matrix(
  c(1.00, 0.50, 0.10, 0.00,
    0.50, 1.00, 0.00, 0.05,
    0.10, 0.00, 1.00, 0.50,
    0.00, 0.05, 0.50, 1.00),
  nrow = 4, ncol = 4,
  dimnames = list(c("A","B","C","D"), c("A","B","C","D"))
)

message(">> Setup de tests completado: modo ligero activo.")
