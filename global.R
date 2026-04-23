# ==============================================================================
# GLOBAL.R — Configuración Global
# Pipeline de Selección Genética — Central Romana v2.0
# ==============================================================================
# Centraliza: librerías, carga de datos, generación de simulados,
# y source de módulos.
# ==============================================================================

# --- 1. LIBRERÍAS ---
source("R/dependencies.R", local = FALSE, encoding = "UTF-8")
load_dependencies()

# --- 2. CARGAR MÓDULOS ---

# HOTFIX: asegurar carga del módulo de trazabilidad
source("R/utils_pedigree.R", local = FALSE, encoding = "UTF-8")
source("R/utils_real_data.R", local = FALSE, encoding = "UTF-8")
source("R/mod_genealogia.R", local = FALSE, encoding = "UTF-8")
source("R/mod_cruzamientos.R", local = FALSE, encoding = "UTF-8")
source("R/mod_estado_variedad.R", local = FALSE, encoding = "UTF-8")
source("R/mod_seleccion.R", local = FALSE, encoding = "UTF-8")
source("R/utils_sim_data.R", local = FALSE, encoding = "UTF-8")
source("R/utils_selection.R", local = FALSE, encoding = "UTF-8")
source("R/utils_db.R", local = FALSE, encoding = "UTF-8")
source("R/mod_archivo.R", local = FALSE, encoding = "UTF-8")
source("R/mod_dashboard.R", local = FALSE, encoding = "UTF-8")

# --- 3. INICIALIZAR BASE DE DATOS ---
db_path <- "data/breeding_system.db"
con <- db_connect(db_path)
db_init_schema(con)

# --- 4. CARGA Y LIMPIEZA DE DATOS (Legacy o DB) ---
# Verificar si la base de datos tiene datos en las tablas principales
check_cat <- dbGetQuery(con, "SELECT COUNT(*) as n FROM catalogo")$n > 0
check_ped <- dbGetQuery(con, "SELECT COUNT(*) as n FROM parentesco")$n > 0
db_has_data <- check_cat && check_ped

if (!db_has_data) {
  message(">> Datos incompletos en DB. Iniciando migración/reparación desde archivos...")
  
  # Cargar desde archivos (Lógica original)
  cat_var <- fread("CAT_VARIEDADES.txt") %>%
    clean_names() %>%
    mutate(id_variedad = as.character(id_variedad))
  
  pedigree_var <- fread("PARENTESCO_VARIEDADES.txt") %>%
    clean_names() %>%
    mutate(across(c(id_variedad, id_variedad_ancestro), as.character))
  
  # Cargar datos reales y familias para categorizar
  df_act2025 <- load_allact_data("AllAct2025.xls")
  df_familias <- load_familias_data("Evaluacion de Familias.xlsx")
  df_categorias <- assign_genetic_categories(df_act2025, df_familias)
  
  # Migrar a la BD (si la función está disponible)
  if (exists("db_migrate_legacy_data", mode = "function")) {
    db_migrate_legacy_data(con, cat_var, pedigree_var, df_categorias)
  } else {
    warning("db_migrate_legacy_data() no está definida; se omite migración legacy.")
  }
}

# Cargar objetos globales desde la Base de Datos
message(">> Cargando datos desde SQLite...")
cat_var <- dbReadTable(con, "catalogo")
pedigree_var <- dbReadTable(con, "parentesco")
df_categorias <- dbReadTable(con, "categorias")

# Re-generar df_ped_wide (necesario para AGHmatrix)
df_ped_wide <- pedigree_var %>%
  filter(id_variedad != id_variedad_ancestro) %>%
  select(id_variedad, id_variedad_ancestro, tipo_ancestro) %>%
  group_by(id_variedad, tipo_ancestro) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = tipo_ancestro, values_from = id_variedad_ancestro) %>%
  clean_names()

# Asegurar que existan las columnas padre y madre
if (!"padre" %in% names(df_ped_wide)) df_ped_wide$padre <- NA_character_
if (!"madre" %in% names(df_ped_wide)) df_ped_wide$madre <- NA_character_

# AGHmatrix requiere que todos los padres/madres existan como individuos en la tabla
all_ids <- unique(c(df_ped_wide$id_variedad, df_ped_wide$padre, df_ped_wide$madre))
all_ids <- all_ids[!is.na(all_ids) & all_ids != "0"]
missing_ids <- setdiff(all_ids, df_ped_wide$id_variedad)

if (length(missing_ids) > 0) {
  missing_df <- data.frame(
    id_variedad = missing_ids,
    padre = "0",
    madre = "0",
    stringsAsFactors = FALSE
  )
  df_ped_wide <- bind_rows(missing_df, df_ped_wide)
}

message("   -> ", nrow(cat_var), " variedades cargadas")
message("   -> ", nrow(pedigree_var), " registros de parentesco")

# --- 5. CATEGORIZACION GENETICA (C1-C6) ---
message(">> Actualizando categorias desde Excel...")
df_act2025 <- load_allact_data("AllAct2025.xls")

# Cargar familias combinando Excel y Base de Datos
df_familias_excel <- load_familias_data("Evaluacion de Familias.xlsx")
df_familias_bd <- dbGetQuery(
  con,
  "SELECT
     CAST(anio AS INTEGER) AS anio,
     CAST(programa AS TEXT) AS programa,
     CAST(experimento AS INTEGER) AS experimento,
     CAST(cruce AS TEXT) AS cruce,
     CAST(madre AS TEXT) AS madre,
     CAST(padre AS TEXT) AS padre,
     CAST(tca AS REAL) AS tca,
     CAST(rend AS REAL) AS rend,
     CAST(tsa AS REAL) AS tsa,
     CAST(indice_tsa AS REAL) AS indice_tsa,
     CAST(accion AS TEXT) AS accion
   FROM familias_evf"
)

# Normalizar nombres de la BD para que coincidan con la lógica interna
if (nrow(df_familias_bd) > 0) {
  df_familias_bd <- df_familias_bd %>%
    rename(
      ano = anio,
      t_c_a = tca,
      rend_96o = rend,
      t_a_a = tsa
    ) %>%
    mutate(ano = as.character(ano)) # Asegurar compatibilidad de tipos
}

# Unir y limpiar (priorizando lo que ya esta en la BD)
df_familias <- bind_rows(df_familias_excel, df_familias_bd) %>%
  distinct(ano, experimento, cruce, .keep_all = TRUE)

df_categorias_excel <- assign_genetic_categories(df_act2025, df_familias)

# Cargar notas y promociones desde la BD para complementar
promociones_bd <- dbReadTable(con, "promociones")
categorias_bd <- dbReadTable(con, "categorias")

# Combinar: Excel manda en metricas, pero BD aporta Notas y nuevas variedades CR
df_categorias <- df_categorias_excel %>%
  left_join(categorias_bd %>% select(variedad, notas), by = "variedad")

# Si hay promociones nuevas que no estan en el Excel, las agregamos como C5 (Comercial/Nuevo)
if (nrow(promociones_bd) > 0) {
  nuevas_cr <- promociones_bd %>%
    filter(!nombre_cr %in% df_categorias$variedad) %>%
    transmute(
      variedad = nombre_cr,
      categoria = "C5: Comercial/Nuevo",
      factor = 0, # Valor por defecto inicial
      status = "P",
      adapt = suelo,
      notas = paste("Promocionada desde:", clon_origen)
    )
  df_categorias <- bind_rows(df_categorias, nuevas_cr)
}

message("   -> ", nrow(df_categorias), " variedades categorizadas (incluyendo promociones)")

# --- 6. DATOS SIMULADOS (Pipeline de Evaluación) ---
message(">> Cargando datos simulados...")
sim_rendimiento <- fread("data/sim/sim_rendimiento.csv")
sim_enfermedades <- fread("data/sim/sim_enfermedades.csv")
sim_seleccion <- fread("data/sim/sim_seleccion.csv")
sim_adaptacion <- fread("data/sim/sim_adaptacion.csv")

message("======================================")
message(">> Sistema listo para iniciar")
message("======================================")
