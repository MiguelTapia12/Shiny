# ==============================================================================
# UTILS_SIM_DATA.R — Generación de Datos Simulados
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Genera datasets simulados calibrados a los rangos reales de AllAct2025.xls
# para variedades sin datos reales. Las 120 variedades de AllAct2025 conservan
# sus valores originales.
# ==============================================================================

# ------------------------------------------------------------------------------
# generate_sim_adaptacion()
#
# Genera adaptación a tipo de suelo simulada basada en frecuencias reales.
# (GOOD: ~60%, CLAY: ~20%, ROCKY: ~20%)
# ------------------------------------------------------------------------------
generate_sim_adaptacion <- function(cat_var, pedigree_var, seed = 42) {
  set.seed(seed + 3)
  
  ids_con_parentesco <- unique(as.character(pedigree_var$id_variedad))
  ids_validos <- intersect(ids_con_parentesco, as.character(cat_var$id_variedad))
  n_vars <- min(500, length(ids_validos))
  ids_muestra <- sample(ids_validos, n_vars)
  
  data.frame(
    id_variedad = ids_muestra,
    adapt_suelo = sample(c("GOOD", "CLAY", "ROCKY"), length(ids_muestra), 
                         replace = TRUE, prob = c(0.60, 0.20, 0.20)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# generate_sim_rendimiento()
#
# Genera datos de rendimiento histórico simulado para variedades con parentesco
# conocido. Calibrado a rangos reales de AllAct2025.
#
# @param cat_var      Catálogo de variedades
# @param pedigree_var Tabla de parentesco
# @param seed         Semilla para reproducibilidad
# @return data.frame con rendimiento simulado
# ------------------------------------------------------------------------------
generate_sim_rendimiento <- function(cat_var, pedigree_var, seed = 42) {
  set.seed(seed)
  
  # Variedades con parentesco conocido
  ids_con_parentesco <- unique(as.character(pedigree_var$id_variedad))
  ids_validos <- intersect(ids_con_parentesco, as.character(cat_var$id_variedad))
  
  # Muestrear un subconjunto representativo (~500 variedades)
  n_vars <- min(500, length(ids_validos))
  ids_muestra <- sample(ids_validos, n_vars)
  
  # Localidades simuladas (secciones de campo tipo Central Romana)
  localidades <- c("SEC-A171", "SEC-A186", "SEC-A187", "SEC-A188", "SEC-A189",
                    "SEC-B101", "SEC-B202", "SEC-C303")
  
  anios <- 2015:2025
  
  # Generar registros: cada variedad tiene 2-5 años de evaluación
  registros <- lapply(ids_muestra, function(id) {
    n_anios <- sample(2:5, 1)
    anios_var <- sort(sample(anios, n_anios))
    n_loc <- sample(1:3, 1)
    locs <- sample(localidades, n_loc)
    
    expand.grid(
      id_variedad = id,
      anio = anios_var,
      localidad = locs,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        # Rangos calibrados a AllAct2025 real
        tca  = round(runif(n(), 16, 40), 2),
        rend = round(runif(n(), 20, 80), 2),
        taa  = round(runif(n(), 40, 130), 2),
        agro = round(runif(n(), 10, 40), 2),
        gen  = round(runif(n(), 20, 40), 2)
      )
  })
  
  df_rend <- bind_rows(registros)
  
  # Agregar nombres comerciales
  df_rend <- df_rend %>%
    left_join(
      cat_var %>% select(id_variedad, descripcion_variedad) %>% distinct(id_variedad, .keep_all = TRUE),
      by = "id_variedad"
    ) %>%
    mutate(descripcion_variedad = ifelse(
      is.na(descripcion_variedad), paste0("ID:", id_variedad), descripcion_variedad
    ))
  
  return(df_rend)
}


# ------------------------------------------------------------------------------
# generate_sim_enfermedades()
#
# Genera datos fitosanitarios simulados con 5 enfermedades reales.
# Escala 1-9 (1=resistente, 9=susceptible), igual que AllAct2025.
#
# @param cat_var      Catálogo de variedades
# @param pedigree_var Tabla de parentesco
# @param seed         Semilla para reproducibilidad
# @return data.frame con evaluaciones fitosanitarias
# ------------------------------------------------------------------------------
generate_sim_enfermedades <- function(cat_var, pedigree_var, seed = 42) {
  set.seed(seed + 1)  # Seed diferente para no correlacionar con rendimiento
  
  ids_con_parentesco <- unique(as.character(pedigree_var$id_variedad))
  ids_validos <- intersect(ids_con_parentesco, as.character(cat_var$id_variedad))
  n_vars <- min(500, length(ids_validos))
  ids_muestra <- sample(ids_validos, n_vars)
  
  anios <- 2018:2025
  
  registros <- lapply(ids_muestra, function(id) {
    n_anios <- sample(1:4, 1)
    anios_var <- sort(sample(anios, n_anios))
    
    data.frame(
      id_variedad = id,
      anio = anios_var,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        # Enfermedades reales de AllAct2025 — escala 1-9
        yellow_leaf  = sample(1:9, n(), replace = TRUE, 
                              prob = c(0.05, 0.08, 0.12, 0.15, 0.20, 0.15, 0.12, 0.08, 0.05)),
        raquitismo   = sample(1:9, n(), replace = TRUE,
                              prob = c(0.10, 0.12, 0.15, 0.18, 0.15, 0.12, 0.08, 0.06, 0.04)),
        carbon       = sample(1:9, n(), replace = TRUE,
                              prob = c(0.15, 0.15, 0.15, 0.12, 0.12, 0.10, 0.08, 0.08, 0.05)),
        roya         = sample(1:9, n(), replace = TRUE,
                              prob = c(0.08, 0.10, 0.12, 0.15, 0.15, 0.15, 0.10, 0.08, 0.07)),
        escaldadura  = sample(1:9, n(), replace = TRUE,
                              prob = c(0.05, 0.08, 0.10, 0.12, 0.20, 0.18, 0.12, 0.10, 0.05)),
        # Score compuesto: promedio ponderado (roya y carbón pesan más)
        disease_score = round(
          (yellow_leaf * 0.15 + raquitismo * 0.15 + carbon * 0.25 + 
           roya * 0.25 + escaldadura * 0.20), 2
        )
      )
  })
  
  df_enf <- bind_rows(registros)
  
  # Agregar nombres
  df_enf <- df_enf %>%
    left_join(
      cat_var %>% select(id_variedad, descripcion_variedad) %>% distinct(id_variedad, .keep_all = TRUE),
      by = "id_variedad"
    )
  
  return(df_enf)
}


# ------------------------------------------------------------------------------
# generate_sim_seleccion()
#
# Genera historia de selección simulada con etapas de avance.
# Basado en la estructura de AllAct2025 (MAXEST, STATUS, PORC_SEL, GCA).
#
# @param cat_var      Catálogo de variedades
# @param pedigree_var Tabla de parentesco
# @param seed         Semilla para reproducibilidad
# @return data.frame con historia de selección
# ------------------------------------------------------------------------------
generate_sim_seleccion <- function(cat_var, pedigree_var, seed = 42) {
  set.seed(seed + 2)
  
  ids_con_parentesco <- unique(as.character(pedigree_var$id_variedad))
  ids_validos <- intersect(ids_con_parentesco, as.character(cat_var$id_variedad))
  n_vars <- min(500, length(ids_validos))
  ids_muestra <- sample(ids_validos, n_vars)
  
  # Etapas de selección en caña de azúcar
  etapas <- c("SEEDLING", "FASE_I", "FASE_II", "FASE_III", "SEMI_COMERCIAL", "COMERCIAL")
  
  registros <- lapply(ids_muestra, function(id) {
    # Cada variedad tiene una etapa máxima alcanzada
    max_etapa <- sample(1:6, 1, prob = c(0.35, 0.25, 0.18, 0.12, 0.07, 0.03))
    anio_cruce <- sample(2005:2022, 1)
    
    # Generar registros por cada etapa alcanzada
    etapas_alcanzadas <- etapas[1:max_etapa]
    
    data.frame(
      id_variedad = id,
      anio_cruce  = anio_cruce,
      anio_eval   = anio_cruce + (seq_along(etapas_alcanzadas) - 1) * 2,
      etapa       = etapas_alcanzadas,
      etapa_num   = seq_along(etapas_alcanzadas),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        status = ifelse(etapa == last(etapas_alcanzadas),
                        sample(c("P", "E"), 1),
                        "E"),
        decision = case_when(
          etapa == last(etapas_alcanzadas) & max_etapa < 6 ~ 
            sample(c("RECHAZADO", "EN_EVALUACION"), 1, prob = c(0.6, 0.4)),
          etapa == last(etapas_alcanzadas) & max_etapa == 6 ~ "SELECCIONADO",
          TRUE ~ "SELECCIONADO"
        ),
        porc_sel = round(runif(n(), 0, 100), 0),
        gcam     = round(runif(n(), -50, 200), 2),
        gcap     = round(runif(n(), -50, 200), 2)
      )
  })
  
  df_sel <- bind_rows(registros)
  
  # Agregar nombres
  df_sel <- df_sel %>%
    left_join(
      cat_var %>% select(id_variedad, descripcion_variedad) %>% distinct(id_variedad, .keep_all = TRUE),
      by = "id_variedad"
    )
  
  return(df_sel)
}


# ------------------------------------------------------------------------------
# generate_all_sim_data()
#
# Genera y guarda los 3 datasets simulados en data/sim/
#
# @param cat_var      Catálogo de variedades
# @param pedigree_var Tabla de parentesco
# @param output_dir   Directorio de salida
# @return lista con los 3 data.frames
# ------------------------------------------------------------------------------
generate_all_sim_data <- function(cat_var, pedigree_var, output_dir = "data/sim") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  message(">> Generando datos simulados de rendimiento...")
  sim_rend <- generate_sim_rendimiento(cat_var, pedigree_var)
  fwrite(sim_rend, file.path(output_dir, "sim_rendimiento.csv"))
  message("   -> ", nrow(sim_rend), " registros generados")
  
  message(">> Generando datos simulados de enfermedades...")
  sim_enf <- generate_sim_enfermedades(cat_var, pedigree_var)
  fwrite(sim_enf, file.path(output_dir, "sim_enfermedades.csv"))
  message("   -> ", nrow(sim_enf), " registros generados")
  
  message(">> Generando datos simulados de selección...")
  sim_sel <- generate_sim_seleccion(cat_var, pedigree_var)
  fwrite(sim_sel, file.path(output_dir, "sim_seleccion.csv"))
  message("   -> ", nrow(sim_sel), " registros generados")
  
  message(">> Generando datos de adaptación de suelo...")
  sim_adapt <- generate_sim_adaptacion(cat_var, pedigree_var)
  fwrite(sim_adapt, file.path(output_dir, "sim_adaptacion.csv"))
  message("   -> ", nrow(sim_adapt), " registros generados")
  
  message(">> Datos simulados guardados en: ", output_dir)
  
  return(list(
    rendimiento  = sim_rend,
    enfermedades = sim_enf,
    seleccion    = sim_sel,
    adaptacion   = sim_adapt
  ))
}
