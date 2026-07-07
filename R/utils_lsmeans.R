# ==============================================================================
# UTILS_LSMEANS.R — Cálculo de Medias Ajustadas (LSMeans) para DBCA
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Modelo: DBCA (Diseño de Bloques Completos al Azar)
#   Rasgo ~ Variedad + (1 | num_replica)
# Ajustado por cada combinación de num_experimento × corte_nombre.
#
# Si el modelo mixto falla (pocas observaciones, un solo bloque, convergencia),
# se degrada automáticamente a un modelo lineal fijo (lm) para mantener la
# robustez del pipeline sin pérdida de datos.
# ==============================================================================

calcular_lsmeans <- function(df, rasgos = c("tca", "rendimiento", "tsh")) {
  # Verificar que lme4 y emmeans están disponibles
  if (!requireNamespace("lme4", quietly = TRUE)) {
    warning("Paquete 'lme4' no disponible. Usando promedios aritméticos.")
    return(calcular_fallback_means(df, rasgos))
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    warning("Paquete 'emmeans' no disponible. Usando promedios aritméticos.")
    return(calcular_fallback_means(df, rasgos))
  }

  library(lme4)
  library(emmeans)

  resultados <- list()

  # Iterar por cada combinación de experimento × corte
  grupos <- unique(df[, c("num_experimento", "corte_nombre")])

  for (i in seq_len(nrow(grupos))) {
    exp_id    <- grupos$num_experimento[i]
    corte_id  <- grupos$corte_nombre[i]

    df_sub <- df %>%
      filter(num_experimento == exp_id, corte_nombre == corte_id)

    # Necesitamos al menos 2 variedades y 2 réplicas para un modelo DBCA
    n_vars <- length(unique(df_sub$variedad))
    n_reps <- length(unique(df_sub$num_replica))

    if (n_vars < 2 || nrow(df_sub) < 3) next

    for (rasgo in rasgos) {
      df_rasgo <- df_sub %>%
        filter(!is.na(.data[[rasgo]])) %>%
        mutate(
          variedad    = as.factor(variedad),
          num_replica = as.factor(num_replica)
        )

      if (nrow(df_rasgo) < 3 || length(unique(df_rasgo$variedad)) < 2) next

      # --- Intentar modelo mixto DBCA ---
      lsmeans_df <- tryCatch({
        if (n_reps >= 2) {
          # Modelo completo: variedad como fijo, bloque como aleatorio
          # Usamos suppressMessages y suppressWarnings para ignorar el 
          # "boundary (singular) fit" que es normal cuando la varianza del bloque es 0.
          mod <- suppressMessages(suppressWarnings(lmer(
            as.formula(paste0(rasgo, " ~ variedad + (1 | num_replica)")),
            data = df_rasgo
          )))
          em <- suppressMessages(emmeans(mod, "variedad"))
        } else {
          # Solo 1 bloque: modelo fijo simple
          mod <- lm(as.formula(paste0(rasgo, " ~ variedad")), data = df_rasgo)
          em <- emmeans(mod, "variedad")
        }

        as.data.frame(em) %>%
          transmute(
            variedad = as.character(variedad),
            lsmean   = emmean,
            se       = SE
          )
      }, error = function(e) {
        # Fallback: promedio aritmético por variedad
        df_rasgo %>%
          group_by(variedad) %>%
          summarise(
            lsmean = mean(.data[[rasgo]], na.rm = TRUE),
            se     = sd(.data[[rasgo]], na.rm = TRUE) / sqrt(n()),
            .groups = "drop"
          ) %>%
          mutate(variedad = as.character(variedad))
      })

      if (!is.null(lsmeans_df) && nrow(lsmeans_df) > 0) {
        # Extraer metadatos del grupo original
        meta <- df_sub %>%
          group_by(variedad) %>%
          summarise(
            suelo       = first(suelo),
            ind_testigo = first(ind_testigo),
            ano_zafra   = first(ano_zafra),
            .groups     = "drop"
          )

        lsmeans_df <- lsmeans_df %>%
          left_join(meta, by = "variedad") %>%
          mutate(
            num_experimento = exp_id,
            corte_nombre    = corte_id,
            rasgo           = rasgo
          )

        resultados[[length(resultados) + 1]] <- lsmeans_df
      }
    }
  }

  if (length(resultados) == 0) {
    warning("No se pudo calcular ninguna media ajustada. Usando fallback.")
    return(calcular_fallback_means(df, rasgos))
  }

  # Consolidar resultados
  df_ajustado <- bind_rows(resultados) %>%
    tidyr::pivot_wider(
      id_cols     = c(variedad, num_experimento, corte_nombre, suelo, ind_testigo, ano_zafra),
      names_from  = rasgo,
      values_from = c(lsmean, se),
      names_glue  = "{rasgo}_{.value}"
    )

  # Renombrar columnas lsmean a los nombres originales de rasgos
  for (r in rasgos) {
    col_lsmean <- paste0(r, "_lsmean")
    col_se     <- paste0(r, "_se")
    if (col_lsmean %in% names(df_ajustado)) {
      names(df_ajustado)[names(df_ajustado) == col_lsmean] <- r
    }
    if (col_se %in% names(df_ajustado)) {
      names(df_ajustado)[names(df_ajustado) == col_se] <- paste0(r, "_se")
    }
  }

  # Agregar columna corte numérico para ordenamiento
  df_ajustado <- df_ajustado %>%
    mutate(
      corte = case_when(
        corte_nombre == "Plantilla" ~ 1L,
        grepl("Reto", corte_nombre) ~ as.integer(gsub("[^0-9]", "", corte_nombre)) + 1L,
        TRUE ~ NA_integer_
      )
    )

  return(df_ajustado)
}


# ==============================================================================
# FALLBACK: Promedios aritméticos simples (si lme4/emmeans no disponibles)
# ==============================================================================
calcular_fallback_means <- function(df, rasgos = c("tca", "rendimiento", "tsh")) {
  df %>%
    group_by(variedad, num_experimento, corte_nombre, suelo, ind_testigo, ano_zafra) %>%
    summarise(
      across(all_of(rasgos), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      corte = case_when(
        corte_nombre == "Plantilla" ~ 1L,
        grepl("Reto", corte_nombre) ~ as.integer(gsub("[^0-9]", "", corte_nombre)) + 1L,
        TRUE ~ NA_integer_
      )
    )
}
