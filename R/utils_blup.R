# ==============================================================================
# utils_blup.R — Motor de Valores Genéticos (Desviaciones Ajustadas)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Descripción:
#   Dado que los ensayos históricos suelen tener N=1 por variedad, el uso de 
#   modelos mixtos puros encoge los valores a cero.
#   Este script implementa el estándar agronómico para ensayos sin repetición:
#   "Desviación contra el Testigo del Ambiente".
#
#   Ambiente = Año + Suelo
#   EBV = Valor Observado - Promedio de Testigos(en ese Ambiente)
# ==============================================================================

# Paquetes cargados en dependencies.R

# ==============================================================================
# FUNCIÓN PRINCIPAL: calcular_ebvs()
# ==============================================================================
calcular_ebvs <- function(archivo, hoja = 1) {

  # ----------------------------------------------------------------------------
  # 1. CARGAR Y LIMPIAR DATOS
  # ----------------------------------------------------------------------------
  message(">> Cargando Registro de Ensayos...")

  df <- read_excel(archivo, sheet = hoja) %>%
    janitor::clean_names() %>%
    mutate(
      variedad   = toupper(trimws(variedad)),
      ano        = as.character(ano),
      suelo      = toupper(trimws(suelo)), # El suelo es vital ahora
      es_testigo = toupper(trimws(es_testigo)) == "SI",
      # Convertir rasgos a numérico
      tca    = suppressWarnings(as.numeric(tca)),
      rend   = suppressWarnings(as.numeric(rend)),
      pureza = suppressWarnings(as.numeric(pureza)),
      fibra  = suppressWarnings(as.numeric(fibra)),
      pol    = suppressWarnings(as.numeric(pol))
    ) %>%
    # Crear la columna de Ambiente
    mutate(ambiente = paste(ano, suelo, sep = " - "))

  message(sprintf("   Total registros cargados: %d", nrow(df)))
  message(sprintf("   Variedades únicas:        %d", n_distinct(df$variedad)))
  message(sprintf("   Ambientes únicos:         %d", n_distinct(df$ambiente)))

  # ----------------------------------------------------------------------------
  # 2. CALCULAR LÍNEAS BASE POR AMBIENTE (BASELINE)
  # ----------------------------------------------------------------------------
  message("\n>> Calculando líneas base por Ambiente (Año + Suelo)...")
  
  rasgos <- c("tca", "rend", "pureza", "fibra", "pol")
  
  # Calculamos el promedio de los testigos para cada ambiente y cada rasgo
  baselines <- df %>%
    group_by(ambiente) %>%
    summarise(
      n_total = n(),
      n_testigos = sum(es_testigo, na.rm = TRUE),
      
      # Si hay testigos, promediamos los testigos. Si NO hay, usamos el promedio general del ambiente.
      base_tca    = if(sum(es_testigo[!is.na(tca)]) > 0) mean(tca[es_testigo], na.rm = T) else mean(tca, na.rm = T),
      base_rend   = if(sum(es_testigo[!is.na(rend)]) > 0) mean(rend[es_testigo], na.rm = T) else mean(rend, na.rm = T),
      base_pureza = if(sum(es_testigo[!is.na(pureza)]) > 0) mean(pureza[es_testigo], na.rm = T) else mean(pureza, na.rm = T),
      base_fibra  = if(sum(es_testigo[!is.na(fibra)]) > 0) mean(fibra[es_testigo], na.rm = T) else mean(fibra, na.rm = T),
      base_pol    = if(sum(es_testigo[!is.na(pol)]) > 0) mean(pol[es_testigo], na.rm = T) else mean(pol, na.rm = T),
      
      .groups = "drop"
    ) %>%
    mutate(
      calidad_baseline = ifelse(n_testigos > 0, "Testigo Oficial", "Promedio General")
    )

  reporte_ambientes <- baselines %>% select(ambiente, n_total, n_testigos, calidad_baseline)

  # ----------------------------------------------------------------------------
  # 3. CALCULAR DESVIACIONES (EBVs)
  # ----------------------------------------------------------------------------
  message("\n>> Calculando desviaciones ajustadas (EBVs)...")

  df_calculado <- df %>%
    left_join(baselines, by = "ambiente") %>%
    mutate(
      ebv_tca    = tca - base_tca,
      ebv_rend   = rend - base_rend,
      ebv_pureza = pureza - base_pureza,
      ebv_fibra  = fibra - base_fibra,
      ebv_pol    = pol - base_pol
    )

  # Si una variedad aparece varias veces (muy raro, pero posible), promediamos su EBV
  ebvs_final <- df_calculado %>%
    group_by(variedad) %>%
    summarise(
      ebv_tca    = mean(ebv_tca, na.rm = TRUE),
      ebv_rend   = mean(ebv_rend, na.rm = TRUE),
      ebv_pureza = mean(ebv_pureza, na.rm = TRUE),
      ebv_fibra  = mean(ebv_fibra, na.rm = TRUE),
      ebv_pol    = mean(ebv_pol, na.rm = TRUE),
      
      total_obs  = n(),
      es_testigo = any(es_testigo),
      .groups = "drop"
    ) %>%
    # Reemplazar NaN con NA
    mutate(across(starts_with("ebv_"), ~ifelse(is.nan(.), NA, .))) %>%
    mutate(confianza = ifelse(es_testigo, "Testigo", "Ajustada por Ambiente")) %>%
    select(-es_testigo)

  message(sprintf(">> ✅ TABLA FINAL: %d variedades con valores calculados.", nrow(ebvs_final)))

  return(list(
    ebvs    = ebvs_final,
    reporte = reporte_ambientes
  ))
}

# ==============================================================================
# FUNCIÓN AUXILIAR: guardar_ebvs_en_db()
# ==============================================================================
guardar_ebvs_en_db <- function(ebvs_df, con) {
  if (nrow(ebvs_df) == 0) {
    warning("Tabla de EBVs vacía. No se guardó nada.")
    return(invisible(NULL))
  }
  
  # 1. Tabla de acceso rápido (overwrite)
  dbWriteTable(con, "ebvs_variedades", ebvs_df, overwrite = TRUE)
  
  # 2. Historial de EBVs (append)
  ebvs_history <- ebvs_df %>%
    mutate(timestamp_calculo = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
  dbWriteTable(con, "historial_ebvs", ebvs_history, append = TRUE)
  
  message(sprintf(">> ✅ EBVs guardados en BD (Fast + Historial): %d variedades.", nrow(ebvs_df)))
}

# ==============================================================================
# FUNCIÓN AUXILIAR: get_ebv_variedad()
# ==============================================================================
get_ebv_variedad <- function(variedad_nombre, ebvs_df, rasgo = "ebv_rend") {
  resultado <- ebvs_df %>%
    filter(variedad == toupper(trimws(variedad_nombre))) %>%
    pull(!!sym(rasgo))
  
  if (length(resultado) == 0 || is.na(resultado)) return(0)
  return(resultado[1])
}
