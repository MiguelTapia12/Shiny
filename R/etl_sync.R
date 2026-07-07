# ==============================================================================
# ETL_SYNC.R — Proceso de Extracción, Transformación y Carga (ETL)
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Este script centraliza la lógica pesada de limpieza y sincronización.
# La aplicación Shiny solo consume los resultados ya procesados.
# ==============================================================================

run_full_etl_sync <- function(con, allact_file, families_file, progress_callback = NULL) {
  
  resultado_final <- list(ok = TRUE, msg = "")
  
  tryCatch({
    # --- 1. Cargar AllAct (Metadata de Variedades) ---
    if (!is.null(progress_callback)) progress_callback(0.1, "Procesando catálogo AllAct...")
    df_act_new <- load_allact_data(allact_file)
    db_log_sync(con, allact_file, "Carga AllAct", "Éxito", nrow(df_act_new))
    
    # --- 2. Cargar Evaluación de Familias ---
    if (!is.null(progress_callback)) progress_callback(0.3, "Procesando evaluación de familias...")
    df_fam_excel <- load_familias_data(families_file)
    
    # Combinar con datos históricos de la BD
    df_fam_bd <- dbReadTable(con, "familias_evf")
    if (nrow(df_fam_bd) > 0) {
      df_fam_bd <- df_fam_bd %>%
        rename(ano = anio, t_c_a = tca, rend_96o = rend, t_a_a = tsa) %>%
        mutate(ano = as.character(ano))
      
      df_fam_total <- bind_rows(df_fam_excel, df_fam_bd) %>%
        distinct(ano, experimento, cruce, .keep_all = TRUE)
    } else {
      df_fam_total <- df_fam_excel
    }
    db_log_sync(con, families_file, "Carga Familias", "Éxito", nrow(df_fam_total))
    
    # --- 3. Sincronizar Repositorio (Legacy TXTs) ---
    if (!is.null(progress_callback)) progress_callback(0.5, "Sincronizando parentesco...")
    db_sync_repository_to_db(con)
    
    # --- 4. Recalcular Categorías Genéticas (C1-C5) ---
    if (!is.null(progress_callback)) progress_callback(0.7, "Recalculando mérito genético...")
    nuevas_categorias <- assign_genetic_categories(df_act_new, df_fam_total)
    
    # --- 5. Persistir en Base de Datos ---
    if (!is.null(progress_callback)) progress_callback(0.9, "Persistiendo datos en BD...")
    
    # Actualizar tabla de categorías (sobrescribir con lo último)
    # 4.5. Preservar Notas históricas antes de borrar
    categorias_viejas <- dbReadTable(con, "categorias")
    if (nrow(categorias_viejas) > 0 && "notas" %in% names(categorias_viejas)) {
      nuevas_categorias <- nuevas_categorias %>%
        left_join(categorias_viejas %>% select(variedad, notas), by = "variedad")
    } else {
      nuevas_categorias$notas <- NA_character_
    }
    
    # Seleccionar exclusivamente las columnas que componen el esquema en SQLite
    cols_bd <- c("variedad", "categoria", "factor", "disease", "y", "q", "agro", "gen", "evf_info", "adapt", "status", "maxest", "notas", "carbon", "roya", "es")
    nuevas_categorias_db <- nuevas_categorias %>% 
      distinct(variedad, .keep_all = TRUE) %>%
      select(any_of(cols_bd))
    
    dbExecute(con, "DELETE FROM categorias")
    dbWriteTable(con, "categorias", nuevas_categorias_db, append = TRUE)
    
    # Actualizar tabla de familias (sobrescribir para mantener distinct)
    # En un sistema más grande usaríamos UPSERT, aquí recreamos para simplicidad 2026.
    dbExecute(con, "DELETE FROM familias_evf")
    df_fam_to_db <- df_fam_total %>%
      rename(anio = ano, tca = t_c_a, rend = rend_96o, tsa = t_a_a) %>%
      select(any_of(c("anio", "programa", "experimento", "cruce", "madre", "padre", "tca", "rend", "tsa", "indice_tsa", "accion")))
      
    dbWriteTable(con, "familias_evf", df_fam_to_db, append = TRUE)
    
    if (!is.null(progress_callback)) progress_callback(1.0, "Sincronización finalizada.")
    
    resultado_final$msg <- "Sincronización completada: Catálogo y Familias actualizados."
    db_log_sync(con, "SISTEMA", "Sync Completa", "Finalizado con Éxito")
    
  }, error = function(e) {
    db_log_sync(con, "SISTEMA", "Sync Completa", paste("Error:", e$message))
    resultado_final$ok <<- FALSE
    resultado_final$msg <<- e$message
  })
  
  return(resultado_final)
}
