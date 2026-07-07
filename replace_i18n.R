replace_in_file <- function(filepath, replacements) {
  content <- readChar(filepath, file.info(filepath)$size)
  Encoding(content) <- "UTF-8"
  for (i in 1:nrow(replacements)) {
    old <- replacements[i, "old"]
    new <- replacements[i, "new"]
    content <- gsub(old, new, content, fixed = TRUE)
  }
  
  con <- file(filepath, "w", encoding = "UTF-8")
  writeChar(content, con, eos = NULL)
  close(con)
}

sel_rep <- data.frame(
  old = c(
    '"Evaluación de Familias (EVF)"', '"Año Zafra (Colecta):"', '"Año Cruce:"', '"Cargar Excel de Familias"',
    '"Pre-seleccionar"', '"Confirmar Selección"', '"Año Sel:"', '"Año Cru:"', '"Suelo:"',
    '"Ver Datos de Campo (API)"', '"⚙️ Carga Manual / Excel (Admin)"', '"Subir Excel de Campo"',
    '"Confirmar Carga Manual"', '"Candidatos Promocionados desde"', '"Vista Previa del Excel de Campo"',
    '"Año de Selección:"', '"Experimento:"', '"División:"', '"Actualizar Analítica"',
    '"Descargar Datos de Variedades"', '"Descargar Lista"', '" Curvas de Isoproductividad (Arrastre para Zoom y Doble Clic para Alejar)"',
    '"Paleta de Gradiente:"', '"Testigo Base:"', '"Contornos:"', '"Relleno Térmico"',
    '" Distribución de Brix / Calidad (Selección vs Testigos vs Rechazo)"', '" Top 10 Cruces / Familias con Más Selecciones"',
    '"Analítica de Variedades"', '"Lista de Corte / Selección (Material Seleccionado)"',
    '"Registro y Sincronización"', '"Estudio de Variedades"', '"Analítica y Lista de Corte"',
    '"Gestión del Pipeline de Selección"', '"Captura de Campo (App)"', '"Filtros y Acciones"',
    '"Actualizar Tabla"', '"Promover a ST1"', '"Eliminar Registros"', '"Exportar CSV"',
    '"Registros de Campo — field_captures"'
  ),
  new = c(
    'tags$span(`data-i18n`="sel_evf_title", "Evaluación de Familias (EVF)")',
    'tags$span(`data-i18n`="lbl_harvest_year", "Año Zafra (Colecta):")',
    'tags$span(`data-i18n`="lbl_cross_year", "Año Cruce:")',
    'tags$span(`data-i18n`="btn_upload_families_excel", "Cargar Excel de Familias")',
    'tags$span(`data-i18n`="btn_preselect", "Pre-seleccionar")',
    'tags$span(`data-i18n`="btn_confirm_selection", "Confirmar Selección")',
    'tags$span(`data-i18n`="lbl_sel_year", "Año Sel:")',
    'tags$span(`data-i18n`="lbl_cross_year_short", "Año Cru:")',
    'tags$span(`data-i18n`="lbl_soil", "Suelo:")',
    'tags$span(`data-i18n`="btn_view_field_data", "Ver Datos de Campo (API)")',
    'tags$span(`data-i18n`="sel_manual_upload", "⚙️ Carga Manual / Excel (Admin)")',
    'tags$span(`data-i18n`="btn_upload_field_excel", "Subir Excel de Campo")',
    'tags$span(`data-i18n`="btn_confirm_manual", "Confirmar Carga Manual")',
    'tags$span(`data-i18n`="sel_promoted_candidates", "Candidatos Promocionados desde")',
    'tags$span(`data-i18n`="sel_excel_preview", "Vista Previa del Excel de Campo")',
    'tags$span(`data-i18n`="lbl_selection_year", "Año de Selección:")',
    'tags$span(`data-i18n`="lbl_experiment", "Experimento:")',
    'tags$span(`data-i18n`="lbl_division", "División:")',
    'tags$span(`data-i18n`="btn_update_analytics", "Actualizar Analítica")',
    'tags$span(`data-i18n`="btn_download_variety_data", "Descargar Datos de Variedades")',
    'tags$span(`data-i18n`="btn_download_list", "Descargar Lista")',
    'tags$span(`data-i18n`="chart_isoproductivity", " Curvas de Isoproductividad (Arrastre para Zoom y Doble Clic para Alejar)")',
    'tags$span(`data-i18n`="lbl_gradient_palette", "Paleta de Gradiente:")',
    'tags$span(`data-i18n`="lbl_base_control", "Testigo Base:")',
    'tags$span(`data-i18n`="lbl_contours", "Contornos:")',
    'tags$span(`data-i18n`="lbl_thermal_fill", "Relleno Térmico")',
    'tags$span(`data-i18n`="chart_brix_distribution", " Distribución de Brix / Calidad (Selección vs Testigos vs Rechazo)")',
    'tags$span(`data-i18n`="chart_top_families", " Top 10 Cruces / Familias con Más Selecciones")',
    'tags$span(`data-i18n`="sel_variety_analytics", "Analítica de Variedades")',
    'tags$span(`data-i18n`="sel_cut_list", "Lista de Corte / Selección (Material Seleccionado)")',
    'tags$span(`data-i18n`="tab_register_sync", "Registro y Sincronización")',
    'tags$span(`data-i18n`="tab_variety_study", "Estudio de Variedades")',
    'tags$span(`data-i18n`="tab_analytics_cutlist", "Analítica y Lista de Corte")',
    'tags$span(`data-i18n`="sel_pipeline_mgmt", "Gestión del Pipeline de Selección")',
    'tags$span(`data-i18n`="tab_field_capture_app", "Captura de Campo (App)")',
    'tags$span(`data-i18n`="sel_filters_actions", "Filtros y Acciones")',
    'tags$span(`data-i18n`="btn_update_table", "Actualizar Tabla")',
    'tags$span(`data-i18n`="btn_promote_st1", "Promover a ST1")',
    'tags$span(`data-i18n`="btn_delete_records", "Eliminar Registros")',
    'tags$span(`data-i18n`="btn_export_csv", "Exportar CSV")',
    'tags$span(`data-i18n`="sel_field_records", "Registros de Campo — field_captures")'
  ), stringsAsFactors=FALSE)

cross_rep <- data.frame(
  old = c(
    '"Configuración"', '"Evaluaciones Disponibles"', '"Pool de Madres:"', '"Modo de Planificación:"',
    '"Operativo (Sincronización Floral)"', '"Filtros de Adaptación"', '"Tipo de Suelo Objetivo:"',
    '"Pesos del Modelo"', '"Máxima Consanguinidad (F):"', '"Peso: Diversidad (1-F)"',
    '"Peso: Valor (FACTOR)"', '"Seguridad"', '"Omitir cálculo de F"', '"Top N cruces:"',
    '"Simular Cruzamientos"', '"Exportar Resumen"', '"Herramientas de Campo"', '"Nº Inicial Policruce:"',
    '"Hoja de Montaje"', '"Recomendaciones de Mejoramiento"', '"Biparentales"', '"Registrar Biparentales Seleccionados"',
    '"Ver Análisis (Radar)"', '"Policruces"', '"El sistema utiliza un ratio de 1:2 en FLORES (mínimo 2 flores por hembra)."',
    '"Registrar Policruces Seleccionados"', '"Registro de Cruces"', '"1. Registrar Cruce Ejecutado"',
    '"Fecha del Cruce"', '"Tipo de Cruce:"', '"Gramos cosechados (Fuzz):"', '"Notas (opcional):"',
    '"Guardar Cruce"', '"2. Cruces Registrados"', '"Eliminar Seleccionados"', '"Hoja de Cruces"',
    '"Parámetros Genéticos"', '"Configuración de Índice de Selección (Smith-Hazel)"', '"Heredabilidades (h²)"',
    '"Tonelaje (Y):"', '"Calidad (Q):"', '"Sanidad (Resistencia):"', '"Pesos Económicos ($)"',
    '"Imp. Tonelaje:"', '"Imp. Calidad:"', '"Imp. Sanidad:"', '"Nota: Estos parámetros definen cómo se combinan Y y Q para el Valor Agroeconómico."'
  ),
  new = c(
    'tags$span(`data-i18n`="cross_sidebar_config", "Configuración")',
    'tags$span(`data-i18n`="cross_available_evals", "Evaluaciones Disponibles")',
    'tags$span(`data-i18n`="cross_pool_mothers", "Pool de Madres:")',
    'tags$span(`data-i18n`="cross_plan_mode", "Modo de Planificación:")',
    'tags$span(`data-i18n`="cross_operative_sync", "Operativo (Sincronización Floral)")',
    'tags$span(`data-i18n`="cross_adapt_filters", "Filtros de Adaptación")',
    'tags$span(`data-i18n`="cross_target_soil", "Tipo de Suelo Objetivo:")',
    'tags$span(`data-i18n`="cross_model_weights", "Pesos del Modelo")',
    'tags$span(`data-i18n`="cross_max_inbreeding", "Máxima Consanguinidad (F):")',
    'tags$span(`data-i18n`="cross_weight_diversity", "Peso: Diversidad (1-F)")',
    'tags$span(`data-i18n`="cross_weight_value", "Peso: Valor (FACTOR)")',
    'tags$span(`data-i18n`="cross_security", "Seguridad")',
    'tags$span(`data-i18n`="cross_skip_f_calc", "Omitir cálculo de F")',
    'tags$span(`data-i18n`="cross_top_n", "Top N cruces:")',
    'tags$span(`data-i18n`="btn_simulate_crosses", "Simular Cruzamientos")',
    'tags$span(`data-i18n`="btn_export_summary", "Exportar Resumen")',
    'tags$span(`data-i18n`="cross_field_tools", "Herramientas de Campo")',
    'tags$span(`data-i18n`="cross_polycross_start", "Nº Inicial Policruce:")',
    'tags$span(`data-i18n`="btn_field_sheet", "Hoja de Montaje")',
    'tags$span(`data-i18n`="cross_recommendations", "Recomendaciones de Mejoramiento")',
    'tags$span(`data-i18n`="tab_biparental", "Biparentales")',
    'tags$span(`data-i18n`="btn_register_biparental", "Registrar Biparentales Seleccionados")',
    'tags$span(`data-i18n`="btn_view_radar", "Ver Análisis (Radar)")',
    'tags$span(`data-i18n`="tab_polycross", "Policruces")',
    'tags$span(`data-i18n`="cross_polycross_note", "El sistema utiliza un ratio de 1:2 en FLORES (mínimo 2 flores por hembra).")',
    'tags$span(`data-i18n`="btn_register_polycross", "Registrar Policruces Seleccionados")',
    'tags$span(`data-i18n`="tab_cross_registry", "Registro de Cruces")',
    'tags$span(`data-i18n`="cross_register_executed", "1. Registrar Cruce Ejecutado")',
    'tags$span(`data-i18n`="lbl_cross_date", "Fecha del Cruce")',
    'tags$span(`data-i18n`="lbl_cross_type", "Tipo de Cruce:")',
    'tags$span(`data-i18n`="lbl_fuzz_grams", "Gramos cosechados (Fuzz):")',
    'tags$span(`data-i18n`="lbl_notes_optional", "Notas (opcional):")',
    'tags$span(`data-i18n`="btn_save_cross", "Guardar Cruce")',
    'tags$span(`data-i18n`="cross_registered_list", "2. Cruces Registrados")',
    'tags$span(`data-i18n`="btn_delete_selected", "Eliminar Seleccionados")',
    'tags$span(`data-i18n`="btn_cross_sheet", "Hoja de Cruces")',
    'tags$span(`data-i18n`="tab_genetic_params", "Parámetros Genéticos")',
    'tags$span(`data-i18n`="cross_smith_hazel_config", "Configuración de Índice de Selección (Smith-Hazel)")',
    'tags$span(`data-i18n`="cross_heritabilities", "Heredabilidades (h²)")',
    'tags$span(`data-i18n`="lbl_tonnage", "Tonelaje (Y):")',
    'tags$span(`data-i18n`="lbl_quality", "Calidad (Q):")',
    'tags$span(`data-i18n`="lbl_health_resistance", "Sanidad (Resistencia):")',
    'tags$span(`data-i18n`="cross_economic_weights", "Pesos Económicos ($)")',
    'tags$span(`data-i18n`="cross_imp_tonnage", "Imp. Tonelaje:")',
    'tags$span(`data-i18n`="cross_imp_quality", "Imp. Calidad:")',
    'tags$span(`data-i18n`="cross_imp_health", "Imp. Sanidad:")',
    'tags$span(`data-i18n`="cross_params_note", "Nota: Estos parámetros definen cómo se combinan Y y Q para el Valor Agroeconómico.")'
  ), stringsAsFactors=FALSE)

cat("Updating mod_seleccion.R...\n")
tryCatch(replace_in_file("R/mod_seleccion.R", sel_rep), error=function(e) cat(e$message, "\n"))

cat("Updating mod_cruzamientos.R...\n")
tryCatch(replace_in_file("R/mod_cruzamientos.R", cross_rep), error=function(e) cat(e$message, "\n"))

cat("Done.\n")
