# ==============================================================================
# UTILS_REAL_DATA.R
# Lector y procesador del archivo maestro de actividades del año (AllAct2025.xls)
# ==============================================================================

# ------------------------------------------------------------------------------
# load_allact_data()
# Lee el excel AllAct2025.xls, limpia los sufijos de las columnas (ej: ",C,10")
# y castea los campos a sus tipos correctos.
# ------------------------------------------------------------------------------
load_allact_data <- function(filepath = "AllAct2025.xls") {
  if (!file.exists(filepath)) {
    stop("No se encontró el archivo ", filepath)
  }
  
  # Leemos como texto para evitar warnings por tipos mixtos (ej. tca),
  # luego casteamos explícitamente en mutate().
  df <- read_excel(filepath, col_types = "text")
  
  # Limpiar nombres de columnas (remover los sufijos tipo ,C,10 o ,N,6,2)
  nombres_limpios <- gsub(",.*", "", colnames(df))
  nombres_limpios <- tolower(trimws(nombres_limpios))
  colnames(df) <- nombres_limpios
  
  # Normalización de nombres de enfermedades comunes
  if ("carb" %in% names(df) && !"carbon" %in% names(df)) df <- df %>% rename(carbon = carb)
  if ("esm" %in% names(df) && !"es" %in% names(df))     df <- df %>% rename(es = esm)
  
  # Asegurar que las columnas existan antes del mutate
  for (col in c("carbon", "roya", "es", "disease", "y", "q", "agro", "gen", "factor", "maxest", "sta")) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }
  
  # Casteo de tipos básicos
  df_clean <- df %>%
    mutate(
      variedad = as.character(variedad),
      madre = as.character(madre),
      padre = as.character(padre),
      adapt = as.character(adapt),
      status = as.character(status),
      
      # Numéricas
      tca    = as.numeric(tca),
      rend   = as.numeric(rend),
      taa    = as.numeric(taa),
      carbon = as.numeric(carbon),
      roya   = as.numeric(roya),
      es     = as.numeric(es),
      disease = as.numeric(disease),
      agro   = as.numeric(agro),
      gen    = as.numeric(gen),
      factor = as.numeric(factor),
      maxest = as.numeric(maxest)
    )
  
  # Llenar posibles NAs en variables críticas
  df_clean <- df_clean %>%
    mutate(
      factor  = if_else(is.na(factor), 0, factor),
      disease = if_else(is.na(disease), 0, disease),
      agro    = if_else(is.na(agro), 0, agro),
      adapt   = case_when(
        toupper(trimws(adapt)) %in% c("GOOD",  "BUENO")             ~ "BUENO",
        toupper(trimws(adapt)) %in% c("CLAY",  "MAL_DRENADO", "MD") ~ "MAL_DRENADO",
        toupper(trimws(adapt)) %in% c("ROCKY", "ROCOSO")            ~ "ROCOSO",
        is.na(adapt)                                                 ~ "BUENO",
        TRUE ~ toupper(trimws(adapt))
      )
    )
  
  return(df_clean)
}

# ------------------------------------------------------------------------------
# load_familias_data()
# Lee el excel 'Evaluacion de Familias.xlsx' y limpia las columnas.
# ------------------------------------------------------------------------------
load_familias_data <- function(filepath = "Evaluacion de Familias.xlsx") {
  if (!file.exists(filepath)) {
    warning("No se encontró el archivo ", filepath)
    return(data.frame()) # Retorna df vacío si no existe
  }
  
  # Leemos como texto para evitar warnings por tipos mixtos (ej. tca/rend/taa),
  # luego casteamos explícitamente en mutate().
  df <- read_excel(filepath, col_types = "text")
  
  # Limpiar nombres de forma flexible
  df_clean <- df %>%
    clean_names()
    
  # Normalizar nombres de columnas si vienen del formato nuevo
  if ("anio" %in% names(df_clean)) df_clean <- df_clean %>% rename(ano = anio)
  if ("tca" %in% names(df_clean))  df_clean <- df_clean %>% rename(t_c_a = tca)
  if ("rend" %in% names(df_clean)) df_clean <- df_clean %>% rename(rend_96o = rend)
  if ("tsa" %in% names(df_clean))  df_clean <- df_clean %>% rename(t_a_a = tsa)
  
  df_clean <- df_clean %>%
    mutate(
      madre = as.character(madre),
      padre = as.character(padre),
      cruce = as.character(cruce),
      ano = as.character(ano),
      t_c_a = as.numeric(t_c_a),
      rend_96o = as.numeric(rend_96o),
      t_a_a = as.numeric(t_a_a),
      accion = trimws(as.character(accion)),
      accion = if_else(is.na(accion) | accion == "", "E", accion)
    )
  
  return(df_clean)
}
