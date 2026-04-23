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
  # Remover espacios y pasar a minúsculas para facilidad de uso
  nombres_limpios <- tolower(trimws(nombres_limpios))
  
  colnames(df) <- nombres_limpios
  
  # Casteo de tipos básicos (asegurarnos que las métricas clave sean numéricas)
  df_clean <- df %>%
    mutate(
      variedad = as.character(variedad),
      madre = as.character(madre),
      padre = as.character(padre),
      adapt = as.character(adapt),
      status = as.character(status),
      estatus = as.character(estatus),
      
      # Numéricas (si alguna falla la coercion, se volverá NA de forma segura)
      tca = as.numeric(tca),
      rend = as.numeric(rend),
      taa = as.numeric(taa),
      
      y_score = as.numeric(y),
      q_score = as.numeric(q),
      carbon = as.numeric(carbon),
      roya = as.numeric(roya),
      es = as.numeric(es),
      disease = as.numeric(disease),
      
      agro = as.numeric(agro),
      gen = as.numeric(gen),
      factor = as.numeric(factor),
      
      maxest = as.numeric(maxest),
      sta = as.numeric(sta)
    )
  
  # Llenar posibles NAs en variables críticas con 0 o el mínimo aceptable para que no rompa la matemática
  df_clean <- df_clean %>%
    mutate(
      factor = if_else(is.na(factor), 0, factor),
      disease = if_else(is.na(disease), 0, disease),
      agro = if_else(is.na(agro), 0, agro),
      adapt = if_else(is.na(adapt), "Cualquiera", adapt)
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
