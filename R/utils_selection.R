# UTILS_SELECTION.R
sugerir_cruces <- function(matriz_A,
                           ids_madres,
                           ids_padres,
                           max_f         = 0.0625,
                           cat_var       = NULL,
                           df_categorias = NULL,
                           filtro_adapt  = NULL) {

  stopifnot(
    is.matrix(matriz_A),
    !is.null(rownames(matriz_A)),
    is.character(ids_madres),
    is.character(ids_padres)
  )

  id_map_name_to_id <- if (!is.null(cat_var)) stats::setNames(cat_var$id_variedad, cat_var$descripcion_variedad) else NULL
  id_map_id_to_name <- if (!is.null(cat_var)) stats::setNames(cat_var$descripcion_variedad, cat_var$id_variedad) else NULL
  
  ids_en_matriz <- rownames(matriz_A)
  
  traducir_ids <- function(ids) {
    if (is.null(id_map_name_to_id)) return(intersect(ids, ids_en_matriz))
    nombres_idx <- !(ids %in% ids_en_matriz)
    ids_out <- ids
    if (any(nombres_idx)) {
      traducidos <- id_map_name_to_id[ids[nombres_idx]]
      ids_out[nombres_idx] <- ifelse(!is.na(traducidos), traducidos, ids[nombres_idx])
    }
    intersect(ids_out, ids_en_matriz)
  }

  m_validos <- traducir_ids(ids_madres)
  p_validos <- traducir_ids(ids_padres)
  
  # Variedades fuera de la matriz → asumir F=0 (no consanguíneas, variedades nuevas)
  m_fuera <- setdiff(ids_madres, c(m_validos,
    if (!is.null(id_map_name_to_id)) id_map_name_to_id[ids_madres] else character(0)))
  p_fuera <- setdiff(ids_padres, c(p_validos,
    if (!is.null(id_map_name_to_id)) id_map_name_to_id[ids_padres] else character(0)))
  # Simplificado: cualquier madre/padre no encontrado en la matriz
  m_fuera <- ids_madres[!ids_madres %in% m_validos]
  p_fuera <- ids_padres[!ids_padres %in% p_validos]

  # Resultado base desde la Matriz A
  comb_matriz <- data.frame(Madre_ID=character(), Padre_ID=character(),
                             F_progenie=numeric(), Madre=character(), Padre=character(),
                             stringsAsFactors=FALSE)
  
  if (length(m_validos) > 0 && length(p_validos) > 0) {
    sub_A <- matriz_A[m_validos, p_validos, drop = FALSE]
    library(data.table)
    dt_comb <- as.data.table(sub_A, keep.rownames = "Madre_ID")
    comb <- melt(dt_comb, id.vars = "Madre_ID", variable.name = "Padre_ID", value.name = "A_val")
    comb <- comb[Madre_ID != Padre_ID]
    comb[, F_progenie := A_val / 2]
    comb[, A_val := NULL]
    comb <- as.data.frame(comb)
    comb$Padre_ID <- as.character(comb$Padre_ID)
    if (!is.null(id_map_id_to_name)) {
      comb$Madre <- dplyr::coalesce(id_map_id_to_name[comb$Madre_ID], comb$Madre_ID)
      comb$Padre <- dplyr::coalesce(id_map_id_to_name[comb$Padre_ID], comb$Padre_ID)
    } else {
      comb$Madre <- comb$Madre_ID
      comb$Padre <- comb$Padre_ID
    }
    comb_matriz <- comb
  }

  # Variedades nuevas (fuera de la matriz) → F=0 con todos los padres disponibles
  comb_nuevas <- data.frame(Madre_ID=character(), Padre_ID=character(),
                              F_progenie=numeric(), Madre=character(), Padre=character(),
                              stringsAsFactors=FALSE)
  
  todos_padres <- unique(c(p_validos, p_fuera))
  
  if (length(m_fuera) > 0 && length(todos_padres) > 0) {
    comb_m_nueva <- expand.grid(Madre_ID = m_fuera, Padre_ID = todos_padres,
                                 stringsAsFactors = FALSE) %>%
      dplyr::filter(Madre_ID != Padre_ID) %>%
      dplyr::mutate(F_progenie = 0, Madre = Madre_ID, Padre = Padre_ID)
    comb_nuevas <- bind_rows(comb_nuevas, comb_m_nueva)
  }
  
  if (length(p_fuera) > 0 && length(m_validos) > 0) {
    comb_p_nueva <- expand.grid(Madre_ID = m_validos, Padre_ID = p_fuera,
                                 stringsAsFactors = FALSE) %>%
      dplyr::filter(Madre_ID != Padre_ID) %>%
      dplyr::mutate(F_progenie = 0, Madre = Madre_ID, Padre = Padre_ID)
    comb_nuevas <- bind_rows(comb_nuevas, comb_p_nueva)
  }

  resultado <- bind_rows(comb_matriz, comb_nuevas)
  
  if (nrow(resultado) == 0) {
    warning("Faltan madres o machos validos en la matriz.")
    return(data.frame(Madre_ID=character(), Padre_ID=character(), F_progenie=numeric(),
                      Madre=character(), Padre=character()))
  }

  return(resultado[order(resultado$F_progenie), ])
}


assign_genetic_categories <- function(df_act, df_fam) {
  # Lista de variedades comerciales proporcionada por el usuario (Cat 4)
  comerciales_list <- c(
    "CR0010", "BR0402", "CR0408", "CR031009", "CR061005", "BR130019", "CR0140026", 
    "CR110021", "CR110022", "CR110019", "CR110011", "CR110029", "CR18004", "BR18004", 
    "CR18006", "CR092020", "CR092005", "CR87339", "CR83323", "CR951007", "CR93003", 
    "CR87220", "CR0026"
  )

  if (nrow(df_fam) == 0) {
    # Si no hay datos de familias, solo podemos categorizar por performance y comerciales
    df_exito <- data.frame(parent = character(), n_cruces = numeric(), tasa_exito = numeric())
  } else {
    # 1. Tasa de Exito Historica
    df_exito <- df_fam %>%
      mutate(parent = ifelse(!is.na(madre) & madre != "", madre, padre)) %>%
      group_by(parent) %>%
      summarise(
        n_cruces = n(),
        n_sel = sum(toupper(trimws(accion)) == "S", na.rm = TRUE),
        tasa_exito = n_sel / n_cruces,
        .groups = "drop"
      ) %>%
      filter(parent != "")
  }
  
  # 2. Categorizacion segun nuevas reglas
  df_cat <- df_act %>%
    left_join(df_exito, by = c("variedad" = "parent")) %>%
    mutate(
      # Asegurar que Y y Q sean numericos
      y = as.numeric(y),
      q = as.numeric(q),
      
      # Cat 1: Progeny Tested (Min 3 cruces evaluados y tasa >= 50%)
      es_cat1 = ifelse(!is.na(tasa_exito) & tasa_exito >= 0.5 & n_cruces >= 3, "C1: Progeny Tested", NA),
      
      # Cat 2: V.H.Q (Tonelaje <= 4 Y Calidad <= 4 en escala inversa)
      es_cat2 = ifelse(y <= 4 & q <= 4, "C2: V.H.Q", NA),
      
      # Cat 3: Alto Y o Q (Tonelaje <= 4 O Calidad <= 4)
      es_cat3 = ifelse(y <= 4 | q <= 4, "C3: Alto Y|Q", NA),
      
      # Cat 4: Variedades Comerciales
      es_cat4 = ifelse(variedad %in% comerciales_list, "C4: Comercial", NA),
      
      evf_info = ifelse(!is.na(n_cruces), paste0(n_sel, "/", n_cruces), "Sin Datos"),
      
      # Categorizacion Jerarquica
      categoria = case_when(
        !is.na(es_cat1) ~ es_cat1,
        !is.na(es_cat4) ~ es_cat4, # Priorizamos comercial despues de Progeny Tested
        !is.na(es_cat2) ~ es_cat2,
        !is.na(es_cat3) ~ es_cat3,
        TRUE ~ "C5: Exploratorio"
      )
    )
  
  return(df_cat)
}
