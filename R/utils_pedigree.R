# ==============================================================================
# UTILS_PEDIGREE.R — Funciones de Ancestría y Pedigrí
# Pipeline de Selección Genética — Central Romana
# ==============================================================================
# Funciones para reconstruir árboles genealógicos compatibles con kinship2.
# Resuelve el problema de integridad referencial: cada dadid/momid debe existir
# como un ID en la tabla de individuos.
# ==============================================================================

# ------------------------------------------------------------------------------
# get_full_ancestry_robust()
# 
# Reconstruye la ancestría completa de una variedad objetivo usando BFS.
# 
# DIFERENCIAS VS versión anterior:
# 1. Usa IDs ÚNICOS por fundador desconocido (no un "0" global)
#    → "UNK_DAD_<id_hijo>" y "UNK_MOM_<id_hijo>"
# 2. Asigna sexo consistente basado en rol (PADRE→1, MADRE→2)
# 3. Protección contra loops circulares
# 4. Validación de integridad referencial post-BFS
# 5. Incluye label (nombre comercial) del catálogo
#
# @param data    data.frame con columnas: id_variedad, id_variedad_ancestro, tipo_ancestro
# @param target_id  ID de la variedad objetivo (character)
# @param cat_var    data.frame del catálogo con id_variedad, descripcion_variedad
# @return data.frame con columnas: id, dadid, momid, sex, label
# ------------------------------------------------------------------------------
get_full_ancestry_robust <- function(data, target_id, cat_var = NULL) {
  
  target_id <- as.character(target_id)
  
  # Estructuras de trabajo
  visited <- character()
  ped <- data.frame(
    id     = character(),
    dadid  = character(),
    momid  = character(),
    sex    = integer(),
    label  = character(),
    stringsAsFactors = FALSE
  )
  queue <- target_id
  
  # --- Registro de roles para asignación de sexo consistente ---
  # Un ID puede aparecer como PADRE en un cruce y MADRE en otro.
  # Recolectamos TODOS los roles antes de asignar sexo.
  role_as_dad <- unique(as.character(
    data$id_variedad_ancestro[data$tipo_ancestro == "PADRE"]
  ))
  role_as_mom <- unique(as.character(
    data$id_variedad_ancestro[data$tipo_ancestro == "MADRE"]
  ))
  
  # ============================
  # PASO 1: BFS — Reconstrucción
  # ============================
  while (length(queue) > 0) {
    current <- queue[1]
    queue   <- queue[-1]
    
    # Protección: ya visitado, NA, o vacío
    if (current %in% visited || is.na(current) || current == "") next
    visited <- c(visited, current)
    
    # Buscar padres en la tabla de parentesco
    parents <- data %>%
      filter(id_variedad == current, id_variedad != id_variedad_ancestro)
    
    dad_raw <- parents %>%
      filter(tipo_ancestro == "PADRE") %>%
      pull(id_variedad_ancestro) %>%
      .[1]
    
    mom_raw <- parents %>%
      filter(tipo_ancestro == "MADRE") %>%
      pull(id_variedad_ancestro) %>%
      .[1]
    
    # --- Manejo de fundadores desconocidos con IDs ÚNICOS ---
    if (is.null(dad_raw) || is.na(dad_raw) || dad_raw == "") {
      dad_id <- paste0("UNK_DAD_", current)
    } else {
      dad_id <- as.character(dad_raw)
    }
    
    if (is.null(mom_raw) || is.na(mom_raw) || mom_raw == "") {
      mom_id <- paste0("UNK_MOM_", current)
    } else {
      mom_id <- as.character(mom_raw)
    }
    
    # Agregar individuo actual al pedigrí
    ped <- rbind(ped, data.frame(
      id     = current,
      dadid  = dad_id,
      momid  = mom_id,
      sex    = NA_integer_,
      label  = NA_character_,
      stringsAsFactors = FALSE
    ))
    
    # Encolar padres reales (no los UNK_*) para seguir explorando
    if (!grepl("^UNK_", dad_id)) queue <- c(queue, dad_id)
    if (!grepl("^UNK_", mom_id)) queue <- c(queue, mom_id)
  }
  
  # ==============================================
  # PASO 2: Agregar fundadores desconocidos (UNK_*)
  # ==============================================
  # Cada UNK_DAD_* y UNK_MOM_* referenciado debe existir como fila
  all_parents <- unique(c(ped$dadid, ped$momid))
  unknown_parents <- all_parents[grepl("^UNK_", all_parents)]
  unknown_parents <- setdiff(unknown_parents, ped$id)
  
  if (length(unknown_parents) > 0) {
    unk_rows <- data.frame(
      id     = unknown_parents,
      dadid  = NA_character_,
      momid  = NA_character_,
      sex    = NA_integer_,
      label  = NA_character_,
      stringsAsFactors = FALSE
    )
    ped <- rbind(ped, unk_rows)
  }
  
  # ========================================================
  # PASO 3: Verificar padres reales que falten como individuos
  # ========================================================
  # Algunos ancestros pueden no haber sido procesados por el BFS
  # (ej: un padre que no tiene datos de parentesco propio)
  all_referenced <- unique(c(ped$dadid, ped$momid))
  all_referenced <- all_referenced[!is.na(all_referenced)]
  missing_ids <- setdiff(all_referenced, ped$id)
  
  if (length(missing_ids) > 0) {
    missing_rows <- data.frame(
      id     = missing_ids,
      dadid  = NA_character_,
      momid  = NA_character_,
      sex    = NA_integer_,
      label  = NA_character_,
      stringsAsFactors = FALSE
    )
    ped <- rbind(ped, missing_rows)
  }
  
  # Eliminar duplicados
  ped <- ped %>% distinct(id, .keep_all = TRUE)
  
  # =======================================
  # PASO 4: Asignación de sexo CONSISTENTE
  # =======================================
  # Regla de prioridad:
  #   1. Si aparece como dadid en el pedigrí local → sex = 1 (macho)
  #   2. Si aparece como momid en el pedigrí local → sex = 2 (hembra)
  #   3. Si es UNK_DAD_* → sex = 1
  #   4. Si es UNK_MOM_* → sex = 2
  #   5. Si aparece como PADRE en datos globales → sex = 1
  #   6. Si aparece como MADRE en datos globales → sex = 2
  #   7. Default (ej: la variedad objetivo) → sex = 1
  
  local_dads <- unique(ped$dadid[!is.na(ped$dadid)])
  local_moms <- unique(ped$momid[!is.na(ped$momid)])
  
  ped$sex <- sapply(ped$id, function(x) {
    if (x %in% local_dads && !(x %in% local_moms)) return(1L)
    if (x %in% local_moms && !(x %in% local_dads)) return(2L)
    if (x %in% local_dads && x %in% local_moms)     return(1L)  # Conflicto: priorizar macho
    if (grepl("^UNK_DAD_", x)) return(1L)
    if (grepl("^UNK_MOM_", x)) return(2L)
    if (x %in% role_as_dad) return(1L)
    if (x %in% role_as_mom) return(2L)
    return(1L)  # Default
  })
  
  # =======================================
  # PASO 5: Labels (nombres comerciales)
  # =======================================
  if (!is.null(cat_var)) {
    cat_lookup <- cat_var %>%
      select(id_variedad, descripcion_variedad) %>%
      distinct(id_variedad, .keep_all = TRUE)
    
    ped <- ped %>%
      left_join(cat_lookup, by = c("id" = "id_variedad")) %>%
      mutate(label = ifelse(
        !is.na(descripcion_variedad),
        descripcion_variedad,
        ifelse(grepl("^UNK_", id), "Desconocido", paste0("ID:", id))
      )) %>%
      select(-descripcion_variedad)
  } else {
    ped$label <- ifelse(
      grepl("^UNK_", ped$id),
      "Desconocido",
      paste0("ID:", ped$id)
    )
  }
  
  # ==========================================
  # PASO 6: Fundadores (UNK_*) → dadid/momid = NA
  # ==========================================
  # kinship2 con IDs character requiere NA (no "0") para indicar
  # "sin padre/madre conocido". Los fundadores UNK_* ya existen como filas,
  # y sus dadid/momid ya son NA. No necesitamos hacer nada aquí.
  # Solo verificamos consistencia.
  
  # ==========================================
  # PASO 7: Validación final de integridad
  # ==========================================
  all_ids_final <- ped$id
  all_dads_final <- ped$dadid[!is.na(ped$dadid)]
  all_moms_final <- ped$momid[!is.na(ped$momid)]
  
  orphan_dads <- setdiff(all_dads_final, all_ids_final)
  orphan_moms <- setdiff(all_moms_final, all_ids_final)
  
  if (length(orphan_dads) > 0 || length(orphan_moms) > 0) {
    warning(paste0(
      "Integridad referencial comprometida. Padres huérfanos: ",
      paste(c(orphan_dads, orphan_moms), collapse = ", ")
    ))
  }
  
  return(ped)
}


# ------------------------------------------------------------------------------
# build_kinship2_pedigree()
#
# Construye un objeto pedigree de kinship2 a partir del resultado de
# get_full_ancestry_robust(). Maneja la conversión de IDs character a numeric
# que requiere kinship2.
#
# @param ped_data  data.frame de get_full_ancestry_robust()
# @param target_id ID de la variedad objetivo para resaltarla
# @return objeto pedigree de kinship2
# ------------------------------------------------------------------------------
build_kinship2_pedigree <- function(ped_data, target_id) {
  
  target_id <- as.character(target_id)
  
  # kinship2::pedigree() con IDs character requiere:
  #   - dadid/momid = NA para fundadores (no "0")
  #   - Cada dadid/momid que no sea NA debe existir en id
  #   - sex debe ser consistente (1=macho, 2=hembra)
  
  # Asegurar que dadid y momid sean NA para fundadores (no "0")
  dadid_clean <- ped_data$dadid
  momid_clean <- ped_data$momid
  dadid_clean[dadid_clean == "0"] <- NA
  momid_clean[momid_clean == "0"] <- NA
  
  ped_obj <- pedigree(
    id    = ped_data$id,
    dadid = dadid_clean,
    momid = momid_clean,
    sex   = ped_data$sex,
    affected = ifelse(ped_data$id == target_id, 1, 0)
  )
  
  return(ped_obj)
}


# ------------------------------------------------------------------------------
# plot_pedigree_robust()
#
# Grafica el pedigrí de una variedad con nombres comerciales, colores,
# y leyenda.
#
# @param data       data.frame de parentesco (pedigree_var)
# @param cat_var    data.frame del catálogo
# @param target     ID o nombre de la variedad objetivo
# @param title      Título del gráfico (opcional)
# @return invisible pedigree object
# ------------------------------------------------------------------------------
plot_pedigree_robust <- function(data, cat_var, target, title = NULL) {
  
  # === 1. Resolver target: nombre → ID ===
  target_char <- as.character(target)
  
  if (!grepl("^[0-9]+$", target_char)) {
    # Es un nombre comercial
    id_info <- cat_var %>%
      filter(
        tolower(descripcion_variedad) == tolower(target_char) |
        tolower(id_variedad) == tolower(target_char)
      ) %>%
      slice(1)
    
    if (nrow(id_info) == 0) stop(paste("Variedad no encontrada:", target))
    
    target_id <- as.character(id_info$id_variedad)
    nombre_target <- id_info$descripcion_variedad
  } else {
    target_id <- target_char
    nombre_target <- cat_var %>%
      filter(id_variedad == target_id) %>%
      pull(descripcion_variedad) %>%
      .[1]
    if (is.na(nombre_target)) nombre_target <- paste0("ID:", target_id)
  }
  
  if (is.null(title)) title <- paste("Pedigree de", nombre_target)
  
  # === 2. Construir pedigrí robusto ===
  ped_data <- get_full_ancestry_robust(data, target_id, cat_var)
  
  # === 3. Construir objeto kinship2 ===
  ped_obj <- build_kinship2_pedigree(ped_data, target_id)
  
  # === 4. Graficar ===
  id_colors <- ifelse(ped_data$id == target_id, "red", "black")
  
  plot(ped_obj,
       col = id_colors,
       symbolsize = 1.3,
       cex = 0.7,
       mar = c(6, 4, 4, 4),
       main = title,
       id = ped_data$label,
       cex.id = 1.0)
  
  legend("bottomleft",
         legend = c("Macho (Padre)", "Hembra (Madre)", "Variedad objetivo"),
         pch = c(0, 1, 16),
         col = c("black", "black", "red"),
         pt.cex = 1.5, cex = 0.8,
         bty = "o", box.col = "black", box.lwd = 1,
         inset = c(0.02, 0.02))
  
  invisible(ped_obj)
}
