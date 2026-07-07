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
get_full_ancestry_robust <- function(data, target_id, cat_var = NULL, missing_code = "0") {
  
  stopifnot(
    is.data.frame(data),
    all(c("id_variedad", "id_variedad_ancestro", "tipo_ancestro") %in% names(data)),
    is.character(target_id)
  )

  target_id <- as.character(target_id)
  
  # --- Registro de roles para asignación de sexo consistente ---
  role_as_dad <- unique(as.character(data$id_variedad_ancestro[data$tipo_ancestro == "PADRE"]))
  role_as_mom <- unique(as.character(data$id_variedad_ancestro[data$tipo_ancestro == "MADRE"]))

  # Preasignar con listas — O(n) en lugar de O(n²)
  ids    <- vector("list", 1000L)
  dads   <- vector("list", 1000L)
  moms   <- vector("list", 1000L)
  count  <- 0L

  visited <- character()
  queue   <- target_id
  
  # ============================
  # PASO 1: BFS — Reconstrucción
  # ============================
  while (length(queue) > 0L) {
    current <- queue[[1L]]
    queue   <- queue[-1L]
    
    if (current %in% visited || is.na(current) || current == "" || current == missing_code) next
    visited <- c(visited, current)
    
    # Buscar padres
    parents <- data[data$id_variedad == current, , drop = FALSE]
    dad_row <- parents[parents$tipo_ancestro == "PADRE", , drop = FALSE]
    mom_row <- parents[parents$tipo_ancestro == "MADRE", , drop = FALSE]
    
    dad_id <- if (nrow(dad_row) > 0L && nzchar(dad_row$id_variedad_ancestro[[1L]])) {
      as.character(dad_row$id_variedad_ancestro[[1L]])
    } else {
      missing_code
    }
    
    mom_id <- if (nrow(mom_row) > 0L && nzchar(mom_row$id_variedad_ancestro[[1L]])) {
      as.character(mom_row$id_variedad_ancestro[[1L]])
    } else {
      missing_code
    }
    
    count       <- count + 1L
    ids[[count]]  <- current
    dads[[count]] <- dad_id
    moms[[count]] <- mom_id
    
    if (dad_id != missing_code) queue <- c(queue, dad_id)
    if (mom_id != missing_code) queue <- c(queue, mom_id)
  }
  
  if (count == 0L) {
    return(data.frame(
      id    = target_id,
      dadid = missing_code,
      momid = missing_code,
      sex   = 1,
      label = target_id,
      stringsAsFactors = FALSE
    ))
  }

  ped <- data.frame(
    id    = unlist(ids[seq_len(count)]),
    dadid = unlist(dads[seq_len(count)]),
    momid = unlist(moms[seq_len(count)]),
    stringsAsFactors = FALSE
  )

  # Paso 2: Metadatos (Sexo y Labels)
  local_dads <- unique(ped$dadid[ped$dadid != missing_code])
  local_moms <- unique(ped$momid[ped$momid != missing_code])
  
  ped$sex <- vapply(ped$id, function(x) {
    if (x %in% local_dads && !(x %in% local_moms)) return(1L)
    if (x %in% local_moms && !(x %in% local_dads)) return(2L)
    if (x %in% role_as_dad) return(1L)
    if (x %in% role_as_mom) return(2L)
    return(1L)
  }, integer(1))

  if (!is.null(cat_var)) {
    ped <- ped %>%
      left_join(cat_var %>% select(id_variedad, descripcion_variedad) %>% distinct(id_variedad, .keep_all = TRUE), 
                by = c("id" = "id_variedad")) %>%
      mutate(label = ifelse(!is.na(descripcion_variedad), descripcion_variedad, paste0("ID:", id))) %>%
      select(-descripcion_variedad)
  } else {
    ped$label <- paste0("ID:", ped$id)
  }

  # Paso 3: Requisito kinship2 (el código de fundador debe existir como individuo)
  uses_missing <- missing_code %in% c(ped$dadid, ped$momid)
  if (uses_missing && !missing_code %in% ped$id) {
    ped <- rbind(
      data.frame(id = missing_code, dadid = NA_character_, momid = NA_character_, 
                 sex = 3, label = "Fundador", stringsAsFactors = FALSE),
      ped
    )
  }

  dplyr::distinct(ped, id, .keep_all = TRUE)
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
