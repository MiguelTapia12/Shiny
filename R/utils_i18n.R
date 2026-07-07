# ==============================================================================
# utils_i18n.R — Motor de Traducción (Servidor R/Shiny)
# CR Breeding System — Módulo 8: Internacionalización
# ==============================================================================
# USO:
#   En global.R, carga el diccionario UNA sola vez:
#     i18n_dict <- load_i18n("www/traducciones.csv")
#
#   En cualquier módulo Shiny:
#     tr("axis_tca", input$lang)          # → "TCA (Ton/Acre)" o "TCA (Ton/Acre)"
#     tr("soil_good", input$lang)         # → "Bueno" o "Good"
#     tr("cat_elite", input$lang)         # → "Perfil Élite" o "Elite Profile"
#
#   El argumento `lang` acepta "es" o "en" (insensible a mayúsculas/minúsculas).
#   Si una clave no existe, devuelve la propia clave entre corchetes [key].
# ==============================================================================

# ── Carga el diccionario desde CSV ───────────────────────────────────────────
# Retorna: named list con sublistas "es" y "en", indexadas por key.
# Se llama UNA VEZ en global.R; el resultado se almacena como i18n_dict.

load_i18n <- function(path = "www/traducciones.csv") {
  if (!file.exists(path)) {
    warning(sprintf("[i18n] Archivo de traducciones no encontrado: %s. Usando claves como fallback.", path))
    return(list(es = list(), en = list()))
  }
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) {
      warning(sprintf("[i18n] Error al leer %s: %s", path, e$message))
      return(data.frame(key = character(), es = character(), en = character()))
    }
  )
  if (!all(c("key", "es", "en") %in% names(df))) {
    warning("[i18n] El CSV debe tener columnas: key, es, en")
    return(list(es = list(), en = list()))
  }
  dict <- list(
    es = setNames(as.list(df$es), df$key),
    en = setNames(as.list(df$en), df$key)
  )
  message(sprintf(">> [i18n] Diccionario cargado: %d claves.", nrow(df)))
  dict
}

# ── Función principal de traducción ──────────────────────────────────────────
# Argumentos:
#   key    : clave del diccionario (character, longitud 1 o vectorizada)
#   lang   : "es" o "en" (default "es")
#   dict   : el objeto retornado por load_i18n() — usa i18n_dict global si no se especifica
#
# Retorna: character del mismo largo que key

tr <- function(key, lang = "es", dict = NULL) {
  if (is.null(dict)) {
    if (exists("i18n_dict", envir = .GlobalEnv)) {
      dict <- get("i18n_dict", envir = .GlobalEnv)
    } else {
      warning("[i18n] i18n_dict no encontrado en global env. Usa load_i18n() en global.R")
      return(key)
    }
  }

  lang_norm <- tolower(trimws(lang))
  if (!lang_norm %in% c("es", "en")) lang_norm <- "es"

  lang_dict <- dict[[lang_norm]]

  vapply(key, function(k) {
    val <- lang_dict[[k]]
    if (is.null(val) || length(val) == 0 || is.na(val) || val == "") {
      return(paste0("[", k, "]"))
    }
    as.character(val)
  }, character(1), USE.NAMES = FALSE)
}

# ── Variante vectorizada con fallback legible ────────────────────────────────
# Útil para traducir vectores de categorías en ggplot (scale_x_discrete labels)
# Ejemplo: tr_vec(c("cat_elite","cat_heavy","cat_sweet"), input$lang)

tr_vec <- function(keys, lang = "es", dict = NULL) {
  tr(keys, lang = lang, dict = dict)
}

# ── Traducción de etiquetas de facets (soil names, stage names) ──────────────
# Devuelve un named vector para usar en facet_wrap/facet_grid labeller
# Ejemplo: facet_wrap(~suelo, labeller = as_labeller(tr_facet_soil(input$lang)))

tr_facet_soil <- function(lang = "es") {
  c(
    "BUENO"       = tr("soil_good",      lang),
    "ROCOSO"      = tr("soil_rocky",     lang),
    "MAL_DRENADO" = tr("soil_poor_drain", lang)
  )
}

tr_facet_stage <- function(lang = "es") {
  c(
    "ST1" = tr("stage_st1", lang),
    "ST2" = tr("stage_st2", lang),
    "ST3" = tr("stage_st3", lang),
    "ST4" = tr("stage_st4", lang),
    "ST5" = tr("stage_st5", lang)
  )
}

# ── Traducción de decisiones S/T/R ───────────────────────────────────────────
tr_decision <- function(decision_vec, lang = "es") {
  map <- c(
    "S" = tr("decision_select",  lang),
    "T" = tr("decision_witness", lang),
    "R" = tr("decision_reject",  lang)
  )
  dplyr::recode(decision_vec, !!!map, .default = decision_vec)
}

# ── Traducción de categorías varietales ──────────────────────────────────────
tr_category <- function(cat_vec, lang = "es") {
  map <- c(
    "Perfil \u00c9lite"  = tr("cat_elite",       lang),
    "Pesada"        = tr("cat_heavy",        lang),
    "Dulce"         = tr("cat_sweet",        lang),
    "Deficiente"    = tr("cat_deficient",    lang),
    "Equilibrada"   = tr("cat_balanced",     lang),
    "Industrial"    = tr("cat_industrial",   lang),
    "Comercial"     = tr("cat_commercial",   lang),
    "Pre-comercial" = tr("cat_precommercial", lang),
    "Nueva"         = tr("cat_new",          lang)
  )
  dplyr::recode(cat_vec, !!!map, .default = cat_vec)
}

# ── Helper para títulos de ejes según rasgo seleccionado ─────────────────────
tr_axis_trait <- function(trait_key, lang = "es", relative = FALSE) {
  base <- tr(trait_key, lang)
  if (isTRUE(relative)) {
    prefix <- if (lang == "en") "Relative % of " else "% Relativo de "
    return(paste0(prefix, base))
  }
  base
}
