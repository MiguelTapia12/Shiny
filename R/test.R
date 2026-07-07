params <-
list(iso_data = "NA", vars_sel = "NA", gradient = "Vice City", 
    anio = "NA", suelo = "NA")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 9.2, fig.height = 6.5, dpi = 180)
library(ggplot2)
library(knitr)
library(dplyr)
options(knitr.table.format = "latex")

tex_escape <- function(x) {
  if (is.null(x) || all(is.na(x))) return("")
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%#&$\\{\\}])", "\\\\\\1", x)
  x
}

meses_es <- c("enero","febrero","marzo","abril","mayo","junio",
              "julio","agosto","septiembre","octubre","noviembre","diciembre")
fecha_header <- sprintf("%d de %s de %d",
  as.integer(format(Sys.Date(), "%d")),
  meses_es[as.integer(format(Sys.Date(), "%m"))],
  as.integer(format(Sys.Date(), "%Y")))

clasificar_cuadrante <- function(tca, rend, med_tca, med_rend) {
  if (is.na(tca) || is.na(rend)) return("Sin datos")
  if (tca >= med_tca && rend >= med_rend) return("Elite")
  if (tca >= med_tca && rend <  med_rend) return("Pesada")
  if (tca <  med_tca && rend >= med_rend) return("Dulce")
  return("Deficiente")
}


## ----portada-cmd, results='asis'----------------------------------------------
cat(sprintf("\\newcommand{\\reportdate}{%s}\n", fecha_header))


## ----portada, results='asis'--------------------------------------------------
df_iso <- params$iso_data
anio_txt <- tex_escape(if (!is.null(params$anio) && !all(is.na(params$anio))) paste(params$anio, collapse=", ") else "Todos")
suelo_txt <- tex_escape(if (!is.null(params$suelo) && length(params$suelo) > 0) paste(params$suelo, collapse=", ") else "Todos los suelos")

n_vars <- if (!is.null(df_iso) && is.data.frame(df_iso)) nrow(df_iso) else 0
n_test <- if (!is.null(df_iso) && is.data.frame(df_iso) && "ind_testigo" %in% names(df_iso)) {
  sum(df_iso$ind_testigo == "S", na.rm = TRUE)
} else 0
n_cand <- n_vars - n_test

med_tca  <- if (!is.null(df_iso) && "TCA" %in% names(df_iso)) round(median(df_iso$TCA, na.rm=TRUE), 2) else "N/D"
med_rend <- if (!is.null(df_iso) && "Rendimiento" %in% names(df_iso)) round(median(df_iso$Rendimiento, na.rm=TRUE), 2) else "N/D"

if (!is.null(df_iso) && "TSH" %in% names(df_iso)) {
  med_taa <- round(median(df_iso$TSH, na.rm=TRUE), 2)
} else if (!is.null(df_iso) && "TCA" %in% names(df_iso) && "Rendimiento" %in% names(df_iso)) {
  med_taa <- round(median(df_iso$TCA * df_iso$Rendimiento / 100, na.rm=TRUE), 2)
} else {
  med_taa <- "N/D"
}

fecha_txt <- format(Sys.Date(), "%d/%m/%Y")

cat(sprintf("
\\begin{titlepage}
\\thispagestyle{empty}

\\AddToShipoutPictureBG*{%%
  \\AtPageLowerLeft{%%
    \\textcolor{crgreen}{\\rule{1.2cm}{\\paperheight}}%%
  }%%
}

\\vspace*{0.6cm}
\\hspace{1.8cm}%%
\\begin{minipage}{\\dimexpr\\textwidth-0.2cm}
  {\\fontsize{11}{13}\\selectfont\\color{crgray}\\textbf{CENTRAL ROMANA CORP.}}\\\\[2pt]
  {\\fontsize{9}{11}\\selectfont\\color{crgray} Programa de Mejoramiento Gen\\'etico de Ca\\~na de Az\\'ucar}
\\end{minipage}

\\vspace{0.5cm}
\\hspace{1.8cm}\\textcolor{crgreen2}{\\rule{\\dimexpr\\textwidth-0.2cm}{1.2pt}}
\\vspace{1.8cm}

\\hspace{1.8cm}%%
\\begin{minipage}{\\dimexpr\\textwidth-0.2cm}
  {\\fontsize{9}{11}\\selectfont\\color{cramber}\\textbf{\\MakeUppercase{INTELIGENCIA ANAL\\'ITICA}}}\\\\[10pt]
  {\\fontsize{28}{34}\\selectfont\\bfseries\\color{crgreen} Reporte de\\\\Isoproductividad}\\\\[10pt]
  {\\fontsize{13}{16}\\selectfont\\color{crgray} Curvas Iso TAA \\textperiodcentered{} An\\'alisis de Cuadrantes \\textperiodcentered{} Clasificaci\\'on Productiva}
\\end{minipage}

\\vspace{1.8cm}

\\hspace{1.8cm}%%
\\begin{minipage}{\\dimexpr\\textwidth-0.2cm}
  \\colorbox{crgreenxlight}{%%
    \\parbox{\\dimexpr\\textwidth-0.2cm-6pt}{%%
      \\vspace{5pt}%%
      \\hspace{4pt}{\\small\\color{crgray}\\textbf{SUELO(S):}\\quad}{\\small\\color{craccent}\\textit{%s}}\\quad|\\quad{\\small\\color{crgray}\\textbf{VARIEDADES ANALIZADAS:}\\quad}{\\small\\color{craccent}\\textit{%d (%d Candidatas, %d Testigos)}}%%
      \\vspace{5pt}%%
    }%%
  }
\\end{minipage}

\\vspace{1.4cm}

\\hspace{1.8cm}%%
\\begin{minipage}{\\dimexpr\\textwidth-0.2cm}
  \\begin{tabular}{p{2.8cm}p{2.8cm}p{2.8cm}p{2.8cm}p{2.8cm}}
    \\colorbox{crgreenlight}{\\parbox[c][2.0cm][c]{2.6cm}{\\centering
      {\\color{crgray}\\tiny\\textbf{CANDIDATAS}}\\\\[5pt]
      {\\color{crgreen}\\fontsize{22}{24}\\selectfont\\textbf{%d}}
    }} &
    \\colorbox{crbluelight}{\\parbox[c][2.0cm][c]{2.6cm}{\\centering
      {\\color{crgray}\\tiny\\textbf{TESTIGOS}}\\\\[5pt]
      {\\color{crblue}\\fontsize{22}{24}\\selectfont\\textbf{%d}}
    }} &
    \\colorbox{crgreenlight}{\\parbox[c][2.0cm][c]{2.6cm}{\\centering
      {\\color{crgray}\\tiny\\textbf{MED. TCA (T/Ac)}}\\\\[5pt]
      {\\color{crgreen}\\large\\textbf{%s}}
    }} &
    \\colorbox{crgreenlight}{\\parbox[c][2.0cm][c]{2.6cm}{\\centering
      {\\color{crgray}\\tiny\\textbf{MED. REND (\\%%)}}\\\\[5pt]
      {\\color{crgreen}\\large\\textbf{%s}}
    }} &
    \\colorbox{crgreenlight}{\\parbox[c][2.0cm][c]{2.6cm}{\\centering
      {\\color{crgray}\\tiny\\textbf{MED. TAA (T/Ac)}}\\\\[5pt]
      {\\color{crgreen}\\large\\textbf{%s}}
    }} \\\\
  \\end{tabular}
\\end{minipage}

\\vfill

\\hspace{1.8cm}\\textcolor{crgreen2}{\\rule{\\dimexpr\\textwidth-0.2cm}{0.6pt}}
\\\\[6pt]
\\hspace{1.8cm}%%
\\begin{minipage}{\\dimexpr\\textwidth-0.2cm}
  \\begin{tabular}{p{8cm} r}
    {\\small\\color{crgray} Generado por el M\\'odulo de Inteligencia Anal\\'itica} &
    {\\small\\color{crgray}\\textbf{%s}} \\\\
  \\end{tabular}
\\end{minipage}
\\end{titlepage}
",
suelo_txt, n_vars, n_cand, n_test, n_cand, n_test, med_tca, med_rend, med_taa, fecha_txt))


## ----resumen-ejecutivo, results='asis'----------------------------------------
df_iso <- params$iso_data

if (!is.null(df_iso) && is.data.frame(df_iso) && nrow(df_iso) > 0 &&
    "TCA" %in% names(df_iso) && "Rendimiento" %in% names(df_iso)) {

  med_tca  <- median(df_iso$TCA, na.rm = TRUE)
  med_rend <- median(df_iso$Rendimiento, na.rm = TRUE)

  df_iso$Cuadrante <- mapply(clasificar_cuadrante,
                              df_iso$TCA, df_iso$Rendimiento,
                              MoreArgs = list(med_tca = med_tca, med_rend = med_rend))

  n_elite <- sum(df_iso$Cuadrante == "Elite",      na.rm = TRUE)
  n_pes   <- sum(df_iso$Cuadrante == "Pesada",     na.rm = TRUE)
  n_dul   <- sum(df_iso$Cuadrante == "Dulce",      na.rm = TRUE)
  n_def   <- sum(df_iso$Cuadrante == "Deficiente", na.rm = TRUE)

  testigos_globales <- if("ind_testigo" %in% names(df_iso)) unique(df_iso$Variedad[df_iso$ind_testigo == "S"]) else character(0)

  cands_sorted <- df_iso %>% 
    filter(!(Variedad %in% testigos_globales)) %>%
    arrange(desc(if("TSH" %in% names(.)) TSH else (TCA * Rendimiento / 100)))

  lider <- if (nrow(cands_sorted) > 0) cands_sorted$Variedad[1] else NA
  lider_taa <- if (nrow(cands_sorted) > 0) {
    if("TSH" %in% names(cands_sorted)) cands_sorted$TSH[1] else (cands_sorted$TCA[1] * cands_sorted$Rendimiento[1] / 100)
  } else NA

  lider2 <- if (nrow(cands_sorted) > 1) cands_sorted$Variedad[2] else NA
  lider2_taa <- if (nrow(cands_sorted) > 1) {
    if("TSH" %in% names(cands_sorted)) cands_sorted$TSH[2] else (cands_sorted$TCA[2] * cands_sorted$Rendimiento[2] / 100)
  } else NA

  lider3 <- if (nrow(cands_sorted) > 2) cands_sorted$Variedad[3] else NA
  lider3_taa <- if (nrow(cands_sorted) > 2) {
    if("TSH" %in% names(cands_sorted)) cands_sorted$TSH[3] else (cands_sorted$TCA[3] * cands_sorted$Rendimiento[3] / 100)
  } else NA

  top_txt <- ""
  if (!is.na(lider2) && !is.na(lider3)) {
     top_txt <- sprintf("Dentro del grupo pre-comercial, las candidatas ubicadas en las líneas de TAA más altas, ocupando segundo y tercer lugar, son %s (%.2f T/Ac) y %s (%.2f T/Ac).", lider2, lider2_taa, lider3, lider3_taa)
  } else if (!is.na(lider2)) {
     top_txt <- sprintf("Dentro del grupo pre-comercial, la candidata ubicada en la siguiente línea de TAA más alta es %s (%.2f T/Ac).", lider2, lider2_taa)
  }

  lider_txt <- if (!is.na(lider) && !is.na(lider_taa)) {
    sprintf("**%s** como el material de mayor productividad integral en el grupo evaluado, produciendo **%.2f T/Ac** de TAA", lider, lider_taa)
  } else {
    "**N/D** como el material de mayor productividad integral en el grupo evaluado"
  }

  n_cand_r <- if ("ind_testigo" %in% names(df_iso)) sum(df_iso$ind_testigo != "S", na.rm=TRUE) else nrow(df_iso)
  n_test_r <- if ("ind_testigo" %in% names(df_iso)) sum(df_iso$ind_testigo == "S", na.rm=TRUE) else 0

  cat(sprintf(
"Este documento sintetiza el análisis de Curvas de Isoproductividad TAA para **%d variedades** (%d candidatas pre-comerciales, %d testigos de control) del Programa de Mejoramiento Genético Central Romana. El análisis identifica a %s.

La metodología clasifica cada variedad según su posición relativa respecto a las medianas de **TCA** (%.2f T/Ac), **Rendimiento** (%.2f\\%%) y **TAA**, generando cuatro perfiles productivos: **%d Élite** (alto TCA + alto Rendimiento), **%d Pesadas** (alto TCA + bajo Rendimiento), **%d Dulces** (bajo TCA + alto Rendimiento) y **%d Deficientes** (bajo en ambos rasgos).

\\vspace{0.4em}
\\noindent\\textbf{\\textcolor{crgreen}{Análisis de Productividad Integral:}} La curva de isoproductividad muestran cuales genotipos dominan la producción de azucar en un suelo determinado. El material **%s** se posiciona en la curva más alta (\\textbf{%.2f T/Ac} de TAA), demostrando superioridad absoluta en la producción de azúcar por área. %s",
  nrow(df_iso), n_cand_r, n_test_r,
  lider_txt,
  med_tca, med_rend,
  n_elite, n_pes, n_dul, n_def,
  lider, lider_taa, top_txt))

} else {
  cat("\\textcolor{crgray}{\\textit{No hay datos suficientes para generar el resumen ejecutivo.}}\n")
}


## ----plot-iso, fig.width=9.4, fig.height=6.8, fig.align='center', out.width="100%"----
df_iso <- params$iso_data

if (!is.null(df_iso) && is.data.frame(df_iso) && nrow(df_iso) > 0 &&
    "TCA" %in% names(df_iso) && "Rendimiento" %in% names(df_iso)) {

  testigos_vec <- if ("ind_testigo" %in% names(df_iso)) {
    df_iso$Variedad[df_iso$ind_testigo == "S"]
  } else character(0)

  tryCatch({
    create_iso_plot(
      media_df          = df_iso,
      testigos          = testigos_vec,
      selected_gradient = if (!is.null(params$gradient)) params$gradient else "Vice City",
      plot_title        = "Curvas de Isoproductividad TAA",
      y_title           = "TCA (Ton/Acre)",
      y2_title          = "TAA (Ton/Acre)",
      use_filled_contour = TRUE
    )
  }, error = function(e) {
    ggplot(df_iso, aes(x = Rendimiento, y = TCA, label = Variedad)) +
      geom_point(size = 3, color = "#0B5C2E") +
      geom_text(vjust = -0.7, size = 2.5, color = "#4B5563") +
      geom_vline(xintercept = median(df_iso$Rendimiento, na.rm=TRUE), linetype="dashed", color="#9CA3AF") +
      geom_hline(yintercept = median(df_iso$TCA, na.rm=TRUE), linetype="dashed", color="#9CA3AF") +
      labs(x="Rendimiento (%)", y="TCA (Ton/Acre)", title="Dispersión TCA vs Rendimiento") +
      theme_minimal(base_size = 11)
  })

} else {
  cat("\\textcolor{crgray}{\\textit{No hay datos suficientes para construir las curvas.}}\n")
}


## ----analisis-cuadrantes, results='asis'--------------------------------------
df_iso <- params$iso_data

if (!is.null(df_iso) && is.data.frame(df_iso) && nrow(df_iso) > 0 &&
    "TCA" %in% names(df_iso) && "Rendimiento" %in% names(df_iso)) {

  med_tca  <- median(df_iso$TCA, na.rm = TRUE)
  med_rend <- median(df_iso$Rendimiento, na.rm = TRUE)

  df_iso$Cuadrante <- mapply(clasificar_cuadrante,
                              df_iso$TCA, df_iso$Rendimiento,
                              MoreArgs = list(med_tca = med_tca, med_rend = med_rend))

  cuadrantes_info <- list(
    list(nombre="Elite",      color="crgreen",  bg="crgreenxlight",
         titulo="PERFIL ELITE -- Alto TCA + Alto Rendimiento",
         descripcion="Materiales con desempeño superior en ambas dimensiones productivas, garantizando alta rentabilidad agroindustrial.",
         recomendacion="AVANZAR. Candidatos ideales para liberación comercial. Maximizan la producción de azúcar por área sin sacrificar tonelaje en campo ni eficiencia de extracción en fábrica."),
    list(nombre="Pesada",     color="crblue",   bg="crbluelight",
         titulo="PERFIL PESADO -- Alto TCA + Bajo Rendimiento",
         descripcion="Materiales que logran su producción de azúcar a través de una gran biomasa (tonelaje), con menor concentración sacarina.",
         recomendacion="AVANZAR CON PRECAUCIÓN. Útiles para suelos limitantes donde el vigor es esencial. Tomar en cuenta que el alto volumen incrementa costos de transporte y tiempo de molienda."),
    list(nombre="Dulce",      color="cramber",  bg="cramberlight",
         titulo="PERFIL DULCE -- Bajo TCA + Alto Rendimiento",
         descripcion="Materiales que logran su producción de azúcar a través de alta calidad sacarina, compensando una menor producción de biomasa.",
         recomendacion="AVANZAR CON PRECAUCIÓN. Materiales de alta eficiencia industrial. Ideales para reducir costos logísticos y de molienda. Requieren ubicarse en suelos fértiles que soporten su desarrollo."),
    list(nombre="Deficiente", color="crred",    bg="crredlight",
         titulo="PERFIL DEFICIENTE -- Bajo TCA + Bajo Rendimiento",
         descripcion="Materiales con desempeño inferior a la media operativa tanto en campo (biomasa) como en fábrica (rendimiento).",
         recomendacion="DESCARTAR. No avanzar a fase comercial, a menos que posean rasgos defensivos excepcionales (ej. tolerancia a sequía o enfermedades) que justifiquen su conservación como progenitores.")
  )

  for (q_info in cuadrantes_info) {
    df_q <- df_iso %>%
      filter(Cuadrante == q_info$nombre) %>%
      arrange(desc(if("TSH" %in% names(.)) TSH else (TCA * Rendimiento / 100)))

    if (nrow(df_q) == 0) next

    n_q      <- nrow(df_q)
    cands_q_all <- df_q$Variedad[!(df_q$Variedad %in% testigos_globales)]
    n_cands  <- length(cands_q_all)

    top_q    <- df_q[1, ]
    lider_name <- top_q$Variedad
    top_tca  <- round(top_q$TCA[1], 2)
    top_rend <- round(top_q$Rendimiento[1], 2)
    top_tsh  <- if ("TSH" %in% names(top_q)) round(top_q$TSH[1], 2) else round(top_tca * top_rend / 100, 2)

    sintesis_txt <- sprintf("En este perfil se agrupan %d variedades precomerciales. ", n_cands)
    
    if (n_cands > 0) {
      cands_df <- df_q %>% filter(Variedad %in% cands_q_all) %>%
                  mutate(TAA_val = if ("TSH" %in% names(.)) round(TSH, 2) else round(TCA * Rendimiento / 100, 2))
                  
      lider_q_cand <- cands_df$Variedad[1]
      lider_q_taa <- cands_df$TAA_val[1]
      
      sintesis_txt <- paste0(sintesis_txt, sprintf("El material más destacado es **%s** con un TAA de %.2f T/Ac.", lider_q_cand, lider_q_taa))
      
      if (n_cands > 1) {
        if (n_cands == 2) {
          sintesis_txt <- paste0(sintesis_txt, sprintf(" La segunda destacada es %s (%.2f T/Ac).", cands_df$Variedad[2], cands_df$TAA_val[2]))
        } else {
          otros_cands <- cands_df$Variedad[-1]
          otros_taas <- cands_df$TAA_val[-1]
          
          if (length(otros_cands) > 6) {
            otros_cands <- head(otros_cands, 6)
            otros_taas <- head(otros_taas, 6)
          }
          lista_cands <- paste(sprintf("%s (%.2f)", otros_cands, otros_taas), collapse = ", ")
          sintesis_txt <- paste0(sintesis_txt, " Las candidatas pre-comerciales restantes aquí son: ", lista_cands, ".")
        }
      }
    } else {
      sintesis_txt <- "No hay variedades precomerciales clasificadas en este perfil."
    }

    cat(sprintf(
"\\vspace{0.6em}
\\noindent
\\begin{tcolorbox}[enhanced,breakable,
  colback=%s,colframe=%s,
  leftrule=5pt,toprule=0.4pt,bottomrule=0.4pt,rightrule=0.4pt,
  arc=2pt,boxsep=3pt,left=8pt,right=8pt,top=7pt,bottom=7pt]
{\\normalsize\\bfseries\\color{%s} %s}\\quad
{\\colorbox{%s}{\\small\\color{%s}\\textbf{N = %d}}}

\\vspace{4pt}{\\small\\color{crgray}
%s}

\\vspace{3pt}
{\\color{%s}\\textbf{Lider del perfil:}} %s --- TCA: \\textbf{%.2f T/Ac}, Rend: \\textbf{%.2f\\%%}, TAA: \\textbf{%.2f T/Ac}

\\vspace{3pt}
{\\color{craccent}\\textit{\\textbf{Analisis:}}} %s

\\vspace{3pt}
\\textbf{Síntesis:} {\\small\\color{crgray} %s}
\\end{tcolorbox}

",
      q_info$bg, q_info$color,
      q_info$color, q_info$titulo,
      q_info$color, q_info$bg, n_q,
      q_info$descripcion,
      q_info$color, tex_escape(lider_name),
      top_tca, top_rend, top_tsh,
      q_info$recomendacion,
      sintesis_txt
    ))
  }

} else {
  cat("\\textcolor{crgray}{\\textit{No hay datos suficientes para analizar cuadrantes.}}\n")
}


## ----tabla-iso, results='asis'------------------------------------------------
df_iso <- params$iso_data

if (!is.null(df_iso) && is.data.frame(df_iso) && nrow(df_iso) > 0 &&
    "TCA" %in% names(df_iso) && "Rendimiento" %in% names(df_iso)) {

  med_tca  <- median(df_iso$TCA, na.rm = TRUE)
  med_rend <- median(df_iso$Rendimiento, na.rm = TRUE)

  df_iso$Cuadrante <- mapply(clasificar_cuadrante,
                              df_iso$TCA, df_iso$Rendimiento,
                              MoreArgs = list(med_tca = med_tca, med_rend = med_rend))

  df_tabla <- df_iso %>%
    mutate(
      Tipo        = if("ind_testigo" %in% names(.)) ifelse(ind_testigo=="S","Testigo","Candidata") else "Candidata",
      TCA         = round(TCA, 2),
      Rendimiento = round(Rendimiento, 2),
      TAA         = if("TSH" %in% names(.)) round(TSH, 2) else round(TCA * Rendimiento / 100, 2)
    ) %>%
    arrange(desc(TAA)) %>%
    select(Variedad, Tipo, Cuadrante, `TCA (T/Ac)` = TCA,
           `Rend (\\%)` = Rendimiento, `TAA (T/Ac)` = TAA)

  testigo_idx <- which(df_tabla$Tipo == "Testigo")
  cand_idx    <- which(df_tabla$Tipo == "Candidata")
  elite_idx   <- which(df_tabla$Cuadrante == "Elite")

  if (requireNamespace("kableExtra", quietly = TRUE)) {
    library(kableExtra)
    k <- knitr::kable(
        df_tabla, format = "latex", booktabs = TRUE, escape = FALSE,
        align = c("l","c","c","r","r","r"),
        caption = "Clasificacion de Materiales por Cuadrante de Isoproductividad"
      ) %>%
      kable_styling(latex_options = c("hold_position"),
                    font_size = 8.5, full_width = FALSE) %>%
      row_spec(0, bold = TRUE, color = "white", background = "#0B5C2E") %>%
      row_spec(testigo_idx, background = "#EFF6FF", color = "#1D4ED8") %>%
      row_spec(cand_idx,    background = "#F0FDF4") %>%
      row_spec(elite_idx,   bold = TRUE)
    k
  } else {
    knitr::kable(df_tabla, caption = "Clasificacion de Materiales por Cuadrante")
  }
} else {
  cat("\\textcolor{crgray}{\\textit{No hay datos tabulares disponibles.}}\n")
}

