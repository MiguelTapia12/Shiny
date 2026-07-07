---
title: "Reporte de Inteligencia Analítica - Isoproductividad"
output:
  pdf_document:
    toc: false
    df_print: kable
    keep_tex: false
    latex_engine: pdflatex
fontsize: 10.5pt
geometry: "top=2cm, bottom=2.2cm, left=2.4cm, right=2.4cm"
header-includes:
  - \usepackage{xcolor}
  - \usepackage{fancyhdr}
  - \usepackage{titlesec}
  - \usepackage{colortbl}
  - \usepackage{booktabs}
  - \usepackage{array}
  - \usepackage{calc}
  - \usepackage{graphicx}
  - \usepackage{framed}
  - \usepackage{mdframed}
  - \usepackage{tcolorbox}
  - \usepackage{tabularx}
  - \usepackage{microtype}
  - \usepackage{setspace}
  - \usepackage{enumitem}
  - \usepackage{multicol}
  - \usepackage{eso-pic}
  - \usepackage{pifont}
  - \tcbuselibrary{skins,breakable}
  - \definecolor{crgreen}{HTML}{0B5C2E}
  - \definecolor{crgreen2}{HTML}{15803D}
  - \definecolor{crgreen3}{HTML}{22C55E}
  - \definecolor{crgreenlight}{HTML}{E8F5EC}
  - \definecolor{crgreenxlight}{HTML}{F0FDF4}
  - \definecolor{crgray}{HTML}{4B5563}
  - \definecolor{crgraylight}{HTML}{F3F4F6}
  - \definecolor{crgrayxlight}{HTML}{F9FAFB}
  - \definecolor{cramber}{HTML}{B45309}
  - \definecolor{cramberlight}{HTML}{FFFBEB}
  - \definecolor{crblue}{HTML}{1D4ED8}
  - \definecolor{crbluelight}{HTML}{EFF6FF}
  - \definecolor{crred}{HTML}{B91C1C}
  - \definecolor{crredlight}{HTML}{FEF2F2}
  - \definecolor{craccent}{HTML}{065F46}
  - \definecolor{shadecolor}{HTML}{EAF7EE}
  - \definecolor{sidebar}{HTML}{0B5C2E}
  - \renewcommand{\maketitle}{}
  - \setlength{\headheight}{16pt}
  - \pagestyle{fancy}
  - \fancyhf{}
  - \renewcommand{\headrulewidth}{0pt}
  - \renewcommand{\footrulewidth}{0pt}
  - \fancyhead[L]{\colorbox{crgreen}{\parbox[c][10pt][c]{5pt}{\hspace{5pt}}}\hspace{4pt}\small\color{crgray}\textbf{CR Breeding}\enspace\textcolor{crgreen3}{|}\enspace Isoproductividad}
  - \fancyhead[R]{\small\color{crgray}\reportdate}
  - \fancyfoot[L]{\footnotesize\color{crgray}\textit{Confidencial — Uso interno}}
  - \fancyfoot[R]{\footnotesize\color{crgray}Página \thepage}
  - \titleformat{\section}{\large\bfseries\color{crgreen}}{}{0em}{}[\vspace{-2pt}\color{crgreen2}\rule{\textwidth}{1.4pt}\vspace{2pt}]
  - \titleformat{\subsection}{\normalsize\bfseries\color{crgreen2}}{}{0em}{}
  - \titlespacing*{\section}{0pt}{1.6em}{0.7em}
  - \titlespacing*{\subsection}{0pt}{1em}{0.4em}
  - \setlength{\parskip}{0.5em}
  - \setlength{\parindent}{0pt}
  - \let\oldsnugshade\snugshade
  - \let\endoldsnugshade\endsnugshade
  - \renewenvironment{snugshade}{\oldsnugshade\small\color{crgray}}{\endoldsnugshade}
params:
  iso_data: NA
  vars_sel: NA
  gradient: "Vice City"
  anio: NA
  suelo: NA
---



\newcommand{\reportdate}{29 de junio de 2026}


```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```
## Error:
## ! objeto 'n_vars' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```
## Error:
## ! objeto 'suelo_txt' no encontrado
```

# Resumen Ejecutivo


```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

# Curvas de Isoproductividad TAA

Las curvas de nivel representan combinaciones iguales de Toneladas de Azúcar por Acre (TAA). Los materiales ubicados sobre curvas más altas poseen mayor productividad integral, ya que maximizan la rentabilidad final, lográndolo ya sea por la vía de un mayor tonelaje (perfil pesado) o por una mayor concentración sacarina (perfil dulce). Las líneas de referencia indican las medianas del grupo, definiendo los cuatro cuadrantes de clasificación.


```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```{=latex}
\footnotesize{\color{crgray}$\circ$ \textbf{Candidata} \quad $\triangle$ \textbf{Testigo de control} (borde oscuro) \quad Líneas de referencia: medianas del grupo}\normalsize
```

# Análisis por Cuadrante


```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

\newpage

# Tabla Comparativa de Isoproductividad


```
## Error:
## ! objeto 'params' no encontrado
```

```
## Error:
## ! objeto 'df_iso' no encontrado
```

```{=latex}
\vspace{4pt}
{\footnotesize\color{crgray}
{\color{crblue}\rule{8pt}{6pt}}~\textbf{Testigo de control}\quad
{\color{crgreen3}\rule{8pt}{6pt}}~\textbf{Variedad candidata (fondo verde)}\quad
{\color{crgreen}\textbf{Negrita}}~Cuadrante Elite
}\normalsize
```

---

\vspace{6pt}
\begin{center}
\small\color{crgray}
\textbf{CR Breeding} --- Sistema de Mejoramiento Genetico $\cdot$ Central Romana Corp.\\[2pt]
\textit{Documento generado automaticamente por el Modulo de Inteligencia Analitica $\cdot$ \reportdate}
\end{center}
