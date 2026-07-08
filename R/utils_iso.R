# utils_iso.R
# Utilidades para Gráficos de Isoproductividad
# Basado en el script original Iso series_plano.R

library(ggplot2)
library(dplyr)
library(ggnewscale)
library(ggpubr)

inst_colors <- list(
  "Rosado" = "#ff647d",
  "Verde" = "#26D07C",
  "Morado" = "#5F249F",
  "Fucsia" = "#f12391",
  "Naranja" = "#ffbf00",
  "Azul" = "#54bbdb",
  "Rojo" = "#e63946",
  "Amarillo" = "#ffd60a",
  "Azul Normal" = "#1B22E0",
  "Turquesa" = "#1abc9c",
  "Gris" = "#e2e2e2",
  "Malva" = "#f1d7ff"
)

gradient_options <- list(
  "Vice City" = list(name = "Vice City", low = "#3494E6", high = "#EC6EAD"),
  "Evening Night" = list(name = "Evening Night", low = "#005AA7", high = "#FFFDE4"),
  "Ibiza Sunset" = list(name = "Ibiza Sunset", low = "#ee0979", high = "#ff6a00"),
  "Purple Love" = list(name = "Purple Love", low = "#cc2b5e", high = "#753a88"),
  "Green Beach" = list(name = "Green Beach", low = "#02AAB0", high = "#00CDAC"),
  "Deep Blue" = list(name = "Deep Blue", low = "#6D8AE7", high = "#0A2A8B"),
  "Flickr" = list(name = "Flickr", low = "#40c9ff", high = "#e81cff"),
  "Sublime Vivid" = list(name = "Sublime Vivid", low = "#ff9a44", high = "#fcff9e"),
  "Blush" = list(name = "Blush", low = "#ff6e7f", high = "#bfe9ff"),
  "Moonlit Asteroid" = list(name = "Moonlit Asteroid", low = "#0f2027", high = "#2c5364"),
  "MegaTron" = list(name = "MegaTron", low = "#c6ffdd", high = "#f7797d"),
  "DG" = list(name = "DG", low = "#ff00cc", high = "#333399"), 
  "Azure Pop" = list(name = "Azure Pop", low = "#ef32d9", high = "#89fffd"),
  "Tranfile" = list(name = "Tranfile", low = "#16bffd", high = "#cb3066"),
  "Timber" = list(name = "Timber", low = "#00dbde", high = "#fc00ff"),
  "Sky" = list(name = "Sky", low = "#fff", high = "#076585")
)

create_iso_plot <- function(
    media_df,  # Data frame con: Variedad, TCH, Sacarosa, TSH
    testigos = c(),  
    colors_input = "",  
    x_min = NA, x_max = NA,  
    y_min = NA, y_max = NA,  
    use_filled_contour = TRUE,  
    selected_gradient = "default",  
    bins = 10,  
    size_c = 0.5,  
    shape = 21,  
    size_p = 3,  
    label_mode = "Todas las etiquetas",  
    size_va = 3,  
    x_title = "Rendimiento (%)",  
    y_title = "TCA (Ton/Ha)",  
    y2_title = "TSH (Ton/Ha)",  
    plot_title = "Iso Plot",  
    axis_title_size = 12  
) {
  
  if ("Rendimiento" %in% colnames(media_df) && !"Sacarosa" %in% colnames(media_df)) {
    media_df <- media_df %>% rename(SAC = Rendimiento)
  } else if ("Sacarosa" %in% colnames(media_df)) {
    media_df <- media_df %>% rename(SAC = Sacarosa)
  } else {
    stop("Data must have 'Sacarosa' or 'Rendimiento' column.")
  }
  
  if ("TCA" %in% colnames(media_df) && !"TCH" %in% colnames(media_df)) {
    media_df <- media_df %>% rename(TCH = TCA)
  }
  
  if (nrow(media_df) == 0) {
    return(ggplot() + geom_text(aes(x = 0, y = 0, label = "No hay datos disponibles para estos filtros."), size = 6) + theme_void())
  }
  if (sum(is.finite(media_df$TCH)) == 0 || sum(is.finite(media_df$SAC)) == 0) {
    return(ggplot() + geom_text(aes(x = 0, y = 0, label = "No hay valores válidos de TCA o Rendimiento."), size = 6) + theme_void())
  }
  
  testigos <- sort(testigos)
  testigos <- intersect(testigos, media_df$Variedad)
  
  media_df <- media_df %>% mutate(check = ifelse(Variedad %in% testigos, Variedad, "Z"))
  levels_check <- unique(c(testigos, "Z"))
  media_df$check <- factor(media_df$check, levels = levels_check)
  
  # Asignación de colores
  color_input <- trimws(colors_input)
  if (color_input != "") {
    color_pairs <- unlist(strsplit(color_input, ";"))
    color_pairs <- color_pairs[color_pairs != ""]
    
    color_mapping <- list()
    for (pair in color_pairs) {
      pair_split <- unlist(strsplit(pair, ":"))
      if (length(pair_split) != 2) next
      variedad <- trimws(pair_split[1])
      color_name <- trimws(pair_split[2])
      if (color_name %in% names(inst_colors)) {
        color_mapping[[variedad]] <- inst_colors[[color_name]]
      }
    }
    color_mapping <- unlist(color_mapping)
    
    if ("other" %in% names(color_mapping)) {
      color_mapping["Z"] <- color_mapping["other"]
      color_mapping <- color_mapping[names(color_mapping) != "other"]
    }
    
    colors_to_use <- rep(inst_colors[["Morado"]], length(levels_check))
    names(colors_to_use) <- levels_check
    for (level in levels_check) {
      if (level %in% names(color_mapping)) {
        colors_to_use[level] <- color_mapping[level]
      }
    }
  } else {
    n_colors_needed <- length(levels_check)
    if (n_colors_needed == 1) {
      colors_to_use <- c("Z" = inst_colors[["Azul Normal"]])
    } else {
      colors_to_use <- c(head(unlist(inst_colors), n_colors_needed - 1), inst_colors[["Gris"]])
      names(colors_to_use) <- levels_check
    }
  }
  
  if (!is.na(x_min) && !is.na(x_max)) {
    # Zoom limits provided
  } else {
    min_sac <- min(media_df$SAC, na.rm = TRUE) * 0.9
    max_sac <- max(media_df$SAC, na.rm = TRUE) * 1.1
    x_min <- floor(min_sac / 0.5) * 0.5
    x_max <- ceiling(max_sac / 0.5) * 0.5
    if (x_min >= x_max) { x_min <- x_min - 1; x_max <- x_max + 1 }
  }
  
  if (!is.na(y_min) && !is.na(y_max)) {
    # Zoom limits provided
  } else {
    min_tch <- min(media_df$TCH, na.rm = TRUE) * 0.9
    max_tch <- max(media_df$TCH, na.rm = TRUE) * 1.1
    y_min <- floor(min_tch / 10) * 10
    y_max <- ceiling(max_tch / 10) * 10
    if (y_min >= y_max) { y_min <- y_min - 10; y_max <- y_max + 10 }
  }
  
  # Ensure dynamic breaks based on range difference
  step_x <- max(0.1, round((x_max - x_min) / 6, 1))
  step_y <- max(1, round((y_max - y_min) / 6, 0))
  
  major_breaks_x <- seq(from = x_min, to = x_max, by = step_x)
  major_breaks_y <- seq(from = y_min, to = y_max, by = step_y)
  
  grid <- expand.grid(
    SAC = seq(x_min, x_max, length.out = 200),
    TCH = seq(y_min, y_max, length.out = 200)
  ) %>% distinct()
  grid$TSH <- grid$TCH * grid$SAC / 100
  grid <- grid %>% filter(is.finite(TSH))
  
  tsh_min <- round((min(major_breaks_y) * max(major_breaks_x)) / 100, 2)
  tsh_max <- round((max(major_breaks_y) * max(major_breaks_x)) / 100, 2)
  n_breaks <- max(3, length(major_breaks_y))
  tsh_breaks <- seq(tsh_min, tsh_max, length.out = n_breaks)
  
  TSHLabel <- data.frame(
    TSH = tsh_breaks,
    SAC = max(major_breaks_x, na.rm = TRUE)
  )
  TSHLabel$TCH <- TSHLabel$TSH * 100 / TSHLabel$SAC
  
  p <- ggplot(media_df, aes(x = SAC, y = TCH))
  
  if (use_filled_contour && selected_gradient %in% names(gradient_options)) {
    gradient <- gradient_options[[selected_gradient]]
    p <- p +
      geom_raster(data = grid, aes(x = SAC, y = TCH, fill = TSH), alpha = 0.7) +
      scale_fill_gradient(low = gradient$low, high = gradient$high, name = "TSH") +
      geom_contour(data = grid, aes(x = SAC, y = TCH, z = TSH), bins = bins, colour = "white", linewidth = size_c) +
      new_scale_fill() +
      geom_vline(xintercept = mean(media_df$SAC, na.rm = TRUE), color = "white", linetype = 4, linewidth = 0.3) +
      geom_hline(yintercept = mean(media_df$TCH, na.rm = TRUE), color = "white", linetype = 4, linewidth = 0.3) +
      annotate("point", x = mean(media_df$SAC, na.rm = TRUE), y = mean(media_df$TCH, na.rm = TRUE), color = "white", shape = 18, size = 1.7)
  } else {
    p <- p +
      geom_contour(data = grid, aes(x = SAC, y = TCH, z = TSH), bins = bins, color = "black", 
                   linewidth = size_c, alpha = 1) +
      geom_vline(xintercept = mean(media_df$SAC, na.rm = TRUE), color = "red", linetype = 4, linewidth = 0.2) +
      geom_hline(yintercept = mean(media_df$TCH, na.rm = TRUE), color = "red", linetype = 4, linewidth = 0.2) +
      annotate("point", x = mean(media_df$SAC, na.rm = TRUE), y = mean(media_df$TCH, na.rm = TRUE), color = "red", shape = 18, size = 1.7)
  }
  
  p <- p +
    geom_point(aes(fill = check), shape = shape, size = size_p) +
    scale_fill_manual(values = colors_to_use, guide = "none") +
    theme_pubr() +
    scale_x_continuous(
      breaks = major_breaks_x, 
      labels = sprintf("%.1f", major_breaks_x), 
      limits = c(x_min, x_max), 
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = major_breaks_y, 
      labels = sprintf("%.0f", major_breaks_y), 
      limits = c(y_min, y_max), 
      expand = c(0, 0),
      sec.axis = sec_axis(
        trans = ~ . * max(grid$SAC, na.rm = TRUE) / 100, 
        breaks = tsh_breaks,
        labels = sprintf("%.1f", tsh_breaks),
        name = paste(y2_title, " (Media:", sprintf("%.1f", mean(media_df$TSH, na.rm = TRUE)), ")")
      )
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = axis_title_size),
      legend.position = "none",
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      axis.line = element_blank()
    ) +
    labs(
      x = paste(x_title, " (Media:", sprintf("%.1f", mean(media_df$SAC, na.rm = TRUE)), ")"),
      y = paste(y_title, " (Media:", sprintf("%.1f", mean(media_df$TCH, na.rm = TRUE)), ")"),
      title = plot_title
    )
  
  if (label_mode == "Todas las etiquetas") {
    p <- p + geom_text(aes(label = Variedad), vjust = -0.7, size = size_va, fontface = "bold")
  } else if (length(testigos) > 0) {
    p <- p + geom_text(data = subset(media_df, Variedad %in% testigos), aes(label = Variedad), vjust = -0.7, size = size_va, fontface = "bold")
  }
  
  return(p)
}
