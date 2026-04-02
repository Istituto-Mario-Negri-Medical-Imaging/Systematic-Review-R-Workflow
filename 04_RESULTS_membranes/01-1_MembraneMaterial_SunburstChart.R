# Membrane Origin e Material ####

setwd("../04_RESULTS_membranes")

# Funzioni per creare le tabelle origin e material ####
make_origin_tbl <- function(mem_df, denom_n) {
  mem_df %>%
    filter(!is.na(Membrane_Origin), Membrane_Origin != "") %>%
    count(Membrane_Origin, name = "count") %>%
    mutate(
      percent = (count / denom_n) * 100
    ) %>%
    arrange(desc(count), Membrane_Origin)
}

make_material_tbl <- function(mem_df, denom_n) {
  mem_df %>%
    filter(
      !is.na(Membrane_Origin), Membrane_Origin != "",
      !is.na(Membrane_Material), Membrane_Material != ""
    ) %>%
    group_by(Membrane_Origin, Membrane_Material) %>%
    summarise(
      count = n(),
      ID_Articles = paste(unique(ID_Article), collapse = "; "),
      Membrane_Description = paste(unique(na.omit(Membrane_Description)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(
      percent = (count / denom_n) * 100
    ) %>%
    arrange(desc(count), Membrane_Origin, Membrane_Material, Membrane_Description)
}

for (nm in names(membrane_list)) {
  mem_df <- membrane_list[[nm]]
  denom_n <- nrow(mem_df)  # numero di membrane
  
  # Macro: origin
  origin_tbl <- make_origin_tbl(mem_df, denom_n)
  assign(paste0("Mem_origin_", nm), origin_tbl, envir = .GlobalEnv)
  
  # Micro: material
  material_tbl <- make_material_tbl(mem_df, denom_n)
  assign(paste0("Mem_material_", nm), material_tbl, envir = .GlobalEnv)
}

# (Opzionale) lista nomi creati per verifica rapida
created_tables <- c(
  paste0("Mem_origin_", names(membrane_list)),
  paste0("Mem_material_", names(membrane_list))
)
created_tables

# Export Excel file ####
write_xlsx(
  list(
    Gut = Mem_material_Gut,
    Kidney = Mem_material_Kidney,
    Lung = Mem_material_Lung,
    Brain = Mem_material_Brain,
    Placenta = Mem_material_Placenta,
    All_Organs = Mem_material_All
  ),
  "01_Membrane_Material.xlsx"
)

# Grafico: Sunburst Plots for Each Organ and all (Origin -> Material) ####
origin_color_map <- c(
  "Synthetic"     = "#98BEE4",
  "Natural"       = "#ACD193",
  "Mix"           = "#F5A48B",
  "Membrane-less" = "#FFE393",
  "Unknown"       = "#BC8FDD"
)

create_sunburst_plot <- function(df_counts, title_text, origin_color_map) {
  
  if ("Count" %in% names(df_counts) && !"count" %in% names(df_counts)) {
    df_counts <- df_counts %>% rename(count = Count)
  }
  
  df_counts <- df_counts %>%
    mutate(
      Membrane_Origin   = trimws(as.character(Membrane_Origin)),
      Membrane_Material = trimws(as.character(Membrane_Material))
    ) %>%
    filter(
      !is.na(Membrane_Origin), Membrane_Origin != "",
      !is.na(Membrane_Material), Membrane_Material != ""
    ) %>%
    group_by(Membrane_Origin) %>%
    arrange(desc(count), Membrane_Material, .by_group = TRUE) %>%
    mutate(
      origin_id   = Membrane_Origin,
      material_id = paste(Membrane_Origin, Membrane_Material, sep = "__")
    ) %>%
    ungroup()
  
  origins <- unique(df_counts$origin_id)
  
  origin_values <- sapply(origins, function(x) sum(df_counts$count[df_counts$origin_id == x]))
  
  # Opzione B: numeri solo tra parentesi
  origin_labels <- ifelse(
    origins %in% c("Unknown", "Membrane-less"),
    "",
    paste0(origins, " (", origin_values, ")")
  )
  material_labels <- paste0(df_counts$Membrane_Material, " (", df_counts$count, ")")
  
  ids     <- c(origins, df_counts$material_id)
  labels  <- c(origin_labels, material_labels)
  parents <- c(rep("", length(origins)), df_counts$origin_id)
  values  <- c(origin_values, df_counts$count)
  
  # Colori origin (interno)
  origin_colors <- unname(origin_color_map[origins])
  origin_colors[is.na(origin_colors)] <- "#BDBDBD"
  
  # Sfumature materiali (esterno) basate sul colore origin
  df_counts <- df_counts %>%
    group_by(origin_id) %>%
    mutate(
      base_col = origin_color_map[origin_id],
      base_col = ifelse(is.na(base_col), "#BDBDBD", base_col),
      k = n(),
      idx = row_number(),
      idx_norm = ifelse(k == 1, 0, (idx - 1) / (k - 1)),
      shade_amount = 0.15 + idx_norm * (0.90 - 0.25),
      material_color = colorspace::lighten(base_col, amount = shade_amount)
    ) %>%
    ungroup()
  
  material_colors <- df_counts$material_color
  colors <- c(origin_colors, material_colors)
  
  plot_ly(
    ids = ids,
    labels = labels,
    parents = parents,
    values = values,
    type = "sunburst",
    rotation = 60,
    branchvalues = "total",
    marker = list(colors = colors),
    maxdepth = 2,
    textfont = list(color = "black"),
    textinfo = "label",
    insidetextorientation = "radial"
  ) %>%
    layout(title = list(text = title_text))
}

# Crea e salva sunburst in html ####
Mem_sunburst_Gut      <- create_sunburst_plot(Mem_material_Gut,      "Gut",        origin_color_map)
Mem_sunburst_Kidney   <- create_sunburst_plot(Mem_material_Kidney,   "Kidney",     origin_color_map)
Mem_sunburst_Lung     <- create_sunburst_plot(Mem_material_Lung,     "Lung",       origin_color_map)
Mem_sunburst_Brain    <- create_sunburst_plot(Mem_material_Brain,    "Brain",      origin_color_map)
Mem_sunburst_Placenta <- create_sunburst_plot(Mem_material_Placenta, "Placenta",   origin_color_map)
Mem_sunburst_All      <- create_sunburst_plot(Mem_material_All,      "All Organs", origin_color_map)

htmlwidgets::saveWidget(Mem_sunburst_Gut,      "01_Mem_Sunburst_Gut.html", selfcontained = TRUE)
htmlwidgets::saveWidget(Mem_sunburst_Kidney,   "01_Mem_Sunburst_Kidney.html", selfcontained = TRUE)
htmlwidgets::saveWidget(Mem_sunburst_Lung,     "01_Mem_Sunburst_Lung.html", selfcontained = TRUE)
htmlwidgets::saveWidget(Mem_sunburst_Brain,    "01_Mem_Sunburst_Brain.html", selfcontained = TRUE)
htmlwidgets::saveWidget(Mem_sunburst_Placenta, "01_Mem_Sunburst_Placenta.html", selfcontained = TRUE)
htmlwidgets::saveWidget(Mem_sunburst_All,      "01_Mem_Sunburst_All.html", selfcontained = TRUE)