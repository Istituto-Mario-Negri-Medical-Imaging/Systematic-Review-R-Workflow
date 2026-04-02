# Membrane Origin e Material ####

# Grafico stack bar

## 1. Convertire le tabelle già create in un formato adatto al grafico ####
prepare_origin_plot_tbl <- function(origin_tbl, organ_name) {
  origin_tbl %>%
    mutate(
      Organ = organ_name,
      Level = "Origin",
      Category = Membrane_Origin,
      Parent_Origin = Membrane_Origin
    ) %>%
    select(Organ, Level, Category, Parent_Origin, count, percent)
}

prepare_material_plot_tbl <- function(material_tbl, organ_name) {
  material_tbl %>%
    mutate(
      Organ = organ_name,
      Level = "Material",
      Category = Membrane_Material,
      Parent_Origin = Membrane_Origin
    ) %>%
    select(Organ, Level, Category, Parent_Origin, count, percent)
}

## 2. Unione delle tabelle già esistenti ####

origin_plot_tbl <- bind_rows(
  prepare_origin_plot_tbl(Mem_origin_Gut,      "Gut"),
  prepare_origin_plot_tbl(Mem_origin_Kidney,   "Kidney"),
  prepare_origin_plot_tbl(Mem_origin_Lung,     "Lung"),
  prepare_origin_plot_tbl(Mem_origin_Brain,    "Brain"),
  prepare_origin_plot_tbl(Mem_origin_Placenta, "Placenta"),
  prepare_origin_plot_tbl(Mem_origin_All,      "Overall")
)

material_plot_tbl <- bind_rows(
  prepare_material_plot_tbl(Mem_material_Gut,      "Gut"),
  prepare_material_plot_tbl(Mem_material_Kidney,   "Kidney"),
  prepare_material_plot_tbl(Mem_material_Lung,     "Lung"),
  prepare_material_plot_tbl(Mem_material_Brain,    "Brain"),
  prepare_material_plot_tbl(Mem_material_Placenta, "Placenta"),
  prepare_material_plot_tbl(Mem_material_All,      "Overall")
)

plot_tbl <- bind_rows(origin_plot_tbl, material_plot_tbl)

## 3. Ordine dei fattori ####

organ_order <- c("Gut", "Kidney", "Lung", "Brain", "Placenta", "Overall")
level_order <- c("Origin", "Material")

plot_tbl <- plot_tbl %>%
  mutate(
    Organ = factor(Organ, levels = organ_order),
    Level = factor(Level, levels = level_order)
  )

# Etichetta per avere due barre vicine per ciascun organo
plot_tbl <- plot_tbl %>%
  mutate(
    Bar_Label = factor(
      paste(Organ, Level, sep = "\n"),
      levels = c(
        "Gut\nOrigin", "Gut\nMaterial",
        "Kidney\nOrigin", "Kidney\nMaterial",
        "Lung\nOrigin", "Lung\nMaterial",
        "Brain\nOrigin", "Brain\nMaterial",
        "Placenta\nOrigin", "Placenta\nMaterial",
        "Overall\nOrigin", "Overall\nMaterial"
      )
    )
  )


## 4. Palette colori ####
#    - colori base per Origin
#    - sfumature per Material, coerenti con la Origin madre

origin_color_map <- c(
  "Synthetic"     = "#98BEE4",
  "Natural"       = "#ACD193",
  "Mix"           = "#F5A48B",
  "Membrane-less" = "#FFE393",
  "Unknown"       = "#BC8FDD"
)

# Colori per le categorie Origin
origin_colors <- origin_plot_tbl %>%
  distinct(Category, Parent_Origin) %>%
  mutate(
    fill_color = origin_color_map[Category],
    fill_color = ifelse(is.na(fill_color), "#BDBDBD", fill_color)
  ) %>%
  select(Category, fill_color)

# Colori per le categorie Material:
# ogni materiale eredita la base dal Parent_Origin e riceve una sfumatura diversa
material_colors <- material_plot_tbl %>%
  distinct(Category, Parent_Origin) %>%
  arrange(Parent_Origin, Category) %>%
  group_by(Parent_Origin) %>%
  mutate(
    base_col = origin_color_map[Parent_Origin],
    base_col = ifelse(is.na(base_col), "#BDBDBD", base_col),
    n_mat = n(),
    idx = row_number(),
    shade_amount = case_when(
      n_mat == 1 ~ 0.35,
      TRUE ~ seq(0.15, 0.65, length.out = n())
    ),
    fill_color = lighten(base_col, amount = shade_amount)
  ) %>%
  ungroup() %>%
  select(Category, fill_color)

# Palette finale
full_color_map <- bind_rows(origin_colors, material_colors) %>%
  distinct(Category, .keep_all = TRUE)

color_vector <- setNames(full_color_map$fill_color, full_color_map$Category)

## 5. Ordinamento delle categorie nello stack ####
#    - Origin: ordine fisso
#    - Material: ordine per Origin poi per frequenza totale


origin_stack_order <- c("Synthetic", "Natural", "Mix", "Membrane-less", "Unknown")

material_stack_order <- material_plot_tbl %>%
  group_by(Parent_Origin, Category) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(
    factor(Parent_Origin, levels = origin_stack_order),
    desc(total_count),
    Category
  ) %>%
  pull(Category)

category_order <- c(origin_stack_order, material_stack_order)

plot_tbl <- plot_tbl %>%
  mutate(
    Category = factor(Category, levels = unique(category_order))
  )

## 6. Grafico finale ####
membrane_dual_stacked_bar <- ggplot(
  plot_tbl,
  aes(x = Bar_Label, y = count, fill = Category)
) +
  geom_col(width = 0.82, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = color_vector, drop = FALSE) +
  labs(
    title = "Membrane Origin and Membrane Material across Organs-on-Chip",
    x = NULL,
    y = "Count",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, lineheight = 0.9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Visualizzazione
membrane_dual_stacked_bar

membrane_dual_stacked_bar_100 <- ggplot(
  plot_tbl,
  aes(x = Bar_Label, y = percent / 100, fill = Category)
) +
  geom_col(width = 0.82, color = "white", linewidth = 0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = color_vector, drop = FALSE) +
  labs(
    title = "Membrane Origin and Membrane Material across Organs-on-Chip",
    x = NULL,
    y = "Percentage",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, lineheight = 0.9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Visualizzazione
membrane_dual_stacked_bar_100

ggsave("01_Membrane_material.png", width = 12, height = 8, dpi = 300, bg = "white")
ggsave("01_Membrane_material.svg", width = 12, height = 8, bg = "white")
