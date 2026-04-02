# Correlazione Membrane_Origin, MacroScale e MicroScale ####

# Membrane_Origin - MacroScale ####

# prendi i dati
mem_origin_macro <- Membranes_All %>%
  select(ID_Article, ID_MembraneMaterial, Organ_membrane, Membrane_Origin, MacroScale) %>%
  distinct(ID_Article, ID_MembraneMaterial, Organ_membrane, .keep_all = TRUE)

# Denominatori: totale articoli per Membrane_Origin
denom_origin <- origin_tbl %>%
  select(Membrane_Origin, count) %>%
  distinct(Membrane_Origin, .keep_all = TRUE) %>%
  rename(N_Articles_Origin = count)

#conteggi
origin_macro <- mem_origin_macro %>%
  filter(!is.na(Membrane_Origin), Membrane_Origin != "",
         !is.na(MacroScale), MacroScale != "") %>%
  count(Membrane_Origin, MacroScale, name = "N_Membranes") %>%
  left_join(denom_origin, by = "Membrane_Origin") %>%
  mutate(Percent_of_articles_in_origin = round((N_Membranes / N_Articles_Origin) * 100, 2)) %>%
  arrange(Membrane_Origin, desc(N_Membranes))

#Wide COUNTS: MacroScale righe, Membrane_Origin colonne, missing=0
origin_macro_wide_counts <- origin_macro %>%
  select(MacroScale, Membrane_Origin, N_Membranes) %>%
  pivot_wider(
    names_from = Membrane_Origin,
    values_from = N_Membranes,
    values_fill = 0
  ) %>%
  arrange(MacroScale)

# Wide PERCENT: MacroScale righe, Membrane_Origin colonne, missing=0
origin_macro_wide_percent <- origin_macro %>%
  select(MacroScale, Membrane_Origin, Percent_of_articles_in_origin) %>%
  pivot_wider(
    names_from = Membrane_Origin,
    values_from = Percent_of_articles_in_origin,
    values_fill = 0
  ) %>%
  arrange(MacroScale)

## Export Excel MembraneOrigin_Macroscale ####
write_xlsx(
  list(
    Wide_Counts = origin_macro_wide_counts,
    Wide_Percent_of_Articles = origin_macro_wide_percent
  ),
  "07_MembraneOrigin_MacroScale.xlsx"
)

# Macroscale - Microscale ####

# prendi i dati
mem_micro_macro <- Membranes_All %>%
  select(ID_Article, ID_MembraneMaterial, Organ_membrane, MacroScale, MicroScale) %>%
  distinct(ID_Article, ID_MembraneMaterial, Organ_membrane, .keep_all = TRUE) %>%
  filter(!is.na(MacroScale), MacroScale != "",
         !is.na(MicroScale), MicroScale != "") %>%
  mutate(MacroScale = str_trim(MacroScale), MicroScale = str_trim(MicroScale)
  )

#conteggi
micro_macro_counts_long <- mem_micro_macro %>%
  count(MicroScale, MacroScale, name = "N") %>%
  arrange(desc(N))

ordine_micro_macro <- c("Porous", "Fibrous", "Vascularized", "Microporous wall", "Hollow tubule", "Side-to-side", "Crypt‑villus pattern", "Micropillar lattice", "Mix")

#tabella wide count
micro_macro_wide_counts <- micro_macro_counts_long %>%
  pivot_wider(
    names_from = MacroScale,
    values_from = N,
    values_fill = 0
  ) %>%
  mutate(MicroScale = factor(MicroScale, levels = ordine_micro_macro)) %>%
  arrange(MicroScale)

## Export Excel Macroscale-Miscoscale ####
write_xlsx(
  list(
    Wide_Counts = micro_macro_wide_counts
    #Percent_within_MicroScale = micro_macro_wide_percent_row,
    #Percent_within_MacroScale = micro_macro_wide_percent_col
  ),
  "07_MicroScale_MacroScale.xlsx"
)

# Membrane_Origin - MacroScale - MicroScale: edges-nodes graph ####

# 1) Pulizia dati
network_data <- Membranes_All %>%
  select(Membrane_Origin, MacroScale, MicroScale) %>%
  mutate(across(everything(), ~ str_trim(as.character(.)))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  filter(!is.na(Membrane_Origin), !is.na(MacroScale), !is.na(MicroScale))

# 2) Ordine desiderato dei nodi
origin_order <- c("Synthetic", "Natural", "Mix", "Membrane-less", "Unknown")

macro_order <- c("Film", "Hydrogel", "Chip", "Hollow tubule")

micro_order <- c(
  "Porous", "Fibrous", "Vascularized", "Microporous wall",
  "Hollow tubule", "Side-to-side", "Crypt‑villus pattern",
  "Micropillar lattice", "Mix"
)

# 3) Mantieni solo i livelli presenti nei dati, rispettando l'ordine scelto
nodes_left <- tibble(
  name = origin_order[origin_order %in% unique(network_data$Membrane_Origin)],
  layer = "Membrane origin",
  x = 1
)

nodes_mid <- tibble(
  name = macro_order[macro_order %in% unique(network_data$MacroScale)],
  layer = "MacroScale",
  x = 2
)

nodes_right <- tibble(
  name = micro_order[micro_order %in% unique(network_data$MicroScale)],
  layer = "MicroScale",
  x = 3
)

# 4) Assegna coordinate y manuali in base all'ordine
nodes_left <- nodes_left %>%
  mutate(y = c(5, 4, 3, 2, 1)[seq_len(n())])

nodes_mid <- nodes_mid %>%
  mutate(y = c(4, 3, 2, 1)[seq_len(n())])

nodes_right <- nodes_right %>%
  mutate(y = c(9, 8, 7, 6, 5, 4, 3, 2, 1)[seq_len(n())])

nodes <- bind_rows(nodes_left, nodes_mid, nodes_right)

# 5) Edge list con frequenze
edges_left <- network_data %>%
  count(Membrane_Origin, MacroScale, name = "weight") %>%
  rename(from = Membrane_Origin, to = MacroScale) %>%
  mutate(layer_from = "Membrane origin", layer_to = "MacroScale")

edges_right <- network_data %>%
  count(MacroScale, MicroScale, name = "weight") %>%
  rename(from = MacroScale, to = MicroScale) %>%
  mutate(layer_from = "MacroScale", layer_to = "MicroScale")

edges <- bind_rows(edges_left, edges_right)

# 6) Tabelle coordinate per join sicuri
nodes_from <- nodes %>%
  transmute(
    from = name,
    layer_from = layer,
    x_from = x,
    y_from = y
  )

nodes_to <- nodes %>%
  transmute(
    to = name,
    layer_to = layer,
    x_to = x,
    y_to = y
  )

edges_plot <- edges %>%
  left_join(nodes_from, by = c("from", "layer_from")) %>%
  left_join(nodes_to, by = c("to", "layer_to")) %>%
  filter(!is.na(x_from), !is.na(y_from), !is.na(x_to), !is.na(y_to))

#trasformazione logaritmica
edges_plot <- edges_plot %>%
  mutate(weight_plot = log10(weight + 1))

# 7) Colori
node_colors <- c(
  "Membrane origin" = "gray40",
  "MacroScale" = "gray40",
  "MicroScale" = "gray40"
)

edge_colors <- c(
  "Membrane origin" = "gray80",
  "MacroScale" = "gray80"
)

## Grafico edges-nodes ####
ggplot() +
  geom_curve(
    data = edges_plot %>% filter(layer_from == "Membrane origin"),
    aes(
      x = x_from, y = y_from,
      xend = x_to, yend = y_to,
      linewidth = weight_plot
    ),
    curvature = 0,
    color = edge_colors["Membrane origin"],
    alpha = 0.8
  ) +
  geom_curve(
    data = edges_plot %>% filter(layer_from == "MacroScale"),
    aes(
      x = x_from, y = y_from,
      xend = x_to, yend = y_to,
      linewidth = weight_plot
    ),
    curvature = 0,
    color = edge_colors["MacroScale"],
    alpha = 0.8
  ) +
  geom_point(
    data = nodes,
    aes(x = x, y = y, fill = layer),
    shape = 21,
    size = 9,
    color = "white",
    stroke = 1.2
  ) +
  geom_text(
    data = nodes %>% filter(layer == "Membrane origin"),
    aes(x = x - 0.03, y = y, label = name),
    hjust = 1,
    vjust = 0.5,
    color = node_colors["Membrane origin"],
    size = 5
  ) +
  geom_text(
    data = nodes %>% filter(layer == "MacroScale"),
    aes(x = x, y = y + 0.22, label = name),
    hjust = 0.5,
    vjust = 0,
    color = node_colors["MacroScale"],
    size = 5
  ) +
  geom_text(
    data = nodes %>% filter(layer == "MicroScale"),
    aes(x = x + 0.03, y = y, label = name),
    hjust = 0,
    vjust = 0.5,
    color = node_colors["MicroScale"],
    size = 5
  ) +
  scale_fill_manual(values = node_colors) +
  scale_linewidth(range = c(0.5, 3.2)) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Membrane origin", "MacroScale", "MicroScale"),
    limits = c(0.65, 3.35)
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Network of Membrane Origin -> MacroScale -> MicroScale",
    x = NULL,
    y = NULL,
    linewidth = "Count"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

## Salva il grafico ####
ggsave("07_network_membrane_macro_micro.png", width = 16, height = 9, dpi = 300, bg = "white")
ggsave("07_network_membrane_macro_micro.svg", width = 16, height = 9, bg = "white")
