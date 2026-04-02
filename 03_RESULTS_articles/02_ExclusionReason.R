# 2_ExclusionReason_Overall ####

# Estrai tutte le colonne delle ragioni di esclusione ####
reasons_all <- bind_rows(
  tibble(Reason = Article_Excluded_Gut$ExcReas_Gut),
  tibble(Reason = Article_Excluded_Kidney$ExcReas_Kidney),
  tibble(Reason = Article_Excluded_Lung$ExcReas_Lung),
  tibble(Reason = Article_Excluded_Brain$ExcReas_Brain),
  tibble(Reason = Article_Excluded_Placenta$ExcReas_Placenta)
)

# Imposta l'ordine desiderato  ####
reason_levels <- c(
  "No full text available",
  "Wrong publication type",
  "Wrong model",
  "No clear model description"
)

# Conteggi e percentuali ####
reasons_df <- reasons_all %>%
  count(Reason) %>%
  mutate(
    Reason = factor(Reason, levels = reason_levels),
    Percent = n / sum(n)
  ) %>%
  arrange(Reason) %>%
  mutate(LabelPie = paste0(scales::percent(Percent, accuracy = 0.1), "\n(", n, ")" ))

# Definisci i colori: ####
reason_colors <- c(
  "No full text available"      = "#F5A48B",
  "Wrong publication type"      = "#ACD193",
  "Wrong model"                 = "#98BEE4",
  "No clear model description"  = "#FFE393"
)

# GRAFICO 2_ExclusionReason_Overall PIE CHART ####

# Etichette legenda con percentuali
legend_labels <- reasons_df %>%
  mutate(LegLabel = paste0(Reason," (", n, ", ",scales::percent(Percent, accuracy = 0.1),")")) %>%
  #mutate(LegLabel = paste0(Reason, " (", scales::percent(Percent, accuracy = 0.1), ")")) %>%
  arrange(Reason) %>%
  pull(LegLabel)

names(legend_labels) <- reasons_df$Reason

# Grafico
ggplot(reasons_df, aes(x = "", y = Percent, fill = Reason)) +
  
  geom_col(color = "white") +
  coord_polar(theta = "y", start = pi/2) +
  
  scale_fill_manual(
    values = reason_colors,
    #labels = legend_labels
  ) +
  
  labs(
    title = "Distribution of reasons for studies exclusion",
    fill = "Legend"
  ) +
  
  geom_text(
    aes(x = 1.2, label = LabelPie),
    position = position_stack(vjust = 0.5),  # centro di ogni fetta
    size = 3,
    color = "black",
    lineheight = 0.9
  ) +
  
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12)
  ) +
  
  guides(
    fill = guide_legend(
      label.hjust = 0,
      override.aes = list(color = NA)
    )
  )

## Salva il grafico ####
ggsave("02_ExclusionReason_Overall.png", width = 8, height = 4, dpi = 300, bg = "white")
ggsave("02_ExclusionReason_Overall.svg", width = 8, height = 4, bg = "white")

# Estrai dati (conteggi) su Excel per organo ####
reasons_by_organ <- bind_rows(
  tibble(Organ = "Gut",      Reason = Article_Excluded_Gut$ExcReas_Gut),
  tibble(Organ = "Kidney",   Reason = Article_Excluded_Kidney$ExcReas_Kidney),
  tibble(Organ = "Lung",     Reason = Article_Excluded_Lung$ExcReas_Lung),
  tibble(Organ = "Brain",    Reason = Article_Excluded_Brain$ExcReas_Brain),
  tibble(Organ = "Placenta", Reason = Article_Excluded_Placenta$ExcReas_Placenta)
) %>%
  mutate(
    Organ = factor(Organ, levels = organi),
    Reason = factor(Reason, levels = reason_levels)
)

reason_wide_counts <- reasons_by_organ %>%
  count(Organ, Reason, name = "n") %>%
  complete(Organ, Reason, fill = list(n = 0)) %>%
  pivot_wider(
    names_from = Organ,
    values_from = n
  ) %>%
  arrange(Reason)

# aggiungi Overall
reason_wide_counts <- reason_wide_counts %>%
  mutate(Overall = rowSums(across(all_of(organi))))

reason_wide_perc <- reasons_by_organ %>%
  count(Organ, Reason, name = "n") %>%
  complete(Organ, Reason, fill = list(n = 0)) %>%
  group_by(Organ) %>%
  mutate(Percent = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from = Organ,
    values_from = Percent
  ) %>%
  arrange(Reason)

# aggiungi Overall %
overall_perc <- reasons_by_organ %>%
  count(Reason) %>%
  mutate(Overall = n / sum(n)) %>%
  select(-n)

reason_wide_perc <- left_join(reason_wide_perc, overall_perc, by = "Reason") %>%
  mutate(across(-Reason, ~ scales::percent(.x, accuracy = 0.1)))

## Export Excel ####
write_xlsx(
  list(
    Wide_Counts = reason_wide_counts,
    Wide_Percentages = reason_wide_perc
  ),
  "02_Exclusion_reason_By_Organ.xlsx"
)
