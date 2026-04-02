# 04: MacroScale Distribution Across Organs ####

# Clean and separate multiple MacroScales
macro_long <- Membranes_All %>%
  filter(!is.na(MacroScale), MacroScale != "") %>%
  separate_rows(MacroScale, sep = ";") %>%
  mutate(MacroScale = str_trim(MacroScale))

# Count distribution per organ
macro_distribution <- macro_long %>%
  group_by(Organ_membrane, MacroScale) %>%
  summarise(unique_Articles = n_distinct(ID_Article), .groups = "drop") %>%
  arrange(Organ_membrane, desc(unique_Articles))

# Merge totals and calculate % per organ
macro_distribution <- macro_distribution %>%
  left_join(articles_per_organ, by = "Organ_membrane") %>%
  mutate(Percentage_within_organ = round((unique_Articles / Total_Unique_Articles) * 100, 2))

# Add Overall column (ID_Article + Organ_membrane pairs)
# Total number of unique (ID_Article, Organ_membrane) pairs
total_unique_pairs_overall <- Membranes_All %>%
  distinct(ID_Article, Organ_membrane) %>%
  nrow()

# Count unique pairs per MacroScale
overall_distribution <- macro_long %>%
  distinct(ID_Article, Organ_membrane, MacroScale) %>%
  group_by(MacroScale) %>%
  summarise(Overall_Unique_Articles = n(), .groups = "drop") %>%
  mutate(Percentage_Overall = round((Overall_Unique_Articles / total_unique_pairs_overall) * 100, 2))

# Merge into main table
macro_distribution <- macro_distribution %>%
  left_join(overall_distribution, by = "MacroScale")

# Create wide-format count table with Overall
macro_wide_counts <- macro_distribution %>%
  select(Organ_membrane, MacroScale, unique_Articles, Overall_Unique_Articles) %>%
  pivot_wider(names_from = Organ_membrane, values_from = unique_Articles, values_fill = 0) %>%
  mutate(Overall = Overall_Unique_Articles) %>%
  select(-Overall_Unique_Articles) %>%
  arrange(desc(Overall)) %>%
  select(MacroScale, all_of(order))

# Create wide-format percentage table with Overall
macro_wide_percent <- macro_distribution %>%
  select(Organ_membrane, MacroScale, Percentage_within_organ, Percentage_Overall) %>%
  pivot_wider(names_from = Organ_membrane, values_from = Percentage_within_organ, values_fill = 0) %>%
  mutate(Overall = Percentage_Overall) %>%
  select(-Percentage_Overall) %>%
  arrange(desc(Overall)) %>%
  select(MacroScale, all_of(order))

# Export to Excel ####
write_xlsx(
  list(
    Wide_Counts = macro_wide_counts,
    Wide_Percentages = macro_wide_percent
  ),
  "04_MacroScale_By_Organ.xlsx"
)

#Sistema tabelle per grafico ####
# CLEAN & RESHAPE PERCENT TABLE (MacroScale)
macro_long_percent <- macro_wide_percent %>%
  mutate(across(all_of(organi),
                ~ suppressWarnings(as.numeric(str_replace(.x, ",", "."))))) %>%  # robust to comma decimals
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_membrane",
    values_to = "Percent"
  )

# CLEAN & RESHAPE COUNTS TABLE (MacroScale)
macro_long_counts <- macro_wide_counts %>%
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_membrane",
    values_to = "Count"
  )

# MERGE % AND COUNTS
macro_data <- macro_long_percent %>%
  left_join(macro_long_counts,
            by = c("MacroScale", "Organ_membrane")) %>%
  mutate(
    Organ_membrane = factor(Organ_membrane, levels = organi),
    MacroScale = factor(MacroScale)
  )

# (Optional) derive a dynamic y-limit if you prefer auto-scaling
ymax <- max(macro_data$Percent, na.rm = TRUE)
ybreak_max <- ceiling(ymax * 1.15 / 10) * 10

ordine_macro <- c("Film", "Hydrogel", "Chip", "Hollow tubule")

# Grafico: GROUPED BAR PLOT (% values, counts as labels) ####
ggplot(macro_data,
       aes(x = factor(MacroScale, levels = ordine_macro),
           y = Percent,
           fill = Organ_membrane)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  
  # Label absolute counts above bars
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5) +
  
  scale_fill_manual(values = colori_organi) +
  
  # Y axis: percent formatting
  scale_y_continuous(
    # If you want fixed limits like before, keep the next line and comment the dynamic one
    limits = c(0, 90),
    # Alternatively, use dynamic scaling:
    # limits = c(0, max(10, ybreak_max)),
    breaks = seq(0, max(macro_data$Percent, na.rm = TRUE) * 1.15, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "MacroScale usage across organs",
    x = NULL,
    y = NULL,
    fill = "Organ"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0,
                               hjust = 0.5,
                               size = 12,
                               color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 12,
                              hjust = 0.5,
                              face = "bold")
  )

#Salva il grafico ####
ggsave("04_MacroScale_organs.png", width = 8, height = 6, dpi = 300, bg = "white")
ggsave("04_MacroScale_organs.svg", width = 8, height = 6, bg = "white")


# Tabella MacroScale ####
## Funzioni per creare la tabella MacroScale ####
make_macroscale_tbl <- function(mem_df, denom_n) {
  mem_df %>%
    filter(!is.na(MacroScale), MacroScale != "") %>%
    group_by(MacroScale) %>%
    summarise(
      count = n(),
      ID_Articles = paste(unique(ID_Article), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(percent = (count / denom_n) * 100) %>%
    arrange(desc(count), MacroScale)
}

for (nm in names(membrane_list)) {
  mem_df <- membrane_list[[nm]]
  denom_n <- nrow(mem_df)  # numero di membrane
  
  macroscale_tbl <- make_macroscale_tbl(mem_df, denom_n)
  assign(paste0("Mem_macroscale_", nm), macroscale_tbl, envir = .GlobalEnv)
}

# (Opzionale) lista nomi creati per verifica rapida
created_tables <- paste0("Mem_macroscale_", names(membrane_list))
created_tables

## Export as Excel file ####
write_xlsx(
  list(
    Gut = Mem_macroscale_Gut,
    Kidney = Mem_macroscale_Kidney,
    Lung = Mem_macroscale_Lung,
    Brain = Mem_macroscale_Brain,
    Placenta = Mem_macroscale_Placenta,
    All_Organs = Mem_macroscale_All
  ),
  "04_MacroScale.xlsx"
)

