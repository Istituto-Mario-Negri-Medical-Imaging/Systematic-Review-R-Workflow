# 02: Coating Material Distribution Across Organs ####

# Clean and separate multiple coatings
coating_long <- Membranes_All %>%
  filter(!is.na(Coating_Material), Coating_Material != "") %>%
  separate_rows(Coating_Material, sep = ";") %>%
  mutate(Coating_Material = str_trim(Coating_Material))

# Count distribution per organ
coating_distribution <- coating_long %>%
  group_by(Organ_membrane, Coating_Material) %>%
  summarise(Unique_Articles = n_distinct(ID_Article), .groups = "drop") %>%
  arrange(Organ_membrane, desc(Unique_Articles))

# Total unique articles per organ
articles_per_organ <- Membranes_All %>%
  group_by(Organ_membrane) %>%
  summarise(Total_Unique_Articles = n_distinct(ID_Article), .groups = "drop")

# Merge totals and calculate % per organ
coating_distribution <- coating_distribution %>%
  left_join(articles_per_organ, by = "Organ_membrane") %>%
  mutate(Percentage_within_organ = round((Unique_Articles / Total_Unique_Articles) * 100, 2))

# Add Overall column (considering ID_Article + Organ_membrane)
# Total number of unique (ID_Article, Organ_membrane) pairs across all data
total_unique_pairs_overall <- Membranes_All %>%
  distinct(ID_Article, Organ_membrane) %>%
  nrow()

# Count unique (ID_Article, Organ_membrane) per Coating_Material
overall_distribution <- coating_long %>%
  distinct(ID_Article, Organ_membrane, Coating_Material) %>%
  group_by(Coating_Material) %>%
  summarise(Overall_Unique_Articles = n(), .groups = "drop") %>%
  mutate(Percentage_Overall = round((Overall_Unique_Articles / total_unique_pairs_overall) * 100, 2))

# Merge overall into coating_distribution
coating_distribution <- coating_distribution %>%
  left_join(overall_distribution, by = "Coating_Material")


# Create wide-format count table with Overall
coating_wide_counts <- coating_distribution %>%
  select(Organ_membrane, Coating_Material, Unique_Articles, Overall_Unique_Articles) %>%
  pivot_wider(names_from = Organ_membrane, values_from = Unique_Articles, values_fill = 0) %>%
  mutate(Overall = Overall_Unique_Articles) %>%
  select(-Overall_Unique_Articles) %>%
  arrange(desc(Overall)) %>%
  select(Coating_Material, all_of(order))

# Create wide-format percentage table with Overall
coating_wide_percent <- coating_distribution %>%
  select(Organ_membrane, Coating_Material, Percentage_within_organ, Percentage_Overall) %>%
  pivot_wider(names_from = Organ_membrane, values_from = Percentage_within_organ, values_fill = 0) %>%
  mutate(Overall = Percentage_Overall) %>%
  select(-Percentage_Overall) %>%
  arrange(desc(Overall)) %>%
  select(Coating_Material, all_of(order))

## Export to Excel ####
write_xlsx(
  list(
    Wide_Counts = coating_wide_counts,
    Wide_Percentages = coating_wide_percent
  ),
  "02_Coating_By_Organ.xlsx"
)

# Sistema tabelle per grafico ####
# CLEAN & RESHAPE PERCENT TABLE
coating_long_percent <- coating_wide_percent %>%
  mutate(across(all_of(organi),
                ~ as.numeric(str_replace(.x, ",", ".")))) %>%   # convert commas to decimal point
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_membrane",
    values_to = "Percent"
  )

# CLEAN & RESHAPE COUNTS TABLE
coating_long_counts <- coating_wide_counts %>%
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_membrane",
    values_to = "Count"
  )

# MERGE % AND COUNTS
plot_coating <- coating_long_percent %>%
  left_join(coating_long_counts,
            by = c("Coating_Material", "Organ_membrane")) %>%
  mutate(
    Organ_membrane = factor(Organ_membrane, levels = organi),
    Coating_Material = factor(Coating_Material)
  )

plot_coating <- plot_coating %>%
  mutate(Coating_Material = recode(Coating_Material, "3D Collagen scaffold" = "3D Collagen\nscaffold"),
         Coating_Material = recode(Coating_Material, "3D CNF scaffold" = "3D CNF\nscaffold"))

ordine_coating <- c("Collagen I", "ECM", "Fibronectin", "Collagen IV", "Laminin", "Collagen", "Collagen III", "Dopamine", "3D Collagen\nscaffold", "3D CNF\nscaffold", "Gelatin")

# Grafico: GROUPED BAR PLOT (% values, counts as labels) ####
ggplot(plot_coating,
       aes(x = factor(Coating_Material, levels = ordine_coating),
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
  
  # Y axis: use percent formatting like the second plot
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, max(plot_coating$Percent) * 1.15, by = 10),  # show every 10
    labels = function(x) paste0(x, "%")                        # add % symbol
  )+
  
  labs(
    title = "Coating material usage across organs",
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
                              face = "bold"
    )
  )

## Salva il grafico ####
ggsave("02_Coating_organs.png", width = 12, height = 6, dpi = 300, bg = "white")
ggsave("02_Coating_organs.svg", width = 12, height = 6, bg = "white")


# Tabella coating material (con split) ####
## Funzioni per creare la tabella Coating ####
make_coating_material_tbl <- function(mem_df, denom_n) {
  mem_df %>%
    filter(!is.na(Coating_Material), Coating_Material != "") %>%
    separate_rows(Coating_Material, sep = ";") %>%
    mutate(Coating_Material = str_trim(Coating_Material)) %>%
    filter(Coating_Material != "") %>%
    group_by(Coating_Material) %>%
    summarise(
      count = n(),
      ID_Articles = paste(unique(ID_Article), collapse = "; "),
      .groups = "drop"
    ) %>%
    #mutate(percent = (count / denom_n) * 100) %>%
    arrange(desc(count), Coating_Material)
}

for (nm in names(membrane_list)) {
  mem_df <- membrane_list[[nm]]
  #denom_n <- nrow(mem_df)  # come nel tuo codice (numero di membrane)
  
  coating_tbl <- make_coating_material_tbl(mem_df) #, denom_n)
  assign(paste0("Coat_material_", nm), coating_tbl, envir = .GlobalEnv)
}

# Export as Excel file ####
write_xlsx(
  list(
    Gut = Coat_material_Gut,
    Kidney = Coat_material_Kidney,
    Lung = Coat_material_Lung,
    Brain = Coat_material_Brain,
    Placenta = Coat_material_Placenta,
    All_Organs = Coat_material_All
  ),
  "02_Coating.xlsx"
)
