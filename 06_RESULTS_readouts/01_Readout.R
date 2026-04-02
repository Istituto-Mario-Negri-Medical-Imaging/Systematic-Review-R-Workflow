# Readout distribution across organs ####

setwd("../06_RESULTS_readouts")

# 1. Clean + deduplica chiave primaria
readout_clean <- Readout_All %>%
  mutate(
    Organ_readout = str_squish(as.character(Organ_readout)),
    Readout = str_squish(as.character(Readout))
  ) %>%
  filter(!is.na(Readout), Readout != "",
         !is.na(Organ_readout), Organ_readout != "") %>%
  distinct(ID_Article, Organ_readout, Readout)

# 2. Count distribution per organ
readout_distribution <- readout_clean %>%
  group_by(Organ_readout, Readout) %>%
  summarise(
    Unique_Articles = n_distinct(ID_Article),
    .groups = "drop"
  ) %>%
  arrange(Organ_readout, desc(Unique_Articles))

# 3. Total unique articles per organ
articles_per_organ <- readout_clean %>%
  group_by(Organ_readout) %>%
  summarise(
    Total_Unique_Articles = n_distinct(ID_Article),
    .groups = "drop"
  )

# 4. Percentuale within organ
readout_distribution <- readout_distribution %>%
  left_join(articles_per_organ, by = "Organ_readout") %>%
  mutate(
    Percentage_within_organ =
      round((Unique_Articles / Total_Unique_Articles) * 100, 2)
  )

# 5. Overall distribution
total_unique_pairs_overall <- readout_clean %>%
  distinct(ID_Article, Organ_readout) %>%
  nrow()

overall_distribution <- readout_clean %>%
  distinct(ID_Article, Organ_readout, Readout) %>%
  group_by(Readout) %>%
  summarise(
    Overall_Unique_Articles = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage_Overall =
      round((Overall_Unique_Articles / total_unique_pairs_overall) * 100, 2)
  )

readout_distribution <- readout_distribution %>%
  left_join(overall_distribution, by = "Readout")

# 6. Wide table counts
readout_wide_counts <- readout_distribution %>%
  select(Organ_readout, Readout, Unique_Articles, Overall_Unique_Articles) %>%
  pivot_wider(
    names_from = Organ_readout,
    values_from = Unique_Articles,
    values_fill = 0
  ) %>%
  mutate(Overall = Overall_Unique_Articles) %>%
  select(-Overall_Unique_Articles) %>%
  arrange(desc(Overall)) %>%
  select(Readout, all_of(order))

# 7. Wide table percentages
readout_wide_percent <- readout_distribution %>%
  select(Organ_readout, Readout, Percentage_within_organ, Percentage_Overall) %>%
  pivot_wider(
    names_from = Organ_readout,
    values_from = Percentage_within_organ,
    values_fill = 0
  ) %>%
  mutate(Overall = Percentage_Overall) %>%
  select(-Percentage_Overall) %>%
  arrange(desc(Overall)) %>%
  select(Readout, all_of(order))

## 8. Export Excel ####
write_xlsx(
  list(
    Wide_Counts = readout_wide_counts,
    Wide_Percentages = readout_wide_percent
  ),
  "01_Readout_By_Organ.xlsx"
)


#Sistema tabelle per grafico ####
readout_long_percent <- readout_wide_percent %>%
  mutate(across(all_of(organi),
                ~ as.numeric(str_replace(as.character(.x), ",", ".")))) %>%
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_readout",
    values_to = "Percent"
  )

# LONG counts (da wide counts)
readout_long_counts <- readout_wide_counts %>%
  pivot_longer(
    cols = all_of(organi),
    names_to = "Organ_readout",
    values_to = "Count"
  )

# MERGE
plot_readout <- readout_long_percent %>%
  left_join(readout_long_counts, by = c("Readout", "Organ_readout")) %>%
  mutate(
    Organ_readout = factor(Organ_readout, levels = organi),
    Readout = factor(Readout)
  )

#Ordine
ordine_readout <- readout_wide_counts %>%
  arrange(desc(Overall)) %>%
  pull(Readout)

plot_readout <- plot_readout %>%
  mutate(Readout = factor(Readout, levels = ordine_readout))

## Grafico ####
ggplot(plot_readout,
       aes(x = Readout, y = Percent, fill = Organ_readout)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5) +
  
  scale_fill_manual(values = colori_organi) +
  
  scale_y_continuous(
    limits = c(0, max(plot_readout$Percent, na.rm = TRUE) * 1.15),
    breaks = seq(0, max(plot_readout$Percent, na.rm = TRUE) * 1.15, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Readout usage across organs",
    x = NULL,
    y = NULL,
    fill = "Organ"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title  = element_text(size = 12, hjust = 0.5, face = "bold")
  )

## Salva il grafico
ggsave("01_Readout_across_organs.png", width = 16, height = 14, dpi = 300, bg = "white")
ggsave("01_Readout_across_organs.svg", width = 16, height = 14, bg = "white")
