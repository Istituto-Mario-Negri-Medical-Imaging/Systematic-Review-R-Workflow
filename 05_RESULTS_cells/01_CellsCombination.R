# Combinazioni di cellule utilizzate per organo ####

setwd("../05_RESULTS_cells")

# 0) Impostazioni
cell_combo <- list(
  Gut      = Cell_Gut,
  Kidney   = Cell_Kidney,
  Lung     = Cell_Lung,
  Brain    = Cell_Brain,
  Placenta = Cell_Placenta
)

top_n_per_organ <- 5      # quante combinazioni mostrare per organo (le altre -> "Other")
organ_prefix_to_drop <- "^Cell_"

recode_immune <- function(x) {
  
  x <- as.character(x)
  x <- str_squish(x)   # rimuove spazi extra
  
  dplyr::case_when(
    x %in% c("Leukocytes",
             "Monocytes/Macrophages",
             "Neutrophils") ~ "Immune",
    TRUE ~ x
  )
}

# 1) Funzione: combinazioni per un organo

analyze_cell_combinations <- function(df, organ_name) {
  
  per_article <- df %>%
    mutate(Cell_Category = recode_immune(Cell_Category)) %>%
    filter(!is.na(ID_Article)) %>%
    filter(!is.na(Cell_Category), str_trim(Cell_Category) != "") %>%
    distinct(ID_Article, Cell_Category) %>%
    group_by(ID_Article) %>%
    summarise(
      Combination = str_c(sort(unique(Cell_Category)), collapse = " + "),
      .groups = "drop"
    )
  
  per_combo <- per_article %>%
    group_by(Combination) %>%
    summarise(
      Count = n_distinct(ID_Article),
      Article_IDs = str_c(sort(unique(ID_Article)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(
      Total_Articles = sum(Count),
      Percent = Count / Total_Articles,
      Organ = organ_name
    ) %>%
    select(Organ, Combination, Count, Total_Articles, Percent, Article_IDs)
  
  per_combo
}

# 2) Applica a tutte le tabelle

cells_combo_all <- imap_dfr(
  cell_combo,
  ~ analyze_cell_combinations(.x, .y)
) %>%
  mutate(
    Organ = str_replace(Organ, organ_prefix_to_drop, "") %>% str_replace_all("_", " ")
  )

## Export excel ####

cells_long <- cells_combo_all %>%
  mutate(Percent = round(Percent * 100, 2))

cells_wide_counts <- cells_combo_all %>%
  select(Organ, Combination, Count) %>%
  pivot_wider(names_from = Organ, values_from = Count, values_fill = 0)

cells_wide_percent <- cells_long %>%
  select(Organ, Combination, Percent) %>%
  pivot_wider(names_from = Organ, values_from = Percent, values_fill = 0)

write_xlsx(
  list(
    Long = cells_long,
    Wide_Counts = cells_wide_counts,
    Wide_Percent = cells_wide_percent
  ),
  "01_Cell_Combinations_By_Organ.xlsx"
)


# Mono-culture, co-culture, tri-culture or more ####

analyze_culture_complexity <- function(df, organ_name) {
  
  df %>%
    mutate(Cell_Category = recode_immune(Cell_Category)) %>%
    filter(!is.na(ID_Article)) %>%
    filter(!is.na(Cell_Category), str_trim(Cell_Category) != "") %>%
    distinct(ID_Article, Cell_Category) %>%              # tipi unici per articolo
    group_by(ID_Article) %>%
    summarise(
      n_types = n_distinct(Cell_Category),
      Culture = case_when(
        n_types == 1 ~ "Mono-culture",
        n_types == 2 ~ "Co-culture",
        n_types >= 3 ~ "Tri-culture or more"
      ),
      .groups = "drop"
    ) %>%
    count(Culture, name = "Count") %>%
    mutate(
      Total_Articles = sum(Count),
      Percent = Count / Total_Articles,
      Organ = organ_name
    ) %>%
    select(Organ, Culture, Count, Total_Articles, Percent)
}

culture_by_organ <- imap_dfr(
  cell_combo,
  ~ analyze_culture_complexity(.x, .y)
) %>%
  mutate(
    Organ = str_replace(Organ, "^Cell_", "")
  )

culture_levels <- c("Mono-culture", "Co-culture", "Tri-culture or more")

plot_culture <- culture_by_organ %>%
  mutate(
    Organ = str_replace(Organ, "^Cell_", ""),
    Organ = factor(Organ, levels = organi),
    Culture = factor(Culture, levels = culture_levels)
  ) %>%
  # aggiunge righe mancanti (barre a 0)
  complete(Organ, Culture, fill = list(Count = 0, Percent = 0)) %>%
  group_by(Organ) %>%
  # Totale articoli per organo (serve se vuoi ricostruire Percent in modo robusto)
  mutate(Total_Articles = ifelse(all(Total_Articles == 0 | is.na(Total_Articles)),
                                 sum(Count),
                                 max(Total_Articles, na.rm = TRUE))) %>%
  ungroup() %>%
  # Percentuale in formato "0–100" come nel tuo plot coating
  mutate(Percent100 = Percent * 100)

## Grafico ####
ggplot(plot_culture,
       aes(x = Culture,
           y = Percent100,
           fill = Organ)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  
  # conteggi sopra le barre (anche per 0)
  geom_text(aes(label = ifelse(Culture == "Mono-culture" & Count == 0, "NA", Count)),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5) +
  
  scale_fill_manual(values = colori_organi) +
  
  scale_y_continuous(
    limits = c(0, max(plot_culture$Percent100) * 1.1),
    breaks = seq(0, max(plot_culture$Percent100) * 1.1, by = 20),
    labels = function(x) paste0(round(x, 0), "%")
  ) +
  
  labs(
    title = "Culture complexity across organs",
    x = NULL,
    y = NULL,
    fill = "Organ"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title  = element_text(size = 12, hjust = 0.5, face = "bold")
  )

## Salva il grafico ####
ggsave("01_CultureComplexity_across_organs.png", width = 12, height = 6, dpi = 300, bg = "white")
ggsave("01_CultureComplexity_across_organs.svg", width = 12, height = 6, bg = "white")

