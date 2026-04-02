# Number of readouts per article by organ ####

# 1) Pulizia dati
readout_count_clean <- Readout_All %>%
  select(ID_Article, ID_Readout, Organ_readout) %>%
  filter(!is.na(ID_Article), !is.na(ID_Readout), !is.na(Organ_readout)) %>%
  mutate(
    ID_Article = str_trim(as.character(ID_Article)),
    ID_readout = str_trim(as.character(ID_Readout)),
    Organ_readout = str_trim(as.character(Organ_readout))
  ) %>%
  filter(!is.na(ID_Article), !is.na(ID_Readout), !is.na(Organ_readout)) %>%
  mutate(
    Organ_readout = factor(Organ_readout, levels = organi)
  ) %>%
  filter(!is.na(Organ_readout))

# 2) Conta quanti readout per ogni coppia (ID_Article, Organ_readout)
readout_counts <- readout_count_clean %>%
  count(ID_Article, Organ_readout, name = "n_readouts")

# 3) Conta quanti articoli hanno ciascun numero di readout
plot_readout_counts <- readout_counts %>%
  count(n_readouts, name = "n_articles")

# 3) Conta quanti articoli hanno ciascun numero di readout, per ogni organo
plot_readout_counts_organ <- readout_counts %>%
  count(Organ_readout, n_readouts, name = "n_articles")

## Export Excel
write_xlsx(
  list(
    counts = plot_readout_counts,
    counts_organ = plot_readout_counts_organ
  ),
  "02_Readouts_Counts.xlsx"
)

## Grafico overall ####
ggplot(plot_readout_counts, aes(x = n_readouts, y = n_articles)) +
  geom_col(fill = "grey80", color = "black", width = 0.8) +
  scale_x_continuous(breaks = seq(min(plot_readout_counts$n_readouts), max(plot_readout_counts$n_readouts), by = 1)) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of readouts per article",
    x = "Number of readouts",
    y = "Number of articles"
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

ggsave("02_Readout_count.png", width = 8, height = 4, dpi = 300, bg = "white")

## Grafico per organo ####
ggplot(plot_readout_counts_organ, aes(x = n_readouts, y = n_articles, fill = Organ_readout)) +
  geom_col(color = "black", width = 0.8) +
  facet_wrap(~ Organ_readout, nrow = 1) +
  scale_fill_manual(values = colori_organi) +
  scale_x_continuous(
    breaks = seq(
      min(plot_readout_counts_organ$n_readouts),
      max(plot_readout_counts_organ$n_readouts),
      by = 1
    )
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of readouts per article by organ",
    x = "Number of readouts",
    y = "Number of articles"
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

ggsave("02_Readout_count_organ.png", width = 12, height = 10, dpi = 300, bg = "white")
