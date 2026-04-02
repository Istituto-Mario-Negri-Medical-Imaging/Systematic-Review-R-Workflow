# 4_Commercial ####

# Crea il dataframe #### 
commercial_df <- map_df(organi, function(org) {
  df <- get(paste0("Article_Included_", org))
  tibble(
    organ = org,
    Commercial = sum(df$CommercialChipPlatform == TRUE, na.rm = TRUE),
    InHouse = sum(df$CommercialChipPlatform == FALSE, na.rm = TRUE)
  )
})

commercial_long <- commercial_df %>%
  pivot_longer(cols = c(InHouse, Commercial),
               names_to = "ChipType",
               values_to = "Count") %>%
  group_by(organ) %>%
  mutate(Percent = Count / sum(Count))

# Calcola overall #### 

commercial_overall <- commercial_long %>%
  group_by(ChipType) %>%
  summarise(Count = sum(Count)) %>%
  mutate(organ = "Overall", Percent = Count / sum(Count))

commercial_long <- bind_rows(commercial_long, commercial_overall)

commercial_long$organ <- factor(
  commercial_long$organ,
  levels = c("Gut", "Kidney", "Lung", "Brain", "Placenta", "Overall")
)

# GRAFICO 4_Commercial ####

# Linea Overall Commercial
yline <- commercial_long %>%
  filter(organ == "Overall", ChipType == "InHouse") %>%
  pull(Percent)

ggplot(commercial_long, 
       aes(x = organ, y = Percent, fill = organ, pattern = ChipType)) +
  
  geom_col_pattern(
    color = "black",
    width = 0.8,
    pattern_fill    = colori_organi[commercial_long$organ],
    pattern_color = "black",
    pattern_alpha = 0.3,
    pattern_angle   = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_size    = 0.25
  ) +
  
  scale_fill_manual(values = colori_organi) +
  
  scale_pattern_manual(values = c(
    InHouse = "none",
    Commercial = "stripe"
  )) +
  
  geom_text(
    data = subset(commercial_long, ChipType == "InHouse"),
    aes(y = 0.10,
        label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")),
    size = 5
  ) +
  
  geom_text(
    data = subset(commercial_long, ChipType == "Commercial"),
    aes(y = 0.90,
        label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")),
    size = 5
  ) +
  
  geom_text(
    data = commercial_long %>% group_by(organ) %>% summarise(Tot = sum(Count)),
    inherit.aes = FALSE,
    aes(x = organ, y = 1.05, label = Tot),
    size = 5
  ) +
  
  geom_hline(yintercept = yline,
             linetype = "dashed",
             linewidth = 0.6) +
  
  scale_y_continuous(
    limits = c(0, 1.1),
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  theme_minimal(base_size = 12) +
  
  labs(
    title = "In-house vs Commercial chip usage across organs",
    x = NULL,
    y = NULL,
    pattern = NULL
  ) +
  
  guides(
    fill = "none",
    pattern = guide_legend(
      override.aes = list(
        fill = c("white", "white"),
        colour = c("black", "black"),
        pattern = c("none", "stripe"),
        pattern_angle = c(0, 45),
        pattern_density = c(0, 0.4),
        pattern_spacing = c(0, 0.05),
        pattern_size = c(0, 0.25),
        pattern_colour = c(NA, "black")
      )
    )
  ) +
  
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.text  = element_text(size = 14),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = "black"),
    legend.position = "right"
  )

## Salva il grafico ####
ggsave("04_Commercial.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("04_Commercial.svg", width = 10, height = 6, bg = "white")
