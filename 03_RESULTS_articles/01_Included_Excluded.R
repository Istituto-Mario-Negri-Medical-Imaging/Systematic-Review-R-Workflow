# 1_Included_Excluded ####

# Crea il dataframe #### 
# Numero excluded-included per ciascun organo 
IncExc_df <- map_df(organi, function(org) { 
  tibble( 
    organ = org, 
    Included = nrow(get(paste0("Article_Included_", org))), 
    Excluded = nrow(get(paste0("Article_Excluded_", org))) 
    ) 
  }) 

# Numero in % excluded-included per ciascun organo 
IncExc_long <- IncExc_df %>% 
  pivot_longer(cols = c(Included, Excluded), 
               names_to = "Status", 
               values_to = "Count") %>% group_by(organ) %>% 
  mutate(Percent = Count / sum(Count)) 

# Conteggio totale per riga Overall 
IncExc_overall <- IncExc_long %>% 
  group_by(Status) %>% 
  summarise(Count = sum(Count)) %>% 
  mutate( 
    organ = "Overall", 
    Percent = Count / sum(Count) 
    ) 

IncExc_long <- bind_rows(IncExc_long, IncExc_overall) 

IncExc_long$organ <- factor( 
  IncExc_long$organ, 
  levels = c("Gut", "Kidney", "Lung", "Brain", "Placenta", "Overall") 
  )

# GRAFICO 1_Included_Excluded ####

# Calcolo linea basata su Overall Included
yline <- IncExc_long %>%
  filter(organ == "Overall", Status == "Included") %>%
  pull(Percent)

ggplot(IncExc_long, 
       aes(x = organ, y = Percent, fill = organ, pattern = Status)) +
  
  geom_col_pattern(
    color = "black",
    width = 0.8,
    pattern_fill    = colori_organi[IncExc_long$organ],
    pattern_color = "black",
    pattern_alpha = 0.3,
    pattern_angle   = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_size    = 0.25
  ) +
  
  scale_fill_manual(values = colori_organi) +
  
  scale_pattern_manual(values = c(
    Included = "none",
    Excluded = "stripe"
  )) +
  
  # TESTO Included (10%)
  geom_text(
    data = subset(IncExc_long, Status == "Included"),
    aes(y = 0.10,
        label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")),
    size = 5
  ) +
  
  # TESTO Excluded (90%)
  geom_text(
    data = subset(IncExc_long, Status == "Excluded"),
    aes(y = 0.90,
        label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")),
    size = 5
  ) +
  
  # Totali sopra le barre
  geom_text(
    data = IncExc_long %>% group_by(organ) %>% summarise(Tot = sum(Count)),
    inherit.aes = FALSE,
    aes(x = organ, y = 1.05, label = Tot),
    size = 5 #cambia la dimensione
    #fontface = "plain" #mettere bold per grassetto
  ) +
  
  # LINEA: percentuale Included Overall
  geom_hline(yintercept = yline,
             linetype = "dashed",
             linewidth = 0.6) +
  
  # Asse Y 0–100 step 20 
  scale_y_continuous(
    limits = c(0, 1.1),
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  theme_minimal(base_size = 12) +
  
  # Titoli e assi
  labs(
    title = "Studies inclusion rates across organs",
    x = NULL,
    y = NULL,
    pattern = NULL        #titolo legenda
  ) +
  
  # GUIDES: personalizza legenda pattern
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
ggsave("01_Included_Excluded.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("01_Included_Excluded.svg", width = 10, height = 6, bg = "white")