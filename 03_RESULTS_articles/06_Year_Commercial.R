# 7_Year_Commercial ####

# Estrai colonne con anno articoli inclusi ####
chip_year_overall <- bind_rows(
  tibble(Year = Article_Included_Gut$PublicYear,      CommercialChipPlatform = Article_Included_Gut$CommercialChipPlatform),
  tibble(Year = Article_Included_Kidney$PublicYear,   CommercialChipPlatform = Article_Included_Kidney$CommercialChipPlatform),
  tibble(Year = Article_Included_Lung$PublicYear,     CommercialChipPlatform = Article_Included_Lung$CommercialChipPlatform),
  tibble(Year = Article_Included_Brain$PublicYear,    CommercialChipPlatform = Article_Included_Brain$CommercialChipPlatform),
  tibble(Year = Article_Included_Placenta$PublicYear, CommercialChipPlatform = Article_Included_Placenta$CommercialChipPlatform)
) %>%
  filter(!is.na(Year), Year != "", !is.na(CommercialChipPlatform)) %>%
  mutate(
    Year = as.integer(Year),
    ChipType = ifelse(CommercialChipPlatform, "Commercial", "InHouse"),
    ChipType = factor(ChipType, levels = c("InHouse", "Commercial"))
  ) %>%
  select(Year, ChipType)

# Conteggi Year × ChipType + completamento anni mancanti
#year_range <- range(chip_year_overall$Year, na.rm = TRUE)
all_years  <- seq(year_range[1], year_range[2], by = 1)

chip_year_counts <- chip_year_overall %>%
  count(Year, ChipType, name = "Count") %>%
  complete(
    Year = all_years,
    ChipType = levels(chip_year_overall$ChipType),
    fill = list(Count = 0)
  ) %>%
  arrange(Year, ChipType)

#Percentuali dentro ogni anno (calcolate sul totale annuale)
chip_year_perc <- chip_year_counts %>%
  group_by(Year) %>%
  mutate(
    TotalYear = sum(Count),
    Percent = ifelse(TotalYear == 0, 0, Count / TotalYear),
    Label = ifelse(Count == 0, "", paste0(round(Percent * 100, 1), "%\n(", Count, ")"))
  ) %>%
  ungroup()

# Se ChipType non è un factor, forzo l'ordine
chip_year_perc_plot <- chip_year_perc %>%
  mutate(
    ChipType = factor(ChipType, levels = c("Commercial", "InHouse")),
    Label = ifelse(
      TotalYear == 0 | Count == 0,
      "",
      paste0(round(Percent * 100, 1), "% (", Count, ")")
    )
  )

totals_year <- chip_year_perc_plot %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Total = sum(Count), .groups = "drop")

# Grafico Year_Commercial ####
ggplot(chip_year_perc_plot, aes(x = Year, y = Count, fill = ChipType)) +
  
  geom_col_pattern(
    aes(pattern = ChipType),
    fill = "#F2F2F2",          # grigio pieno base
    color = "black",
    linewidth = 0.3,
    width = 0.8,
    
    pattern_fill    = "#F2F2F2",
    pattern_color   = "black",
    pattern_alpha   = 0.3,
    pattern_angle   = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_size    = 0.25
  ) +
  
  # etichette INHOUSE → sotto tutte sulla stessa linea
  geom_text(
    data = subset(chip_year_perc_plot, ChipType == "InHouse"),
    aes(label = ifelse(Count == 0, "", paste0(round(Percent*100,1), "%\n(", Count, ")"))),
    y = -2.4,
    size = 3
  ) +
  
  # etichette COMMERCIAL → sopra la barra
  geom_text(
    data = subset(chip_year_perc_plot, ChipType == "Commercial"),
    aes(y = totals_year$Total[match(Year, totals_year$Year)],
        label = ifelse(Count == 0, "", paste0(round(Percent*100,1), "%\n(", Count, ")"))),
    vjust = -0.4,
    size = 3
  ) +
  
  scale_pattern_manual(values = c(
    InHouse = "none",
    Commercial = "stripe"
  )) +
  
  scale_y_continuous(
    limits = c(-3, 45),             
    breaks = seq(0, 45, by = 5)
  ) +
  
  scale_x_continuous(breaks = sort(unique(chip_year_perc_plot$Year))
  ) +
  
  labs(
    title = "In-house vs Commercial chip usage over time",
    x = NULL,
    y = NULL,
    fill = "Chip type"
  ) +
  
  theme_minimal(base_size = 10) +
    
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black")
  ) + 
    
  guides(
    pattern = guide_legend(
      override.aes = list(
        fill = c("#F2F2F2", "#F2F2F2"),
        colour = c("black", "black"),
        pattern = c("none", "stripe"),
        pattern_angle = c(0, 45),
        pattern_density = c(0, 0.4),
        pattern_spacing = c(0, 0.05),
        pattern_size = c(0, 0.25),
        pattern_colour = c(NA, "black")
      )
    ),
    fill = "none"
  )

## Salva il grafico ####
ggsave("06_Year_Commercial.png", width = 8, height = 4, dpi = 300, bg = "white")
ggsave("06_Year_Commercial.svg", width = 8, height = 4, bg = "white")
  