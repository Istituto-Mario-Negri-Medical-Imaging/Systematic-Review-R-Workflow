# 3_InclusionYear ####

# Estrai colonne con anno articoli inclusi ####
year_by_organ <- bind_rows(
  tibble(Organ = "Gut",      Year = Article_Included_Gut$PublicYear),
  tibble(Organ = "Kidney",   Year = Article_Included_Kidney$PublicYear),
  tibble(Organ = "Lung",     Year = Article_Included_Lung$PublicYear),
  tibble(Organ = "Brain",    Year = Article_Included_Brain$PublicYear),
  tibble(Organ = "Placenta", Year = Article_Included_Placenta$PublicYear)
) %>%
  filter(!is.na(Year), Year != "") %>%
  mutate(
    Year = as.integer(Year),
    Organ = factor(Organ, levels = c("Brain", "Lung", "Gut", "Kidney", "Placenta"))
  )

# Crea i dataframe ####
year_overall_df <- year_by_organ %>%
  count(Year, name = "Count") %>%
  arrange(Year)

year_by_organ_df <- year_by_organ %>%
  count(Organ, Year, name = "Count") %>%
  arrange(Organ, Year)

# Completa gli anni mancanti ####
year_range <- range(year_by_organ$Year, na.rm = TRUE)

year_overall_complete <- year_overall_df %>%
  complete(
    Year = seq(year_range[1], year_range[2], by = 1),
    fill = list(Count = 0)
  )

year_by_organ_complete <- year_by_organ_df %>%
  complete(
    Organ,
    Year = seq(year_range[1], year_range[2], by = 1),
    fill = list(Count = 0)
  )

# Calcola le percentuali ####
year_overall_perc <- year_overall_complete %>%
  mutate(Percent = Count / sum(Count))

year_overall_perc <- year_overall_perc %>%
  mutate(Label = paste0(round(Percent * 100, 1), "%\n(", Count, ")"))

# GRAFICO 3_InclusionYear ####
ggplot(year_overall_perc,
    aes(x = factor(Year), y = Count)) +
  geom_col(width = 0.8, fill = "#F2F2F2", color = "black", linewidth = 0.3) +

  geom_text(aes(label = ifelse(Count == 0, "", paste0(round(Count / sum(Count) * 100, 1), "%\n(", Count,")"))),
            vjust = -0.5, 
            size = 3) +
  
  # Modifica l'asse y
  scale_y_continuous(
    #labels = scales::percent_format(accuracy = 1),
    limits = c(0, 45),             # Limiti
    breaks = seq(0, 45, by = 5) # Intervalli
  ) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Annual distribution of included studies",
    x = NULL, #"Publication Year",
    y = NULL
  ) +
  
  theme(
    axis.text.x = element_text(color = "black", angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
  )

## Salva il grafico ####
ggsave("03_InclusionYear.png", width = 8, height = 4, dpi = 300, bg = "white")
ggsave("03_InclusionYear.svg", width = 8, height = 4, bg = "white")