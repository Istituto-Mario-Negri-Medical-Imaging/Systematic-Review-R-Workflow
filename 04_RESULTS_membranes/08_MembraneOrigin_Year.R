# MembraneOrigin over time x year ####

# prendi i dati
mem_origin_year <- Membranes_All %>%
  select(ID_Article, ID_MembraneMaterial, Organ_membrane, Membrane_Origin, PublicYear) %>%
  distinct(ID_Article, ID_MembraneMaterial, Organ_membrane, .keep_all = TRUE) %>%
  mutate(PublicYear = as.integer(PublicYear))

origin_year_long <- mem_origin_year %>%
  count(PublicYear, Membrane_Origin, name = "N_Membranes")

#conteggi
origin_year_wide_counts <- origin_year_long %>%
  pivot_wider(
    names_from = Membrane_Origin,
    values_from = N_Membranes,
    values_fill = 0
  ) %>%
  arrange(PublicYear)

## Salva Excel
write_xlsx(
  list(Wide_Counts = origin_year_wide_counts),
  "08_MembraneOrigin_by_PublicYear.xlsx"
)

# Ottieni i dati in percentuale per il grafico
origin_year_plot <- origin_year_wide_counts %>%
  pivot_longer(-PublicYear,
               names_to = "Membrane_Origin",
               values_to = "Count") %>%
  group_by(PublicYear) %>%
  mutate(Total_Year = sum(Count), Percent_within_year = Count / Total_Year * 100) %>%
  ungroup()

# Grafico (DA SISTEMARE)
ggplot(origin_year_plot,
       aes(x = factor(PublicYear),
           y = Count,
           fill = Membrane_Origin)) +
  
  geom_bar(stat = "identity") +
  
  theme_minimal(base_size = 12) +
  
  labs(
    title = "Articles per year by Membrane Origin",
    x = "Publication Year",
    y = "Number of Articles",
    fill = "Membrane Origin"
  )
ggsave("08_MembraneOrigin_Year.png", width = 12, height = 8, dpi = 300, bg = "white")
