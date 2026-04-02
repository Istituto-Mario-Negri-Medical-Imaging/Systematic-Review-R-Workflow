# 5_AimCategory

# Estrai colonne con AimCategory ####

#Togli possibili spazi con tab nelle colonne
Article_Included_Gut <- Article_Included_Gut %>%
  mutate(AimCategory = str_trim(AimCategory))

Article_Included_Kidney <- Article_Included_Kidney %>%
  mutate(AimCategory = str_trim(AimCategory))

Article_Included_Lung <- Article_Included_Lung %>%
  mutate(AimCategory = str_trim(AimCategory))

included_by_organ <- bind_rows(
  tibble(Organ = "Gut",      AimCategory = Article_Included_Gut$AimCategory),
  tibble(Organ = "Kidney",   AimCategory = Article_Included_Kidney$AimCategory),
  tibble(Organ = "Lung",     AimCategory = Article_Included_Lung$AimCategory),
  tibble(Organ = "Brain",    AimCategory = Article_Included_Brain$AimCategory),
  tibble(Organ = "Placenta", AimCategory = Article_Included_Placenta$AimCategory)
) %>%
  filter(!is.na(AimCategory), AimCategory != "") %>%
  mutate(
    Organ = factor(Organ, levels = c("Gut", "Kidney", "Lung", "Brain", "Placenta")),
    AimCategory = factor(AimCategory, levels = c(
      "Chip development, engineering, and monitoring",
      "Physiological modeling",
      "Disease and personalized modeling",
      "Drug testing and toxicity",
      "ADME and pharmacokinetics",
      "Others"
    ))
  )

# Definisci le Aim Category ####
categories <- c(
  "Chip development, engineering, and monitoring",
  "Physiological modeling",
  "Disease and personalized modeling",
  "Drug testing and toxicity",
  "ADME and pharmacokinetics",
  "Others"
)

# Crea i dataframe e completa i dati mancanti ####
aim_df <- included_by_organ %>%
  count(Organ, AimCategory, name = "Count") %>%
  complete(Organ = c("Gut", "Kidney", "Lung", "Brain", "Placenta"), 
           AimCategory = categories, 
           fill = list(Count = 0)) %>%
  group_by(Organ) %>%
  mutate(
    Percent = Count / sum(Count),
    Label = paste0(round(Percent * 100, 1), "% (", Count, ")")
  ) %>%
  ungroup()

# Calcola percentuali ####
aim_by_organ_perc <- aim_df %>%
  filter(Organ != "Overall") %>%   # opzionale, se vuoi escludere Overall
  mutate(Organ = factor(Organ, levels = c("Gut", "Kidney", "Lung", "Brain", "Placenta")))

aim_overall_perc <- aim_df %>%
  group_by(AimCategory) %>%
  summarise(Count = sum(Count)) %>%
  mutate(
    Percent = Count / sum(Count),
    Label = paste0(round(Percent * 100, 1), "% (", Count, ")"),
    LabelPie = paste0(round(Percent * 100, 1), "%\n(", Count, ")"),
    AimCategory = factor(AimCategory, levels = categories)
  )

# Grafico 5_AimCategory_Organ: barre _ per organ ####
aim_by_organ_perc <- aim_by_organ_perc %>%
  mutate(
    AimCategory = factor(AimCategory, levels = c(
      "Chip development, engineering, and monitoring",
      "Physiological modeling",
      "Disease and personalized modeling",
      "Drug testing and toxicity",
      "ADME and pharmacokinetics",
      "Others"
    )),
    Organ = factor(Organ, levels = c("Gut", "Kidney", "Lung", "Brain", "Placenta"))
  )

# Manda a capo AIm Category sotto le barre
aim_by_organ_perc <- aim_by_organ_perc %>%
  mutate(AimCategory_label = recode(AimCategory,
                                    "Chip development, engineering, and monitoring" = "Chip development,\nengineering,\nand monitoring",
                                    "Physiological modeling" = "Physiological\nmodeling",
                                    "Disease and personalized modeling" = "Disease and\npersonalized\nmodeling",
                                    "Drug testing and toxicity" = "Drug testing\nand toxicity",
                                    "ADME and pharmacokinetics" = "ADME and\npharmacokinetics",
                                    "Others" = "Others"
  ))


ggplot(aim_by_organ_perc, aes(x = AimCategory_label, y = Percent, fill = Organ)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +  
  scale_fill_manual(values = colori_organi) +
  
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.35,
            size = 4) +

  
  scale_y_continuous(
    limits = c(0, 0.5),  # 0% - 50%
    breaks = seq(0, 0.5, 0.1),  # intervalli ogni 10%
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "Distribution of aim categories per organ",
    x = NULL,
    y = NULL,
    fill = "Organ" 
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
  )

## Salva il grafico ####
ggsave("05_AimCategory_Organ.png", width = 12, height = 6, dpi = 300, bg = "white")
ggsave("05_AimCategory_Organ.svg", width = 12, height = 6, bg = "white")

# Grafico 5_AimCategory_Overall: pie chart _ overall ####
aim_overall_perc <- aim_overall_perc %>%
  mutate(AimCategory = factor(AimCategory, levels = c(
    "Chip development, engineering, and monitoring",
    "Physiological modeling",
    "Disease and personalized modeling",
    "Drug testing and toxicity",
    "ADME and pharmacokinetics",
    "Others"
  )))

legend_labels <- paste0(
  levels(aim_overall_perc$AimCategory),
  " (",
  aim_overall_perc$Count[match(levels(aim_overall_perc$AimCategory),
                               aim_overall_perc$AimCategory)],
  ", ",
  round(aim_overall_perc$Percent[match(levels(aim_overall_perc$AimCategory),
                                       aim_overall_perc$AimCategory)] * 100, 1),
  "%)"
)

ggplot(aim_overall_perc, aes(x = "", y = Percent, fill = AimCategory)) +
  
  geom_col(color = "white") +
  coord_polar(theta = "y", start = pi/2) + 
  
  scale_fill_manual(
    #values = c(
    #"#F5F5F5", "#E6E6E6", "#D2D2D2", "#898989", "#5A5A5A", "#3C3C3C"),
    values = c(
    "#F5A48B", "#FFE393", "#ACD193", "#98BEE4", "#BC8FDD", "#E6E6E6"),
    #labels = legend_labels
    ) +
  
  labs(
    title = "Distribution of aim categories",
    fill = "Legend"
  ) +
  
  geom_text(
    aes(x = 1.2, label = ifelse(Percent < 0.01, "", LabelPie)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    lineheight = 0.9
  ) +
  
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14 , face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14)
  ) +
  
  guides(
    fill = guide_legend(
      label.hjust = 0,
      override.aes = list(color = NA)
    )
  )

## Salva il grafico ####
ggsave("05_AimCategory_Overall.png", width = 10, height = 4, dpi = 300, bg = "white")
ggsave("05_AimCategory_Overall.svg", width = 10, height = 4, bg = "white")