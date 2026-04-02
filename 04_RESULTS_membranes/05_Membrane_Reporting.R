# 05: Reporting Dati sulle membrane ####

applicabili_pore_spacing <- c(1,2,8,9,14,15,16,17,22,29)

ordine_parametri <- c("Material", "Thickness", "PoreSize", "Porosity", "Spacing", "Stiffness", "OtherTests")

ordine_status <- c("Reported", "Not applicable", "Not reported")

membrane_reporting <- Membranes_All %>%
  distinct(ID_Article, Organ_membrane, .keep_all = TRUE) %>%
  transmute(
    ID_Article,
    Organ_membrane,
    Material = case_when(ID_MembraneMaterial == 9  ~ NA, TRUE ~ (ID_MembraneMaterial != 13)),
    Thickness = case_when(ID_MembraneMaterial == 9 ~ NA, FALSE ~ is.na(Thickness), TRUE ~ !is.na(Thickness)),
    Porosity = case_when(ID_MembraneMaterial == 9 ~ NA, FALSE ~ is.na(Porosity_or_PoreDensity), TRUE ~ !is.na(Porosity_or_PoreDensity)),
    PoreSize = case_when((ID_MembraneMaterial %in% applicabili_pore_spacing) ~ NA, FALSE ~ is.na(PoreSize), TRUE ~ !is.na(PoreSize)),
    Spacing = case_when((ID_MembraneMaterial %in% applicabili_pore_spacing) ~ NA, FALSE ~ is.na(Spacing), TRUE ~ !is.na(Spacing)),
    Stiffness = case_when(ID_MembraneMaterial == 9 ~ NA, FALSE ~ is.na(Stiffness_or_Young.s_modulus), TRUE ~ !is.na(Stiffness_or_Young.s_modulus)),
    OtherTests = case_when(ID_MembraneMaterial == 9 ~ NA, FALSE ~ is.na(Other_tests) & Other_tests == "", TRUE ~ !is.na(Other_tests) & Other_tests != "")
  )

membrane_summary <- membrane_reporting %>%
  pivot_longer(
    cols = -c(ID_Article, Organ_membrane),
    names_to = "Parameter",
    values_to = "Reported"
  ) %>%
  mutate(
    Status = case_when(
      is.na(Reported) ~ "Not applicable",
      Reported        ~ "Reported",
      TRUE            ~ "Not reported"
    ),
    Parameter = factor(Parameter, levels = ordine_parametri),
    Status    = factor(Status, levels = ordine_status)
  ) %>%
  count(Parameter, Status, name = "Count") %>%
  group_by(Parameter) %>%
  mutate(
    Total = sum(Count),
    Percent = Count / Total
  ) %>%
  ungroup()

#membrane_summary <- membrane_summary %>%
#  mutate(Parameter = factor(Parameter, levels = ordine_parametri)) %>%
#  mutate(Status = factor(Status, levels = c("Reported", "Not applicable", "Not reported")))

# GRAFICO_MembraneParameters_Reporting ####
ggplot(membrane_summary,
       aes(x = Parameter, y = Percent, fill = Status, pattern = Status)) +
  
  geom_col_pattern(
    color = "black",
    width = 0.8,
    position = position_stack(reverse = TRUE),
    
    # impostazioni pattern (valgono solo quando pattern != "none")
    pattern_fill    = "#F2F2F2",
    pattern_color   = "black",
    pattern_alpha   = 0.3,
    pattern_angle   = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_size    = 0.25
  ) +
  
  scale_fill_manual(values = c(
    "Reported" = "#F2F2F2",
    "Not applicable" = "white",
    "Not reported" = "#F2F2F2"
  )) +
  
  scale_pattern_manual(values = c(
    "Reported" = "none",
    "Not applicable" = "none",
    "Not reported" = "stripe"
  ), breaks = ordine_status) +
  
  # TESTO Reported (parte bassa): sotto la barra
  geom_text(
    data = subset(membrane_summary, Status == "Reported"),
    aes(
      y = -0.07,
      label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")
    ),
    size = 4
  ) +
  
  # TESTO Not reported (parte alta): sopra la barra
  geom_text(
    data = subset(membrane_summary, Status == "Not reported"),
    aes(
      y = 1.07,
      label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")
    ),
    size = 4
  ) +
  
  # TESTO Not applicable (dentro, al centro)
  geom_text(
    data = subset(membrane_summary, Status == "Not applicable"),
    aes(y= 0.5, label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")),
    #position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 4
  ) +
  
  # Asse Y 0–100 step 20
  scale_y_continuous(
    limits = c(-0.12, 1.12), #modifica i limiti dell'asse y
    breaks = base::seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  theme_minimal(base_size = 12) +
  
  labs(
    title = "Reporting of membrane parameters",
    x = NULL,
    y = NULL,
    pattern = NULL,
    fill = NULL
  ) +
  
  guides(
    fill = "none",
    pattern = guide_legend(
      override.aes = list(
        fill    = c("#F2F2F2", "white", "#F2F2F2"),
        colour  = c("black", "black", "black"),
        pattern = c("none", "none", "stripe")
      )
    )
  ) +
  
  theme(
    axis.text.x = element_text(size = 11, color = "black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black"),
    plot.title  = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = "black"),
    legend.position = "right"
  )

# Salva il grafico ####
ggsave("05_Membrane_Reporting_Stack100_1.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("05_Membrane_Reporting_Stack100_1.svg", width = 10, height = 6, bg = "white")
