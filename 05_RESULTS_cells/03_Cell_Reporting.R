# Reporting Dati sulle cellule ####

cell_reporting <- Cell_All %>%
  distinct(ID_Article, Organ_cell, ID_Cell, .keep_all = TRUE) %>%
  transmute(
    ID_Article,
    ID_Cell,
    Organ_cell,
    Flow,
    #Flow = as.logical(Flow),
    Flow = replace_na(as.logical(Flow), FALSE),
    CellDensity  = !is.na(CellDensity) & CellDensity != 0,
    CellCultureTime   = !is.na(CellCultureTime) & CellCultureTime != 0,
    
    # Se Flow == FALSE => NA (non considerato)
    FlowRate    = if_else(Flow, !is.na(FlowRate) & FlowRate != 0, NA),
    ShearStress = if_else(Flow, !is.na(ShearStress) & ShearStress != 0, NA),
    FlowTime    = if_else(Flow, !is.na(FlowTime) & FlowTime != 0, NA)
  )

cell_summary <- cell_reporting %>%
  pivot_longer(
    cols = -c(ID_Article, ID_Cell, Organ_cell, Flow),
    names_to = "Parameter",
    values_to = "Reported"
  ) %>%
  filter(!is.na(Reported)) %>%  # <-- fondamentale: rimuove i non-applicabili
  mutate(Status = ifelse(Reported, "Reported", "Not reported")) %>%
  count(Parameter, Status, name = "Count") %>%
  group_by(Parameter) %>%
  mutate(
    Total = sum(Count),
    Percent = Count / Total
  ) %>%
  ungroup()

cell_summary <- cell_summary %>%
  mutate(Parameter = factor(Parameter, levels = c("CellDensity", "CellCultureTime", "FlowRate", "FlowTime", "ShearStress")))


## GRAFICO_CellParameters_Reporting ####
ggplot(cell_summary,
       aes(x = Parameter, y = Percent, pattern = Status)) +
  
  # BARRE
  geom_col_pattern(
    fill = "#F2F2F2",
    color = "black",
    width = 0.8,
    pattern_fill    = "#F2F2F2",
    pattern_color   = "black",
    pattern_alpha   = 0.3,
    pattern_angle   = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_size    = 0.25
  ) +
  
  scale_pattern_manual(values = c(
    `Not reported` = "stripe",
    Reported = "none"
  )) +
  
  # TESTO Reported (parte bassa): sotto la barra
  geom_text(
    data = subset(cell_summary, Status == "Reported"),
    aes(
      y = -0.07,
      label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")
    ),
    size = 4
  ) +
  
  # TESTO Not reported (parte alta): sopra la barra
  geom_text(
    data = subset(cell_summary, Status == "Not reported"),
    aes(
      y = 1.07,
      label = paste0(round(Percent * 100, 1), "%\n(", Count, ")")
    ),
    size = 4
  ) +
  
  # Asse Y 0–100 step 20
  scale_y_continuous(
    limits = c(-0.12, 1.12), #modifica i limiti dell'asse y
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  theme_minimal(base_size = 12) +
  
  labs(
    title = "Reporting of cell parameters",
    x = NULL,
    y = NULL,
    pattern = NULL
  ) +
  
  guides(
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
    axis.text.x = element_text(size = 11, color = "black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black"),
    plot.title  = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = "black"),
    legend.position = "right"
  )

## Salva il grafico ####
ggsave("03_Cell_Reporting_Stack100.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("03_Cell_Reporting_Stack100.svg", width = 10, height = 6, bg = "white")


