# 06: Material and Membrane parameters
# Box plot ####

## Material Category and Thikness: correlation ####

# 1) Converti Thickness in micrometri (µm) quando l'unità non è µm
Membranes_thickness_clean <- Membranes_All %>%
  select(ID_Article, Title, Thickness, ThicknessUnit, Organ_membrane, Membrane_Origin, CommercialChipPlatform) %>%
  filter(!is.na(ThicknessUnit)) %>%
  mutate(Thickness = as.numeric(str_replace(as.character(Thickness), ",", "."))) %>%
  mutate(
    Thickness = case_when(
      ThicknessUnit == "nm" ~ Thickness / 1000,
      ThicknessUnit == "mm" ~ Thickness * 1000,
      ThicknessUnit == "µm" ~ Thickness,
      TRUE ~ Thickness
    )
  ) %>%
  filter(Membrane_Origin != "Mix") %>%
  filter(Thickness != 3000)

Membranes_thickness_clean <- Membranes_thickness_clean %>%
  mutate(
    Membrane_Origin = factor(Membrane_Origin, levels = c("Synthetic", "Natural")),
    CommercialChipPlatform = factor(
      CommercialChipPlatform,
      levels = c(TRUE, FALSE),
      labels = c("Commercial", "In-house")),
    Group = interaction(Membrane_Origin, CommercialChipPlatform, sep = " - ", lex.order = TRUE)
  )

Membranes_thickness_clean$Group <- factor(
  Membranes_thickness_clean$Group,
  levels = c(
    "Synthetic - Commercial",
    "Synthetic - In-house",
    "Natural - Commercial",
    "Natural - In-house"
  )
)

# Grafico con tutti i punti + mediana nera + 95% CI
### mediana ####
median_ci_boot <- function(x, conf = 0.95, nboot = 2000) {
  x <- x[is.finite(x)]
  
  med <- median(x, na.rm = TRUE)
  
  boot_medians <- replicate(nboot, {
    median(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_medians, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  data.frame(
    median_thickness = med,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

thickness_df <- Membranes_thickness_clean %>%
  group_by(Membrane_Origin, CommercialChipPlatform) %>%
  do(median_ci_boot(.$Thickness)) %>%
  ungroup()

### media ####
# Funzione per media + 95% CI bootstrap
mean_ci_boot <- function(x, conf = 0.95, nboot = 2000) {
  x <- x[is.finite(x)]
  
  mean_value <- mean(x, na.rm = TRUE)
  
  boot_means <- replicate(nboot, {
    mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_means, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  data.frame(
    mean_thickness = mean_value,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

thickness_df <- Membranes_thickness_clean %>%
  group_by(Membrane_Origin, CommercialChipPlatform) %>%
  do(mean_ci_boot(.$Thickness)) %>%
  ungroup()

### Grafico scala logaritmica ####
ggplot(Membranes_thickness_clean,
       aes(x = CommercialChipPlatform, y = Thickness)) +
  geom_hline(
    yintercept = c(0.5, 5, 50, 500),
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin),
    width = 0.12, size = 2.2, alpha = 0.9
  ) +
  geom_errorbar(
    data = thickness_df,
    aes(
      x = CommercialChipPlatform,
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 0.08,
    linewidth = 0.6,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_crossbar(
    data = thickness_df,
    aes(
      x = CommercialChipPlatform,
      y = median_thickness,
      ymin = median_thickness,
      ymax = median_thickness
    ),
    width = 0.45,
    fatten = 0,
    linewidth = 0.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Membrane_Origin, nrow = 1) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house" = "#F5A48B"),
    name = "Platform"
  ) +
  scale_shape_manual(
    values = c("Synthetic" = 16, "Natural" = 17),
    name = "Material category"
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.5, 1, 5, 10, 50, 100, 500),
    labels = c("0.1", "0.5", "1", "5", "10", "50", "100", "500")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Thickness",
    x = NULL,
    y = expression(paste("Thickness (", mu, "m, log"[10], ")"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

### Grafico scala normale ####
ggplot(Membranes_thickness_clean,
       aes(x = CommercialChipPlatform, y = Thickness)) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin),
    width = 0.12, size = 2.2, alpha = 0.9
  ) +
  geom_errorbar(
    data = thickness_df,
    aes(
      x = CommercialChipPlatform,
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 0.08,
    linewidth = 0.6,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_crossbar(
    data = thickness_df,
    aes(
      x = CommercialChipPlatform,
      y = median_thickness,
      ymin = median_thickness,
      ymax = median_thickness
    ),
    width = 0.45,
    fatten = 0,
    linewidth = 0.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Membrane_Origin, nrow = 1) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house" = "#F5A48B"),
    name = "Platform"
  ) +
  scale_y_continuous(limits = c(0, 250)) +
  scale_shape_manual(
    values = c("Synthetic" = 16, "Natural" = 17),
    name = "Material category"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Thickness",
    x = NULL,
    y = expression(paste("Thickness (", mu, "m)"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

ggsave("06_boxplot_material_thickness_median.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("06_boxplot_material_thickness_median.svg", width = 10, height = 6, bg = "white")

# Boxplot thickness per categoria di materiale (Membrane_Origin) e chip (Commercial/In-house)
ggplot(Membranes_thickness_clean,
       aes(x = CommercialChipPlatform, y = Thickness)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin), width = 0.12, size = 2, alpha = 0.9 ) +
  facet_wrap(~ Membrane_Origin, nrow = 1) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house"   = "#F5A48B"),
    name = "Platform"
  ) +
  scale_shape_manual(
    values = c("Synthetic" = 16, "Natural"   = 17),
    name = "Material category"
  ) +
  scale_y_continuous(limits = c(0, 250)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Thickness",
    x = NULL,
    y = expression(paste("Thickness (", mu, "m)"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text  = element_text(size = 12, face = "bold"),
    plot.title  = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

ggsave("06_boxplot_material_thickness_old.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("06_boxplot_material_thickness_old.svg", width = 10, height = 6, bg = "white")


## Material Category and PoreSize: correlation ####
Membranes_Pore_clean <- Membranes_All %>%
  select(ID_Article, Title, PoreSize, PoreSizeUnit,
         Organ_membrane, Membrane_Origin, CommercialChipPlatform) %>%
  filter(!is.na(PoreSizeUnit)) %>%
  filter(str_to_lower(PoreSizeUnit) %in% c("µm", "μm", "um")) %>%
  mutate(PoreSize = as.numeric(str_replace(as.character(PoreSize), ",", "."))) %>%
  filter(!is.na(PoreSize)) %>%
  filter(Membrane_Origin != "Unknown",
         Membrane_Origin != "Mix") %>%
  mutate(
    Membrane_Origin = factor(
      Membrane_Origin, 
      levels = c("Synthetic", "Natural")),
    CommercialChipPlatform = factor(
      CommercialChipPlatform,
      levels = c(TRUE, FALSE),
      labels = c("Commercial", "In-house")
    )
  )

# Grafico con tutti i punti + mediana nera + 95% CI
### mediana ####
median_ci_boot <- function(x, conf = 0.95, nboot = 2000) {
  x <- x[is.finite(x)]
  
  med <- median(x, na.rm = TRUE)
  
  boot_medians <- replicate(nboot, {
    median(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_medians, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  data.frame(
    median_value = med,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

pore_df <- Membranes_Pore_clean %>%
  group_by(Membrane_Origin, CommercialChipPlatform) %>%
  do(median_ci_boot(.$PoreSize)) %>%
  ungroup()

### Grafico scala logaritmica ####
ggplot(Membranes_Pore_clean,
       aes(x = CommercialChipPlatform, y = PoreSize)) +
  geom_hline(
    yintercept = c(0.5, 5, 50),
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin),
    width = 0.12, size = 2.2, alpha = 0.9
  ) +
  geom_errorbar(
    data = pore_df,
    aes(
      x = CommercialChipPlatform,
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 0.08,
    linewidth = 0.6,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_crossbar(
    data = pore_df,
    aes(
      x = CommercialChipPlatform,
      y = median_value,
      ymin = median_value,
      ymax = median_value
    ),
    width = 0.45,
    fatten = 0,
    linewidth = 0.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Membrane_Origin, nrow = 1) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house" = "#F5A48B"),
    name = "Platform"
  ) +
  scale_shape_manual(
    values = c("Synthetic" = 16, "Natural" = 17),
    name = "Material category"
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.5, 1, 5, 10, 50, 100),
    labels = c("0.1", "0.5", "1", "5", "10", "50", "100")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Pore size",
    x = NULL,
    y = expression(paste("Pore size (", mu, "m, log"[10], ")"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right",
  )

ggsave("06_boxplot_material_poresize.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave("06_boxplot_material_poresize.svg", width = 10, height = 6, bg = "white")

### Grafico scala normale ####
ggplot(Membranes_Pore_clean,
        aes(x = CommercialChipPlatform, y = PoreSize)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin), width = 0.12, size = 2, alpha = 0.9) +
  facet_wrap(~ Membrane_Origin, nrow = 1) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house"   = "#F5A48B"),
    name = "Platform"
  ) +
  scale_shape_manual(
    values = c("Synthetic" = 16, "Natural"   = 17),
    name = "Material category"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Pore size",
    x = NULL,
    y = expression(paste("Pore size (", mu, "m)"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    strip.text  = element_text(size = 12, face = "bold"),
    plot.title  = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

ggsave("06_boxplot_material_poresize_old.png", width = 8, height = 6, dpi = 300, bg = "white")
ggsave("06_boxplot_material_poresize_old.svg", width = 8, height = 6, bg = "white")

## Material Category and Spacing: correlation ####
Membranes_Spacing_clean <- Membranes_All %>%
  select(ID_Article, Title, Spacing, SpacingUnit,
         Organ_membrane, Membrane_Origin, CommercialChipPlatform) %>%
  filter(!is.na(SpacingUnit)) %>%
  filter(str_to_lower(SpacingUnit) %in% c("µm", "μm", "um")) %>%
  mutate(Spacing = as.numeric(str_replace(as.character(Spacing), ",", "."))) %>%
  filter(!is.na(Spacing)) %>%
  filter(Membrane_Origin == "Synthetic") %>%   # spacing solo per sintetiche
  mutate(
    Membrane_Origin = factor(Membrane_Origin,
                             levels = c("Synthetic")),
    CommercialChipPlatform = factor(
      CommercialChipPlatform,
      levels = c(TRUE, FALSE),
      labels = c("Commercial", "In-house")
    )
  )

### Grafico ####
ggplot(Membranes_Spacing_clean,
       aes(x = CommercialChipPlatform, y = Spacing)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(
    aes(color = CommercialChipPlatform, shape = Membrane_Origin), width = 0.12, size = 2, alpha = 0.9) +
  scale_color_manual(
    values = c("Commercial" = "#ACD193", "In-house"   = "#F5A48B"),
    name = "Platform"
  ) +
  scale_shape_manual(
    values = c("Synthetic" = 16),
    name = "Material category"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Spacing",
    x = NULL,
    y = expression(paste("Spacing (", mu, "m)"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    plot.title  = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

ggsave("06_boxplot_material_spacing_old.png", width = 8, height = 6, dpi = 300, bg = "white")
ggsave("06_boxplot_material_spacing_old.svg", width = 8, height = 6, bg = "white")

## Material Category and Stiffness: correlation ####
### Salva Excel ####
Membranes_Stiffness_clean <- Membranes_All %>%
  select(ID_Article, Title, Stiffness_or_Young.s_modulus, Stiffness_unit, Thickness, ThicknessUnit, Other_tests, Organ_membrane, Membrane_Origin, CommercialChipPlatform) %>%
  filter(!is.na(Stiffness_or_Young.s_modulus)) 

write_xlsx(
  list(
    stiffness_article = Membranes_Stiffness_clean
  ),
  "06_Material_Stiffness_Article.xlsx"
)
