# Shear stress ####

## Organ and Shear Stress ####

organi <- c("Gut", "Kidney", "Lung", "Brain", "Placenta")

# 1) Pulizia dati e conversione unità
Cell_shear_clean <- Cell_All %>%
  select(Organ_cell, ShearStress, ShearStressUnit) %>%
  filter(!is.na(Organ_cell), !is.na(ShearStress), !is.na(ShearStressUnit)) %>%
  mutate(
    ShearStress = as.numeric(str_replace(as.character(ShearStress), ",", "."))
  ) %>%
  mutate(
    ShearStress = case_when(
      ShearStressUnit == "Pa"  ~ ShearStress / 10,
      ShearStressUnit == "mPa" ~ ShearStress / 10000,
      TRUE ~ ShearStress
    ),
  ShearStressUnit = "dyne/cm2"
  ) %>%
  filter(!is.na(ShearStress), ShearStress > 0) %>%
  mutate(
    Organ_cell = factor(Organ_cell, levels = organi)
  ) %>%
  filter(!is.na(Organ_cell))

write_xlsx(
  list(
    Sheet1 = Cell_shear_clean
  ),
  "04_ShearStress_By_Organ.xlsx"
)


# 2) Funzione per mediana + 95% CI bootstrap
median_ci_boot <- function(x, conf = 0.95, nboot = 2000) {
  x <- x[is.finite(x)]
  
  median_value <- median(x, na.rm = TRUE)
  
  boot_medians <- replicate(nboot, {
    median(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_medians, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  data.frame(
    median_shear = median_value,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

# 3) Calcolo mediana e 95% CI per organo
shear_df <- Cell_shear_clean %>%
  group_by(Organ_cell) %>%
  do(median_ci_boot(.$ShearStress)) %>%
  ungroup()

# 2) Funzione per media + 95% CI bootstrap
mean_ci_boot <- function(x, conf = 0.95, nboot = 2000) {
  x <- x[is.finite(x)]
  
  mean_value <- mean(x, na.rm = TRUE)
  
  boot_means <- replicate(nboot, {
    mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_means, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  data.frame(
    mean_shear = mean_value,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}

# 3) Calcolo media e 95% CI per organo
shear_df <- Cell_shear_clean %>%
  group_by(Organ_cell) %>%
  do(mean_ci_boot(.$ShearStress)) %>%
  ungroup()


## Grafico gray ####
ggplot(Cell_shear_clean, aes(x = Organ_cell, y = ShearStress)) +
  geom_jitter(
    width = 0.28,
    size = 2.5,
    alpha = 0.9,
    shape = 21,
    stroke = 0.9,
    color = "black",
    fill = "lightgrey"
  ) +
  geom_errorbar(
    data = shear_df,
    aes(x = Organ_cell, ymin = ci_low, ymax = ci_high),
    width = 0.16,
    linewidth = 0.8,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_crossbar(
    data = shear_df,
    aes(
      x = Organ_cell,
      y = mean_shear,
      ymin = mean_shear,
      ymax = mean_shear
    ),
    width = 0.48,
    fatten = 0,
    linewidth = 0.9,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_y_log10(
    breaks = 10^seq(-7, 2, by = 1),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Shear stress",
    x = NULL,
    y = expression(paste("dynes/cm"^2))
  ) +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 15)),
    axis.text.x = element_text(size = 18, face = "bold", color = "black"),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),
    axis.line = element_line(linewidth = 1.8, color = "black"),
    axis.ticks = element_line(linewidth = 1.8, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none"
  )

## Grafico color ####
ggplot(Cell_shear_clean, aes(x = Organ_cell, y = ShearStress)) +
  geom_jitter(
    aes(color = Organ_cell),
    width = 0.18,
    size = 2.5,
    alpha = 1,
    shape = 16
  ) +
  geom_errorbar(
    data = shear_df,
    aes(x = Organ_cell, ymin = ci_low, ymax = ci_high),
    width = 0.08,
    linewidth = 0.6,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_crossbar(
    data = shear_df,
    aes(
      x = Organ_cell,
      y = median_shear,
      ymin = median_shear,
      ymax = median_shear
    ),
    width = 0.45,
    fatten = 0,
    linewidth = 0.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = colori_organi) +
  scale_y_log10(
    breaks = 10^seq(-7, 2, by = 1),
    labels = function(x) parse(text = paste0("10^", log10(x)))
    #labels = scales::label_math(10^.x)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Shear stress",
    x = NULL,
    y = expression(paste("Shear stress (dyne/cm"^2, " log"[10], ")"))
  ) +
  theme(
    axis.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

ggsave("04_shearstress_by_organ_median.png", width = 11, height = 7, dpi = 300, bg = "white")
ggsave("04_shearstress_by_organ_median.svg", width = 11, height = 7, bg = "white")
