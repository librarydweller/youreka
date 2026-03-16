#Load Library
library(tidyverse)
library (broom)
library (patchwork)
df <- read.csv("filtered_youreka_set.csv")

# Convert to long format
df_long <- df %>%
  pivot_longer(
    cols = c(
      `Tumor.Drug.Accumulation`,
      `Heart.Drug.Accumulation`,
      `Liver.Drug.Accumulation`,
      `Spleen.Drug.Accumulation`,
      `Lung.Drug.Accumulation`,
      `Kidney.Drug.Accumulation`
    ),
    names_to = "Location",
    values_to = "Drug_Accumulation"
  ) %>%
  mutate(
    Location = recode(
      Location,
      `Tumor%Drug Accumulation` = "Tumor",
      `Heart%Drug Accumulation` = "Heart",
      `Liver%Drug Accumulation` = "Liver",
      `Spleen%Drug Accumulation` = "Spleen",
      `Lung%Drug Accumulation` = "Lung",
      `Kidney%Drug Accumulation` = "Kidney"
    ),
    Core_Material = as.factor(Core_Material)
  ) %>%
  drop_na(Core_Material, Size, Zeta_Potential, Drug_Accumulation)

# Fit one regression model per location
model_results <- df_long %>%
  group_by(Location) %>%
  nest() %>%
  mutate(
    model = map(
      data,
      ~ lm(
        Drug_Accumulation ~ Size + Zeta_Potential + Core_Material + Tumor_Size..cm3.,
        data = .x
      )
    ),
    augmented = map(model, broom::augment),
    stats = map(model, broom::glance),
    tidy_out = map(model, broom::tidy)
  )

#Regression

plot_data <- model_results %>%
  select(Location, augmented) %>%
  unnest(augmented)

model_stats <- model_results %>%
  select(Location, stats) %>%
  unnest(stats) %>%
  mutate(label = paste0("R² = ", round(r.squared, 3)))

p1 <- ggplot(plot_data, aes(x = .fitted, y = Drug_Accumulation)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Location, scales = "free") +
  geom_text(
    data = model_stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.2,
    size = 4
  ) +
  labs(
    x = "Predicted % Drug Accumulation",
    y = "Observed % Drug Accumulation",
    title = "Observed vs Predicted Drug Accumulation",
    subtitle = "Models use Size, Zeta Potential, Core Material and Tumor Size"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold")
  )


#Coefficient Plot

coef_data <- model_results %>%
  select(Location, tidy_out) %>%
  unnest(tidy_out) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "Size" ~ "Size",
      term == "Zeta_Potential" ~ "Zeta Potential",
      term == "Tumor_Size..cm3." ~ "Tumor Size",
      str_detect(term, "Core_Material") ~ str_replace(term, "Core_Material", "Core Material: "),
      TRUE ~ term
    )
  )

p2 <- ggplot(coef_data, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = estimate - std.error * 1.96,
        xmax = estimate + std.error * 1.96),
    height = 0.2
  ) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    x = "Regression Coefficient Estimate",
    y = "Predictor",
    title = "Effect Sizes of Nanocarrier Properties on Drug Accumulation",
    subtitle = "Points show coefficient estimates; bars show 95% confidence intervals"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold")
  )


p1 / p2
