#Load Library
library(tidyverse)

df <- read.csv("filtered_youreka_set.csv")

#Zeta Potential

## Convert to long format
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
    )
  )

## Bin Zeta Potential into 10 mV bins
bin_width_zeta <- 10

df_binned <- df_long %>%
  mutate(
    Zeta_bin_mid = floor(Zeta_Potential / bin_width_zeta) * bin_width_zeta + (bin_width_zeta / 2)
  ) %>%
  group_by(Location, Zeta_bin_mid) %>%
  summarise(
    Mean_Drug_Accumulation = mean(Drug_Accumulation, na.rm = TRUE),
    .groups = "drop"
  )

## Bar plot with numeric x-axis
ggplot(df_binned, aes(x = Zeta_bin_mid, y = Mean_Drug_Accumulation)) +
  geom_col(width = 8) +
  facet_wrap(~Location, scales = "free_y") +
  labs(
    x = "Zeta Potential (mV, bin midpoint)",
    y = "% Drug Accumulation",
    title = "Drug Accumulation vs Zeta Potential (10 mV bins)"
  ) +
  scale_x_continuous(breaks = seq(
    floor(min(df_binned$Zeta_bin_mid)),
    ceiling(max(df_binned$Zeta_bin_mid)),
    by = 10
  )) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Core Material
## Summarize by Core Material and Location
df_core <- df_long %>%
  filter(!is.na(`Core_Material`)) %>%
  group_by(Location, `Core_Material`) %>%
  summarise(
    Mean_Drug_Accumulation = mean(Drug_Accumulation, na.rm = TRUE),
    .groups = "drop"
  )

## Plot
ggplot(df_core, aes(x = `Core_Material`, y = Mean_Drug_Accumulation)) +
  geom_col() +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    x = "Core Material",
    y = "% Drug Accumulation",
    title = "Drug Accumulation by Core Material"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Size
## Bin Size into 25 nm bins
bin_width_zeta_size <- 25

df_size <- df_long %>%
  filter(!is.na(Size)) %>%
  mutate(
    Size_bin_mid = floor(Size / bin_width_zeta_size) * bin_width_zeta_size + (bin_width_zeta_size / 2)
  ) %>%
  group_by(Location, Size_bin_mid) %>%
  summarise(
    Mean_Drug_Accumulation = mean(Drug_Accumulation, na.rm = TRUE),
    .groups = "drop"
  )

## Plot
ggplot(df_size, aes(x = Size_bin_mid, y = Mean_Drug_Accumulation)) +
  geom_col(width = 20) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    x = "Particle Size (nm, bin midpoint)",
    y = "% Drug Accumulation",
    title = "Drug Accumulation by Particle Size (25 nm bins)"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(df_size$Size_bin_mid, na.rm = TRUE)),
      ceiling(max(df_size$Size_bin_mid, na.rm = TRUE)),
      by = 25
    )
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Tumor Size
# Bin tumor size
bin_width <- 0.5

df_tumor_size <- df_long %>%
  filter(!is.na(`Tumor_Size..cm3.`)) %>%
  mutate(
    Tumor_Size_bin_mid = floor(`Tumor_Size..cm3.` / bin_width) * bin_width + (bin_width / 2)
  ) %>%
  group_by(Location, Tumor_Size_bin_mid) %>%
  summarise(
    Mean_Drug_Accumulation = mean(Drug_Accumulation, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(df_tumor_size, aes(x = Tumor_Size_bin_mid, y = Mean_Drug_Accumulation)) +
  geom_col(width = 0.4) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    x = "Tumor Size (cm³, bin midpoint)",
    y = "% Drug Accumulation",
    title = "Drug Accumulation by Tumor Size"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(df_tumor_size$Tumor_Size_bin_mid, na.rm = TRUE)),
      ceiling(max(df_tumor_size$Tumor_Size_bin_mid, na.rm = TRUE)),
      by = 0.5
    )
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
