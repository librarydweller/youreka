library(tidyverse)
library(ggplot2)
library(ggcorrplot)

data <- read.csv("filtered_youreka_set.csv")

# Clean Tumor_Size to numeric (extract number before "cm3")
data <- data |>
  mutate(Tumor_Size_numeric = as.numeric(
                                         str_extract(Tumor_Size, "\\d+\\.?\\d*")))

# Select numerical columns for correlation
numerical_data <- data |>
  select(Size, Zeta_Potential, Tumor_Size_numeric, Tumor,
         Heart, Liver, Spleen, Lung, Kidney)

# Compute correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Plot correlation matrix
p_corr <- ggcorrplot(cor_matrix,
                     method = "circle",
                     type = "lower",
                     lab = TRUE,
                     title = "Correlation Matrix of 
                     Nanoparticle Properties and Biodistribution")

# 1. Plot comparing core material vs drug accumulation
p1 <- ggplot(data, aes(x = Core_Material, y = Tumor)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Core Material vs Drug Accumulation (Tumor)",
    x = "Core Material",
    y = "Drug Accumulation"
  ) +
  theme_minimal()

# 2. Plot comparing zeta potential vs drug accumulation
p2 <- ggplot(data, aes(x = Zeta_Potential, y = Tumor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Zeta Potential vs Drug Accumulation (Tumor)",
    x = "Zeta Potential",
    y = "Drug Accumulation"
  ) +
  theme_minimal()

# 3. Plot comparing nanoparticle size vs drug accumulation
p3 <- ggplot(data, aes(x = Size, y = Tumor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Nanoparticle Size vs Drug Accumulation (Tumor)",
    x = "Nanoparticle Size (nm)",
    y = "Drug Accumulation"
  ) +
  theme_minimal()

pdf("plots_and_correlation.pdf")
print(p1)
print(p2)
print(p3)
print(p_corr)

dev.off()