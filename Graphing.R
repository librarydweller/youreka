library(tidyverse)
library(ggplot2)

# Load the cleaned data
data <- read.csv("clean_youreka_set.csv")

# Fill down the nanoparticle properties, filter NAs, and extract size
data_clean <- data |>
  fill(Core_Material, Size, Zeta_Potential, Tumor_Size, .direction = "down") |>
  filter(!is.na(Tumor)) |>
  mutate(Size_numeric = as.numeric(str_extract(Size, "(?<=: )\\d+\\.?\\d*")))




# 1. Plot comparing core material vs drug accumulation (Tumor)
p1 <- ggplot(data_clean, aes(x = Core_Material, y = Tumor)) +
  geom_boxplot() +
  labs(title = "Core Material vs Drug Accumulation (Tumor)",
       x = "Core Material",
       y = "Drug Accumulation") +
  theme_minimal()

# 2. Plot comparing zeta potential vs drug accumulation
p2 <- ggplot(data_clean, aes(x = Zeta_Potential, y = Tumor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Zeta Potential vs Drug Accumulation (Tumor)",
       x = "Zeta Potential",
       y = "Drug Accumulation (Tumor)") +
  theme_minimal()

# 3. Plot comparing nanoparticle size vs drug accumulation
p3 <- ggplot(data_clean, aes(x = Size_numeric, y = Tumor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Nanoparticle Size vs Drug Accumulation (Tumor)",
       x = "Nanoparticle Size (nm)",
       y = "Drug Accumulation (Tumor)") +
  theme_minimal()

# For Fun Histogram (bell curve approximation)
p4 <- ggplot(data_clean, aes(x = Tumor)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Drug Accumulation (Tumor)",
       x = "Drug Accumulation",
       y = "Frequency") +
  theme_minimal()

# Save all plots to a single PDF file
pdf("all_plots.pdf")

print(p1)
print(p2)
print(p3)
print(p4)

dev.off()
