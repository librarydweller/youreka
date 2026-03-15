# Filter the Youreka dataset
library(tidyverse)

# Load the cleaned data
data <- read.csv("clean_youreka_set.csv", na.strings = character(0))

# Fill down the nanoparticle properties
data_filled <- data |>
  fill(Core_Material, Size, Zeta_Potential, Tumor_Size, .direction = "down")

# Convert Time_Point to numeric (in case it's character)
data_filled <- data_filled |>
  mutate(
    Time_Point = as.numeric(Time_Point),
    Size = as.numeric(str_extract(Size, "\\d+\\.?\\d*")),
    Tumor_Size = str_extract(Tumor_Size, "\\d+\\.?\\d* cm3")
  )

# Define the core materials to keep
keep_materials <- c(
  "Gold", "Iron Oxide", "Silica", "Copper", "Cu",
  "Polymeric", "Polymerics", "Liposome", "Liposomes",
  "Hydrogel", "Manganese", "Dendrimer", "Dendrimers"
)

# Filter to keep only specified core materials, excluding Iron Oxide Polymerics
data_filtered <- data_filled |>
  filter(
    Core_Material %in% keep_materials &
      !str_detect(Core_Material, "Iron Oxide Polymeric")
  )

data_final <- data_filtered |>
  group_by(Core_Material, Size, Zeta_Potential, Tumor_Size) |>
  filter(Time_Point == max(Time_Point, na.rm = TRUE)) |>
  ungroup() |>
  filter(!if_any(everything(), ~ . == "NA"))

# Save the filtered data to a new CSV
write.csv(data_final, "filtered_youreka_set.csv", row.names = FALSE)

# Optional: View the result
print("Filtered data saved to filtered_youreka_set.csv")
print(head(data_final))