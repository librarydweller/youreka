#filtering data process
library(haven)
library(tidyverse)

dataset <- read.csv("/Users/amy/Downloads/youreka_dataset.csv")

#obtaining columns
dataset_clean <- dataset[, c("Core.material",
                             "Size",
                             "Zeta.Potential",
                             "Tumor.Size",
                             "Biodistribution...ID.",
                             "X", "X.1", "X.2", "X.3", "X.4")]

#renaming columns
colnames(dataset_clean) <- c("Core_Material",
                             "Size",
                             "Zeta_Potential",
                             "Tumor_Size",
                             "Tumor",
                             "Heart",
                             "Liver",
                             "Spleen",
                             "Lung",
                             "Kidney")

#replaced NA with blanks for better viewing
View(replace(dataset_clean, is.na(dataset_clean), ""))
write_csv(dataset_clean, "clean_youreka_set.csv", na = "")