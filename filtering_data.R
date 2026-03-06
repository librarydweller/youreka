library(haven)
library(tidyverse)

dataset <- read.csv("/Users/amy/Downloads/youreka_dataset.csv")

dataset_clean <- dataset[, c("Core.material",
                             "Size",
                             "Zeta.Potential",
                             "Tumor.Size",
                             "Biodistribution...ID.",
                             "X",
                             "X.1",
                             "X.2",
                             "X.3",
                             "X.4")]



head(dataset_clean, 10)
