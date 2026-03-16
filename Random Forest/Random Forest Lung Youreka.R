#Load Libraries
library(randomForest)
library(caret)
library(ranger)
library(pROC)
library(boot)
library(ggplot2)
library(phyloseq)
library(tidyverse)
library (broom)
library (patchwork)
library(tidymodels)
library(vip)
library(dplyr)
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


# Create binary outcome within each organ
df_long <- df_long %>%
  group_by(Location) %>%
  mutate(
    Accumulation_Class = if_else(
      Drug_Accumulation > median(Drug_Accumulation, na.rm = TRUE),
      "High",
      "Low"
    )
  ) %>%
  ungroup() %>%
  mutate(
    Accumulation_Class = factor(Accumulation_Class, levels = c("Low", "High"))
  )

#Lung Data
df_lung<-df_long %>%
  filter(Location == "Lung.Drug.Accumulation")
df_lung<-select(df_lung,-(5:6))

predictors = df_lung %>% select(-Accumulation_Class)

outcome = df_lung %>% pull(Accumulation_Class) %>% 
  factor(levels = c("High","Low"))

# Randomly subsets the rows into k equal bins.
k = 10
set.seed(127)
folds = createFolds(outcome, k = k, list = TRUE)

# Each of these folds will take a turn being the test dataset.
str(folds)
#Hyperparameter Tuning
# mtry: number of variables that will be used per forest. 
#       High = overfitting, low = uninformative

# splitrule: affects how decision trees are calculated.
#            Use gini or extratrees for boolean outcomes (ex. subject)
#            Use variance for continuous outcomes (ex. age)

# min.node.size: Related to tree complexity. Larger = simpler tree.
# Often best as a proportional fraction of your sample size.

# These are generic values. Depending on your dataset, you may need to adjust the numeric ranges up or down.
tune_grid = expand.grid(mtry = c(1,2,3,4), 
                        splitrule = c("gini","extratrees"),
                        min.node.size = c(25,49,99))

#Run RF
source('randomforest_functions.R')
pd_model = run_rf(X = predictors, y = outcome, 
                  fold_list = folds,
                  hyper = tune_grid, 
                  rngseed = 127)
names(pd_model)

#Interpretation

roc_test = roc(pd_model$test_labels$true_labels,
               pd_model$test_labels$predicted_probabilities)
roc_train = roc(pd_model$train_labels$true_labels,
                pd_model$train_labels$predicted_probabilities)
ggplot() +
  # Training data: this is a type of control
  geom_line(aes(x = 1 - roc_train$specificities, 
                y = roc_train$sensitivities), 
            color = "red",size=1) +
  # Test data: tells us the strength of the prediction
  geom_line(aes(x = 1 - roc_test$specificities,
                y = roc_test$sensitivities), 
            color = "black",size=1) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed",size=1) +
  labs(x = "False Positive Rate", y = "True Positive Rate") +
  annotate("text", x = 0.7, y = 0.2, 
           label = sprintf("Train (red): %.2f (%.2f-%.2f)\nTest (black): %.2f (%.2f-%.2f)",
                           auc(roc_train), pd_model$auc_train_ci[1], pd_model$auc_train_ci[2],
                           auc(roc_test), pd_model$auc_test_ci[1], pd_model$auc_test_ci[2]), 
           size = 6) +
  theme_minimal(base_size=18)

pd_model$importance

pd_model$importance %>% 
  # Data are automatically arranged by decreasing importance - turn it into a factor.
  # Otherwise the features will show up alphabetically in the plot.
  mutate(Feature = factor(.$Feature,levels = .$Feature)) %>% 
  ggplot(aes(Feature,MeanDecreaseGini,fill=MeanDecreaseGini)) +
  geom_col() +
  theme_classic(base_size=18) +
  theme(axis.text.x = element_text(angle=70, vjust = 1, hjust=1)) +
  ylab('Importance (Gini)') + xlab(NULL)

