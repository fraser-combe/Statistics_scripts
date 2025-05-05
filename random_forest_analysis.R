# random_forest_analysis.R
# Author: Fraser Combe
# Description: Example script to run a Random Forest classifier in R

# Load required libraries
library(tidyverse)
library(randomForest)
library(caret)

# Load example dataset (you can replace this with your own CSV)
data <- iris
data$Species <- as.factor(data$Species)

# --- Split into train/test sets (80/20) ---
set.seed(123)
train_index <- createDataPartition(data$Species, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# --- Fit Random Forest model ---
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 500, importance = TRUE)
print(rf_model)

# --- Predict on test set ---
predictions <- predict(rf_model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$Species)
print(conf_matrix)

# --- Feature importance ---
importance_values <- importance(rf_model)
print(importance_values)

# Plot variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# --- Save results ---
write.csv(importance_values, "rf_feature_importance.csv")
ggsave("rf_variable_importance.png", plot = last_plot())

sink("rf_results.txt")
cat("Random Forest Summary:\n")
print(rf_model)
cat("\nConfusion Matrix:\n")
print(conf_matrix)
sink()
