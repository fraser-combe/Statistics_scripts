# eda_stats_analysis.R
# Author: Fraser Combe
# Description: Example EDA and statistical test pipeline with normality checks

# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(readxl)

# Load dataset


# Open file dialog to select an Excel file
file_path <- file.choose()

# Read the first sheet of the selected Excel file
data <- read_excel(file_path)
#or use dummy data
data <- mtcars

data$am <- factor(data$am, labels = c("Automatic", "Manual"))
data$cyl <- factor(data$cyl)

# --- Exploratory Data Analysis ---

str(data)
summary(data)

# Correlation matrix
cor_matrix <- round(cor(data), 2)
print(cor_matrix)

# Boxplot of mpg by transmission
ggplot(data, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "MPG by Transmission Type", x = "Transmission", y = "Miles per Gallon")

# --- Normality Test (Shapiro-Wilk) ---

# Overall normality test on darta
shapiro_all <- shapiro.test(data$mpg)
cat("Shapiro-Wilk test for mpg (entire dataset):\n")
print(shapiro_all)

# Normality by transmission group
shapiro_auto <- shapiro.test(data$mpg[data$am == "Automatic"])
shapiro_manual <- shapiro.test(data$mpg[data$am == "Manual"])

cat("\nShapiro-Wilk test for Automatic group:\n")
print(shapiro_auto)
cat("\nShapiro-Wilk test for Manual group:\n")
print(shapiro_manual)

# --- Choose t-test or Wilcoxon based on normality ---

normal_auto <- shapiro_auto$p.value > 0.05
normal_manual <- shapiro_manual$p.value > 0.05

if (normal_auto && normal_manual) {
  cat("\nData appears normal. Performing parametric t-test:\n")
  test_result <- t.test(mpg ~ am, data = data)
} else {
  cat("\nData not normal. Performing non-parametric Wilcoxon test:\n")
  test_result <- wilcox.test(mpg ~ am, data = data)
}
print(test_result)

# --- ANOVA or Kruskal-Wallis for mpg ~ cyl ---

# Check normality by cylinder group
group_normality <- data %>%
  group_by(cyl) %>%
  summarise(p = shapiro.test(mpg)$p.value)

cat("\nShapiro-Wilk p-values by cylinder group:\n")
print(group_normality)

if (all(group_normality$p > 0.05)) {
  cat("\nAll groups normal. Performing ANOVA:\n")
  anova_model <- aov(mpg ~ cyl, data = data)
  print(summary(anova_model))
  
  if (summary(anova_model)[[1]][["Pr(>F)"]][1] < 0.05) {
    cat("\nPost-hoc Tukey test:\n")
    print(TukeyHSD(anova_model))
  }
  
} else {
  cat("\nAt least one group not normal. Performing Kruskal-Wallis test:\n")
  kruskal <- kruskal.test(mpg ~ cyl, data = data)
  print(kruskal)
}

# --- Save outputs ---

ggsave("boxplot_mpg_transmission.png")
write.csv(cor_matrix, "correlation_matrix.csv")

sink("statistical_results.txt")
cat("Shapiro-Wilk Results:\n")
print(shapiro_all)
print(shapiro_auto)
print(shapiro_manual)
cat("\n\nGroup Normality (cyl):\n")
print(group_normality)
cat("\n\nGroup Comparison Result (Transmission):\n")
print(test_result)
cat("\n\nGroup Comparison Result (Cylinders):\n")
if (exists("anova_model")) print(summary(anova_model))
if (exists("kruskal")) print(kruskal)
sink()
