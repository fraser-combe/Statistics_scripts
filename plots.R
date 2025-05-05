library(tidyverse)

# Read the data
data <- read_csv("example_data.csv")

# Boxplot: Score by Group
ggplot(data, aes(x = Group, y = Score, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Score by Group")

# Barplot: Category counts
ggplot(data, aes(x = Category, fill = Category)) +
  geom_bar() +
  labs(title = "Barplot of Category Counts")

# Histogram: Measurement
ggplot(data, aes(x = Measurement)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Measurements")

# Scatterplot: Score vs Measurement, colored by Group
ggplot(data, aes(x = Measurement, y = Score, color = Group)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot of Score vs Measurement by Group")

# Load required libraries
library(tidyverse)
library(ggpubr)
library(reshape2)
library(corrplot)

# --- Generate Example Data (can replace with read_csv) ---
data <- tibble(
  ID = 1:30,
  Group = rep(c("A", "B", "C"), each = 10),
  Score = c(rnorm(10, mean = 75, sd = 5),
            rnorm(10, mean = 85, sd = 4),
            rnorm(10, mean = 70, sd = 6)),
  Measurement = c(rnorm(10, 5.5, 0.2),
                  rnorm(10, 6.0, 0.3),
                  rnorm(10, 5.0, 0.2)),
  Timepoint = rep(1:10, 3)
)

# --- Boxplot ---
p1 <- ggplot(data, aes(x = Group, y = Score, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot: Score by Group") +
  theme_minimal()

# --- Barplot (mean Score by Group) ---
p2 <- data %>%
  group_by(Group) %>%
  summarise(mean_score = mean(Score)) %>%
  ggplot(aes(x = Group, y = mean_score, fill = Group)) +
  geom_col() +
  labs(title = "Barplot: Mean Score by Group") +
  theme_minimal()

# --- Histogram ---
p3 <- ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 3, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Scores") +
  theme_minimal()

# --- Scatterplot ---
p4 <- ggplot(data, aes(x = Measurement, y = Score, color = Group)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot: Score vs Measurement") +
  theme_minimal()

# --- Line Plot (Score over Timepoint for one Group) ---
p5 <- data %>%
  filter(Group == "A") %>%
  ggplot(aes(x = Timepoint, y = Score)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(title = "Line Plot: Group A Score Over Time") +
  theme_minimal()

# --- Correlation Heatmap ---
numeric_data <- select(data, Score, Measurement, Timepoint)
cor_matrix <- cor(numeric_data)
png("correlation_heatmap.png", width = 600, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")
dev.off()

# --- Save all other plots ---
ggsave("boxplot_score_group.png", plot = p1)
ggsave("barplot_mean_score.png", plot = p2)
ggsave("histogram_score.png", plot = p3)
ggsave("scatterplot_score_vs_measurement.png", plot = p4)
ggsave("lineplot_groupA_score_time.png", plot = p5)
