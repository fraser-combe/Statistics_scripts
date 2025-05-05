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
