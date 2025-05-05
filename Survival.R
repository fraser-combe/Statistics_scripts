library(dplyr)
library(tidyr)

getwd()
#read_data
my_data <- read_csv("PATH/TO/DATA")

# Determine the range of years
years <- range(my_data$Year)
############################Filter to 2015
# Filter the dataset to only include years up to 2015
my_data <- my_data %>% filter(Year <= 2015)

my_data <- my_data %>% filter(Year <= 2020)
# Define explicit year range for the study to only go up to 2015
study_years <- 1999:2015

#########################
# Assuming my_data is already loaded and contains the Sex column
# Filtering data for males
data_males <- my_data %>% filter(Sex == "m")

# Filtering data for females
data_females <- my_data %>% filter(Sex == "f")

# If you converted 'Sex' to a factor of 0 and 1 earlier:
data_males <- my_data %>% filter(Sex == 1)
data_females <- my_data %>% filter(Sex == 0)

# Define explicit year range for the study
study_years <- 1999:2020

# Function to generate capture history
generate_capture_history <- function(data, study_years) {
  capture_history_wide <- data %>%
    group_by(`Ring No`, Year) %>%
    summarise(Captured = 1, .groups = "drop") %>%
    pivot_wider(names_from = Year, values_from = Captured, values_fill = list(Captured = 0))
  
  # Ensure columns for all study years are present
  for (year in study_years) {
    if (!as.character(year) %in% names(capture_history_wide)) {
      capture_history_wide[[as.character(year)]] <- 0
    }
  }
  
  # Order columns by year
  capture_history_wide <- capture_history_wide %>%
    select(`Ring No`, all_of(as.character(study_years)))
  
  return(capture_history_wide)
}

# Generate capture history with specified study year range for males and females
capture_history_males <- generate_capture_history(data_males, study_years)
capture_history_females <- generate_capture_history(data_females, study_years)

head(capture_history_males)

library(ggplot2)

# Assuming 'capture_history_males' is already defined
# For demonstration, let's use a subset to keep the plot readable
capture_history_subset <- head(capture_history_females, 100) # Adjust number of rows as needed

# Melting the data frame for use with ggplot
capture_history_melted <- reshape2::melt(capture_history_subset, id.vars = 'Ring No')

# Plotting
ggplot(capture_history_melted, aes(x = variable, y = `Ring No`, fill = factor(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"), name = "Captured", labels = c("No", "Yes")) +
  labs(x = "Year", y = "Ring No", title = "Capture History Matrix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = "bottom")



# Print the first few rows to check
head(capture_history_males)
head(capture_history_females)

getwd()
library(rjags)

# Assuming capture_history_males is already created and formatted correctly
# Convert capture history tibble to a matrix if it's not already
capture_matrix_females <- as.matrix(capture_history_females[,-1]) # Exclude the 'Ring No' column for the analysis

capture_matrix_males <- as.matrix(capture_history_males[,-1]) # Exclude the 'Ring No' column for the analysis

ncol(capture_matrix_females)

# Number of individuals and corrected number of transitions
# Assuming both male and female matrices have the same structure
N <- nrow(capture_matrix_males)  # Number of individuals
T_corrected <- ncol(capture_matrix_males) - 1  # Correct number of transitions, assuming one less than total columns

#females
# Number of individuals and corrected number of transitions
# Assuming both male and female matrices have the same structure
N <- nrow(capture_matrix_females)  # Number of individuals
T_corrected <- ncol(capture_matrix_females) - 1  # Correct number of transitions, assuming one less than total columns


# Print to verify
cat("Number of individuals (N):", N, "\n")
cat("Corrected number of transitions (T):", T_corrected, "\n")

# Initial survival probability for demonstration
initial_survival_prob_value <- 0.7

###Males
# Preparing data list for JAGS
data_list_for_jags <- list(
  observed = capture_matrix_males,  # Assuming you're analyzing male capture history
  N = nrow(capture_matrix_males),
  T = ncol(capture_matrix_males) - 1, # Adjust if your first column is not year data
  initial_survival_prob = 0.7  # Example value, adjust based on your assumptions
)
####Females
data_list_for_females <- list(
  observed = capture_matrix_females,
  N = nrow(capture_matrix_females),
  T = ncol(capture_matrix_females) - 1,
  initial_survival_prob = 0.6  # Example value
)


# Initial values function
init_values <- function() {
  list(
    mu_Phi = rnorm(1, 0, 0.1),
    tau_Phi = rgamma(1, 1, 0.1),
    p_global = rep(0.5, data_list_for_jags$T),
    individual_effect = rnorm(data_list_for_jags$N, 0, 0.1)
  )
}

# Initial values function
init_values <- function() {
  list(
    mu_Phi = rnorm(1, 0, 0.1),
    tau_Phi = rgamma(1, 1, 0.1),
    p_global = rep(0.5, data_list_for_females$T),
    individual_effect = rnorm(data_list_for_females$N, 0, 0.1)
  )
}

library(rjags)

# Model file

# Create JAGS model object
jags_model <- jags.model(file = "capture_model.jags",
                         data = data_list_for_females,
                         inits = init_values,
                         n.chains = 3,
                         n.adapt = 5000)

# Update model (burn-in)
update(jags_model, 500)

# Sample from the posterior
samples <- coda.samples(jags_model,
                        variable.names = c("mu_Phi", "tau_Phi", "p_global", "Phi"),
                        n.iter = 1000)

# Check convergence and summarize results
print(summary(samples))

# Assuming 'Phi' represents survival probabilities across all years
# Convert samples to mcmc.list for easier manipulation
library(coda)
Phi_samples_list <- as.mcmc.list(samples)


# Compute summary statistics for 'Phi' for males
summary_stats_male <- summary(Phi_samples_list)

# Extract mean survival probabilities and their 95% CIs for males
means_male <- summary_stats_male$statistics[,"Mean"]
lower_ci_male <- summary_stats_male$quantiles[,"2.5%"]
upper_ci_male <- summary_stats_male$quantiles[,"97.5%"]

# Adjusted way to print each year's data, skipping the first problematic value:
for(year in 2:length(means_male)) {  # Start from 2 to ignore the initial problematic survival estimate
  actual_year = 1999 + (T - 1)  # Adjust to correctly reflect the year intervals for survival probabilities
  cat(sprintf("Survival from Year %d to %d: Mean Phi = %.3f, 95%% CI = [%.3f, %.3f]\n",
              actual_year - 1, actual_year, means_male[year], lower_ci_male[year], upper_ci_male[year]))
}

# Compute summary statistics for 'Phi' for females
summary_stats_female <- summary(Phi_samples_list)

# Extract mean survival probabilities and their 95% CIs for females
means_female <- summary_stats_female$statistics[,"Mean"]
lower_ci_female <- summary_stats_female$quantiles[,"2.5%"]
upper_ci_female <- summary_stats_female$quantiles[,"97.5%"]

head(means_female)

# Example of trimming to the period of interest
period_of_interest <- 1999:2020
means_female <- means_female[1:length(period_of_interest)]
lower_ci_female <- lower_ci_female[1:length(period_of_interest)]
upper_ci_female <- upper_ci_female[1:length(period_of_interest)]

# Adjusted way to print each year's data, skipping the first problematic value:
for(year in 2:length(means_female)) {  # Start from 2 to ignore the initial problematic survival estimate
  actual_year = 1999 + (year - 1)  # Adjust to correctly reflect the year intervals for survival probabilities
  cat(sprintf("Survival from Year %d to %d: Mean Phi = %.3f, 95%% CI = [%.3f, %.3f]\n",
              actual_year - 1, actual_year, means_female[year], lower_ci_female[year], upper_ci_female[year]))
}

###################################
# Extract detection probabilities
p_global_summary <- summary(samples)$statistics[grep("p_global", rownames(summary(samples)$statistics)), ]
p_global_ci <- summary(samples)$quantiles[grep("p_global", rownames(summary(samples)$quantiles)), ]

# Combine into data frame
p_global_df <- data.frame(
  year = study_years[-1],  # p_global covers transitions, so skip year 1
  mean = p_global_summary[, "Mean"],
  lower = p_global_ci[, "2.5%"],
  upper = p_global_ci[, "97.5%"]
)

# Plot detection probabilities
ggplot(p_global_df, aes(x = year, y = mean)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Annual Detection Probabilities",
       x = "Year",
       y = "Detection Probability") +
  theme_minimal()

ggsave("detection_probability_plot.png", width = 8, height = 5, dpi = 300)
####################3


##################3 working here
# Correctly define the range of years for both males and females
years_male = 1999:2015
years_female = 1999:2020  # Extend this if female data goes until 2019

# Creating the data frame
male_survival <- data.frame(
  year = years_male,
  mean_survival = means_male,  # Replace with actual means for males
  lower_ci = lower_ci_male,  # Replace with actual lower CI for males
  upper_ci = upper_ci_male,  # Replace with actual upper CI for males
  sex = rep("Male", length(means_male))
)

# Check the resulting data frame
print(male_survival)
# Optionally, save to CSV
write.csv(male_survival, "male_survival.csv", row.names = FALSE)

female_survival <- data.frame(
  year = years_female,
  mean_survival = means_female,  # Assuming these variables are correctly sized for the years you have
  lower_ci = lower_ci_female,
  upper_ci = upper_ci_female,
  sex = rep("Female", length(means_female))  # Ensure this matches the length of your means vector
)

# Assuming means, lower_ci, and upper_ci are correctly extracted for the available years
# and that these vectors correctly represent survival probabilities for years you have model output for
samples <- coda.samples(model = jags_model, variable.names = c("Phi"), n.iter = 10000)

# Convert samples to a format that can be summarized
Phi_samples_list <- as.mcmc.list(samples)

# Compute summary statistics for 'Phi' across all years
summary_stats <- summary(Phi_samples_list)


# Ensure this vector's length matches your survival estimate vectors
if(length(actual_years_female) != length(means)) {
  stop("Year and survival estimate lengths do not match.")
}

# Create the data frame using the corrected years and survival estimate vectors
female_survival <- data.frame(
  year_transition = actual_years_female,
  mean_survival = means,  # Use the correct vector for means
  lower_ci = lower_ci,
  upper_ci = upper_ci,
  sex = rep("Female", length(means))
)

# Note: Replace 'means', 'lower_ci', and 'upper_ci' with the correct vectors for females if they were incorrectly referenced


# Check the resulting data frame
print(female_survival)
# Optionally, save to CSV
write.csv(female_survival, "female_survival.csv", row.names = FALSE)


combined_survival <- read.csv("/media/fraser/3d2cd45b-5ad4-417b-9eda-52b71c760f28/backup_home/Desktop/Other stuff/Lithuania_analysis/combined_survial.csv")
# Adjust the year to reflect survival over the winter
combined_survival$year <- paste(combined_survival$year, combined_survival$year + 1, sep="/")

# Adjust the dataset to remove the last year's label since it won't have a following year for comparison
# If your dataset includes the last winter period, adjust accordingly
combined_survival <- combined_survival[-nrow(combined_survival), ]

# Plotting
ggplot(combined_survival, aes(x = year, y = mean_survival, group = sex, color = sex)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +  # Setting y-axis from 0 to 1
  labs(title = "Annual Survival Probabilities by Sex Over Winter",
       x = "Winter Period",
       y = "Survival Probability") +
  theme_minimal() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




# Create data frames for each gender
male_survival <- data.frame(
  year = 2000:2015,
  mean_survival = means,
  lower_ci = lower_ci,
  upper_ci = upper_ci,
  sex = "Male"
)

female_survival <- data.frame(
  year = 1999:2015,
  mean_survival = means_female,
  lower_ci = lower_ci_female,
  upper_ci = upper_ci_female,
  sex = "Female"
)

# Combine into a single data frame
combined_survival <- rbind(male_survival, female_survival)


# Assuming 'means', 'lower_ci', and 'upper_ci' are of the same length and correspond to these years
# Plotting
plot(years, means, type="o", pch=20, xlab="Year", ylab="Mean Survival Probability",
     ylim=range(c(lower_ci, upper_ci, means)), col="blue", main="Annual Survival Probabilities with 95% CI")

# Adding error bars for 95% CI
for(i in 1:length(years)) {
  segments(years[i], lower_ci[i], years[i], upper_ci[i], col="red")
  points(years[i], means[i], pch=20, col="blue")  # Reinforce point color over segments if needed
}

# Enhancing plot
grid()
legend("topright", legend=c("Mean Survival", "95% CI"), col=c("blue", "red"), pch=c(20, NA), lty=c(NA,1))


###########################33
library(ggplot2)

# Example data frames for male and female survival probabilities
# Assume 'year', 'mean_survival', 'lower_ci', 'upper_ci' are columns in these data frames
male_survival <- data.frame(year = 1999:2019, mean_survival = runif(21, 0.7, 0.9), lower_ci = runif(21, 0.6, 0.8), upper_ci = runif(21, 0.8, 1))
female_survival <- data.frame(year = 1999:2019, mean_survival = runif(21, 0.7, 0.9), lower_ci = runif(21, 0.6, 0.8), upper_ci = runif(21, 0.8, 1))

# Melt the data frames for plotting with ggplot2
male_survival$sex <- 'Male'
female_survival$sex <- 'Female'
combined_survival <- rbind(male_survival, female_survival)

# Plot
# Plot
ggplot(combined_survival, aes(x = year, y = mean_survival, color = sex)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = sex), alpha = 0.2) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +  # Line colors
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +  # CI fill colors
  labs(title = "Annual Survival Probabilities by Sex", x = "Year", y = "Survival Probability") +
  theme_minimal() +
  theme(legend.title = element_blank())

library(ggplot2)

# Create a small offset for year for males and females
combined_survival$year_offset <- ifelse(combined_survival$sex == "Male", 
                                        combined_survival$year - 0.1, 
                                        combined_survival$year + 0.1)

# Adjust your ggplot2 code to use 'year_offset' for x-axis
ggplot(combined_survival, aes(x = year_offset, y = mean_survival, color = sex, group = sex)) +
  geom_point() +  # Add points for mean survival
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +  # Error bars for 95% CI
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +  # Adjust fill colors for visibility
  labs(title = "Annual Survival Probabilities by Sex", x = "Year", y = "Survival Probability") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = combined_survival$year)  # Ensure x-axis labels match original years

# Note: Adjust 'scale_x_continuous' if necessary to refine x-axis labels

library(ggplot2)
# Assuming combined_survival is already prepared and contains 'sex', 'year', 'mean_survival', 'lower_ci', and 'upper_ci'

# Plot for males with adjusted y-axis
male_plot <- ggplot(subset(combined_survival, sex == "Male"), aes(x = year, y = mean_survival)) +
  geom_point(color = "blue") +
  geom_line(color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +
  scale_y_continuous(limits = c(0, 1.0)) +  # Adjust y-axis
  labs(title = "Male Annual Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal()

# Plot for females with adjusted y-axis
female_plot <- ggplot(subset(combined_survival, sex == "Female"), aes(x = year, y = mean_survival)) +
  geom_point(color = "red") +
  geom_line(color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  scale_y_continuous(limits = c(0, 1.0)) +  # Adjust y-axis
  labs(title = "Female Annual Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal()

# Display plots
print(male_plot)
print(female_plot)

# Using patchwork to combine plots
library(patchwork)
(male_plot + female_plot) & scale_y_continuous(limits = c(0, 1.0))

# Or using gridExtra
library(gridExtra)
grid.arrange(male_plot, female_plot, ncol = 2)




#################################


survival_data <- read_csv("combined_survial.csv")
combined_survival <- read.csv("/media/fraser/3d2cd45b-5ad4-417b-9eda-52b71c760f28/backup_home/Desktop/Other stuff/Lithuania_analysis/combined_survial.csv")

survival_data <- read_csv("combined_survial_year.csv")


# Splitting the data into two subsets
data_males <- subset(survival_data, sex == "Male")
data_females <- subset(survival_data, sex == "Female")

# Plot for Males
plot_males <- ggplot(data_males, aes(x = year, y = mean_survival)) +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +
  labs(title = "Male Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal()

# Plot for Females
plot_females <- ggplot(data_females, aes(x = year, y = mean_survival)) +
  geom_line(color = "red") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  labs(title = "Female Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal()

# Displaying plots side by side
library(gridExtra)
grid.arrange(plot_females, plot_males, ncol = 2)

library(ggplot2)
library(gridExtra)  # For arranging the plots side by side

# Convert 'year' from character to factor to ensure proper order in the plot
data_males$year <- factor(data_males$year, levels = data_males$year)
data_females$year <- factor(data_females$year, levels = data_females$year)

# Plot for Males
# Plot for Males with thicker line
plot_males <- ggplot(data_males, aes(x = year, y = mean_survival, group = 1)) +
  geom_line(color = "blue", size = 1.0) +  # Adjusted line thickness
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +
  labs(title = "Male Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for Females with thicker line
plot_females <- ggplot(data_females, aes(x = year, y = mean_survival, group = 1)) +
  geom_line(color = "red", size = 1.0) +  # Adjusted line thickness
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  labs(title = "Female Survival Probabilities", x = "Year", y = "Survival Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displaying plots side by side
grid.arrange(plot_females,plot_males, ncol = 2)

library(ggplot2)
library(dplyr)

# Assuming data_males and data_females are already defined as shown previously
# First, we need to merge the two datasets into one
# Assuming 'combined_data' is your dataframe
combined_data <- combined_data %>%
  mutate(year_numeric = as.numeric(sub("/.*", "", year)))  # Convert "1999/2000" to 1999 as numeric

combined_data <-survival_data
# Adjusted plot code
ggplot(combined_data, aes(x = year_numeric, y = mean_survival, group = sex, color = sex)) +
  geom_line(size = 1) +  # Thicker line
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_wrap(~sex, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(title = "Survival Probabilities by Sex",
       x = "Year",
       y = "Survival Probability") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust for better x-axis label readability
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# This assumes you've transformed 'year' to a numeric 'year_numeric' for better plotting.

# Use the original 'year' variable and adjust the plot accordingly
ggplot(combined_data, aes(x = year, y = mean_survival, group = sex, color = sex)) +
  geom_line(size = 1) +  # Thicker line
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_wrap(~sex, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(title = "",
       x = "Year",
       y = "Survival Probability (Phi)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate for readability
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_x_discrete(guide = guide_axis(n.dodge=1))  # Adjust to prevent label overlap

# This code assumes 'combined_data' is your dataframe and 'year' is treated as a categorical variable.

# Use the original 'year' variable and adjust the plot accordingly
# Define dodge position
dodge <- position_dodge(width = 0.2)  # Adjust 'width' to control the dodge amount

ggplot(combined_data, aes(x = year, y = mean_survival, group = sex, color = sex)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = dodge, alpha = 0.5) +  # Apply dodge to error bars
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(title = "",
       x = "Year" ,
       y = "Survival Probability (Phi)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate for readability
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_x_discrete(guide = guide_axis(n.dodge=1))  # Adjust to prevent label overlap




#########################33 More plioting survival estimates

library(ggplot2)
library(dplyr)

# Ensure 'sex' is a factor
combined_survival$sex <- factor(combined_survival$sex, levels = c("Male", "Female"))

# Create dodge position for offsetting overlapping points and error bars
dodge <- position_dodge(width = 0.5)

# Plot
# Ensure 'sex' is a factor
combined_survival$sex <- factor(combined_survival$sex, levels = c("Male", "Female"))

# Create dodge position
dodge <- position_dodge(width = 0.5)

# Plot
# Ensure 'sex' is a factor
combined_survival$sex <- factor(combined_survival$sex, levels = c("Male", "Female"))

# Create dodge position
dodge <- position_dodge(width = 0.5)

# Plot
ggplot(combined_survival, aes(x = year, y = mean_survival, color = sex, group = sex)) +
  geom_point(position = dodge, size = 2) +
  geom_line(aes(linetype = sex), position = dodge, size = 0.75) +  # Only lines use linetype
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.75, position = dodge) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_linetype_manual(values = c("Male" = "solid", "Female" = "dashed")) +
  labs(
    x = "Year",
    y = "Mean Survival Probability (Phi)",
    color = "Sex",
    linetype = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Save the plot as a high-resolution PNG
ggsave(
  filename = "survival_plot.png",
  plot = last_plot(),         # or replace with your plot object if assigned (e.g., p)
  width = 8,                  # width in inches
  height = 6,                 # height in inches
  dpi = 600                   # dots per inch for high-quality print
)

# Or save as TIFF (journal-friendly format)
# Save as high-resolution PNG with white background
ggsave(
  filename = "survival_plot.png",
  plot = last_plot(),        # or your plot object (e.g., p)
  width = 9,
  height = 7,
  dpi = 800,
  bg = "white"               # this sets the background to white
)

# Save as high-resolution TIFF with white background
ggsave(
  filename = "survival_plot.tiff",
  plot = last_plot(),
  width = 9,
  height = 9,
  dpi = 800,
  compression = "lzw",
  bg = "white"
)

