# Load necessary libraries
library(readxl)
library(lme4)
library(ggplot2)
library(dplyr)

# Define the file path
file_path <- "C:/Users/Admin/Desktop/Analysis/PERIODICITY ANALYSIS/MESODERM TISSUE COMPARISON.xlsx"

# Read data from Excel file
data <- read_excel(file_path)

# Ensure embryo and stage are treated as factors
data$embryo <- factor(data$embryo)
data$STAGE <- factor(data$STAGE)

# Remove empty columns
data <- data[, colSums(is.na(data)) < nrow(data)]

# Remove rows with missing values
data <- data[complete.cases(data$STAGE), ]

# Filter data for stages Mesoderm E6.5 and Mesoderm E7.25
data_filtered <- data %>%
  filter(STAGE %in% c("Mesoderm E6.5", "Mesoderm E7.25"))

# Calculate mean and standard deviation for each stage
summary_data <- data_filtered %>%
  group_by(STAGE) %>%
  summarize(
    mean_periodicity = mean(periodicity),
    sd_periodicity = sd(periodicity)
  ) %>%
  ungroup()

# Perform t-test between Mesoderm E6.5 and Mesoderm E7.25
t_test_result <- t.test(data_filtered$periodicity ~ data_filtered$STAGE)
print(t_test_result)

# Define a color palette for stages
stage_colors <- c("Mesoderm E6.5" = "blue", "Mesoderm E7.25" = "red")

# Boxplot of mean periodicity by stage without whiskers
p <- ggplot(data_filtered, aes(x = STAGE, y = periodicity, fill = STAGE)) +
  geom_boxplot(alpha = 0.4, width = 0.6) +  # Width of the boxplot
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_errorbar(data = summary_data, aes(y = mean_periodicity, ymin = mean_periodicity, ymax = mean_periodicity), width = 0, position = position_dodge(0.8)) +  # Whiskers with width 0
  geom_point(data = summary_data, aes(y = mean_periodicity), position = position_dodge(0.8), size = 3, color = "transparent") +
  scale_fill_manual(values = stage_colors) +
  labs(title = "Mean Periodicity Comparison between Mesoderm E6.5 and Mesoderm E7.25",
       x = "Developmental Stage",
       y = "Periodicity of Calcium Oscillation (min)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        axis.line = element_line(colour = "black"),  # Add black lines for axes
        axis.text.x = element_text(face = "bold"),  # Bold x-axis labels
        legend.position = "none")  # Remove legend

# Add significance annotation if there is a significant difference
if (t_test_result$p.value < 0.05) {
  p <- p + annotate("text", x = 1.5, y = max(data_filtered$periodicity) + 0.5, label = "*", size = 6, color = "red")
}

# Print the plot
print(p)

# Print the summary data
print(summary_data)
