# Load necessary libraries
library(readxl)
library(lme4)
library(ggplot2)
library(dplyr)

# Define the file path
file_path <- "C:/Users/Admin/Desktop/Analysis/PERIODICITY ANALYSIS/MESODERM E6.5 - E7.25 COMPARISON.xlsx"

# Read data from Excel file
data <- read_excel(file_path)

# Ensure embryo and stage are treated as factors
data$embryo <- factor(data$embryo)
data$STAGE <- factor(data$STAGE)

# Remove empty columns
data <- data[, colSums(is.na(data)) < nrow(data)]

# Remove rows with missing values
data <- data[complete.cases(data$STAGE), ]

# Calculate the mean periodicity for each cell
mean_data <- data %>%
  group_by(cell, embryo) %>%
  summarize(mean_periodicity = mean(periodicity, na.rm = TRUE), .groups = 'drop')

# Filter data for stages Mesoderm E6.5 and Mesoderm E7.25
data_filtered <- data %>%
  filter(STAGE %in% c("Mesoderm E6.5", "Mesoderm E7.25"))

# Merge filtered data with mean_data to include stage information
data_filtered <- merge(data_filtered, mean_data, by = c("cell", "embryo"))

# Calculate mean and standard deviation for each stage
summary_data <- data_filtered %>%
  group_by(STAGE) %>%
  summarize(
    mean_periodicity = mean(mean_periodicity),
    sd_periodicity = sd(mean_periodicity)
  ) %>%
  ungroup()

# Perform t-test between Mesoderm E6.5 and Mesoderm E7.25
t_test_result <- t.test(mean_periodicity ~ STAGE, data = data_filtered)
print(t_test_result)

# Define a color palette for stages
stage_colors <- c("Mesoderm E6.5" = "blue", "Mesoderm E7.25" = "red")

# Boxplot of mean periodicity by stage without whiskers
p <- ggplot(data_filtered, aes(x = STAGE, y = mean_periodicity, fill = STAGE)) +
  geom_boxplot(alpha = 0.4, width = 0.6) +  # Width of the boxplot
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_errorbar(data = summary_data, aes(y = mean_periodicity, ymin = mean_periodicity, ymax = mean_periodicity), width = 0, position = position_dodge(0.8)) +  # Whiskers with width 0
  geom_point(data = summary_data, aes(y = mean_periodicity), position = position_dodge(0.8), size = 3, color = "transparent") +
  scale_fill_manual(values = stage_colors) +
  labs(x = NULL,  # Remove x-axis label
       y = "Periodicity of Calcium Oscillations (min)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        axis.line.x = element_line(color = "black", size = 1.5),  # Add black line and make it bolder for x-axis
        axis.line.y = element_line(color = "black", size = 1.5),  # Add black line and make it bolder for y-axis
        axis.text.x = element_text(face = "bold", size = 18, color = "black"),  # Bold, larger x-axis labels
        axis.text.y = element_text(face = "bold", size = 18, color = "black"),  # Bold, larger y-axis labels
        axis.title = element_text(face = "bold", size = 20),  # Bold, larger axis titles
        legend.position = "none",  # Remove legend
        plot.title = element_blank(),  # Remove plot title
        plot.subtitle = element_blank(),  # Remove plot subtitle
        plot.caption = element_blank()) +  # Remove plot caption
  geom_text(data = summary_data, aes(x = 1.5, y = max(data_filtered$mean_periodicity), label = ifelse(t_test_result$p.value < 0.05, "*", "")), size = 10, color = "red")  # Significance asterisk

# Print the plot
print(p)

# Print the summary data
print(summary_data)
