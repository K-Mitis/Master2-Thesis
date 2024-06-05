# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(emmeans)

# Define file path
file_path <- "C:/Users/Admin/Desktop/Analysis/PERIODICITY ANALYSIS/Periodicity analysis Pooled.xlsx"

# Read data from the first sheet
data_sheet1 <- read_excel(file_path, sheet = 1)

# Analysis for the first sheet
# Calculate the mean periodicity for each cell
mean_data_sheet1 <- aggregate(periodicity ~ cell + tissue + embryo, data = data_sheet1, FUN = mean, na.rm = TRUE) # Add na.rm = TRUE to handle NA values

# Convert embryo to factor
mean_data_sheet1$embryo <- factor(mean_data_sheet1$embryo)

# Calculate mean and standard deviation for each tissue
summary_data_sheet1 <- mean_data_sheet1 %>%
  group_by(tissue) %>%
  summarize(mean_periodicity = mean(periodicity, na.rm = TRUE), sd_periodicity = sd(periodicity, na.rm = TRUE)) %>%
  ungroup()

# Define a color palette for embryos dynamically based on the number of unique embryos
num_embryos_sheet1 <- length(unique(mean_data_sheet1$embryo))
embryo_colors_sheet1 <- scales::hue_pal()(num_embryos_sheet1)

# Reorder levels of tissue factor
mean_data_sheet1$tissue <- factor(mean_data_sheet1$tissue, levels = c("MESODERM","EPIBLAST", "EXE", "em-VE", "exe-VE"))  

# Boxplot of mean periodicity by tissue with custom colors for embryos, including mean and SD
p <- ggplot(mean_data_sheet1, aes(x = tissue, y = periodicity, fill = tissue)) +
  geom_boxplot(alpha = 0.4, position = position_dodge(width = 0.9)) + # Adjust dodge width for better alignment
  geom_jitter(aes(color = "black"), size = 2, alpha = 0.6, position = position_jitterdodge(dodge.width = 0)) + # Adjust jittering and make dodge position transparent
  scale_color_manual(values = "black", guide = FALSE) + # Set point color to black and remove legend
  labs(title = "Mean Periodicity by Tissue with Standard Deviation (Sheet 1)",
       x = "Tissue",
       y = "Periodicity of Calcium Oscillations (min)") + # Remove color legend title
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(face = "bold"),
        axis.line.x = element_line(color = "black", size = 1),  # Add black axis line at bottom
        axis.line.y = element_line(color = "black", size = 1),  # Add black axis line at left
        plot.margin = margin(20, 20, 20, 20)) +  # Add margin around the plot
  guides(fill = "none")  # Remove fill legend

# Extract fill colors from the plot
fill_colors <- unique(p$data$fill)

# Print the fill colors
print(fill_colors)

# Print the plot
print(p)

# Perform ANOVA analysis
anova_result <- aov(periodicity ~ tissue + embryo, data = mean_data_sheet1)

# Perform Tukey's HSD test
tukey_results <- TukeyHSD(anova_result)

# Display the Tukey's HSD test results
tukey_results
# Extract fill colors from the plot
fill_colors <- unique(p$data$fill)

# Print the fill colors
print(fill_colors)