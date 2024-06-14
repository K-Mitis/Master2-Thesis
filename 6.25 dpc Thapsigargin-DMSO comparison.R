# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create the data frame with the provided values
data <- data.frame(
  Embryo = 1:7,
  DMSO_P_D = c(350, 346, 348, 354, 352, 346, 351),
  DMSO_A_P = c(205, 199, 221, 215, 185, 220, 197),
  Thaps_P_D = c(209, 196, 276, 288, 201, 265, NA),
  Thaps_A_P = c(134, 114, 143, 160, 124, 133, NA)
)

# Reshape the data for ggplot2
data_melted <- melt(data, id.vars = "Embryo", variable.name = "Measurement", value.name = "Values")

# Remove NA values for plotting
data_melted <- na.omit(data_melted)

# Rearrange the factor levels for the Measurements
data_melted$Measurement <- factor(data_melted$Measurement, levels = c("DMSO_P_D", "Thaps_P_D", "DMSO_A_P", "Thaps_A_P"))

# Define specific colors for each Measurement category
colors <- c("DMSO_P_D" = "#F8766D", "DMSO_A_P" = "#7CAE00", "Thaps_P_D" = "#00BFC4", "Thaps_A_P" = "#C77CFF")

# Create the box plots
ggplot(data_melted, aes(x = Measurement, y = Values)) +
  geom_boxplot(color = "black") + 
  geom_jitter(aes(fill = Measurement), color = "black", size = 5, shape = 21, stroke = 1, position = position_jitter(width = 0.1, height = 0)) + 
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Add horizontal line at y = 0
  labs(x = NULL, y = "μM") +  # Name y-axis μM
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.2),  # Add x-axis line
        axis.line.y = element_line(color = "black", size = 1.2),
        legend.position = "none") +
  scale_fill_manual(values = colors) + # Assign specific fill colors to each jitter point
  scale_y_continuous(limits = c(100, max(data_melted$Values) + 50), breaks = seq(100, max(data_melted$Values) + 50, by = 50))

# Save the plot
ggsave("Measurement_Box_Plots.png", width = 12, height = 8)

# Remove NA values for analysis
proximal_distal_length_data <- na.omit(data[, c("DMSO_P_D", "Thaps_P_D")])
anterior_posterior_length_data <- na.omit(data[, c("DMSO_A_P", "Thaps_A_P")])

# Perform t-tests
proximal_distal_length_ttest <- t.test(proximal_distal_length_data$DMSO_P_D, proximal_distal_length_data$Thaps_P_D)
anterior_posterior_length_ttest <- t.test(anterior_posterior_length_data$DMSO_A_P, anterior_posterior_length_data$Thaps_A_P)

# Print the t-test results
print("Proximal-Distal Length Comparison (DMSO vs Thaps):")
print(proximal_distal_length_ttest)
print("Anterior-Posterior Length Comparison (DMSO vs Thaps):")
print(anterior_posterior_length_ttest)
