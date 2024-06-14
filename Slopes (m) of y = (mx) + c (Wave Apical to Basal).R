# Load necessary libraries
library(ggplot2)

# Create the data frame with the provided values
data <- data.frame(
  Values = c(0.3937, 0.2865, 0.3105, 0.3446, 0.8923, 1.4808)
)

# Define colors for each dot
colors <- rainbow(nrow(data))

# Create the plot
ggplot(data, aes(y = Values, x = factor(1))) +
  geom_point(aes(color = colors), size = 5, position = position_jitter(width = 0.1, height = 0)) + # Assign different colors to each dot
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Add horizontal line at y = 0
  labs(x = NULL, y = "Values") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),  # This removes the x-axis line
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color = "black", linewidth = 1.2)) +
  scale_y_continuous(limits = c(-0.5, 1.5), breaks = c(-0.5, 0, 0.5, 1, 1.5)) +
  guides(color = "none") # Hide color legend

# Save the plot
ggsave("Individual_Values_Plot.png", width = 12, height = 8)
