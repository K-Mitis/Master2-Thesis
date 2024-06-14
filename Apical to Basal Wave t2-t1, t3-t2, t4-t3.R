# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create the data frame with the provided values
data <- data.frame(
  `t21/2 – t11/2` = c(0.336, 0.57, 0.354, 0.072, 1.884, 2.088),
  `t31/2 – t21/2` = c(0.642, 0.006, 0.396, 0.288, 0.24, 1.938),
  `t41/2 – t31/2` = c(0.126, 0.378, 0.156, 0.69, 0.774, 0.264)
)

# Reshape the data for ggplot2
data_melted <- melt(data, variable.name = "Time_Difference", value.name = "Values")

# Define specific colors for each Time_Difference category
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

# Create the box plots
ggplot(data_melted, aes(x = Time_Difference, y = Values)) +
  geom_boxplot(color = "black") + 
  geom_jitter(aes(color = Time_Difference), size = 5, position = position_jitter(width = 0.1, height = 0)) + 
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Add horizontal line at y = 0
  labs(x = NULL, y = "Time (sec)") +  # Remove x-axis title
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.title.x = element_blank(),  # This removes the x-axis title
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_blank(),  # This removes the x-axis line
        axis.line.y = element_line(color = "black", linewidth = 1.2),
        legend.position = "none") +
  scale_color_manual(values = colors) + # Assign specific colors to each jitter point
  scale_y_continuous(limits = c(-0.5, max(data_melted$Values) + 0.5), breaks = seq(-0.5, max(data_melted$Values) + 0.5, by = 0.5))

# Save the plot
ggsave("Box_Plots.png", width = 12, height = 8)
