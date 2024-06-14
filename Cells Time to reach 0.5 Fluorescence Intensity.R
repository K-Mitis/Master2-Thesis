library(ggplot2)
library(ggrepel)
library(dplyr)
library(grid)

data <- data.frame(
  Measurement = factor(rep(c("t1 1/2", "t2 1/2", "t3 1/2", "t4 1/2"), 4), 
                       levels = c("t1 1/2", "t2 1/2", "t3 1/2", "t4 1/2")),
  Time_sec = c(11.735448, 12.069264, 12.709434, 12.834312,
               12.291414, 12.861306, 12.868614, 13.244142,
               12.088314, 12.440802, 12.834162, 12.992076,
               12.097932, 12.171192, 12.458562, 13.150902),
  Group = rep(c("Cell 1", "Cell 2", "Cell 3", "Cell 4"), each = 4)
)

# Convert Measurement to numeric for fitting
data$Measurement_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", data$Measurement))

# Fit linear models for each group and extract the coefficients
fit_models <- data %>%
  group_by(Group) %>%
  do(model = lm(Time_sec ~ Measurement_num, data = .))

# Extract coefficients
equations <- fit_models %>%
  summarise(Group, 
            intercept = coef(model)[1], 
            slope = coef(model)[2])

# Define colors for groups
colors <- c("Cell 1" = "#F8766D", "Cell 2" = "#7CAE00", "Cell 3" = "#00BFC4", "Cell 4" = "#C77CFF")

# Create the plot
plot <- ggplot(data, aes(x = Measurement, y = Time_sec, group = Group, color = Group)) +
  geom_point(size = 5) +
  geom_line(size = 1.5) +
  labs(x = NULL, y = "Time (sec)") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1.2)) +
  geom_label_repel(aes(label = round(Time_sec, 2)),
                   size = 4, fontface = "bold",
                   box.padding = 0.35, point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(limits = c(11.5, 13.5)) +
  geom_smooth(aes(x = as.numeric(Measurement), y = Time_sec, color = Group), method = "lm", se = FALSE, linetype = "dotted", size = 0.8)

# Create a custom legend with colored text
custom_legend <- grobTree(
  textGrob(paste0(equations$Group[1], ": y = ", round(equations$slope[1], 2), "x + ", round(equations$intercept[1], 2)), 
           x = 0.95, y = 0.30, hjust = 1, gp = gpar(col = colors[equations$Group[1]], fontsize = 12, fontface = "bold")),
  textGrob(paste0(equations$Group[2], ": y = ", round(equations$slope[2], 2), "x + ", round(equations$intercept[2], 2)), 
           x = 0.95, y = 0.25, hjust = 1, gp = gpar(col = colors[equations$Group[2]], fontsize = 12, fontface = "bold")),
  textGrob(paste0(equations$Group[3], ": y = ", round(equations$slope[3], 2), "x + ", round(equations$intercept[3], 2)), 
           x = 0.95, y = 0.20, hjust = 1, gp = gpar(col = colors[equations$Group[3]], fontsize = 12, fontface = "bold")),
  textGrob(paste0(equations$Group[4], ": y = ", round(equations$slope[4], 2), "x + ", round(equations$intercept[4], 2)), 
           x = 0.95, y = 0.15, hjust = 1, gp = gpar(col = colors[equations$Group[4]], fontsize = 12, fontface = "bold"))
)

# Add custom legend to the plot
plot <- plot + annotation_custom(custom_legend)

# Save the plot
ggsave("combined_time_measurements_plot_with_trendlines.png", plot, width = 12, height = 8)

plot
