# Load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(ggsignif)

# Define the path to your Excel file
file_path <- "C:/Users/Admin/Desktop/Analysis/Supplementary Table for Report.xlsx"

# Read the data from Sheet 3, treating the first row as column names
data <- read_excel(file_path, sheet = 3, col_names = TRUE)

# Melt the data for easier plotting with ggplot2
data_melted <- melt(data, id.vars = NULL, variable.name = "Tissue", value.name = "Measurement")

# Ensure all values are within the y-axis limits by capping them at 50
data_melted$Measurement <- pmin(data_melted$Measurement, 50)

# Perform ANOVA
anova_result <- aov(Measurement ~ Tissue, data = data_melted)
anova_summary <- summary(anova_result)
print(anova_summary)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Extract significant pairs from Tukey's HSD test results
significant_pairs <- as.data.frame(tukey_result$Tissue)
significant_pairs <- significant_pairs[significant_pairs$p.adj < 0.05, ]

# Define colors for each tissue type
tissue_colors <- c(
  "EXE" = "#40E0D0",
  "EPIBLAST" = "lightgoldenrod",
  "MESODERM" = "red",
  "em-VE" = "lightblue",
  "exe-VE" = "#FF69B4"
)

# Create a box plot
p <- ggplot(data_melted, aes(x = Tissue, y = Measurement, fill = Tissue)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 1.5, alpha = 0.9) +  # Increase the size of the dots
  scale_fill_manual(values = tissue_colors) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) + # Set y-axis limits to go up to 50
  labs(
    y = "# cells ≥ 1 Calcium oscillation / 10 min"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 1.5),  # Make axis lines bolder
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(face = "bold", size = 18),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # Increase top margin
  )

# Add significance annotations manually if there are significant pairs
if (nrow(significant_pairs) > 0) {
  for (i in 1:nrow(significant_pairs)) {
    tissues <- strsplit(rownames(significant_pairs)[i], "-")[[1]]
    p_value <- significant_pairs$p.adj[i]
    annotation <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*", "ns")))
    
    # Adjust y_position for annotations
    y_position <- 50 + (i * 1.5)
    
    p <- p + geom_signif(
      comparisons = list(tissues),
      annotations = annotation,
      y_position = y_position,
      tip_length = 0.01,
      textsize = 10,        # Increase text size
      size = 1.5,           # Increase line thickness
      vjust = 0.5           # Adjust vertical justification
    )
  }
}

# Print the plot
print(p)

# Save the plot with larger dimensions and higher resolution
ggsave("boxplot_large.png", plot = p, 
