# Load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(ggsignif)

# Define the path to your Excel file
file_path <- "C:/Users/Admin/Desktop/Analysis/Supplementary Table for Report.xlsx"

# Read the data from Sheet 4, treating the first row as column names
data <- read_excel(file_path, sheet = 4, col_names = TRUE)

# Check for missing values
if (any(is.na(data))) {
  print("Warning: Data contains missing values.")
}

# Melt the data for easier plotting with ggplot2
data_melted <- melt(data, id.vars = NULL, variable.name = "Tissue", value.name = "Measurement")

# Handle missing values by setting them to NA
data_melted$Measurement <- ifelse(is.na(data_melted$Measurement), NA, data_melted$Measurement)

# Check the range of Measurement values
print(summary(data_melted$Measurement))

# Print rows that are out of range but do not remove them
out_of_range <- data_melted[data_melted$Measurement > 30 | data_melted$Measurement < 0, ]
print("Rows that are out of range:")
print(out_of_range)

# Perform ANOVA, removing rows with NA values just for this step
anova_result <- aov(Measurement ~ Tissue, data = na.omit(data_melted))
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

# Calculate the minimum and maximum measurement values
min_measurement <- min(data_melted$Measurement, na.rm = TRUE)
max_measurement <- max(data_melted$Measurement, na.rm = TRUE)
print(paste("Min measurement:", min_measurement))
print(paste("Max measurement:", max_measurement))

# Set dynamic y-axis limits if necessary
y_max_limit <- ifelse(max_measurement > 30, max_measurement, 30)

# Create a box plot
p <- ggplot(data_melted, aes(x = Tissue, y = Measurement, fill = Tissue)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers
  geom_jitter(color = "black", size = 1.5, alpha = 0.9, width = 0.2, na.rm = TRUE) +  # Increase the size of the dots and adjust the width, na.rm = TRUE to handle NAs
  scale_fill_manual(values = tissue_colors) +
  scale_y_continuous(limits = c(0, y_max_limit), breaks = seq(0, y_max_limit, 10), expand = c(0.02, 0)) + # Adjust y-axis limits
  labs(
    y = "# of cells with repeated oscillations / 10 min"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 18, margin = margin(t = 10)),
    axis.text.y = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 1.5),  # Make axis lines bolder
    plot.margin = margin(t = 50, r = 20, b = 20, l = 20)  # Increase top margin
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
    y_position <- y_max_limit + (i * 1.5)
    
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
ggsave("boxplot_large.png", plot = p, width = 12, height = 8, dpi = 300)
