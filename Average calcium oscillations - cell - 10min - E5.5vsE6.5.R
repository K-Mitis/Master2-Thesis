# Load necessary libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# Read data from Excel file and specified sheet
file_path <- "C:/Users/Admin/Desktop/Analysis/Supplementary Table for Report.xlsx"
sheet_name <- 10
data <- read_excel(file_path, sheet = sheet_name)

# Exclude the last column
data <- data[, -ncol(data)]

# Convert data to long format
data_long <- gather(data, Tissue, Periodicity)

# Define colors for each tissue type including MESODERM E6.5
tissue_colors <- c("EPIBLAST E5.5" = "lightgoldenrod", "EPIBLAST E6.5" = "lightgoldenrod",
                   "EXE E5.5" = "#40E0D0", "EXE E6.5" = "#40E0D0",
                   "em-VE E5.5" = "lightblue", "em-VE E6.5" = "lightblue",
                   "exe-VE E5.5" = "#FF69B4", "exe-VE E6.5" = "#FF69B4",
                   "MESODERM E5.5" = "red", "MESODERM E6.5" = "red")  # Adding color for MESODERM E6.5

# Reorder levels of the Tissue variable
data_long$Tissue <- factor(data_long$Tissue, levels = c("EPIBLAST E5.5", "EPIBLAST E6.5", 
                                                        "EXE E5.5", "EXE E6.5",
                                                        "em-VE E5.5", "em-VE E6.5",
                                                        "exe-VE E5.5", "exe-VE E6.5",
                                                        "MESODERM E5.5", "MESODERM E6.5"))  # Include MESODERM E6.5

# Plot with colors assigned to tissue types and black dots
ggplot(data_long, aes(x = Tissue, y = Periodicity, fill = Tissue)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(0.2), alpha = 0.5, color = "black") +  # Add jittered points with black color
  scale_fill_manual(values = tissue_colors) +  # Assign colors to tissue types
  labs(title = NULL,
       x = NULL,
       y = "Average calcium oscillations / cell / 10min") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        axis.line = element_line(color = "black"),  # Add black lines to axes
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(face = "bold", size = 12),  # Make x-axis annotations bold and larger
        axis.text.y = element_text(color = "black", face = "bold", size = 12),  # Make y-axis annotations bold, black, and larger
        axis.title.y = element_text(color = "black", face = "bold", size = 16),  # Make y-axis title bold, black, and larger
        axis.title.x = element_text(color = "black", face = "bold", size = 12))  # Make x-axis title bold, black, and larger

# Save the plot with larger dimensions and higher resolution
ggsave("boxplot_periodicity_supplementary.png", width = 12, height = 8, dpi = 300)

# Function to perform Welch's t-test between unpaired tissues
compare_tissues_welch <- function(data, tissue1, tissue2) {
  data1 <- filter(data, Tissue == tissue1)
  data2 <- filter(data, Tissue == tissue2)
  
  # Ensure the periodicity values are numeric
  data1$Periodicity <- as.numeric(data1$Periodicity)
  data2$Periodicity <- as.numeric(data2$Periodicity)
  
  # Perform Welch's t-test only if both groups have more than 1 observation
  if (nrow(data1) > 1 & nrow(data2) > 1) {
    t_test_result <- t.test(data1$Periodicity, data2$Periodicity, var.equal = FALSE)
  } else {
    t_test_result <- NA
  }
  
  t_test_result
}

# Define tissue pairs for comparison
tissue_pairs <- list(c("EPIBLAST E5.5", "EPIBLAST E6.5"),
                     c("EXE E5.5", "EXE E6.5"),
                     c("em-VE E5.5", "em-VE E6.5"),
                     c("exe-VE E5.5", "exe-VE E6.5"),
                     c("MESODERM E5.5", "MESODERM E6.5"))

# Compare each tissue pair and store results
results <- lapply(tissue_pairs, function(pair) {
  compare_tissues_welch(data_long, pair[1], pair[2])
})

# Display results
names(results) <- sapply(tissue_pairs, function(pair) paste(pair, collapse = " vs "))
results

# Print results
for (comparison in names(results)) {
  cat("\nComparison: ", comparison, "\n")
  print(results[[comparison]])
}
