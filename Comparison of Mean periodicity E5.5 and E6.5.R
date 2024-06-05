library(readxl)
library(ggplot2)
library(tidyr)

# Read data from Excel file
file_path <- "C:/Users/Admin/Desktop/Analysis/E5.5 AND E6.5PERIODICITY TISSUE COMPARISON.xlsx"
data <- read_excel(file_path)

# Convert data to long format
data_long <- gather(data, Tissue, Periodicity)

# Define colors for each tissue type
tissue_colors <- c("EPIBLAST E5.5" = "lightgoldenrod", "EPIBLAST E6.5" = "lightgoldenrod",
                   "EXE E5.5" = "#40E0D0", "EXE E6.5" = "#40E0D0",
                   "em-VE E5.5" = "lightblue", "em-VE E6.5" = "lightblue",
                   "exe-VE E5.5" = "#FF69B4", "exe-VE E6.5" = "#FF69B4")

# Reorder levels of the Tissue variable
data_long$Tissue <- factor(data_long$Tissue, levels = c("EPIBLAST E5.5", "EPIBLAST E6.5", 
                                                        "EXE E5.5", "EXE E6.5",
                                                        "em-VE E5.5", "em-VE E6.5",
                                                        "exe-VE E5.5", "exe-VE E6.5"))

# Plot with colors assigned to tissue types
ggplot(data_long, aes(x = Tissue, y = Periodicity, fill = Tissue)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = tissue_colors) +  # Assign colors to tissue types
  labs(title = "Periodicity Tissue Comparison",
       x = "Tissue",
       y = "Periodicity of Calcium Oscillations (min)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        axis.line = element_line(color = "black"),  # Add black lines to axes
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(face = "bold"))  # Make tissue names bold
