# Load necessary libraries
library(readxl)
library(dplyr)

# Load the data from the second sheet
file_path <- "C:/Users/Admin/Desktop/Analysis/PERIODICITY ANALYSIS/Periodicity analysis Pooled.xlsx"
data <- read_excel(file_path, sheet = 1)

# Remove empty rows
data <- na.omit(data)

# Convert the columns to appropriate types
data <- data %>% mutate(
  periodicity = as.numeric(periodicity),
  cell = as.integer(cell),
  embryo = as.integer(embryo)
)

# Group by cell to calculate average periodicity for each cell
cell_summary <- data %>%
  group_by(cell) %>%
  summarise(
    Average_Periodicity_Cell = mean(periodicity, na.rm = TRUE)
  )

# Join cell-level summary with the original data
data_with_cell_avg <- left_join(data, cell_summary, by = "cell")

# Group by cell and tissue to calculate the rest of the statistics
summary_data <- data_with_cell_avg %>%
  group_by(cell, tissue) %>%
  summarise(
    StDev = sd(periodicity, na.rm = TRUE),
    StError = sd(periodicity, na.rm = TRUE) / sqrt(n()),
    Oscillations_N = sum(!is.na(periodicity)),
    Max = max(periodicity, na.rm = TRUE),
    Min = min(periodicity, na.rm = TRUE),
    Average_Periodicity_Cell = first(Average_Periodicity_Cell)  # Take the first value of average periodicity per cell
  ) %>%
  # Group by tissue to calculate mean and rest
  group_by(tissue) %>%
  summarise(
    Average_Periodicity_Cell = mean(Average_Periodicity_Cell, na.rm = TRUE),
    Average_StDev = mean(StDev, na.rm = TRUE),
    Average_StError = mean(StError, na.rm = TRUE),
    Total_Oscillations_N = sum(Oscillations_N),
    Max = max(Max),
    Min = min(Min)
  )

# Set options to display all columns
options(width = 120)

# Print the summary data
print(summary_data)
