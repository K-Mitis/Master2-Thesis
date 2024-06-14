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

# Group by tissue to calculate required statistics
summary_data <- data %>%
  group_by(tissue) %>%
  summarise(
    Average_Periodicity = mean(periodicity, na.rm = TRUE),
    StDev = sd(periodicity, na.rm = TRUE),
    StError = sd(periodicity, na.rm = TRUE) / sqrt(n()),
    Oscillations_N = sum(!is.na(periodicity)),
    Max = max(periodicity, na.rm = TRUE),
    Min = min(periodicity, na.rm = TRUE)
  )

# Print the summary data
print(summary_data)
