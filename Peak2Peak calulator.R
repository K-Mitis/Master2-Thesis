# Load necessary libraries
install.packages("signal")  # Install only if not installed yet
library(signal)
# Load necessary libraries
install.packages("pracma")  # Install only if not installed yet
library(pracma)
# Load necessary libraries
if (!require("signal")) install.packages("signal")
if (!require("pracma")) install.packages("pracma")
library(signal)
library(pracma)

# Define time intervals
time_intervals <- c(0, 0.166666667, 0.333333333, 0.5, 0.666666667, 0.833333333, 1, 1.166666667, 1.333333333, 1.5, 
                    1.666666667, 1.833333333, 2, 2.166666667, 2.333333333, 2.5, 2.666666667, 2.833333333, 3,
                    3.166666667, 3.333333333, 3.5, 3.666666667, 3.833333333, 4, 4.166666667, 4.333333333, 4.5,
                    4.666666667, 4.833333333, 5, 5.166666667, 5.333333333, 5.5, 5.666666667, 5.833333333, 6,
                    6.166666667, 6.333333333, 6.5, 6.666666667, 6.833333333, 7, 7.166666667, 7.333333333, 7.5,
                    7.666666667, 7.833333333, 8, 8.166666667, 8.333333333, 8.5, 8.666666667, 8.833333333, 9,
                    9.166666667, 9.333333333, 9.5, 9.666666667, 9.833333333, 10)

# Define fluorescence data (provided by user)
fluorescence_data <- c(
  0.211871827, 0.198608662, 0.099231488, 0.251971037, 0.153527753,
  0.143502477, 0.148484175, 0.184536315, 0.227375739, 0.109007453,
  0.148670763, 0.208571111, 0.17426257, 0.178745645, 0.139206095,
  0.179492737, 0.121211386, 0.134536537, 0.176504055, 0.08715142,
  0.14306742, 0.102344878, 0.007014879, 0.048671206, 0.085844458,
  0.111622115, 0.46747575, 0.478122672, 0.086342343, 0.148234864,
  0.072456584, 0.189393305, 0.228185659, 0.078807968, 0.017724524,
  0.111746718, 0.081548076, 0.179804034, 0.146865178, 0.112618623,
  0.150788383, 0.205333117, 0.095806906, 0.095432991, 0.103963875,
  0.122394483, 0.137649821, 0.255146307, 0.156267862, 0.245246582,
  0.237961519, 0.163428322, 1, 0.007014879, 0.152842542, 0.244000762,
  0.040763549, 0.085034538, 0.12843457, 0.142818108, 0.195308684,
  0.089828805
)

# Define a threshold value
threshold <- 0.5

# Find local maxima
peaks <- findpeaks(fluorescence_data)

# If the first data point is above the threshold, consider it as a peak
if (fluorescence_data[1] > threshold) {
  peaks <- rbind(c(fluorescence_data[1], 1), peaks)
}

# Check if any peaks were found
if (nrow(peaks) > 0) {
  # Threshold peaks based on peak value
  thresholded_peaks <- peaks[peaks[, 1] > threshold, ]
  
  # Calculate peak-to-peak times
  peak_times <- time_intervals[thresholded_peaks[, 2]]
  peak_to_peak_times <- c(NA, diff(peak_times))
  
  # Combine peak indices, peak times, and peak-to-peak times into a dataframe
  peak_to_peak_data <- data.frame(Peak_Index = thresholded_peaks[, 2], Peak_Time = peak_times, Peak_to_Peak_Time = peak_to_peak_times)
  
  # Print each value in its own row
  cat("Peak_Index\n")
  cat(paste(peak_to_peak_data$Peak_Index, collapse = "\n"), "\n\n")
  
  cat("Peak_Time\n")
  cat(paste(peak_to_peak_data$Peak_Time, collapse = "\n"), "\n\n")
  
  cat("Peak_to_Peak_Time\n")
  cat(paste(peak_to_peak_data$Peak_to_Peak_Time, collapse = "\n"), "\n\n")
} else {
  print("No peaks found above the threshold.")
}