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
  0.193857125, 0.162177552, 0.151492282, 0.141444646, 0.241496448,
  0.243813591, 0.182020704, 0.182131217, 1, 0.812677783,
  0.598573089, 0.446304303, 0.349790936, 0.263935807, 0.169587197,
  0.135145693, 0.116731255, 0.075932847, 0.074420387, 0.075808818,
  0.060793945, 0.052315305, 0.039811745, 0.040047611, 0.459459013,
  0.764775561, 0.554180945, 0.366831696, 0.208844966, 0.120602641,
  0.102840677, 0.102298803, 0.10866781, 0.084369614, 0.050192333,
  0.030792556, 0.026961718, 0.038521725, 0.04242138, 0.029501211,
  0.204279497, 0.764760809, 0.547229517, 0.383288887, 0.271261883,
  0.140723532, 0.101355162, 0.030903157, 0.017969561, 0.015610634,
  0.011933418, 0.012502235, 0.009838713, 0.01035223, 0.018108342,
  0.041518286, 0.003093558, 0.00062403, 0.020149159, 0.017234842, 0
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
  
  # Ensure the last point is considered a peak if it exceeds the threshold
  if (fluorescence_data[length(fluorescence_data)] > threshold) {
    thresholded_peaks <- rbind(thresholded_peaks, c(fluorescence_data[length(fluorescence_data)], length(fluorescence_data)))
  }
  
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
  
  # Plot the fluorescence data and mark peaks
  plot(time_intervals, fluorescence_data, type = "l", xlab = "Time (min)", ylab = "Normalised Calcium Intensity - a.u", main = "Intensity vs Time", axes = FALSE)
  points(peak_times, thresholded_peaks[, 1], col = "red", pch = 19)
  
  # Add axes with adjusted borders
  axis(side = 1, labels = TRUE, at = pretty(time_intervals))
  axis(side = 2, labels = TRUE, at = pretty(fluorescence_data))
  
  # Draw the top and right borders only
  box(bty = "l")
} else {
  print("No peaks found above the threshold.")
}
