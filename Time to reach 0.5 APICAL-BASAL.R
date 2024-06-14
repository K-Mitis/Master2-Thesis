# Input the data
time <- c(0, 1.970913, 3.941826, 5.91274002, 7.88365302, 9.85456602, 11.82547998, 13.79639202, 15.76730502, 17.73821802, 19.70913102, 21.68004402, 23.650959, 25.621872, 27.592785, 29.563698, 31.534611, 33.505524, 35.476437, 37.44735, 39.41826198, 41.38917498, 43.36008798, 45.33100098, 47.301918, 49.272831)

fluorescence_data1 <- c(0.037787705, 0.021935495, 0.036182699, 0.023283052, 0.020568357, 
                        0.086494008, 0.408592158, 0.795248535, 0.980700019, 0.970396401, 
                        1, 0.961340722, 0.901934285, 0.783755725, 0.73237446, 
                        0.638370428, 0.49441212, 0.365315496, 0.278643222, 0.204593386, 
                        0.142036325, 0.084869171, 0.049042933, 0.029921183, 0, 0.015436112)

fluorescence_data2 <- c(0.060883156, 0.025578531, 0.039816373, 0.011547661, 0.014941486, 
                        0.025950855, 0.250196592, 0.725508152, 0.941931324, 0.961549909, 
                        1, 0.880509875, 0.938909823, 0.863581851, 0.747609897, 
                        0.699350323, 0.577252693, 0.450478011, 0.33992809, 0.276064724, 
                        0.132445026, 0.134845558, 0.061214475, 0.049708151, 0, 0.004470078)

fluorescence_data3 <- c(0.09486282, 0.029281442, 0.073060363, 0.055456643, 0.06955116, 
                        0.048438326, 0.252891023, 0.719783485, 0.843179794, 0.961549909, 
                        0.963642447, 0.918080783, 0.89719815, 0.842719838, 0.643042048, 
                        0.670885202, 0.655467899, 0.469194202, 0.293274907, 0.278605644, 
                        0.172870049, 0.110107539, 0.125122681, 0.141862859, 0, 0.056780141)

fluorescence_data4 <- c(0.09784369, 0.045505783, 0.109921498, 0.064538014, 0.083448099, 
                        0.081160143, 0.118553147, 0.648488921, 0.881202944, 0.97438009, 
                        1, 0.967670188, 0.872296945, 0.877634487, 0.703479765, 
                        0.683837878, 0.557995766, 0.453624665, 0.313355908, 0.275841218, 
                        0.13624346, 0.135968497, 0.035776659, 0, 0.042639301, 0.028029552)

# Function for linear interpolation
interpolate <- function(x1, y1, x2, y2, y) {
  x1 + (y - y1) * (x2 - x1) / (y2 - y1)
}

# Adjusting plot margins and y-axis limits
par(mar=c(5, 6, 4, 2) + 0.1)  # Add more space around the plot

# Plot the graph with all fluorescence data sets
plot(time, fluorescence_data1, type="l", col="blue", lwd=4, xlab="Time (sec)", ylab="Fluorescence Intensity a.u", ylim=c(0, 1.05), yaxp=c(0, 1, 10), xaxs="i", yaxs="i", xaxt='n', yaxt='n', bty='n', cex.lab=2, cex.axis=1.5)
lines(time, fluorescence_data2, col="red", lwd=4)
lines(time, fluorescence_data3, col="green", lwd=4)
lines(time, fluorescence_data4, col="orange", lwd=4)

# Add axes manually with bolder lines and larger annotations
axis(1, at = seq(0, 60, by = 5), cex.axis=1.5, lwd=3)
axis(2, at = seq(0, 1, by = 0.1), labels=seq(0, 1, by = 0.1), cex.axis=1.5, lwd=3)

# Find the peak intensity and its index for all fluorescence data sets
peak_index_data1 <- which.max(fluorescence_data1)
peak_intensity_data1 <- fluorescence_data1[peak_index_data1]

peak_index_data2 <- which.max(fluorescence_data2)
peak_intensity_data2 <- fluorescence_data2[peak_index_data2]

peak_index_data3 <- which.max(fluorescence_data3)
peak_intensity_data3 <- fluorescence_data3[peak_index_data3]

peak_index_data4 <- which.max(fluorescence_data4)
peak_intensity_data4 <- fluorescence_data4[peak_index_data4]

# Subset the data up to the peak for all fluorescence data sets
time_subset_data1 <- time[1:peak_index_data1]
fluorescence_subset_data1 <- fluorescence_data1[1:peak_index_data1]

time_subset_data2 <- time[1:peak_index_data2]
fluorescence_subset_data2 <- fluorescence_data2[1:peak_index_data2]

time_subset_data3 <- time[1:peak_index_data3]
fluorescence_subset_data3 <- fluorescence_data3[1:peak_index_data3]

time_subset_data4 <- time[1:peak_index_data4]
fluorescence_subset_data4 <- fluorescence_data4[1:peak_index_data4]

# Identify the indices surrounding the point where fluorescence first hits 0.5 for all fluorescence data sets
index1_data1 <- max(which(fluorescence_subset_data1 < 0.5))
index2_data1 <- min(which(fluorescence_subset_data1 > 0.5))

index1_data2 <- max(which(fluorescence_subset_data2 < 0.5))
index2_data2 <- min(which(fluorescence_subset_data2 > 0.5))

index1_data3 <- max(which(fluorescence_subset_data3 < 0.5))
index2_data3 <- min(which(fluorescence_subset_data3 > 0.5))

index1_data4 <- max(which(fluorescence_subset_data4 < 0.5))
index2_data4 <- min(which(fluorescence_subset_data4 > 0.5))

# Values at surrounding points for all fluorescence data sets
x1_data1 <- time_subset_data1[index1_data1]
y1_data1 <- fluorescence_subset_data1[index1_data1]
x2_data1 <- time_subset_data1[index2_data1]
y2_data1 <- fluorescence_subset_data1[index2_data1]

x1_data2 <- time_subset_data2[index1_data2]
y1_data2 <- fluorescence_subset_data2[index1_data2]
x2_data2 <- time_subset_data2[index2_data2]
y2_data2 <- fluorescence_subset_data2[index2_data2]

x1_data3 <- time_subset_data3[index1_data3]
y1_data3 <- fluorescence_subset_data3[index1_data3]
x2_data3 <- time_subset_data3[index2_data3]
y2_data3 <- fluorescence_subset_data3[index2_data3]

x1_data4 <- time_subset_data4[index1_data4]
y1_data4 <- fluorescence_subset_data4[index1_data4]
x2_data4 <- time_subset_data4[index2_data4]
y2_data4 <- fluorescence_subset_data4[index2_data4]

# Perform interpolation to find the exact time when fluorescence first hits 0.5 for all fluorescence data sets
time_point_data1 <- interpolate(x1_data1, y1_data1, x2_data1, y2_data1, 0.5)
time_point_data2 <- interpolate(x1_data2, y1_data2, x2_data2, y2_data2, 0.5)
time_point_data3 <- interpolate(x1_data3, y1_data3, x2_data3, y2_data3, 0.5)
time_point_data4 <- interpolate(x1_data4, y1_data4, x2_data4, y2_data4, 0.5)

# Print the result for all fluorescence data sets
cat("Time to reach 0.5 intensity for fluorescence data 1:", time_point_data1, "\n")
cat("Time to reach 0.5 intensity for fluorescence data 2:", time_point_data2, "\n")
cat("Time to reach 0.5 intensity for fluorescence data 3:", time_point_data3, "\n")
cat("Time to reach 0.5 intensity for fluorescence data 4:", time_point_data4, "\n")

# Calculate time differences
time_diff_1_2 <- time_point_data2 - time_point_data1
time_diff_2_3 <- time_point_data3 - time_point_data2
time_diff_3_4 <- time_point_data4 - time_point_data3

# Print the time differences with more decimals
cat("Time difference between fluorescence data 1 and 2:", round(time_diff_1_2, 4), "\n")
cat("Time difference between fluorescence data 2 and 3:", round(time_diff_2_3, 4), "\n")
cat("Time difference between fluorescence data 3 and 4:", round(time_diff_3_4, 4), "\n")

# Add dots at the points where fluorescence first hits 0.5 with a black border
points(time_point_data1, 0.5, col="black", pch=21, bg="blue", lwd=4, cex=2)
points(time_point_data2, 0.5, col="black", pch=21, bg="red", lwd=4, cex=2)
points(time_point_data3, 0.5, col="black", pch=21, bg="green", lwd=4, cex=2)
points(time_point_data4, 0.5, col="black", pch=21, bg="orange", lwd=4, cex=2)

# Add vertical dotted lines to the x-axis with increased line width
segments(x0 = time_point_data1, y0 = 0.5, x1 = time_point_data1, y1 = 0, col = "blue", lty=2, lwd=4)
segments(x0 = time_point_data2, y0 = 0.5, x1 = time_point_data2, y1 = 0, col = "red", lty=2, lwd=4)
segments(x0 = time_point_data3, y0 = 0.5, x1 = time_point_data3, y1 = 0, col = "green", lty=2, lwd=4)
segments(x0 = time_point_data4, y0 = 0.5, x1 = time_point_data4, y1 = 0, col = "orange", lty=2, lwd=4)

# Annotations with dynamically created expressions
text(x = max(time), y = max(fluorescence_data1) - 0.05, 
     labels = bquote(t2[1/2] - t1[1/2] * ":" ~ .(round(time_diff_1_2, 4)) ~ " sec"), pos = 2, col = "black", cex=1.5, font=2)
text(x = max(time), y = max(fluorescence_data1) - 0.15, 
     labels = bquote(t3[1/2] - t2[1/2] * ":" ~ .(round(time_diff_2_3, 4)) ~ " sec"), pos = 2, col = "black", cex=1.5, font=2)
text(x = max(time), y = max(fluorescence_data1) - 0.25, 
     labels = bquote(t4[1/2] - t3[1/2] * ":" ~ .(round(time_diff_3_4, 4)) ~ " sec"), pos = 2, col = "black", cex=1.5, font=2)
