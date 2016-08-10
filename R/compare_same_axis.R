# This script compares measurements from different sources with the same unit (with possibly dissimilar dataset sizes) using linear approximation
# in order for this to work, you must first perform an outer join (keep all data) of the data you want to compare into one joined dataset, sharing one key

bottom_axis <- further_joined_data$Timestamp
bottom_axis_name <- "Time (24hr)"

common_measurement_name <- "Altitude (meters)"

measurement_1 <- further_joined_data$Altitude_m.x
measurement_1_source <- "Link-TLM"
measurement_1_color <- "red"

measurement_2 <- further_joined_data$Altitude_m.y
measurement_2_source <- "Tyrion"
measurement_2_color <- "purple"

domain <- c(min(tlm_data$Timestamp), max(tlm_data$Timestamp))

################################################################################

plot(approxfun(bottom_axis, measurement_1), xlim = domain, type = "l", col = measurement_1_color, xlab = bottom_axis_name, ylab = common_measurement_name)
points(bottom_axis, measurement_1, col = measurement_1_color, pch = 15, cex = 0.5)

curve(f(x), xlim = domain, col = measurement_2_color, add = TRUE)
points(bottom_axis, measurement_2, col = measurement_2_color, pch = 16, cex = 0.5)

legend("topleft", legend = c(measurement_1_source, measurement_2_source), text.col = c(measurement_1_color, measurement_2_color), pch = c(16, 15), col = c(measurement_1_color, measurement_2_color))

grid()