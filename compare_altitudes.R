# This script compares differing altitudes with dissimilar dataset sizes using linear approximation

plot(approxfun(further_joined_data$Timestamp, further_joined_data$Altitude_m.x), xlim = domain, type = "l", col = "red", xlab = "Time", ylab = "Altitude (meters)")

domain <- c(min(tlm_data$Timestamp), max(tlm_data$Timestamp))

curve(f(x), xlim = domain, col = "purple", add = TRUE)

points(tlm_data$Timestamp, tlm_data$Altitude_m, col = "red", pch = 15, cex = 0.5)

points(tyrion$Timestamp, tyrion$Altitude_m, col = "purple", pch = 16, cex = 0.5)

legend("topleft", legend = c("Tyrion", "LINK-TLM"), text.col = c("purple", "red"), pch = c(16, 15), col = c("purple", "red"))

grid()