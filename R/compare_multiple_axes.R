# This script compares measurements from different sources with DIFFERENT units (with possibly dissimilar dataset sizes) using linear approximation
# in order for this to work, you must first perform an outer join (keep all data) of the data you want to compare into one joined dataset, sharing one key
# taken from http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes

bottom_axis <- joined_data$Timestamp
bottom_axis_name <- "Time (24hr)"

left_axis <- joined_data$Counts_Per_Minute
left_axis_name <- "Radiation (counts per minute)"

right_axis <- joined_data$Altitude_m
right_axis_name <- "Altitude (meters)"

domain <- c(min(tlm_data$Timestamp), max(tlm_data$Timestamp))
title <- paste(left_axis_name, "and", right_axis_name, "vs", bottom_axis_name)

################################################################################

par(mar = c(3, 3, 3, 4) + 0.1)
plot(approxfun(bottom_axis, left_axis), xlim = domain, axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue", main = title)
points(bottom_axis, left_axis, pch = 16, cex = 0.5, col = "blue")

#mtext(left_axis_name, side = 2, col = "blue", line = 2.5)
axis(2, col.axis = "blue", las = 1)

box()
par(new = TRUE)

plot(approxfun(bottom_axis, right_axis), xlim = domain, xlab = "", ylab = "", axes = FALSE, type = "l", col = "red")
points(bottom_axis, right_axis, pch = 15, cex = 0.5, col = "red")
# mtext(right_axis_name, side = 4, col = "red", line = 4)
axis(4, col = "red", col.axis = "red", las = 1)

axis.POSIXct(1,bottom_axis) # use axis.POSIXct for POSIXct datetime objects
#axis(1, pretty(range(bottom_axis),10))
#mtext(bottom_axis_name, side = 1, col = "black", line = 2.5) 

legend("topleft", legend = c(left_axis_name, right_axis_name), text.col = c("blue", "red"), pch = c(16, 15), col = c("blue", "red"))
grid()

# If you'd rather use plotrix (doesn't support POSIXct axis):
#install.packages("plotrix")
#library(plotrix)
#twoord.plot(ly = left_axis, ry = right_axis, lx = bottom_axis, rx = bottom_axis, main = paste(left_axis_name, "and", right_axis_name, "vs", bottom_axis_name), ylab = left_axis_name, rylab = right_axis_name, xlab = bottom_axis_name)

# save to PDF
dev.copy(pdf, paste(launch_number, "/", launch_number, "_", title, ".pdf", sep = ""))
dev.off()