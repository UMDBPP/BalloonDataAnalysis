# taken from http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
bottom_axis <- as.POSIXct(joined_data$Timestamp, tz = Sys.timezone())
bottom_axis_name <- "Time (Hours)"
    
right_axis <- joined_data$Alt.ft.
right_axis_name <- "Altitude (feet)"
    
left_axis <- joined_data$Counts_Per_Minute
left_axis_name <- "Radiation (Counts per Minute)"

title <- paste(left_axis_name, "and", right_axis_name, "vs", bottom_axis_name)

par(mar = c(5, 4, 4, 6) + 0.1)
plot(bottom_axis, left_axis, pch = 16, axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue", main = title)
mtext(left_axis_name, side = 2, col = "blue", line = 2.5)
axis(2, col.axis = "blue", las = 1)

box()
par(new = TRUE)

plot(bottom_axis, right_axis, pch = 15, xlab = "", ylab = "", axes = FALSE, type = "l", col = "red")
mtext(right_axis_name, side = 4, col = "red", line = 4)
axis(4, col = "red", col.axis = "red", las = 1)

# use axis.POSIXct for POSIXct datetime objects
axis.POSIXct(1,bottom_axis)
#axis(1, pretty(range(bottom_axis),10))
mtext(bottom_axis_name, side = 1, col = "black", line = 2.5) 

legend("topleft", legend = c(left_axis_name, right_axis_name), text.col = c("blue", "red"), pch = c(16, 15), col = c("blue", "red"))

# If you'd rather use plotrix (doesn't support POSIXct):
#install.packages("plotrix")
#library(plotrix)
#twoord.plot(ly = left_axis, ry = right_axis, lx = bottom_axis, rx = bottom_axis, main = paste(left_axis_name, "and", right_axis_name, "vs", bottom_axis_name), ylab = left_axis_name, rylab = right_axis_name, xlab = bottom_axis_name)

# save to PDF
dev.copy(pdf, paste(launch_number, "/", launch_number, "_", title, ".pdf", sep = ""))
dev.off()