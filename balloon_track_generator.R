# This script makes a map of a balloon track, assuming an input CSV file with rows of lat, long, and altitude
# Requires ggmap to be installed (install.packages("ggmap"))

library(ggmap)

launch <- "NS57"
data_file <- paste(launch, "/", launch, "_parsedPackets.txt", sep = "")

# 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map
zoom_level <- 10

# read csv file
data <- read.csv(data_file)

# get underlying terrain map using mean coordinates
map <- get_map(location = c(lon = mean(data$Lon), lat = mean(data$Lat)), zoom = zoom_level, maptype = "terrain")

# plot points onto map and render to plot viewer
ggmap(map) + geom_point(data = as.data.frame(cbind(lon = data$Lon,lat = data$Lat)), aes(x = lon, y = lat, fill = data$Alt.ft.), size = 3, shape = 21)

# save plot image to PNG file (NSXX_map.png)
dev.copy(png, paste(launch, "map.png", sep = "_"))
dev.off()
