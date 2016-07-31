# This script makes a map of a balloon track, assuming an input CSV file with rows of lat, long, and altitude
# Requires ggmap to be installed (install.packages("ggmap"))

library(ggmap)

data_file <- "NS57_parsedPackets.txt"

data <- read.csv(data_file)
# get base map using bounding box coordinates
map <- get_map(location = c(min(data$Lon), min(data$Lat), max(data$Lon), max(data$Lat)), zoom = 10, maptype = "terrain")
# plot points on map and render to plot viewer
ggmap(map) + geom_point(data = as.data.frame(cbind(lon = data$Lon,lat = data$Lat)), aes(x = lon, y = lat, fill = data$Alt.ft.), size = 3, shape = 21)