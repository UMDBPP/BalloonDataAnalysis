# This script makes a map of a balloon track, assuming an input CSV file with rows of lat, long, and altitude
# Requires package "ggmap" to be installed (install.packages("ggmap"))

launch_number <- "NS57"
launch_timezone <- Sys.timezone() # get a list of possible timezones by running OlsonNames()
map_title <- "Altitude (feet)" # set to element_blank() for none
output_file_name <- "map"
input_filename <- paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = "")
zoom_level <- 10 # 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map

################################################################################

#install.packages("ggmap")
require(ggmap)

# read csv file
tlm_data <- read.csv(input_filename)

# get Unix epoch timestamps
tlm_data$Timestamp <- as.POSIXct(format(as.POSIXct(tlm_data$Timestamp, tz = launch_timezone), tz = Sys.timezone()), tz = Sys.timezone())

# get underlying terrain map using mean coordinates
map <- get_map(location = c(lon = mean(tlm_data$Lon), lat = mean(tlm_data$Lat)), zoom = zoom_level, maptype = "terrain")

# render to plot viewer
ggmap(map) + geom_point(data = as.data.frame(cbind(lon = tlm_data$Lon,lat = tlm_data$Lat)), aes(x = lon, y = lat, colour = tlm_data$Altitude_m), size = 2) + labs(x="Longitude", y="Latitude", title=map_title) + theme(legend.title=element_blank())

# save to PDF
dev.copy(pdf, paste(launch_number, "/", launch_number,"_", output_file_name, ".pdf", sep = ""))
dev.off()
