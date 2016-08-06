# This script makes a map of a balloon track, assuming an input CSV file with rows of lat, long, and altitude
# Requires ggmap to be installed (install.packages("ggmap"))

library(ggmap)

# define launch number
launch_number <- "NS57"

# define launch timezone (get a list of possible timezones by running OlsonNames())
launch_timezone <- Sys.timezone()

# set to element_blank() for none
map_title <- "Altitude (feet)"

# define output filename
output_file_name <- "map"

# define filename
tlm_file <- paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = "")

# 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map
zoom_level <- 10

# read csv file
tlm_data <- read.csv(tlm_file)
colnames(tlm_data)[9] <- "Log"
tlm_data$Log <- NULL

# get Unix epoch timestamps
tlm_data$Timestamp <- as.POSIXct(format(as.POSIXct(tlm_data$Timestamp, tz = launch_timezone), tz = Sys.timezone()), tz = Sys.timezone())

# get underlying terrain map using mean coordinates
map <- get_map(location = c(lon = mean(tlm_data$Lon), lat = mean(tlm_data$Lat)), zoom = zoom_level, maptype = "terrain")

# render to plot viewer
ggmap(map) + geom_point(data = as.data.frame(cbind(lon = tlm_data$Lon,lat = tlm_data$Lat)), aes(x = lon, y = lat, colour = tlm_data$Alt.ft.), size = 2) + labs(x="Longitude", y="Latitude", title=map_title) + theme(legend.title=element_blank())

# save to PDF
dev.copy(pdf, paste(launch_number, "/", launch_number,"_", output_file_name, ".pdf", sep = ""))
dev.off()
