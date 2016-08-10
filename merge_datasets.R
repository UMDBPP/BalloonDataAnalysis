# This script merges datasets, specifically from IRENE and LINK-TLM and creates a new "merged_data" data frame

#install.packages("measurements")

library(measurements)


# define launch number
launch_number <- "NS57"
# define launch timezone (get a list of possible timezones by running OlsonNames())
launch_timezone <- Sys.timezone()
# define filenames
tlm_file <- paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = "")
irene_file <- paste(launch_number, "/", launch_number, "LaunchData", ".txt", sep = "")


# read LINK-TLM data from CSV format
tlm_data <- read.csv(tlm_file)
colnames(tlm_data) <- c("Callsign", "Timestamp", "Latitude", "Longitude", "Altitude_m", "Downrange_Distance_m", "Ascent_Rate_m_s", "Ground_Speed_m_s", "Log")
# remove log column from LINK-TLM data
tlm_data$Log <- NULL
# convert altitude to meters
tlm_data$Altitude_m <- conv_unit(tlm_data$Altitude_m, "ft", "m")
# get Unix epoch timestamps for LINK-TLM
tlm_data$Timestamp <- as.POSIXct(format(as.POSIXct(tlm_data$Timestamp, tz = launch_timezone), tz = Sys.timezone()), tz = Sys.timezone())


# read IRENE data from CSV format
irene_data <- read.csv(irene_file)
colnames(irene_data) <- c("Date", "Time", "Counts_Per_Minute", "Unit")
# remove unit column from IRENE data
irene_data$Unit <- NULL
# get Unix epoch timestamps for IRENE (which records in Zulu time)
irene_data$Timestamp <- as.POSIXct(format(as.POSIXct(paste(irene_data$Date, irene_data$Time), format = "%m/%d/%y %H:%M:%S", tz = "Zulu"), tz = Sys.timezone()), tz = Sys.timezone())
irene_data$Date <- NULL
irene_data$Time <- NULL


# join tables (inner join with POSIX timestamp as key)
joined_data <- merge(x = tlm_data, y = irene_data, by = c("Timestamp"), all = TRUE)
# make a copy of joined data that we will now merge (outer join)
merged_data <- joined_data


# add IRENE data to LINK-TLM rows
# TODO find a better way to interpolate data than "nearest past neighbor"
while(anyNA(merged_data$Counts_Per_Minute))
{
    merged_data$Counts_Per_Minute[is.na(merged_data$Counts_Per_Minute)] = merged_data$Counts_Per_Minute[which(is.na(merged_data$Counts_Per_Minute)) - 1]
}
# remove non TLM rows (complete left outer join) and write to CSV
merged_data <- subset(merged_data,!(is.na(Callsign)))
write.csv(merged_data, paste(launch_number, "/", launch_number, "_outer_join", ".txt", sep = ""), row.names=FALSE)
