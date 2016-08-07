# This script merges datasets, specifically from IRENE and LINK-TLM and creates a new "joined_data" data frame

# define launch number
launch_number <- "NS57"

# define launch timezone (get a list of possible timezones by running OlsonNames())
launch_timezone <- Sys.timezone()

# define filenames
tlm_file <- paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = "")
irene_file <- paste(launch_number, "/", launch_number, "LaunchData", ".txt", sep = "")

# read IRENE data from CSV format
irene_data <- read.csv(irene_file)
colnames(irene_data) <- c("Date", "Time", "Counts_Per_Minute", "Unit")

# remove unit column from IRENE data
irene_data$Unit <- NULL

# read LINK-TLM data from CSV format
tlm_data <- read.csv(tlm_file)

# get Unix epoch timestamps for LINK-TLM
tlm_data$Timestamp <- as.POSIXct(format(as.POSIXct(tlm_data$Timestamp, tz = launch_timezone), tz = Sys.timezone()), tz = Sys.timezone())

# get Unix epoch timestamps for IRENE (which records in Zulu time)
irene_data$Timestamp <- as.POSIXct(format(as.POSIXct(paste(irene_data$Date, irene_data$Time), format = "%m/%d/%y %H:%M:%S", tz = "Zulu"), tz = Sys.timezone()), tz = Sys.timezone())
irene_data$Date <- NULL
irene_data$Time <- NULL

# join tables (inner join with POSIX timestamp as key)
joined_data <- merge(x = tlm_data, y = irene_data, by = c("Timestamp"), all = TRUE)

# add IRENE data to LINK-TLM rows
# TODO find a better way to interpolate data than "nearest past neighbor"
while(anyNA(joined_data$Counts_Per_Minute))
{
    joined_data$Counts_Per_Minute[is.na(joined_data$Counts_Per_Minute)] = joined_data$Counts_Per_Minute[which(is.na(joined_data$Counts_Per_Minute)) - 1]
}

# remove IRENE rows
joined_data <- subset(joined_data,!(is.na(Callsign)))

write.csv(joined_data, paste(launch_number, "/", launch_number, "_tlm_irene_merged", ".txt", sep = ""), row.names=FALSE)
