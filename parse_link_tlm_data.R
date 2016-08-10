parse_link_tlm_data <- function(link_tlm_data_file, launch_timezone = Sys.timezone()) # run OlsonNames() for a list of timezones
{
    link_tlm_data_file <- paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = "")
    
    # read from CSV format
    link_tlm_data <- read.csv(link_link_tlm_data_file)
    colnames(link_tlm_data) <- c("Callsign", "Timestamp", "Latitude", "Longitude", "Altitude_m", "Downrange_Distance_m", "Ascent_Rate_m_s", "Ground_Speed_m_s", "Log")
    
    # remove "Log" column
    link_tlm_data$Log <- NULL
    
    # convert altitude to meters
    link_tlm_data$Altitude_m <- conv_unit(link_tlm_data$Altitude_m, "ft", "m")
    
    # get Unix epoch timestamps
    link_tlm_data$Timestamp <- as.POSIXct(format(as.POSIXct(link_tlm_data$Timestamp, tz = launch_timezone), tz = Sys.timezone()), tz = Sys.timezone())
    
    return(link_tlm_data)
}