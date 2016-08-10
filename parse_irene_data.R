parse_irene_data <- function(irene_data_file)
{
    # read from CSV format
    irene_data <- read.csv(irene_data_file)
    colnames(irene_data) <- c("Date", "Time", "Counts_Per_Minute", "Unit")
    
    # remove "Unit" column
    irene_data$Unit <- NULL
    
    # get Unix epoch timestamps (IRENE records in Zulu time to separate Date and Time columns, and in American date format)
    irene_data$Timestamp <- as.POSIXct(format(as.POSIXct(paste(irene_data$Date, irene_data$Time), format = "%m/%d/%y %H:%M:%S", tz = "Zulu"), tz = Sys.timezone()), tz = Sys.timezone())
    irene_data$Date <- NULL
    irene_data$Time <- NULL
    
    return(irene_data)
}