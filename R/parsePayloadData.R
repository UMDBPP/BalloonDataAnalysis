#' Parse UMDBPP payload data from file
#'
#' Parses and cleans data from given data file, assuming format of given payload.
#' @param data_file Data file in CSV format.
#' @param payload Payload from which data originated. Defaults to "LINK-TLM".
#' @param launch_timezone Timezone of launch. Run OlsonNames for a list of timezones. Defaults to system timezone.
#' @return Returns data frame of parsed payload data, standardized to POSIXct timestamps and SI units.
#' @export
#' @importFrom measurements conv_unit
#' @examples
#' tlm_data <- parsePayloadData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- parsePayloadData("NS57LaunchData.txt", "IRENE")

parsePayloadData <-
    function(data_file,
             payload = c("LINK-TLM", "IRENE", "CellTracker"),
             launch_timezone = Sys.timezone())
    {
        if (payload == "LINK-TLM")
        {
            # read from CSV format
            parsed_data <- read.csv(data_file)
            colnames(parsed_data) <-
                c(
                    "Callsign",
                    "Timestamp",
                    "Latitude",
                    "Longitude",
                    "Altitude_m",
                    "Downrange_Distance_m",
                    "Ascent_Rate_m_s",
                    "Ground_Speed_m_s",
                    "Log"
                )

            # remove "Log" column
            parsed_data$Log <- NULL

            # convert to meters
            parsed_data$Altitude_m <-
                measurements::conv_unit(parsed_data$Altitude_m, "ft", "m")
            parsed_data$Downrange_Distance_m <-
                measurements::conv_unit(parsed_data$Downrange_Distance_m, "mi", "m")
            parsed_data$Ascent_Rate_m_s <-
                measurements::conv_unit(parsed_data$Ascent_Rate_m_s,
                                        "ft_per_sec",
                                        "m_per_sec")
            parsed_data$Ground_Speed_m_s <-
                measurements::conv_unit(parsed_data$Ground_Speed_m_s, "mph", "m_per_sec")

            # get Unix epoch timestamps
            parsed_data$Timestamp <-
                as.POSIXct(format(
                    as.POSIXct(parsed_data$Timestamp, tz = launch_timezone),
                    tz = Sys.timezone()
                ), tz = Sys.timezone())
        }
        else if (payload == "IRENE")
        {
            internal_timezone = "Zulu"

            # read from CSV format
            parsed_data <- read.csv(data_file)
            colnames(parsed_data) <-
                c("Date", "Time", "Counts_Per_Minute", "Unit")

            # remove "Unit" column
            parsed_data$Unit <- NULL

            # get Unix epoch timestamps (IRENE records in Zulu time to separate Date and Time columns, and in American date format)
            parsed_data$Timestamp <-
                as.POSIXct(format(
                    as.POSIXct(
                        paste(parsed_data$Date, parsed_data$Time),
                        format = "%m/%d/%y %H:%M:%S",
                        tz = "Zulu"
                    ),
                    tz = Sys.timezone()
                ), tz = Sys.timezone())
            parsed_data$Date <- NULL
            parsed_data$Time <- NULL
        }
        else if (payload == "CellTracker")
        {
            internal_timezone <- "GMT"

            parsed_data <-
                read.csv(
                    data_file,
                    col.names = c(
                        "Timestamp",
                        "Latitude",
                        "Longtitude",
                        "Altitude_m",
                        "Signal_Strength"
                    )
                )

            # convert to POSIXct timestamps
            parsed_data$Timestamp <-
                as.POSIXct(format(as.POSIXct(
                    paste("2016-09-17", parsed_data$Timestamp),
                    "%Y-%m-%d %T" ,
                    tz = internal_timezone
                ),
                tz = Sys.timezone()), tz = Sys.timezone())

            # remove rows with NA timestamps
            parsed_data <-
                parsed_data[complete.cases(parsed_data), ]

            # remove rows with 0 data which infers no signal
            parsed_data <-
                parsed_data[apply(parsed_data[c(2:5)], 1, function(z)
                    ! any(z == 0)), ]
        }
        else
        {
            stop("Parsing for that payload is not written yet.")
        }
        return(parsed_data)
    }
