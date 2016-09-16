#' parsePayloadData
#'
#' Parses and cleans data from IRENE given input data file.
#' @param data_file Data file in CSV format.
#' @param data_source Source of data, can be LINK-TLM or IRENE
#' @param launch_timezone Timezone of launch. Run OlsonNames for a list of timezones. Defaults to system timezone.
#' @keywords
#' @export
#' @examples
#' library(balloonDataAnaylsis)
#' tlm_data <- parsePayloadData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- parsePayloadData("NS57LaunchData.txt", "IRENE")

parsePayloadData <-
    function(data_file,
             data_source,
             launch_timezone = Sys.timezone())
    {
        requireNamespace("measurements")

        if (data_source == "LINK-TLM")
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
        else if (data_source == "IRENE")
        {
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
        else
        {
            stop("Data source not recognized.")
        }
        return(parsed_data)
    }
