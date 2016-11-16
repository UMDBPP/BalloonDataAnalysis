#' Parse UMDBPP payload data from file
#'
#' Parses and cleans data from given data file, assuming format of given payload.
#' @param data_file Data file in CSV format.
#' @param payload Payload from which data originated. Defaults to "LINK-TLM".
#' @param launch_date Date of launch in YYYY-MM-DD format. Defaults to NULL.
#' @param launch_timezone Timezone of launch. Run OlsonNames for a list of timezones. Defaults to system timezone.
#' @return Returns data frame of parsed payload data, standardized to POSIXct timestamps and SI units.
#' @export
#' @importFrom measurements conv_unit
#' @examples
#' \dontrun{
#' tlm_data <- parsePayloadData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- parsePayloadData("NS57LaunchData.txt", "IRENE")
#' }

parsePayloadData <-
    function(data_file,
             payload = c("LINK-TLM", "IRENE", "CellTracker"),
             launch_date = NULL,
             launch_timezone = Sys.timezone())
    {
        if (payload == "LINK-TLM")
        {
            internal_timezone = launch_timezone

            # get string from file
            file_string <-
                readChar(data_file, file.info(data_file)$size)

            # remove lines from software restarts
            file_string <-
                gsub("LOG BEGINNING ON.*?>\n", "", file_string)

            # remove log strings at ends of lines
            file_string <- gsub("  <.*?>", "", file_string)

            # read from CSV format
            parsed_data <- read.csv(textConnection(file_string))

            # reorder columns to put timestamp first and callsign last
            parsed_data <- parsed_data[c(2, 3, 4, 5, 6, 7, 8, 1)]

            colnames(parsed_data) <-
                c(
                    "DateTime",
                    "Latitude",
                    "Longitude",
                    "Altitude_m",
                    "Downrange_Distance_m",
                    "Ascent_Rate_m_s",
                    "Ground_Speed_m_s",
                    "Callsign"
                )

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
            parsed_data$DateTime <-
                as.POSIXct(parsed_data$DateTime, tz = internal_timezone)
            attr(parsed_data$DateTime, "tzone") <- launch_timezone

            # replace 0 with NA
            parsed_data[parsed_data == 0] <- NA
        }
        else if (payload == "IRENE")
        {
            internal_timezone = "Zulu"

            # read from CSV format
            parsed_data <- read.csv(data_file)
            colnames(parsed_data) <-
                c("Date", "Time", "Reading", "Unit")

            # get Unix epoch timestamps (IRENE records in Zulu time to separate Date and Time columns, and in American date format)
            parsed_data$DateTime <-
                as.POSIXct(
                    paste(parsed_data$Date, parsed_data$Time),
                    format = "%m/%d/%y %H:%M:%S",
                    tz = internal_timezone
                )
            attr(parsed_data$DateTime, "tzone") <- launch_timezone

            parsed_data$Date <- NULL
            parsed_data$Time <- NULL

            # reorder columns to put timestamp first
            parsed_data <- parsed_data[c(3, 1, 2)]

            # replace 0 with NA
            parsed_data[parsed_data == 0] <- NA
        }
        else if (payload == "CellTracker")
        {
            if (is.null(launch_date))
            {
                stop("Cell Tracker requires launch date.")
            }

            internal_timezone <- "Zulu"

            parsed_data <-
                read.csv(
                    data_file,
                    col.names = c(
                        "DateTime",
                        "Latitude",
                        "Longitude",
                        "Altitude_m",
                        "Signal_Strength"
                    )
                )

            # convert to POSIXct timestamps
            parsed_data$DateTime <-
                as.POSIXct(paste(launch_date, parsed_data$DateTime),
                           "%Y-%m-%d %T",
                           tz = internal_timezone)
            attr(parsed_data$DateTime, "tzone") <- launch_timezone

            # remove rows with NA timestamps
            parsed_data <-
                parsed_data[complete.cases(parsed_data), ]

            # remove all rows containing 0
            parsed_data[parsed_data == 0] <- NA
            parsed_data <- parsed_data[complete.cases(parsed_data), ]
        }
        else
        {
            stop("Parsing for that payload is not written yet.")
        }
        return(parsed_data)
    }
