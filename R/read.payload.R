#' Read UMDBPP payload data from file
#'
#' This function will read and parse data University of Maryland Balloon Payload Program (UMDBPP) data logs.
#' It standardizes units and calculates ascent rate, ground speed, and downrange distance per entry.
#'
#' @param logfile Path to CSV log file.
#' @param data_source Source from which data originated, from the list c("LINK-TLM", "CellTracker", "IRENE").
#' @param flight_number Flight designation.
#' @param flight_date Date of flight in "YYYY-MM-DD" format. Defaults to NULL.
#' @param start_time Time in 24 hour "HH:MM:SS" format. If a time is given, dataset will be cut to after start time.
#' @param end_time Time in 24 hour "HH:MM:SS" format. If a time is given, dataset will be cut to before end time.
#' @param timezone Timezone of launch. Run OlsonNames for a list of accepted timezones.
#' @return Returns data frame of parsed payload data, standardized to POSIXct timestamps and SI units.
#' @export
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stats complete.cases
#' @examples
#' \dontrun{
#' NS57.Coord <- read.payload("NS57_parsedPackets.txt", "LINK-TLM", "NS57", start_time = "07:50:31", end_time = "09:27:34")
#' NS57.Rad <- read.payload("NS57LaunchData.txt", "IRENE", "NS57", start_time = "07:50:31", end_time = "09:27:34")
#' }

read.payload <-
    function(logfile,
             data_source,
             flight_number,
             flight_date = NULL,
             start_time = NULL,
             end_time = NULL,
             timezone = Sys.timezone())
    {
        if (!(data_source %in% c("LINK-TLM", "CellTracker", "IRENE")))
        {
            stop("Parsing for that data source has not yet been written.")
        }

        if (data_source == "LINK-TLM")
        {
            internal_timezone <- timezone

            # get string from file
            file_string <-
                readChar(logfile, file.info(logfile)$size)

            # remove startup lines from software restarts
            file_string <-
                gsub("LOG BEGINNING ON.*?>\n", "", file_string)

            # remove extra header lines from some software restarts
            file_string <-
                gsub("Callsign,*?>\nCallsign,",
                     "Callsign,",
                     file_string)

            # remove the log string from the end of each line
            file_string <- gsub("  <.*?>", "", file_string)

            # read data to local variable, assuming CSV format
            parsed_data <- read.csv(textConnection(file_string))

            # reorder columns
            parsed_data <- parsed_data[c(2:8, 1)]

            # rename columns
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

            # get Unix epoch timestamps
            parsed_data$DateTime <-
                as.POSIXct(parsed_data$DateTime, tz = internal_timezone)
        }
        else if (data_source == "CellTracker")
        {
            if (is.null(flight_date))
            {
                stop("Cell Tracker requires flight_date")
            }

            internal_timezone <- "Zulu"

            parsed_data <- read.csv(logfile, row.names = NULL)

            # check for degree-minute-second coordinate format and convert to decimal coordinates
            if (ncol(parsed_data) == 9)
            {
                parsed_data$Latitude <-
                    parsed_data[[2]] + (parsed_data[[3]] / 60) + (parsed_data[[4]] / 60 / 60)
                parsed_data$Longitude <-
                    parsed_data[[5]] + (parsed_data[[6]] / 60) + (parsed_data[[7]] / 60 / 60)
                parsed_data <- parsed_data[c(1, 10, 11, 8, 9)]
            }

            # rename columns
            colnames(parsed_data) <- c("DateTime",
                                       "Latitude",
                                       "Longitude",
                                       "Altitude_m",
                                       "Signal_Strength")

            # remove all rows containing 0 (no signal)
            parsed_data[parsed_data == 0] <- NA
            parsed_data <-
                parsed_data[complete.cases(parsed_data), ]

            # convert to POSIXct timestamps
            parsed_data$DateTime <-
                as.POSIXct(paste(flight_date, parsed_data$DateTime),
                           "%Y-%m-%d %T",
                           tz = internal_timezone)
        }
        else if (data_source == "IRENE")
        {
            # IRENE records in Zulu time in American date format, and to separate Date and Time columns
            internal_timezone = "Zulu"

            # read from CSV format
            parsed_data <- read.csv(logfile)

            # rename columns
            colnames(parsed_data) <-
                c("Date", "Time", "Reading", "Unit")

            # get Unix epoch timestamps
            parsed_data$DateTime <-
                as.POSIXct(
                    paste(parsed_data$Date, parsed_data$Time),
                    format = "%m/%d/%y %H:%M:%S",
                    tz = internal_timezone
                )
        }

        # change internal timezone to flight timezone
        attr(parsed_data$DateTime, "tzone") <- timezone

        # reorder rows by timestamp
        parsed_data <-
            parsed_data[order(parsed_data$DateTime), ]

        # extract flight date from timestamp if not given
        if (is.null(flight_date))
        {
            flight_date <- as.Date(median(parsed_data$DateTime))
        }

        # if start and / or end times are not given, use first and / or last entries respectively
        if (is.null(start_time))
        {
            launch_data <- head(parsed_data, n = 1)
        }
        else
        {
            launch_data <-
                tail(subset(parsed_data,
                            subset = parsed_data$DateTime <= as.POSIXct(
                                paste(flight_date, start_time, timezone)
                            )),
                     n = 1)
        }

        if (is.null(end_time))
        {
            landing_data <- tail(parsed_data, n = 1)
        }
        else
        {
            landing_data <-
                head(subset(parsed_data,
                            subset = parsed_data$DateTime >= as.POSIXct(
                                paste(flight_date, end_time, timezone)
                            )),
                     n = 1)
        }

        # cut to flight time
        parsed_data <-
            subset(
                parsed_data,
                subset = parsed_data$DateTime >= launch_data$DateTime &
                    parsed_data$DateTime <= landing_data$DateTime
            )

        if (data_source == "LINK-TLM")
        {
            for (source_name in c(levels(parsed_data$Callsign)))
            {
                source_log <-
                    subset(parsed_data, subset = parsed_data$Callsign == source_name)

                # calculate downrange distance as distance from launch coordinates
                source_log$Downrange_Distance_m <-
                    .distances(
                        launch_data$Latitude,
                        launch_data$Longitude,
                        source_log$Latitude,
                        source_log$Longitude
                    )

                # convert altitude from feet to meters
                source_log$Altitude_m <-
                    source_log$Altitude_m * 0.3048

                # calculate ascent rates
                source_log$Ascent_Rate_m_s <-
                    .rates(source_log$Altitude_m,
                           source_log$DateTime)

                # calculate ground speeds
                source_log$Ground_Speed_m_s <-
                    abs(.rates(
                        .distances(
                            source_log$Latitude,
                            source_log$Longitude,
                            source_log$Latitude[c(2:nrow(source_log), nrow(source_log))],
                            source_log$Longitude[c(2:nrow(source_log), nrow(source_log))]
                        ),
                        source_log$DateTime
                    ))

                parsed_data[which(parsed_data$Callsign == source_name),] <-
                    source_log
            }
        }
        else if (data_source == "CellTracker")
        {
            # calculate downrange distance as distance from launch coordinates
            parsed_data$Downrange_Distance_m <-
                .distances(
                    launch_data$Latitude,
                    launch_data$Longitude,
                    parsed_data$Latitude,
                    parsed_data$Longitude
                )

            # convert altitude from feet to meters
            parsed_data$Altitude_m <-
                parsed_data$Altitude_m * 0.3048

            # calculate ascent rates
            parsed_data$Ascent_Rate_m_s <-
                .rates(parsed_data$Altitude_m, parsed_data$DateTime)

            # calculate ground speeds
            parsed_data$Ground_Speed_m_s <-
                abs(.rates(
                    .distances(
                        parsed_data$Latitude,
                        parsed_data$Longitude,
                        parsed_data$Latitude[c(2:nrow(parsed_data), nrow(parsed_data))],
                        parsed_data$Longitude[c(2:nrow(parsed_data), nrow(parsed_data))]
                    ),
                    parsed_data$DateTime
                ))

            # reorder columns
            parsed_data <- parsed_data[c(1:4, 6:8, 5)]
        }
        else if (data_source == "IRENE")
        {
            # remove Date and Time columns
            parsed_data$Date <- NULL
            parsed_data$Time <- NULL

            # reorder columns
            parsed_data <- parsed_data[c(3, 1, 2)]
        }

        # add data source column
        parsed_data$Data_Source <- data_source

        # add data source column
        parsed_data$Flight <- flight_number

        return(parsed_data)
    }

# define function to get distance between two coordinates
.distances <- function(lat_1, lon_1, lat_2, lon_2)
{
    R <- 6378137
    a <-
        sin(((lat_2 - lat_1) * pi / 180) / 2) ^ 2 +
        cos(lat_1 * pi / 180) * cos(lat_2 * pi / 180) * sin(((lon_2 - lon_1) * pi / 180) / 2) ^ 2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))

    return(R * c)
}

.rates <- function(positions, times)
{
    output <-
        c(0,
          (positions[1:(length(positions) - 1)] - positions[2:length(positions)]) / as.numeric(times[1:(length(positions) - 1)] - times[2:length(times)], units = "secs"))

    return(output)
}
