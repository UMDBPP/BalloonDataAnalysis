#' Read UMDBPP payload data from file
#'
#' This function will read and parse data University of Maryland Balloon Payload Program (UMDBPP) data logs.
#' It standardizes units and calculates ascent rate, ground speed, and downrange distance per entry.
#'
#' @param file Path to log file.
#' @param data_source Source from which data originated, from the list c("LINK-TLM", "CellTracker", "APRS", "IRENE").
#' @param flight_number Flight designation.
#' @param flight_date Date of flight in "YYYY-MM-DD" format. Defaults to NULL.
#' @param start_time Time in 24 hour "HH:MM:SS" format. If a time is given, dataset will be cut to after start time.
#' @param end_time Time in 24 hour "HH:MM:SS" format. If a time is given, dataset will be cut to before end time.
#' @param timezone Timezone of launch. Run OlsonNames for a list of accepted timezones.
#' @param callsign Callsign of data, in the case of single-callsign APRS data.
#' @return Returns data frame of parsed payload data, standardized to POSIXct timestamps and SI units.
#' @export
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stats complete.cases
#' @importFrom stats median
#' @importFrom jsonlite fromJSON
#' @importFrom geosphere distGeo
#' @examples
#' \dontrun{
#' NS57.Coord <- read.payload("NS57_parsedPackets.txt",
#'         data_source = "LINK-TLM",
#'         flight_number = "NS57",
#'         start_time = "07:50:31",
#'         end_time = "09:27:34"
#' )
#'
#' NS57.Rad <- read.payload("NS57LaunchData.txt",
#'         data_source = "IRENE",
#'         flight_number = "NS57",
#'         start_time = "07:50:31",
#'         end_time = "09:27:34"
#' )
#' }

read.payload <-
  function(file,
           data_source,
           flight_number = NULL,
           flight_date = NULL,
           start_time = NULL,
           end_time = NULL,
           timezone = Sys.timezone(),
           callsign = NULL)
  {
    if (!(data_source %in% c("LINK-TLM", "CellTracker", "APRS", "IRENE")))
    {
      stop("Parsing for that data source has not yet been written.")
    }

    # attempt to parse flight number if not given
    if (is.null(flight_number))
    {
      matched <- regmatches(file, regexpr("(?i)(NS[0-9]+)", file))
      if (length(matched) > 0)
      {
        flight_number <- toupper(matched)
      }
      else
      {
        stop("Could not parse flight number from filename. Please specify flight number.")
      }
    }

    if (data_source %in% c("LINK-TLM", "CellTracker", "APRS"))
    {
      requireNamespace("geosphere")
    }

    if (data_source == "LINK-TLM")
    {
      internal_timezone <- timezone

      file_extension <-
        tail(unlist(strsplit(file, "[.]")), n = 1)

      if (file_extension == "json")
      {
        requireNamespace("jsonlite")
        parsed_data <- fromJSON(file)
        parsed_data <- parsed_data[c(3:6, 9, 7, 8, 2)]
      }
      else if (file_extension == "txt")
      {
        # get string from file
        file_string <-
          readChar(file, file.info(file)$size)

        # remove startup lines from software restarts
        file_string <-
          gsub("(LOG BEGINNING ON .*)(\n\n)", "", file_string, perl = TRUE)

        # remove the log string from the end of each line
        file_string <-
          gsub("( < |,)(LOGGED AT )(.+)( >|,)(\n)",
               "\n",
               file_string,
               perl = TRUE)

        # remove extra header lines from some software restarts
        file_string <-
          gsub("(.*)(\r?\n\\1)+",
               "\\1\n",
               file_string)

        # remove extra newlines
        file_string <-
          gsub("(\r?\n)+",
               "\\1",
               file_string)

        # read data to local variable, assuming CSV format
        parsed_data <- read.csv(textConnection(file_string))

        # reorder columns
        parsed_data <- parsed_data[c(2:8, 1)]
      }

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

      # convert altitude from feet to meters
      parsed_data$Altitude_m <-
        parsed_data$Altitude_m * 0.3048

      # make sure callsigns are factors
      parsed_data$Callsign <- as.factor(parsed_data$Callsign)

      parsed_data$Downrange_Distance_m <- 0
      parsed_data$Ascent_Rate_m_s <- 0
      parsed_data$Ground_Speed_m_s <- 0

      # get Unix epoch timestamps
      parsed_data$DateTime <-
        as.POSIXct(parsed_data$DateTime,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = internal_timezone)
    }
    else if (data_source == "APRS")
    {
      internal_timezone <- timezone

      parsed_data <- read.csv(file, row.names = NULL)[1:4]

      # rename columns
      colnames(parsed_data) <- c("DateTime",
                                 "Latitude",
                                 "Longitude",
                                 "Altitude_m")

      # remove all rows containing 0 (no signal)
      parsed_data[parsed_data == 0] <- NA
      #parsed_data <-
      #  parsed_data[complete.cases(parsed_data), ]

      # convert to POSIXct timestamps
      parsed_data$DateTime <-
        as.POSIXct(format(as.POSIXct(parsed_data$DateTime, tz = internal_timezone), tz = Sys.timezone()))

      # remove duplicate packets
      for (value in unique(paste(parsed_data$Latitude, parsed_data$Longitude, parsed_data$Altitude_m)))
      {
        if (nrow(parsed_data[paste(parsed_data$Latitude, parsed_data$Longitude, parsed_data$Altitude_m) == value, c(2:4)]) > 1)
        {
          parsed_data[paste(parsed_data$Latitude, parsed_data$Longitude, parsed_data$Altitude_m) == value, c(2:4)][-1,] <- 0
        }

      }

      parsed_data[parsed_data[2] == 0 & parsed_data[3] == 0 & parsed_data[4] == 0, c(2:4)] <- NA
      parsed_data <- parsed_data[complete.cases(parsed_data), ]
    }
    else if (data_source == "CellTracker")
    {
      if (is.null(flight_date))
      {
        stop("Cell Tracker requires flight_date")
      }

      internal_timezone <- "Zulu"

      parsed_data <- read.csv(file, row.names = NULL)

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
      parsed_data <- read.csv(file)

      # remove empty lines
      parsed_data <- parsed_data[complete.cases(parsed_data), ]

      # rename columns
      colnames(parsed_data) <-
        c("Date", "Time", "Reading", "Unit")

      # get Unix epoch timestamps
      parsed_data$DateTime <-
        as.POSIXct(paste(parsed_data$Date, parsed_data$Time),
                   format = "%m/%d/%y %H:%M:%S",
                   tz = internal_timezone)
    }

    # change internal timezone to flight timezone
    attr(parsed_data$DateTime, "tzone") <- timezone

    # reorder rows by timestamp
    parsed_data <-
      parsed_data[order(parsed_data$DateTime), ]

    # extract flight date from timestamp if not given
    if (is.null(flight_date))
    {
      flight_date <- as.Date(parsed_data$DateTime[1])
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
                      paste(flight_date, start_time), tz = timezone
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
                      paste(flight_date, end_time), tz = timezone
                    )),
             n = 1)
    }

    if (!is.null(start_time) | !is.null(end_time))
    {
      # cut to flight time
      parsed_data <-
        subset(
          parsed_data,
          subset = parsed_data$DateTime >= launch_data$DateTime &
            parsed_data$DateTime <= landing_data$DateTime
        )
    }

    if (data_source == "LINK-TLM")
    {
      for (source_name in c(levels(parsed_data$Callsign)))
      {
        source_log <-
          subset(parsed_data, subset = parsed_data$Callsign == source_name)

        # calculate downrange distance as distance from launch coordinates
        source_log$Downrange_Distance_m <-
          geosphere::distGeo(c(launch_data$Longitude, launch_data$Latitude),
                             as.matrix(source_log[c("Longitude", "Latitude")]))

        # calculate ascent rates
        source_log$Ascent_Rate_m_s <-
          .rates(source_log$Altitude_m,
                 source_log$DateTime)

        # calculate ground speeds
        source_log$Ground_Speed_m_s <-
          geosphere::distGeo(as.matrix(source_log[c("Longitude", "Latitude")]),
                             as.matrix(source_log[c(1, 1:(nrow(source_log) - 1)), c("Longitude", "Latitude")])) /
          as.numeric(source_log$DateTime - source_log$DateTime[c(1, 1:(nrow(source_log) - 1))],
                     units = "secs")

        parsed_data[parsed_data$Callsign == source_name,] <-
          source_log
      }
    }
    else if (data_source == "CellTracker" |
             data_source == "APRS")
    {
      # calculate downrange distance as distance from launch coordinates
      parsed_data$Downrange_Distance_m <-
        geosphere::distGeo(c(launch_data$Longitude, launch_data$Latitude),
                           as.matrix(parsed_data[c("Longitude", "Latitude")]))

      # calculate ascent rates
      parsed_data$Ascent_Rate_m_s <-
        .rates(parsed_data$Altitude_m, parsed_data$DateTime)

      # calculate ground speeds
      parsed_data$Ground_Speed_m_s <-
        geosphere::distGeo(as.matrix(parsed_data[c("Longitude", "Latitude")]),
                           as.matrix(parsed_data[c(1, 1:(nrow(parsed_data) - 1)), c("Longitude", "Latitude")])) /
        as.numeric(parsed_data$DateTime - parsed_data$DateTime[c(1, 1:(nrow(parsed_data) - 1))], units = "secs")

      # reorder columns
      if (data_source == "CellTracker")
      {
        parsed_data <- parsed_data[c(1:4, 6:8, 5)]
      }
    }
    else if (data_source == "IRENE")
    {
      # remove Date and Time columns
      parsed_data$Date <- NULL
      parsed_data$Time <- NULL

      # reorder columns
      parsed_data <- parsed_data[c(3, 1, 2)]
    }

    # add callsign column
    if (!is.null(callsign))
    {
      parsed_data$Callsign <- callsign
    }

    # add data source column
    parsed_data$Data_Source <- data_source

    # add data source column
    parsed_data$Flight <- flight_number

    # replace NaN values with 0
    parsed_data[is.na(parsed_data)] <- 0

    return(parsed_data)
  }

.rates <- function(positions, times)
{
  output_rates = (positions - positions[c(1, 1:(length(positions) - 1))]) /
    as.numeric(times - times[c(1, 1:(length(times) - 1))], units = "secs")
  output_rates[is.nan(output_rates)] <- 0
  return(output_rates)
}
