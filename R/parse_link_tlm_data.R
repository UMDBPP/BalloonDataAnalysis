#' Parse Link-TLM Data
#'
#' Parses and cleans data from Link-TLM given input data file.
#' @param irene_data_file Data file from Link-TLM
#' @param launch_timezone Timezone of launch site. Run OlsonNames() for a list of timezone names. Defaults to system timezone.
#' @keywords
#' @export
#' @examples
#' parse_link_tlm_data("NS57_parsedPackets.txt")

parse_link_tlm_data <-
    function(link_tlm_data_file, launch_timezone = Sys.timezone())
    {
        require(measurements)

        # read from CSV format
        link_tlm_data <- read.csv(link_tlm_data_file)
        colnames(link_tlm_data) <-
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
        link_tlm_data$Log <- NULL

        # convert to meters
        link_tlm_data$Altitude_m <-
            conv_unit(link_tlm_data$Altitude_m, "ft", "m")
        link_tlm_data$Downrange_Distance_m <-
            conv_unit(link_tlm_data$Downrange_Distance_m, "mi", "m")
        link_tlm_data$Ascent_Rate_m_s <-
            conv_unit(link_tlm_data$Ascent_Rate_m_s, "ft_per_sec", "m_per_sec")
        link_tlm_data$Ground_Speed_m_s <-
            conv_unit(link_tlm_data$Ground_Speed_m_s, "mph", "m_per_sec")

        # get Unix epoch timestamps
        link_tlm_data$Timestamp <-
            as.POSIXct(format(
                as.POSIXct(link_tlm_data$Timestamp, tz = launch_timezone),
                tz = Sys.timezone()
            ), tz = Sys.timezone())

        return(link_tlm_data)
    }
