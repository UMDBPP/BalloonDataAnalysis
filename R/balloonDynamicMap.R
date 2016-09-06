#' balloonDynamicMap
#'
#' Plots given longitudes and latitudes onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param longitude List of longitudes. Required.
#' @param latitude List of latitudes. Required.
#' @param data_frame Entire data frame. Required.
#' @examples
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' balloonDynamicMap(tlm_data$Longitude, tlm_data$Latitude, tlm_data)

balloonDynamicMap <- function(longitude, latitude, data_frame)
{
    requireNamespace("googleVis")

    tip <- ""

    for (row in 1:nrow(data_frame))
    {
        for (col in 1:ncol(data_frame))
        {
            if (col == 1)
            {
                tip[row] <-
                    paste(tip[row],
                          colnames(data_frame)[col],
                          " = ",
                          data_frame[row, col],
                          sep = "")
            }
            else
            {
                tip[row] <-
                    paste(tip[row],
                          "<BR>",
                          colnames(data_frame)[col],
                          " = ",
                          data_frame[row, col],
                          sep = "")
            }
        }
    }

    # call google visualzation engine for mapping. Returns HTML code for an interactive map.
    map <-
        googleVis::gvisMap(data.frame(paste(latitude, longitude, sep = ":"), tip))

    # render to plot viewer
    plot(map)
}
