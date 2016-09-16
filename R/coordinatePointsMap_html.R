#' coordinatePointsMap_html
#'
#' Plots given longitudes and latitudes onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param latitude List of latitudes. Required.
#' @param longitude List of longitudes. Required.
#' @param data_frame Entire data frame. Required.
#' @param plot Whether to open the HTML in a web browser. Defaults to TRUE.
#' @param output_html_file Write HTML map to this filepath if not null. Defaults to NULL.
#' @examples
#' library(balloonDataAnaylsis)
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' coordinatePointsMap_html(tlm_data$Latitude, tlm_data$Longitude, tlm_data)

coordinatePointsMap_html <-
    function(latitude,
             longitude,
             data_frame,
             plot = TRUE,
             output_html_file = NULL)
    {
        requireNamespace("googleVis")

        # generate HTML tip data from data frame
        tip <- ""
        for (row in 1:nrow(data_frame))
        {
            for (col in 1:ncol(data_frame))
            {
                if (col == 1)
                {
                    tip[row] <-
                        paste(colnames(data_frame)[col],
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
        gvis_map <-
            googleVis::gvisMap(
                data.frame(paste(latitude, longitude, sep = ":"), tip),
                options = list(
                    showTip = TRUE,
                    showLine = TRUE,
                    enableScrollWheel = TRUE,
                    mapType = 'terrain',
                    useMapTypeControl = TRUE
                )
            )

        if (!is.null(output_html_file))
        {
            # open file with given filepath
            html_file = file(output_html_file, open = "wt")
            # get html string
            html_string <-
                paste(
                    c(
                        gvis_map$html$header,
                        paste(gvis_map$html$chart, collapse = ""),
                        gvis_map$html$caption,
                        gvis_map$html$footer
                    ),
                    collapse = "\n"
                )
            # write HTML to file
            write(html_string, file = html_file)
            close(html_file)
        }

        if (plot)
        {
            # render to plot viewer
            plot(gvis_map)
        }
    }
