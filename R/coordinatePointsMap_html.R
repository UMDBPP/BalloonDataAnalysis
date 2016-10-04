#' Plot coordinate pairs onto Google Maps HTML
#'
#' Plots given longitudes and latitudes onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param data_frame Data frame that must at least contain the fields "Latitude" and "Longitude". Required.
#' @param api_key Google Maps API Key. Required if you want to host the HTML output on a server. Defaults to NULL.
#' @param plot Whether to open map in browser. Defaults to FALSE.
#' @param output_html_file Name of output file. Defaults to NULL for no file writing.
#' @export
#' @importFrom googleVis gvisMap
#' @examples
#' coordinatePointsMap_html(NS57_LINK_TLM)

coordinatePointsMap_html <-
    function(latitudes,
             longitudes,
             data_frame,
             api_key = NULL,
             plot = TRUE,
             output_html_file = NULL)
    {
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
                data.frame(paste(latitudes, longitudes, sep = ":"), tip),
                options = list(
                    apiKey = api_key,
                    showTip = TRUE,
                    enableScrollWheel = TRUE,
                    mapType = 'terrain',
                    useMapTypeControl = TRUE,
                    height = "100%",
                    width = "100%"
                )
            )

        if (!is.null(output_html_file))
        {
            if (tools::file_ext(output_html_file) != "html")
            {
                output_html_file <- paste(output_html_file, "html", sep = ".")
            }

            # open file
            html_file = file(output_html_file, open = "wt")

            # get html string
            html_string <-
                paste(
                    c(
                        gvis_map$html$header,
                        paste(gvis_map$html$chart, collapse = "")
                        # gvis_map$html$caption,
                        # gvis_map$html$footer
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
