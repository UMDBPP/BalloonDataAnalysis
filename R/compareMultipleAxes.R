#' compareMultipleAxes
#'
#' Plots two series with different units that share a common key.
#' Works best with interpolated data; balloonMergeDatasets(y1, y2, interpolate = TRUE)
#' Inspired by http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param x Shared x data (key).
#' @param y1 First series.
#' @param y2 Second series.
#' @param domain Defaults to domain of x axis.
#' @param x_name Defaults to "x".
#' @param x_unit Defaults to NULL.
#' @param y1_name Defaults to "y1".
#' @param y1_unit Defaults to NULL.
#' @param y1_color Defaults to "red".
#' @param y2_name Defaults to "y2".
#' @param y2_unit Defaults to NULL.
#' @param y2_color Defaults to "blue".
#' @param title Title of plot. Defaults to "y1_name (y1_unit) and y2_name (y2_unit) vs x_name (x_unit)".
#' @param add_legend Defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' library(balloonDataAnaylsis)
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- balloonParseData("NS57LaunchData.txt", "IRENE")
#' joined_data <- balloonMergeDatasets(tlm_data, irene_data, interpolate = TRUE)
#' compareMultipleAxes(
#'     joined_data$Timestamp,
#'     joined_data$Counts_Per_Minute,
#'     joined_data$Altitude_m,
#'     domain = c(min(tlm_data$Timestamp), max(tlm_data$Timestamp)),
#'     x_name = "Time",
#'     x_unit = "24hr",
#'     y1_name = "Geiger Counter",
#'     y1_unit = "counts per minute",
#'     y2_name = "Altitude",
#'     y2_unit = "meters",
#'     add_legend = TRUE
#' )

compareMultipleAxes <-
    function(x,
             y1,
             y2,
             domain = NULL,
             x_name = "x",
             x_unit = NULL,
             y1_name = "y1",
             y1_unit = NULL,
             y1_color = "red",
             y2_name = "y2",
             y2_unit = NULL,
             y2_color = "blue",
             title = NULL,
             add_legend = FALSE)
    {
        # if domain was not given, use domain of x
        if (is.null(domain))
        {
            domain <- c(min(x), max(x))
        }

        # if title was not given, use "y1_name (y1_unit) and y2_name (y2_unit) vs x_name (x_unit)"
        if (is.null(title))
        {
            title <- y1_name
            if (!is.null(y1_unit))
            {
                title <- paste(title, " (", y1_unit, ") and ", sep = "")
            }

            title <- paste(title, y2_name, sep = "")
            if (!is.null(y2_unit))
            {
                title <- paste(title, " (", y2_unit, ") vs ", sep = "")
            }

            title <- paste(title, x_name, sep = "")
            if (!is.null(x_unit))
            {
                title <- paste(title, " (", x_unit, ")", sep = "")
            }
        }

        # set plot margins to accomodate for labels
        par(mar = c(4, 4, 4, 4) + 0.1)

        plot(
            x,
            y1,
            xlim = domain,
            axes = FALSE,
            xlab = "",
            ylab = "",
            type = "l",
            col = y1_color,
            main = title
        )

        points(x,
               y1,
               pch = 16,
               cex = 0.5,
               col = y1_color)

        axis(2, col.axis = y1_color, las = 1)
        mtext(y1_unit,
              side = 2,
              col = y1_color,
              line = 3)

        par(new = TRUE)

        plot(
            x,
            y2,
            xlim = domain,
            xlab = "",
            ylab = "",
            axes = FALSE,
            type = "l",
            col = y2_color
        )

        points(x,
               y2,
               pch = 15,
               cex = 0.5,
               col = y2_color)

        axis(4, col.axis = y2_color, las = 1)
        mtext(y2_unit,
              side = 4,
              col = y2_color,
              line = 3)

        if (class(x)[1] == "POSIXct")
        {
            axis.POSIXct(1, x) # use axis.POSIXct for POSIXct datetime objects
        }
        else
        {
            axis(1, pretty(range(x), 10))
        }
        mtext(x_name,
              side = 1,
              col = "black",
              line = 2.5)

        # draw legend if needed
        if (add_legend)
        {
            legend(
                "topleft",
                legend = c(y1_name, y2_name),
                text.col = c(y1_color, y2_color),
                pch = c(16, 15),
                col = c(y1_color, y2_color)
            )
        }

        # draw box and grid
        box()
        grid()
    }
