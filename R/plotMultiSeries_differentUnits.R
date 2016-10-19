#' Plot multiple series with different units on the same axis
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
#' @param legend_pos Position of legend, NULL for no legend. Defaults to "topleft".
#' @export
#' @examples
#' joined_data <- joinData_interpolate(NS57_LINK_TLM, NS57_IRENE, "Timestamp", interpolate = TRUE)
#' plotMultiSeries_differentUnits(
#'     joined_data$Timestamp,
#'     joined_data$Reading,
#'     joined_data$Altitude_m,
#'     domain = c(min(NS57_LINK_TLM$Timestamp), max(NS57_LINK_TLM$Timestamp)),
#'     x_name = "Time",
#'     x_unit = "24hr",
#'     y1_name = "Geiger Counter",
#'     y1_unit = "counts per minute",
#'     y2_name = "Altitude",
#'     y2_unit = "meters",
#'     legend_pos = NULL
#' )

plotMultiSeries_differentUnits <-
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
             legend_pos = "topleft")
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

            title <- paste(title, y2_name, " ", sep = "")
            if (!is.null(y2_unit))
            {
                title <- paste(title, "(", y2_unit, ") vs ", sep = "")
            }

            title <- paste(title, x_name, " ", sep = "")
            if (!is.null(x_unit))
            {
                title <- paste(title, "(", x_unit, ")", sep = "")
            }
        }

        # calculate plot margins
        bottom_mar = 3
        left_mar = 2
        top_mar = 4
        right_mar = 2

        temp = max(y1)
        while (temp >= 1)
        {
            temp = temp / 10
            left_mar = left_mar + 1
        }

        temp = max(y2)
        while (temp >= 1)
        {
            temp = temp / 10
            right_mar = right_mar + 1
        }

        ratio = 0
        if (left_mar > right_mar)
        {
            ratio = right_mar / left_mar
        }
        else if (right_mar > left_mar)
        {
            ratio = left_mar / right_mar
        }
        left_mar = left_mar * ratio
        right_mar = right_mar * ratio

        # set plot margins
        par(mar = c(bottom_mar, left_mar, top_mar, right_mar))

        plot(
            x,
            y1,
            xlim = domain,
            axes = FALSE,
            xlab = "",
            ylab = "",
            type = "p",
            pch = 16,
            cex = 0.5,
            col = y1_color,
            main = title
        )

        lines(x, y1, col = y1_color)

        axis(2, col.axis = y1_color, las = 1)
        mtext(y1_unit,
              side = 2,
              col = y1_color,
              line = left_mar - 1)

        par(new = TRUE)

        plot(
            x,
            y2,
            xlim = domain,
            xlab = "",
            ylab = "",
            axes = FALSE,
            type = "p",
            pch = 15,
            cex = 0.5,
            col = y2_color
        )

        lines(x, y2, col = y2_color)

        axis(4, col.axis = y2_color, las = 1)
        mtext(y2_unit,
              side = 4,
              col = y2_color,
              line = right_mar - 1)

        if (class(x)[1] == "POSIXct")
        {
            axis.POSIXct(1, x) # use axis.POSIXct for POSIXct datetime objects
        }
        else
        {
            axis(1, pretty(range(x), 10))
        }
        mtext(
            paste(x_name, " (", x_unit, ")", sep = ""),
            side = 1,
            col = "black",
            line = bottom_mar - 1
        )

        # draw legend if needed
        if (!is.null(legend_pos))
        {
            legend(
                legend_pos,
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
