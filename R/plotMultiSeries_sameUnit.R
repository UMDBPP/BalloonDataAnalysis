#' Plot multiple series with the same unit but possibly different dataset sizes on the same axis
#'
#' Plots two series with the same unit (with possibly dissimilar dataset sizes) on the same graph using linear approximation
#' Works best with outer joined data; balloonMergeDatasets(y1, y2)
#' @param x Shared x data (key).
#' @param y1 First series.
#' @param y2 Second series.
#' @param domain Domain of plot.
#' @param range Range of plot.
#' @param x_axis_units Defaults to "x".
#' @param y_axis_units Defaults to "y".
#' @param y1_name Defaults to "y1".
#' @param y1_color Defaults to "red".
#' @param y2_name Defaults to "y2".
#' @param y2_color Defaults to "blue".
#' @param title Defaults to "y1_name and y2_name vs x_axis_units"
#' @param legend_pos Position of legend, NULL for no legend. Defaults to "topleft".
#' @export
#' @examples
#' tlm_data <- parsePayloadData("NS57_parsedPackets.txt", "LINK-TLM")
#' plotMultiSeries_sameUnit(
#'     tlm_data$Timestamp,
#'     tlm_data$Ascent_Rate_m_s,
#'     tlm_data$Ground_Speed_m_s,
#'     x_axis_units = "Time (24hr)",
#'     y_axis_units = "meters per second",
#'     y1_name = "Ascent Rate",
#'     y2_name = "Ground Speed",
#'     legend_pos = "bottomleft"
#' )

plotMultiSeries_sameUnit <-
    function(x,
             y1,
             y2,
             domain = NULL,
             range = NULL,
             x_axis_units = "x",
             y_axis_units = "y",
             y1_name = "y1",
             y1_color = "red",
             y2_name = "y2",
             y2_color = "blue",
             title = NULL,
             legend_pos = "topleft")
    {
        if (is.null(domain))
        {
            domain <- c(min(x), max(x))
        }

        if (is.null(range))
        {
            range <-
                c(min(c(min(y1), min(y2))), max(c(max(y1), max(y2))))
        }

        if (is.null(title))
        {
            title <-
                paste(y1_name,
                      "and",
                      y2_name,
                      "vs",
                      x_axis_units)
        }

        plot(
            x,
            y1,
            xlim = domain,
            ylim = range,
            type = "p",
            pch = 15,
            cex = 0.5,
            col = y1_color,
            xlab = x_axis_units,
            ylab = y_axis_units,
            main = title
        )

        points(x,
               y2,
               col = y2_color,
               pch = 16,
               cex = 0.5)

        lines(x, y1, col = y1_color)
        lines(x, y2, col = y2_color)

        if (!is.null(legend_pos))
        {
            legend(
                legend_pos,
                legend = c(y1_name, y2_name),
                text.col = c(y1_color, y2_color),
                pch = c(15, 16),
                col = c(y1_color, y2_color)
            )
        }

        grid()
    }
