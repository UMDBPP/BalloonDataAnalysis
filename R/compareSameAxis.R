#' compareSameAxis
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
#' @keywords
#' @export
#' @examples
#' library(balloonDataAnaylsis)
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' compareSameAxis(
#'     tlm_data$Timestamp,
#'     tlm_data$Ascent_Rate_m_s,
#'     tlm_data$Ground_Speed_m_s,
#'     x_axis_units = "Time (24hr)",
#'     y_axis_units = "meters per second",
#'     y1_name = "Ascent Rate",
#'     y2_name = "Ground Speed"
#' )

compareSameAxis <-
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
             title = NULL)
    {
        if (is.null(domain))
        {
            domain <- c(min(x), max(x))
        }

        if (is.null(range))
        {
            range <-
                c(min(c(
                    min(y1), min(y2)
                )), max(c(
                    max(y1), max(y2)
                )))
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
            approxfun(x, y1),
            xlim = domain,
            ylim = range,
            type = "l",
            col = y1_color,
            xlab = x_axis_units,
            ylab = y_axis_units,
            main = title
        )

        points(
            x,
            y1,
            col = y1_color,
            pch = 15,
            cex = 0.5
        )

        f <- approxfun(x, y2)

        curve(f(x),
              xlim = domain,
              col = y2_color,
              add = TRUE)

        points(
            x,
            y2,
            col = y2_color,
            pch = 16,
            cex = 0.5
        )

        legend(
            "topleft",
            legend = c(y1_name, y2_name),
            text.col = c(y1_color, y2_color),
            pch = c(15, 16),
            col = c(y1_color, y2_color)
        )

        grid()
    }
