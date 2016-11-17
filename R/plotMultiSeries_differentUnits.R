#' Plot multiple series with different units on the same axis
#'
#' Plots two series with different units that share a common key.
#' Works best with interpolated data; balloonMergeDatasets(y_left, y2, interpolate = TRUE)
#' Inspired by http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param x Shared x data (key).
#' @param y_left First series.
#' @param y_right Second series.
#' @param xlim Defaults to domain of x axis.
#' @param xlab X series label.
#' @param ylab_left Left Y series label.
#' @param col_left Defaults to "red".
#' @param ylab_right Right Y series label.
#' @param col_right Defaults to "blue".
#' @param type Defaults to FALSE.
#' @param type Defaults to FALSE.
#' @param main Title of plot. Defaults to NULL.
#' @export
#' @examples
#' joined_data <- joinData_interpolate(NS57_LINK_TLM, NS57_IRENE, "DateTime", interpolate = TRUE)
#' plotMultiSeries_differentUnits(
#'     x = joined_data$DateTime,
#'     y_left = joined_data$Reading,
#'     y_right = joined_data$Altitude_m,
#'     xlim = c(min(NS57_LINK_TLM$DateTime), max(NS57_LINK_TLM$DateTime)),
#'     xlab = "Time",
#'     ylab_left = "Geiger Counter (CPM)",
#'     ylab_right = "Altitude (m)",
#'     type = "l"
#' )

plotMultiSeries_differentUnits <-
    function(x,
             y_left,
             y_right,
             xlim = NULL,
             xlab = "x",
             ylab_left = NULL,
             col_left = "red",
             ylab_right = NULL,
             col_right = "blue",
             type = "p",
             main = NULL)
    {
        # if xlim was not given, use domain of x
        if (is.null(xlim))
        {
            xlim <- c(min(x), max(x))
        }

        if (is.null(xlab))
        {
            xlab <- deparse(substitute(x))
        }

        if (is.null(ylab_left))
        {
            ylab_left <- deparse(substitute(y_left))
        }

        if (is.null(ylab_right))
        {
            ylab_right <- deparse(substitute(y_right))
        }

        # calculate plot margins
        bottom_mar = 2.75
        left_mar = 2.75
        top_mar = 2.75
        right_mar = 2.75

        temp = max(y_left)
        while (temp >= 1)
        {
            temp = temp / 10
            left_mar = left_mar + 1
        }

        temp = max(y_right)
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
            y_left,
            xlim = xlim,
            axes = FALSE,
            xlab = "",
            ylab = "",
            type = type,
            col = col_left,
            main = main
        )

        axis(2, col.axis = col_left, las = 1)

        # write left axis label
        mtext(ylab_left,
              side = 2,
              col = col_left,
              line = left_mar - 1)

        par(new = TRUE)

        plot(
            x,
            y_right,
            xlim = xlim,
            xlab = "",
            ylab = "",
            axes = FALSE,
            type = type,
            col = col_right
        )

        axis(4, col.axis = col_right, las = 1)

        p <- par('usr')

        # write right axis label
        text(
            p[2] + right_mar * 350,
            mean(p[3:4]),
            labels = ylab_right,
            col = col_right,
            xpd = NA,
            srt = -90
        )

        #mtext(ylab_right,
        #      side = 4,
        #      col = col_right,
        #      line = right_mar - 1)

        if ("POSIXct" %in% class(x))
        {
            axis.POSIXct(1, seq(
                from = xlim[1],
                to = xlim[2],
                by = 60
            ), format = "%H:%M:%S") # use axis.POSIXct for POSIXct datetime objects
        }
        else
        {
            axis(1, pretty(xlim, 10))
        }

        # write bottom axis label
        mtext(xlab,
              side = 1,
              col = "black",
              line = bottom_mar - 1)

        # draw box
        box()
    }
