#' Plot multiple series with different units on the same axis
#'
#' Plots two series with different units that share a common key.
#' Works best with interpolated data; balloonMergeDatasets(y_left, y2, interpolate = TRUE)
#' Inspired by http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param x X series.
#' @param y Y series.
#' @param xlim Defaults to domain of x axis.
#' @param xlab X labels.
#' @param ylab Y labels.
#' @param type Defaults to FALSE.
#' @param main Title of plot. Defaults to NULL.
#' @export
#' @examples
#' plotMultiSeries_differentUnits(
#'     x = list(NS57_IRENE$DateTime, NS57_LINK_TLM$DateTime),
#'     y = list(NS57_IRENE$Reading, NS57_LINK_TLM$Altitude_m),
#'     xlim = c(min(NS57_LINK_TLM$DateTime), max(NS57_LINK_TLM$DateTime)),
#'     xlab = c("Time", "Time"),
#'     ylab = c("Geiger Counter (CPM)", "Altitude (m)"),
#'     type = "l"
#' )

plotMultiSeries_differentUnits <-
    function(x,
             y,
             xlim = NULL,
             xlab = NULL,
             ylab = NULL,
             type = "p",
             main = NULL)
    {
        # define initial plot margins
        bottom_mar = 2.75
        left_mar = 0
        top_mar = 2.75
        right_mar = 0

        # value to add to margin for each digit
        digit_width <- 0.8

        # get x series labels
        for (index in c(1, length(x)))
        {
            if (is.null(xlab[index]))
            {
                xlab[index] <- deparse(substitute(x[index]))
            }
        }

        # get left and right plot margins for odd and even series respectively
        for (index in c(1, length(y)))
        {
            if (is.null(ylab[index]))
            {
                ylab[index] <- deparse(substitute(y[index]))
            }

            temp = max(as.numeric(y[[index]]))

            if (index %% 2 == 0)
            {
                right_mar = right_mar + 1.5
                while (temp >= 1)
                {
                    temp = temp / 10
                    right_mar = right_mar + digit_width
                }
            }
            else
            {
                left_mar = left_mar + 1.5
                while (temp >= 1)
                {
                    temp = temp / 10
                    left_mar = left_mar + digit_width
                }
            }
        }

        # if xlim was not given, use domain of x
        if (is.null(xlim))
        {
            xlim <- range(unlist(x))
        }

        # set plot margins
        par(mar = c(bottom_mar, left_mar, top_mar, right_mar))

        for (index in c(1, length(y)))
        {
            plot(
                x[[index]],
                y[[index]],
                xlim = xlim,
                axes = FALSE,
                xlab = "",
                ylab = "",
                type = type,
                col = index,
                main = main
            )

            if (index %% 2 == 0)
            {
                side = 4
                side_mar <- right_mar
            }
            else
            {
                side = 2
                side_mar <- left_mar
            }

            # write axis
            axis(side = side,
                 col.axis = index,
                 las = 1)
            mtext(ylab[index],
                  side = side,
                  col = index,
                  line = side_mar - 1)

            par(new = TRUE)
        }

        side = 1

        # use axis.POSIXct for POSIXct datetime objects
        if ("POSIXct" %in% class(x[[1]]))
        {
            axis.POSIXct(side = side,
                         seq(
                             from = as.POSIXct(xlim[1], origin = "1970-01-01"),
                             to = as.POSIXct(xlim[2], origin = "1970-01-01"),
                             by = 60
                         ),
                         format = "%H:%M:%S")
        }
        else
        {
            axis(side = side, pretty(xlim, 10))
        }

        # write axis label
        mtext(xlab,
              side = side,
              col = "black",
              line = bottom_mar - 1)

        # draw box
        box()
    }
