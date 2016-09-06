#' compareMultipleAxes
#'
#' Plots measurements from different sources with DIFFERENT units (with possibly dissimilar dataset sizes) using linear approximation
#' Data must share the same axis data! The best way to ensure this is for them to be in the same dataset and share a common field.
#' taken from http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param bottom_axis Shared x axis.
#' @param measurement_1 First set of measurements.
#' @param measurement_2 Second set of measurements.
#' @param domain Domain of plot.
#' @param range Range of plot.
#' @param bottom_axis_name Defaults to "x".
#' @param common_measurement_name Defaults to "y".
#' @param measurement_1_name Source of measurement_1 data.
#' @param measurement_1_color Defaults to "red".
#' @param measurement_2_name Source of measurement_2 data.
#' @param measurement_2_color Defaults to "blue".
#' @keywords
#' @export
#' @examples
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- balloonParseData("NS57LaunchData.txt", "IRENE")
#' joined_data <- balloonMergeDatasets(tlm_data, irene_data)
#' compareMultipleAxes(joined_data$Timestamp, joined_data$Counts_Per_Minute, joined_data$Altitude_m, domain = c(min(tlm_data$Timestamp), max(tlm_data$Timestamp)), bottom_axis_name = "Time (24hr)", measurement_1_name = "Radiation (counts per minute)", measurement_2_name = "Altitude (meters)")

compareMultipleAxes <-
    function(bottom_axis,
             measurement_1,
             measurement_2,
             domain = NULL,
             range = NULL,
             bottom_axis_name = "x",
             common_measurement_name = "y",
             measurement_1_name = "measurement_1",
             measurement_1_color = "red",
             measurement_2_name = "measurement_2",
             measurement_2_color = "blue")
    {
        if (is.null(domain))
        {
            domain <- c(min(bottom_axis), max(bottom_axis))
        }

        if (is.null(range))
        {
            range <-
                c(min(c(
                    min(measurement_1), min(measurement_2)
                )), max(c(
                    max(measurement_1), max(measurement_2)
                )))
        }

        if (is.null(title))
        {
            title <-
                paste(measurement_1_name,
                      "and",
                      measurement_2_name,
                      "vs",
                      bottom_axis_name)
        }

        # If you'd rather use plotrix (doesn't support POSIXct axis):
        #require(plotrix)
        #twoord.plot(ly = measurement_1, ry = measurement_2, lx = bottom_axis, rx = bottom_axis, main = title, ylab = measurement_1_name, rylab = measurement_2_name, xlab = bottom_axis_name)

        par(mar = c(3, 3, 3, 4) + 0.1)
        plot(
            approxfun(bottom_axis, measurement_1),
            xlim = domain,
            axes = FALSE,
            xlab = "",
            ylab = "",
            type = "l",
            col = measurement_1_color,
            main = title
        )
        points(
            bottom_axis,
            measurement_1,
            pch = 16,
            cex = 0.5,
            col = measurement_1_color
        )

        #mtext(measurement_1_name, side = 2, col = "blue", line = 2.5)
        axis(2, col.axis = "blue", las = 1)

        box()
        par(new = TRUE)

        plot(
            approxfun(bottom_axis, measurement_2),
            xlim = domain,
            xlab = "",
            ylab = "",
            axes = FALSE,
            type = "l",
            col = "red"
        )

        points(
            bottom_axis,
            measurement_2,
            pch = 15,
            cex = 0.5,
            col = "red"
        )

        #mtext(measurement_2_name, side = 4, col = "red", line = 4)
        axis(4,
             col = "red",
             col.axis = "red",
             las = 1)

        axis.POSIXct(1, bottom_axis) # use axis.POSIXct for POSIXct datetime objects
        #axis(1, pretty(range(bottom_axis),10))
        #mtext(bottom_axis_name, side = 1, col = "black", line = 2.5)

        legend(
            "topleft",
            legend = c(measurement_1_name, measurement_2_name),
            text.col = c("blue", "red"),
            pch = c(16, 15),
            col = c("blue", "red")
        )
        grid()
    }
