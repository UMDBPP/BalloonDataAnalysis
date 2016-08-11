#' Compare Same Axis
#'
#' Plots measurements from different sources with the same unit (with possibly dissimilar dataset sizes) using linear approximation
#' in order for this to work, you must first perform an outer join (keep all data) of the data you want to compare into one joined dataset, sharing one key
#' @param bottom_axis Shared x axis.
#' @param measurement_1 First set of measurements.
#' @param measurement_2 Second set of measurements.
#' @param bottom_axis_name Defaults to "x".
#' @param common_measurement_name Defaults to "y".
#' @param measurement_1_name Source of measurement_1 data.
#' @param measurement_1_color Defaults to "red".
#' @param measurement_2_name Source of measurement_2 data.
#' @param measurement_2_color Defaults to "blue".
#' @keywords
#' @export
#' @examples
#' compare_same_axis(tlm_data$Timestamp, tlm_data$Ascent_Rate_m_s, tlm_data$Ground_Speed_m_s, bottom_axis_name = "Time (24hr)", common_measurement_name = "meters per second", measurement_1_name = "Ascent Rate", measurement_2_name = "Ground Speed")

compare_same_axis <-
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
             measurement_2_color = "blue",
             title = NULL)
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

        plot(
            approxfun(bottom_axis, measurement_1),
            xlim = domain,
            ylim = range,
            type = "l",
            col = measurement_1_color,
            xlab = bottom_axis_name,
            ylab = common_measurement_name,
            main = title
        )

        points(
            bottom_axis,
            measurement_1,
            col = measurement_1_color,
            pch = 15,
            cex = 0.5
        )

        f <- approxfun(bottom_axis, measurement_2)

        curve(f(x),
              xlim = domain,
              col = measurement_2_color,
              add = TRUE)

        points(
            bottom_axis,
            measurement_2,
            col = measurement_2_color,
            pch = 16,
            cex = 0.5
        )

        legend(
            "topleft",
            legend = c(measurement_1_name, measurement_2_name),
            text.col = c(measurement_1_color, measurement_2_color),
            pch = c(15, 16),
            col = c(measurement_1_color, measurement_2_color)
        )

        grid()
    }
