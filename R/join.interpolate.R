#' Join datasets and interpolate missing fields
#'
#' Merges datasets, for instance IRENE and LINK-TLM, and interpolates numeric data to replace NA.
#'
#' @param data_1 First dataset.
#' @param data_2 Second dataset.
#' @param by One or more keys with which to join datasets, ordered by priority.
#' @param exclude List of column names to exclude from interpolation.
#' @export
#' @importFrom zoo na.approx
#' @importFrom zoo na.fill
#' @examples
#' NS57.RadCoord <- join.interpolate(NS57.Coord, NS57.Rad, exclude = c("Reading", "Unit"))
#' plot(NS57.RadCoord$Reading, NS57.RadCoord$Altitude_m)
#'

join.interpolate <-
    function(data_1,
             data_2,
             by = c("DateTime", "Data_Source", "Flight"),
             exclude = NULL)
    {
        # outer join tables by key
        joined_data <-
            merge(
                x = data_1,
                y = data_2,
                by = c(by),
                all = TRUE
            )

        # replace NA values with interpolated values using zoo package
        if (any(is.na(joined_data)))
        {
            data_types <- lapply(joined_data, class)

            for (column in colnames(joined_data))
            {
                if ("numeric" %in% data_types[[column]] |
                    "integer" %in% data_types[[column]] &
                    !(column %in% by) &
                    !(column %in% exclude))
                {
                    requireNamespace("zoo")

                    joined_data[[column]] <-
                        zoo::na.fill(zoo::na.approx(joined_data[[column]], joined_data[[by[1]]], na.rm = FALSE),
                                     "extend")
                }
            }
        }

        return(joined_data)
    }
