#' Join datasets and interpolate missing fields
#'
#' Merges datasets, specifically from IRENE and LINK-TLM
#' @param data_1 First dataset.
#' @param data_2 Second dataset.
#' @param key Key with which to join datasets.
#' @param interpolate Wether to interpolate data. Defaults to FALSE.
#' @export
#' @importFrom zoo na.approx
#' @importFrom zoo na.fill
#' @examples
#' joined_data <- joinData_interpolate(NS57_LINK_TLM, NS57_IRENE, "Timestamp")
#' joined_data_interpolated <-
#'      joinData_interpolate(NS57_LINK_TLM,
#'                          NS57_IRENE,
#'                          "Timestamp",
#'                          interpolate = TRUE
#'      )

joinData_interpolate <-
    function(data_1, data_2, key, interpolate = FALSE)
    {
        # outer join tables by key
        joined_data <-
            merge(
                x = data_1,
                y = data_2,
                by = c(key),
                all = TRUE
            )

        # interpolate using zoo package
        if (interpolate)
        {
            key_is_POSIXct <- (class(joined_data[[key]])[1] == "POSIXct")
            for (colname in colnames(joined_data))
            {
                joined_data[[colname]] <-
                    zoo::na.fill(zoo::na.approx(joined_data[[colname]], joined_data[[key]], na.rm = FALSE),
                                 "extend")
            }
            if (key_is_POSIXct)
            {
                joined_data[[key]] <- as.POSIXct(joined_data[[key]], origin = "1970-01-01")
            }
        }

        return(joined_data)
    }
