#' joinData_interpolate
#'
#' Merges datasets, specifically from IRENE and LINK-TLM
#' @param data_1 First dataset.
#' @param data_2 Second dataset.
#' @param key Key with which to join datasets.
#' @param interpolate Whether to interpolate data for missing fields. Defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' library(balloonDataAnaylsis)
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' irene_data <- balloonParseData("NS57LaunchData.txt", "IRENE")
#' joined_data <- joinData_interpolate(tlm_data, irene_data, "Timestamp")
#' joined_data_interpolated <- joinData_interpolate(tlm_data, irene_data, "Timestamp", interpolate = TRUE)

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
            requireNamespace("zoo")
            for (colname in colnames(joined_data))
            {
                joined_data[[colname]] <-
                    zoo::na.fill(zoo::na.approx(joined_data[[colname]], joined_data[[key]], na.rm = FALSE),
                                 "extend")
            }
        }

        return(joined_data)
    }
