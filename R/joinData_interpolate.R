#' Join datasets and interpolate missing fields
#'
#' Merges datasets, specifically from IRENE and LINK-TLM
#' @param data_1 First dataset.
#' @param data_2 Second dataset.
#' @param by Key with which to join datasets.
#' @export
#' @importFrom zoo na.approx
#' @importFrom zoo na.fill
#' @examples
#' interpolated_dataset <- merge.interpolate(NS57_LINK_TLM, NS57_IRENE)
#'

merge.interpolate <-
    function(data_1,
             data_2,
             by)
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

            for (key in by)
            {
                for (column in colnames(joined_data))
                {
                    if ("numeric" %in% data_types[[column]] |
                        "integer" %in% data_types[[column]])
                    {
                        joined_data[[column]] <-
                            zoo::na.fill(
                                zoo::na.approx(joined_data[[column]], joined_data[[key]], na.rm = FALSE),
                                "extend"
                            )
                    }
                }
            }
        }

        return(joined_data)
    }
