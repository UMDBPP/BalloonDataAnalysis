#' Merge Datasets
#'
#' Merges datasets, specifically from IRENE and LINK-TLM
#' @param data_1 First dataset.
#' @param data_2 Second dataset.
#' @param join_type Outer or inner join. Defaults to "outer".
#' @keywords
#' @export
#' @examples
#' tlm_data <- parse_link_tlm_data("NS57_parsedPackets.txt")
#' irene_data <- parse_irene_data("NS57LaunchData.txt")
#' joined_data <- merge_datasets(tlm_data, irene_data)

merge_datasets <- function(data_1, data_2, join_type = "outer")
{
    # join tables (outer join with POSIX timestamp as key)
    joined_data <-
        merge(
            x = tlm_data,
            y = irene_data,
            by = c("Timestamp"),
            all = TRUE
        )

    if (join_type == "inner")
    {
        # add missing data to LINK-TLM rows
        # TODO find a better way to interpolate data than "nearest past neighbor"
        while (anyNA(joined_data$Counts_Per_Minute))
        {
            joined_data$Counts_Per_Minute[is.na(joined_data$Counts_Per_Minute)] = joined_data$Counts_Per_Minute[which(is.na(joined_data$Counts_Per_Minute)) - 1]
        }

        # remove rows not belonging to LINK-TLM and write to CSV
        joined_data <- subset(joined_data, !(is.na(Callsign)))
    }

    return(joined_data)
}
