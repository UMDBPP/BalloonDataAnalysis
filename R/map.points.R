#' Plot coordinate pairs onto Google map tile
#'
#' Plots given longitudes and latitudes onto a Google map tile of the area.
#'
#' @param location_data Dataset with latitude longitude pairs.
#' @param zoom Zoom level of map, range 1 to 21, defaults to 10.
#' @export
#' @importFrom ggmap get_map
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_quickmap
#' @examples
#' map.points(NS57.Coord)

map.points <-
    function(location_data,
             zoom = 10)
    {
        if ("Flight" %in% colnames(location_data))
        {
            title = location_data$Flight
        }
        else
        {
            title = NULL
        }

        # get map boundaries (min lon, min lat, max lon, max lat)
        boundaries <-
            c(
                min(location_data$Longitude),
                min(location_data$Latitude),
                max(location_data$Longitude),
                max(location_data$Latitude)
            )

        requireNamespace("ggmap")

        # get underlying terrain map
        map <-
            ggmap::get_map(location = boundaries,
                           scale = 2,
                           maptype = "terrain")

        requireNamespace("ggplot2")

        # plot base layer to map tile
        ggmap(map, extent = "normal") + ggplot2::coord_quickmap() +
            ggplot2::geom_point(data = location_data,
                                ggplot2::aes_string(x = "Longitude",
                                                    y = "Latitude")) +
            ggplot2::geom_path(data = location_data, ggplot2::aes_string(x = "Longitude", y = "Latitude")) +
            ggplot2::labs(title = title, x = NULL, y = NULL)
    }
