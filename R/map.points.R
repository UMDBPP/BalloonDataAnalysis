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
        requireNamespace("ggmap")

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

        # get underlying terrain map
        map <-
            get_map(location = boundaries,
                    scale = 2,
                    maptype = "terrain")

        # plot base layer to map tile
        ggmap(map, extent = "normal") + coord_quickmap() +
            geom_point(data = location_data,
                       aes_string(x = "Longitude",
                                  y = "Latitude")) +
            geom_path(data = location_data, aes_string(x = "Longitude", y = "Latitude")) +
            labs(title = title, x = NULL, y = NULL)
    }
