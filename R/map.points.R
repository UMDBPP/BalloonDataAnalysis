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
           colour = NULL,
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
      ggmap::get_map(
        location = boundaries,
        scale = 2,
        zoom = zoom,
        maptype = "terrain"
      )

    requireNamespace("ggplot2")

    if (!is.null(colour))
    {
      if (class(colour) == "character")
      {
        colour <- location_data[[colour]]
      }
    }
    else
    {
      colour <- "black"
    }

    # plot base layer to map tile
    ggmap(map,
          base_layer = ggplot2::ggplot(data = location_data,
                                       ggplot2::aes(x = Longitude, y = Latitude, colour = colour)),
          extent = "normal") +
      ggplot2::coord_quickmap() +
      ggplot2::geom_path() + ggplot2::geom_point() +
      ggplot2::labs(title = title, x = NULL, y = NULL)
  }
