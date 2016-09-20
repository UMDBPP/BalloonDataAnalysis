#' Plot coordinate pairs onto static map using ggplot2
#'
#' Plots given longitudes and latitudes onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param latitude List of latitudes. Required.
#' @param longitude List of longitudes. Required.
#' @param api_key Google Maps API Key. Required.
#' @param point_color Color of points. Set to a constant color, or to a list to represent data graphically. Defaults to "blue".
#' @param point_size Size of points. Set to a constant size, or to a list to represent data graphically. Defaults to 2.
#' @param title Map title. Set to element_blank() for none. Defaults to "Altitude (meters)".
#' @param zoom_level Zoom level of map. 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map. Defaults to 10.
#' @export
#' @importFrom ggmap get_map
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @examples
#' tlm_data <- parsePayloadData("NS57_parsedPackets.txt", "LINK-TLM")
#' coordinatePointsMap_static(
#'     tlm_data$Latitude,
#'     tlm_data$Longitude,
#'     api_key = "YOUR_API_KEY",
#'     point_color = tlm_data$Altitude_m,
#'     title = "Altitude (meters)"
#' )

coordinatePointsMap_static <-
    function(latitude,
             longitude,
             api_key,
             point_color = "blue",
             point_size = 3,
             title = "Balloon Track",
             zoom_level = 10)
    {
        # pass variables to the global environment so that ggmap can see them
        point_color <<- point_color
        point_size <<- point_size

        # get underlying terrain map using mean coordinates
        map <-
            ggmap::get_map(
                location = c(lon = mean(longitude),
                             lat = mean(latitude)),
                api_key = api_key,
                zoom = zoom_level,
                maptype = "terrain"
            )

        # render to plot viewer
        ggmap::ggmap(map) + ggplot2::geom_point(
            data = as.data.frame(cbind(lon = longitude, lat = latitude)),
            ggplot2::aes(x = lon,
                         y = lat,
                         colour = point_color),
            size = point_size
        ) + ggplot2::labs(x = "Longitude", y = "Latitude", title = title)

        # remove global variables
        rm(point_color, pos =  globalenv())
        rm(point_size, pos = globalenv())
    }
