#' Plot coordinate pairs onto static map using ggplot2
#'
#' Plots given longitudess and latitudess onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param latitudes List of latitudes. Required.
#' @param longitudes List of longitudes. Required.
#' @param point_color Color of points. Set to a constant color, or to a list to represent data graphically. Defaults to "red".
#' @param point_size Size of points. Set to a constant size, or to a list to represent data graphically. Defaults to 2.
#' @param zoom_level Zoom level of map. 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map. Defaults to 10.
#' @export
#' @importFrom ggmap get_map
#' @importFrom ggmap ggmap
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @examples
#' coordinatePointsMap_static(
#'     NS57_LINK_TLM$Latitude,
#'     NS57_LINK_TLM$Longitude,
#'     point_color = NS57_LINK_TLM$Altitude_m,
#' )

coordinatePointsMap_static <-
    function(latitudes,
             longitudes,
             point_color = "red",
             point_size = 3,
             zoom_level = 10)
    {
        # pass variables to the global environment so that ggmap can see them
        point_color <<- point_color
        point_size <<- point_size

        # get underlying terrain map using mean coordinates
        map <-
            ggmap::get_map(
                location = c(lon = mean(longitudes),
                             lat = mean(latitudes)),
                zoom = zoom_level,
                maptype = "terrain"
            )

        # render to plot viewer
        ggmap::ggmap(map) + ggplot2::geom_point(data = as.data.frame(cbind(lon = longitudes, lat = latitudes)),
                                                ggplot2::aes(colour = point_color),
                                                size = point_size)

        # remove global variables
        rm(point_color, pos =  globalenv())
        rm(point_size, pos = globalenv())
    }
