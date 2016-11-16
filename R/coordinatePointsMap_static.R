#' Plot coordinate pairs onto static map using ggplot2
#'
#' Plots given longitudess and latitudess onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param latitudes List of latitudes. Required.
#' @param longitudes List of longitudes. Required.
#' @param point_color Color of points. Set to a constant color, or to a list to represent data graphically. Defaults to "red".
#' @param point_size Size of points. Set to a constant size, or to a list to represent data graphically. Defaults to 2.
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
             point_size = 3)
    {
        require(ggplot2)
        require(ggmap)

        # pass variables to the global environment so that ggmap can see them
        #point_color_global <<- point_color
        #point_size_global <<- point_size

        # get map boundaries (min lon, min lat, max lon, max lat)
        boundaries <-
            c(min(longitudes),
              min(latitudes),
              max(longitudes),
              max(latitudes))

        # get underlying terrain map
        map <-
            get_map(location = boundaries,
                    maptype = "terrain",
                    zoom = 10)

        points <-
            data.frame(latitude = latitudes, longitude = longitudes)

        # render to plot viewer
        ggmap(map, base_layer = ggplot(
            points,
            aes(
                x = longitudes,
                y = latitudes
            )
        )) + geom_path()

        # remove global variables
        #rm(point_color_global, pos =  globalenv())
        #rm(point_size_global, pos =  globalenv())
    }
