#' balloonStaticMap
#'
#' Plots given longitudes and latitudes onto a terrain map of the area. Pass a list as the "point_color" and / or "point_size" arguments in order to represent data graphically.
#' @param latitude List of latitudes. Required.
#' @param longitude List of longitudes. Required.
#' @param point_color Color of points. Set to a constant color, or to a list to represent data graphically. Defaults to "blue".
#' @param point_size Size of points. Set to a constant size, or to a list to represent data graphically. Defaults to 2.
#' @param title Map title. Set to element_blank() for none. Defaults to "Altitude (meters)".
#' @param zoom_level Zoom level of map. 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map. Defaults to 10.
#' @examples
#' tlm_data <- balloonParseData("NS57_parsedPackets.txt", "LINK-TLM")
#' balloonStaticMap(tlm_data$Latitude, tlm_data$Longitude, point_color = tlm_data$Altitude_m, title = "Altitude (meters)")

balloonStaticMap <-
    function(latitude,
             longitude,
             point_color = "blue",
             point_size = 3,
             title = "Balloon Track",
             zoom_level = 10)
    {
        requireNamespace("ggmap")

        # pass variables to the global environment so that ggmap can see them
        point_color <<- point_color
        point_size <<- point_size

        # get underlying terrain map using mean coordinates
        map <-
            ggmap::get_map(
                location = c(lon = mean(longitude),
                             lat = mean(latitude)),
                zoom = zoom_level,
                maptype = "terrain"
            )

        # render to plot viewer
        ggmap::ggmap(map) + geom_point(
            data = as.data.frame(cbind(lon = longitude, lat = latitude)),
            aes(x = lon,
                y = lat,
                colour = point_color),
            size = point_size
        ) + labs(x = "Longitude", y = "Latitude", title = title)
    }
