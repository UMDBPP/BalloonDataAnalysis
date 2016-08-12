#' Generate Balloon Track
#'
#' Plots measurements from different sources with DIFFERENT units (with possibly dissimilar dataset sizes) using linear approximation
#' in order for this to work, you must first perform an outer join (keep all data) of the data you want to compare into one joined dataset, sharing one key
#' taken from http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param longitude
#' @param latitude
#' @param color
#' @param size
#' @param title Map title. Set to element_blank() for none. Defaults to "Altitude (meters)".
#' @param zoom_level Zoom level of map. 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map. Defaults to 10.
#' @keywords
#' @export
#' @examples
#' tlm_data <- parse_link_tlm_data("NS57_parsedPackets.txt")
#' generate_balloon_track(tlm_data$Longitude, tlm_data$Latitude, color = tlm_data$Altitude_m, title = "Altitude (meters)")

generate_balloon_track <-
    function(longitude,
             latitude,
             title = "Balloon Track",
             zoom_level = 10)
    {
        require(ggmap)

        # get underlying terrain map using mean coordinates
        map <-
            get_map(
                location = c(lon = mean(longitude),
                             lat = mean(latitude)),
                zoom = zoom_level,
                maptype = "terrain"
            )

        # render to plot viewer
        ggmap(map) + geom_point(
            data = as.data.frame(cbind(lon = longitude, lat = latitude)),
            aes(
                x = lon,
                y = lat
            ),
            size = 2
        ) + labs(x = "Longitude", y = "Latitude", title = title)
    }
