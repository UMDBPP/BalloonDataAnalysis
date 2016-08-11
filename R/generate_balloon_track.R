# This script makes a map of a balloon track, assuming an input CSV file with rows of lat, long, and altitude
# Requires package "ggmap" to be installed (install.packages("ggmap"))

#' Generate Balloon Track
#'
#' Plots measurements from different sources with DIFFERENT units (with possibly dissimilar dataset sizes) using linear approximation
#' in order for this to work, you must first perform an outer join (keep all data) of the data you want to compare into one joined dataset, sharing one key
#' taken from http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#' @param input_data Input dataset.
#' @param map_title Map title. Set to element_blank() for none. Defaults to "Altitude (meters)".
#' @param zoom_level Zoom level of map. 3 is world, 10 is city, 21 is street. May have to fine tune this variable to get good map. Defaults to 10.
#' @keywords
#' @export
#' @examples
#' generate_balloon_track(tlm_data)

generate_balloon_track <-
    function(input_data,
             map_title = "Altitude (meters)",
             zoom_level = 10)
    {
        require(ggmap)

        # get underlying terrain map using mean coordinates
        map <-
            get_map(
                location = c(
                    lon = mean(input_data$Longitude),
                    lat = mean(input_data$Latitude)
                ),
                zoom = zoom_level,
                maptype = "terrain"
            )

        # render to plot viewer
        ggmap(map) + geom_point(
            data = as.data.frame(
                cbind(lon = input_data$Longitude, lat = input_data$Latitude)
            ),
            aes(
                x = lon,
                y = lat,
                colour = input_data$Altitude_m
            ),
            size = 2
        ) + labs(x = "Longitude", y = "Latitude", title = map_title) + theme(legend.title =
                                                                             element_blank())
    }
