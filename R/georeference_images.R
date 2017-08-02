#' Calculate corner coordinates of aerial images
#'
#' Obtain corner coordinates from magetometer and IMU data
#'
#' @param mag_x Magetometer X data
#' @param mag_y Magetometer X data
#' @param mag_z Magetometer X data
#' @param pitch IMU pitch data
#' @param roll IMU pitch data
#' @param yaw IMU pitch data
#'
#' @author Zach
#'
#' @export
#' @importFrom geosphere destPoint
#'
#' @examples
#'

georeference_images <- function(mag_x,
                         mag_y,
                         mag_z,
                         pitch,
                         roll,
                         yaw)
{
    vfov = 48.83
    hfov = 62.2  #52.53

    ns67.joined <-
        merge(ns67_truncated[c(1, 6:8, 9:11, 18)],
              ns67_cell[c(1, 2:4, 6:7)],
              by = "DateTime")

    X = ns67.joined$Mag_x
    Y = ns67.joined$Mag_y
    Z = ns67.joined$Mag_z

    pitch = ns67.joined$Pitch
    roll = ns67.joined$Roll
    yaw = ns67.joined$Yaw

    X_h = X * cos(pitch) + Y * sin(roll) * sin(pitch) - Z * cos(roll) * sin(pitch)

    Y_h = Y * cos(roll) - Z * sin(roll)

    headings <- atan2(Y_h, X_h) * 180 / pi  #+ 180

    headings[X_h < 0] <- 180 - headings[X_h < 0]
    headings[X_h > 0 && Y_h < 0] <- -headings[X_h > 0 && Y_h < 0]
    headings[X_h > 0 &&
                 Y_h > 0] <- 360 - headings[X_h > 0 && Y_h > 0]
    headings[X_h == 0 && Y_h < 0] <- 90
    headings[X_h == 0 && Y_h > 0] <- 270

    # construct data frame, get compass bearings in degrees
    corner_coordinates <-
        data.frame(
            DateTime = ns67.joined$DateTime,
            path = as.character(ns67.joined$Message),
            altitude = ns67.joined$Altitude_m,
            pitch = ns67.joined$Pitch,
            roll = ns67.joined$Roll,
            yaw = ns67.joined$Yaw,
            bearing = headings,
            lon = ns67.joined$Longitude,
            lat = ns67.joined$Latitude,
            ul = numeric(nrow(ns67.joined)),
            ur = numeric(nrow(ns67.joined)),
            ll = numeric(nrow(ns67.joined)),
            lr = numeric(nrow(ns67.joined))
        )

    # calculate lower left corner
    corner_coordinates$ll <-
        geosphere::destPoint(
            p = as.matrix(corner_coordinates[c("lon", "lat")]),
            b = corner_coordinates$bearing -
                90,
            d = corner_coordinates$altitude * tan(hfov / 2 * pi / 180)
        )

    # calculate lower right corner
    corner_coordinates$lr <-
        geosphere::destPoint(
            p = as.matrix(corner_coordinates[c("lon", "lat")]),
            b = corner_coordinates$bearing +
                90,
            d = corner_coordinates$altitude * tan(hfov / 2 * pi / 180)
        )

    # get footprint heights
    orthagonal_distance_ground <-
        corner_coordinates$altitude * tan(vfov * pi / 180)

    # get direct distances
    orthagonal_distance_direct <-
        sqrt(orthagonal_distance_ground ^ 2 + corner_coordinates$altitude ^ 2)

    corner_distance_direct <-
        orthagonal_distance_direct / cos(hfov / 2 * pi / 180)

    corner_distance_ground <-
        sqrt(corner_distance_direct ^ 2 - corner_coordinates$altitude ^ 2)

    angle_to_corner <-
        acos(orthagonal_distance_ground / corner_distance_ground) *
        180 / pi

    # calculate upper left corner
    corner_coordinates$ul <-
        geosphere::destPoint(
            p = as.matrix(corner_coordinates[c("lon", "lat")]),
            b = corner_coordinates$bearing -
                angle_to_corner,
            d = corner_distance_ground
        )

    # calculate upper right corner
    corner_coordinates$ur <-
        geosphere::destPoint(
            p = as.matrix(corner_coordinates[c("lon", "lat")]),
            b = corner_coordinates$bearing +
                angle_to_corner,
            d = corner_distance_ground
        )

    return(corner_coordinates)
}
