#' Calculate corner coordinates of aerial images
#'
#' Obtain corner coordinates from magetometer and IMU data
#'
#' @param v_fov Vertical camera field of view angle
#' @param h_fov Horizontal camera field of view angle
#' @param mag_x Magetometer X data
#' @param mag_y Magetometer Y data
#' @param mag_z Magetometer Z data
#' @param pitch IMU pitch data
#' @param roll IMU roll data
#' @param yaw IMU yaw data
#'
#' @export
#' @importFrom geosphere destPoint
#'
#' @examples
#'

georeference_images <- function(v_fov,
		h_fov,
		mag_x,
		mag_y,
		mag_z,
		pitch,
		roll,
		yaw)
{
	v_fov = 48.83
	h_fov = 62.2  #52.53

	ns67.joined <-
			merge(ns67_truncated[c(1, 6:8, 9:11, 18)],
					ns67_cell[c(1, 2:4, 6:7)],
					by = "DateTime")

	mag_x = ns67.joined$Mag_x
	mag_y = ns67.joined$Mag_y
	mag_z = ns67.joined$Mag_z

	pitch = ns67.joined$Pitch
	roll = ns67.joined$Roll
	yaw = ns67.joined$Yaw

	X_h = mag_x * cos(pitch) + mag_y * sin(roll) * sin(pitch) - mag_z * cos(roll) * sin(pitch)

	Y_h = mag_y * cos(roll) - mag_z * sin(roll)

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
					d = corner_coordinates$altitude * tan(h_fov / 2 * pi / 180)
			)

	# calculate lower right corner
	corner_coordinates$lr <-
			geosphere::destPoint(
					p = as.matrix(corner_coordinates[c("lon", "lat")]),
					b = corner_coordinates$bearing +
							90,
					d = corner_coordinates$altitude * tan(h_fov / 2 * pi / 180)
			)

	# get footprint heights
	orthagonal_distance_ground <-
			corner_coordinates$altitude * tan(v_fov * pi / 180)

	# get direct distances
	orthagonal_distance_direct <-
			sqrt(orthagonal_distance_ground ^ 2 + corner_coordinates$altitude ^ 2)

	corner_distance_direct <-
			orthagonal_distance_direct / cos(h_fov / 2 * pi / 180)

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
