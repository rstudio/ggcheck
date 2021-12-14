#' Which coordinate system does a plot use?
#'
#' @param p A ggplot2 object
#'
#' @return A character string that corresponds to the suffix of a ggplot2
#'   \code{coord_} function, e.g. \code{"cartesian"}.
#' @family functions for checking coordinate systems
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth() +
#'   coord_polar()
#' get_coordinate_system(p)
get_coordinate_system <- function(p) {
  stop_if_not_ggplot(p)
  coords <- class(p$coordinates)[1]
  gsub("coord", "", tolower(coords))
}

#' Does a plot use the correct coordinate system?
#'
#' \code{uses_coordinate_system} checks whether a plot uses the coordinate
#' system you describe. To describe a coordinate system, use the character
#' string that matches the suffix of the ggplot2 \code{coord_} function that
#' would make the coordinate system. The default coordinate system for ggplot2
#' plots is \code{"cartesian"}.
#'
#' @param p A ggplot2 object
#' @param coordinates A character string that corresponds to the suffix of a
#'   ggplot2 \code{coord_} function, e.g. \code{"cartesian"}.
#'
#' @return \code{TRUE} or \code{FALSE}
#' @family functions for checking coordinate systems
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth() +
#'   coord_polar()
#' uses_coordinate_system(p, coordinates = "polar")
uses_coordinate_system <- function(p, coordinates) {
  stop_if_not_ggplot(p)
  coordinates == get_coordinate_system(p)
}
