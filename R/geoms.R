#' List the geoms used by a plot
#'
#' \code{get_geoms} returns a vector of geom names, written as character
#' strings, that describes which geoms in which order are used by a plot.
#'
#' @param p A ggplot object
#'
#' @return A vector of character strings. Each element corresponds to the suffix
#'   of a ggplot2 \code{geom_} function, e.g. \code{c("point", "line", "smooth")}.
#'
#' @family functions for checking geoms
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' get_geoms(p)
get_geoms <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_geom, "a", p = p)
}

#' Does a plot use an exact set of geoms?
#'
#' \code{geoms_match} tests whether the geoms used by a plot exactly match the
#' set of geoms described by \code{geoms} in both order and type.
#'
#' @param p A ggplot object
#' @param geoms A vector of character strings. Each element should correspond to
#'   the suffix of a ggplot2 \code{geom_} function, e.g. \code{c("point",
#'   "line", "smooth")}.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking geoms
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' geoms_match(p, geoms = c("point", "smooth"))
geoms_match <- function(p, geoms) {
  identical(geoms, get_geoms(p))
}

#' Does a plot use one or more geoms?
#'
#' \code{use_geoms} tests whether a plot uses one or more geoms in its layers.
#' The geoms can appear in any order in the plot and can be accompanied by other
#' geoms that are not checked for.
#'
#' @param p A ggplot object
#' @param geoms A vector of character strings. Each element should correspond to
#'   the suffix of a ggplot2 \code{geom_} function, e.g. \code{c("point",
#'   "line", "smooth")}.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking geoms
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' uses_geoms(p, geoms = "point")
uses_geoms <- function(p, geoms) {
  all(geoms %in% get_geoms(p))
}

#' Which geom is used in the ith layer?
#'
#' \code{ith_geom} returns the type of geom used by the ith layer.
#'
#' @param p A ggplot object
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on.
#'
#' @return A character string that corresponds to the suffix of a ggplot2
#'   \code{geom_} function, e.g. \code{"point"}.
#'
#' @family functions for checking geoms
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' ith_geom_is(p, geom = "smooth", i = 2)
ith_geom <- function(p, i) {

  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

#' Is the ith geom what it should be?
#'
#' \code{ith_geom_is} checks whether the ith layer uses the prescribed type of geom.
#'
#' @param p A ggplot object
#' @param geom A character string that corresponds to
#'   the suffix of a ggplot2 \code{geom_} function, e.g. \code{"point"}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_geom_is} will check the
#'   geom used by the ith layer.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking geoms
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' ith_geom_is(p, geom = "smooth", i = 2)
ith_geom_is <- function(p, geom, i = 1) {
  geom_i <- ith_geom(p, i)
  geom_i == geom
}





