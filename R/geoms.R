
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
  vapply(seq_len(n), ith_geom, character(1), p = p)
}

#' Does a plot use one or more geoms?
#'
#' \code{use_geoms} tests whether a plot uses one or more geoms in its layers created
#' using a \code{geom} function without setting the \code{stat} parameter. If checking
#' for a layer that is created using a \code{stat} function, please use \code{uses_stats} instead.
#'
#' The geoms can appear in any order in the plot and can be accompanied by other
#' geoms that are not checked for. However, if \code{exact} is set to \code{TRUE}, the
#' the plot will have to exactly match the target geoms.
#'
#' @param p A ggplot object
#' @param geoms A vector of character strings. Each element should correspond to
#'   the suffix of a ggplot2 \code{geom_} function, e.g. \code{c("point",
#'   "line", "smooth")}.
#' @param exact if \code{TRUE}, use exact matching
#' @param stat if \code{TRUE}, check that the \code{stat} argument was not set beyond default
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
uses_geoms <- function(p, geoms, stat = FALSE, exact = FALSE) {
  # TODO: test the stat combo case
  # map the GEOM + STAT from instructor's `geoms`
  geoms <- map_geoms(geoms, stat)
  pgeoms <- get_geoms(p)
  if (exact) {
    return(identical(geoms, get_geoms(p)))
  } else {
    return(all(geoms %in% get_geoms(p)))
  }
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
#' ith_geom(p, i = 2)
ith_geom <- function(p, i) {
  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

# Problem: most instructors might use uses_geoms to grade for a plot, but layers can be created with both
# geom_ and stat_ functions and the underlying geom class is not necessarily the name of plot (e.g. geom_qq -> "point")

# naive idea: just manually maintain the default mapping btw geom/stat with a table or some such.
# TODO add column that is just the plot type
# (but do we even need to do that if instructors would more often refer to plot by geom type?)

# reprex::reprex({
#   library(dplyr)
#   all_geoms_stats <- tribble(
#     ~geom_fn, ~geom, ~stat,
#     "point", "point", "identity",
#     "density2d", "density2d", "density2d",
#     "qq", "point", "qq",
#     "boxplot", "boxplot", "boxplot",
#     "line", "line", "identity",
#     "bar", "bar", "count",
#     "col", "col", "identity",
#   )
#   layer <- filter(all_geoms_stats, geom_fn == "qq")
#   layer$geom
#   layer$stat
# })
#
#
# # if looking for geom_qq then you can try and see if there is a stat associated with it
# subset(all_geom_stats, geom == "qq")$stat
#
# # there are independent stat_ functions that set a default geom
# # e.g. stat_summary() uses "geom_pointrange" but geom_pointrange() has "stat_identity"
# # so we can still add a row for that case and get geom if we need
# subset(all_geom_stats, stat == "summary")$geom

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
