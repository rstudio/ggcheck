# TODO-Nischal reduce duplication

#' List the stats used by a plot
#'
#' \code{get_stats} returns a vector of stats names, written as character
#' strings, that describes which stats in which order are used by a plot.
#'
#' @param p A ggplot object
#'
#' @return A vector of character strings. Each element corresponds to the suffix
#'   of a ggplot2 \code{stat_} function, e.g. \code{c("identity", "smooth")}.
#'
#' @family functions for checking stats
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' get_stats(p, c("identity", "smooth"))
get_stats <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_stat, character(1), p = p)
}

#' Does a plot use one or more stats?
#'
#' \code{uses_stats} tests whether a plot uses one or more stats in its layers.
#' The stats can appear in any order in the plot and can be accompanied by other
#' stats that are not checked for.
#'
#' @param p A ggplot object
#' @param stats A vector of character strings. Each element should correspond to
#'   the suffix of a ggplot2 \code{stat_} function, e.g. \code{c("identity", "smooth")}.
#' @param exact if \code{TRUE}, use exact matching
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking stats
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' uses_stats(p, stats = "smooth")
uses_stats <- function(p, stats, exact = FALSE) {
  if (exact) {
    return(identical(stats, get_stats(p)))
  } else {
    return(all(stats %in% get_stats(p)))
  }
}

#' Which stat is used in the ith layer?
#'
#' \code{ith_stat} returns the type of stat used by the ith layer.
#'
#' @param p A ggplot object
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on.
#'
#' @return A character string that corresponds to the suffix of a ggplot2
#'   \code{stat_} function, e.g. \code{"qq"}.
#'
#' @family functions for checking stats
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = diamonds, aes(sample = price)) +
#'   geom_qq()
#' ith_stat(p, i = 1)
ith_stat <- function(p, i) {
  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }
  stat <- class(p$layers[[i]]$stat)[1]
  gsub("stat", "", tolower(stat))
}
