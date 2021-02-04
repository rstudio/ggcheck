
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
#' get_stats(p)
get_stats <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_stat, character(1), p = p)
}

#' Does a plot use one or more stats?
#'
#' \code{uses_stats} tests whether a plot uses one or more stats in its layers.
#'
#' The stats can appear in any order in the plot and can be accompanied by other
#' stats that are not checked for. However, if \code{exact} is set to \code{TRUE}, the
#' the plot will have to exactly match the target stats.
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
#' uses_stats(p, stats = c("identity", "smooth"), exact = TRUE)
uses_stats <- function(p, stats, exact = FALSE) {
  # map the GEOM + STAT for plot and the instructor's target stats
  stats <- lapply(stats, map_stat)
  pstats <- lapply(get_stats(p), map_stat)
  if (exact) {
    return(identical(stats, pstats))
  } else {
    return(all(stats %in% pstats))
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

#' Is the ith stat what it should be?
#'
#' \code{ith_stat_is} checks whether the ith layer uses the prescribed type of stat
#'
#' @param p A ggplot object
#' @param stat A character string that corresponds to
#'   the suffix of a ggplot2 \code{stat_} function, e.g. \code{"identity"}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_stat_is} will check the
#'   stat used by the ith layer.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking stats
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = diamonds, aes(sample = price)) +
#'   geom_qq()
#' ith_stat_is(p, i = 1, "qq")
ith_stat_is <- function(p, stat, i = 1) {
  stat_i <- ith_stat(p, i)
  stat_i == stat
}
