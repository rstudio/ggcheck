
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
#' By default, the plot must have the exact stats or geom/stat combinations and in the same order.
#' However, if \code{exact} is set to \code{FALSE}, the plot stats or geom/stat combinations do not have to be exact.
#'
#' @param p A ggplot object
#' @param stats A vector of character strings. Each element should correspond to
#'   the suffix of a ggplot2 \code{stat_} function, e.g. \code{c("identity", "smooth")}.
#' @param exact if \code{TRUE}, use exact matching
#' @param geoms A character vector to optionally check for the geoms corresponding to stats
#'   e.g. c("point", "smooth") if checking c("identity", "smooth")
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
#' uses_stats(p, c("smooth", "identity"), geoms = c("smooth", "point"))
uses_stats <- function(p, stats, geoms = NULL, exact = TRUE) {
  # map the GEOM + STAT for plot and the instructor's target stats
  stats <- lapply(stats, map_stat)
  # if geoms is specified override the GEOM(s) defaults of geoms
  if (!is.null(geoms)) {
    # number of geoms have to be the same as number of stats.
    if (length(geoms) != length(stats)) {
      stop("Grading error: number of geoms supplied don't match number of stats.")
    }
    # map user supplied geoms suffixes to actual class names
    geoms <- lapply(geoms, map_geom)
    stats <- lapply(seq_along(stats), function(s) {
      stats[[s]]$GEOM <- geoms[[s]]$GEOM
      stats[[s]]
    })
  }
  pstats <- get_geoms_stats(p)
  if (exact) {
    return(identical(stats, pstats))
  } else {
    return(all(stats %in% pstats))
  }
}

#' Does a layer use a specific stat parameter?
#'
#' \code{uses_stat_param} is a mirror function of \code{uses_geom_param} but instead of checking a plot's
#' geom layer, it checks that a plot's stat layer uses a specific stat parameter.
#'
#' To specify a specific stat layer, either specify using position using the \code{i} index or
#' by using a combination of \code{stat} function suffix name and \code{i} to check the ith layer that
#' uses the stat.
#'
#' @param p A ggplot object
#' @param stat A character string found in the suffix of a ggplot2 stat function,
#'  e.g. \code{"bin"}.
#' @param params A named list of stat or geom parameter values, e.g. \code{list(bins = 200)}
#' @param i A numerical index, e.g. \code{1}.
#'
#' @return A boolean
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(diamonds, aes(carat)) +
#'   stat_bin(bins = 200)
#' uses_stat_param(p, stat = "bin", params = list(bins = 200))
uses_stat_param <- function(p, stat, params, i = NULL) {
  layer <- get_stat_layer(p, stat = stat, i)$layer
  user_params <- names(params)
  # collect geom and stat parameters
  all_params <- c(layer$geom_params, layer$stat_params)
  p_params <- names(all_params)
  # check if user supplied invalid parameters
  invalid_params <- !(user_params %in% p_params)
  if (any(invalid_params)) {
    stop(
      "Grading error: the supplied parameters ",
      paste0("'", user_params[invalid_params], "'", collapse = ", "), " are invalid."
    )
  }
  # check both the user parameters contained in plot's geom and stat parameters
  identical(params, all_params[user_params])
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
  if (!inherits(p, "ggplot")) {
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
