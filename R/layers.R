#' How many layers are in a plot?
#'
#' @param p A ggplot object
#'
#' @return Numeric. The number of layers.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' n_layers(p)
n_layers <- function(p) {
  length(p$layers)
}

#' Isolate a layer from a plot
#'
#' \code{get_layer} returns a layer from a plot along with the global data sets
#' and aesthetic mappings that the layer may inherit from.
#'
#' Users can specify a layer in several ways:
#'
#' \enumerate{
#'  \item By order of appearance with \code{i}. The first layer to appear in the
#'  plot (the one drawn first, on the bottom) corresponds to \code{i = 1}.
#'  \item By type of geom with \code{geom}. \code{get_layer} will return the
#'  first layer that uses the geom.
#'  \item By a combination of \code{geom} and
#'  \code{i}. \code{get_layer} will return the ith layer that uses the geom.
#'  \item By type of stat with \code{stat}. \code{get_layer} will return the
#'  first layer that uses the stat
#'  \item By a combination of \code{stat} and
#'  \code{i}. \code{get_layer} will return the ith layer that uses the stat.
#' }
#'
#' @param p A ggplot object
#' @param geom A character string found in the suffix of a ggplot2 geom function,
#'  e.g. \code{"point"}.
#' @param stat A character string found in the suffix of a ggplot2 stat function,
#'  e.g. \code{"bin"}.
#' @param i A numerical index, e.g. \code{1}.
#'
#' @return An object with class \code{layer_to_check} to be manipulated further
#'  with ggcheck functions.
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   stat_smooth()
#'
#' get_layer(p, i = 1)
#' get_layer(p, geom = "point")
#' get_layer(p, stat = "smooth")
#' @noRd
get_layer <- function(p, geom = NULL, stat = NULL, i = NULL) {
  stop_if_not_ggplot(p)

  if (!is.null(geom) && !is.null(stat)) {
    stop("Grading error: cannot identify a layer with a combination of geom and stat name. Please pick one or the other.")
  }

  if (is.null(geom) && is.null(stat)) {
    # index is a position
    index <- i
  } else if (!is.null(geom)) {
    # index is a geom layer
    geom <- map_geom(geom)$GEOM
    if (is.null(i)) {
      index <- which(get_geoms(p) == geom)[1]
    } else {
      index <- which(get_geoms(p) == geom)[i]
    }
  } else if (!is.null(stat)) {
    # index is a stat layer
    stat <- map_stat(stat)$STAT
    if (is.null(i)) {
      index <- which(get_stats(p) == stat)[1]
    } else {
      index <- which(get_stats(p) == stat)[i]
    }
  }

  # index has to be valid
  if (index > length(p$layers)) {
    stop("Grading error: cannot find specified layer. Use checks to check that desired layer exists before inspecting the layer.")
  }

  l <- list(
    layer = p$layers[[index]],
    global_data = get_data(p),
    global_mapping = get_mappings(p)
  )
  structure(l, class = "layer_to_check")
}

#' Isolate a geom layer from a plot
#'
#' \code{get_geom_layer} returns a geom layer from a plot along with the global data sets
#' and aesthetic mappings that the layer may inherit from.
#'
#' Users can specify a layer in one of 3 ways:
#'
#' \enumerate{
#'  \item By order of appearance with \code{i}. The first layer to appear in the
#'  plot (the one drawn first, on the bottom) corresponds to \code{i = 1}.
#'  \item By type of geom with \code{geom}. \code{get_geom_layer} will return the
#'  first layer that uses the geom.
#'  \item By a combination of \code{geom} and
#'  \code{i}. \code{get_geom_layer} will return the ith layer that uses the geom.
#' }
#'
#' @param p A ggplot object
#' @param geom A character string found in the suffix of a ggplot2 geom function,
#'  e.g. \code{"point"}.
#' @param i A numerical index, e.g. \code{1}.
#'
#' @return An object with class \code{layer_to_check} to be manipulated further
#'  with ggcheck functions.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(color = "red") +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth(se = FALSE)
#'
#' get_geom_layer(p, i = 1)
#' get_geom_layer(p, geom = "smooth")
#' get_geom_layer(p, geom = "point", i = 2)
get_geom_layer <- function(p, geom = NULL, i = NULL) {
  stop_if_not_ggplot(p)

  if (is.null(geom) && is.null(i)) {
    stop("Grading error: cannot identify which layer to grade. Please specify at least one of geom or i.")
  }

  get_layer(p, geom = geom, i = i)
}

#' Isolate a stat layer from a plot
#'
#' \code{get_stat_layer} returns a stat layer from a plot along with the global data sets
#' and aesthetic mappings that the layer may inherit from.
#'
#' Users can specify a layer in one of 3 ways:
#'
#' \enumerate{
#'  \item By order of appearance with \code{i}. The first layer to appear in the
#'  plot (the one drawn first, on the bottom) corresponds to \code{i = 1}.
#'  \item By type of stat with \code{stat}. \code{get_stat_layer} will return the
#'  first layer that uses the stat
#'  \item By a combination of \code{stat} and
#'  \code{i}. \code{get_stat_layer} will return the ith layer that uses the stat
#' }
#'
#' @param p A ggplot object
#' @param stat A character string found in the suffix of a ggplot2 stat function,
#'  e.g. \code{"bin"}.
#' @param i A numerical index, e.g. \code{1}.
#'
#' @return An object with class \code{layer_to_check} to be manipulated further
#'  with ggcheck functions.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = diamonds, aes(price)) +
#'   stat_bin(bins = 20, binwidth = 500)
#'
#' get_stat_layer(p, i = 1)
#' get_stat_layer(p, stat = "bin")
get_stat_layer <- function(p, stat = NULL, i = NULL) {
  stop_if_not_ggplot(p)

  if (is.null(stat) && is.null(i)) {
    stop("Grading error: cannot identify which layer to grade. Please specify at least one of stat or i.")
  }

  get_layer(p, stat = stat, i = i)
}

is_layer_to_check <- function(x) inherits(x, "layer_to_check")
