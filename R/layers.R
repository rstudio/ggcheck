
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

#'Isolate a layer from a plot
#'
#' TODO-Nischal layer can be either geom/stat, we should rethink about how to
#' make this more general to handle both.
#'
#'\code{get_layer} returns a layer from a plot along with the global data sets
#'and aesthetic mappings that the layer may inherit from.
#'
#'Users can specify a layer in one of three ways:
#'
#'\enumerate{
#'  \item By order of appearance with \code{i}. The first layer to appear in the
#'  plot (the one drawn first, on the bottom) corresponds to \code{i = 1}.
#'  \item By type of geom with \code{geom}. \code{get_layer} will return the
#'  first layer that uses the geom.
#'  \item By a combination of \code{geom} and
#'  \code{i}. \code{get_layer} will return the ith layer that uses the geom.
#'}
#'
#'@param p A ggplot object
#'@param geom A character string found in the suffix of a ggplot2 geom function,
#'  e.g. \code{"point"}.
#'  TODO-Nischal do we want to do suffix? seems easier to stick to function names...
#'@param i A numerical index, e.g. \code{1}.
#'
#'@return An object with class \code{layer_to_check} to be manipulated further
#'  with ggcheck functions.
#'@export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#'
#' get_layer(p, i = 1)
#' get_layer(p, geom = "smooth")
get_layer <- function(p, geom = NULL, i = NULL) {

  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  if (is.null(geom) && is.null(i)) {
    stop("Grading error: cannot identify which layer to grade. (For the grader) please specify at least one of geom or i.")
  } else if (is.null(geom)) {
    index <- i
  } else if (is.null(i)) {
    index <- which(get_geoms(p) == geom)[1]
  } else {
    index <- which(get_geoms(p) == geom)[i]
  }

  # index may be `NA` so we can check for that
  if (index > length(p$layers)) {
    stop("Grading error: cannot find specified layer. (For the grader) use checks to check that desired layer exists before inspecting the layer.")
  }

  l <- list(
    layer = p$layers[[index]],
    global_data = get_data(p),
    global_mapping = get_mappings(p)
  )
  structure(l, class = "layer_to_check")
}

is_layer_to_check <- function(x) inherits(x, "layer_to_check")
