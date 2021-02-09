is_empty_data <- function(d) {
  identical(d, structure(list(), class = "waiver"))
}

get_global_data_from_layer <- function(l) {
  stopifnot(is_layer_to_check(l))

  data <- l$global_data

  if (is_empty_data(data)) {
    return(NULL)
  }

  data
}

#' Get the data set used by a plot or layer
#'
#' \code{get_data} returns the data set used by a ggplot object or a single
#' layer extracted from the object with \code{\link{get_layer}}.
#'
#' When passed a ggplot object (i.e. a plot), \code{get_data} will return only
#' the data that has been set globally with \code{\link[ggplot2]{ggplot}}.
#'
#' When passed a single layer from a plot, the behavior of \code{get_data} will
#' depend on the \code{local_only} argument passed to \code{...}. If
#' \code{local_only = TRUE}, \code{get_data} will return only the data set, if
#' any, that was defined locally in the function that created the layer. If
#' \code{local_only = FALSE}, \code{get_data} will return the data used by the
#' layer, whether or not that data was defined globally in
#' \code{\link[ggplot2]{ggplot}} or locally.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param local_only \code{TRUE} or \code{FALSE}. Should \code{get_data} onbly
#'   return data defined locally in the layer?
#'
#' @return A data frame. If no data set is found, \code{get_data} returns
#'   \code{NULL}
#'
#' @family functions for checking data
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' d2 <- head(mpg)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(data = d2, color = "red") +
#'   geom_point()
#' get_data(p)
#' get_data(get_layer(p, i = 1))
get_data <- function(p, local_only = FALSE) {
  UseMethod("get_data")
}

#' @export
get_data.ggplot <- function(p, local_only = FALSE) {
  p$data
}

#' @export
get_data.layer_to_check <- function(p, local_only = FALSE) {
  data <- p$layer$data

  # if no local data
  if (is_empty_data(data)) {
    if (local_only) {
      return(NULL)
    } else {
      return(get_global_data_from_layer(p))
    }
  }

  data
}


#' Does a plot or layer use the correct data set?
#'
#' \code{uses_data} checks whether the data set used by a plot or layer matches
#' the data set provided.
#'
#' When passed a ggplot object (i.e. a plot), \code{uses_data} will check only
#' the data that has been set globally with \code{\link[ggplot2]{ggplot}}.
#'
#' When passed a single layer from a plot, the behavior of \code{uses_data} will
#' depend on the \code{local_only} argument passed to \code{...}. If
#' \code{local_only = TRUE}, \code{uses_data} will check only the data set, if
#' any, that was defined locally in the function that created the layer. If
#' \code{local_only = FALSE}, \code{uses_data} will check the data used by the
#' layer, whether or not that data was defined globally in
#' \code{\link[ggplot2]{ggplot}} or locally.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param data A data frame
#' @param local_only \code{TRUE} or \code{FALSE}. See the details.
#'
#' @return A data frame.
#'
#' @family functions for checking data
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' d2 <- head(mpg)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(data = d2, color = "red") +
#'   geom_point()
#' uses_data(p, mpg)
#' uses_data(get_layer(p, i = 1), data = head(mpg))
uses_data <- function(p, data, local_only = FALSE) {
  identical(data, get_data(p, local_only))
}

#' Which data set does the ith layer use?
#'
#' \code{ith_data} returns the data set used by the ith layer.
#'
#' If \code{local_only = TRUE}, \code{ith_data} returns the data set,
#' if any, that was defined locally in the function that created the ith layer.
#' If \code{local_only = FALSE}, \code{ith_data} returns the data used by
#' the ith layer, whether or not that data was defined globally in
#' \code{\link[ggplot2]{ggplot}} or locally.
#'
#' Functions that use the \code{ith_} prefix are designed to eliminate the need
#' to call \code{get_layer} to check a specific layer in a plot, e.g. \code{p
#' %>% get_layer(geom = "point") %>% get_data()}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on.
#' @param local_only \code{TRUE} or \code{FALSE}. See the details.
#'
#' @return A data frame. If no data set is found, \code{ith_data} returns \code{NULL}.
#'
#' @family functions for checking data
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' d2 <- head(mpg)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(data = d2, color = "red") +
#'   geom_point()
#' ith_data(p, i = 1)
ith_data <- function(p, i, local_only = FALSE) {
  if (!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  get_data(get_layer(p, i = i), local_only)
}

#' Does the ith layer use the correct data set?
#'
#' \code{ith_data_is} checks whether the student uses the supplied data set for
#' the ith layer of their plot.
#'
#' Functions that use the \code{ith_} prefix are designed to eliminate the need
#' to call \code{get_layer} to check a specific layer in a plot, e.g. \code{p
#' %>% get_layer(geom = "point") %>% uses_data(mpg)}.
#'
#' If \code{local_only = TRUE}, \code{ith_data_is} will check only the data set,
#' if any, that was defined locally in the function that created the ith layer.
#' If \code{local_only = FALSE}, \code{ith_data_is} will check the data used by
#' the ith layer, whether or not that data was defined globally in
#' \code{\link[ggplot2]{ggplot}} or locally.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param data A data frame
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on.
#' @param local_only \code{TRUE} or \code{FALSE}. See the details.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking data
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' d2 <- head(mpg)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(data = d2, color = "red") +
#'   geom_point()
#' ith_data_is(p, data = head(mpg), i = 1)
ith_data_is <- function(p, data, i, local_only = FALSE) {
  identical(data, ith_data(p, i, local_only))
}
