#' Are aesthetic mapping specifications "identical"?
#'
#' The ggplot2 package uses quosures to record aesthetic mappings. These record
#' both the mapping described as well as the environment in which the mapping
#' was described. As a result, it is difficult to compare mappings created by
#' students in one environment to mappings created on the fly by graders in
#' another environment. \code{identical_aes} facilitates comparison by ignoring
#' the environments associated with an aesthetic mapping specification. If the
#' two specifications contain identical expressions, e.g. \code{x = displ},
#' etc., \code{identical_aes} returns \code{TRUE}.
#'
#' @param a1 The output of \code{\link[ggplot2]{aes}}, perhaps extracted from a ggplot object.
#' @param a2 The output of \code{\link[ggplot2]{aes}}, perhaps extracted from a ggplot object.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family functions for checking mappings
#'
#' @export
identical_aes <- function(a1, a2) {
  # remove environments
  a1 <- lapply(a1, `attributes<-`, NULL)
  a2 <- lapply(a2, `attributes<-`, NULL)
  identical(a1, a2)
}


aes_c <- function(a1, a2) {
  aesthetics <- names(a2)
  a1[aesthetics] <- a2
  a1
}

#' Get aesthetic mappings from a layer or plot
#'
#' \code{get_mappings} returns the mappings used by a ggplot object or a single
#' layer extracted from the object with \code{\link{get_layer}}.
#'
#' When passed a ggplot object (i.e. a plot), \code{get_mappings} will return
#' only the mappings that have been set globally with
#' \code{\link[ggplot2]{ggplot}}. When passed a single layer from a plot, the
#' behavior of \code{get_mappings} will depend on the value of
#' \code{local_only}. If \code{local_only = TRUE}, \code{get_mappings} will
#' return only the mappings defined locally in a layer. When \code{local_only =
#' FALSE}, \code{get_mappings} will return the combination of global and local
#' methods that will be used to plot a layer.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param local_only \code{TRUE} or \code{FALSE}. Should \code{get_mappings}
#'   return only the mappings defined locally in a layer. This has no effect
#'   when \code{p} is a ggplot object.
#'
#' @return A list with class uneval, as returned by \code{\link[ggplot2]{aes}}
#'   Components of the list are either quosures or constants.
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class))
#' get_mappings(p)
#' get_mappings(get_layer(p, i = 1), local_only = FALSE)
get_mappings <- function(p, local_only = FALSE) {
  UseMethod("get_mappings")
}

#' @export
get_mappings.ggplot <- function(p, local_only = FALSE) {
  p$mapping
}

#' @export
get_mappings.layer_to_check <- function(p, local_only = FALSE) {
  local_mappings <- p$layer$mapping

  if (local_only) {
    return(local_mappings)
  } else {
    return(aes_c(p$global_mapping, local_mappings))
  }
}

#' Does a plot or layer use one or more mappings?
#'
#' \code{uses_mappings} checks whether the student used one or more mappings in
#' their plot. Unlike, \code{\link{mappings_match}}, \code{use_mappings} ignores
#' whether or not the student also supplied additional mappings.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param local_only If \code{TRUE}, \code{uses_mappings} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{uses_mappings} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A logical value.
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class))
#' uses_mappings(p, aes(x = displ))
#' uses_mappings(get_layer(p, i = 1), aes(x = displ, color = class), local_only = FALSE)
#' uses_mappings(get_layer(p, i = 1), aes(x = displ, color = class), local_only = TRUE)
uses_mappings <- function(p, mappings, local_only = FALSE) {
  aes_map <- get_mappings(p, local_only)
  all(names(mappings) %in% names(aes_map)) &&
    identical_aes(mappings, aes_map[names(mappings)])
}

#' Do the mappings of a plot or layer exactly match the set supplied?
#'
#' \code{mappings_match} checks whether the student used the exact set of
#' supplied mappings in their plot.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param local_only If \code{TRUE}, \code{mappings_match} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{mappings_match} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A logical value
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class))
#' mappings_match(p, aes(x = displ, y = hwy))
#' mappings_match(get_layer(p, i = 1), aes(x = displ,  y = hwy, color = class), local_only = FALSE)
#' mappings_match(get_layer(p, i = 1), aes(x = displ, y = hwy, color = class), local_only = TRUE)
mappings_match <- function(p, mappings, local_only = FALSE) {
  identical_aes(mappings, get_mappings(p, local_only))
}

#' Return the aesthetic mappings used by the ith layer
#'
#' \code{ith_mappings} returns the mappings used by a ggplot object or a single
#' layer extracted from the object with \code{\link{get_layer}}.
#'
#' Functions that use the \code{ith_} prefix are
#' designed to eliminate the need to call \code{get_layer} to check a specific
#' layer in a plot, e.g. \code{p %>% get_layer(geom = "point") %>% get_mappings()}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_mappings_use} will check the
#'   aesthetics used by the ith layer.
#' @param local_only If \code{TRUE}, \code{ith_mappings_use} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{ith_mappings_use} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A list with class uneval, as returned by \code{\link[ggplot2]{aes}}
#'   Components of the list are either quosures or constants.
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' ith_mappings(p, i = 1, local_only = FALSE)
#' ith_mappings(p, i = 1, local_only = TRUE)
#' ith_mappings(p, i = 2, local_only = FALSE)
ith_mappings <- function(p, i, local_only = FALSE) {
  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  get_mappings(get_layer(p, i = i), local_only)
}

#' Does the ith layer use one or more aesthetic mappings?
#'
#' \code{ith_mappings_use} checks whether the student uses the supplied mappings
#' in the ith layer of their plot. Unlike \code{\link{ith_mappings_match}},
#' \code{ith_mappings_use} ignores whether or not the student supplied
#' additional mappings as well. Functions that use the \code{ith_} prefix are
#' designed to eliminate the need to call \code{get_layer} to check a specific
#' layer in a plot, e.g. \code{p %>% get_layer(geom = "point") %>%
#' uses_mappings(aes(color = class))}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_mappings_use} will check the
#'   aesthetics used by the ith layer.
#' @param local_only If \code{TRUE}, \code{ith_mappings_use} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{ith_mappings_use} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A logical value
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' ith_mappings_use(p, i = 1, aes(x = displ), local_only = FALSE)
#' ith_mappings_use(p, i = 1, aes(x = displ), local_only = TRUE)
#' ith_mappings_use(p, i = 2, aes(x = displ, y = hwy), local_only = FALSE)
ith_mappings_use <- function(p, mappings, i, local_only = FALSE) {
  aes_map <- get_mappings(get_layer(p, i = i), local_only)
  all(names(mappings) %in% names(aes_map)) &&
    identical_aes(mappings, aes_map[names(mappings)])
}

#' Do the mappings of the ith layer exactly match the set supplied?
#'
#' \code{ith_mappings_match} checks whether the student used the exact set of
#' supplied mappings in the ith layer of their plot. Functions that use the
#' \code{ith_} prefix are designed to eliminate the need to call
#' \code{get_layer} to check a specific layer in a plot, e.g. \code{p %>%
#' get_layer(geom = "point") %>% mappings_match(aes(color = class))}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_mappings_match} will check the
#'   aesthetics used by the ith layer.
#' @param local_only If \code{TRUE}, \code{ith_mappings_match} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{ith_mappings_match} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A logical value
#'
#' @family functions for checking mappings
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth()
#' ith_mappings_match(p, i = 1, aes(x = displ,  y = hwy, color = class), local_only = FALSE)
#' ith_mappings_match(p, i = 1, aes(color = class), local_only = TRUE)
#' ith_mappings_match(p, i = 2, aes(x = displ, y = hwy), local_only = FALSE)
ith_mappings_match <- function(p, mappings, i, local_only = FALSE) {
  identical_aes(mappings, get_mappings(get_layer(p, i = i), local_only))
}

