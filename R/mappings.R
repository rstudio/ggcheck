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
  # strip environments associated with the aesthetics before comparing
  a1 <- lapply(a1, deparse)
  a2 <- lapply(a2, deparse)
  identical(a1, a2)
}

aes_c <- function(a1, a2) {
  # override the a1 aesthetics with the a2 aesthetics
  # NOTE: this is used internally for `get_mappings.layer_to_check` when
  # retrieving the aesthetics for a particular layer by overriding global aesthetics.
  aesthetics <- names(a2)
  a1[aesthetics] <- a2
  a1
}

#' Get aesthetic mappings from a layer or plot
#'
#' \code{get_mappings} returns the mappings used by a ggplot object or a single
#' layer extracted from the object with \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
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
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
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
#' get_mappings(get_geom_layer(p, i = 1), local_only = FALSE)
get_mappings <- function(p, local_only = FALSE) {
  UseMethod("get_mappings")
}

#' @export
get_mappings.ggplot <- function(p, local_only = FALSE) {
  global_map <- p$mapping

  if (local_only) {
    return(global_map)
  }

  layer_maps <- purrr::map(p$layers, "mapping")

  if (length(layer_maps) < 1) {
    return(global_map)
  }

  layer_names <- purrr::reduce(purrr::map(layer_maps, names), intersect)

  if (length(layer_names) < 1) {
    return(global_map)
  }

  layer_names <- purrr::set_names(layer_names)

  layer_maps_ubiquitous <- purrr::map(layer_names, function(name) {
    # If the aesthetic has the same value across all layers, return its
    # value in the first layer; otherwise return NULL
    if (all_identical(purrr::map(layer_maps, name))) {
      layer_maps[[1]][[name]]
    }
  })

  aes_map <- c(global_map, purrr::compact(layer_maps_ubiquitous))
  class(aes_map) <- "uneval"
  aes_map
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
#' their plot. By default, \code{uses_mappings} ignores whether or not the student
#' also supplied additional mappings. Use \code{uses_extra_mappings} to check if they did.
#' If \code{exact} is \code{TRUE}, then all of the mappings have to match exactly.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param local_only If \code{TRUE}, \code{uses_mappings} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{uses_mappings} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#' @param exact If \code{TRUE}, mappings need to be mapped exactly
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
#' uses_mappings(get_geom_layer(p, i = 1), aes(x = displ, color = class), local_only = FALSE)
#' uses_mappings(get_geom_layer(p, i = 1), aes(x = displ, color = class), local_only = TRUE)
#' uses_mappings(p, aes(x = displ, y = hwy), exact = TRUE)
uses_mappings <- function(p, mappings, local_only = FALSE, exact = FALSE) {
  aes_map <- get_mappings(p, local_only)
  mapping_names <- names(mappings)
  if (exact) {
    return(identical_aes(mappings, get_mappings(p, local_only)))
  } else {
    return(
      all(mapping_names %in% names(aes_map)) && identical_aes(mappings, aes_map[mapping_names])
    )
  }
}

#' Does the plot uses extra aesthetic mappings?
#'
#' \code{uses_extra_mappings} checks if a student's plot contains more than the
#' required aesthetic mappings. Note that we still return \code{TRUE} if
#' the student's plot differs from the required aesthetic mappings because they
#' are technically extra mappings from required set. We recommend you use
#' \code{uses_mapping} checks for checking required mappings before \code{uses_extra_mappings}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param local_only If \code{TRUE}, \code{uses_extra_mappings} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{uses_extra_mappings} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = diamonds, aes(x = cut, sample = price)) +
#'   geom_qq()
#' uses_extra_mappings(p, aes(sample = price))
uses_extra_mappings <- function(p, mappings, local_only = FALSE) {
  aes_map <- get_mappings(p, local_only)
  aes_names <- names(aes_map)
  mapping_names <- names(mappings)
  # the plot has any variables beyond target mappings
  any(!(aes_names %in% mapping_names))
}

#' Does a plot use one or more aesthetics?
#'
#' \code{uses_aesthetics} checks whether the student used one or more aesthetics.
#'
#' By default, \code{uses_aesthetics} requires that only one of the
#' aesthetics need to be used. Set \code{exact} to \code{TRUE} to check if all of
#' the variables have to be matched exactly.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}..
#' @param aesthetics character vector of variables to check for, e.g. "x" or c("x")
#' @param exact If \code{TRUE}, variables need to be mapped exactly
#' @param local_only \code{TRUE} or \code{FALSE}. Should \code{uses_aesthetics} only
#'   return mappings defined locally in the layer?
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class))
#' uses_aesthetics(p, "x")
#' uses_aesthetics(p, c("x", "y"))
#' uses_aesthetics(get_geom_layer(p, "point"), c("x", "y", "color"), local_only = TRUE)
#' uses_aesthetics(get_geom_layer(p, "point"), c("x", "y"), local_only = FALSE)
uses_aesthetics <- function(p, aesthetics, local_only = FALSE, exact = FALSE) {
  pmaps_names <- names(get_mappings(p, local_only = local_only))
  # NOTE: ggplot2 seems to switch aesthetic color to colour, so we standardize it to 'color'
  pmaps_names[which(pmaps_names == "colour")] <- "color"
  aesthetics[which(aesthetics == "colour")] <- "color"
  if (exact) {
    return(identical(aesthetics, pmaps_names))
  } else {
    return(any(aesthetics %in% pmaps_names))
  }
}

#' Return the aesthetic mappings used by the ith layer
#'
#' \code{ith_mappings} returns the mappings used by a ggplot object or a single
#' layer extracted from the object with \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
#'
#' Functions that use the \code{ith_} prefix are
#' designed to eliminate the need to call \code{get_layer} to check a specific
#' layer in a plot, e.g. \code{p %>% get_geom_layer(geom = "point") %>% get_mappings()}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
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
  stop_if_not_ggplot(p)
  get_mappings(get_layer(p, i = i), local_only)
}

#' Does the ith layer use one or more aesthetic mappings?
#'
#' \code{ith_mappings_use} checks whether the student uses the supplied mappings
#' in the ith layer of their plot.
#'
#' \code{ith_mappings_use} ignores whether or not the student supplied
#' additional mappings as well. Functions that use the \code{ith_} prefix are
#' designed to eliminate the need to call \code{get_layer} to check a specific
#' layer in a plot, e.g. \code{p %>% get_geom_layer(geom = "point") %>%
#' uses_mappings(aes(color = class))}.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_geom_layer}} or \code{\link{get_stat_layer}}.
#' @param mappings One or more aesthetic mappings created with
#'   \code{\link[ggplot2]{aes}}.
#' @param i A numerical index that corresponds to the first layer of a plot (1),
#'   the second layer (2), and so on. \code{ith_mappings_use} will check the
#'   aesthetics used by the ith layer.
#' @param local_only If \code{TRUE}, \code{ith_mappings_use} will check only the
#'   mappings defined locally in a layer for the presence of \code{mappings}. If
#'   \code{FALSE}, \code{ith_mappings_use} will check for \code{mappings} in the
#'   combination of global and local methods that will be used to plot a layer.
#' @param exact If \code{TRUE}, mappings need to be mapped exactly
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
ith_mappings_use <- function(p, mappings, i, local_only = FALSE, exact = FALSE) {
  layer <- get_layer(p, i = i)
  aes_map <- get_mappings(layer, local_only)
  if (exact) {
    return(identical_aes(mappings, aes_map))
  } else {
    return(
      all(names(mappings) %in% names(aes_map)) &&
        identical_aes(mappings, aes_map[names(mappings)])
    )
  }
}
