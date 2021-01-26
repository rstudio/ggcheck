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
  # ignore environments associated with the aesthetics
  a1 <- lapply(a1, rlang::as_name)
  a2 <- lapply(a2, rlang::as_name)
  identical(a1, a2)
}

aes_c <- function(a1, a2) {
  # TODO document this
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
#' their plot. \code{uses_mappings} by default ignores whether or not the student
#' also supplied additional mappings. If \code{exact} is \code{TRUE}, then all mappings
#' have to match.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
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
#' uses_mappings(get_layer(p, i = 1), aes(x = displ, color = class), local_only = FALSE)
#' uses_mappings(get_layer(p, i = 1), aes(x = displ, color = class), local_only = TRUE)
#' uses_mappings(p, aes(x = displ, y = hwy), exact = TRUE)
uses_mappings <- function(p, mappings, local_only = FALSE, exact = FALSE) {
  #TODO-Nischal for exact case, should we return how many items don't match, e.g. a list(matched = TRUE, extra = 1)
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
#'   \code{\link{get_layer}}.
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
  mapping_names <- names(mappings)
  aes_names <- names(aes_map)
  return(length(aes_names[!(aes_names %in% mapping_names)]) > 0)
}

#' Does a plot use one or more variables?
#'
#' TODO-Nischal need to fix error when running example, suspicion: get_layer breaks because it
#' is relying on get_geom and is not aware of get_stats cases. Might be worth rethinking the
#' entire geom_ vs stat_ dynamics.
#'
#' \code{uses_variables} checks whether the student used one or more variables in
#' their plot aesthetics. By default, \code{uses_variables} requires that only one of the
#' variables need to be used. Set \code{exact} to \code{TRUE} to check if all of the variables
#' have to be used.
#'
#' @param p A ggplot object or a layer extracted from a ggplot object with
#'   \code{\link{get_layer}}.
#' @param vars character vector of variables to check for, e.g. c("x")
#' @param i the ith layer to check
#' @param exact If \code{TRUE}, variables need to be mapped exactly
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(data = diamonds, aes(x = cut, sample = price)) +
#'   geom_qq()
#' uses_variables(p, c("x", "y"))
uses_variables <- function(p, vars, i = 1, exact = FALSE) {
  layers <- get_layer(p, i = i)
  # TODO-Nischal do we always want to grab all variables or be able to isolate certain layer?
  pmaps_names <- c(names(layers$global_mapping), names(layers$layer$mapping))
  # NOTE: ggplot2 seems to switch aesthetic color to colour, so we standardize here
  pmaps_names[which(pmaps_names == "colour")] <- "color"
  vars[which(vars == "colour")] <- "color"
  matches <- vars %in% pmaps_names
  if (exact) {
    return(identical(vars, pmaps_names))
  } else {
    return(any(matches))
  }
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
#' in the ith layer of their plot.
#'
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

# TODO implement a function that checks if student provided extra mappings?

