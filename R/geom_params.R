#' Does a layer use one of more specific parameters?
#'
#' \code{uses_geom_params} checks that a plot's geom layer uses a specific parameter.
#'
#' To specify a specific geom layer, either specify using position using the \code{i} index or
#' by using a combination of \code{geom} function suffix name and \code{i} to check the ith layer that
#' uses the geom.
#'
#' The \code{params} argument accepts a list that contains geom, stat, or aes
#' parameters. This offers flexibility in certain situations where setting a
#' parameter on a \code{geom_} function is actually setting a stat parameter or
#' aes parameter. For example, in \code{geom_histogram(binwidth = 500)}, the
#' \code{binwidth} is a stat parameter, while in
#' \code{geom_histogram(fill = "blue")}, the \code{fill} is an aes parameter.
#' \code{uses_geom_params} will take this into account and check geom, stat, and
#' aes parameters.
#'
#' Note that `uses_geom_params()` can detect aes _parameters_, but not aes
#' _mappings_. Parameters are set to static values directly within a layer (e.g.
#' `geom_point(color = "blue")`), while mappings associate variables in the data with plot aesthetics using
#' [`aes()`][ggplot2::aes] (e.g. `geom_point(aes(color = class))`).
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = diamonds, aes(x = cut, y = price)) +
#'   geom_boxplot(varwidth = TRUE, outlier.alpha = 0.01, fill = "blue")
#'
#' uses_geom_params(
#'   p, "boxplot", list(varwidth = TRUE, outlier.alpha = 0.01, fill = "blue")
#' )
#'
#' uses_geom_params(
#'   p, "boxplot", varwidth = TRUE, outlier.alpha = 0.01, fill = "blue"
#' )
#'
#' # Unnamed arguments check that a parameter is set to any value
#' uses_geom_params(p, "boxplot", "fill")
#' @param p A ggplot object
#' @param geom A character string found in the suffix of a ggplot2 geom function,
#'  e.g. \code{"point"}.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Named values or [character] strings.
#'   Unnamed arguments will check whether any value was set for that parameter.
#'   Named arguments will check whether the parameter with the same name has a
#'   matching value.
#'   Each argument should have a name matching a [ggplot][ggplot2::ggplot]
#'   layer parameter.
#'   Values may be passed as arguments or as list elements.
#' @param params A named list of geom or stat parameter values, e.g.
#'   \code{list(outlier.alpha = 0.01)}.
#'   This list is combined with any inputs to `...`
#' @inheritParams get_geom_layer
#'
#' @return A named logical vector of the same length as the number of inputs
#'   to `...`.
#' @family functions for checking geom parameters
#' @export
uses_geom_params <- function(p, geom, ..., params = NULL, i = NULL) {
  stop_if_not_ggplot(p)

  layer <- get_geom_layer(p, geom = geom, i = i)$layer

  params <- c(params, flatten_dots(...))
  named  <- names(params) != ""

  user_params         <- names(params)
  user_params[!named] <- as.character(params[!named])

  default_params <- purrr::map_lgl(params, inherits, ".default_params")
  params[default_params] <- purrr::map(
    names(params)[default_params],
    ~ unlist(unname(default_params(p, geom, ., i = i)))
  )

  result        <- logical(length(params))
  names(result) <- user_params

  user_params[user_params == "color"] <- "colour"

  # Collect geom, stat, and aes parameters
  all_params <- c(layer$geom_params, layer$stat_params, layer$aes_params)

  # Add inherited default parameters
  default_params <- default_params(p, geom)
  inherited <- !names(default_params) %in% names(all_params)
  all_params_with_inherited <- c(all_params, default_params[inherited])

  result[named] <- purrr::map2_lgl(
    params[named], all_params_with_inherited[user_params][named], identical
  )
  result[!named] <- user_params[!named] %in% names(all_params)
  result
}

#' @rdname uses_geom_params
#' @export
uses_geom_param <- uses_geom_params

#' What are the default parameters for a plot layer?
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_smooth(aes(color = class))
#'
#' # Returns the parameters the ggplot would use by default for a layer
#' default_params(p, "smooth", "linetype")
#' default_params(p, "smooth", c("se", "level"))
#' default_params(p, "smooth")
#'
#' # If a parameter does not exist, returns NULL
#' default_params(p, "smooth", "shape")
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' default_params(p, "smooth", "color")
#' default_params(p, "smooth", "colour")
#' @inheritParams uses_geom_params
#' @param params A [character] vector.
#'   `default_params()` returns the default parameter value with a name matching
#'   each string in `params`.
#'   If `params` is [`NULL`] (the default), the default values for
#'   all parameters are returned.
#'
#' @return A named [list] of the same length as `params`, or, if `params` is
#'   [`NULL`], a named list of default values for all parameters of `geom`.
#' @family functions for checking geom parameters
#' @export
default_params <- function(p, geom, params = NULL, i = NULL) {
  UseMethod("default_params")
}

#' @export
default_params.default <- function(p, geom, params = NULL, i = NULL) {
  if (!missing(p)) {
    stop_if_not_ggplot()
  }

  structure(list(), class = c(".default_params", "ggcheck_placeholder"))
}

#' @export
default_params.ggplot <- function(p, geom, params = NULL, i = NULL) {
  layer <- get_geom_layer(p, geom = geom, i = i)$layer

  if (!is.character(params) && !is.null(params)) {
    stop(
      "`params` must be a character vector or `NULL`.",
      call. = FALSE
    )
  }

  names(params) <- params
  params[params == "color"] <- "colour"

  snake_class <- utils::getFromNamespace("snake_class", "ggplot2")

  default_geom <- utils::getFromNamespace(snake_class(layer$geom), "ggplot2")()
  default_stat <- utils::getFromNamespace(snake_class(layer$stat), "ggplot2")()

  result <- c(
    default_geom$geom$default_aes,
    default_geom$geom_params,
    default_geom$stat_params,
    default_stat$geom$default_aes,
    default_stat$geom_params,
    default_stat$stat_params
  )

  # Remove duplicate entries
  # (some params have the same default in geom_params and stat_params)
  result <- result[unique(names(result))]

  if (length(params)) {
    result <- result[params]
    names(result) <- names(params)
  }

  result
}
