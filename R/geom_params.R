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
#' @param i A numerical index, e.g. \code{1}.
#'
#' @return A named logical vector of the same length as the number of inputs
#'   to `...`.
#' @export
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
uses_geom_params <- function(p, geom, ..., params = NULL, i = NULL) {
  stop_if_not_ggplot(p)

  layer <- get_geom_layer(p, geom = geom, i = i)$layer

  params <- c(params, capture_dots(...))
  named  <- names(params) != ""

  user_params         <- names(params)
  user_params[!named] <- as.character(params[!named])

  result        <- logical(length(params))
  names(result) <- user_params

  user_params[user_params == "color"] <- "colour"

  # collect geom, stat, and aes parameters
  all_params <- c(layer$geom_params, layer$stat_params, layer$aes_params)

  result[named] <- purrr::map2_lgl(
    params[named], all_params[user_params][named], identical
  )
  result[!named] <- user_params[!named] %in% names(all_params)
  result
}

#' @rdname uses_geom_params
#' @export
uses_geom_param <- uses_geom_params
