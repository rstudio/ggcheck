#' Placeholders for default values
#'
#' @description
#' These functions generate placeholder values.
#' - `default_label()` can be used as a named argument in [uses_labels()]
#'   to check that a label matches the result of [get_default_labels()]
#'   with that name.
#' - `default_param()` can be used as a named argument in [uses_geom_params()]
#'   to check that a parameter matched the result of [get_default_params()]
#'   with that name.
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = trans)) +
#'   geom_smooth(se = FALSE) +
#'   labs(title = "My plot", x = "Weight", y = "MPG")
#'
#' uses_labels(p, x = default_label(), color = default_label())
#'
#' uses_geom_params(p, "smooth", size = default_param(), se = default_param())
#' @return A placeholder value to be used within [uses_labels()]
#'   or [uses_geom_params()].
#' @export
default_label <- function() {
  structure(list(), class = c(".default_label", "ggcheck_placeholder"))
}

#' @rdname default_label
#' @export
default_param <- function() {
  structure(list(), class = c(".default_param", "ggcheck_placeholder"))
}
