#' List the labels used by a plot
#'
#' `get_labels()` returns a named [list] of [labels][ggplot2::labs],
#' written as [character] strings, indicating which labels are used by a plot.
#'
#' Note that `get_labels()` will return [`NULL`] if a label is explicitly set to
#' [`NULL`] ***or*** if a requested aesthetic is not present in the plot.
#'
#' @param p A [ggplot][ggplot2::ggplot] object
#' @param aes If `aes` is a [character] vector, returns only the labels
#'   corresponding to the included aesthetics.
#'   Defaults to [`NULL`], which returns all labels.
#'
#' @return A named list of character strings.
#'
#' @family functions for checking labels
#' @export
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth() +
#'   labs(x = "Weight", y = "MPG", color = NULL)
#'
#' get_labels(p)
#' get_labels(p, c("x", "y"))
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' get_labels(p, "color")
#' get_labels(p, "colour")
get_labels <- function(p, aes = NULL) {
  if (is.null(aes)) {return(p$labels)}

  aes[aes == "color"] <- "colour"

  p$labels[aes]
}

#' Does a plot use one or more labels?
#'
#' `uses_labels()` tests whether a plot uses one or more [labels][ggplot2::labs].
#'
#' Note that `uses_labels()` will match [`NULL`] if a label is explicitly set to
#' [`NULL`] ***or*** if a requested aesthetic is not present in the plot.
#'
#' @param p A ggplot object
#' @param ... Named [character] strings.
#'   Each argument should have a name matching a [ggplot][ggplot2::ggplot]
#'   [aesthetic][ggplot2::aes] and a value matching the expected label.
#'
#' @return [`TRUE`], if all labels match arguments to `...`, or [`FALSE`] if at
#'   least one label does not match
#'
#' @family functions for checking labels
#' @export
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth() +
#'   labs(x = "Weight", y = "MPG", color = NULL)
#'
#' uses_labels(p, x = "Weight")
#' uses_labels(p, x = "Weight", y = "MPG", color = NULL)
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' uses_labels(p, color = NULL)
#' uses_labels(p, colour = NULL)
uses_labels <- function(p, ...) {
  args <- list(...)

  if (length(args) == 0) {
    stop(
      "You must pass an argument to `...` in `uses_labels()`.",
      call. = FALSE
    )
  }

  if (!all(is_scalar_string_or_null(args))) {
    stop(
      "All inputs to `...` must be character vectors of length 1 or `NULL`.",
      call. = FALSE
    )
  }

  if (is.null(names(args)) || any(names(args) == "")) {
    stop("All inputs to `...` must be named.", call. = FALSE)
  }

  labels <- get_labels(p, names(args))

  all(mapply(identical, args, labels))

is_scalar_string_or_null <- function(x) {
  vapply(
    x,
    function(x) rlang::is_scalar_character(x) || rlang::is_null(x),
    logical(1)
  )
}
