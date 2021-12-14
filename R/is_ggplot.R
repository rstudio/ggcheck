#' Check if an object is a ggplot
#'
#' @description
#' `is_ggplot()` tests if an object is a [ggplot][ggplot2::ggplot].
#'
#' `stop_if_not_ggplot()` signals an error if an object is not
#' a [ggplot][ggplot2::ggplot].
#'
#' `fail_if_not_ggplot()` returns a [failing grade][gradethis::fail] if an
#' object is not a [ggplot][ggplot2::ggplot].
#'
#' @examples
#' require(ggplot2)
#'
#' p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point()
#' is_ggplot(p_valid)
#' stop_if_not_ggplot(p_valid)
#' fail_if_not_ggplot(p_valid)
#'
#' p_invalid <- geom_point()
#' is_ggplot(p_invalid)
#' \dontrun{
#' stop_if_not_ggplot(p_invalid)
#' }
#' fail_if_not_ggplot(p_valid)
#' @param p An object
#'
#' @param message A message to be displayed if `p` is not a
#'   [ggplot][ggplot2::ggplot] object.
#'
#' @param env Environment in which to find `.result`.
#'   Most users of `ggcheck` will not need to use this argument.
#'
#' @return `is_ggplot()` returns [`TRUE`] if `p` is a [ggplot][ggplot2::ggplot]
#' object; otherwise it returns [`FALSE`].
#'
#' `stop_if_not_ggplot()` returns an error if `p` is not a
#' [ggplot][ggplot2::ggplot] object; other it invisibly returns [`NULL`].
#'
#' `fail_if_not_ggplot()` returns a [failing grade][gradethis::fail] if `p` is
#' not a [ggplot][ggplot2::ggplot] object; other it invisibly returns [`NULL`].
#'
#' @export
is_ggplot <- function(p) {
  inherits(p, "ggplot")
}

#' @rdname is_ggplot
#' @export
stop_if_not_ggplot <- function(p, message = getOption("ggcheck.error")) {
  if (is_ggplot(p)) {
    return(invisible(NULL))
  }

  if (is.null(message)) {
    message <- paste0(
      '`p` must be a "ggplot" object, not an object of class ',
      '"', class(p)[[1]], '"'
    )
  }

  stop(message, call. = FALSE)
}

#' @rdname is_ggplot
#' @export
fail_if_not_ggplot <- function(
  p = .result,
  message = getOption("ggcheck.fail"),
  env = parent.frame()
) {
  if (is_ggplot(p)) {
    return(invisible(NULL))
  }

  if (inherits(p, ".result")) {
    p <- get(".result", env)
  }

  if (is.null(message)) {
    message <- paste0(
      'I expected your code to create a ggplot, ',
      'but it created an object of class "', class(p)[[1]], '"'
    )
  }

  gradethis::fail(message)
}
