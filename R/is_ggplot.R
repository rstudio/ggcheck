#' Check if an object is a ggplot
#'
#' @description
#' `is_ggplot()` tests if an object is a [ggplot][ggplot2::ggplot].
#'
#' `stop_if_not_ggplot()` signals an error if an object is not
#' a [ggplot][ggplot2::ggplot].
#'
#' @examples
#' require(ggplot2)
#'
#' p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point()
#' is_ggplot(p_valid)
#' stop_if_not_ggplot(p_valid)
#'
#' p_invalid <- geom_point()
#' is_ggplot(p_invalid)
#' \dontrun{
#' stop_if_not_ggplot(p_invalid)}
#' @param p An object
#'
#' @return `is_ggplot()` returns [`TRUE`] if `p` is a [ggplot][ggplot2::ggplot]
#' object; otherwise it returns [`FALSE`].
#'
#' `stop_if_not_ggplot()` returns an error if `p` is not a
#' [ggplot][ggplot2::ggplot] object; other it invisibly returns [`NULL`].
#'
#' @export
is_ggplot <- function(p) {
  inherits(p, "ggplot")
}

#' @rdname is_ggplot
#' @export
stop_if_not_ggplot <- function(p) {
  if (!is_ggplot(p)) {
    class <- class(p)

    stop(
      '`p` must be a "ggplot" object, not an object of class ',
      '"', class[[1]], '"',
      call. = FALSE
    )
  }
}
