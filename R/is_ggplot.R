#' Check if an object is a ggplot
#'
#' @examples
#' require(ggplot2)
#'
#' p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point()
#' is_ggplot(p_valid)
#'
#' p_invalid <- geom_point()
#' is_ggplot(p_invalid)
#' @param p An object
#'
#' @return [`TRUE`] if `p` is a [ggplot][ggplot2::ggplot] object,
#' otherwise [`FALSE`]
#'
#' @export
is_ggplot <- function(p) {
  inherits(p, "ggplot")
}

