#' Compare two `ggplot`s to check whether they are equal
#'
#' @param x,y Two `ggplot` objects to compare
#' @param ... Unused
#'
#' @seealso [gradethis::gradethis_equal()] for the generic function.
#' @inherit gradethis::gradethis_equal return
#' @importFrom gradethis gradethis_equal
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggcheck)
#' library(gradethis)
#'
#' cty_plot <- ggplot(mpg, aes(x = displ, y = cty)) + geom_point()
#' hwy_plot <- ggplot(mpg, aes(x = displ, y = cty)) + geom_point()
#'
#' gradethis_equal(cty_plot, hwy_plot)
#' gradethis_equal(cty_plot, cty_plot)
gradethis_equal.ggplot <- function(x, y, ...) {
  try(ggplot2::ggplot_build(x), silent = TRUE)
  try(ggplot2::ggplot_build(y), silent = TRUE)
  NextMethod()
}
