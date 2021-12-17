
# ggplot2 default mappings from a `geom_` function suffix to geom and stat class names when
# creating a layer using a `geom_` function.
# NOTE: this could be dynamically generated as well but would require extra dependency of {sf} package
geom_lookup <- data.frame(
  geom = c(
    "abline",
    "hline",
    "vline",
    "bar",
    "col",
    "bin2d",
    "blank",
    "boxplot",
    "contour",
    "contour_filled",
    "count",
    "density",
    "density2d",
    "density2d_filled",
    "dotplot",
    "errorbarh",
    "function",
    "hex",
    "freqpoly",
    "histogram",
    "jitter",
    "crossbar",
    "errorbar",
    "linerange",
    "pointrange",
    "map",
    "path",
    "line",
    "step",
    "point",
    "polygon",
    "qq_line",
    "qq",
    "quantile",
    "ribbon",
    "area",
    "rug",
    "segment",
    "curve",
    "smooth",
    "spoke",
    "label",
    "text",
    "raster",
    "rect",
    "tile",
    "violin",
    "sf",
    "sf_label",
    "sf_text"
  ),
  GEOM = c(
    "abline",
    "hline",
    "vline",
    "bar",
    "col",
    "bin2d",
    "blank",
    "boxplot",
    "contour",
    "contourfilled",
    "point",
    "density",
    "density2d",
    "density2dfilled",
    "dotplot",
    "errorbarh",
    "function",
    "hex",
    "path",
    "bar",
    "point",
    "crossbar",
    "errorbar",
    "linerange",
    "pointrange",
    "map",
    "path",
    "line",
    "step",
    "point",
    "polygon",
    "path",
    "point",
    "quantile",
    "ribbon",
    "area",
    "rug",
    "segment",
    "curve",
    "smooth",
    "spoke",
    "label",
    "text",
    "raster",
    "rect",
    "tile",
    "violin",
    "sf",
    "label",
    "text"
  ),
  STAT = c(
    "identity",
    "identity",
    "identity",
    "count",
    "identity",
    "bin2d",
    "identity",
    "boxplot",
    "contour",
    "contourfilled",
    "sum",
    "density",
    "density2d",
    "density2dfilled",
    "bindot",
    "function",
    "identity",
    "binhex",
    "bin",
    "bin",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "qqline",
    "qq",
    "quantile",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "smooth",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "identity",
    "ydensity",
    "sf",
    "sfcoordinates",
    "sfcoordinates"
  ),
  stringsAsFactors = FALSE
)

# ggplot2 default mappings from a `stat_` function suffix to geom and stat class names when
# creating a layer using a `stat_` function.
# NOTE: this could be dynamically generated as well and would not require any extra dependencies
stat_lookup <- data.frame(
  stat = c(
    "bin",
    "bin_2d",
    "bin_hex",
    "bin2d",
    "binhex",
    "boxplot",
    "contour",
    "contour_filled",
    "count",
    "density",
    "density_2d",
    "density_2d_filled",
    "density2d",
    "density2d_filled",
    "ecdf",
    "ellipse",
    "function",
    "identity",
    "qq",
    "qq_line",
    "quantile",
    "sf",
    "sf_coordinates",
    "smooth",
    "spoke",
    "sum",
    "summary",
    "summary_2d",
    "summary_bin",
    "summary_hex",
    "summary2d",
    "unique",
    "ydensity"
  ),
  GEOM = c(
    "bar",
    "tile",
    "hex",
    "tile",
    "hex",
    "boxplot",
    "contour",
    "contourfilled",
    "bar",
    "area",
    "density2d",
    "density2dfilled",
    "density2d",
    "density2dfilled",
    "step",
    "path",
    "function",
    "point",
    "point",
    "path",
    "quantile",
    "rect",
    "point",
    "smooth",
    "spoke",
    "point",
    "pointrange",
    "tile",
    "pointrange",
    "hex",
    "tile",
    "point",
    "violin"
  ),
  STAT = c(
    "bin",
    "bin2d",
    "binhex",
    "bin2d",
    "binhex",
    "boxplot",
    "contour",
    "contourfilled",
    "count",
    "density",
    "density2d",
    "density2dfilled",
    "density2d",
    "density2dfilled",
    "ecdf",
    "ellipse",
    "function",
    "identity",
    "qq",
    "qqline",
    "quantile",
    "sf",
    "sfcoordinates",
    "smooth",
    "identity",
    "sum",
    "summary",
    "summary2d",
    "summarybin",
    "summaryhex",
    "summary2d",
    "unique",
    "ydensity"
  ),
  stringsAsFactors = FALSE
)

#' Helper function to create the GEOM_STAT list structure
#'
#' @param geom A character (e.g. "point")
#' @param stat A character (e.g. "qq")
#'
#' @return list structure with "GEOM_STAT" class
#'
#' @examples
#' geom_stat(geom = "point", stat = "qq")
#' @noRd
geom_stat <- function(geom, stat) {
  structure(
    list(GEOM = geom, STAT = stat),
    class = "GEOM_STAT"
  )
}

#' Given a geom_ function suffix (e.g. "point"), \code{map_geom} returns the ggplot2
#' geom/stat class names. using the \code{geom_lookup} table.
#'
#' @param geom A character (e.g. "point")
#'
#' @return a \code{GEOM_STAT} list structure
#'
#' @examples
#' map_geom("qq")
#' @noRd
map_geom <- function(geom) {
  # check if the geom suffix does not exist
  if (!(geom %in% geom_lookup$geom)) {
    stop("Grading error: the supplied geom '", geom, "' does not exist.")
  }
  # GEOM + STAT combination
  geom_stat(
    geom = geom_lookup$GEOM[which(geom_lookup$geom == geom)],
    stat = geom_lookup$STAT[which(geom_lookup$geom == geom)]
  )
}

#' Given a stat_ function suffix (e.g. "qq"), \code{map_stat} returns the ggplot2
#' geom/stat class names using the \code{stat_lookup} table.
#'
#' @param stat A character (e.g. "qq")
#'
#' @return a \code{GEOM_STAT} list structure
#'
#' @examples
#' map_stat("qq")
#' @noRd
map_stat <- function(stat) {
  # check if the stat suffix does not exist
  if (!(stat %in% stat_lookup$stat)) {
    stop("Grading error: the supplied stat '", stat, "' does not exist.")
  }
  # GEOM + STAT combination
  geom_stat(
    geom = stat_lookup$GEOM[which(stat_lookup$stat == stat)],
    stat = stat_lookup$STAT[which(stat_lookup$stat == stat)]
  )
}

flatten_dots <- function(...) {
  args <- rlang::flatten(rlang::dots_list(...))
  args <- rlang::dots_list(!!!args, .homonyms = "error")
  args
}
