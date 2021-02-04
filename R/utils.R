
# ggplot2 default mappings from a `geom_` suffix to geom and stat class names when
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
    "contour_filled",
    "density",
    "point",
    "density2d",
    "density2d_filled",
    "dotplot",
    "errorbarh",
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
    "bin_2d",
    "identity",
    "boxplot",
    "contour",
    "contour_filled",
    "sum",
    "density",
    "identity",
    "density2d",
    "density2d_filled",
    "binplot",
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

# ggplot2 default mappings from a `stat_` suffix to geom and stat class names when
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

# Given a geom_ function suffix (e.g. "point"), `map_stat` returns the ggplot2 geom/stat class names
# using the `geom_lookup` table (e.g. list(GEOM = "point", STAT = "identity"))
map_geom <- function(geom) {
  # check if the geom suffix does not exist
  if (!(geom %in% geom_lookup$geom)) {
    stop("Grading error: the supplied geom '", geom, "' does not exist.")
  }
  # return GEOM + STAT combination
  return(
    list(
      GEOM = geom_lookup$GEOM[which(geom_lookup$geom == geom)],
      STAT = geom_lookup$STAT[which(geom_lookup$geom == geom)]
    )
  )
}

# Given a stat_ function suffix (e.g. "qq"), `map_stat` returns the ggplot2 geom/stat class names
# using the `stat_lookup` table (e.g. list(GEOM = "point", STAT = "qq"))
map_stat <- function(stat) {
  # check if the stat suffix does not exist
  if (!(stat %in% stat_lookup$stat)) {
    stop("Grading error: the supplied stat '", stat, "' does not exist.")
  }
  # return GEOM + STAT combination
  return(
    list(
      GEOM = stat_lookup$GEOM[which(stat_lookup$stat == stat)],
      STAT = stat_lookup$STAT[which(stat_lookup$stat == stat)]
    )
  )
}

#' Grade a ggplot object with \code{gradethis::grade_result} conditions.
#'
#' @param p_chunk Source text for the ggplot code
#' @param p A ggplot object
#' @param ... 1 or more \code{gradethis_condition}
#' @export
grade_plot <- function(p_chunk, p, ...) {
  # env args
  run_result <- list(
    chunk = p_chunk,
    returned = p,
    env_pre = new.env(parent = globalenv()),
    env_post = new.env(parent = globalenv())
  )

  # prep the grading env
  grade_env <- list2env(
    list(
      .envir_prep = new.env(),
      .last_value = run_result$returned,
      .envir_result = run_result$env_post
    )
  )

  # grade
  gradethis::grade_result(
    default_correct = TRUE,
    glue_correct = "{ .message }",
    glue_incorrect = "{ .message }",
    ...
  )(grade_env)
}
