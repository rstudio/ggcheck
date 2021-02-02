
# ggplot2 default mappings between geom and stat
# NOTE: this mapping is geom-centric, meaning it accurately represents the mapping
# when creating a layer using a `geom_` function. This means that when using some stat
# functions to create a layer, the Geom_ class is not the same as when calling the respective
# geom_ function. This is the case for the stat_sf* functions, for example.
# A potential workaround is to create another stat_lookup table that maintains stat => geom mapping.
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

# maps user supplied geom suffixes to ggplot2 geom/stat suffixes using the `geom_lookup`
# lookup table that maps the default geom/stat combinations.
map_geoms <- function(geoms, stat = FALSE) {
  if (is.null(geoms)) {
    stop("A geom must be specified to map to ggplot2 geom(s).")
  }

  # check if any of the geoms suffixes do not exist
  invalid_geoms <- Filter(function(x) !(x %in% geom_lookup$geom), geoms)
  if (length(invalid_geoms) > 0) {
    stop(
      "The following suffixes do not map to any ggplot2 geom: ",
        paste0(invalid_geoms, collapse = ", ")
    )
  }
  combo <- list(
    GEOM = geom_lookup$GEOM[which(geom_lookup$geom %in% geoms)],
    STAT = geom_lookup$STAT[which(geom_lookup$geom %in% geoms)]
  )
  # if stat is TRUE, return the GEOM/STAT combo
  if (stat) {
    return(combo)
  } else {
    # else, return the GEOM only
    return(combo$GEOM)
  }
}

# helper function to return all geom_, stat_ function names from the ggplot2 package
available_layers <- function() {
  require(ggplot2)
  ggplot_names <- lsf.str("package:ggplot2")
  # collection of ggplot geom/stat functions
  list(
    geom_functions = ggplot_names[grepl(pattern = "^geom_", x =  ggplot_names)],
    stat_functions = ggplot_names[grepl(pattern = "^stat_", x =  ggplot_names)]
  )
}

# helper function that returns all Geom_ objects by executing names returned from `available_layers`
get_all_geom_objects <- function() {
  require(sf)
  all_layers <- available_layers()
  stripped_geom_names <- lapply(all_layers$geom_functions, function(x) gsub("geom_", "", x))
  # call all geom_ functions for further inspection of objects
  geom_stats <- lapply(
    all_layers$geom_functions,
    function(l) {
      tryCatch({
        if (l == "geom_map") {
          do.call(l, list(map = data.frame(x = 1, y = 1, id = 1)))
        } else if (l == "geom_sf") {
          do.call(l, list())[[1]]
        } else {
          do.call(l, list())
        }
      },
      error = function(e) {
        paste0(l)
      }
      )
    }
  )
  names(geom_stats) <- stripped_geom_names
  geom_stats
}
all_geoms <- NULL

# helper function to return geom and stat class given a geom suffix name.
# TODO: we could switch to only using this instead of a lookup table, but
# it would require an additional {sf} package dependency for `geom_sf_` functions
geom_class <- function(suffix) {
  if (is.null(all_geoms)) {
    all_geoms <- get_all_geom_objects()
  }
  list(
    GEOM = class(all_geoms[[suffix]]$geom)[1],
    STAT = class(all_geoms[[suffix]]$stat)[1]
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
