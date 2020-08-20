
n_layers <- function(p) {
  length(p$layers)
}

get_layer <- function(p, geom = NULL, i = NULL) {

  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  if (is.null(geom) && is.null(i)) {
    stop("Grading error: cannot identify which layer to grade. (For the grader) please specify at least one of geom or i.")
  } else if (is.null(geom)) {
    index <- i
  } else if (is.null(i)) {
    index <- which(get_geoms(p) == geom)[1]
  } else {
    index <- which(get_geoms(p) == geom)[i]
  }

  if (index > length(p$layers)) {
    stop("Grading error: cannot find specified layer. (For the grader) use checks to check that desired layer exists before inspecting the layer.")
  }

  l <- list(
    layer = p$layers[[index]],
    global_data = get_data(p),
    global_mapping = global_mappings(p)
  )
  structure(l, class = "layer_to_check")
}

is_layer_to_check <- function(x) inherits(x, "layer_to_check")
