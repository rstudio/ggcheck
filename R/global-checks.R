
global_data <- function(p) {
  p$data
}

uses_global_data <- function(p, data) {
  identical(global_data(p), data)
}

# Returns NULL if layer does not exist
# if i not specified, returns first instance of described geom
determine_layer <- function(p, geom = NULL, i = NULL) {
  if (is.null(geom) && is.null(i)) {
    stop("Grading error: cannot identify which layer to grade. (For the grader) please specify at least one of geom or i.")
  } else if (is.null(geom)) {
    index <- i
  } else if (is.null(i)) {
    index <- which(geoms(p) == geom)[1]
  } else {
    index <- which(geoms(p) == geom)[i]
  }

  if (index > length(p$layers)) {
    stop("Grading error: cannot find specified layer. (For the grader) use checks to check that desired layer exists before inspecting the layer.")
  }

  p$layers[[index]]
}

get_data_from_layer <- function(l) {
  l$data
}

is_empty_data <- function(d) {
  identical(d, structure(list(), class = "waiver"))
}

data_for_layer <- function(p, geom = NULL, i = NULL, local_only = FALSE) {
  l <- determine_layer(p, geom, i)
  local_data <- get_data_from_layer(l)

  if (local_only || length(local_data)) {
    if (is_empty_data(local_data)) {
      local_data <- NULL
    }
    return(local_data)
  } else {
    global <- global_data(p)
    if (is_empty_data(global)) {
      global <- NULL
    }
    return(global)
  }
}

uses_data_in_layer <- function(p, data, geom = NULL, i = NULL, local_only = FALSE){
  d <- data_for_layer(p, geom, i, local_only)
  identical(data, d)
}

global_mappings <- function(p) {
  p$mapping
}

# will environments matter here?
uses_global_mappings <- function(p, mappings) {
  identical(global_mappings(p), mappings)
}

# will environments matter here?
uses_global_mapping <- function(p, mapping) {
  mappings <- global_mappings(p)
  names(mapping) %in% names(mappings) && identical(mapping, mappings[names(mapping)])
}

get_mappings_from_layer <- function (l) {
  l$mapping
}

aes_c <- function(a1, a2) {
  aesthetics <- names(a2)
  a1[aesthetics] <- a2
  a1
}

mappings_for_layer <- function(p, geom = NULL, i = NULL, local_only = FALSE) {
  l <- determine_layer(p, geom, i)
  local_mappings <- get_mappings_from_layer(l)

  if (local_only) {
    return(local_mappings)
  } else {
    return(aes_c(global_mappings(p), local_mappings))
  }
}

layer_mappings_match <- function(p, mappings, geom = NULL, i = NULL, local_only = FALSE) {
  identical(mappings, mappings_for_layer(p, geom, i, local_only))
}

uses_mappings_in_layer <- function(p, mapping, geom = NULL, i = NULL, local_only = FALSE) {
  mappings <- mappings_for_layer(p, geom, i, local_only)
  names(mapping) %in% names(mappings) && identical(mapping, mappings[names(mapping)])
}

n_layers <- function(p) {
  length(p$layers)
}

ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

ith_geom_is <- function(p, geom, i = 1) {
  geom_i <- ith_geom(p, i)
  geom_i == geom
}

geoms <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_geom, "a", p = p)
}

uses_geom <- function(p, geom) {
  geom %in% geoms(p)
}

uses_geoms <- function(p, geoms) {
  identical(geoms, geoms(p))
}

coordinate_system <- function(p) {
  coords <- class(p$coordinates)[1]
  gsub("coord", "", tolower(coords))
}

uses_coordinate_system <- function(p, coordinates) {
  coordinates == coordinate_system(p)
}


