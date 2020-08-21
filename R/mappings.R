aes_c <- function(a1, a2) {
  aesthetics <- names(a2)
  a1[aesthetics] <- a2
  a1
}

get_mappings <- function(p, ...) {
  UseMethod("get_mappings")
}

get_mappings.ggplot <- function(p, ...) {
  p$mapping
}

get_mappings.layer_to_check <- function(p, local_only = TRUE) {
  local_mappings <- p$layer$mapping

  if (local_only) {
    return(local_mappings)
  } else {
    return(aes_c(p$global_mapping, local_mappings))
  }
}

uses_mappings <- function(p, mappings, local_only = FALSE) {
  aes_map <- get_mappings(p, local_only)
  names(mappings) %in% names(aes_map) &&
    identical(mappings, aes_map[names(mappings)])
}

mappings_match <- function(p, mappings, local_only = FALSE) {
  identical(mappings, get_mappings(p, local_only))
}

ith_mappings <- function(p, i, local_only = TRUE) {
  if(!inherits(p, "ggplot")) {
    stop("p should be a ggplot object")
  }

  get_mappings(get_layer(p, i = i), local_only)
}

ith_mappings_use <- function(p, mappings, i, local_only = TRUE) {
  aes_map <- get_mappings(get_layer(p, i = i), local_only)
  names(mappings) %in% names(aes_map) &&
    identical(mappings, aes_map[names(mappings)])
}

ith_mappings_match <- function(p, mappings, i, local_only = TRUE) {
  identical(mappings, get_mappings(get_layer(p, i = i), local_only))
}

