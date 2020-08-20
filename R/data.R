is_empty_data <- function(d) {
  identical(d, structure(list(), class = "waiver"))
}

get_global_data_from_layer <- function(l) {
  stopifnot(is_layer_to_check(l))

  data <- l$global_data

  if (is_empty_data(data)) {
    return(NULL)
  }

  data
}

get_data <- function(p, ...) {
  UseMethod("get_data")
}

get_data.ggplot <- function(p, ...) {
  p$data
}

get_data.layer_to_check <- function(p, local_only = TRUE) {

  data <- p$layer$data

  # if no local data
  if (is_empty_data(data)) {
    if (local_only) {
      return(NULL)
    } else {
      return(get_global_data_from_layer(p))
    }
  }

  data
}

uses_data <- function(p, data, local_only = FALSE) {
  identical(data, get_data(p, local_only))
}
