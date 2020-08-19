
global_data <- function(p) {
  p$data
}

uses_global_data <- function(p, data) {
  identical(global_data(p), data)
}

global_mappings <- function(p) {
  p$mapping
}

uses_global_mappings <- function(p, mappings) {
  identical(global_mappings(p), mappings)
}

uses_global_mapping <- function(p, mapping) {
  mappings <- global_mappings(p)
  names(mapping) %in% names(mappings) && identical(mapping, mappings[names(mapping)])
}

n_layers <- function(p) {
  length(p$layers)
}

ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
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
