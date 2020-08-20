# geoms are character strings, i.e. "point", "smooth"

ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

ith_geom_is <- function(p, geom, i = 1) {
  geom_i <- ith_geom(p, i)
  geom_i == geom
}

get_geoms <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_geom, "a", p = p)
}

geoms_match <- function(p, geoms) {
  identical(geoms, get_geoms(p))
}

uses_geom <- function(p, geom) {
  all(geom %in% get_geoms(p))
}





