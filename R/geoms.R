# geoms are character strings, i.e. "point", "smooth"
ith_geom <- function(p, i) {
  geom <- class(p$layers[[i]]$geom)[1]
  gsub("geom", "", tolower(geom))
}

get_geoms <- function(p) {
  n <- n_layers(p)
  vapply(seq_len(n), ith_geom, "a", p = p)
}

