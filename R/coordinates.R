get_coordinate_system <- function(p) {
  coords <- class(p$coordinates)[1]
  gsub("coord", "", tolower(coords))
}

uses_coordinate_system <- function(p, coordinates) {
  coordinates == get_coordinate_system(p)
}
