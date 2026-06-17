# Does a plot use the correct coordinate system?

`uses_coordinate_system` checks whether a plot uses the coordinate
system you describe. To describe a coordinate system, use the character
string that matches the suffix of the ggplot2 `coord_` function that
would make the coordinate system. The default coordinate system for
ggplot2 plots is `"cartesian"`.

## Usage

``` r
uses_coordinate_system(p, coordinates)
```

## Arguments

- p:

  A ggplot2 object

- coordinates:

  A character string that corresponds to the suffix of a ggplot2
  `coord_` function, e.g. `"cartesian"`.

## Value

`TRUE` or `FALSE`

## See also

Other functions for checking coordinate systems:
[`get_coordinate_system()`](https://rstudio.github.io/ggcheck/reference/get_coordinate_system.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth() +
  coord_polar()
uses_coordinate_system(p, coordinates = "polar")
#> [1] TRUE
```
