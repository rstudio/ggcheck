# Which coordinate system does a plot use?

Which coordinate system does a plot use?

## Usage

``` r
get_coordinate_system(p)
```

## Arguments

- p:

  A ggplot2 object

## Value

A character string that corresponds to the suffix of a ggplot2 `coord_`
function, e.g. `"cartesian"`.

## See also

Other functions for checking coordinate systems:
[`uses_coordinate_system()`](https://rstudio.github.io/ggcheck/reference/uses_coordinate_system.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth() +
  coord_polar()
get_coordinate_system(p)
#> [1] "polar"
```
