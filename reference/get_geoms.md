# List the geoms used by a plot

`get_geoms` returns a vector of geom names, written as character
strings, that describes which geoms in which order are used by a plot.

## Usage

``` r
get_geoms(p)
```

## Arguments

- p:

  A ggplot object

## Value

A vector of character strings. Each element corresponds to the suffix of
a ggplot2 `geom_` function, e.g. `c("point", "line", "smooth")`.

## See also

Other functions for checking geoms:
[`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md),
[`ith_geom()`](https://rstudio.github.io/ggcheck/reference/ith_geom.md),
[`ith_geom_is()`](https://rstudio.github.io/ggcheck/reference/ith_geom_is.md),
[`ith_geom_stat()`](https://rstudio.github.io/ggcheck/reference/ith_geom_stat.md),
[`uses_geoms()`](https://rstudio.github.io/ggcheck/reference/uses_geoms.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
get_geoms(p)
#> [1] "point"  "smooth"
```
