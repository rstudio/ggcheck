# Which geom is used in the ith layer?

`ith_geom` returns the type of geom used by the ith layer.

## Usage

``` r
ith_geom(p, i)
```

## Arguments

- p:

  A ggplot object

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on.

## Value

A character string that corresponds to the suffix of a ggplot2 `geom_`
function, e.g. `"point"`.

## See also

Other functions for checking geoms:
[`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md),
[`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md),
[`ith_geom_is()`](https://rstudio.github.io/ggcheck/reference/ith_geom_is.md),
[`ith_geom_stat()`](https://rstudio.github.io/ggcheck/reference/ith_geom_stat.md),
[`uses_geoms()`](https://rstudio.github.io/ggcheck/reference/uses_geoms.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
ith_geom(p, i = 2)
#> [1] "smooth"
```
