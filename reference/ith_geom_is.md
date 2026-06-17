# Is the ith geom what it should be?

`ith_geom_is` checks whether the ith layer uses the prescribed type of
geom.

## Usage

``` r
ith_geom_is(p, geom, i = 1)
```

## Arguments

- p:

  A ggplot object

- geom:

  A character string that corresponds to the suffix of a ggplot2 `geom_`
  function, e.g. `"point"`.

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on. `ith_geom_is` will check the geom
  used by the ith layer.

## Value

`TRUE` or `FALSE`

## See also

Other functions for checking geoms:
[`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md),
[`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md),
[`ith_geom()`](https://rstudio.github.io/ggcheck/reference/ith_geom.md),
[`ith_geom_stat()`](https://rstudio.github.io/ggcheck/reference/ith_geom_stat.md),
[`uses_geoms()`](https://rstudio.github.io/ggcheck/reference/uses_geoms.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
ith_geom_is(p, geom = "smooth", i = 2)
#> [1] TRUE
```
