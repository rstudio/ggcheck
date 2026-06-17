# Does a plot use one or more geoms?

`use_geoms` tests whether a plot uses one or more geoms created using a
`geom`. If checking for a layer that is created using a `stat` function,
please use `uses_stats` instead.

## Usage

``` r
uses_geoms(p, geoms, stats = NULL, exact = TRUE)
```

## Arguments

- p:

  A ggplot object

- geoms:

  A vector of character strings. Each element should correspond to the
  suffix of a ggplot2 `geom_` function, e.g.
  `c("point", "line", "smooth")`.

- stats:

  A character vector to optionally check for the stats corresponding to
  geoms e.g. c("identity", "smooth") if checking c("point", "smooth")

- exact:

  A boolean to indicate whether to use exact matching

## Value

`TRUE` or `FALSE`

## Details

By default, the plot must have the exact geoms or geom/stat combinations
and in the same order. However, if `exact` is set to `FALSE`, the plot
geoms or geom/stat combinations do not have to be exact.

## See also

Other functions for checking geoms:
[`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md),
[`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md),
[`ith_geom()`](https://rstudio.github.io/ggcheck/reference/ith_geom.md),
[`ith_geom_is()`](https://rstudio.github.io/ggcheck/reference/ith_geom_is.md),
[`ith_geom_stat()`](https://rstudio.github.io/ggcheck/reference/ith_geom_stat.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
uses_geoms(p, geoms = "point")
#> [1] FALSE
uses_geoms(p, geoms = c("point", "smooth"), exact = TRUE)
#> [1] TRUE
uses_geoms(p, geoms = c("point", "smooth"), stats = c("identity", "smooth"))
#> [1] TRUE
```
