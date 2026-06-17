# Does a plot use one or more stats?

`uses_stats` tests whether a plot uses one or more stats in its layers.

## Usage

``` r
uses_stats(p, stats, geoms = NULL, exact = TRUE)
```

## Arguments

- p:

  A ggplot object

- stats:

  A vector of character strings. Each element should correspond to the
  suffix of a ggplot2 `stat_` function, e.g. `c("identity", "smooth")`.

- geoms:

  A character vector to optionally check for the geoms corresponding to
  stats e.g. c("point", "smooth") if checking c("identity", "smooth")

- exact:

  if `TRUE`, use exact matching

## Value

`TRUE` or `FALSE`

## Details

By default, the plot must have the exact stats or geom/stat combinations
and in the same order. However, if `exact` is set to `FALSE`, the plot
stats or geom/stat combinations do not have to be exact.

## See also

Other functions for checking stats:
[`get_stats()`](https://rstudio.github.io/ggcheck/reference/get_stats.md),
[`ith_stat()`](https://rstudio.github.io/ggcheck/reference/ith_stat.md),
[`ith_stat_is()`](https://rstudio.github.io/ggcheck/reference/ith_stat_is.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
uses_stats(p, stats = "smooth")
#> [1] FALSE
uses_stats(p, stats = c("identity", "smooth"), exact = TRUE)
#> [1] TRUE
uses_stats(p, c("smooth", "identity"), geoms = c("smooth", "point"))
#> [1] FALSE
```
