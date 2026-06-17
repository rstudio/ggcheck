# List the stats used by a plot

`get_stats` returns a vector of stats names, written as character
strings, that describes which stats in which order are used by a plot.

## Usage

``` r
get_stats(p)
```

## Arguments

- p:

  A ggplot object

## Value

A vector of character strings. Each element corresponds to the suffix of
a ggplot2 `stat_` function, e.g. `c("identity", "smooth")`.

## See also

Other functions for checking stats:
[`ith_stat()`](https://rstudio.github.io/ggcheck/reference/ith_stat.md),
[`ith_stat_is()`](https://rstudio.github.io/ggcheck/reference/ith_stat_is.md),
[`uses_stats()`](https://rstudio.github.io/ggcheck/reference/uses_stats.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
get_stats(p)
#> [1] "identity" "smooth"  
```
