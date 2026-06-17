# Which stat is used in the ith layer?

`ith_stat` returns the type of stat used by the ith layer.

## Usage

``` r
ith_stat(p, i)
```

## Arguments

- p:

  A ggplot object

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on.

## Value

A character string that corresponds to the suffix of a ggplot2 `stat_`
function, e.g. `"qq"`.

## See also

Other functions for checking stats:
[`get_stats()`](https://rstudio.github.io/ggcheck/reference/get_stats.md),
[`ith_stat_is()`](https://rstudio.github.io/ggcheck/reference/ith_stat_is.md),
[`uses_stats()`](https://rstudio.github.io/ggcheck/reference/uses_stats.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = diamonds, aes(sample = price)) +
  geom_qq()
ith_stat(p, i = 1)
#> [1] "qq"
```
