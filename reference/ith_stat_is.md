# Is the ith stat what it should be?

`ith_stat_is` checks whether the ith layer uses the prescribed type of
stat

## Usage

``` r
ith_stat_is(p, stat, i = 1)
```

## Arguments

- p:

  A ggplot object

- stat:

  A character string that corresponds to the suffix of a ggplot2 `stat_`
  function, e.g. `"identity"`.

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on. `ith_stat_is` will check the stat
  used by the ith layer.

## Value

`TRUE` or `FALSE`

## See also

Other functions for checking stats:
[`get_stats()`](https://rstudio.github.io/ggcheck/reference/get_stats.md),
[`ith_stat()`](https://rstudio.github.io/ggcheck/reference/ith_stat.md),
[`uses_stats()`](https://rstudio.github.io/ggcheck/reference/uses_stats.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = diamonds, aes(sample = price)) +
  geom_qq()
ith_stat_is(p, i = 1, "qq")
#> [1] TRUE
```
