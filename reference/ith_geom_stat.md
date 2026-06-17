# Which geom/stat combination is used in the ith layer?

`ith_geom_stat` returns the type of geom used by the ith layer according
to a geom/stat combination.

## Usage

``` r
ith_geom_stat(p, i)
```

## Arguments

- p:

  A ggplot object

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on.

## Value

A list of lists with a GEOM and STAT strings, each corresponding to the
suffix of a ggplot2 `geom_` function (e.g. `"point"`), and `stat_`
function (e.g. `"identity"`). e.g. list(list(GEOM = "point", STAT =
"identity"))

## See also

Other functions for checking geoms:
[`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md),
[`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md),
[`ith_geom()`](https://rstudio.github.io/ggcheck/reference/ith_geom.md),
[`ith_geom_is()`](https://rstudio.github.io/ggcheck/reference/ith_geom_is.md),
[`uses_geoms()`](https://rstudio.github.io/ggcheck/reference/uses_geoms.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
ith_geom_stat(p, i = 2)
#> $GEOM
#> [1] "smooth"
#> 
#> $STAT
#> [1] "smooth"
#> 
#> attr(,"class")
#> [1] "GEOM_STAT"
```
