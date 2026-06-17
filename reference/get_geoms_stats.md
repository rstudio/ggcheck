# List the geom and stat combination used by all layers of a plot.

List the geom and stat combination used by all layers of a plot.

## Usage

``` r
get_geoms_stats(p)
```

## Arguments

- p:

  A ggplot object

## Value

A list of lists with a GEOM and STAT character. e.g. list(list(GEOM =
"point", STAT = "identity"))

## See also

Other functions for checking geoms:
[`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md),
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
get_geoms_stats(p)
#> [[1]]
#> $GEOM
#> [1] "point"
#> 
#> $STAT
#> [1] "identity"
#> 
#> attr(,"class")
#> [1] "GEOM_STAT"
#> 
#> [[2]]
#> $GEOM
#> [1] "smooth"
#> 
#> $STAT
#> [1] "smooth"
#> 
#> attr(,"class")
#> [1] "GEOM_STAT"
#> 
```
