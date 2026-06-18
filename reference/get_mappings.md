# Get aesthetic mappings from a layer or plot

`get_mappings` returns the mappings used by a ggplot object or a single
layer extracted from the object with
[`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
or
[`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md).

## Usage

``` r
get_mappings(p, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
  or
  [`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md).

- local_only:

  `TRUE` or `FALSE`. Should `get_mappings` return only the mappings
  defined locally in a layer. This has no effect when `p` is a ggplot
  object.

## Value

A list with class uneval, as returned by
[`aes`](https://ggplot2.tidyverse.org/reference/aes.html) Components of
the list are either quosures or constants.

## Details

When passed a ggplot object (i.e. a plot), `get_mappings` will return
only the mappings that have been set globally with
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html). When
passed a single layer from a plot, the behavior of `get_mappings` will
depend on the value of `local_only`. If `local_only = TRUE`,
`get_mappings` will return only the mappings defined locally in a layer.
When `local_only = FALSE`, `get_mappings` will return the combination of
global and local methods that will be used to plot a layer.

## See also

Other functions for checking mappings:
[`identical_aes()`](https://rstudio.github.io/ggcheck/reference/identical_aes.md),
[`ith_mappings()`](https://rstudio.github.io/ggcheck/reference/ith_mappings.md),
[`ith_mappings_use()`](https://rstudio.github.io/ggcheck/reference/ith_mappings_use.md),
[`uses_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_mappings.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class))
get_mappings(p)
#> $x
#> <quosure>
#> expr: ^displ
#> env:  0x55e890f56e00
#> 
#> $y
#> <quosure>
#> expr: ^hwy
#> env:  0x55e890f56e00
#> 
#> $colour
#> <quosure>
#> expr: ^class
#> env:  0x55e890f56e00
#> 
#> attr(,"class")
#> [1] "uneval"
get_mappings(get_geom_layer(p, i = 1), local_only = FALSE)
#> $x
#> <quosure>
#> expr: ^displ
#> env:  0x55e890f56e00
#> 
#> $y
#> <quosure>
#> expr: ^hwy
#> env:  0x55e890f56e00
#> 
#> $colour
#> <quosure>
#> expr: ^class
#> env:  0x55e890f56e00
#> 
#> attr(,"class")
#> [1] "uneval"
```
