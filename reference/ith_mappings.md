# Return the aesthetic mappings used by the ith layer

`ith_mappings` returns the mappings used by a ggplot object or a single
layer extracted from the object with
[`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
or
[`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md).

## Usage

``` r
ith_mappings(p, i, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
  or
  [`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md).

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on. `ith_mappings_use` will check the
  aesthetics used by the ith layer.

- local_only:

  If `TRUE`, `ith_mappings_use` will check only the mappings defined
  locally in a layer for the presence of `mappings`. If `FALSE`,
  `ith_mappings_use` will check for `mappings` in the combination of
  global and local methods that will be used to plot a layer.

## Value

A list with class uneval, as returned by
[`aes`](https://ggplot2.tidyverse.org/reference/aes.html) Components of
the list are either quosures or constants.

## Details

Functions that use the `ith_` prefix are designed to eliminate the need
to call `get_layer` to check a specific layer in a plot, e.g.
`p %>% get_geom_layer(geom = "point") %>% get_mappings()`.

## See also

Other functions for checking mappings:
[`get_mappings()`](https://rstudio.github.io/ggcheck/reference/get_mappings.md),
[`identical_aes()`](https://rstudio.github.io/ggcheck/reference/identical_aes.md),
[`ith_mappings_use()`](https://rstudio.github.io/ggcheck/reference/ith_mappings_use.md),
[`uses_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_mappings.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
ith_mappings(p, i = 1, local_only = FALSE)
#> Aesthetic mapping: 
#> * `x`      -> `displ`
#> * `y`      -> `hwy`
#> * `colour` -> `class`
ith_mappings(p, i = 1, local_only = TRUE)
#> Aesthetic mapping: 
#> * `colour` -> `class`
ith_mappings(p, i = 2, local_only = FALSE)
#> Aesthetic mapping: 
#> * `x` -> `displ`
#> * `y` -> `hwy`
```
