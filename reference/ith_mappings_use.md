# Does the ith layer use one or more aesthetic mappings?

`ith_mappings_use` checks whether the student uses the supplied mappings
in the ith layer of their plot.

## Usage

``` r
ith_mappings_use(p, mappings, i, local_only = FALSE, exact = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
  or
  [`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md).

- mappings:

  One or more aesthetic mappings created with
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html).

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on. `ith_mappings_use` will check the
  aesthetics used by the ith layer.

- local_only:

  If `TRUE`, `ith_mappings_use` will check only the mappings defined
  locally in a layer for the presence of `mappings`. If `FALSE`,
  `ith_mappings_use` will check for `mappings` in the combination of
  global and local methods that will be used to plot a layer.

- exact:

  If `TRUE`, mappings need to be mapped exactly

## Value

A logical value

## Details

`ith_mappings_use` ignores whether or not the student supplied
additional mappings as well. Functions that use the `ith_` prefix are
designed to eliminate the need to call `get_layer` to check a specific
layer in a plot, e.g. `p uses_mappings(aes(color = class))`.

## See also

Other functions for checking mappings:
[`get_mappings()`](https://rstudio.github.io/ggcheck/reference/get_mappings.md),
[`identical_aes()`](https://rstudio.github.io/ggcheck/reference/identical_aes.md),
[`ith_mappings()`](https://rstudio.github.io/ggcheck/reference/ith_mappings.md),
[`uses_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_mappings.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
ith_mappings_use(p, i = 1, aes(x = displ), local_only = FALSE)
#> [1] TRUE
ith_mappings_use(p, i = 1, aes(x = displ), local_only = TRUE)
#> [1] FALSE
ith_mappings_use(p, i = 2, aes(x = displ, y = hwy), local_only = FALSE)
#> [1] TRUE
```
