# Does a plot or layer use one or more mappings?

`uses_mappings` checks whether the student used one or more mappings in
their plot. By default, `uses_mappings` ignores whether or not the
student also supplied additional mappings. Use `uses_extra_mappings` to
check if they did. If `exact` is `TRUE`, then all of the mappings have
to match exactly.

## Usage

``` r
uses_mappings(p, mappings, local_only = FALSE, exact = FALSE)
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

- local_only:

  If `TRUE`, `uses_mappings` will check only the mappings defined
  locally in a layer for the presence of `mappings`. If `FALSE`,
  `uses_mappings` will check for `mappings` in the combination of global
  and local methods that will be used to plot a layer.

- exact:

  If `TRUE`, mappings need to be mapped exactly

## Value

A logical value.

## See also

Other functions for checking mappings:
[`get_mappings()`](https://rstudio.github.io/ggcheck/reference/get_mappings.md),
[`identical_aes()`](https://rstudio.github.io/ggcheck/reference/identical_aes.md),
[`ith_mappings()`](https://rstudio.github.io/ggcheck/reference/ith_mappings.md),
[`ith_mappings_use()`](https://rstudio.github.io/ggcheck/reference/ith_mappings_use.md)

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class))
uses_mappings(p, aes(x = displ))
#> [1] TRUE
uses_mappings(get_geom_layer(p, i = 1), aes(x = displ, color = class), local_only = FALSE)
#> [1] TRUE
uses_mappings(get_geom_layer(p, i = 1), aes(x = displ, color = class), local_only = TRUE)
#> [1] FALSE
uses_mappings(p, aes(x = displ, y = hwy), exact = TRUE)
#> [1] FALSE
```
