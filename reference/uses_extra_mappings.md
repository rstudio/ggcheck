# Does the plot uses extra aesthetic mappings?

`uses_extra_mappings` checks if a student's plot contains more than the
required aesthetic mappings. Note that we still return `TRUE` if the
student's plot differs from the required aesthetic mappings because they
are technically extra mappings from required set. We recommend you use
`uses_mapping` checks for checking required mappings before
`uses_extra_mappings`.

## Usage

``` r
uses_extra_mappings(p, mappings, local_only = FALSE)
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

  If `TRUE`, `uses_extra_mappings` will check only the mappings defined
  locally in a layer for the presence of `mappings`. If `FALSE`,
  `uses_extra_mappings` will check for `mappings` in the combination of
  global and local methods that will be used to plot a layer.

## Value

A logical value.

## Examples

``` r
require(ggplot2)
p <- ggplot(data = diamonds, aes(x = cut, sample = price)) +
  geom_qq()
uses_extra_mappings(p, aes(sample = price))
#> [1] TRUE
```
