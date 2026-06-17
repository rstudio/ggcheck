# Does a plot use one or more aesthetics?

`uses_aesthetics` checks whether the student used one or more
aesthetics.

## Usage

``` r
uses_aesthetics(p, aesthetics, local_only = FALSE, exact = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
  or
  [`get_stat_layer`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md)..

- aesthetics:

  character vector of variables to check for, e.g. "x" or c("x")

- local_only:

  `TRUE` or `FALSE`. Should `uses_aesthetics` only return mappings
  defined locally in the layer?

- exact:

  If `TRUE`, variables need to be mapped exactly

## Value

A logical value.

## Details

By default, `uses_aesthetics` requires that only one of the aesthetics
need to be used. Set `exact` to `TRUE` to check if all of the variables
have to be matched exactly.

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class))
uses_aesthetics(p, "x")
#> [1] TRUE
uses_aesthetics(p, c("x", "y"))
#> [1] TRUE
uses_aesthetics(get_geom_layer(p, "point"), c("x", "y", "color"), local_only = TRUE)
#> [1] TRUE
uses_aesthetics(get_geom_layer(p, "point"), c("x", "y"), local_only = FALSE)
#> [1] TRUE
```
