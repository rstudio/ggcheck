# Does a layer use one of more specific parameters?

`uses_geom_params` checks that a plot's geom layer uses a specific
parameter.

## Usage

``` r
uses_geom_params(p, geom, ..., params = NULL, i = NULL)

uses_geom_param(p, geom, ..., params = NULL, i = NULL)
```

## Arguments

- p:

  A ggplot object

- geom:

  A character string found in the suffix of a ggplot2 geom function,
  e.g. `"point"`.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Named values or [character](https://rdrr.io/r/base/character.html)
  strings. Unnamed arguments will check whether any value was set for
  that parameter. Named arguments will check whether the parameter with
  the same name has a matching value. Each argument should have a name
  matching a
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) layer
  parameter. Values may be passed as arguments or as list elements.

- params:

  A named list of geom or stat parameter values, e.g.
  `list(outlier.alpha = 0.01)`. This list is combined with any inputs to
  `...`

- i:

  A numerical index, e.g. `1`.

## Value

A named logical vector of the same length as the number of inputs to
`...`.

## Details

To specify a specific geom layer, either specify using position using
the `i` index or by using a combination of `geom` function suffix name
and `i` to check the ith layer that uses the geom.

The `params` argument accepts a list that contains geom, stat, or aes
parameters. This offers flexibility in certain situations where setting
a parameter on a `geom_` function is actually setting a stat parameter
or aes parameter. For example, in `geom_histogram(binwidth = 500)`, the
`binwidth` is a stat parameter, while in
`geom_histogram(fill = "blue")`, the `fill` is an aes parameter.
`uses_geom_params` will take this into account and check geom, stat, and
aes parameters.

Note that `uses_geom_params()` can detect aes *parameters*, but not aes
*mappings*. Parameters are set to static values directly within a layer
(e.g. `geom_point(color = "blue")`), while mappings associate variables
in the data with plot aesthetics using
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) (e.g.
`geom_point(aes(color = class))`).

## See also

Other functions for checking geom parameters:
[`get_default_params()`](https://rstudio.github.io/ggcheck/reference/get_default_params.md)

## Examples

``` r
require(ggplot2)

p <- ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot(varwidth = TRUE, outlier.alpha = 0.01, fill = "blue")

uses_geom_params(
  p, "boxplot", list(varwidth = TRUE, outlier.alpha = 0.01, fill = "blue")
)
#>      varwidth outlier.alpha          fill 
#>          TRUE         FALSE          TRUE 

uses_geom_params(
  p, "boxplot", varwidth = TRUE, outlier.alpha = 0.01, fill = "blue"
)
#>      varwidth outlier.alpha          fill 
#>          TRUE         FALSE          TRUE 

# Unnamed arguments check that a parameter is set to any value
uses_geom_params(p, "boxplot", "fill")
#> fill 
#> TRUE 
```
