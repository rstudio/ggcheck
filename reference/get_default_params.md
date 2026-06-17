# What are the default parameters for a plot layer?

What are the default parameters for a plot layer?

## Usage

``` r
get_default_params(p, geom, params = NULL, i = NULL)
```

## Arguments

- p:

  A ggplot object

- geom:

  A character string found in the suffix of a ggplot2 geom function,
  e.g. `"point"`.

- params:

  A [character](https://rdrr.io/r/base/character.html) vector.
  `get_default_params()` returns the default parameter value with a name
  matching each string in `params`. If `params` is
  [`NULL`](https://rdrr.io/r/base/NULL.html) (the default), the default
  values for all parameters are returned.

- i:

  A numerical index, e.g. `1`.

## Value

A named [list](https://rdrr.io/r/base/list.html) of the same length as
`params`, or, if `params` is [`NULL`](https://rdrr.io/r/base/NULL.html),
a named list of default values for all parameters of `geom`.

## See also

Other functions for checking geom parameters:
[`uses_geom_params()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md)

## Examples

``` r
require(ggplot2)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = class))

# Returns the parameters the ggplot would use by default for a layer
get_default_params(p, "smooth", "linetype")
#> $linetype
#> <quosure>
#> expr: ^from_theme(linetype)
#> env:  namespace:ggplot2
#> 
get_default_params(p, "smooth", c("se", "level"))
#> $se
#> [1] TRUE
#> 
#> $level
#> [1] 0.95
#> 
get_default_params(p, "smooth")
#> $colour
#> <quosure>
#> expr: ^from_theme(colour %||% accent)
#> env:  namespace:ggplot2
#> 
#> $fill
#> <quosure>
#> expr: ^from_theme(fill %||% col_mix(ink, paper, 0.6))
#> env:  namespace:ggplot2
#> 
#> $linewidth
#> <quosure>
#> expr: ^from_theme(2 * linewidth)
#> env:  namespace:ggplot2
#> 
#> $linetype
#> <quosure>
#> expr: ^from_theme(linetype)
#> env:  namespace:ggplot2
#> 
#> $weight
#> [1] 1
#> 
#> $alpha
#> [1] 0.4
#> 
#> $na.rm
#> [1] FALSE
#> 
#> $orientation
#> [1] NA
#> 
#> $se
#> [1] TRUE
#> 
#> $method
#> NULL
#> 
#> $formula
#> NULL
#> 
#> $n
#> [1] 80
#> 
#> $span
#> [1] 0.75
#> 
#> $fullrange
#> [1] FALSE
#> 
#> $xseq
#> NULL
#> 
#> $level
#> [1] 0.95
#> 
#> $method.args
#> list()
#> 

# If a parameter does not exist, returns NULL
get_default_params(p, "smooth", "shape")
#> $shape
#> NULL
#> 

# The colo(u)r aesthetic can be matched with or without a u
get_default_params(p, "smooth", "color")
#> $color
#> <quosure>
#> expr: ^from_theme(colour %||% accent)
#> env:  namespace:ggplot2
#> 
get_default_params(p, "smooth", "colour")
#> $colour
#> <quosure>
#> expr: ^from_theme(colour %||% accent)
#> env:  namespace:ggplot2
#> 
```
