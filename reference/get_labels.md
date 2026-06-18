# List the labels used by a plot

`get_labels()` returns a named [list](https://rdrr.io/r/base/list.html)
of [labels](https://ggplot2.tidyverse.org/reference/labs.html), written
as [character](https://rdrr.io/r/base/character.html) strings,
indicating which labels are used by a plot.

## Usage

``` r
get_labels(p, aes = NULL)
```

## Arguments

- p:

  A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object

- aes:

  If `aes` is a [character](https://rdrr.io/r/base/character.html)
  vector, returns only the labels corresponding to the included
  aesthetics. Defaults to [`NULL`](https://rdrr.io/r/base/NULL.html),
  which returns all labels.

## Value

A named list of character strings.

## Details

Note that `get_labels()` will return
[`NULL`](https://rdrr.io/r/base/NULL.html) if a label is explicitly set
to [`NULL`](https://rdrr.io/r/base/NULL.html) ***or*** if a requested
aesthetic is not present in the plot.

## See also

Other functions for checking labels:
[`get_default_labels()`](https://rstudio.github.io/ggcheck/reference/get_default_labels.md),
[`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)

## Examples

``` r
require(ggplot2)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth() +
  labs(x = "Weight", y = "MPG", color = NULL)

get_labels(p)
#> $x
#> [1] "Weight"
#> 
#> $y
#> [1] "MPG"
#> 
#> $colour
#> NULL
#> 
get_labels(p, c("x", "y"))
#> $x
#> [1] "Weight"
#> 
#> $y
#> [1] "MPG"
#> 

# The colo(u)r aesthetic can be matched with or without a u
get_labels(p, "color")
#> $color
#> NULL
#> 
get_labels(p, "colour")
#> $colour
#> NULL
#> 
```
