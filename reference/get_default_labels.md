# What is the default label for a plot aesthetic?

What is the default label for a plot aesthetic?

## Usage

``` r
get_default_labels(p, aes = NULL)
```

## Arguments

- p:

  A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object

- aes:

  If `aes` is a [character](https://rdrr.io/r/base/character.html)
  vector, returns only the default labels (based on the plot `p`) that
  correspond to the included aesthetics. Defaults to
  [`NULL`](https://rdrr.io/r/base/NULL.html), which returns the default
  values of all labels.

## Value

A named [list](https://rdrr.io/r/base/list.html) in which each element
is a [character](https://rdrr.io/r/base/character.html) string or
[`NULL`](https://rdrr.io/r/base/NULL.html). Strings are returned for
aesthetics with a default value.
[`NULL`](https://rdrr.io/r/base/NULL.html) is returned for aesthetics
that do not exist in the plot, or non-aesthetic labels that do not have
a default value, like `title`.

## See also

Other functions for checking labels:
[`get_labels()`](https://rstudio.github.io/ggcheck/reference/get_labels.md),
[`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)

## Examples

``` r
require(ggplot2)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class, shape = drv)) +
  geom_smooth() +
  labs(title = "My plot", x = "Weight", y = "MPG", color = NULL)

# Returns the label the ggplot would create by default for an aesthetic
get_default_labels(p, "x")
#> $x
#> [1] "displ"
#> 
get_default_labels(p, c("x", "y"))
#> $x
#> [1] "displ"
#> 
#> $y
#> [1] "hwy"
#> 
get_default_labels(p)
#> $x
#> [1] "displ"
#> 
#> $y
#> [1] "hwy"
#> 
#> $colour
#> [1] "class"
#> 
#> $title
#> NULL
#> 
#> $shape
#> [1] "drv"
#> 

# If an aesthetic does not exist, returns NULL
get_default_labels(p, "size")
#> $size
#> NULL
#> 

# Non-aesthetic labels have no default value, so they also return NULL
get_default_labels(p, "title")
#> $title
#> NULL
#> 
get_default_labels(p, "comment")
#> $comment
#> NULL
#> 

# The colo(u)r aesthetic can be matched with or without a u
get_default_labels(p, "color")
#> $color
#> [1] "class"
#> 
get_default_labels(p, "colour")
#> $colour
#> [1] "class"
#> 
```
