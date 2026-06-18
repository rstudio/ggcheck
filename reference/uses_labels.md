# Does a plot use one or more labels?

`uses_labels()` tests whether a plot uses one or more
[labels](https://ggplot2.tidyverse.org/reference/labs.html).

## Usage

``` r
uses_labels(p, ...)
```

## Arguments

- p:

  A ggplot object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  [Character](https://rdrr.io/r/base/character.html) strings. Unnamed
  arguments will check whether a label exists for that aesthetic. Named
  arguments will check whether the aesthetic with the same name has a
  label with a matching value. Each argument should have a matching
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
  [aesthetic](https://ggplot2.tidyverse.org/reference/aes.html) or
  [label](https://ggplot2.tidyverse.org/reference/labs.html). Strings
  may be input as individual arguments or as list elements.

## Value

A named logical vector of the same length as the number of inputs to
`...`.

## Details

Note that `uses_labels()` will match
[`NULL`](https://rdrr.io/r/base/NULL.html) if a label is explicitly set
to [`NULL`](https://rdrr.io/r/base/NULL.html) ***or*** if a requested
aesthetic is not present in the plot.

## See also

Other functions for checking labels:
[`get_default_labels()`](https://rstudio.github.io/ggcheck/reference/get_default_labels.md),
[`get_labels()`](https://rstudio.github.io/ggcheck/reference/get_labels.md)

## Examples

``` r
require(ggplot2)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class, shape = drv)) +
  geom_smooth() +
  labs(title = "My plot", x = "Weight", y = "MPG", color = NULL)

# Unnamed arguments check if a label is set for the given aesthetic
uses_labels(p, "title", "subtitle", "x", "y")
#>    title subtitle        x        y 
#>     TRUE    FALSE     TRUE     TRUE 

# The check will return TRUE for labels set to NULL
uses_labels(p, "color")
#> color 
#>  TRUE 

# The check will return TRUE for aesthetics with default labels
uses_labels(p, "shape")
#> shape 
#>  TRUE 

# Named arguments check if the label matches an expected value
uses_labels(p, x = "Weight")
#>    x 
#> TRUE 
uses_labels(p, x = "Weight", y = "MPG", color = NULL)
#>     x     y color 
#>  TRUE  TRUE  TRUE 

# You can check for default labels with default_label()
uses_labels(p, shape = default_label(), x = default_label())
#> shape     x 
#>  TRUE FALSE 

# The colo(u)r aesthetic can be matched with or without a u
uses_labels(p, color = NULL)
#> color 
#>  TRUE 
uses_labels(p, colour = NULL)
#> colour 
#>   TRUE 

# Inputs can be passed from a list, with or without the !!! operator
label_list <- list(x = "Weight", y = "MPG", color = NULL)
uses_labels(p, label_list)
#>     x     y color 
#>  TRUE  TRUE  TRUE 
uses_labels(p, !!!label_list)
#>     x     y color 
#>  TRUE  TRUE  TRUE 
```
