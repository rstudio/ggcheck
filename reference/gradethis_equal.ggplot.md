# Compare two `ggplot`s to check whether they are equal

Compare two `ggplot`s to check whether they are equal

## Usage

``` r
# S3 method for class 'ggplot'
gradethis_equal(x, y, ...)
```

## Arguments

- x, y:

  Two `ggplot` objects to compare

- ...:

  Unused

## Value

A [logical](https://rdrr.io/r/base/logical.html) value of length one, or
an internal gradethis error.

## See also

[`gradethis::gradethis_equal()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_equal.html)
for the generic function.

## Examples

``` r
library(ggplot2)
library(ggcheck)
library(gradethis)

cty_plot <- ggplot(mpg, aes(x = displ, y = cty)) + geom_point()
hwy_plot <- ggplot(mpg, aes(x = displ, y = cty)) + geom_point()

gradethis_equal(cty_plot, hwy_plot)
#> [1] TRUE
gradethis_equal(cty_plot, cty_plot)
#> [1] TRUE
```
