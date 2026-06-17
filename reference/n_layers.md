# How many layers are in a plot?

How many layers are in a plot?

## Usage

``` r
n_layers(p)
```

## Arguments

- p:

  A ggplot object

## Value

Numeric. The number of layers.

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
n_layers(p)
#> [1] 2
```
