# Isolate a stat layer from a plot

`get_stat_layer` returns a stat layer from a plot along with the global
data sets and aesthetic mappings that the layer may inherit from.

## Usage

``` r
get_stat_layer(p, stat = NULL, i = NULL)
```

## Arguments

- p:

  A ggplot object

- stat:

  A character string found in the suffix of a ggplot2 stat function,
  e.g. `"bin"`.

- i:

  A numerical index, e.g. `1`.

## Value

An object with class `layer_to_check` to be manipulated further with
ggcheck functions.

## Details

Users can specify a layer in one of 3 ways:

1.  By order of appearance with `i`. The first layer to appear in the
    plot (the one drawn first, on the bottom) corresponds to `i = 1`.

2.  By type of stat with `stat`. `get_stat_layer` will return the first
    layer that uses the stat

3.  By a combination of `stat` and `i`. `get_stat_layer` will return the
    ith layer that uses the stat

## Examples

``` r
require(ggplot2)
p <- ggplot(data = diamonds, aes(price)) +
  stat_bin(bins = 20, binwidth = 500)

get_stat_layer(p, i = 1)
#> $layer
#> geom_bar: na.rm = FALSE, orientation = NA
#> stat_bin: na.rm = FALSE, orientation = NA, binwidth = 500, bins = 20, center = NULL, boundary = NULL, closed = c("right", "left"), pad = FALSE, breaks = NULL, drop = none
#> position_stack 
#> 
#> $global_data
#> # A tibble: 53,940 × 10
#>    carat cut       color clarity depth table price     x     y     z
#>    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#>  1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
#>  2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
#>  3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
#>  4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
#>  5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
#>  6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
#>  7  0.24 Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47
#>  8  0.26 Very Good H     SI1      61.9    55   337  4.07  4.11  2.53
#>  9  0.22 Fair      E     VS2      65.1    61   337  3.87  3.78  2.49
#> 10  0.23 Very Good H     VS1      59.4    61   338  4     4.05  2.39
#> # ℹ 53,930 more rows
#> 
#> $global_mapping
#> Aesthetic mapping: 
#> * `x` -> `price`
#> 
#> attr(,"class")
#> [1] "layer_to_check"
get_stat_layer(p, stat = "bin")
#> $layer
#> geom_bar: na.rm = FALSE, orientation = NA
#> stat_bin: na.rm = FALSE, orientation = NA, binwidth = 500, bins = 20, center = NULL, boundary = NULL, closed = c("right", "left"), pad = FALSE, breaks = NULL, drop = none
#> position_stack 
#> 
#> $global_data
#> # A tibble: 53,940 × 10
#>    carat cut       color clarity depth table price     x     y     z
#>    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#>  1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
#>  2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
#>  3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
#>  4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
#>  5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
#>  6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
#>  7  0.24 Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47
#>  8  0.26 Very Good H     SI1      61.9    55   337  4.07  4.11  2.53
#>  9  0.22 Fair      E     VS2      65.1    61   337  3.87  3.78  2.49
#> 10  0.23 Very Good H     VS1      59.4    61   338  4     4.05  2.39
#> # ℹ 53,930 more rows
#> 
#> $global_mapping
#> Aesthetic mapping: 
#> * `x` -> `price`
#> 
#> attr(,"class")
#> [1] "layer_to_check"
```
