# Isolate a geom layer from a plot

`get_geom_layer` returns a geom layer from a plot along with the global
data sets and aesthetic mappings that the layer may inherit from.

## Usage

``` r
get_geom_layer(p, geom = NULL, i = NULL)
```

## Arguments

- p:

  A ggplot object

- geom:

  A character string found in the suffix of a ggplot2 geom function,
  e.g. `"point"`.

- i:

  A numerical index, e.g. `1`.

## Value

An object with class `layer_to_check` to be manipulated further with
ggcheck functions.

## Details

Users can specify a layer in one of 3 ways:

1.  By order of appearance with `i`. The first layer to appear in the
    plot (the one drawn first, on the bottom) corresponds to `i = 1`.

2.  By type of geom with `geom`. `get_geom_layer` will return the first
    layer that uses the geom.

3.  By a combination of `geom` and `i`. `get_geom_layer` will return the
    ith layer that uses the geom.

## Examples

``` r
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(color = "red") +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE)

get_geom_layer(p, i = 1)
#> $layer
#> geom_point: na.rm = FALSE
#> stat_identity: na.rm = FALSE
#> position_identity 
#> 
#> $global_data
#> # A tibble: 234 × 11
#>    manufacturer model   displ  year   cyl trans drv     cty   hwy fl   
#>    <chr>        <chr>   <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#>  1 audi         a4        1.8  1999     4 auto… f        18    29 p    
#>  2 audi         a4        1.8  1999     4 manu… f        21    29 p    
#>  3 audi         a4        2    2008     4 manu… f        20    31 p    
#>  4 audi         a4        2    2008     4 auto… f        21    30 p    
#>  5 audi         a4        2.8  1999     6 auto… f        16    26 p    
#>  6 audi         a4        2.8  1999     6 manu… f        18    26 p    
#>  7 audi         a4        3.1  2008     6 auto… f        18    27 p    
#>  8 audi         a4 qua…   1.8  1999     4 manu… 4        18    26 p    
#>  9 audi         a4 qua…   1.8  1999     4 auto… 4        16    25 p    
#> 10 audi         a4 qua…   2    2008     4 manu… 4        20    28 p    
#> # ℹ 224 more rows
#> # ℹ 1 more variable: class <chr>
#> 
#> $global_mapping
#> Aesthetic mapping: 
#> * `x` -> `displ`
#> * `y` -> `hwy`
#> 
#> attr(,"class")
#> [1] "layer_to_check"
get_geom_layer(p, geom = "smooth")
#> $layer
#> geom_smooth: na.rm = FALSE, orientation = NA, se = FALSE
#> stat_smooth: na.rm = FALSE, orientation = NA, se = FALSE
#> position_identity 
#> 
#> $global_data
#> # A tibble: 234 × 11
#>    manufacturer model   displ  year   cyl trans drv     cty   hwy fl   
#>    <chr>        <chr>   <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#>  1 audi         a4        1.8  1999     4 auto… f        18    29 p    
#>  2 audi         a4        1.8  1999     4 manu… f        21    29 p    
#>  3 audi         a4        2    2008     4 manu… f        20    31 p    
#>  4 audi         a4        2    2008     4 auto… f        21    30 p    
#>  5 audi         a4        2.8  1999     6 auto… f        16    26 p    
#>  6 audi         a4        2.8  1999     6 manu… f        18    26 p    
#>  7 audi         a4        3.1  2008     6 auto… f        18    27 p    
#>  8 audi         a4 qua…   1.8  1999     4 manu… 4        18    26 p    
#>  9 audi         a4 qua…   1.8  1999     4 auto… 4        16    25 p    
#> 10 audi         a4 qua…   2    2008     4 manu… 4        20    28 p    
#> # ℹ 224 more rows
#> # ℹ 1 more variable: class <chr>
#> 
#> $global_mapping
#> Aesthetic mapping: 
#> * `x` -> `displ`
#> * `y` -> `hwy`
#> 
#> attr(,"class")
#> [1] "layer_to_check"
get_geom_layer(p, geom = "point", i = 2)
#> $layer
#> mapping: colour = ~class 
#> geom_point: na.rm = FALSE
#> stat_identity: na.rm = FALSE
#> position_identity 
#> 
#> $global_data
#> # A tibble: 234 × 11
#>    manufacturer model   displ  year   cyl trans drv     cty   hwy fl   
#>    <chr>        <chr>   <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#>  1 audi         a4        1.8  1999     4 auto… f        18    29 p    
#>  2 audi         a4        1.8  1999     4 manu… f        21    29 p    
#>  3 audi         a4        2    2008     4 manu… f        20    31 p    
#>  4 audi         a4        2    2008     4 auto… f        21    30 p    
#>  5 audi         a4        2.8  1999     6 auto… f        16    26 p    
#>  6 audi         a4        2.8  1999     6 manu… f        18    26 p    
#>  7 audi         a4        3.1  2008     6 auto… f        18    27 p    
#>  8 audi         a4 qua…   1.8  1999     4 manu… 4        18    26 p    
#>  9 audi         a4 qua…   1.8  1999     4 auto… 4        16    25 p    
#> 10 audi         a4 qua…   2    2008     4 manu… 4        20    28 p    
#> # ℹ 224 more rows
#> # ℹ 1 more variable: class <chr>
#> 
#> $global_mapping
#> Aesthetic mapping: 
#> * `x` -> `displ`
#> * `y` -> `hwy`
#> 
#> attr(,"class")
#> [1] "layer_to_check"
```
