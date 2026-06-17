# Get the data set used by a plot or layer

`get_data` returns the data set used by a ggplot object or a single
layer extracted from the object with
[`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md).

## Usage

``` r
get_data(p, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md).

- local_only:

  `TRUE` or `FALSE`. Should `get_data` onbly return data defined locally
  in the layer?

## Value

A data frame. If no data set is found, `get_data` returns `NULL`

## Details

When passed a ggplot object (i.e. a plot), `get_data` will return only
the data that has been set globally with
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

When passed a single layer from a plot, the behavior of `get_data` will
depend on the `local_only` argument passed to `...`. If
`local_only = TRUE`, `get_data` will return only the data set, if any,
that was defined locally in the function that created the layer. If
`local_only = FALSE`, `get_data` will return the data used by the layer,
whether or not that data was defined globally in
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) or
locally.

## See also

Other functions for checking data:
[`ith_data()`](https://rstudio.github.io/ggcheck/reference/ith_data.md),
[`ith_data_is()`](https://rstudio.github.io/ggcheck/reference/ith_data_is.md),
[`uses_data()`](https://rstudio.github.io/ggcheck/reference/uses_data.md)

## Examples

``` r
require(ggplot2)
d2 <- head(mpg)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point()
get_data(p)
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
get_data(get_geom_layer(p, i = 1))
#> # A tibble: 6 × 11
#>   manufacturer model displ  year   cyl trans    drv     cty   hwy fl   
#>   <chr>        <chr> <dbl> <int> <int> <chr>    <chr> <int> <int> <chr>
#> 1 audi         a4      1.8  1999     4 auto(l5) f        18    29 p    
#> 2 audi         a4      1.8  1999     4 manual(… f        21    29 p    
#> 3 audi         a4      2    2008     4 manual(… f        20    31 p    
#> 4 audi         a4      2    2008     4 auto(av) f        21    30 p    
#> 5 audi         a4      2.8  1999     6 auto(l5) f        16    26 p    
#> 6 audi         a4      2.8  1999     6 manual(… f        18    26 p    
#> # ℹ 1 more variable: class <chr>
```
