# Which data set does the ith layer use?

`ith_data` returns the data set used by the ith layer.

## Usage

``` r
ith_data(p, i, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md).

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on.

- local_only:

  `TRUE` or `FALSE`. See the details.

## Value

A data frame. If no data set is found, `ith_data` returns `NULL`.

## Details

If `local_only = TRUE`, `ith_data` returns the data set, if any, that
was defined locally in the function that created the ith layer. If
`local_only = FALSE`, `ith_data` returns the data used by the ith layer,
whether or not that data was defined globally in
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) or
locally.

Functions that use the `ith_` prefix are designed to eliminate the need
to call `get_geom_layer` to check a specific layer in a plot, e.g.
`p %>% get_geom_layer(geom = "point") %>% get_data()`.

## See also

Other functions for checking data:
[`get_data()`](https://rstudio.github.io/ggcheck/reference/get_data.md),
[`ith_data_is()`](https://rstudio.github.io/ggcheck/reference/ith_data_is.md),
[`uses_data()`](https://rstudio.github.io/ggcheck/reference/uses_data.md)

## Examples

``` r
require(ggplot2)
d2 <- head(mpg)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point()
ith_data(p, i = 1)
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
