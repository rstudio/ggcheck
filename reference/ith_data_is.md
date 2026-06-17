# Does the ith layer use the correct data set?

`ith_data_is` checks whether the student uses the supplied data set for
the ith layer of their plot.

## Usage

``` r
ith_data_is(p, data, i, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md).

- data:

  A data frame

- i:

  A numerical index that corresponds to the first layer of a plot (1),
  the second layer (2), and so on.

- local_only:

  `TRUE` or `FALSE`. See the details.

## Value

`TRUE` or `FALSE`

## Details

Functions that use the `ith_` prefix are designed to eliminate the need
to call `get_geom_layer` to check a specific layer in a plot, e.g.
`p %>% get_geom_layer(geom = "point") %>% uses_data(mpg)`.

If `local_only = TRUE`, `ith_data_is` will check only the data set, if
any, that was defined locally in the function that created the ith
layer. If `local_only = FALSE`, `ith_data_is` will check the data used
by the ith layer, whether or not that data was defined globally in
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) or
locally.

## See also

Other functions for checking data:
[`get_data()`](https://rstudio.github.io/ggcheck/reference/get_data.md),
[`ith_data()`](https://rstudio.github.io/ggcheck/reference/ith_data.md),
[`uses_data()`](https://rstudio.github.io/ggcheck/reference/uses_data.md)

## Examples

``` r
require(ggplot2)
d2 <- head(mpg)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point()
ith_data_is(p, data = head(mpg), i = 1)
#> [1] TRUE
```
