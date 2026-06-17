# Does a plot or layer use the correct data set?

`uses_data` checks whether the data set used by a plot or layer matches
the data set provided.

## Usage

``` r
uses_data(p, data, local_only = FALSE)
```

## Arguments

- p:

  A ggplot object or a layer extracted from a ggplot object with
  [`get_geom_layer`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md).

- data:

  A data frame

- local_only:

  `TRUE` or `FALSE`. See the details.

## Value

A data frame.

## Details

When passed a ggplot object (i.e. a plot), `uses_data` will check only
the data that has been set globally with
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

When passed a single layer from a plot, the behavior of `uses_data` will
depend on the `local_only` argument passed to `...`. If
`local_only = TRUE`, `uses_data` will check only the data set, if any,
that was defined locally in the function that created the layer. If
`local_only = FALSE`, `uses_data` will check the data used by the layer,
whether or not that data was defined globally in
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) or
locally.

## See also

Other functions for checking data:
[`get_data()`](https://rstudio.github.io/ggcheck/reference/get_data.md),
[`ith_data()`](https://rstudio.github.io/ggcheck/reference/ith_data.md),
[`ith_data_is()`](https://rstudio.github.io/ggcheck/reference/ith_data_is.md)

## Examples

``` r
require(ggplot2)
d2 <- head(mpg)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point()
uses_data(p, mpg)
#> [1] TRUE
uses_data(get_geom_layer(p, i = 1), data = head(mpg))
#> [1] TRUE
```
