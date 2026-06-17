# Placeholders for default values

These functions generate placeholder values.

- `default_label()` can be used as a named argument in
  [`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
  to check that a label matches the result of
  [`get_default_labels()`](https://rstudio.github.io/ggcheck/reference/get_default_labels.md)
  with that name.

- `default_param()` can be used as a named argument in
  [`uses_geom_params()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md)
  to check that a parameter matched the result of
  [`get_default_params()`](https://rstudio.github.io/ggcheck/reference/get_default_params.md)
  with that name.

## Usage

``` r
default_label()

default_param()
```

## Value

A placeholder value to be used within
[`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
or
[`uses_geom_params()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md).

## Examples

``` r
require(ggplot2)
#> Loading required package: ggplot2
#> 
#> Attaching package: ‘ggplot2’
#> The following object is masked from ‘package:ggcheck’:
#> 
#>     is_ggplot

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = trans)) +
  geom_smooth(se = FALSE) +
  labs(title = "My plot", x = "Weight", y = "MPG")

uses_labels(p, x = default_label(), color = default_label())
#>     x color 
#> FALSE FALSE 

uses_geom_params(p, "smooth", size = default_param(), se = default_param())
#>  size    se 
#>  TRUE FALSE 
```
