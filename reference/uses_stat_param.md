# Does a layer use a specific stat parameter?

`uses_stat_param` is a mirror function of `uses_geom_param` but instead
of checking a plot's geom layer, it checks that a plot's stat layer uses
a specific stat parameter.

## Usage

``` r
uses_stat_param(p, stat, params, i = NULL)
```

## Arguments

- p:

  A ggplot object

- stat:

  A character string found in the suffix of a ggplot2 stat function,
  e.g. `"bin"`.

- params:

  A named list of stat or geom parameter values, e.g. `list(bins = 200)`

- i:

  A numerical index, e.g. `1`.

## Value

A boolean

## Details

To specify a specific stat layer, either specify using position using
the `i` index or by using a combination of `stat` function suffix name
and `i` to check the ith layer that uses the stat.

## Examples

``` r
require(ggplot2)
p <- ggplot(diamonds, aes(carat)) +
  stat_bin(bins = 200)
uses_stat_param(p, stat = "bin", params = list(bins = 200))
#> [1] TRUE
```
