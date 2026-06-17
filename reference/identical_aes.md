# Are aesthetic mapping specifications "identical"?

The ggplot2 package uses quosures to record aesthetic mappings. These
record both the mapping described as well as the environment in which
the mapping was described. As a result, it is difficult to compare
mappings created by students in one environment to mappings created on
the fly by graders in another environment. `identical_aes` facilitates
comparison by ignoring the environments associated with an aesthetic
mapping specification. If the two specifications contain identical
expressions, e.g. `x = displ`, etc., `identical_aes` returns `TRUE`.

## Usage

``` r
identical_aes(a1, a2)
```

## Arguments

- a1:

  The output of
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html), perhaps
  extracted from a ggplot object.

- a2:

  The output of
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html), perhaps
  extracted from a ggplot object.

## Value

`TRUE` or `FALSE`

## See also

Other functions for checking mappings:
[`get_mappings()`](https://rstudio.github.io/ggcheck/reference/get_mappings.md),
[`ith_mappings()`](https://rstudio.github.io/ggcheck/reference/ith_mappings.md),
[`ith_mappings_use()`](https://rstudio.github.io/ggcheck/reference/ith_mappings_use.md),
[`uses_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_mappings.md)
