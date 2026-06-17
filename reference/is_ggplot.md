# Check if an object is a ggplot

`is_ggplot()` tests if an object is a
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

`stop_if_not_ggplot()` signals an error if an object is not a
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

`fail_if_not_ggplot()` returns a [failing
grade](https://pkgs.rstudio.com/gradethis/reference/graded.html) if an
object is not a
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Usage

``` r
is_ggplot(p)

stop_if_not_ggplot(p, message = getOption("ggcheck.error"))

fail_if_not_ggplot(
  p = .result,
  message = getOption("ggcheck.fail"),
  env = parent.frame()
)
```

## Arguments

- p:

  An object

- message:

  A message to be displayed if `p` is not a
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

- env:

  Environment in which to find `.result`. Most users of `ggcheck` will
  not need to use this argument.

## Value

`is_ggplot()` returns [`TRUE`](https://rdrr.io/r/base/logical.html) if
`p` is a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object; otherwise it returns
[`FALSE`](https://rdrr.io/r/base/logical.html).

`stop_if_not_ggplot()` returns an error if `p` is not a
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object;
other it invisibly returns [`NULL`](https://rdrr.io/r/base/NULL.html).

`fail_if_not_ggplot()` returns a [failing
grade](https://pkgs.rstudio.com/gradethis/reference/graded.html) if `p`
is not a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object; other it invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Examples

``` r
require(ggplot2)

p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point()
is_ggplot(p_valid)
#> [1] TRUE
stop_if_not_ggplot(p_valid)
fail_if_not_ggplot(p_valid)

p_invalid <- geom_point()
is_ggplot(p_invalid)
#> [1] FALSE
if (FALSE) { # \dontrun{
stop_if_not_ggplot(p_invalid)
} # }
fail_if_not_ggplot(p_valid)
```
