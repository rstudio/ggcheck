# Changelog

## ggcheck 0.0.5

- Add
  [`gradethis_equal()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_equal.html)
  method for `ggplot` objects
  ([\#37](https://github.com/rstudio/ggcheck/issues/37)).

## ggcheck 0.0.4 (2022-04-14)

- Allow uses_mappings() to find mappings that appear in every layer of
  the plot ([\#35](https://github.com/rstudio/ggcheck/issues/35)).

## ggcheck 0.0.3 (2022-02-24)

- Fixed a bug when
  [`fail_if_not_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  attempted to access a `gradethis` `.result` object
  ([\#33](https://github.com/rstudio/ggcheck/issues/33))

## ggcheck 0.0.2 (2021-12-22)

### New Features

- New functions for checking plot labels
  ([\#22](https://github.com/rstudio/ggcheck/issues/22))
  - [`get_labels()`](https://rstudio.github.io/ggcheck/reference/get_labels.md)
    lists (a subset of) the labels of a plot
  - [`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
    checks if labels match their expected values
- Added
  [`fail_if_not_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  for use in grading code to check that the submitted result is a
  ggplot, powered by the lower-level testing function
  [`is_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  ([\#29](https://github.com/rstudio/ggcheck/issues/29)).

### Improvements and Updates

- Improve handling of `...` in
  [`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
  ([\#26](https://github.com/rstudio/ggcheck/issues/26))
- Unnamed arguments to
  [`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
  check if label is set
  ([\#27](https://github.com/rstudio/ggcheck/issues/27))
- Check for `aes` params in
  [`uses_geom_param()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md)
  ([\#28](https://github.com/rstudio/ggcheck/issues/28))

## ggcheck 0.0.1 (2021-10-22)

- Initial release of ggcheck
