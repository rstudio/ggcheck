# ggcheck 0.0.5



# ggcheck 0.0.4 (2022-04-14)

*  Allow uses_mappings() to find mappings that appear in every layer of the plot (#35).

# ggcheck 0.0.3 (2022-02-24)

- Fixed a bug when `fail_if_not_ggplot()` attempted to access a `gradethis` `.result` object (#33)

# ggcheck 0.0.2 (2021-12-22)

## New Features

* New functions for checking plot labels (#22)
    - `get_labels()` lists (a subset of) the labels of a plot
    - `uses_labels()` checks if labels match their expected values
- Added `fail_if_not_ggplot()` for use in grading code to check that the submitted result is a ggplot, powered by the lower-level testing function `is_ggplot()` (#29).

## Improvements and Updates

- Improve handling of `...` in `uses_labels()` (#26)
- Unnamed arguments to `uses_labels()` check if label is set (#27)
- Check for `aes` params in `uses_geom_param()` (#28)

# ggcheck 0.0.1 (2021-10-22)

- Initial release of ggcheck
