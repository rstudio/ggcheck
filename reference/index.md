# Package index

## Get Plot Components

- [`get_coordinate_system()`](https://rstudio.github.io/ggcheck/reference/get_coordinate_system.md)
  : Which coordinate system does a plot use?
- [`get_data()`](https://rstudio.github.io/ggcheck/reference/get_data.md)
  : Get the data set used by a plot or layer
- [`get_default_labels()`](https://rstudio.github.io/ggcheck/reference/get_default_labels.md)
  : What is the default label for a plot aesthetic?
- [`get_default_params()`](https://rstudio.github.io/ggcheck/reference/get_default_params.md)
  : What are the default parameters for a plot layer?
- [`get_geom_layer()`](https://rstudio.github.io/ggcheck/reference/get_geom_layer.md)
  : Isolate a geom layer from a plot
- [`get_geoms()`](https://rstudio.github.io/ggcheck/reference/get_geoms.md)
  : List the geoms used by a plot
- [`get_geoms_stats()`](https://rstudio.github.io/ggcheck/reference/get_geoms_stats.md)
  : List the geom and stat combination used by all layers of a plot.
- [`get_labels()`](https://rstudio.github.io/ggcheck/reference/get_labels.md)
  : List the labels used by a plot
- [`get_mappings()`](https://rstudio.github.io/ggcheck/reference/get_mappings.md)
  : Get aesthetic mappings from a layer or plot
- [`get_stat_layer()`](https://rstudio.github.io/ggcheck/reference/get_stat_layer.md)
  : Isolate a stat layer from a plot
- [`get_stats()`](https://rstudio.github.io/ggcheck/reference/get_stats.md)
  : List the stats used by a plot

## Locate Components by Layer

- [`n_layers()`](https://rstudio.github.io/ggcheck/reference/n_layers.md)
  : How many layers are in a plot?
- [`ith_data()`](https://rstudio.github.io/ggcheck/reference/ith_data.md)
  : Which data set does the ith layer use?
- [`ith_data_is()`](https://rstudio.github.io/ggcheck/reference/ith_data_is.md)
  : Does the ith layer use the correct data set?
- [`ith_geom()`](https://rstudio.github.io/ggcheck/reference/ith_geom.md)
  : Which geom is used in the ith layer?
- [`ith_geom_is()`](https://rstudio.github.io/ggcheck/reference/ith_geom_is.md)
  : Is the ith geom what it should be?
- [`ith_geom_stat()`](https://rstudio.github.io/ggcheck/reference/ith_geom_stat.md)
  : Which geom/stat combination is used in the ith layer?
- [`ith_mappings()`](https://rstudio.github.io/ggcheck/reference/ith_mappings.md)
  : Return the aesthetic mappings used by the ith layer
- [`ith_mappings_use()`](https://rstudio.github.io/ggcheck/reference/ith_mappings_use.md)
  : Does the ith layer use one or more aesthetic mappings?
- [`ith_stat()`](https://rstudio.github.io/ggcheck/reference/ith_stat.md)
  : Which stat is used in the ith layer?
- [`ith_stat_is()`](https://rstudio.github.io/ggcheck/reference/ith_stat_is.md)
  : Is the ith stat what it should be?

## Test that a Plot Uses a Component

- [`identical_aes()`](https://rstudio.github.io/ggcheck/reference/identical_aes.md)
  : Are aesthetic mapping specifications "identical"?
- [`uses_aesthetics()`](https://rstudio.github.io/ggcheck/reference/uses_aesthetics.md)
  : Does a plot use one or more aesthetics?
- [`uses_coordinate_system()`](https://rstudio.github.io/ggcheck/reference/uses_coordinate_system.md)
  : Does a plot use the correct coordinate system?
- [`uses_data()`](https://rstudio.github.io/ggcheck/reference/uses_data.md)
  : Does a plot or layer use the correct data set?
- [`uses_extra_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_extra_mappings.md)
  : Does the plot uses extra aesthetic mappings?
- [`uses_geom_params()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md)
  [`uses_geom_param()`](https://rstudio.github.io/ggcheck/reference/uses_geom_params.md)
  : Does a layer use one of more specific parameters?
- [`uses_geoms()`](https://rstudio.github.io/ggcheck/reference/uses_geoms.md)
  : Does a plot use one or more geoms?
- [`uses_labels()`](https://rstudio.github.io/ggcheck/reference/uses_labels.md)
  : Does a plot use one or more labels?
- [`uses_mappings()`](https://rstudio.github.io/ggcheck/reference/uses_mappings.md)
  : Does a plot or layer use one or more mappings?
- [`uses_stat_param()`](https://rstudio.github.io/ggcheck/reference/uses_stat_param.md)
  : Does a layer use a specific stat parameter?
- [`uses_stats()`](https://rstudio.github.io/ggcheck/reference/uses_stats.md)
  : Does a plot use one or more stats?

## Miscellaneous Helper Functions

- [`is_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  [`stop_if_not_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  [`fail_if_not_ggplot()`](https://rstudio.github.io/ggcheck/reference/is_ggplot.md)
  : Check if an object is a ggplot

- [`get_default_labels()`](https://rstudio.github.io/ggcheck/reference/get_default_labels.md)
  : What is the default label for a plot aesthetic?

- [`get_default_params()`](https://rstudio.github.io/ggcheck/reference/get_default_params.md)
  : What are the default parameters for a plot layer?

- [`default_label()`](https://rstudio.github.io/ggcheck/reference/default_label.md)
  [`default_param()`](https://rstudio.github.io/ggcheck/reference/default_label.md)
  : Placeholders for default values

- [`gradethis_equal(`*`<ggplot>`*`)`](https://rstudio.github.io/ggcheck/reference/gradethis_equal.ggplot.md)
  :

  Compare two `ggplot`s to check whether they are equal
