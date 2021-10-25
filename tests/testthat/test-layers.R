require(ggplot2, quietly = TRUE)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(color = "red") +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption = "CAPTION")

test_that("Identifies number of layers", {
  expect_equal(
    n_layers(p),
    3
  )
})

test_that("Identifies a geom layer", {
  default_geom_point <- get_geom_layer(p, geom = "point")
  expect_true(inherits(default_geom_point, "layer_to_check"))
  expect_equal(default_geom_point$layer, p$layers[[1]])
  expect_equal(default_geom_point$global_data, p$data)
  expect_equal(default_geom_point$global_mapping, p$mapping)
})

test_that("Identifies the ith geom layer", {
  second_geom_point <- get_geom_layer(p, geom = "point", i = 2)
  expect_true(inherits(second_geom_point, "layer_to_check"))
  expect_equal(second_geom_point$layer, p$layers[[2]])
  expect_equal(second_geom_point$global_data, p$data)
  expect_equal(second_geom_point$global_mapping, p$mapping)
})

test_that("Identifies a stat layer", {
  default_stat_smooth <- get_stat_layer(p, stat = "smooth")
  expect_true(inherits(default_stat_smooth, "layer_to_check"))
  expect_equal(default_stat_smooth$layer, p$layers[[3]])
  expect_equal(default_stat_smooth$global_data, p$data)
  expect_equal(default_stat_smooth$global_mapping, p$mapping)
})

test_that("Identifies a stat layer", {
  first_stat_smooth <- get_stat_layer(p, stat = "smooth", i = 1)
  expect_true(inherits(first_stat_smooth, "layer_to_check"))
  expect_equal(first_stat_smooth$layer, p$layers[[3]])
  expect_equal(first_stat_smooth$global_data, p$data)
  expect_equal(first_stat_smooth$global_mapping, p$mapping)
})

test_that("Throws an error for getting a layer with invalid parameters", {
  expect_error(get_geom_layer(p))
  expect_error(get_geom_layer(p, i = 0))
  expect_error(get_geom_layer(p, ""))
  expect_error(get_geom_layer(p, "line"))
  # geom and stat should not be specified together
  expect_error(get_geom_layer(p, geom = "point", stat = "identity"))
})
