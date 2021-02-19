context("Layers")
require(ggplot2)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption = "CAPTION")

test_that("Identifies number of layers", {
  expect_equal(
    n_layers(p),
    2
  )
})

# TODO add tests for getting a layer by geom, stat and i

test_that("Throws an error for getting a layer with invalid parameter", {
  expect_error(get_layer(p))
  expect_error(get_layer(p, i = 0))
  expect_error(get_layer(p, ""))
  expect_error(get_layer(p, "line"))
  # geom and stat should not be specified together
  expect_error(get_layer(p, geom = "point", stat = "identity"))
})
