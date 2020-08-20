context("Layers")

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption ="CAPTION")

test_that("Identifies number of layers", {
  expect_equal(
    n_layers(p),
    2
  )
})
