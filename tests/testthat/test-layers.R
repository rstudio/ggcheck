context("Layers")

test_that("Identifies number of layers", {
  expect_equal(
    n_layers(p),
    2
  )
})
