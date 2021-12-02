require(ggplot2)


test_that("is_ggplot", {
  p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point()
  expect_true(is_ggplot(p_valid))

  p_invalid <- geom_point()
  expect_false(is_ggplot(p_invalid))

  p_basic <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
  expect_true(is_ggplot(p_basic))
})
