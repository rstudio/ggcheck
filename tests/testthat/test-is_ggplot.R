require(ggplot2)

p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point()

p_invalid <- geom_point()

p_basic <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))

p_minimal <- ggplot()

test_that("is_ggplot", {
  expect_true(is_ggplot(p_valid))
  expect_false(is_ggplot(p_invalid))
  expect_true(is_ggplot(p_basic))
  expect_true(is_ggplot(p_minimal))
})

test_that("stop_if_not_ggplot", {
  expect_invisible(expect_null(stop_if_not_ggplot(p_valid)))
  expect_error(stop_if_not_ggplot(p_invalid))
  expect_invisible(expect_null(stop_if_not_ggplot(p_basic)))
  expect_invisible(expect_null(stop_if_not_ggplot(p_minimal)))
})
