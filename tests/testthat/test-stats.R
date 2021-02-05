context("Stats")
require(ggplot2)

p <- ggplot(data = diamonds, aes(x = cut, y = price)) +
  stat_boxplot(outlier.alpha = 0.01) +
  stat_summary()

test_that("Identifies ith stat", {
  expect_equal(
    ith_stat(p, 1),
    "boxplot"
  )
  expect_equal(
    ith_stat(p, 2),
    "summary"
  )
})

test_that("Checks ith stat", {
  expect_true(ith_stat_is(p, "boxplot", i = 1))
  expect_true(ith_stat_is(p, "summary", i = 2))
})

test_that("Identifies sequence of stats", {
  expect_equal(
    get_stats(p),
    c("boxplot", "summary")
  )
})

test_that("Checks whether a sequence of stats are used", {
  expect_true(uses_stats(p, "boxplot"))
  # order does not matter if exact = FALSE
  expect_true(uses_stats(p, c("summary", "boxplot")))
  # order matters if exact = TRUE
  expect_false(uses_stats(p, c("summary", "boxplot"), exact = TRUE))
  expect_true(uses_stats(p, c("boxplot", "summary"), exact = TRUE))
})

test_that("Throws a grading error when checking an invalid stat", {
  expect_error(uses_stats(p, "line"))
  expect_error(uses_stats(p, c("boxplot", "line")))
})
