context("Stats")
require(ggplot2)

p <- ggplot(data = diamonds, aes(sample = price)) +
  geom_qq() +
  geom_vline(xintercept = 0)

test_that("Identifies ith stat", {
  expect_equal(
    ith_stat(p, 1),
    "qq"
  )
  expect_equal(
    ith_stat(p, 2),
    "identity"
  )
})

test_that("Checks ith stat", {
  expect_true(ith_stat_is(p, "qq", i = 1))
  expect_true(ith_stat_is(p, "identity", i = 2))
})

test_that("Identifies sequence of stats", {
  expect_equal(
    get_stats(p),
    c("qq", "identity")
  )
})

test_that("Checks whether a stat is used", {
  expect_true(uses_stats(p, "qq"))
  expect_true(uses_stats(p, "identity"))
  # order does not matter if exact = FALSE
  expect_true(uses_stats(p, c("identity", "qq")))
  # order matters if exact = TRUE
  expect_false(uses_stats(p, c("identity", "qq"), exact = TRUE))
})

test_that("Throws a grading error when checking an invalid stat", {
  expect_error(uses_stats(p, "line"))
  expect_error(uses_stats(p, c("qq", "line")))
})
