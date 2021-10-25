require(ggplot2, quietly = TRUE)

p <- ggplot(data = diamonds, aes(x = cut, y = price)) +
  stat_boxplot(outlier.alpha = 0.01) +
  stat_summary()

p2 <- ggplot(data = diamonds, aes(sample = price)) +
  geom_qq()

test_that("Identifies ith stat", {
  expect_equal(
    ith_stat(p, 1),
    "boxplot"
  )
  expect_equal(
    ith_stat(p, 2),
    "summary"
  )
  expect_equal(
    ith_stat(p2, 1),
    "qq"
  )
})

test_that("Checks ith stat", {
  expect_true(ith_stat_is(p, "boxplot", i = 1))
  expect_true(ith_stat_is(p, "summary", i = 2))
  expect_true(ith_stat_is(p2, "qq", i = 1))
})

test_that("Identifies sequence of stats", {
  expect_equal(
    get_stats(p),
    c("boxplot", "summary")
  )
  expect_equal(
    get_stats(p2),
    "qq"
  )
})

test_that("Checks whether a sequence of stats are used", {
  expect_true(uses_stats(p, "boxplot", exact = FALSE))
  # order does not matter if exact = FALSE
  expect_true(uses_stats(p, c("summary", "boxplot"), exact = FALSE))
  # order matters in default case because exact = TRUE
  expect_false(uses_stats(p, c("summary", "boxplot")))
  expect_true(uses_stats(p, c("boxplot", "summary")))
  expect_true(uses_stats(p2, "qq"))
})

test_that("Checks whether stat and geom combinations are used", {
  expect_true(uses_stats(p, stats = c("boxplot", "summary"), geoms = c("boxplot", "pointrange")))
  expect_false(uses_stats(p, stats = c("boxplot", "summary"), geoms = c("boxplot", "point")))
  # geom suffix gets properly mapped
  expect_true(uses_stats(p2, stats = "qq", geoms = "qq"))
  # if instructor already knows geom is "point", that works too
  expect_true(uses_stats(p2, stats = "qq", geoms = "point"))
  expect_false(uses_stats(p2, stats = "qq", geoms = "line"))
  # wrong geom
  expect_error(uses_stats(p, stats = c("boxplot", "summary"), geoms = c("boxplot", "summary")))
  # throw error if length of stats does not match total number of geoms
  expect_error(uses_stats(p, stats = c("boxplot", "summary"), geoms = c("boxplot")))
  expect_error(uses_stats(p2, stats = "qq", geoms = c("qq", "point")))
})

test_that("Throws a grading error when checking an invalid stat", {
  expect_error(uses_stats(p, "line"))
  expect_error(uses_stats(p, c("boxplot", "line")))
  expect_error(uses_stats(p2, "point"))
})

test_that("Throws a grading error when checking an invalid stat", {
  # invalid stats
  expect_error(uses_stats(p, stats = c("boxplott", "summary"), geoms = c("boxplot", "pointrange")))
  expect_error(uses_stats(p2, stats = "qqq", geoms = "qq"))
  # invalid geom
  expect_error(uses_stats(p, stats = c("boxplot", "summary"), geoms = c("boxplot", "summary")))
  expect_error(uses_stats(p2, stats = "qq", geoms = "qqq"))
})

test_that("Checks whether a stat uses a specfic parameter value", {
  # check a default parameter
  expect_true(uses_stat_param(p, stat = "boxplot", params = list(na.rm = FALSE, coef = 1.5)))
  expect_true(uses_stat_param(p, stat = "summary", params = list(fun = NULL)))
  # check set parameters
  expect_true(uses_stat_param(p, stat = "boxplot", params = list(outlier.alpha = 0.01)))
})

test_that("Throws a grading error when checking an invalid geom parameter", {
  # typo
  expect_error(uses_stat_param(p, stat = "boxplot", params = list(coeff = FALSE)))
  # invalid parameter for stat
  expect_error(uses_stat_param(p, stat = "summary", params = list(outlier.alpha = 0.01)))
  expect_error(uses_stat_param(p, stat = "boxplot", params = list(bad_param = NULL)))
  expect_error(uses_stat_param(p, stat = "summary", params = list(bad1 = 1, bad2 = 2)))
})
