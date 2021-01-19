context("Geoms")
require(ggplot2)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption ="CAPTION")

d2 <- head(mpg)

p2 <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption ="CAPTION")

test_that("Identifies ith geom", {
  expect_equal(
    ith_geom(p, 1),
    "point"
  )
  expect_equal(
    ith_geom(p, 2),
    "smooth"
  )
})

test_that("Checks ith geom", {
  expect_true(ith_geom_is(p, "point", i = 1))
  expect_true(ith_geom_is(p, "smooth", i = 2))
  expect_false(ith_geom_is(p, "smooth", i = 1))
  expect_false(ith_geom_is(p, "point", i = 2))
})

test_that("Identifies sequence of geoms", {
  expect_equal(
    get_geoms(p),
    c("point", "smooth")
  )
})

test_that("Checks whether a geom is used", {
  expect_true(uses_geoms(p, "point"))
  expect_true(uses_geoms(p, "smooth"))
  expect_true(uses_geoms(p, c("point", "smooth")))
  expect_false(uses_geoms(p, "line"))
  expect_false(uses_geoms(p, c("point", "line")))
})

test_that("Checks whether a sequence of geoms is used", {
  expect_true(uses_geoms(p, c("point", "smooth"), exact = TRUE))
  expect_false(uses_geoms(p, "point", exact = TRUE))
  expect_false(uses_geoms(p, "smooth", exact = TRUE))
  expect_false(uses_geoms(p, c("point", "line"), exact = TRUE))
})

