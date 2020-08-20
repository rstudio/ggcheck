context("Geoms")

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

