context("Test for global plot features")

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth(se = FALSE) +
    labs(title = "TITLE", subtitle = "SUBTITLE", caption ="CAPTION")

test_that("Identifies global data", {
  expect_equal(
    global_data(p),
    mpg
  )
})

test_that("Checks whether data is used globally", {
  expect_true(uses_global_data(p, data = mpg))
  expect_false(uses_global_data(p, data = mtcars))
})


test_that("Identifies global mapping", {
  expect_equal(
    global_mappings(p),
    aes(x = displ, y = hwy)
  )
})

test_that("Checks whether mappings are used globally", {
  expect_true(uses_global_mappings(p, mappings = aes(x= displ, y = hwy)))
  expect_false(uses_global_mappings(p, mappings = aes(x= displ)))
  expect_false(uses_global_mappings(p, mappings = aes(x= hwy, y = displ)))
})

test_that("Checks whether a single mapping is used globally", {
  expect_true(uses_global_mapping(p, mapping = aes(x = displ)))
  expect_true(uses_global_mapping(p, mapping = aes(y = hwy)))
  expect_false(uses_global_mapping(p, mapping = aes(x = hwy)))
})

test_that("Identifies number of layers", {
  expect_equal(
    n_layers(p),
    2
  )
})

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

test_that("Identifies sequence of geoms", {
  expect_equal(
    geoms(p),
    c("point", "smooth")
  )
})

test_that("Checks whether a geom is used", {
  expect_true(uses_geom(p, "point"))
  expect_true(uses_geom(p, "smooth"))
  expect_false(uses_geom(p, "line"))
})

test_that("Checks whether a sequence of geoms is used", {
  expect_true(uses_geoms(p, c("point", "smooth")))
  expect_false(uses_geoms(p, "point"))
  expect_false(uses_geoms(p, "smooth"))
  expect_false(uses_geoms(p, c("point", "line")))
})

test_that("Identifies coordinate system", {
  expect_equal(
    coordinate_system(p),
    "cartesian"
  )
  expect_equal(
    coordinate_system(p + coord_cartesian()),
    "cartesian"
  )
  expect_equal(
    coordinate_system(p + coord_cartesian(xlim = c(0,1))),
    "cartesian"
  )
})

test_that("Checks whether a coordinate system is used", {
  expect_true(uses_coordinate_system(p, "cartesian"))
  expect_true(uses_coordinate_system(p + coord_cartesian(), "cartesian"))
  expect_true(uses_coordinate_system(p + coord_cartesian(xlim = c(0,1)), "cartesian"))
  expect_false(uses_coordinate_system(p, "polar"))
})
