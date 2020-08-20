context("Test for global plot features")

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

test_that("Identifies local data", {
  expect_equal(
    data_for_layer(p, geom = "point"),
    mpg
  )
  expect_equal(
    data_for_layer(p, i = 1),
    mpg
  )
  expect_equal(
    data_for_layer(p2, geom = "point"),
    d2
  )
  expect_equal(
    data_for_layer(p2, i = 1),
    d2
  )
  expect_equal(
    data_for_layer(p2, geom = "point", i = 2),
    mpg
  )
  expect_equal(
    data_for_layer(p, geom = "point", local_only = TRUE),
    NULL
  )
  expect_equal(
    data_for_layer(p, i = 1, local_only = TRUE),
    NULL
  )
  expect_equal(
    data_for_layer(p2, geom = "point", local_only = TRUE),
    d2
  )
  expect_equal(
    data_for_layer(p2, i = 1, local_only = TRUE),
    d2
  )
  expect_equal(
    data_for_layer(p2, geom = "point", i = 2, local_only = TRUE),
    NULL
  )
})

test_that("Checks whether data is used by layer", {
  expect_true(uses_data_in_layer(p2, d2, i = 1))
  expect_true(uses_data_in_layer(p2, mpg, i = 2))
  expect_false(uses_data_in_layer(p2, mpg, i = 2, local_only = TRUE))
  expect_false(uses_data_in_layer(p2, mpg, i = 1))
  expect_false(uses_data_in_layer(p2, d2, i = 2))
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

test_that("Identifies local mappings", {
  expect_equal(
    mappings_for_layer(p2, geom = "point", i = 1),
    aes(x = displ, y = hwy)
  )
  expect_equal(
    mappings_for_layer(p2, geom = "point", i = 2),
    aes(x = displ, y = hwy, color = class)
  )
  expect_equal(
    mappings_for_layer(p2, geom = "point", i = 1, local_only = TRUE),
    NULL
  )
  expect_equal(
    mappings_for_layer(p2, geom = "point", i = 2, local_only = TRUE),
    aes(color = class)
  )
})

test_that("Checks whether layer mappings exactly match", {
  expect_true(layer_mappings_match(p2, aes(x = displ, y = hwy, color = class), i = 2))
  expect_true(layer_mappings_match(p2, aes(y = hwy, x = displ, color = class), i = 2))
  expect_false(layer_mappings_match(p2, aes(x = displ, y = hwy), i = 2))
  expect_false(layer_mappings_match(p2, aes(x = displ, y = hwy, color = class), i = 2, local_only = TRUE))
  expect_true(layer_mappings_match(p2, aes(color = class), i = 2, local_only = TRUE))
})

test_that("Checks whether layer uses a mapping", {
  expect_true(uses_mappings_in_layer(p2, aes(x = displ), i = 2))
  expect_true(uses_mappings_in_layer(p2, aes(color = class), i = 2))
  expect_false(uses_mappings_in_layer(p2, aes(x = displ), i = 2, local_only = TRUE))
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

test_that("Checks ith geom", {
  expect_true(ith_geom_is(p, "point", i = 1))
  expect_true(ith_geom_is(p, "smooth", i = 2))
  expect_false(ith_geom_is(p, "smooth", i = 1))
  expect_false(ith_geom_is(p, "point", i = 2))
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
