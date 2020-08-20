context("Coordinate Systems")


p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "TITLE", subtitle = "SUBTITLE", caption ="CAPTION")

test_that("Identifies coordinate system", {
  expect_equal(
    get_coordinate_system(p),
    "cartesian"
  )
  expect_equal(
    get_coordinate_system(p + coord_cartesian()),
    "cartesian"
  )
  expect_equal(
    get_coordinate_system(p + coord_cartesian(xlim = c(0,1))),
    "cartesian"
  )
})

test_that("Checks whether a coordinate system is used", {
  expect_true(uses_coordinate_system(p, "cartesian"))
  expect_true(uses_coordinate_system(p + coord_cartesian(), "cartesian"))
  expect_true(uses_coordinate_system(p + coord_cartesian(xlim = c(0,1)), "cartesian"))
  expect_false(uses_coordinate_system(p, "polar"))
})

