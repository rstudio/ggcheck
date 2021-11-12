require(ggplot2, quietly = TRUE)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  labs(
    title = "TITLE",
    subtitle = "SUBTITLE",
    caption = "CAPTION",
    x = "X",
    y = "Y",
    color = NULL
  )

test_that("Identifies labels", {
  expect_equal(
    get_labels(p),
    list(
      x = "X",
      y = "Y",
      colour = NULL,
      title = "TITLE",
      subtitle = "SUBTITLE",
      caption = "CAPTION"
    )
  )

  expect_equal(
    get_labels(p, "x"),
    list(x = "X")
  )

  expect_equal(
    get_labels(p, c("x", "y")),
    list(x = "X", y = "Y")
  )

  expect_equal(
    get_labels(p, c("y", "x")),
    list(y = "Y", x = "X")
  )

  expect_equal(
    get_labels(p, "color"),
    list(colour = NULL)
  )
})

test_that("Checks whether a label is used", {
  expect_true(uses_labels(p, x = "X"))
  expect_true(uses_labels(p, x = "X", y = "Y"))
  expect_true(uses_labels(p, color = NULL))
  expect_true(uses_labels(p, x = "X", y = "Y", color = NULL))
  expect_false(uses_labels(p, x = "Incorrect"))
  expect_false(uses_labels(p, x = "X", y = "Incorrect"))
  expect_false(uses_labels(p, fill = "Incorrect"))
})

test_that("Throws a grading error when label is not a string or NULL", {
  expect_error(uses_labels(p, x = c("X", "Y")))
  expect_error(uses_labels(p, x = 1))
  expect_error(uses_labels(p, color = character(0)))
})

test_that("Throws a grading error when argument is not named", {
  expect_error(uses_labels(p, "X"))
  expect_error(uses_labels(p, c(x = "X", y = "Y")))
  expect_error(uses_labels(p))
})