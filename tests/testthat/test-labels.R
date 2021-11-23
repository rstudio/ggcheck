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
    color = NULL,
    fill = character(0)
  )

test_that("Identifies labels", {
  expect_equal(
    get_labels(p),
    list(
      x = "X",
      y = "Y",
      colour = NULL,
      fill = character(0),
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
  expect_equal(uses_labels(p, x = "X"),          c(x = TRUE))
  expect_equal(uses_labels(p, x = "X", y = "Y"), c(x = TRUE, y = TRUE))

  expect_equal(uses_labels(p, color = NULL),         c(color = TRUE))
  expect_equal(uses_labels(p, fill  = NULL),         c(fill  = TRUE))
  expect_equal(uses_labels(p, color = character(0)), c(color = TRUE))
  expect_equal(uses_labels(p, fill  = character(0)), c(fill  = TRUE))

  expect_equal(
    uses_labels(p, x = "X", y = "Y", color = NULL),
    c(x = TRUE, y = TRUE, color = TRUE)
  )

  expect_equal(uses_labels(p, x = "Incorrect"), c(x = FALSE))
  expect_equal(uses_labels(p, x = "X", y = "Incorrect"), c(x = TRUE, y = FALSE))
  expect_equal(uses_labels(p, fill = "Incorrect"), c(fill = FALSE))
})

test_that("Throws a grading error when label is not a string or NULL", {
  expect_error(uses_labels(p, x = c("X", "Y")))
  expect_error(uses_labels(p, x = 1))
  expect_error(uses_labels(p, color = FALSE))
})

test_that("Throws a grading error when argument is not named", {
  expect_error(uses_labels(p, "X"))
  expect_error(uses_labels(p, c(x = "X", y = "Y")))
  expect_error(uses_labels(p))
})
