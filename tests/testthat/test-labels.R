require(ggplot2, quietly = TRUE)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  labs(
    title = "TITLE",
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
      title = "TITLE"
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

test_that("Inputs from list", {
  expect_equal(
    uses_labels(p, list(x = "X", y = "Y", color = "C")),
    c(x = TRUE, y = TRUE, color = FALSE)
  )
  expect_equal(
    uses_labels(p, x = "X", list(y = "Y", color = "C")),
    c(x = TRUE, y = TRUE, color = FALSE)
  )

  expect_equal(
    uses_labels(p, !!!list(x = "X", y = "Y", color = "C")),
    c(x = TRUE, y = TRUE, color = FALSE)
  )
  expect_equal(
    uses_labels(p, x = "X", !!!list(y = "Y", color = "C")),
    c(x = TRUE, y = TRUE, color = FALSE)
  )

  expect_equal(
    uses_labels(p, x = "X", list(y = "Y"), !!!list(color = "C")),
    c(x = TRUE, y = TRUE, color = FALSE)
  )
})

test_that("unnamed inputs", {
  expect_equal(uses_labels(p, "x"),        c(x = TRUE))
  expect_equal(uses_labels(p, "x", "y"),   c(x = TRUE, y = TRUE))
  expect_equal(uses_labels(p, "title"),    c(title = TRUE))
  expect_equal(uses_labels(p, "subtitle"), c(subtitle = FALSE))

  expect_equal(uses_labels(p, x = "X", "title"), c(x = TRUE, title = TRUE))
  expect_equal(
    uses_labels(p, list(x = "X", "title")),
    c(x = TRUE, title = TRUE)
  )
  expect_equal(
    uses_labels(p, !!!list(x = "X", "title")),
    c(x = TRUE, title = TRUE)
  )
})

test_that("Throws a grading error when label is not a string or NULL", {
  expect_error(uses_labels(p, x = c("X", "Y")))
  expect_error(uses_labels(p, x = 1))
  expect_error(uses_labels(p, color = FALSE))
})

test_that("Throws a grading error when name is duplicated", {
  expect_error(uses_labels(p, x = "X", x = "X"))
  expect_error(uses_labels(p, x = "X", list(x = "X", y = "Y")))
  expect_error(uses_labels(p, x = "X", !!!list(x = "X", y = "Y")))
  expect_error(uses_labels(p, list(x = "X"), list(x = "X", y = "Y")))
  expect_error(uses_labels(p, list(x = "X"), !!!list(x = "X", y = "Y")))
  expect_error(uses_labels(p, !!!list(x = "X"), !!!list(x = "X", y = "Y")))
})
