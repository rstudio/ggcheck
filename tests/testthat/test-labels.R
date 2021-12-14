require(ggplot2, quietly = TRUE)

p <-
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class, shape = drv)) +
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
      title = "TITLE",
      shape = "drv"
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
    list(color = NULL)
  )

  expect_equal(
    get_labels(p, "subtitle"),
    list(subtitle = NULL)
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
  expect_equal(uses_labels(p, "shape"),    c(shape = TRUE))
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

test_that("default labels", {
  # Returns the label the ggplot would create by default for an aesthetic
  expect_equal(default_label(p, "x"),     list(x     = "displ"))
  expect_equal(default_label(p, "y"),     list(y     = "hwy"))
  expect_equal(default_label(p, "color"), list(color = "class"))
  expect_equal(default_label(p, "shape"), list(shape = "drv"))

  expect_equal(
    default_label(p),
    list(
      x      = "displ",
      y      = "hwy",
      colour = "class",
      fill   = NULL,
      title  = NULL,
      shape  = "drv"
    )
  )

  # If an aesthetic does not exist, returns NULL
  expect_equal(default_label(p, "size"), list(size = NULL))

  # If an aesthetic has no default, returns NULL
  expect_equal(default_label(p, "title"), list(title = NULL))

  # The colo(u)r aesthetic can be matched with or without a u
  expect_equal(default_label(p, "color"),  list(color  = "class"))
  expect_equal(default_label(p, "colour"), list(colour = "class"))

  # Works with no arguments within `uses_labels()`
  expect_equal(
    uses_labels(p, x = default_label(), shape = default_label()),
    c(x = FALSE, shape = TRUE)
  )
  expect_equal(uses_labels(p, color = default_label()), c(color = FALSE))
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

test_that("stop_if_not_ggplot", {
  expect_error(
    uses_labels(
      geom_point(data = mpg, mapping = aes(x = displ, y = hwy)),
      x = "displ", y = "hwy"
    ),
    '`p` must be a "ggplot" object'
  )
})
