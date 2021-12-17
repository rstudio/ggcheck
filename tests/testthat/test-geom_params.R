require(ggplot2, quietly = TRUE)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE)

p3 <- ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot(varwidth = TRUE, outlier.alpha = 0.01)

p4 <- ggplot(data = diamonds, aes(price)) +
  geom_histogram(bins = 20, binwidth = 500)

p5 <- ggplot(data = diamonds, aes(price)) +
  geom_histogram(fill = "blue", color = "red")

test_that("Checks whether a geom uses a specfic parameter value", {
  # check a default parameter
  expect_equal(
    uses_geom_params(p, geom = "smooth", params = list(na.rm = FALSE)),
    c(na.rm = TRUE)
  )
  expect_equal(
    uses_geom_params(p, geom = "smooth", na.rm = FALSE),
    c(na.rm = TRUE)
  )

  # check set parameters
  expect_equal(
    uses_geom_params(p, geom = "smooth", params = list(se = FALSE)),
    c(se = TRUE)
  )
  expect_equal(
    uses_geom_params(p, geom = "smooth", se = FALSE),
    c(se = TRUE)
  )

  expect_equal(
    uses_geom_params(p3, geom = "boxplot", params = list(varwidth = TRUE, outlier.alpha = 0.01)),
    c(varwidth = TRUE, outlier.alpha = TRUE)
  )

  expect_equal(
    uses_geom_params(p3, geom = "boxplot", varwidth = TRUE, outlier.alpha = 0.01),
    c(varwidth = TRUE, outlier.alpha = TRUE)
  )

  # check parameter of a geom which is a stat parameter
  expect_equal(
    uses_geom_params(p4, geom = "histogram", params = list(bins = 20, binwidth = 500)),
    c(bins = TRUE, binwidth = TRUE)
  )
  expect_equal(
    uses_geom_params(p4, geom = "histogram", bins = 20, binwidth = 500),
    c(bins = TRUE, binwidth = TRUE)
  )

  # check parameter of a geom which is an aes parameter
  expect_equal(
    uses_geom_params(p5, geom = "histogram", params = list(fill = "blue")),
    c(fill = TRUE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", fill = "blue"),
    c(fill = TRUE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", fill = "red"),
    c(fill = FALSE)
  )
})

test_that("uses_geom_param() alias", {
  # check a default parameter
  expect_equal(
    uses_geom_param(p, geom = "smooth", params = list(na.rm = FALSE)),
    c(na.rm = TRUE)
  )
  expect_equal(
    uses_geom_param(p, geom = "smooth", na.rm = FALSE),
    c(na.rm = TRUE)
  )

  # check set parameters
  expect_equal(
    uses_geom_param(p, geom = "smooth", params = list(se = FALSE)),
    c(se = TRUE)
  )
  expect_equal(
    uses_geom_param(p, geom = "smooth", se = FALSE),
    c(se = TRUE)
  )

  expect_equal(
    uses_geom_param(p3, geom = "boxplot", params = list(varwidth = TRUE, outlier.alpha = 0.01)),
    c(varwidth = TRUE, outlier.alpha = TRUE)
  )

  expect_equal(
    uses_geom_param(p3, geom = "boxplot", varwidth = TRUE, outlier.alpha = 0.01),
    c(varwidth = TRUE, outlier.alpha = TRUE)
  )

  # check parameter of a geom which is a stat parameter
  expect_equal(
    uses_geom_param(p4, geom = "histogram", params = list(bins = 20, binwidth = 500)),
    c(bins = TRUE, binwidth = TRUE)
  )
  expect_equal(
    uses_geom_param(p4, geom = "histogram", bins = 20, binwidth = 500),
    c(bins = TRUE, binwidth = TRUE)
  )

  # check parameter of a geom which is an aes parameter
  expect_equal(
    uses_geom_param(p5, geom = "histogram", params = list(fill = "blue")),
    c(fill = TRUE)
  )
  expect_equal(
    uses_geom_param(p5, geom = "histogram", fill = "blue"),
    c(fill = TRUE)
  )
  expect_equal(
    uses_geom_param(p5, geom = "histogram", fill = "red"),
    c(fill = FALSE)
  )

  # support color and colour
  expect_equal(
    uses_geom_param(p5, geom = "histogram", color = "red"),
    c(color = TRUE)
  )
  expect_equal(
    uses_geom_param(p5, geom = "histogram", colour = "red"),
    c(colour = TRUE)
  )
  expect_equal(
    uses_geom_param(p5, geom = "histogram", color = "blue"),
    c(color = FALSE)
  )
  expect_equal(
    uses_geom_param(p5, geom = "histogram", colour = "blue"),
    c(colour = FALSE)
  )
})

test_that("unnamed uses_geom_params", {
  expect_equal(
    uses_geom_params(p5, geom = "histogram", "fill"),
    c(fill = TRUE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", "color"),
    c(color = TRUE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", "colour"),
    c(colour = TRUE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", "linetype"),
    c(linetype = FALSE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", "fill", "color", "linetype"),
    c(fill = TRUE, color = TRUE, linetype = FALSE)
  )
  expect_equal(
    uses_geom_params(p5, geom = "histogram", fill = "blue", "color", "linetype"),
    c(fill = TRUE, color = TRUE, linetype = FALSE)
  )
})

test_that("Return FALSE when checking an invalid geom parameter", {
  # typo
  expect_equal(
    uses_geom_param(p, geom = "smooth", params = list(see = FALSE)),
    c(see = FALSE)
  )
  # invalid parameter for geom
  expect_equal(
    uses_geom_param(p3, geom = "boxplot", params = list(bins = 20, outlier.alpha = 0.01)),
    c(bins = FALSE, outlier.alpha = TRUE)
  )
  expect_equal(
    uses_geom_param(p4, geom = "histogram", params = list(bins = 20, outlier.alpha = 0.01)),
    c(bins = TRUE, outlier.alpha = FALSE)
  )
  # multiple invalid parameters
  expect_equal(
    uses_geom_param(p3, geom = "boxplot", params = list(varwidthh = TRUE, outlierr.alpha = 0.01)),
    c(varwidthh = FALSE, outlierr.alpha = FALSE)
  )
})

test_that("default_params()", {
  expect_equal(
    default_params(p, "point"),
    list(
      shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA,
      stroke = 0.5, na.rm = FALSE
    )
  )

  expect_equal(default_params(p, "point", "color"), list(color = "black"))

  expect_equal(
    default_params(p, "smooth", c("se", "level")),
    list(se = TRUE, level = 0.95)
  )

  expect_equal(
    uses_geom_params(
      p, "smooth", se = default_params(), level = default_params()
    ),
    c(se = FALSE, level = TRUE)
  )
})
