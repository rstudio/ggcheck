context("Data")
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

test_that("Identifies global data", {
  expect_equal(
    get_data(p),
    mpg
  )
})

test_that("Checks whether data is used globally", {
  expect_true(uses_data(p, data = mpg))
  expect_false(uses_data(p, data = mtcars))
})

test_that("Identifies local data", {
  expect_equal(
    p %>% get_layer(geom = "point") %>% get_data(local_only = TRUE),
    NULL
  )
  expect_equal(
    p %>% get_layer(geom = "point") %>% get_data(local_only = FALSE),
    mpg
  )
  expect_equal(
    p %>% get_layer(i = 1) %>% get_data(local_only = FALSE),
    mpg
  )
  expect_equal(
    p2 %>% get_layer(geom = "point") %>% get_data(),
    d2
  )
  expect_equal(
    p2 %>% get_layer(geom = "point") %>% get_data(local_only = FALSE),
    d2
  )
  expect_equal(
    p2 %>% get_layer(i = 1) %>% get_data(),
    d2
  )
  expect_equal(
    p2 %>% get_layer(i = 1) %>% get_data(local_only = FALSE),
    d2
  )
  expect_equal(
    p2 %>% get_layer(geom = "point", i = 2) %>% get_data(local_only = TRUE),
    NULL
  )
  expect_equal(
    p2 %>% get_layer(geom = "point", i = 2) %>% get_data(local_only = FALSE),
    mpg
  )
})

test_that("Checks whether data is used by layer", {
  expect_true(p2 %>% get_layer(i = 1) %>% uses_data(d2))
  expect_true(p2 %>% get_layer(i = 2) %>% uses_data(mpg))
  expect_false(p2 %>% get_layer(i = 2) %>% uses_data(mpg, local_only = TRUE))
  expect_false(p2 %>% get_layer(i = 1) %>% uses_data(mpg))
  expect_false(p2 %>% get_layer(i = 2) %>% uses_data(d2))
})

test_that("Identifies the data set used by ith layer", {
  expect_equal(
    p2 %>% ith_data(1),
    d2
  )
  expect_equal(
    p2 %>% ith_data(1, local_only = FALSE),
    d2
  )
  expect_equal(
    p2 %>% ith_data(2, local_only = FALSE),
    mpg
  )
  expect_equal(
    p2 %>% ith_data(2, local_only = TRUE),
    NULL
  )
})

test_that("Checks the data used by ith layer", {
  expect_true(p2 %>% ith_data_is(data = d2, i = 1))
  expect_true(p2 %>% ith_data_is(data = d2, i =1, local_only = FALSE))
  expect_true(p2 %>% ith_data_is(data = mpg, i = 2, local_only = FALSE))
  expect_true(p2 %>% ith_data_is(data = NULL, i = 2, local_only = TRUE))
  expect_false(p2 %>% ith_data_is(data = mpg, i = 1))
  expect_false(p2 %>% ith_data_is(data = mpg, i =1, local_only = FALSE))
  expect_false(p2 %>% ith_data_is(data = NULL, i = 2, local_only = FALSE))
  expect_false(p2 %>% ith_data_is(data = mpg, i = 2, local_only = TRUE))
})
