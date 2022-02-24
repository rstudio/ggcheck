require(ggplot2)

p_valid <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point()

p_invalid <- geom_point()

p_basic <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))

p_minimal <- ggplot()

test_that("is_ggplot", {
  expect_true(is_ggplot(p_valid))
  expect_true(is_ggplot(p_basic))
  expect_true(is_ggplot(p_minimal))

  expect_false(is_ggplot(p_invalid))
})

test_that("stop_if_not_ggplot", {
  expect_invisible(expect_null(stop_if_not_ggplot(p_valid)))
  expect_invisible(expect_null(stop_if_not_ggplot(p_basic)))
  expect_invisible(expect_null(stop_if_not_ggplot(p_minimal)))

  expect_error(stop_if_not_ggplot(p_invalid))
})

test_that("fail_if_not_ggplot", {
  expect_invisible(expect_null(fail_if_not_ggplot(p_valid)))
  expect_invisible(expect_null(fail_if_not_ggplot(p_basic)))
  expect_invisible(expect_null(fail_if_not_ggplot(p_minimal)))

  expect_s3_class(fail_if_not_ggplot(p_invalid), "gradethis_graded")
  expect_false(fail_if_not_ggplot(p_invalid)$correct)
})

test_that("fail_if_not_ggplot() within mock_this_exercise()", {
  # Should fail
  expect_snapshot(
    gradethis::grade_this({
      fail_if_not_ggplot()
    })(gradethis::mock_this_exercise(.user_code = "2"))
  )

  # Should fail
  expect_snapshot(
    gradethis::grade_this({
      fail_if_not_ggplot()
    })(gradethis::mock_this_exercise(.user_code = "ggplot2::geom_point()"))
  )

  # Should not fail
  expect_null(
    gradethis::grade_this({
      fail_if_not_ggplot()
    })(gradethis::mock_this_exercise(.user_code = "ggplot2::ggplot()"))
  )
})
