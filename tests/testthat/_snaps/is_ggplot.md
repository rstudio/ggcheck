# fail_if_not_ggplot() within mock_this_exercise()

    Code
      gradethis::grade_this({
        fail_if_not_ggplot()
      })(gradethis::mock_this_exercise(.user_code = "2"))
    Output
      <gradethis_graded: [Incorrect]
        I expected your code to create a ggplot, but it created an object of
        class "numeric"
      >

---

    Code
      gradethis::grade_this({
        fail_if_not_ggplot()
      })(gradethis::mock_this_exercise(.user_code = "ggplot2::geom_point()"))
    Output
      <gradethis_graded: [Incorrect]
        I expected your code to create a ggplot, but it created an object of
        class "LayerInstance"
      >

