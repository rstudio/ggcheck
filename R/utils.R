#' Grade a ggplot object with \code{gradethis::grade_result} conditions.
#'
#' @param p_chunk Source text for the ggplot code
#' @param p A ggplot object
#' @param ... 1 or more \code{gradethis_condition}
#' @export
grade_plot <- function(p_chunk, p, ...) {
  # env args
  run_result <- list(
    chunk = p_chunk,
    returned = p,
    env_pre = new.env(parent = globalenv()),
    env_post = new.env(parent = globalenv())
  )

  # prep the grading env
  grade_env <- list2env(
    list(
      .envir_prep = new.env(),
      .last_value = run_result$returned,
      .envir_result = run_result$env_post
    )
  )

  # grade
  gradethis::grade_result(
    default_correct = TRUE,
    glue_correct = "{ .message }",
    glue_incorrect = "{ .message }",
    ...
  )(grade_env)
}
