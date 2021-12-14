#' List the labels used by a plot
#'
#' `get_labels()` returns a named [list] of [labels][ggplot2::labs],
#' written as [character] strings, indicating which labels are used by a plot.
#'
#' Note that `get_labels()` will return [`NULL`] if a label is explicitly set to
#' [`NULL`] ***or*** if a requested aesthetic is not present in the plot.
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class)) +
#'   geom_smooth() +
#'   labs(x = "Weight", y = "MPG", color = NULL)
#'
#' get_labels(p)
#' get_labels(p, c("x", "y"))
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' get_labels(p, "color")
#' get_labels(p, "colour")
#' @param p A [ggplot][ggplot2::ggplot] object
#' @param aes If `aes` is a [character] vector, returns only the labels
#'   corresponding to the included aesthetics.
#'   Defaults to [`NULL`], which returns all labels.
#'
#' @return A named list of character strings.
#'
#' @family functions for checking labels
#' @export
get_labels <- function(p, aes = NULL) {
  if (is.null(aes)) {return(p$labels)}

  label_names <- aes
  label_names[aes == "color"] <- "colour"

  result <- p$labels[label_names]

  # Restore names from inputs so spelling of "colo(u)r" matches
  names(result) <- aes

  result
}

#' Does a plot use one or more labels?
#'
#' `uses_labels()` tests whether a plot uses one or more [labels][ggplot2::labs].
#'
#' Note that `uses_labels()` will match [`NULL`] if a label is explicitly set to
#' [`NULL`] ***or*** if a requested aesthetic is not present in the plot.
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class, shape = drv)) +
#'   geom_smooth() +
#'   labs(title = "My plot", x = "Weight", y = "MPG", color = NULL)
#'
#' # Unnamed arguments check if a label is set for the given aesthetic
#' uses_labels(p, "title", "subtitle", "x", "y")
#'
#' # The check will return TRUE for labels set to NULL
#' uses_labels(p, "color")
#'
#' # The check will return TRUE for aesthetics with default labels
#' uses_labels(p, "shape")
#'
#' # Named arguments check if the label matches an expected value
#' uses_labels(p, x = "Weight")
#' uses_labels(p, x = "Weight", y = "MPG", color = NULL)
#'
#' # You can check for default labels with default_label()
#' uses_labels(p, shape = default_label(), x = default_label())
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' uses_labels(p, color = NULL)
#' uses_labels(p, colour = NULL)
#'
#' # Inputs can be passed from a list, with or without the !!! operator
#' label_list <- list(x = "Weight", y = "MPG", color = NULL)
#' uses_labels(p, label_list)
#' uses_labels(p, !!!label_list)
#' @param p A ggplot object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   [Character][character] strings.
#'   Unnamed arguments will check whether a label exists for that aesthetic.
#'   Named arguments will check whether the aesthetic with the same name
#'   has a label with a matching value.
#'   Each argument should have a matching [ggplot][ggplot2::ggplot]
#'   [aesthetic][ggplot2::aes] or [label][ggplot2::labs].
#'   Strings may be input as individual arguments or as list elements.
#'
#' @return A logical vector of the same length as the number of inputs to `...`.
#'
#' @family functions for checking labels
#' @export
uses_labels <- function(p, ...) {
  args <- rlang::flatten(rlang::dots_list(...))
  args <- rlang::dots_list(!!!args, .homonyms = "error")

  if (length(args) == 0) {
    stop(
      "You must pass an argument to `...` in `uses_labels()`.",
      call. = FALSE
    )
  }

  default_labels <- purrr::map_lgl(args, inherits, ".default_label")

  args[default_labels] <- purrr::map(
    names(args)[default_labels], ~ unlist(default_label(p, .))
  )

  if (!all(is_scalar_string_or_null(args))) {
    stop(
      "All inputs to `...` must be character vectors of length 1 or `NULL`.",
      call. = FALSE
    )
  }

  if (is.null(names(args))) {
    names(args) <- rep("",    length(args))
    named       <- rep(FALSE, length(args))
  } else {
    named <- names(args) != ""
  }

  result         <- logical(length(args))
  result[!named] <- check_labels_set(p, args[!named])
  result[named]  <- check_labels_match(p, args[named])

  # Ensure names of result vector match names in `...`:
  # - Names of inputs for named inputs
  # - Values of inputs for unnamed inputs
  names(result) <- coalesce_chr(names(args), args)

  result
}

#' What is the default label for a plot aesthetic?
#'
#' @examples
#' require(ggplot2)
#'
#' p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
#'   geom_point(mapping = aes(color = class, shape = drv)) +
#'   geom_smooth() +
#'   labs(title = "My plot", x = "Weight", y = "MPG", color = NULL)
#'
#' # Returns the label the ggplot would create by default for an aesthetic
#' default_label(p, "x")
#' default_label(p, c("x", "y"))
#' default_label(p)
#'
#' # If an aesthetic does not exist, returns NULL
#' default_label(p, "size")
#'
#' # Non-aesthetic labels have no default value, so they also return NULL
#' default_label(p, "title")
#' default_label(p, "comment")
#'
#' # The colo(u)r aesthetic can be matched with or without a u
#' default_label(p, "color")
#' default_label(p, "colour")
#' @param p A [ggplot][ggplot2::ggplot] object
#' @param aes If `aes` is a [character] vector, returns only the default labels
#'   (based on the plot `p`) that correspond to the included aesthetics.
#'   Defaults to [`NULL`], which returns the default values of all labels.
#'
#' @return A named [list] in which each element is a [character] string
#'   or [`NULL`].
#'   Strings are returned for aesthetics with a default value.
#'   [`NULL`] is returned for aesthetics that do not exist in the plot,
#'   or non-aesthetic labels that do not have a default value, like `title`.
#'
#' @family functions for checking labels
#' @export
default_label <- function(p, aes = NULL) {
  UseMethod("default_label")
}

#' @export
default_label.default <- function(p, aes = NULL) {
  if (!missing(p)) {
    rlang::abort("`p` must be a `ggplot` object.")
  }

  structure(list(), class = c(".default_label", "ggcheck_placeholder"))
}

#' @export
default_label.ggplot <- function(p, aes = NULL) {
  if (is.null(aes)) {
    aes <- names(p$labels)
  }

  if (!is.character(aes)) {
    rlang::abort("`aes` must be a character vector or NULL.")
  }

  names(aes) <- aes

  aes[aes == "color"] <- "colour"

  make_labels <- utils::getFromNamespace("make_labels", "ggplot2")

  purrr::map(
    aes,
    function(aes) {
      # If an aesthetic exists in multiple layers, ggplot gives it a default
      # label based on the lowest level of the plot in which it appears

      # First check if the aesthetic exists in the base plot,
      # and return that label if it does
      if (!is.null(p$mapping[[aes]])) {
        return(as.character(make_labels(p$mapping[aes])))
      }

      # Then check if the aesthetic exists in any layer,
      # and return the label for the lowest layer is it does
      for (layer in p$layers) {
        if (!is.null(layer$mapping[[aes]])) {
          return(as.character(make_labels(layer$mapping[aes])))
        }
      }

      # If the aesthetic doesn't exist in the base plot or any layer,
      # its default label is `NULL`
      # (this always applies to non-aesthetic labels, like `title`)
      NULL
    }
  )
}

check_labels_set <- function(p, labels) {
  if (!length(labels)) {
    return(logical(0))
  }

  labels <- as.character(labels)
  labels[labels == "color"] <- "colour"

  labels %in% names(p$labels)
}

check_labels_match <- function(p, label_values) {
  if (!length(label_values)) {
    return(logical(0))
  }

  plot_labels <- get_labels(p, names(label_values))

  purrr::map2_lgl(
    label_values, plot_labels,
    ~ isTRUE(all.equal(as.character(.x), as.character(.y)))
  )
}

is_scalar_string_or_null <- function(x) {
  vapply(
    x,
    function(x) rlang::is_scalar_character(x) || length(x) == 0,
    logical(1)
  )
}

coalesce_chr <- function(x, y) {
  x[x == ""] <- y[x == ""]
  x
}
