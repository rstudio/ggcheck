% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{default_label}
\alias{default_label}
\title{What is the default label for a plot aesthetic?}
\usage{
default_label(p, aes = NULL)
}
\arguments{
\item{p}{A \link[ggplot2:ggplot]{ggplot} object}

\item{aes}{If \code{aes} is a \link{character} vector, returns only the default labels
(based on the plot \code{p}) that correspond to the included aesthetics.
Defaults to \code{\link{NULL}}, which returns the default values of all labels.}
}
\value{
A named \link{list} in which each element is a \link{character} string
or \code{\link{NULL}}.
Strings are returned for aesthetics with a default value.
\code{\link{NULL}} is returned for aesthetics that do not exist in the plot,
or non-aesthetic labels that do not have a default value, like \code{title}.
}
\description{
What is the default label for a plot aesthetic?
}
\examples{
require(ggplot2)

p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class, shape = drv)) +
  geom_smooth() +
  labs(title = "My plot", x = "Weight", y = "MPG", color = NULL)

# Returns the label the ggplot would create by default for an aesthetic
default_label(p, "x")
default_label(p, c("x", "y"))
default_label(p)

# If an aesthetic does not exist, returns NULL
default_label(p, "size")

# Non-aesthetic labels have no default value, so they also return NULL
default_label(p, "title")
default_label(p, "comment")

# The colo(u)r aesthetic can be matched with or without a u
default_label(p, "color")
default_label(p, "colour")
}
\seealso{
Other functions for checking labels: 
\code{\link{get_labels}()},
\code{\link{uses_labels}()}
}
\concept{functions for checking labels}