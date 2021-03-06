% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_data}
\alias{get_data}
\title{Get the data set used by a plot or layer}
\usage{
get_data(p, local_only = FALSE)
}
\arguments{
\item{p}{A ggplot object or a layer extracted from a ggplot object with
\code{\link{get_geom_layer}}.}

\item{local_only}{\code{TRUE} or \code{FALSE}. Should \code{get_data} onbly
return data defined locally in the layer?}
}
\value{
A data frame. If no data set is found, \code{get_data} returns
\code{NULL}
}
\description{
\code{get_data} returns the data set used by a ggplot object or a single
layer extracted from the object with \code{\link{get_geom_layer}}.
}
\details{
When passed a ggplot object (i.e. a plot), \code{get_data} will return only
the data that has been set globally with \code{\link[ggplot2]{ggplot}}.

When passed a single layer from a plot, the behavior of \code{get_data} will
depend on the \code{local_only} argument passed to \code{...}. If
\code{local_only = TRUE}, \code{get_data} will return only the data set, if
any, that was defined locally in the function that created the layer. If
\code{local_only = FALSE}, \code{get_data} will return the data used by the
layer, whether or not that data was defined globally in
\code{\link[ggplot2]{ggplot}} or locally.
}
\examples{
require(ggplot2)
d2 <- head(mpg)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(data = d2, color = "red") +
  geom_point()
get_data(p)
get_data(get_geom_layer(p, i = 1))
}
\seealso{
Other functions for checking data: 
\code{\link{ith_data_is}()},
\code{\link{ith_data}()},
\code{\link{uses_data}()}
}
\concept{functions for checking data}
