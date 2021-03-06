% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layers.R
\name{get_geom_layer}
\alias{get_geom_layer}
\title{Isolate a geom layer from a plot}
\usage{
get_geom_layer(p, geom = NULL, i = NULL)
}
\arguments{
\item{p}{A ggplot object}

\item{geom}{A character string found in the suffix of a ggplot2 geom function,
e.g. \code{"point"}.}

\item{i}{A numerical index, e.g. \code{1}.}
}
\value{
An object with class \code{layer_to_check} to be manipulated further
with ggcheck functions.
}
\description{
\code{get_geom_layer} returns a geom layer from a plot along with the global data sets
and aesthetic mappings that the layer may inherit from.
}
\details{
Users can specify a layer in one of 3 ways:

\enumerate{
\item By order of appearance with \code{i}. The first layer to appear in the
plot (the one drawn first, on the bottom) corresponds to \code{i = 1}.
\item By type of geom with \code{geom}. \code{get_geom_layer} will return the
first layer that uses the geom.
\item By a combination of \code{geom} and
\code{i}. \code{get_geom_layer} will return the ith layer that uses the geom.
}
}
\examples{
require(ggplot2)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(color = "red") +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE)

get_geom_layer(p, i = 1)
get_geom_layer(p, geom = "smooth")
get_geom_layer(p, geom = "point", i = 2)
}
