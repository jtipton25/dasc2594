% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_transform_coordinates.R
\name{scale_data}
\alias{scale_data}
\title{Scale Data Frame}
\usage{
scale_data(data, limit = 5)
}
\arguments{
\item{data}{Dataframe containing at least one numeric column.}

\item{limit}{Maximum magnitude value in range of coordinates after scaling, defaults to 5.}
}
\value{
A dataframe that has been scaled so that the largest value in a numeric column is 5.
}
\description{
Helper function for adding coordinates onto animation of transformation.
}
\examples{
library(tibble)
scale_data(tibble(x = 1:10, y = -1:-10))
}
