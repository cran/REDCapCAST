% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_factor.R
\name{get_attr}
\alias{get_attr}
\title{Extract attribute. Returns NA if none}
\usage{
get_attr(data, attr = NULL)
}
\arguments{
\item{data}{vector}

\item{attr}{attribute name}
}
\value{
character vector
}
\description{
Extract attribute. Returns NA if none
}
\examples{
attr(mtcars$mpg, "label") <- "testing"
do.call(c, sapply(mtcars, get_attr))
\dontrun{
mtcars |>
  numchar2fct(numeric.threshold = 6) |>
  ds2dd_detailed()
}
}
