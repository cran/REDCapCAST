% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds2dd_detailed.R
\name{numchar2fct}
\alias{numchar2fct}
\title{Applying var2fct across data set}
\usage{
numchar2fct(data, numeric.threshold = 6, character.throshold = 6)
}
\arguments{
\item{data}{dataset. data.frame or tibble}

\item{numeric.threshold}{threshold for var2fct for numeric columns. Default
is 6.}

\item{character.throshold}{threshold for var2fct for character columns.
Default is 6.}
}
\value{
data.frame or tibble
}
\description{
Individual thresholds for character and numeric columns
}
\examples{
mtcars |> str()
\dontrun{
mtcars |>
  numchar2fct(numeric.threshold = 6) |>
  str()
}
}
