% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{clean_redcap_name}
\alias{clean_redcap_name}
\title{clean_redcap_name}
\usage{
clean_redcap_name(x)
}
\arguments{
\item{x}{vector or data frame for cleaning}
}
\value{
vector or data frame, same format as input
}
\description{
Stepwise removal on non-alphanumeric characters, trailing white space,
substitutes spaces for underscores and converts to lower case.
Trying to make up for different naming conventions.
}
