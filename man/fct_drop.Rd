% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_drop.R
\name{fct_drop}
\alias{fct_drop}
\alias{fct_drop.data.frame}
\title{Drop unused levels preserving label data}
\usage{
fct_drop.data.frame(x, ...)
}
\arguments{
\item{x}{Factor to drop unused levels}

\item{...}{Other arguments passed down to method.}
}
\description{
This extends [forcats::fct_drop()] to natively work across a data.frame and
replace [base::droplevels()].
}
