% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_wider.R
\name{suffix2label}
\alias{suffix2label}
\title{Transfer variable name suffix to label in widened data}
\usage{
suffix2label(
  data,
  suffix.sep = "____",
  attr = "label",
  glue.str = "{label} ({paste(suffixes,collapse=', ')})"
)
}
\arguments{
\item{data}{data.frame}

\item{suffix.sep}{string to split suffix(es). Passed to \link[base]{strsplit}}

\item{attr}{label attribute. Default is "label"}

\item{glue.str}{glue string for new label. Available variables are "label"
and "suffixes"}
}
\value{
data.frame
}
\description{
Transfer variable name suffix to label in widened data
}
