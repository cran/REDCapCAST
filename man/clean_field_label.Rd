% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_redcap_tables.R
\name{clean_field_label}
\alias{clean_field_label}
\title{Very simple function to remove rich text formatting from field label
and save the first paragraph ('<p>...</p>').}
\usage{
clean_field_label(data)
}
\arguments{
\item{data}{field label}
}
\value{
character vector
}
\description{
Very simple function to remove rich text formatting from field label
and save the first paragraph ('<p>...</p>').
}
\examples{
clean_field_label("<div class=\"rich-text-field-label\"><p>Fazekas score</p></div>")
}
