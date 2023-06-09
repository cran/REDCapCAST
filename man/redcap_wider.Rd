% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_wider.R
\name{redcap_wider}
\alias{redcap_wider}
\title{Redcap Wider}
\usage{
redcap_wider(
  list,
  event.glue = "{.value}_{redcap_event_name}",
  inst.glue = "{.value}_{redcap_repeat_instance}"
)
}
\arguments{
\item{list}{A list of data frames.}

\item{event.glue}{A dplyr::glue string for repeated events naming}

\item{inst.glue}{A dplyr::glue string for repeated instruments naming}
}
\value{
The list of data frames in wide format.
}
\description{
Converts a list of REDCap data frames from long to wide format.
Handles longitudinal projects, but not yet repeated instruments.
}
\examples{
list <- list(data.frame(record_id = c(1,2,1,2),
redcap_event_name = c("baseline", "baseline", "followup", "followup"),
age = c(25,26,27,28)),
data.frame(record_id = c(1,2),
redcap_event_name = c("baseline", "baseline"),
gender = c("male", "female")))
redcap_wider(list)
}
