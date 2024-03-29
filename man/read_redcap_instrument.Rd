% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_redcap_instrument.R
\name{read_redcap_instrument}
\alias{read_redcap_instrument}
\title{Convenience function to download complete instrument, using token storage
in keyring.}
\usage{
read_redcap_instrument(
  key,
  uri,
  instrument,
  raw_or_label = "raw",
  id_name = "record_id",
  records = NULL
)
}
\arguments{
\item{key}{key name in standard keyring for token retrieval.}

\item{uri}{REDCap database API uri}

\item{instrument}{instrument name}

\item{raw_or_label}{raw or label passed to `REDCapR::redcap_read()`}

\item{id_name}{id variable name. Default is "record_id".}

\item{records}{specify the records to download. Index numbers.
Numeric vector.}
}
\value{
data.frame
}
\description{
Convenience function to download complete instrument, using token storage
in keyring.
}
