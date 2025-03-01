% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/REDCap_split.r
\name{REDCap_split}
\alias{REDCap_split}
\title{Split REDCap repeating instruments table into multiple tables}
\usage{
REDCap_split(
  records,
  metadata,
  primary_table_name = "",
  forms = c("repeating", "all")
)
}
\arguments{
\item{records}{Exported project records. May be a \code{data.frame},
\code{response}, or \code{character} vector containing JSON from an API
call.}

\item{metadata}{Project metadata (the data dictionary). May be a
\code{data.frame}, \code{response}, or \code{character} vector containing
JSON from an API call.}

\item{primary_table_name}{Name given to the list element for the primary
output table. Ignored if \code{forms = 'all'}.}

\item{forms}{Indicate whether to create separate tables for repeating
instruments only or for all forms.}
}
\value{
A list of \code{"data.frame"}s. The number of tables will differ
  depending on the \code{forms} option selected.
  \itemize{
    \item \code{'repeating'}: one base table and one or more
    tables for each repeating instrument.
    \item \code{'all'}: a data.frame for each instrument, regardless of
    whether it is a repeating instrument or not.
  }
}
\description{
This will take output from a REDCap export and split it into a base table
and child tables for each repeating instrument. Metadata
is used to determine which fields should be included in each resultant table.
}
\examples{
\dontrun{
# Using an API call -------------------------------------------------------

library(RCurl)

# Get the records
records <- postForm(
  uri = api_url, # Supply your site-specific URI
  token = api_token, # Supply your own API token
  content = "record",
  format = "json",
  returnFormat = "json"
)

# Get the metadata
metadata <- postForm(
  uri = api_url, # Supply your site-specific URI
  token = api_token, # Supply your own API token
  content = "metadata",
  format = "json"
)

# Convert exported JSON strings into a list of data.frames
REDCapCAST::REDCap_split(records, metadata)

# Using a raw data export -------------------------------------------------

# Get the records
records <- read.csv("/path/to/data/ExampleProject_DATA_2018-06-03_1700.csv")

# Get the metadata
metadata <- read.csv(
  "/path/to/data/ExampleProject_DataDictionary_2018-06-03.csv"
)

# Split the tables
REDCapCAST::REDCap_split(records, metadata)

# In conjunction with the R export script ---------------------------------

# You must set the working directory first since the REDCap data export
# script contains relative file references.
old <- getwd()
setwd("/path/to/data/")

# Run the data export script supplied by REDCap.
# This will create a data.frame of your records called 'data'
source("ExampleProject_R_2018-06-03_1700.r")

# Get the metadatan
metadata <- read.csv("ExampleProject_DataDictionary_2018-06-03.csv")

# Split the tables
REDCapCAST::REDCap_split(data, metadata)
setwd(old)
}
}
\author{
Paul W. Egeler
}
