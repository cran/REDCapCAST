#' Data set for demonstration
#'
#' This is a small dataset from a REDCap database for demonstrational purposes.
#' Contains only synthetic data.
#'
#' @format A data frame with 22 variables:
#' \describe{
#'   \item{record_id}{ID, numeric}
#'   \item{redcap_event_name}{Event name, character}
#'   \item{redcap_repeat_instrument}{Repeat instrument, character}
#'   \item{redcap_repeat_instance}{Repeat instance, numeric}
#'   \item{cpr}{CPR number, character}
#'   \item{inclusion}{Inclusion date, Date}
#'   \item{inclusion_time}{Inclusion time, hms}
#'   \item{dob}{Date of birth, Date}
#'   \item{age}{Age decimal, numeric}
#'   \item{age_integer}{Age integer, numeric}
#'   \item{sex}{Legal sex, character}
#'   \item{cohabitation}{Cohabitation status, character}
#'   \item{hypertension}{Hypertension, character}
#'   \item{diabetes}{diabetes, character}
#'   \item{region}{region, character}
#'   \item{baseline_data_start_complete}{Completed, character}
#'   \item{mrs_assessed}{mRS Assessed, character}
#'   \item{mrs_date}{Assesment date, Date}
#'   \item{mrs_score}{Categorical score, numeric}
#'   \item{mrs_complete}{Complete, numeric}
#'   \item{event_datetime}{Event datetime, POSIXct}
#'   \item{event_age}{Age at time of event, numeric}
#'   \item{event_type}{Event type, character}
#'   \item{new_event_complete}{Completed, character}
#'
#' }
#' @usage data(redcapcast_data)
"redcapcast_data"
