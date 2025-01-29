## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(REDCapCAST)

## ----eval=FALSE---------------------------------------------------------------
# shiny_cast()

## ----eval=FALSE---------------------------------------------------------------
# easy_redcap(
#   uri = "YOUR URI",
#   project.name = "MY_PROJECT",
#   widen.data = TRUE,
#   fields = c("record_id", "OTHER FIELDS")
# )

## -----------------------------------------------------------------------------
redcapcast_data |> gt::gt()

## -----------------------------------------------------------------------------
redcapcast_meta |> gt::gt()

## -----------------------------------------------------------------------------
labelled_data <-
  apply_field_label(
    data = redcapcast_data,
    meta = redcapcast_meta
  ) |>
  apply_factor_labels(meta = redcapcast_meta)

## -----------------------------------------------------------------------------
list <-
  REDCap_split(
    records = labelled_data,
    metadata = redcapcast_meta,
    forms = "all"
  ) |>
  # Next steps cleans up and removes generic columns
  sanitize_split()
str(list)

## -----------------------------------------------------------------------------
wide_data <- redcap_wider(list,
  event.glue = "{.value}____{redcap_event_name}",
  inst.glue = "{.value}____{redcap_repeat_instance}"
)
wide_data |> str()

## -----------------------------------------------------------------------------
wide_data_suffixes <- wide_data |> suffix2label()

## -----------------------------------------------------------------------------
wide_data_suffixes |> 
  as_factor()|>
  dplyr::select(sex, hypertension, diabetes,mrs_score____follow2) |>
  gtsummary::tbl_summary(type = gtsummary::all_dichotomous() ~ "categorical")

