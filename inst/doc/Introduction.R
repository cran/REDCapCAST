## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(REDCapCAST)

## -----------------------------------------------------------------------------
redcapcast_data |> gt::gt()

## -----------------------------------------------------------------------------
redcapcast_meta |> gt::gt()

## -----------------------------------------------------------------------------
list <-
  REDCap_split(records = redcapcast_data,
                           metadata = redcapcast_meta,
                           forms = "repeating")
str(list)

## -----------------------------------------------------------------------------
list <-
  REDCap_split(records = redcapcast_data,
                           metadata = redcapcast_meta,
                           forms = "all")
str(list)

## ----eval=FALSE---------------------------------------------------------------
#  # read_redcap_tables(uri = "YOUR URI", token = "YOUR TOKEN")

## -----------------------------------------------------------------------------
# redcap_wider(ds)

