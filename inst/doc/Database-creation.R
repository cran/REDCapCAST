## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(REDCapCAST)

## ----eval=FALSE---------------------------------------------------------------
#  mtcars |>
#    dplyr::mutate(record_id = seq_len(dplyr::n())) |>
#    ds2dd() |>
#    str()

## ----eval=FALSE---------------------------------------------------------------
#  dd_ls <- mtcars |>
#    dplyr::mutate(record_id = seq_len(dplyr::n())) |>
#    dplyr::select(record_id, dplyr::everything()) |>
#    ds2dd_detailed()
#  dd_ls |>
#    str()

## ----eval=FALSE---------------------------------------------------------------
#  write.csv(dd_ls$meta, "datadictionary.csv")

## ----eval=FALSE---------------------------------------------------------------
#  REDCapR::redcap_metadata_write(
#    dd_ls$meta,
#    redcap_uri = keyring::key_get("DB_URI"),
#    token = keyring::key_get("DB_TOKEN")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  REDCapR::redcap_write(
#    dd_ls$data,
#    redcap_uri = keyring::key_get("DB_URI"),
#    token = keyring::key_get("DB_TOKEN")
#  )

