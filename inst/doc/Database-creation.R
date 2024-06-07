## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(REDCapCAST)

## ----eval=TRUE----------------------------------------------------------------
d1 <- mtcars |>
  dplyr::mutate(record_id = seq_len(dplyr::n())) |>
  ds2dd() 

d1 |>
  gt::gt()

## ----eval=TRUE----------------------------------------------------------------
d2 <- REDCapCAST::redcapcast_data |> 
  dplyr::mutate(record_id = seq_len(dplyr::n()),
                region=factor(region)) |>
  dplyr::select(record_id, dplyr::everything()) |>
  (\(.x){
    .x[!grepl("_complete$",names(.x))]
  })() |> 
  (\(.x){
    .x[!grepl("^redcap",names(.x))]
  })() |>  
  ds2dd_detailed() |> 
  purrr::pluck("meta") 

d2 |> 
  gt::gt()

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

