## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# REDCapCAST::shiny_cast()

## -----------------------------------------------------------------------------
library(REDCapCAST)
ds <- REDCap_split(
    records = redcapcast_data,
    metadata = redcapcast_meta,
    forms = "all"
  ) |> 
  sanitize_split() |>
  redcap_wider() 
str(ds)

## -----------------------------------------------------------------------------
ds|> 
  ds2dd_detailed(metadata = names(REDCapCAST::redcapcast_meta))|>  
  purrr::pluck("data") |> 
  str()

## -----------------------------------------------------------------------------
ds|> 
  ds2dd_detailed(metadata = names(REDCapCAST::redcapcast_meta))|>  
  purrr::pluck("meta") |> 
  head(10)

## -----------------------------------------------------------------------------
ds_parsed <- redcapcast_data |> 
  dplyr::mutate(dplyr::across(dplyr::everything(),as.character)) |> 
  parse_data()
str(ds_parsed)

## -----------------------------------------------------------------------------
redcapcast_data |> 
  dplyr::mutate(dplyr::across(dplyr::everything(),as.character)) |> 
  parse_data(ignore.vars = c("record_id","cpr")) |> 
  str()

## -----------------------------------------------------------------------------
mtcars |> str()
mtcars |>
  numchar2fct(numeric.threshold = 6) |>
  str()

## -----------------------------------------------------------------------------
ds_parsed|>
  numchar2fct(numeric.threshold = 2) |>
  str()

