## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Laad systeemvariabelen.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Doel
##
## Opmerkingen:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Definieer functie
set_all_envs <- function(var.name, var.value) {
  args <- list(var.value)
  names(args) <- var.name
  do.call(Sys.setenv, args)
}

## Lees in systeemvariabelen excel bestand
##' *INFO*: Dit is momenteel enkel beschikbaar op "main", vandaar volledig bestandspad
to_set <- readxl::read_xlsx("G:/DSZ/SA2016/Datasets/Documentatie/Project/renviron.xlsx")

## zet variabelen in R system variables
pmap(list(to_set$variable, to_set$value), set_all_envs)

## Set SHAREPOINT_DIR
Sys.setenv(SHAREPOINt_DIR = paste0("C:/Users/", Sys.getenv("USERNAME"), "/Vrije Universiteit Amsterdam/"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clear_script_objects()
