## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 00. Voorbereidingen.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: De working directory wordt bepaald door de locatie van het project
## (sa-scripts)
## De specifieke functies en libraries voor dit project worden ingeladen
##
## Afhankelijkheden: Verbinding met G:/
##
## Datasets: geen
##
## Opmerkingen:
## 1) Draai voor het inlezen van de juiste packages eerst het bestand Installeer Packages.R
## Eerst werd installeer packages.R aangeroepen, maar deze bestaat niet meer.
## source("99. Functies & Libraries/Install sa packages.R")
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Determine the branch
Sys.setenv("BRANCH" = system("git branch --show-current", intern = TRUE))

renv::restore(prompt = FALSE)

## Get all numbered setup scripts from 00_setup directory
setup_scripts <- list.files(path = "00_project/", pattern = "^\\d+.*\\.R$", full.names = TRUE)

## Source each setup script in order
purrr::walk(setup_scripts, source)
rm(setup_scripts)

## Get all functions in R/ directory
functions <- list.files(path = "R/", full.names = TRUE)
purrr::walk(functions, source)
rm(functions)

## Slackr setup sa-bot
slackr_setup(
  channel = "#rstudio-meldingen",
  username = Sys.getenv("SLACK_BOT"),
  icon_emoji = "",
  incoming_webhook_url = Sys.getenv("SLACK_WEBHOOK"),
  token = Sys.getenv("SLACK_TOKEN"),
  config_file = "~/.slackr",
  echo = FALSE
)

## Set DEFAULT_KEEP_LIST environment variable
object_names <- ls(envir = .GlobalEnv)

default_keep_list <- paste(object_names, collapse = " ")

Sys.setenv(DEFAULT_KEEP_LIST = default_keep_list)

clear_global_proj()
