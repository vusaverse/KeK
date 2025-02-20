## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Installeer Packages.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden alle benodigde packages geinstalleerd als ze nog
## niet geinstalleerd zijn, vervolgens worden deze ingeladen.
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Basis_packages <- c(
  "devtools", ## Gebruikt om te installeren via Github.
  "httr", ## Gebruikt om met HTTP te werken.
  "janitor", ## Gebruikt om namen op te schonen van speciale tekens.
  "lubridate", ## Gebruikt om te werken met data en tijden.
  "purrr", ## Gebruikt om met functies and vectoren te werken.
  "readr", ## Gebruikt om data (csv, tsv, and fwf) in te lezen.
  "slackr", ## Gebruikt voor het sturen van berichten in Slack.
  "stringr", ## Gebruikt voor functies om met strings te werken.
  "tibble", ## Gebruikt voor bewerken en aanmaken van tibbles.
  "tidyr", ## Gebruikt om data op te schonen in de tidverse omgeving.
  "utils", ## Gebruikt voor utility functies
  "dplyr" ## Gebruikt voor de dplyr omgeving.
)

## Laad de packages in de library
suppressMessages(purrr::walk(Basis_packages, ~ library(.x, character.only = TRUE, warn.conflicts = FALSE)))


## Ruim op
rm(Basis_packages)
