## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Install sa package.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Hier worden de locale packages ingeladen
##
## Afhankelijkheden: G schijf
##
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INSTALLEREN VUSAVERSE
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bepaal packages en run package voor package
## TODO: Via git gaan dependencies goed, maar install iedere keert kost tijd
##' *INFO* Zie https://vustudentanalytics.atlassian.net/wiki/spaces/SWPRJ/pages/3776905719/21.02.2023
## NB: Volgorde moet rekening houden met dependencies, begin met vvcommander
sa_packages <- c(
  "vvauditor",
  # "vvconverter",
  "vvmover",
  # "vvsculptor",
  # "vvfiller",
  "vusa"
)

purrr::walk(sa_packages, ~ library(., character.only = TRUE, warn.conflicts = FALSE))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clear_script_objects()
