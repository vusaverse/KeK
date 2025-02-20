## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Get HRM export (export Manipuleren HRM personeel.R)
dfHRM_personeel <- read_file_proj("KEK_HRM_compleet")

## Get HRM data
dfFac_Personeel <- get_kek_data("faculteitpersoneels")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EDIT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO: Moet al aanwezig zijn -> toevoegen aan manipuleren?
## Add verslagjaar
dfHRM_personeel <- dfHRM_personeel %>%
  mutate(unl_verslagjaar = Jaar)

## Pivot
dfHRM_personeel_pivot <- dfHRM_personeel %>%
  pivot_wider(
    names_from = Veldnaam,
    values_from = FTE_gemiddelde
  )

## Read documentation
KeK_HRM_naming <- read_documentation(
  "Documentatie_KeK_FAC_personeel_API.csv"
)

## FTE = gemiddeld aantal FTE -> gecheckt met documentatie
# names(dfHRM_personeel_pivot)
# [1] "Jaar"                          "Faculteit"                     "Docent"
# [4] "Docent - OW"                   "Docent - OZ"                   "Hoogleraar"
# [7] "Hoogleraar - OW"               "Hoogleraar - OZ"               "Onderzoeker"
# [10] "Onderzoeker - OW"              "Onderzoeker - OZ"              "Onderzoeker - VAL"
# [13] "Promovendus"                   "Promovendus - OW"              "Promovendus - OZ"
# [16] "Student-assistent"             "Student-assistent - OW"        "Student-assistent - OZ"
# [19] "Universitair docent"           "Universitair docent - OW"      "Universitair docent - OZ"
# [22] "Universitair hoofddocent"      "Universitair hoofddocent - OW" "Universitair hoofddocent - OZ"
# [25] "OBP - Schaal 11 en hoger"      "OBP - Schaal 5 t/m 10"         "Decaan"
# [28] "Decaan - OW"                   "Decaan - OZ"                   "Hoogleraar - VAL"
# [31] "Promovendus - VAL"             "Student-assistent - VAL"       "Universitair docent - VAL"

## TODO; Welke KeK variabele naam hoort hier bij?
## TODO: Documentatiebestand bekijken/aan Judith vragen
## <functie> - VAL
## Decaan

## Pas kolomnamen aan zodat deze overeenkomen met KeK data entry app
dfHRM_data_entry_app <- dfHRM_personeel_pivot %>%
  wrapper_translate_colnames_documentation(KeK_HRM_naming)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LOOKUP VARIABLES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lookup variables in table_summary tab Fac (unl_FaculteitPersoneel):
# unl_faculteitnaam = Faculteit naam Targets:unl_faculteit
# unl_jaar = jaar | Targets:unl_collegejaar
# unl_rang = rang | Targets:unl_rangenlijst
# unl_verslagjaar = verslagjaar | Targets:unl_kalenderjaar

### Faculteit ####
## unl_faculteitnaam = Faculteit naam Targets:unl_faculteit
dffaculteit <- get_kek_data("faculteits")

# Clean faculty names in both datasets
dffaculteit <- dffaculteit %>%
  mutate(clean_name = clean_faculty_name(unl_afkortingfaculteit))

## TODO: check variabele unl_faculteitpersoneelid
dfHRM_data_entry_app <- dfHRM_data_entry_app %>%
  mutate(clean_faculteit = clean_faculty_name(unl_faculteitpersoneelid))

# Add the new column to dfHRM_data_entry_app
dfHRM_data_entry_app <- dfHRM_data_entry_app %>%
  mutate(`unl_FaculteitNaam@odata.bind` = sapply(clean_faculteit, function(faculty) {
    match <- dffaculteit %>%
      filter(clean_name == faculty | unl_afkortingfaculteit == faculty) %>%
      pull(unl_faculteitid)

    if (length(match) > 0) {
      paste0("unl_faculteits(", match, ")")
    } else {
      NA_character_
    }
  })) %>%
  select(-clean_faculteit, -unl_faculteitpersoneelid)

### Jaar ####
## unl_jaar = jaar | Targets:unl_collegejaar
## Gebruik collegejaar id uit KeK
dfcolleges <- get_kek_data("collegejaars")


dfHRM_data_entry_app <- dfHRM_data_entry_app %>%
  mutate(
    academic_year = convert_to_academic_year(unl_jaar),
    `unl_Jaar@odata.bind` = sapply(academic_year, function(year) {
      match <- dfcolleges %>%
        filter(unl_name == year) %>%
        pull(unl_collegejaarid)

      if (length(match) > 0) {
        paste0("unl_collegejaars(", match, ")")
      } else {
        NA_character_
      }
    })
  )

### Rang ####
## unl_rang = rang | Targets:unl_rangenlijst
dfRang <- get_kek_data("rangenlijsts")

dfHRM_data_entry_app <- dfHRM_data_entry_app %>%
  mutate(
    `unl_Verslagjaar@odata.bind` = sapply(unl_verslagjaar, function(year) {
      match <- dfkalenderjaar %>%
        filter(unl_name == year) %>%
        pull(unl_kalenderjaarid)

      if (length(match) > 0) {
        paste0("unl_kalenderjaars(", match, ")")
      } else {
        NA_character_
      }
    })
  )


### Verslagjaar ####
## unl_verslagjaar = verslagjaar | Targets:unl_kalenderjaar
## Verslagjaar is hetzelfde als boekjaar/kalenderjaar
dfkalenderjaar <- get_kek_data("kalenderjaars")

dfHRM_data_entry_app <- dfHRM_data_entry_app %>%
  mutate(
    `unl_Verslagjaar@odata.bind` = sapply(unl_verslagjaar, function(year) {
      match <- dfkalenderjaar %>%
        filter(unl_name == year) %>%
        pull(unl_kalenderjaarid)

      if (length(match) > 0) {
        paste0("unl_kalenderjaars(", match, ")")
      } else {
        NA_character_
      }
    })
  )

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Send POST ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Sample data for testing
dfHRM_data_entry_app2 <- dfHRM_data_entry_app %>%
  select(
    -unl_jaar,
    -unl_verslagjaar,
    -academic_year
  )

bbb <- send_data_to_kek(
  dfHRM_data_entry_app2,
  "faculteitpersoneels"
)

clear_script_objects()
