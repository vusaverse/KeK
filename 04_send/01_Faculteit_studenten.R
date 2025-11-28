## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfFac_Studenten <- read_file_proj("KEK_Faculteit_studenten", dir = "1. Ingelezen data/")

dfKeK_FAC_studenten <- get_kek_data(endpoint = "faculteitstudentens")


dfFac_Studenten_ENHANCED <- dfFac_Studenten %>% 
  mutate(total_B = `B-EER` + `B-NIET-EER` + `B-NL`, 
         total_M = `M-EER` + `M-NIET-EER` + `M-NL`)

## Lees documentatie in
KeK_Faculteit_studenten_naming <- read_documentation(
  "Documentatie_KeK_FAC_studenten_API.csv"
)

##' *TODO*
##' No year in endpoint
##' No clean faculty in endpoint

## Pas kolomnamen aan zodat deze overeenkomen met KeK data entry app
dfFac_Studenten2 <- dfFac_Studenten_ENHANCED %>%
  wrapper_translate_colnames_documentation(KeK_Faculteit_studenten_naming)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
##' Fac Lookups
##' list of columns that are lookup in faculteit_studenten:
# unl_collegejaar
# unl_jaar
# unl_faculteit

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. collegejaar ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


dfcolleges <- get_kek_data("collegejaars")

dfFac_Studenten2 <- dfFac_Studenten2 %>%
  mutate(
    academic_year = convert_to_academic_year(unl_jaar),
    `unl_Collegejaar@odata.bind` = sapply(academic_year, function(year) {
      match <- dfcolleges %>%
        filter(unl_name == year) %>%
        pull(unl_collegejaarid)

      if (length(match) > 0) {
        paste0("unl_collegejaars(", match, ")")
      } else {
        NA_character_
      }
    })
  ) %>%
  select(-academic_year, -unl_jaar)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. faculteit ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dffaculteit <- get_kek_data("faculteits")





# Clean faculty names in both datasets
dffaculteit <- dffaculteit %>%
  mutate(clean_name = clean_faculty_name(unl_afkortingfaculteit))

dfFac_Studenten2 <- dfFac_Studenten2 %>%
  mutate(clean_faculteit = clean_faculty_name(unl_faculteit))

# Add the new column to dfTT_data_entry_app
dfFac_Studenten2 <- dfFac_Studenten2 %>%
  mutate(`unl_Faculteit@odata.bind` = sapply(clean_faculteit, function(faculty) {
    match <- dffaculteit %>%
      filter(clean_name == faculty | unl_afkortingfaculteit == faculty) %>%
      pull(unl_faculteitid)

    if (length(match) > 0) {
      paste0("unl_faculteits(", match, ")")
    } else {
      NA_character_
    }
  })) %>%
  select(-clean_faculteit, -unl_faculteit) %>% 
  filter(!is.na(`unl_Faculteit@odata.bind`)) %>% 
  filter(!is.na(`unl_Collegejaar@odata.bind`))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Send data to KeK ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbb <- send_data_to_kek(dfFac_Studenten2, "faculteitstudentens")

clear_script_objects()
