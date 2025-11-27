## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Gebruik de OPLAS om KeK export aan te maken
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## validation purposes
dfTrue <- readxl::read_xlsx(paste0(Sys.getenv("RAW_DATA_DIR"), "/KeK/Overzicht Opleiding.xlsx"))

##' *INFO*:
# ECs Eigen Studenten Elders -------------- kan als 0 worden ingevuld
# ECs Verleend aan Externe Studenten -------------- kan als 0 worden ingevuld
# Vergoeding Per EC Betaald -------------- NVT
# Vergoeding Per EC Ontvangen -------------- NVT

dfOPLAS <- readrds_csv(output = "2. Geprepareerde data/OPLAS.rds") ## vervang door UAS vak?

## Nog prefix "VU - " gewenst?
vFaculteiten_VU_voluit <- dfOPLAS %>%
  pull(UAS_Opleiding_Faculteitsnaam) %>%
  unique()

dfOpleidingen_test <- dfOPLAS %>%
  filter(!is.na(INS_Faculteit), !is.na(INS_Opleidingsnaam_2002)) %>%
  select(
    INS_Faculteit, # Faculteit
    INS_Opleidingsnaam_2002, # Naam Opleiding
    INS_Inschrijvingsjaar, # Collegejaar
    INS_Soort_onderwijs, # Onderwijssoort
    INS_Opleidingscode_Z08,
    INS_Opleidingsvorm, # Onderwijsvorm ---------------->   ergens DEELTIJD?????
    OPL_Code_actueel, INS_Opleidingscode_Z08, # Opleidingscode ISAT

    OPL_Instructietaal, # Taal
    ## EXTRA
    Graadtoevoeging
  ) %>%
  distinct()


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
##' Voorbereiden wegschrijven naar API
##'

KeK_opleidingen_naming <- read_documentation(
  "Documentatie_KeK_Opleidingen_API.csv"
)

## Pas kolomnamen aan zodat deze overeenkomen met KeK data entry app
dfTT_data_entry_app <- dfOpleidingen_test %>%
  wrapper_translate_colnames_documentation(KeK_opleidingen_naming)



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. collegejaar ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


dfcolleges <- get_kek_data("collegejaars")


dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(
    academic_year = convert_to_academic_year(unl_collegejaar),
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
  select(-academic_year, -unl_collegejaar)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. faculteit ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dffaculteit <- get_kek_data("faculteits")

# Clean faculty names in both datasets
dffaculteit <- dffaculteit %>%
  mutate(clean_name = clean_faculty_name(unl_afkortingfaculteit))

dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(clean_faculteit = clean_faculty_name(unl_faculteit))

# Add the new column to dfTT_data_entry_app
dfTT_data_entry_app <- dfTT_data_entry_app %>%
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
  select(-clean_faculteit, -unl_faculteit)



##' Translate values to code
dfTT_data_entry_app2 <- dfTT_data_entry_app %>%
  mutate(
    unl_vorm = case_when(
      unl_vorm == "VOLTIJD" ~ 1,
      unl_vorm == "DEELTIJD" ~ 2,
      unl_vorm == "DUAAL" ~ 3,
      TRUE ~ NA_real_
    ),
    unl_graad = case_when(
      unl_graad == "B" ~ 1,
      unl_graad == "M" ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(unl_opleidingscodeisat = as.character(unl_opleidingscodeisat)) %>%
  group_by(unl_opleidingnaam, unl_opleidingscodeisat) %>%
  mutate(unl_graad = if_else(is.na(unl_graad), 
                             first(unl_graad[!is.na(unl_graad)]), 
                             unl_graad)) %>%
  ungroup() %>%
  select(
    -unl_taalvanopleiding,
    -OPL_Code_actueel
  ) %>% 
  distinct()


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Send POST ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


bbb <- send_data_to_kek(dfTT_data_entry_app2, "opleidings")

clear_script_objects()
