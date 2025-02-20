## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) TODO: Gebruik ruw 1cHO inschrijvingen data
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## validation purposes
dfTrue <- readxl::read_xlsx(paste0(Sys.getenv("RAW_DATA_DIR"), "/KeK/Overzicht Opleiding Studenten.xlsx")) %>%
  distinct(Naam, .keep_all = TRUE)

# Gevraagd:
# NL Premaster
# EER (Excl. NL) Premaster
# Niet-EER Premaster

bestandspad <- get_recent_file(
  paste0(
    Sys.getenv("NETWORK_DIR"),
    "Datasets/MIVU/sftp_download/"
  ),
  "ODW205 - VUanalytics 1CHO Inschrijving",
  date_type = "filename_ymd"
)

## Lees het bestand Inschrijvingen in uit het zip-bestand
Inschrijvingen_1cho <- unzip_read_delim(
  bestandspad,
  delim = ",",
  na = c("", "NA", "#", "Niet toegewezen"),
  col_types = cols(
    .default = col_guess(),
    INS_Studentnummer = col_double(),
    INS_Vooropleiding_voor_HO_gem_cijfer = col_double(),
    DEM_Nationaliteit_3 = col_integer(),
    INS_Vooropleiding_binnen_HO_soort = col_character(),
    INS_Vooropleiding_voor_HO_postcode_student = col_integer(),
    INS_Vooropleiding_voor_HO_postcode_woonadres = col_integer(),
    INS_Vooropleiding_voor_HO_Soort = col_character(),
    INS_Hoogste_vooropleiding_soort_1CHO = col_character(),
    INS_Hoogste_vooropleiding_binnen_HO_code = col_integer()
  )
)

## Lees het namingbestand in
Inschrijvingen_1cho_naming <- read_documentation("Documentatie_Inschrijvingen_1CHO_VUdata.csv")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# assert_naming(Inschrijvingen_1cho, Inschrijvingen_1cho_naming, "Inschrijvingen_1cho")
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pas kolomnamen aan met behulp van de documentatie
Inschrijvingen_1cho <- wrapper_translate_colnames_documentation(
  Inschrijvingen_1cho,
  Inschrijvingen_1cho_naming
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  distinct() %>%
  filter(!is.na(INS_Studentnummer)) %>%
  mutate(
    INS_Faculteit = stringi::stri_trans_general(INS_Faculteit, "Latin-ASCII"),
    INS_Opleidingsnaam_2002 = stringi::stri_trans_general(INS_Opleidingsnaam_2002, "Latin-ASCII")
  )


dfPremaster_EER <- Inschrijvingen_1cho %>%
  filter( # INS_Opleidingsfase_actueel == "S", ## Selecteer niet alleen  studenten die in de premaster zitten
    INS_Instelling == "VU"
  ) %>%
  group_by(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, INS_Opleidingscode_actueel, DEM_Nationaliteit_EER_naam) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = DEM_Nationaliteit_EER_naam, values_from = n) %>%
  mutate(across(c(NL, EER, `NIET-EER`), ~ replace_na(., 0))) %>%
  mutate(total = NL + EER + `NIET-EER`)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfPremaster_EER, "KEK_Opleiding_studenten")

clear_script_objects()
