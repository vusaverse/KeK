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
  group_by(INS_Inschrijvingsjaar_EOI, INS_Opleidingsnaam_2002, DEM_Nationaliteit_EER_naam) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = DEM_Nationaliteit_EER_naam, values_from = n) %>%
  mutate(across(c(NL, EER, `NIET-EER`), ~ replace_na(., 0))) %>%
  mutate(total = NL + EER + `NIET-EER`)


### Validatie

## Sommige opleidingsnamen staan in hoofdletters in onze dataset
dfTrue <- dfTrue %>%
  mutate(Naam_hoofdletters = str_to_upper(Naam))

## Selecteer alleen de opleidingen die in dfTrue staan
dfPremaster_EER_test <- dfPremaster_EER %>%
  filter(map_lgl(str_to_upper(INS_Opleidingsnaam_2002), ~ any(str_detect(dfTrue$Naam_hoofdletters, fixed(.x))))) %>%
  filter(INS_Inschrijvingsjaar_EOI == 2021) %>%
  mutate(INS_Opleidingsnaam_2002_nieuw = str_to_upper(INS_Opleidingsnaam_2002))

## Format de opleidingsnamen voor de join
dfTrue <- dfTrue %>%
  mutate(Naam_nieuw = map_chr(Naam_hoofdletters, ~ {
    match <- dfPremaster_EER_test$INS_Opleidingsnaam_2002_nieuw[str_detect(.x, fixed(dfPremaster_EER_test$INS_Opleidingsnaam_2002_nieuw))]
    case_when(
      length(match) > 0 ~ match, # If match is found, use the matching value from df2
      TRUE ~ .x # Otherwise, keep the original value from col1
    )
  })) %>%
  select(Naam_nieuw, everything())

## Maak nieuwe validatie df aan
dfPremaster_EER_Validatie <- dfPremaster_EER_test %>%
  mutate(INS_Opleidingsnaam_2002 = INS_Opleidingsnaam_2002_nieuw) %>%
  select(-INS_Opleidingsnaam_2002_nieuw) %>%
  left_join(
    dfTrue %>%
      select(
        Naam_nieuw,
        `NL Totaal Inschrijvingen`,
        `EER (Excl. NL) Totaal Inschrijvingen`,
        `Niet-EER Totaal Inschrijvingen`
      ),
    by = c("INS_Opleidingsnaam_2002" = "Naam_nieuw")
  ) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  # Use mutate instead of summarize to add the Totaal_kek column
  mutate(Totaal_kek = sum(`NL Totaal Inschrijvingen`,
    `EER (Excl. NL) Totaal Inschrijvingen`,
    `Niet-EER Totaal Inschrijvingen`,
    na.rm = TRUE
  )) %>%
  ungroup()

## bereken verschillen
dfPremaster_EER_Validatie <- dfPremaster_EER_Validatie %>%
  mutate(
    verschil_nl = select(., 7) - select(., 3),
    verschil_eer = select(., 8) - select(., 4),
    verschil_nieteer = select(., 9) - select(., 5),
    verschil_totaal = select(., 10) - select(., 6)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file(dfPremaster_EER, "KEK_Opleiding_studenten", destination = "20. Test/", save_csv = TRUE)

clear_script_objects()
