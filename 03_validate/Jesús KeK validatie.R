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
# Gevraagd

# Faculteit
# Collegejaar
# Bachelor Inschrijvingen
# Master Inschrijvingen
# Premaster Inschrijvingen
# Postmaster Inschrijvingen

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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. VALIDEREN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## -----------------------------------------------------------------------------
## VRAAG 1: HOE ZIEN DE CIJFERS ERUIT OP OPLEIDINGSNIVEAU?

## Pas filters toe op ROD data zodat deze overeenkomt met BI
dfInschrijvingen_per_fase_PIM <- Inschrijvingen_1cho %>%
  filter(
    INS_Instelling == "VU",
    # S = Premaster
    # Q = Postmaster
    INS_Opleidingsfase_actueel %in% c("B", "M")
  ) %>%
  filter(INS_Soort_inschrijving_1CHO == 1) %>%
  ## Voor post-master lijkt EOI niet veelzeggend
  group_by(
    INS_Inschrijvingsjaar,
    INS_Faculteit,
    INS_Opleidingsnaam_2002,
    DEM_Nationaliteit_EER_naam
  ) %>%
  summarise(grand_total_BI = n_distinct(INS_Studentnummer)) %>% # tel unieke studenten
  pivot_wider(
    names_from = DEM_Nationaliteit_EER_naam,
    values_from = grand_total_BI
  ) %>%
  mutate(grand_totaal_BI = rowSums(across(tail(names(.), 3)), na.rm = TRUE))



## Maak controledataset BI
dfValidate <- dfInschrijvingen_per_fase_PIM %>%
  filter(INS_Faculteit == "SBE") %>%
  filter(INS_Inschrijvingsjaar == 2021)

## Sommige opleidingsnamen staan in hoofdletters in onze dataset
dfTrue <- dfTrue %>%
  mutate(Naam_hoofdletters = str_to_upper(Naam))

## Selecteer alleen de opleidingen die in dfTrue staan
dfValidate <- dfValidate %>%
  filter(map_lgl(str_to_upper(INS_Opleidingsnaam_2002), ~ any(str_detect(dfTrue$Naam_hoofdletters, fixed(.x))))) %>%
  mutate(INS_Opleidingsnaam_2002 = str_to_upper(INS_Opleidingsnaam_2002))

## Format de opleidingsnamen voor de join
dfTrue <- dfTrue %>%
  mutate(Naam_nieuw = map_chr(Naam_hoofdletters, ~ {
    match <- dfValidate$INS_Opleidingsnaam_2002[str_detect(.x, fixed(dfValidate$INS_Opleidingsnaam_2002))]
    case_when(
      length(match) > 0 ~ match, # If match is found, use the matching value from df2
      TRUE ~ .x # Otherwise, keep the original value from col1
    )
  })) %>%
  select(Naam_nieuw, everything())

dfTrue_new <- dfTrue %>%
  select(
    Naam_nieuw,
    `NL Totaal Inschrijvingen`,
    `EER (Excl. NL) Totaal Inschrijvingen`,
    `Niet-EER Totaal Inschrijvingen`
  ) %>%
  mutate(grand_totaal_kek = rowSums(across(tail(names(.), 3)), na.rm = TRUE))

## Join
dfOpleiding_Validatie <- dfValidate %>%
  left_join(dfTrue_new, by = c("INS_Opleidingsnaam_2002" = "Naam_nieuw")) %>%
  ungroup() %>% # Ungroup the data if it is grouped
  mutate(EER_verschil = EER - `EER (Excl. NL) Totaal Inschrijvingen`) %>%
  mutate(NIET_EER_verschil = `NIET-EER` - `Niet-EER Totaal Inschrijvingen`) %>%
  mutate(NL_verschil = NL - `NL Totaal Inschrijvingen`) %>%
  mutate(Totaal_verschil = grand_totaal_BI - grand_totaal_kek) %>%
  mutate(fase = substr(INS_Opleidingsnaam_2002, 1, 1))

## Faculteitsniveau
dfFaculteit_Validatie <- dfOpleiding_Validatie %>%
  select(-INS_Inschrijvingsjaar) %>%
  group_by(fase) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))


## Pas filters toe op ROD data zodat deze overeenkomt met BI
dfTest <- Inschrijvingen_1cho %>%
  filter(
    INS_Instelling == "VU",
    # S = Premaster
    # Q = Postmaster
    INS_Opleidingsfase_actueel %in% c("B", "M"),
    INS_Inschrijvingsjaar == 2021,
    INS_Faculteit == "SBE",
    str_detect(INS_Opleidingsnaam_2002, "M Entre", ),
    INS_Inclusief_UvA == "Nee"
  )


dfBI <- read_csv2("C:/Users/kzh421/kzh421/Documents/11. Detailrapport inschrijvinge.csv")

dfBI_NL <- dfBI %>%
  filter(
    EER == "NL",
    `Opl fase` == "B"
  )

## Pas filters toe op ROD data zodat deze overeenkomt met BI
dfOns <- Inschrijvingen_1cho %>%
  filter(
    INS_Instelling == "VU",
    # S = Premaster
    # Q = Postmaster
    INS_Opleidingsfase_actueel %in% c("B"),
    INS_Inschrijvingsjaar == 2021,
    INS_Faculteit == "SBE",
    INS_Inclusief_UvA == "Nee",
    DEM_Nationaliteit_EER_naam == "NL",
    INS_Soort_inschrijving_1CHO == 1
  )


dfBI_diff <- dfBI_NL %>%
  filter(!Studentnr. %in% dfOns$INS_Studentnummer) %>%
  pull(Studentnr.)

Test <- Inschrijvingen_1cho %>%
  filter(
    INS_Instelling == "VU",
    # S = Premaster
    # Q = Postmaster
    INS_Opleidingsfase_actueel %in% c("B"),
    INS_Inschrijvingsjaar == 2021,
    INS_Faculteit == "SBE",
    INS_Inclusief_UvA == "Nee",
    INS_Studentnummer %in% dfBI_diff
  )

tabyl(Test$INS_Hoogste_vooropleiding_soort)
