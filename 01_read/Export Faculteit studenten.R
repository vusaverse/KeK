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
dfTrue <- readxl::read_xlsx(paste0(Sys.getenv("RAW_DATA_DIR"), "/KeK/Overzicht Faculteit Studenten.xlsx"))
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

##' TODO per DEM
# dfInschrijvingen_per_fase_PIM <- Inschrijvingen_1cho %>%
#   filter(INS_Instelling == "VU",
#          # S = Premaster
#          # Q = Postmaster
#          INS_Maand_inschrijving_van == 9,
#          INS_Opleidingsvorm == "Voltijd",
#          INS_Indicatie_actief_op_peildatum == TRUE,
#          INS_Opleidingsfase_actueel %in% c("B", "M", "Q", "S")) %>%
#   ## Voor post-master lijkt EOI niet veelzeggend
#   # group_by(INS_Inschrijvingsjaar, INS_Faculteit , INS_Opleidingsfase_actueel, DEM_Nationaliteit_EER_naam) %>%
#   group_by(INS_Inschrijvingsjaar, INS_Faculteit, INS_Opleidingsfase_actueel, DEM_Nationaliteit_EER_naam ) %>%
#   summarise(n = n(), .groups = "drop") #%>%
#   # pivot_wider(names_from = INS_Opleidingsfase_actueel, values_from = n) %>%
#   # mutate(across(c(B, M, Q, S), ~replace_na(., 0))) %>%
#   # mutate(total = B + M + Q + S)

dfInschrijvingen_per_fase_PIM <- Inschrijvingen_1cho %>%
  filter(
    INS_Instelling == "VU",
    INS_Maand_inschrijving_van == 9,
    INS_Opleidingsvorm == "Voltijd",
    INS_Indicatie_actief_op_peildatum == TRUE,
    INS_Opleidingsfase_actueel %in% c("B", "M", "Q", "S")
  ) %>%
  group_by(INS_Inschrijvingsjaar, INS_Faculteit, INS_Opleidingsfase_actueel, DEM_Nationaliteit_EER_naam) %>%
  summarise(n = n(), .groups = "drop") %>%
  unite("combined_column", INS_Opleidingsfase_actueel, DEM_Nationaliteit_EER_naam, sep = "-") %>%
  pivot_wider(names_from = combined_column, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  mutate(total = sum(c_across(-c(INS_Inschrijvingsjaar, INS_Faculteit)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    S = `S-EER` + `S-NIET-EER` + `S-NL`,
    Q = `Q-EER` + `Q-NIET-EER` + `Q-NL`
  )
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfInschrijvingen_per_fase_PIM, "KEK_Faculteit_studenten")

clear_script_objects()
