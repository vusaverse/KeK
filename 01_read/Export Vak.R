## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dfVAKAS <- readrds_csv(output = "3. Analyseset/Vakken_Analyseset_lang_na_stap_2.rds")

##' Needed to determine INS_Opleidingscode from RES_Studieprogramma_ID
dfOpleidingskoppel <- readrds_csv(output = "2. Geprepareerde data/INS_Opleidingkoppel.rds") %>% 
  select(RES_Studieprogramma_ID, 
         INS_Opleidingscode_actueel
  ) %>% 
  distinct()

dfDates <- readrds_csv(output = "2. Geprepareerde data/ACA_Dates.rds")

## validation purposes
dfTrue <- readxl::read_xlsx(paste0(Sys.getenv("RAW_DATA_DIR"), "/KeK/Overzicht Vak 6_8_2024 15-30-41.xlsx")) %>%
  select(-Eigenaar)

dfDates_join <- dfDates %>%
  filter(!is.na(ACA_Periode)) %>%
  select(INS_Inschrijvingsjaar, ACA_Periode, ACA_Einddatum_prev, ACA_Einddatum) # %>%
# mutate(weeks_diff = round(as.numeric(difftime(ACA_Einddatum, ACA_Einddatum_prev, units = "weeks"))))

dfResultaten <- readrds_csv(output = "1. Ingelezen data/RES_Resultaten.rds")

dfResultaten_koppeling <- dfResultaten %>%
  distinct(
    RES_Module_ID,
    INS_Studentnummer,
    Academisch_jaar,
    # RES_Boeking_datum,
    .keep_all = TRUE
  ) %>%
  ## filter enkel jaren van Evalytics
  filter(
    Academisch_jaar >= 2021,
    ## onderstaande zogt voor 1/3 niet overeen
    RES_Studieprogramma_ID != 99999999,
    RES_Boekingsstatus_omschrijving %in% c(
      "Afgesloten",
      "Zondersuccesbeëindigd"
    )
  ) %>%
  ## groupby and count
  group_by(
    Academisch_jaar,
    RES_Module_ID,
    RES_Module_code
  ) %>%
  summarise(
    total_students = n(),
    count_afgesloten = sum(RES_Boekingsstatus_omschrijving == "Afgesloten"),
    count_zondersucces = sum(RES_Boekingsstatus_omschrijving == "Zondersuccesbeëindigd")
  ) %>%
  distinct()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfVAK <- dfVAKAS %>%
  filter(UAS_Vak_Jaar >= 2021) %>%
  left_join(dfDates_join, by = c(
    "UAS_Vak_Jaar" = "INS_Inschrijvingsjaar",
    "UAS_Vak_Periode_start" = "ACA_Periode"
  )) %>%
  left_join(dfResultaten_koppeling, by = c(
    "UAS_Vak_Jaar" = "Academisch_jaar",
    "UAS_Vak_Code" = "RES_Module_code"
  )) %>%
  rename(Startmoment_vak = ACA_Einddatum_prev) %>%
  ## Merge insrtuctie taal kolommen
  mutate(OPL_Instructietaal = case_when(
    is.na(OPL_Instructietaal) ~
      case_when(
        UAS_Opleiding_Onderwijstaal == "Engels (EN)" ~ "Engels",
        UAS_Opleiding_Onderwijstaal == "Nederlands (NL)" ~ "Nederlands",
        UAS_Opleiding_Onderwijstaal == "Tweetalig (Z1)" ~ "Nederlands/Engels",
        TRUE ~ UAS_Opleiding_Onderwijstaal
      ),
    TRUE ~ OPL_Instructietaal
  )) %>%
  select(
    UAS_Vak_Code, # KeK Opleidingscode ------------- NVT
    UAS_Vak_Jaar, # Jaargang
    UAS_Vak_Naam_NL, # Vak Naam
    UAS_Vak_Studiepunten, # EC's; verder hebben wij ook minimum en maximum EC
    UAS_Groep_Verplichte_keuze, # Verplicht/Keuze; niet helemaal zeker
    UAS_Vak_Periode_start, Startmoment_vak, # Startdatum
    UAS_Vak_Duur, # Aantal Collegeweken
    total_students, # Aantal Deelnemers
    OPL_Instructietaal, UAS_Opleiding_Onderwijstaal, # Taal; zie ook OPLAS repo om dit te mergen?
    RES_Studieprogramma_ID,
    # Hoorcollege Aantal personen WP
    # Hoorcollege Aantal personen OBP
    # Werkcollege Aantal personen WP
    # Werkcollege Aantal personen OBP
    # Practicum Aantal personen WP
    # Practicum Aantal personen OBP
    # Werkvorm 1 Aantal personen WP
    # Werkvorm 1 Aantal personen OBP
    # Werkvorm 2 Aantal personen WP
    # Werkvorm 2 Aantal personen OBP

    UAS_Groep_Groepstype,
    UAS_Vak_Opleidingsnaam_eigenaar,
    UAS_Vak_Opleidingscode_eigenaar,
    ## TERMTIME?
    # Werkvorm 3 Naam
    # Werkvorm 3 Groepen
    # Werkvorm 3 Uren Per Week Per Groep
    # Werkvorm 3 Aantal Weken
    # Werkvorm 3 Aantal personen WP
    # Werkvorm 3 Aantal personen OBP
    # Toetsvorm 1; ------------------------- UAS?
    # Aantal deelnemers toets 1: ---------------------------- VUDATA ODW012?
    # Toetsvorm 2
    # Aantal deelnemers toets 2
    # Toetsvorm 3
    # Aantal deelnemers toets 3

    ## VUDATA ODW012?
    count_afgesloten # Aantal geslaagden vak
  ) %>%
  filter(!is.na(UAS_Vak_Naam_NL)) %>%
  distinct() %>%
  ## bij dubbelingen missende waarden wellicht nog wat trucjes nodig, zie E_IBA1_ACC voor >= 2022
  distinct(UAS_Vak_Code, UAS_Vak_Jaar, UAS_Vak_Periode_start, Startmoment_vak, .keep_all = TRUE) %>%
  mutate(UAS_Vak_Periode_einde = UAS_Vak_Periode_start + UAS_Vak_Duur - 1) %>%
  left_join(dfDates_join, by = c(
    "UAS_Vak_Jaar" = "INS_Inschrijvingsjaar",
    "UAS_Vak_Periode_einde" = "ACA_Periode"
  )) %>%
  ## if UAS_Vak_Periode_start == 3 then add 14 days to startmoment_vak
  mutate(Startmoment_vak = case_when(
    UAS_Vak_Periode_start == 3 ~ Startmoment_vak + 14,
    TRUE ~ Startmoment_vak
  )) %>%
  mutate(weeks_diff = round(as.numeric(difftime(ACA_Einddatum, Startmoment_vak, units = "weeks")))) %>% 
  left_join(dfOpleidingskoppel, by = c("RES_Studieprogramma_ID"))


dfVAK_validatie <- dfVAK %>%
  filter(
    UAS_Vak_Jaar == 2021,
    UAS_Vak_Code %in% dfTrue$Vakcode
  ) %>%
  left_join(dfTrue, by = c("UAS_Vak_Code" = "Vakcode")) %>%
  mutate(
    ECs_overeen = UAS_Vak_Studiepunten == `EC's`,
    Startdatum_date = as_date(Startdatum) + 1,
    Startdatum_overeen = Startdatum_date == Startmoment_vak,
    verschil_startdatum = as.numeric(Startmoment_vak - Startdatum_date),
    Aantal_deelnemers_overeen = total_students == `Aantal Deelnemers`,
    Verschil_aantal_deelnemers = total_students - `Aantal Deelnemers`,
    Aantal_weken_overeen = weeks_diff == `Aantal Collegeweken`,
    Verschil_aantal_weken = weeks_diff - `Aantal Collegeweken`,
    KEK_verplicht_boolean = `Verplicht/Keuze` == "Verplicht",
    verplicht_overeen = UAS_Groep_Verplichte_keuze == KEK_verplicht_boolean,
    Aantal_geslaagden_overeen = count_afgesloten == `Aantal Geslaagden Eindtoets`,
    verschil_geslaagden = count_afgesloten - `Aantal Geslaagden Eindtoets`
  )


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Add scriptie information

dfSc <- dfVAKAS %>% filter(UAS_Vak_scriptie) %>% 
  select(UAS_Vak_Jaar, UAS_Vak_Code, UAS_Vak_scriptie, UAS_Opleiding_Fase_minimaal_generiek, UAS_Vak_Periode_start) %>% 
  filter(!is.na(UAS_Opleiding_Fase_minimaal_generiek)) %>%
  ## mutate case when for new variable unl_eindtoetsnaam, if UAS_Opleiding_Fase_minimaal_generiek == Bachelor then 941790000, IF 
  ## UAS_Opleiding_Fase_minimaal_generiek == Master then 941790001
  mutate(unl_eindtoetsnaam = case_when(
    UAS_Opleiding_Fase_minimaal_generiek == "Bachelor" ~ 941790000,
    UAS_Opleiding_Fase_minimaal_generiek == "Master" ~ 941790001,
    TRUE ~ NA_real_
  )) %>% 
  select(UAS_Vak_Jaar, UAS_Vak_Code, unl_eindtoetsnaam, UAS_Vak_Periode_start) %>% 
  distinct()


dfVAK <- dfVAK %>% 
  left_join(dfSc, by = c("UAS_Vak_Jaar", "UAS_Vak_Code", "UAS_Vak_Periode_start")) %>% 
  distinct(UAS_Vak_Code, UAS_Vak_Jaar, UAS_Vak_Periode_start, Startmoment_vak, .keep_all = TRUE)

## Add stage information
dfStage <- dfVAKAS %>% filter(UAS_Vak_stage) %>% 
  select(UAS_Vak_Jaar, UAS_Vak_Code, UAS_Vak_stage, UAS_Opleiding_Fase_minimaal_generiek, UAS_Vak_Periode_start) %>%
  filter(!is.na(UAS_Opleiding_Fase_minimaal_generiek)) %>%
  mutate(unl_eindtoetsnaam = 941790007) %>% 
  select(UAS_Vak_Jaar, UAS_Vak_Code, unl_eindtoetsnaam, UAS_Vak_Periode_start) %>% 
  distinct()

##' *TODO* why reallocated values?
dfVAK <- dfVAK %>%
  left_join(
    dfStage %>% 
      select(UAS_Vak_Jaar, UAS_Vak_Code, UAS_Vak_Periode_start, unl_eindtoetsnaam),
    by = c("UAS_Vak_Jaar", "UAS_Vak_Code", "UAS_Vak_Periode_start")
  ) %>%
  mutate(
    unl_eindtoetsnaam = coalesce(unl_eindtoetsnaam.y, unl_eindtoetsnaam.x)
  ) %>%
  select(-unl_eindtoetsnaam.x, -unl_eindtoetsnaam.y) %>% 
  distinct(UAS_Vak_Code, UAS_Vak_Jaar, UAS_Vak_Periode_start, Startmoment_vak, .keep_all = TRUE)



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfVAK, "KEK_Vakken")

##' *TODO*
# write_file(dfVAK_validatie, "KEK_Vakken_validatie", destination = "20. Test/", save_csv = TRUE)

clear_script_objects()
