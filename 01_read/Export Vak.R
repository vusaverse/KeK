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

dfuas_vakken <- load_rds_from_azure(
  config = .config,
  container_type = "bronze",
  file_name = "UAS_Vakken_enriched.rds"
)

dfuas_groepen <- load_rds_from_azure(
  config = .config,
  container_type = "bronze",
  file_name = "UAS_Groepen_van_vakken_enriched.rds"
) %>% 
  select(
    uas_groep_jaar,
    code_mg,
    verplichte_keuze
  ) %>% 
  distinct()

##' Needed to determine INS_Opleidingscode from RES_Studieprogramma_ID
# dfOpleidingskoppel <- readrds_csv(output = "2. Geprepareerde data/INS_Opleidingkoppel.rds") %>% 
#   select(RES_Studieprogramma_ID, 
#          INS_Opleidingscode_actueel
#   ) %>% 
#   distinct()

dfDates <- readrds_csv(output = "2. Geprepareerde data/ACA_Dates.rds")

## validation purposes
# dfTrue <- readxl::read_xlsx(paste0(Sys.getenv("RAW_DATA_DIR"), "/KeK/Overzicht Vak 6_8_2024 15-30-41.xlsx")) %>%
#   select(-Eigenaar)

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
    count_zondersucces = sum(RES_Boekingsstatus_omschrijving == "Zondersuccesbeëindigd"),
    count_with_result = sum(!is.na(RES_Beoordeling))
  ) %>%
  distinct()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Determine period start. Previously done in sa-scripts, not in ea-repo yet.
## TODO remove if logic is added to ETL in EA
dfuas_vakken <- dfuas_vakken  %>%
  ## Kies bij vakken met met meerdere periode(blokken) voor de laatste periode
  ## Dit zijn meestal scripties die ook in eerste semester kunnen worden
  ## gevolgd, maar in curriculum in tweede semester thuis horen.
  mutate(
    uas_aangeboden_periodes = case_when(
      str_detect(uas_aangeboden_periodes, ",") ~ str_split_i(uas_aangeboden_periodes, ", ", i = -1),
      .default = uas_aangeboden_periodes
    )
  ) %>%
  ## Transformeer nu alle waardes naar periodes of NA
  mutate(
    uas_aangeboden_periodes = case_when(
      uas_aangeboden_periodes == "Semester 1" ~ "Periode 1+2+3",
      uas_aangeboden_periodes == "Semester 2" ~ "Periode 4+5+6",
      uas_aangeboden_periodes == "Zomerperiode" ~ "Periode 7",
      uas_aangeboden_periodes == "025" ~ NA_character_,
      ## Ook hiervoor geldt dat het vaak scripties zijn
      uas_aangeboden_periodes == "Ac. Jaar (september)" &
        uas_studiepunten >= 54 ~ "Periode 1+2+3+4+5+6",
      uas_aangeboden_periodes == "Ac. Jaar (september)" &
        uas_studiepunten >= 42 ~ "Periode 2+3+4+5+6",
      uas_aangeboden_periodes == "Ac. Jaar (september)" &
        uas_studiepunten >= 36 ~ "Periode 3+4+5+6",
      uas_aangeboden_periodes == "Ac. Jaar (september)" &
        uas_studiepunten >= 24 ~ "Periode 4+5+6",
      uas_aangeboden_periodes == "Ac. Jaar (september)" &
        uas_studiepunten >= 9 ~ "Periode 5+6",
      ## Dit zijn vaak mentor / tutoraat vakken
      uas_aangeboden_periodes == "Ac. Jaar (september)" ~ "Periode 1+2+3+4+5+6",
      .default = uas_aangeboden_periodes
    ),
    ## Zet periodes van vak in een lijst
    uas_aangeboden_periodes_lijst = str_extract_all(uas_aangeboden_periodes, "[0-9]"),
    ## Bepaal eerste periode vak
    uas_vak_periode_start = parse_number(uas_aangeboden_periodes)
  ) %>%
  rowwise() %>%
  mutate(
    ## Bepaal duratie van vak
    uas_vak_duur = sum(str_count(uas_aangeboden_periodes, "[+]"),
                       1,
                       na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  distinct()


dfVAK <- dfuas_vakken %>%
  filter(uas_vak_jaar >= 2021) %>%
  left_join(dfDates_join, by = c(
    "uas_vak_jaar" = "INS_Inschrijvingsjaar",
    "uas_vak_periode_start" = "ACA_Periode"
  )) %>%
  left_join(dfResultaten_koppeling, by = c(
    "uas_vak_jaar" = "Academisch_jaar",
    "uas_code" = "RES_Module_code"
  )) %>%
  rename(startmoment_vak = ACA_Einddatum_prev) %>%
  ## Merge insrtuctie taal kolommen
  mutate(uas_onderwijstaal = case_when(
          uas_onderwijstaal == "Engels (EN)" ~ "Engels",
          uas_onderwijstaal == "Nederlands (NL)" ~ "Nederlands",
          uas_onderwijstaal == "Tweetalig (Z1)" ~ "Nederlands/Engels",
          .default = uas_onderwijstaal
      )
  ) %>%
  select(
    uas_code, # KeK Opleidingscode ------------- NVT
    uas_vak_jaar, # Jaargang
    uas_vaknaam_nl, # Vak Naam
    uas_studiepunten, # EC's; verder hebben wij ook minimum en maximum EC
    uas_groep_verplichte_keuze, # Verplicht/Keuze; niet helemaal zeker
    uas_vak_periode_start, startmoment_vak, # Startdatum
    uas_vak_duur, # Aantal Collegeweken
    total_students, # Aantal Deelnemers
    uas_onderwijstaal, # Taal; zie ook OPLAS repo om dit te mergen?
    uas_extern_id,
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
    
    uas_groep_groepstype,
    uas_groep_college_jaar,
    uas_vak_opleidingsnaam_eigenaar,
    uas_vak_opleidingscode_eigenaar,
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
    count_with_result,
    count_afgesloten # Aantal geslaagden vak
  ) %>%
  #filter(!is.na(uas_vaknaam_nl)) %>%
  distinct() %>%
  ## bij dubbelingen missende waarden wellicht nog wat trucjes nodig, zie E_IBA1_ACC voor >= 2022
  distinct(uas_code, uas_vak_jaar, uas_vak_periode_start, startmoment_vak, .keep_all = TRUE) %>%
  mutate(uas_vak_periode_einde = uas_vak_periode_start + uas_vak_Duur - 1) %>%
  left_join(dfDates_join, by = c(
    "uas_vak_jaar" = "INS_Inschrijvingsjaar",
    "uas_vak_periode_einde" = "ACA_Periode"
  )) %>%
  ## if uas_vak_periode_start == 3 then add 14 days to startmoment_vak
  mutate(startmoment_vak = case_when(
    uas_vak_periode_start == 3 ~ startmoment_vak + 14,
    TRUE ~ startmoment_vak
  )) %>%
  mutate(weeks_diff = round(as.numeric(difftime(ACA_Einddatum, startmoment_vak, units = "weeks")))) #%>% 
  #left_join(dfOpleidingskoppel, by = c("RES_Studieprogramma_ID"))


dfVAK_validatie <- dfVAK %>%
  filter(
    uas_vak_jaar == 2021,
    uas_code %in% dfTrue$Vakcode
  ) %>%
  left_join(dfTrue, by = c("uas_code" = "Vakcode")) %>%
  mutate(
    ECs_overeen = uas_studiepunten == `EC's`,
    Startdatum_date = as_date(Startdatum) + 1,
    Startdatum_overeen = Startdatum_date == startmoment_vak,
    verschil_startdatum = as.numeric(startmoment_vak - Startdatum_date),
    Aantal_deelnemers_overeen = total_students == `Aantal Deelnemers`,
    Verschil_aantal_deelnemers = total_students - `Aantal Deelnemers`,
    Aantal_weken_overeen = weeks_diff == `Aantal Collegeweken`,
    Verschil_aantal_weken = weeks_diff - `Aantal Collegeweken`,
    KEK_verplicht_boolean = `Verplicht/Keuze` == "Verplicht",
    verplicht_overeen = uas_Groep_Verplichte_keuze == KEK_verplicht_boolean,
    Aantal_geslaagden_overeen = count_afgesloten == `Aantal Geslaagden Eindtoets`,
    verschil_geslaagden = count_afgesloten - `Aantal Geslaagden Eindtoets`
  )


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Add scriptie information

dfSc <- dfVAKAS %>% filter(uas_vak_scriptie) %>% 
  select(uas_vak_jaar, uas_code, uas_vak_scriptie, uas_Opleiding_Fase_minimaal_generiek, uas_vak_periode_start) %>% 
  filter(!is.na(uas_Opleiding_Fase_minimaal_generiek)) %>%
  ## mutate case when for new variable unl_eindtoetsnaam, if uas_Opleiding_Fase_minimaal_generiek == Bachelor then 941790000, IF 
  ## uas_Opleiding_Fase_minimaal_generiek == Master then 941790001
  mutate(unl_eindtoetsnaam = case_when(
    uas_Opleiding_Fase_minimaal_generiek == "Bachelor" ~ 941790000,
    uas_Opleiding_Fase_minimaal_generiek == "Master" ~ 941790001,
    TRUE ~ NA_real_
  )) %>% 
  select(uas_vak_jaar, uas_code, unl_eindtoetsnaam, uas_vak_periode_start) %>% 
  distinct()


dfVAK <- dfVAK %>% 
  left_join(dfSc, by = c("uas_vak_jaar", "uas_code", "uas_vak_periode_start")) %>% 
  distinct(uas_code, uas_vak_jaar, uas_vak_periode_start, startmoment_vak, .keep_all = TRUE)

## Add stage information
dfStage <- dfVAKAS %>% filter(uas_vak_stage) %>% 
  select(uas_vak_jaar, uas_code, uas_vak_stage, uas_Opleiding_Fase_minimaal_generiek, uas_vak_periode_start) %>%
  filter(!is.na(uas_Opleiding_Fase_minimaal_generiek)) %>%
  mutate(unl_eindtoetsnaam = 941790007) %>% 
  select(uas_vak_jaar, uas_code, unl_eindtoetsnaam, uas_vak_periode_start) %>% 
  distinct()

##' *TODO* why reallocated values?
dfVAK <- dfVAK %>%
  left_join(
    dfStage %>% 
      select(uas_vak_jaar, uas_code, uas_vak_periode_start, unl_eindtoetsnaam),
    by = c("uas_vak_jaar", "uas_code", "uas_vak_periode_start")
  ) %>%
  mutate(
    unl_eindtoetsnaam = coalesce(unl_eindtoetsnaam.y, unl_eindtoetsnaam.x)
  ) %>%
  select(-unl_eindtoetsnaam.x, -unl_eindtoetsnaam.y) %>% 
  distinct(uas_code, uas_vak_jaar, uas_vak_periode_start, startmoment_vak, .keep_all = TRUE)


##' attempt to fill missing unl_eindtoetsnaam with filled dfToetsvormen variable toetsvorm_code 
##' 
dfToetsvormen <- readrds_csv(output = "20. Test/dftoetsvorm.csv") %>% 
  mutate(toetsvorm_code = as.integer(toetsvorm_code))

dfVAK %>% filter(is.na(unl_eindtoetsnaam)) %>% dim
# [1] 18691    23

dfVAK <- dfVAK %>% 
  left_join(
    dfToetsvormen %>%
      select(
        uas_vak_jaar,
        Code,
        toetsvorm_code
      ) %>%
      distinct(),
    by = c("uas_vak_jaar", "uas_code" = "Code")
  ) %>%
  mutate(
    unl_eindtoetsnaam = coalesce(toetsvorm_code, unl_eindtoetsnaam)
  ) %>%
  select(-toetsvorm_code) %>%
  distinct(uas_code, uas_vak_jaar, uas_vak_periode_start, startmoment_vak, .keep_all = TRUE)

dfVAK %>% filter(is.na(unl_eindtoetsnaam)) %>% dim
# [1] 9931   23


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfVAK, "KEK_Vakken")

##' *TODO*
# write_file(dfVAK_validatie, "KEK_Vakken_validatie", destination = "20. Test/", save_csv = TRUE)

clear_script_objects()
