## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: education-analytics@vu.nl
##
##' *INFO*:
## Termtime activiteiten data bewerken voor import KeK Data Entry App
## TODO: Checken + verbeteren
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen termtime activiteiten data bestanden
## Is export van script: Tomer Export Termtime EY.R

##' *TODO*:
##' Het toevoegen van Jaar in het onderliggende script
##' Voor nu een temp fix
##' LET OP: unl_jaar is een LookUp field
dfTT_activiteiten <- readrds_csv(output = "20. Test/TT_Activiteit_data.rds")

dfTT_activiteiten <- dfTT_activiteiten %>%
  mutate(unl_jaar = "2021-2022")

dfTT_activiteiten_years <- readrds_csv(output = "20. Test/TT_Activiteit_data_all_years.rds")

dfTT_activiteiten <- dfTT_activiteiten %>% 
  bind_rows(dfTT_activiteiten_years)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Extract weken uit de Events subtabel
dfTT_activiteiten$week_count <- sapply(dfTT_activiteiten$events, unpack_weeks)

## Extract duur van werkvormen uit de events subtabel
dfTT_activiteiten$durationmin <- sapply(dfTT_activiteiten$events, unpack_duration)

## Bereken de gemiddelde duur van een werkvorm over de weken uit de events subtabel
dfTT_activiteiten <- dfTT_activiteiten %>%
  mutate(Gemiddelde_duur = map_dbl(durationmin, ~ if (length(.x) > 0) mean(.x) else NA)) %>% 
  filter(!is.na(Gemiddelde_duur))

## Summarize to create a new column with unique numbers
dfTT_summary <- dfTT_activiteiten %>%
  group_by(moduleCode, type, unl_jaar) %>%
  select(
    Gemiddelde_duur,
    studentGroups,
    repeatOf,
    week_count
  ) %>%
  summarise(
    # groups = sum(is.na(repeatOf)),
    groups = n_distinct(studentGroups),
    Unique_Numbers = c(list(table(unlist(week_count)))), # totale hoeveelheid weken in een vector
    total_weken = length(Unique_Numbers[[1]]), # telling van de lengte van de vector om aantal weken te vinden
    sumweek = sum(Unique_Numbers[[1]]), # optelling van de hoeveelheid unieke weken. Omdat een vak soms meerdere keren per week gegeven wordt
    totale_duur_per_groep_per_week = mean(Gemiddelde_duur), # gemiddelde duur over de weken dat een werkvorm gegeven wordt in minuten.
    totale_contact_uren = (sumweek * totale_duur_per_groep_per_week) / 60
  ) %>%
  select(-sumweek, -Unique_Numbers)

## Maak overzicht type werkvormen
## VU
dfTT_overzicht_werkvormen <- dfTT_summary %>%
  tabyl(type) %>%
  rename(aantal = n) %>%
  arrange(desc(aantal))

## Filter op type werkvorm & pivot
## TODO: select type werkvorm
dfTT_type_werkvorm <- dfTT_summary %>%
  mutate(type = case_when(
    ## vaste vormen
    type %in% c("Hoorcollege", "Lecture") ~ "Hoorcollege",
    type %in% c("Werkcollege", "Instructiecollege") ~ "Werk/instructiecollege",
    type %in% c("Practicum", "Computerpracticum", "training") ~ "Practicum/Lab",
    ## vormen 1, 2, 3
    type == "Werkgroep" ~ "Groepsopdracht (geroosterde begeleiding)",
    type %in% c("Excursie", "Veldwerk", "Symposium") ~ "Excursie",
    type == "Studiegroep" ~ "Groepsopdracht (niet geroosterde begeleiding)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(type)) %>%
  pivot_wider(
    names_from = type,
    values_from = c(
      "groups",
      "total_weken",
      "totale_duur_per_groep_per_week",
      "totale_contact_uren"
    )
  )


## Kolommen sorteren zodat deze overeenkomen met excel import template van de Data Entry App
## TODO: verbeteren
# dfTT_data_entry_app2 <- dfTT_type_werkvorm[, c(1, 2, 8, 14, 20, 3, 9, 15, 21, 4, 10, 16, 22, 5, 11, 17, 23, 6, 12, 18, 24, 7, 13, 19, 25)]
dfTT_data_entry_app <- dfTT_type_werkvorm


##' TODO:
##' code faculteit uit termtime behouden (departmentCode)
##' haal uit termtime
##'
api_key <- Sys.getenv("TERMTIME_KEY_2425")
base_url <- "https://www.ttportalvu1.com/"
termtime <- vvtermtime::authenticate(api_key, base_url)
df_termtime_faculteiten <- vvtermtime::get_departments(termtime)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Get werkvormen ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Step 1: Identify relevant columns (non-NULL columns)
# Extract column names related to "groups" and "totale_duur_per_groep_per_week"
group_columns <- names(dfTT_data_entry_app) %>%
  str_subset("^groups_") # Columns starting with "groups_"

duration_columns <- names(dfTT_data_entry_app) %>%
  str_subset("^totale_duur_per_groep_per_week_") # Columns starting with "totale_duur_per_groep_per_week_"

# Ensure we are working with non-NULL columns
non_null_columns <- dfTT_data_entry_app %>%
  ungroup() %>% 
  select(all_of(duration_columns)) %>%
  summarise(across(everything(), ~ any(!is.null(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "not_null") %>%
  filter(not_null) %>%
  pull(column)

# Filter group and duration columns to only include non-NULL ones
group_columns <- group_columns[group_columns %in% str_replace(non_null_columns, "totale_duur_per_groep_per_week_", "groups_")]
duration_columns <- duration_columns[duration_columns %in% non_null_columns]

# Step 2: Reshape data into long format for processing
long_df <- dfTT_data_entry_app %>%
  pivot_longer(
    cols = all_of(c(group_columns, duration_columns)),
    names_to = c(".value", "werkvorm"),
    names_pattern = "(.*)_(.*)"
  )

# Step 3: Rank werkvormen by duration for each moduleCode and unl_jaar
ranked_df <- long_df %>%
  filter(werkvorm %in% c(
    "Groepsopdracht (geroosterde begeleiding)",
    "Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)"
  )) %>% 
  group_by(moduleCode, unl_jaar) %>%
  filter(!is.na(totale_duur_per_groep_per_week)) %>% # Remove rows with NA durations
  arrange(desc(totale_duur_per_groep_per_week), .by_group = TRUE) %>% # Sort by duration
  mutate(rank = row_number()) %>% # Rank werkvormen by duration
  ungroup()

# Step 4: Assign werkvorm_1, werkvorm_2, werkvorm_3 based on rank
result_df <- ranked_df %>%
  filter(rank <= 3) %>% # Keep only top 3 werkvormen
  pivot_wider(
    id_cols = c(moduleCode, unl_jaar),
    names_from = rank,
    values_from = werkvorm,
    names_prefix = "name_werkvorm"
  )

# Join back to the original dataset
dfTT_data_entry_app <- dfTT_data_entry_app %>%
  left_join(result_df, by = c("moduleCode", "unl_jaar"))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Join dfVak ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfVAK <- read_file_proj("KEK_Vakken", dir = "1. Ingelezen data/")

Opleidingkoppel <- readrds_csv(output = "2. Geprepareerde Data/INS_Opleidingkoppel.rds")

## Fill some missing opleidingscode_actueel
dfVAK <- dfVAK %>%
  left_join(
    Opleidingkoppel %>%
      distinct(INS_Studieprogramma_CD, INS_Inschrijvingsjaar, .keep_all = TRUE) %>%
      select(INS_Studieprogramma_CD, INS_Inschrijvingsjaar, INS_Opleidingscode_actueel),
    by = c(
      "UAS_Vak_Opleidingscode_eigenaar" = "INS_Studieprogramma_CD",
      "UAS_Vak_Jaar" = "INS_Inschrijvingsjaar"
    )
  ) %>%
  mutate(
    INS_Opleidingscode_actueel = case_when(
      !is.na(INS_Opleidingscode_actueel.x) ~ INS_Opleidingscode_actueel.x,
      !is.na(INS_Opleidingscode_actueel.y) ~ INS_Opleidingscode_actueel.y,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-INS_Opleidingscode_actueel.x, -INS_Opleidingscode_actueel.y)

## Fill missing period information:
dfVAK <- dfVAK %>%
  group_by(UAS_Vak_Code) %>%
  fill(UAS_Vak_Periode_start, UAS_Vak_Periode_einde, .direction = "downup") %>%
  ungroup()


dfVAK <- dfVAK %>%
  group_by(UAS_Vak_Jaar, UAS_Vak_Periode_start) %>%
  fill(Startmoment_vak, ACA_Einddatum, ACA_Einddatum_prev, .direction = "downup") %>%
  ungroup() %>% 
  mutate(weeks_diff = round(as.numeric(difftime(ACA_Einddatum, Startmoment_vak, units = "weeks")))) 


dfToetsen <- dfTT_activiteiten %>% filter(type %in% c("Tentamen schriftelijk",
                                                      "Tentamen digitaal")) %>% 
  select(moduleCode, unl_jaar) %>% 
  distinct(moduleCode, .keep_all = TRUE) %>% 
  mutate(unl_eindtoetsnaam = 941790006)



dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(jaar = parse_number(unl_jaar)) %>%
  left_join(dfVAK, by = c(
    "moduleCode" = "UAS_Vak_Code",
    "jaar" = "UAS_Vak_Jaar"
  )) %>%
  select(
    # -jaar,
    -ACA_Einddatum_prev,
    -UAS_Vak_Periode_einde,
  ) %>%
  left_join(
    dfToetsen %>% select(moduleCode, unl_jaar, unl_eindtoetsnaam),
    by = c("moduleCode", "unl_jaar" = "unl_jaar")
  ) %>%
  mutate(
    unl_eindtoetsnaam = coalesce(unl_eindtoetsnaam.x, unl_eindtoetsnaam.y)
  ) %>%
  select(-unl_eindtoetsnaam.x, -unl_eindtoetsnaam.y, -jaar)


## assign per werkvorm name
source("04_send/99_helper_werkvormen.R")




## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## update unl_vakjaar ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# Create lookup vector (right-hand values map to left-hand codes)
vakjaar_lookup <- c(
  "1" = 1,
  "2" = 2,
  "3" = 3,
  "4" = 941790000,
  "5" = 941790001,
  "Premaster" = 4,  # Same code as "4" based on your options
  "Honoursprogramma" = 5,     # Same code as "5"
  "4" = 941790000,           # Special case for numeric 4
  "5" = 941790001,           # Special case for numeric 5
  "6" = 941790002,           # Special case for numeric 6
  "7" = 941790003           # Special case for numeric 7
)

dfTT_data_entry_app <- dfTT_data_entry_app %>%
  # Create temporary column with parsed numbers
  mutate(parsed_num = parse_number(unl_vakjaar)) %>% 
  # Update original column using temporary values
  mutate(unl_vakjaar = if_else(
    is.na(parsed_num),
    unl_vakjaar,          # Keep original if no number found
    as.character(parsed_num)       # Use parsed number if available
  )) %>%
  # Remove temporary column
  select(-parsed_num) 

dfTT_data_entry_app <- dfTT_data_entry_app %>% 
  mutate(
    unl_vakjaar = recode(
      as.character(unl_vakjaar),  # Ensure character type for matching
      !!!vakjaar_lookup,             # Spread lookup list
      .default = NULL         # Handle non-matching cases
    )
  )

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


##' Gebruik collegejaar id uit KeK
dfcolleges <- get_kek_data("collegejaars")

dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(
    `unl_Jaar@odata.bind` = sapply(unl_jaar, function(year) {
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


dfopleidings <- get_kek_data("opleidings")

## extract unl_jaar column from  unl_name  , example: ""M Computational Science (joint degree) 2019-2020" --> "2019-2020" 
dfopleidings_enriched <- dfopleidings %>% 
  mutate(
    unl_jaar = str_extract(unl_name, "\\d{4}-\\d{4}$")
  ) %>% 
  filter(unl_jaar %in% c(
    "2021-2022",
    "2022-2023",
    "2023-2024",
    "2024-2025"
  )  
  )



# dfTT_data_entry_app_test <- dfTT_data_entry_app %>%
#   rowwise() %>%
#   mutate(
#     `unl_Opleiding@odata.bind` = {
#       match <- dfopleidings_enriched %>%
#         filter(unl_opleidingscodeisat == INS_Opleidingscode_actueel, unl_jaar == unl_jaar) %>%
#         pull(unl_opleidingid)
#       
#       if (length(match) > 0) {
#         paste0("unl_opleidings(", match[1], ")")
#       } else {
#         NA_character_
#       }
#     }
#   ) %>%
#   ungroup()

dfTT_data_entry_app <- dfTT_data_entry_app %>%
  rowwise() %>%
  mutate(
    `unl_Opleiding@odata.bind` = {
      current_unl_jaar <- unl_jaar
      match <- dfopleidings_enriched %>%
        filter(unl_opleidingscodeisat == INS_Opleidingscode_actueel, unl_jaar == current_unl_jaar) %>%
        pull(unl_opleidingid)
      
      if (length(match) > 0) {
        paste0("unl_opleidings(", match[1], ")")
      } else {
        NA_character_
      }
    }
  ) %>%
  ungroup()




##' Replace value by code for unl_werkvormnaam
##'


# Create a lookup table for the werkvormen
werkvorm_lookup <- c(
  "Hoorcollege" = 941790007,
  "Werkcollege" = 941790008,
  "Instructiecollege" = 941790008,
  "Werk/instructiecollege" = 941790008,
  "Practicum/Laboratorium" = 941790009,
  "Practicum/Lab" = 941790009,
  "Groepsopdracht (geroosterde begeleiding)" = 941790004,
  "Groepsopdracht (met docent op afroep)" = 941790011,
  "Groepsopdracht (niet geroosterde begeleiding)" = 941790012,
  "Individuele begeleiding (geen scriptie)" = 941790002,
  "Excursie" = 941790010,
  "Stage" = 941790005,
  "Scriptie/afstudeeropdracht BSc" = 941790000,
  "Scriptie/afstudeeropdracht MSc" = 941790001
)

# Function to map werkvorm names to codes
map_werkvorm_to_code <- function(werkvorm) {
  ifelse(werkvorm %in% names(werkvorm_lookup),
         werkvorm_lookup[werkvorm],
         NA_integer_
  )
}

# Apply the transformation to the dataset
dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(
    unl_werkvorm1naam = map_werkvorm_to_code(unl_werkvorm1naam),
    unl_werkvorm2naam = map_werkvorm_to_code(unl_werkvorm2naam),
    unl_werkvorm3naam = map_werkvorm_to_code(unl_werkvorm3naam)
  )



dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(unl_verplichtkeuze = case_when(
    unl_verplichtkeuze == TRUE ~ "1",
    unl_verplichtkeuze == FALSE ~ "2",
    TRUE ~ NA_character_
  )) %>%
  mutate(unl_verplichtkeuze = parse_number(unl_verplichtkeuze))

## Create lookup for unl_taalvanvak
dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(unl_taalvanvak = case_when(
    unl_taalvanvak == "Nederlands (NL)" ~ 941790000,
    unl_taalvanvak == "Engels (EN)" ~ 941790001,
    unl_taalvanvak == "Anderstalig" ~ 941790002,
    unl_taalvanvak == "Tweetalig (Z1)" ~ 941790002,
    TRUE ~ NA_integer_
  ))

dfTT_data_entry_app2 <- dfTT_data_entry_app %>%
  select(
    -unl_werkvorm2totaalcontacturen,
    -unl_werkvorm1totaalcontacturen,
    -unl_werkvorm3totaalcontacturen,
    # -unl_werkvorm3naam,
    # -unl_jaar,
    -INS_Opleidingscode_actueel,
    # -`unl_Collegejaar@odata.bind`,
    -unl_hoorcollegetotaalcontacturen
  ) %>%
  ungroup() %>%
  unnest() %>% 
  ## minuten naar uur
  mutate(unl_hoorcollegeurenperweekpergroep = unl_hoorcollegeurenperweekpergroep / 60) %>% 
  mutate(unl_werkcollegeurenperweekpergroep = unl_werkcollegeurenperweekpergroep / 60) %>% 
  mutate(unl_practicumurenperweekpergroep = unl_practicumurenperweekpergroep / 60) %>%
  mutate(unl_werkvorm1urenperweekpergroep = unl_werkvorm1urenperweekpergroep / 60) %>% 
  mutate(unl_werkvorm2urenperweekpergroep = unl_werkvorm2urenperweekpergroep / 60) %>% 
  mutate(unl_werkvorm3urenperweekpergroep = unl_werkvorm3urenperweekpergroep / 60)

##' *TODO* 
##' LOSES A LOT OF ENTRIES -------------------------------------------------------------------------
##' 
dfTT_data_entry_app2 <- dfTT_data_entry_app2 %>%
  dplyr::group_by(unl_vakcode) %>%
  dplyr::mutate(
    `unl_Opleiding@odata.bind` =
      dplyr::coalesce(`unl_Opleiding@odata.bind`,
                      first(na.omit(`unl_Opleiding@odata.bind`)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(unl_startdatum)) %>%
  dplyr::filter(!is.na(unl_aantalgeslaagdeneindtoets)) %>% 
  dplyr::filter(!is.na(`unl_Opleiding@odata.bind`))



## Fix: aantal groepn 0, terwijl urenperweekpergroep > 0
dfTT_data_entry_app2 <- dfTT_data_entry_app2 %>%
  mutate(
    # Fix for werkvorm2
    unl_werkvorm2groepen = if_else(
      unl_werkvorm2urenperweekpergroep > 0 & unl_werkvorm2groepen == 0,
      1,
      unl_werkvorm2groepen
    ),
    
    # Fix for hoorcollege
    unl_hoorcollegegroepen = if_else(
      unl_hoorcollegeurenperweekpergroep > 0 & unl_hoorcollegegroepen == 0,
      1,
      unl_hoorcollegegroepen
    )
  )

##' TEMP and hardcoded...
##' 
##' attempt to fill missing unl_eindtoetsnaam with filled dfToetsvormen variable toetsvorm_code 
##' 
dfDoelgroep <- readrds_csv(output = "20. Test/dfdoelgroep.csv") %>% 
  filter(UAS_Vak_Jaar >= 2021) %>% 
  mutate(UAS_Vak_Jaar = as.character(UAS_Vak_Jaar),
         UAS_Vak_Jaar = paste0(UAS_Vak_Jaar, "-", as.numeric(UAS_Vak_Jaar) + 1))

dfDoelgroep <- dfDoelgroep %>%
  group_by(Code) %>%  # group only by 'Code'
  arrange(UAS_Vak_Jaar, .by_group = TRUE) %>%  # order by year for each code
  fill(unl_vak_jaar_code, .direction = "downup") %>%  # fill missing values down then up
  ungroup() %>% 
  distinct()


dfTT_data_entry_app2 <- dfTT_data_entry_app2 %>%
  distinct(unl_vakcode, unl_jaar, .keep_all = TRUE) %>% 
  left_join(
    dfDoelgroep %>%
      select(Code, unl_vak_jaar_code, UAS_Vak_Jaar) %>%
      distinct(),
    by = c("unl_vakcode" = "Code", "unl_jaar" = "UAS_Vak_Jaar"), relationship = "one-to-one"
  ) %>%
  mutate(
    # coalesce unl_vakjaar with unl_vak_jaar_code from lookup, preferring original if present
    unl_vakjaar = coalesce(unl_vakjaar, unl_vak_jaar_code)
  ) 

dfTT_data_entry_app3 <- dfTT_data_entry_app2 %>%
  # Fill toetsnaam (unl_eindtoetsnaam) from previous year if missing
  group_by(unl_vakcode, unl_name) %>%
  arrange(unl_jaar, .by_group = TRUE) %>%
  mutate(unl_eindtoetsnaam = ifelse(
    is.na(unl_eindtoetsnaam),
    lag(unl_eindtoetsnaam, order_by = unl_jaar),
    unl_eindtoetsnaam
  )) %>%
  ungroup() %>%
  # If EC's are NA, remove toetsing (unl_eindtoetsnaam)
  mutate(unl_eindtoetsnaam = ifelse(
    is.na(unl_ecs),
    NA,
    unl_eindtoetsnaam
  )) %>%
  # Default to "schriftelijke toetsing" if eindtoetsnaam still missing and ECS exists
  mutate(unl_eindtoetsnaam = ifelse(
    is.na(unl_eindtoetsnaam) & !is.na(unl_ecs),
    941790006,
    unl_eindtoetsnaam
  )) %>%
  # Fill Verplicht/Keuze (unl_verplichtkeuze) from previous year
  group_by(unl_vakcode, unl_name) %>%
  arrange(unl_jaar, .by_group = TRUE) %>%
  mutate(unl_verplichtkeuze = ifelse(
    is.na(unl_verplichtkeuze),
    lag(unl_verplichtkeuze, order_by = unl_jaar),
    unl_verplichtkeuze
  )) %>%
  ungroup() %>%
  select(-unl_vak_jaar_code, -unl_jaar)


bbb <- send_data_to_kek(dfTT_data_entry_app3, "vaks")


clear_script_objects()
