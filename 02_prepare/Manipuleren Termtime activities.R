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

dfTermtime_activities <- load_rds_from_azure(
  config = .config,
  container_type = "bronze",
  file_name = "Termtime_activities_enriched.rds"
) %>% 
  dplyr::filter(
    str_detect(ter_source_file, "activities_20[0-9]{2}.rds$")
  )
  


## Add year variables
dfTermtime_activities <- dfTermtime_activities %>% 
  dplyr::mutate(
    jaar = as.numeric(str_extract(ter_source_file, "20[0-9]{2}")),
    unl_jaar = paste0(jaar, "-", jaar + 1)
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Extract weken uit de Events subtabel
dfTermtime_activities$week_vector <- sapply(dfTermtime_activities$ter_events, unpack_weeks)

## Extract duur van werkvormen uit de events subtabel
dfTermtime_activities$durationmin <- sapply(dfTermtime_activities$ter_events, unpack_duration)

## Bereken de gemiddelde duur van een werkvorm over de weken uit de events subtabel
dfTermtime_activities <- dfTermtime_activities %>%
  mutate(Gemiddelde_duur = map_dbl(durationmin, ~ if (length(.x) > 0) mean(.x) else NA)) %>% 
  filter(!is.na(Gemiddelde_duur))


contactuur_duration <- 60
## Summarize to create a new column with unique numbers
dfTT_summary <- dfTermtime_activities %>%
  group_by(ter_module_code, ter_type, unl_jaar) %>%
  select(
    Gemiddelde_duur,
    ter_student_groups,
    ter_repeat_of,
    week_vector,
    durationmin
  ) %>%
  summarise(
    n_unique_student_groups = n_distinct(ter_student_groups),
    week_counts = c(list(table(unlist(week_vector)))), # telling van weken per weeknummer
    n_unique_weeks = length(week_counts[[1]]), # telling van de lengte van de vector om aantal unieke weken te vinden
    nActivities = sum(week_counts[[1]]), # optelling van de hoeveelheid  weken. Omdat een vak soms meerdere keren per week gegeven wordt
    totale_duur_rij = sum(unlist(durationmin)),
    gemiddelde_duur_per_groep_per_week =  totale_duur_rij/ n_unique_weeks, # gemiddelde duur over de weken dat een werkvorm gegeven wordt in minuten.
    totale_contacturen = totale_duur_rij / contactuur_duration
  ) %>%
  select(-c(nActivities, week_counts, totale_duur_rij)) 

## Maak overzicht type werkvormen
## VU
dfTT_overzicht_werkvormen <- dfTT_summary %>%
  tabyl(ter_type) %>%
  rename(aantal = n) %>%
  arrange(desc(aantal))



## Filter op type werkvorm & pivot
## TODO: select type werkvorm properly. 
dfTT_type_werkvorm <- dfTT_summary %>%
  mutate(ter_type = case_when(
    ## vaste vormen
    ter_type %in% c("Hoorcollege", "Lecture") ~ "Hoorcollege",
    ter_type %in% c("Werkcollege", "Instructiecollege") ~ "Werk/instructiecollege",
    ter_type %in% c("Practicum", "Computerpracticum", "training") ~ "Practicum/Lab",
    ## vormen 1, 2, 3
    ## TODO hoort werkgroep hier wel in?
    ter_type == "Werkgroep" ~ "Groepsopdracht (geroosterde begeleiding)",
    ## TODO Ik zou symposium niet onder Excursie scharen
    ter_type %in% c("Excursie", "Veldwerk", "Symposium") ~ "Excursie",
    ## TODO snap deze ook niet
    ter_type == "Studiegroep" ~ "Groepsopdracht (niet geroosterde begeleiding)",
    .default = NA_character_
  )) %>%
  filter(!is.na(ter_type)) %>%
  pivot_wider(
    names_from = ter_type,
    values_from = c(
      "n_unique_student_groups",
      "n_unique_weeks",
      "gemiddelde_duur_per_groep_per_week",
      "totale_contacturen"
    )
  )


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Get werkvormen ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Step 1: Identify relevant columns (non-NULL columns)
# Extract column names related to "groups" and "totale_duur_per_groep_per_week"
group_columns <- names(dfTT_type_werkvorm) %>%
  str_subset("^n_unique_student_groups") # Columns starting with "groups_"

duration_columns <- names(dfTT_type_werkvorm) %>%
  str_subset("^gemiddelde_duur_per_groep_per_week") # Columns starting with "gemiddelde_duur_per_groep_per_week"

# Ensure we are working with non-NULL columns
non_null_columns <- dfTT_type_werkvorm %>%
  ungroup() %>% 
  select(all_of(duration_columns)) %>%
  summarise(across(everything(), ~ any(!is.null(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "not_null") %>%
  filter(not_null) %>%
  pull(column)

# Filter group and duration columns to only include non-NULL ones
group_columns <- group_columns[group_columns %in% str_replace(non_null_columns, "gemiddelde_duur_per_groep_per_week_", "n_unique_student_groups_")]
duration_columns <- duration_columns[duration_columns %in% non_null_columns]

# Step 2: Reshape data into long format for processing
dfTT_type_werkvorm_long <- dfTT_type_werkvorm %>%
  pivot_longer(
    cols = all_of(c(group_columns, duration_columns)),
    names_to = c(".value", "werkvorm"),
    names_pattern = "(.*)_(.*)"
  )

# Step 3: Rank werkvormen by duration for each moduleCode and unl_jaar
## TODO what is the point of this block? Why keep top three when only three are selected?
ranked_df <- dfTT_type_werkvorm_long %>%
  filter(werkvorm %in% c(
    "Groepsopdracht (geroosterde begeleiding)",
    "Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)"
  )) %>% 
  ## TODO are we sure we need to rank this by module? Later on in the documentation we seem to assign
  ## specific werkvormen to the numbers, so shouldnt the numbers be consistent across the data
  group_by(ter_module_code, unl_jaar) %>%
  filter(!is.na(gemiddelde_duur_per_groep_per_week)) %>% # Remove rows with NA durations
  arrange(desc(gemiddelde_duur_per_groep_per_week), .by_group = TRUE) %>% # Sort by duration
  mutate(rank = row_number()) %>% # Rank werkvormen by duration
  ungroup()

# Step 4: Assign werkvorm_1, werkvorm_2, werkvorm_3 based on rank
result_df <- ranked_df %>%
  filter(rank <= 3) %>% # Keep only top 3 werkvormen
  pivot_wider(
    id_cols = c(ter_module_code, unl_jaar),
    names_from = rank,
    values_from = werkvorm,
    names_prefix = "name_werkvorm"
  )

# Join back to the original dataset
dfTT_type_werkvorm <- dfTT_type_werkvorm %>%
  left_join(result_df, by = c("ter_module_code", "unl_jaar"))

### TODO End weird block


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Join dfVak ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfVAK <- read_file_proj("KEK_Vakken", dir = "1. Ingelezen data/")

Opleidingkoppel <- readrds_csv(output = "2. Geprepareerde Data/INS_Opleidingkoppel.rds")

## Fill some missing opleidingscode_actueel
## TODO why here and not in the vak script
dfVAK <- dfVAK %>%
  left_join(
    Opleidingkoppel %>%
      distinct(INS_Studieprogramma_CD, INS_Inschrijvingsjaar, .keep_all = TRUE) %>%
      ## TODO not Z08 instead of actueel?
      select(INS_Studieprogramma_CD, INS_Inschrijvingsjaar, INS_Opleidingscode_actueel),
    by = c(
      "UAS_Vak_Opleidingscode_eigenaar" = "INS_Studieprogramma_CD",
      "UAS_Vak_Jaar" = "INS_Inschrijvingsjaar"
    )
  ) %>%
  mutate(
    INS_Opleidingscode_actueel = coalesce(INS_Opleidingscode_actueel.x, INS_Opleidingscode_actueel.y)
  ) %>%
  select(-c(INS_Opleidingscode_actueel.x, INS_Opleidingscode_actueel.y))

## Fill missing period information:
dfVAK <- dfVAK %>%
  arrange(UAS_Vak_Jaar) %>% 
  group_by(UAS_Vak_Code) %>%
  fill(UAS_Vak_Periode_start, UAS_Vak_Periode_einde, .direction = "downup") %>%
  ungroup()


dfVAK <- dfVAK %>%
  group_by(UAS_Vak_Jaar, UAS_Vak_Periode_start) %>%
  fill(Startmoment_vak, ACA_Einddatum, ACA_Einddatum_prev, .direction = "downup") %>%
  ungroup() %>% 
  mutate(weeks_diff = round(as.numeric(difftime(ACA_Einddatum, Startmoment_vak, units = "weeks")))) 

## termtime takes priority when determining whether toetsvorm is tentamen
dfToetsen <- dfTermtime_activities %>% filter(ter_type %in% c("Tentamen schriftelijk",
                                                      "Tentamen digitaal")) %>% 
  select(ter_module_code, unl_jaar) %>% 
  distinct(ter_module_code, .keep_all = TRUE) %>% 
  mutate(unl_eindtoetsnaam = 941790006)

dfTT_type_werkvorm <- dfTT_type_werkvorm %>%
  mutate(jaar = parse_number(unl_jaar)) %>%
  left_join(dfVAK, by = c(
    "ter_module_code" = "UAS_Vak_Code",
    "jaar" = "UAS_Vak_Jaar"
  )) %>%
  select(
    # -jaar,
    -ACA_Einddatum_prev,
    -UAS_Vak_Periode_einde,
  ) %>%
  left_join(
    dfToetsen %>% select(ter_module_code, unl_jaar, unl_eindtoetsnaam),
    by = c("ter_module_code", "unl_jaar" = "unl_jaar")
  ) %>%
  mutate(
    unl_eindtoetsnaam = coalesce(unl_eindtoetsnaam.x, unl_eindtoetsnaam.y)
  ) %>%
  select(-unl_eindtoetsnaam.x, -unl_eindtoetsnaam.y, -jaar)


## assign per werkvorm name
source("99_utils/helper_functions/helper_werkvormen.R")

dfTT_type_werkvorm <- helper_werkvormen(dfTT_type_werkvorm)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Save
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

upload_file_to_azure_blob_storage(
  "eaprodstorage",
  sas_token = Sys.getenv("SAS_Token"),
  container_name ="finance",
  data = dfTT_type_werkvorm,
  dataset_name = "KeK/Termtime_vakdata",
  file_type = "rds"
)

clear_script_objects()
