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
    week_vector
  ) %>%
  summarise(
    # groups = sum(is.na(repeatOf)),
    n_unique_student_groups = n_distinct(ter_student_groups),
    week_counts = c(list(table(unlist(week_vector)))), # telling van weken per weeknummer
    n_unique_weeks = length(week_counts[[1]]), # telling van de lengte van de vector om aantal unieke weken te vinden
    nActivities = sum(week_counts[[1]]), # optelling van de hoeveelheid  weken. Omdat een vak soms meerdere keren per week gegeven wordt
    gemiddelde_duur_per_groep_per_week = mean(Gemiddelde_duur), # gemiddelde duur over de weken dat een werkvorm gegeven wordt in minuten.
    totale_contacturen = (sumweek * totale_duur_per_groep_per_week) / contactuur_duration
  ) %>%
  select(-nActivities, -week_counts)

## Maak overzicht type werkvormen
## VU
dfTT_overzicht_werkvormen <- dfTT_summary %>%
  tabyl(type) %>%
  rename(aantal = n) %>%
  arrange(desc(aantal))