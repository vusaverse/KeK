## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

modular_assign <- function(df, col_base, source_map, name_col) {
  df %>%
    ungroup() %>%
    mutate("{col_base}" := case_when(
      !!sym(name_col) == names(source_map)[1] ~ df[[source_map[[1]]]],
      !!sym(name_col) == names(source_map)[2] ~ df[[source_map[[2]]]],
      !!sym(name_col) == names(source_map)[3] ~ df[[source_map[[3]]]],
      !!sym(name_col) == names(source_map)[4] ~ df[[source_map[[4]]]],
      !!sym(name_col) == names(source_map)[5] ~ df[[source_map[[5]]]],
      !!sym(name_col) == names(source_map)[6] ~ df[[source_map[[6]]]]
    ))
}

# Source lookups for each category:
group_sources <- c(
  "Hoorcollege" = "groups_Hoorcollege",
  "Werk/instructiecollege" = "groups_Werk/instructiecollege",
  "Groepsopdracht (geroosterde begeleiding)" = "groups_Groepsopdracht (geroosterde begeleiding)",
  "Practicum/Lab" = "groups_Practicum/Lab",
  "Excursie" = "groups_Excursie",
  "Groepsopdracht (niet geroosterde begeleiding)" = "groups_Groepsopdracht (niet geroosterde begeleiding)"
)
weeks_sources <- c(
  "Hoorcollege" = "total_weken_Hoorcollege",
  "Werk/instructiecollege" = "total_weken_Werk/instructiecollege",
  "Groepsopdracht (geroosterde begeleiding)" = "total_weken_Groepsopdracht (geroosterde begeleiding)",
  "Practicum/Lab" = "total_weken_Practicum/Lab",
  "Excursie" = "total_weken_Excursie",
  "Groepsopdracht (niet geroosterde begeleiding)" = "total_weken_Groepsopdracht (niet geroosterde begeleiding)"
)
duur_sources <- c(
  "Hoorcollege" = "totale_duur_per_groep_per_week_Hoorcollege",
  "Werk/instructiecollege" = "totale_duur_per_groep_per_week_Werk/instructiecollege",
  "Groepsopdracht (geroosterde begeleiding)" = "totale_duur_per_groep_per_week_Groepsopdracht (geroosterde begeleiding)",
  "Practicum/Lab" = "totale_duur_per_groep_per_week_Practicum/Lab",
  "Excursie" = "totale_duur_per_groep_per_week_Excursie",
  "Groepsopdracht (niet geroosterde begeleiding)" = "totale_duur_per_groep_per_week_Groepsopdracht (niet geroosterde begeleiding)"
)
contact_sources <- c(
  "Hoorcollege" = "totale_contact_uren_Hoorcollege",
  "Werk/instructiecollege" = "totale_contact_uren_Werk/instructiecollege",
  "Groepsopdracht (geroosterde begeleiding)" = "totale_contact_uren_Groepsopdracht (geroosterde begeleiding)",
  "Practicum/Lab" = "totale_contact_uren_Practicum/Lab",
  "Excursie" = "totale_contact_uren_Excursie",
  "Groepsopdracht (niet geroosterde begeleiding)" = "totale_contact_uren_Groepsopdracht (niet geroosterde begeleiding)"
)

df <- dfTT_data_entry_app
for(i in 1:3) {
  df <- modular_assign(df, paste0("group_", i), group_sources, paste0("name_werkvorm", i))
  df <- modular_assign(df, paste0("total_weeks_", i), weeks_sources, paste0("name_werkvorm", i))
  df <- modular_assign(df, paste0("duration_per_group_per_week_", i), duur_sources, paste0("name_werkvorm", i))
  df <- modular_assign(df, paste0("contact_hours_", i), contact_sources, paste0("name_werkvorm", i))
}

dfTT_data_entry_app <- df

# df2 <- df %>% select(
#   moduleCode,
#   starts_with("name_werkvorm"),
#   starts_with("group_"),
#   starts_with("total_weeks_"),
#   starts_with("duration_per_group_per_week_"),
#   starts_with("contact_hours_")
# )
