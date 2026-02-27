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

modular_assign <- function(df2, col_base, source_map, name_col) {
  df2 <- df2 %>%
    ungroup() %>%
    mutate("{col_base}" := case_when(
      !!sym(name_col) == names(source_map)[1] ~ df2[[source_map[[1]]]],
      !!sym(name_col) == names(source_map)[2] ~ df2[[source_map[[2]]]],
      !!sym(name_col) == names(source_map)[3] ~ df2[[source_map[[3]]]],
      !!sym(name_col) == names(source_map)[4] ~ df2[[source_map[[4]]]],
      !!sym(name_col) == names(source_map)[5] ~ df2[[source_map[[5]]]],
      !!sym(name_col) == names(source_map)[6] ~ df2[[source_map[[6]]]]
    ))
  return(df2)
}

helper_werkvormen <- function(df) {
  
  # Source lookups for each category:
  group_sources <- c(
    "Hoorcollege" = "n_unique_student_groups_Hoorcollege",
    "Werk/instructiecollege" = "n_unique_student_groups_Werk/instructiecollege",
    "Groepsopdracht (geroosterde begeleiding)" = "n_unique_student_groups_Groepsopdracht (geroosterde begeleiding)",
    "Practicum/Lab" = "n_unique_student_groups_Practicum/Lab",
    "Excursie" = "n_unique_student_groups_Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)" = "n_unique_student_groups_Groepsopdracht (niet geroosterde begeleiding)"
  )
  weeks_sources <- c(
    "Hoorcollege" = "n_unique_weeks_Hoorcollege",
    "Werk/instructiecollege" = "n_unique_weeks_Werk/instructiecollege",
    "Groepsopdracht (geroosterde begeleiding)" = "n_unique_weeks_Groepsopdracht (geroosterde begeleiding)",
    "Practicum/Lab" = "n_unique_weeks_Practicum/Lab",
    "Excursie" = "n_unique_weeks_Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)" = "n_unique_weeks_Groepsopdracht (niet geroosterde begeleiding)"
  )
  duur_sources <- c(
    "Hoorcollege" = "gemiddelde_duur_per_groep_per_week_Hoorcollege",
    "Werk/instructiecollege" = "gemiddelde_duur_per_groep_per_week_Werk/instructiecollege",
    "Groepsopdracht (geroosterde begeleiding)" = "gemiddelde_duur_per_groep_per_week_Groepsopdracht (geroosterde begeleiding)",
    "Practicum/Lab" = "gemiddelde_duur_per_groep_per_week_Practicum/Lab",
    "Excursie" = "gemiddelde_duur_per_groep_per_week_Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)" = "gemiddelde_duur_per_groep_per_week_Groepsopdracht (niet geroosterde begeleiding)"
  )
  contact_sources <- c(
    "Hoorcollege" = "totale_contacturen_Hoorcollege",
    "Werk/instructiecollege" = "totale_contacturen_Werk/instructiecollege",
    "Groepsopdracht (geroosterde begeleiding)" = "totale_contacturen_Groepsopdracht (geroosterde begeleiding)",
    "Practicum/Lab" = "totale_contacturen_Practicum/Lab",
    "Excursie" = "totale_contacturen_Excursie",
    "Groepsopdracht (niet geroosterde begeleiding)" = "totale_contacturen_Groepsopdracht (niet geroosterde begeleiding)"
  )
  
  for(i in 1:3) {
    df <- modular_assign(df, paste0("group_", i), group_sources, paste0("name_werkvorm", i))
    df <- modular_assign(df, paste0("total_weeks_", i), weeks_sources, paste0("name_werkvorm", i))
    df <- modular_assign(df, paste0("duration_per_group_per_week_", i), duur_sources, paste0("name_werkvorm", i))
    df <- modular_assign(df, paste0("contact_hours_", i), contact_sources, paste0("name_werkvorm", i))
  }
  
  
  ## case_when empty UAS_Groep_Groepstype useUAS_Groep_College_jaar
  df <- df %>%
    ungroup() %>% 
    mutate(
      UAS_Groep_Groepstype = case_when(
        is.na(UAS_Groep_Groepstype) | UAS_Groep_Groepstype == "" ~ as.character(UAS_Groep_College_jaar),
        TRUE ~ UAS_Groep_Groepstype
      )
    )
  
  ## Lees documentatie in
  KeK_termtime_naming <- read_documentation(
    "Documentatie_KeK_TermTime_API.csv"
  )
  
  
  ## Pas kolomnamen aan zodat deze overeenkomen met KeK data entry app
  df <- df %>%
    wrapper_translate_colnames_documentation(KeK_termtime_naming)
  
  
  #' Makes sure that the werkvormnaam becomes null if the other variables are empty for that werkvorm
  
  # helper: TRUE als een list-column element echt leeg/NULL is
  is_empty_list <- function(x) {
    map_lgl(x, ~ is.null(.x) || length(.x) == 0)
  }
  
  clear_werkvorm_name <- function(df, n) {
    groepen_col   <- paste0("unl_werkvorm", n, "groepen")
    weken_col     <- paste0("unl_werkvorm", n, "aantalweken")
    uren_col      <- paste0("unl_werkvorm", n, "urenperweekpergroep")
    totaal_col    <- paste0("unl_werkvorm", n, "totaalcontacturen")
    naam_col      <- paste0("unl_werkvorm", n, "naam")
    
    df %>%
      mutate(
        # test: alle bijbehorende list-kolommen leeg/NULL?
        all_empty_werkvorm = if_all(
          all_of(c(groepen_col, weken_col, uren_col, totaal_col)),
          is_empty_list
        ),
        !!naam_col := if_else(
          all_empty_werkvorm,
          NA_character_,
          !!sym(naam_col)
        )
      ) %>%
      select(-all_empty_werkvorm)
  }
  
  df <- df %>%
    clear_werkvorm_name(1) %>%
    clear_werkvorm_name(2) %>%
    clear_werkvorm_name(3)
  
  return(df)
}

