## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfKEK_FIN <- read_file_proj("KEK_FIN")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EDIT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Add Mapping KeK names
dfKEK_FIN <- dfKEK_FIN %>%
  mapping_translate(
    current = "Toewijzing",
    new = "Mapping KeK",
    mapping_table_name = "Mapping_KEK.csv",
    KeepOriginal = TRUE
  )

## Pivot table
dfKEK_FIN_compleet <- dfKEK_FIN %>%
  select(-c(
    Exploitatieresultaat,
    starts_with("YTD Realisatie - Begroting"),
    starts_with("Realisatie")
  )) %>%
  filter(!is.na(`Toewijzing`) & !is.na(`Mapping KeK`)) %>%
  pivot_longer(
    cols = starts_with("YTD Realisatie"),
    names_to = "Year",
    values_to = "Waarde"
  ) %>%
  mutate(
    Jaar = as.numeric(Jaar),
    Jaar = case_when(
      Year == "YTD Realisatie vorig jaar" ~ Jaar - 1,
      TRUE ~ Jaar
    ),
    row_number = row_number()
  ) %>%
  group_by(
    Faculteit,
    Jaar,
    `Mapping KeK`,
    Toewijzing
  ) %>% 
  filter(Year == "YTD Realisatie vorig jaar" |
           case_when(Jaar == 2023 ~ Year == "YTD Realisatie")) %>% 
  ungroup() %>% 
  group_by(
    Faculteit,
    `Mapping KeK`,
    Jaar
  ) %>%
  summarise(
    Waarde = sum(Waarde, na.rm = TRUE),
    ## TODO kan later weg denk ik
    Naam_op_VU = paste(Toewijzing, collapse = ", "),
    row_number = min(row_number)
  ) %>%
  ungroup() %>%
  rename(Veldnaam = `Mapping KeK`) %>%
  arrange(row_number) %>%
  select(Faculteit, Veldnaam, Waarde, Jaar, Naam_op_VU) %>%
  arrange(Faculteit, Jaar)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfKEK_FIN_compleet, "KEK_FIN_compleet")

clear_script_objects()
