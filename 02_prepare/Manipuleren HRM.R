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
dfKEK_HRM <- read_file_proj("KEK_HRM")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EDIT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Add faculteit using mapping table profit center
dfKEK_HRM <- dfKEK_HRM %>%
  mutate(Profit_center_afkorting = str_sub(`Profit Center aanstelling`, 1, 3)) %>%
  mapping_translate(
    current = "Profit_center_afkorting",
    new = "Faculteit",
    mapping_table_name = "Mapping_KeK_profit_center_faculteit.csv",
    KeepOriginal = FALSE
  )

## Adjust Tandartsdocent and Specialist in opleiding
## "tandartsdocent" = docent en "specialist in opleiding" = AIO
dfKEK_HRM <- dfKEK_HRM %>% 
  mutate(Omschrijving = str_replace(Omschrijving, "Tandartsdocent", "Docent"),
         Omschrijving = str_replace(Omschrijving, "Specialist in opleiding", "Promovendus"))

## Make moth columns
month_cols <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## Calculate FTE means
KEK_HRM_Personeel_prepared <- dfKEK_HRM %>%
  filter(MEASURES == "Doorbelaste FTE") %>%
  mutate(across(where(is.double), ~ replace_na(.x, 0))) %>%
  mutate(FTE_gemiddelde = rowMeans(across(all_of(month_cols)))) %>%
  mutate(FTE_gemiddelde_tot_juni = rowMeans(across(all_of(month_cols[1:6])))) %>%
  mutate(FTE_gemiddelde_vanaf_juli = rowMeans(across(all_of(month_cols[7:12])))) %>%
  select(
    `Salaris Schaal`,
    `WP of OBP`,
    Omschrijving,
    `Profit Center aanstelling`,
    Geldstroom,
    Kernactiviteit,
    Jaar,
    Faculteit,
    FTE_gemiddelde,
    FTE_gemiddelde_tot_juni,
    FTE_gemiddelde_vanaf_juli,
  ) %>%
  mutate(
    salaris_schaal = str_extract(`Salaris Schaal`, "[^/]+$"),
    schaal_5_tm_10 = if_else(salaris_schaal %in% c("005", "006", "007", "008", "009", "010"), TRUE, FALSE),
    schaal_11_hoger = if_else(salaris_schaal %in% c("011", "012", "013", "014", "015", "016", "017", "018", "019", "020"), TRUE, FALSE),
    schaal_anders = ifelse(schaal_5_tm_10 == FALSE & schaal_11_hoger == FALSE, TRUE, FALSE),
    schaal_categorie = case_when(
      schaal_5_tm_10 ~ "Schaal 5 t/m 10",
      schaal_11_hoger ~ "Schaal 11 en hoger",
      schaal_anders ~ "Andere schalen"
    ),
    omschrijving_zonder_nummer = str_replace_all(`Omschrijving`, "\\d+", "") %>% str_trim()
  )

## Student-assistent special classification: previously WP, but now OBP
KEK_HRM_WP_Functie_geldstroom <- KEK_HRM_Personeel_prepared %>%
  filter(
    `WP of OBP` == "WP" | omschrijving_zonder_nummer == "Student-assistent",
    Kernactiviteit != "OV"
  ) %>%
  filter(Geldstroom == "Geldstroom 1") %>%
  group_by(omschrijving_zonder_nummer, Kernactiviteit, Jaar, Faculteit) %>%
  summarise(
    FTE_gemiddelde = sum(FTE_gemiddelde),
    .groups = "drop"
  ) %>%
  mutate(Veldnaam = paste(omschrijving_zonder_nummer, Kernactiviteit, sep = " - ")) %>%
  select(Veldnaam, Jaar, Faculteit, FTE_gemiddelde)

KEK_HRM_WP_Functie <- KEK_HRM_Personeel_prepared %>%
  filter(`WP of OBP` == "WP" | omschrijving_zonder_nummer == "Student-assistent") %>%
  group_by(omschrijving_zonder_nummer, Jaar, Faculteit) %>%
  summarise(
    FTE_gemiddelde = sum(FTE_gemiddelde),
    .groups = "drop"
  ) %>%
  rename(Veldnaam = omschrijving_zonder_nummer) %>%
  select(Veldnaam, Jaar, Faculteit, FTE_gemiddelde)

KEK_HRM_OBP <- KEK_HRM_Personeel_prepared %>%
  filter(`WP of OBP` == "OBP" & omschrijving_zonder_nummer != "Student-assistent") %>%
  group_by(schaal_categorie, Jaar, Faculteit, `WP of OBP`) %>%
  summarise(
    FTE_gemiddelde = sum(FTE_gemiddelde),
    .groups = "drop"
  ) %>%
  mutate(Veldnaam = paste(`WP of OBP`, schaal_categorie, sep = " - ")) %>%
  select(Veldnaam, Jaar, Faculteit, FTE_gemiddelde)

## Combine and arrange according KeK
KEK_HRM_compleet <- bind_rows(
  KEK_HRM_WP_Functie,
  KEK_HRM_WP_Functie_geldstroom,
) %>%
  arrange(Jaar, Faculteit, Veldnaam) %>%
  bind_rows(KEK_HRM_OBP) %>%
  arrange(Jaar, Faculteit)

## Data from 2021 * 2 (only data from july)
KEK_HRM_compleet <- KEK_HRM_compleet %>%
  mutate(FTE_gemiddelde = case_when(Jaar == 2021 ~ FTE_gemiddelde * 2,
                                    TRUE ~ FTE_gemiddelde))

## Mutate verslagjaar
KEK_HRM_compleet <- KEK_HRM_compleet %>%
  mutate(unl_verslagjaar = Jaar)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. CHECK ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'* INFO* Extra test om jaar te splitsen
# KEK_HRM_jaar_splitsen <- KEK_HRM_Personeel_prepared %>%
#   filter(`WP of OBP` == "WP" | omschrijving_zonder_nummer == "Student-assistent") %>%
#   group_by(Jaar, Faculteit) %>%
#   summarise(FTE_gemiddelde = sum(FTE_gemiddelde),
#             FTE_gemiddelde_tot_juni = sum(FTE_gemiddelde_tot_juni),
#             FTE_gemiddelde_vanaf_juli = sum(FTE_gemiddelde_vanaf_juli),
#             .groups = "drop")
#
# KEK_HRM_kernactiviteit_jaar_splitsen <- KEK_HRM_Personeel_prepared %>%
#   filter(`WP of OBP` == "WP" | omschrijving_zonder_nummer == "Student-assistent") %>%
#   group_by(Jaar, Faculteit, Kernactiviteit) %>%
#   summarise(FTE_gemiddelde = sum(FTE_gemiddelde),
#             FTE_gemiddelde_tot_juni = sum(FTE_gemiddelde_tot_juni),
#             FTE_gemiddelde_vanaf_juli = sum(FTE_gemiddelde_vanaf_juli),
#             .groups = "drop")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_file_proj(KEK_HRM_compleet, "KEK_HRM_compleet")

clear_script_objects()
