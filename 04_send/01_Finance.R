## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Get finance export (export Manipuleren Finance Profit Loss.R)
dfKEK_FIN <- read_file_proj("KEK_FIN_compleet")

## Get Finance data
dfFac_Finance <- get_kek_data("faculteitfinanciens")

## Read documentation
KeK_FAC_Finance_naming <- read_documentation(
  "Documentatie_KeK_FAC_Finance_API.csv"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EDIT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfKEK_FIN_pivot <- dfKEK_FIN %>%
  select(-Naam_op_VU) %>%
  pivot_wider(
    names_from = Veldnaam,
    values_from = Waarde
  )

## Vervang NA voor 0 (anders geen totaal in DEA)
dfKEK_FIN_pivot <- dfKEK_FIN_pivot %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

## Pas kolomnamen aan zodat deze overeenkomen met KeK data entry app
dfKEK_FIN_data_entry_app <- dfKEK_FIN_pivot %>%
  wrapper_translate_colnames_documentation(KeK_FAC_Finance_naming)

## Testing filter
# dfKEK_FIN_data_entry_app <- dfKEK_FIN_data_entry_app %>% 
#   filter(unl_faculteit == "ACTA")

## TODO: afronden?
## Alleen 1 decimaal bij volgende variabelen van dfFIN_SBE_data_entry_app:
## unl_overigepersonelelasten
## unl_afschrijvingslasten
## unl_huisvestingslasten
## unl_bijdragenensubsidies

# names(dfKEK_FIN_data_entry_app)
# [1] "unl_faculteit"                       "unl_jaar"
# [3] "unl_budget1egeldstroom"              "unl_bateninternedienstverlening"
# [5] "unl_collegegelden"                   "unl_onderzoek2egeldstroom"
# [7] "unl_onderzoek3egeldstroom"           "unl_contractonderwijs"
# [9] "unl_overigeexternebaten"             "unl_personelelasteneigenpersoneel"
# [11] "unl_overigepersonelelasten"          "unl_externeinhuur"
# [13] "unl_afschrijvingslasten"             "unl_huisvestingslasten"
# [15] "unl_bijdragenensubsidies"            "unl_doorbelastingcentralediensten"
# [17] "unl_kostenonderlingedienstverlening"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. LOOKUP VARIABLES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lookup variables in table_summary tab Fac (unl_FaculteitFinancien):
# unl_faculteit = faculteit naam | Targets:unl_faculteit
# unl_faculteitfinancienvorigjaar = Faculteit Financiën Vorig Jaar | Targets:unl_faculteitfinancien
# unl_jaar = Boekjaar | Targets:unl_kalenderjaar

### Faculteit ####
## unl_faculteit = faculteit naam
## Targets:unl_faculteit

dffaculteit <- get_kek_data("faculteits")


# Clean faculty names in both datasets
dffaculteit <- dffaculteit %>%
  mutate(clean_name = clean_faculty_name(unl_afkortingfaculteit))

dfKEK_FIN_data_entry_app <- dfKEK_FIN_data_entry_app %>%
  mutate(clean_faculteit = clean_faculty_name(unl_faculteit))

# Add the new column to dfFIN_SBE_data_entry_app
dfKEK_FIN_data_entry_app <- dfKEK_FIN_data_entry_app %>%
  mutate(`unl_Faculteit@odata.bind` = sapply(clean_faculteit, function(faculty) {
    match <- dffaculteit %>%
      filter(clean_name == faculty | unl_afkortingfaculteit == faculty) %>%
      pull(unl_faculteitid)

    if (length(match) > 0) {
      paste0("unl_faculteits(", match, ")")
    } else {
      NA_character_
    }
  })) %>%
  select(-clean_faculteit, -unl_faculteit)

### Faculteit financiën vorig jaar ####
## unl_faculteitfinancienvorigjaar = Faculteit Financiën Vorig Jaar
## Targets:unl_faculteitfinancien

## TODO: Nog toevoegen? Na overleg met Judith voor nu overgeslagen
## Niet verplicht in test-omgeving KeK?

### Jaar ####
dfkalenderjaar <- get_kek_data("kalenderjaars")

# Add the new column to dfFIN_SBE_data_entry_app
dfKEK_FIN_data_entry_app <- dfKEK_FIN_data_entry_app %>%
  mutate(
    `unl_Jaar@odata.bind` = sapply(unl_jaar, function(year) {
      match <- dfkalenderjaar %>%
        filter(unl_name == year) %>%
        pull(unl_kalenderjaarid)

      if (length(match) > 0) {
        paste0("unl_kalenderjaars(", match, ")")
      } else {
        NA_character_
      }
    })
  )

## Lege variabelen toevoegen (anders geen totale berekeningen in DEA)
dfKEK_FIN_data_entry_app <- dfKEK_FIN_data_entry_app %>% 
  mutate(unl_onderzoek4egeldstroom = 0,
         unl_aanvullendesubsidies = 0)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Send POST ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
dfKEK_FIN_data_entry_app2 <- dfKEK_FIN_data_entry_app %>%
  select(-unl_jaar)

bbb <- send_data_to_kek(
  dfKEK_FIN_data_entry_app2,
  "faculteitfinanciens"
)

clear_script_objects()
