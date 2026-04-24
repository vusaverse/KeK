

dfTT_data_entry_app <- load_rds_from_azure(
  config = .config,
  container_type = "finance",
  file_name = "KeK/Termtime_vakdata.rds"
)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Get api data ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfcollegejaars <- get_kek_data("collegejaars")
dfopleidings <- get_kek_data("opleidings")
dfvaks <-  get_kek_data("vaks")

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
dfTT_data_entry_app <- dfTT_data_entry_app %>%
  mutate(
    `unl_Jaar@odata.bind` = sapply(unl_jaar, function(year) {
      match <- dfcollegejaars %>%
        filter(unl_name == year) %>%
        pull(unl_collegejaarid)
      
      if (length(match) > 0) {
        paste0("unl_collegejaars(", match, ")")
      } else {
        NA_character_
      }
    })
  )


## extract unl_jaar column from  unl_name  , example: ""M Computational Science (joint degree) 2019-2020" --> "2019-2020" 
dfopleidings_enriched <- dfopleidings %>% 
  mutate(
    unl_jaar = str_extract(unl_name, "\\d{4}-\\d{4}$")
  ) %>% 
  filter(unl_jaar %in% c(
    "2021-2022",
    "2022-2023",
    "2023-2024",
    "2024-2025",
    "2025-2026"
  )  
  )



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
##' Update: removing start date and geslaagden requirements loses a lot less
##' 
dfTT_data_entry_app2 <- dfTT_data_entry_app2 %>%
  dplyr::group_by(unl_vakcode) %>%
  dplyr::mutate(
    `unl_Opleiding@odata.bind` =
      dplyr::coalesce(`unl_Opleiding@odata.bind`,
                      first(na.omit(`unl_Opleiding@odata.bind`)))
  ) %>%
  dplyr::ungroup() %>%
  #dplyr::filter(!is.na(unl_startdatum)) %>%
  #dplyr::filter(!is.na(unl_aantalgeslaagdeneindtoets)) #%>% 
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

## add vak guid obtained from unl DEA via api
dfUNLvak_guid <- dfvaks %>% 
  dplyr::select(
    unl_vakcode,
    unl_vakid,
    `_unl_jaar_value`
  ) %>% 
  dplyr::distinct(
    unl_vakcode,
    `_unl_jaar_value`,
    .keep_all = TRUE
  )

dfcollegejaars <- dfcollegejaars  %>% 
  dplyr::select(unl_collegejaarid, unl_name) %>% 
  dplyr::rename("unl_jaar" = unl_name )

## Add year through id to be able to match vak id to correct module/year combination
dfUNLvak_guid <- dfUNLvak_guid %>% 
  dplyr::left_join(
    dfcollegejaars,
    by = c(
      "_unl_jaar_value" = "unl_collegejaarid"
    ),
    relationship = "many-to-one"
  )

## add vak id
dfTT_data_entry_app2 <- dfTT_data_entry_app2 %>%
  dplyr::left_join(
    dfUNLvak_guid,
    by = c("unl_vakcode", "unl_jaar"),
    relationship = "many-to-one"
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