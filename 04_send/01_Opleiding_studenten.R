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

dfOPL_Studenten <- read_file_proj("KEK_Opleiding_studenten", dir = "1. Ingelezen data/")

dfKeK_OPL_studenten <- get_kek_data(endpoint = "opleidingstudentens")


## Lees documentatie in
KeK_OPL_studenten_naming <- read_documentation(
  "Documentatie_KeK_OPL_studenten_API.csv"
)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. opleiding ####
## list of columns that are lookup in opleiding_studenten:
# unl_graad
# unl_opleiding

## not per year??
##' *TODO*
##' No year in endpoint

##' *INFO*
##' get_kek_data aanpassen want jaar wordt niet opgehaald met eerdere methode


##' *INFO*
##' LET OP endpoints unl_[naam]+s
get_kek_data2 <- function(endpoint, token = Sys.getenv("KEK_ACCESS_TOKEN")) {
  # List of valid endpoints
  valid_endpoints <- c(
    "collegejaars", "faculteitpersoneels", "faculteitfinanciens", "faculteitstudentens",
    "normenpertoetsingvorms", "normenperwerkvorms", "faculteits", "opleidings",
    "opleidingpersoneels", "opleidingstudentens", "vaks", "vakrangenpertoetsingvorms",
    "vakrangenperwerkvorms", "vakwerkvorms", "universiteits"
  )

  # Check if the endpoint is valid
  if (!endpoint %in% valid_endpoints) {
    stop(paste(
      "Invalid endpoint:", endpoint,
      "\nValid endpoints are:", paste(valid_endpoints, collapse = ", ")
    ))
  }
  
  if (!sKEK_environment %in% c("prod", "test")) {
    stop("sKEK_environment must be either 'prod' or 'test'")
  }
  
  if (sKEK_environment == "prod") {
    base_url <- "https://kekunl.api.crm4.dynamics.com/api/data/v9.2/"
  } else {
    base_url <- "https://kekunltest.api.crm4.dynamics.com/api/data/v9.2/"
  }

  # Construct the URL
  url <- paste0(base_url, "unl_", endpoint)

  # Make the GET request
  response <- httr::GET(
    url = url,
    httr::add_headers("Authorization" = paste("Bearer", token))
  )

  # Check for HTTP errors
  httr::stop_for_status(response)

  # Process the response
  df <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON() %>%
    as.data.frame()

  return(df)
}

dfopleidings <- get_kek_data2("opleidings")

dfcollegejaars <- get_kek_data("collegejaars")

dfopleidings2 <- dfopleidings %>%
  left_join(dfcollegejaars, by = c("value._unl_collegejaar_value" = "unl_collegejaarid"))

dfOPL_Studenten2 <- dfOPL_Studenten %>%
  mutate(
    academic_year = convert_to_academic_year(INS_Inschrijvingsjaar),
    `unl_Opleiding@odata.bind` = sapply(seq_len(nrow(.)), function(i) {
      year <- academic_year[i]
      code <- as.character(INS_Opleidingscode_actueel[i])

      match <- dfopleidings2 %>%
        filter(unl_name == year, value.unl_opleidingscodeisat == code) %>%
        slice(1) %>% # This line ensures only the first match is selected
        pull(value.unl_opleidingid)

      if (length(match) > 0) {
        paste0("unl_opleidings(", match, ")")
      } else {
        NA_character_
      }
    })
  ) %>%
  select(
    -academic_year,
    -INS_Inschrijvingsjaar,
    -INS_Opleidingscode_actueel
  )


dfOPL_Studenten2 <- dfOPL_Studenten2 %>%
  wrapper_translate_colnames_documentation(KeK_OPL_studenten_naming)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Send data to KeK ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfOPL_Studenten_test <- dfOPL_Studenten2 %>%
  filter(!is.na(`unl_Opleiding@odata.bind`))


bbb <- send_data_to_kek(dfOPL_Studenten_test, "opleidingstudentens")

clear_script_objects()
