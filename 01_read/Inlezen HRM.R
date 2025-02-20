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

## Get list of all files regarding KeK HRM
lHRM_files <- list.files(
  paste0(
    Sys.getenv("RAW_DATA_DIR"),
    "KeK/HRM/"
  ),
  # pattern = "PL_PnC_PERSONEEL_PROGNOSE_FINAL 2021 fac*.csv", ## Testing
  pattern = "PL_PnC_PERSONEEL_PROGNOSE_FINAL .*.csv",
  full.names = TRUE
)


## Make column names
col_names <- c(
  "Medewerker",
  "UFO profiel",
  "Salaris Schaal",
  "WP of OBP",
  "Omschrijving",
  "Schaaltrede",
  "Profit Center aanstelling",
  "Loonkpl",
  "MEASURES",
  "Ontv. Profit Center",
  "Ontv. Kostendrager",
  "Geldstroom",
  "Kernactiviteit",
  "Jaar_data",
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec"
)

## Read and combine all files
dfKEK_HRM <- map_df(
  lHRM_files,
  ~ read_csv(
    .x,
    skip = 5, ## Empty/rows with filter info
    col_names = col_names,
    # col_types = "ccccnnnn" ## TODO: checken
  ) %>%
    ## Make variable Jaar using file name
    mutate(
      Jaar = parse_number(basename(.x))
    ) %>%
    ## Rename Jaar_data to year of file
    rename(!!as.character(parse_number(basename(.x))) := Jaar_data)
)

## Read the naming file in
## TODO: Create the documentation file
# Dataset_naming <- read_documentation("Documentatie_Dataset.csv")


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## TODO: Perform up-to-date check

# up_to_date(bestandspad = Dataset,
#             frequentie = ,
#             contact = "",
#             inleesscript = Dataset)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# assert_naming(Dataset, Dataset_naming, "Dataset")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Ceate a documentation file, if running for the first time
## create_documentation(Dataset, "Dataset")
write_file_proj(dfKEK_HRM, "KEK_HRM")

clear_script_objects()
