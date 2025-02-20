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
## Get list of all files regarding KeK finance
lFIN_files <- list.files(
  paste0(
    Sys.getenv("RAW_DATA_DIR"),
    "KeK/FIN/"
  ),
  # pattern = "PL_PnC_FINANCIELE_PLANNING SBE.*.csv", ## Testing
  pattern = "PL_PnC_FINANCIELE_PLANNING .*.csv",
  full.names = TRUE
)

## Make column names
col_names <- c(
  "Exploitatieresultaat",
  "Categorie",
  "Subcategorie",
  "Toewijzing",
  "YTD Realisatie vorig jaar",
  "YTD Realisatie",
  "YTD Realisatie - Begroting",
  "Realisatie vorig jaar"
)

## Read and combine all files
dfKEK_FIN <- map_df(
  lFIN_files,
  ~ read_csv(
    .x,
    skip = 4, ## Empty/rows with filter info
    col_names = col_names,
    col_types = "ccccnnnn" ## Change finance columns from double to number
  ) %>%
    ## Make variables Jaar and Faculteit using file name
    mutate(
      Jaar = str_extract(basename(.x), "(\\d{4}\\.csv)") %>%
        parse_number(),
      Faculteit = str_extract(basename(.x), "\\b[A-Za-z]{3,4}\\b")
    )
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


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EDIT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO: Change column names using the documentation
# Dataset <- Dataset %>% wrapper_translate_colnames_documentation(Dataset_naming)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_file_proj(dfKEK_FIN, "KEK_FIN")

clear_script_objects()
