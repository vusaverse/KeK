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

# Define the directories in the order you want
directories <- c(
  "01_read",
  "02_prepare",
  "03_validate",
  "04_send"
)

# Create the Scriptvalidatie data frame
Scriptvalidatie <- map_dfr(directories, ~ tibble(
  Directory = .x,
  Bestandsnaam = list.files(path = .x, full.names = FALSE),
  Bestandspad = file.path(.x, Bestandsnaam)
)) %>%
  arrange(Directory, Bestandsnaam)

# Validate all scripts
Scriptvalidatie <- Scriptvalidatie %>%
  mutate(validatie_df = map(Bestandspad, safely(~ vusa::validate_script_proj(.x, TRUE))))

# If you want to unnest the results (be careful, this might produce a very wide dataframe)
Scriptvalidatie_unnested <- Scriptvalidatie %>%
  mutate(validatie_result = map(validatie_df, "result")) %>%
  select(-validatie_df) %>%
  unnest(validatie_result, names_sep = "_")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Check if there are any errors or warnings

dfScriptkwaliteit_failed <- Scriptvalidatie_unnested %>%
  filter(validatie_result_error_or_warning) %>%
  select(Bestandsnaam, 
         validatie_result_warning_message, 
         validatie_result_error_message, 
         validatie_result_error_or_warning) %>%
  mutate(
    validatie_result_error_message = if (all(is.na(validatie_result_error_message))) as.character(validatie_result_error_message) else
      case_when(
        grepl("Assertion", as.character(validatie_result_error_message)) ~ "Assertion error",
        TRUE ~ validatie_result_error_message
      )
  ) %>%
  drop_na() %>%
  mutate(truncate_message = map_chr(strsplit(validatie_result_error_message, "\\n"), 1))

# Send a slack message if there are any errors
slackr::slackr(cat(paste("The following scripts contain errors: for the full error message see dfScriptkwaliteit_failed$error_message in Check_scriptkwaliteit.R"),
                   paste0(
                     "[", dfScriptkwaliteit_failed$Bestandsnaam, "] \n\t",
                     dfScriptkwaliteit_failed$truncate_message
                   ),
                   sep = "\n"
))

# Check if there are any style issues
dfScriptkwaliteit_failed_style <- Scriptvalidatie_unnested %>%
  filter(validatie_result_style_issues) %>%
  select(Bestandsnaam, validatie_result_style, validatie_result_execution_time) %>%
  drop_na()


