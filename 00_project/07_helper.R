## Source all helper functions in the designated directory
helper_functions <- "99_utils/helper_functions/"
files <- list.files(helper_functions, pattern = "\\.R$", full.names = TRUE) %>%
  purrr::walk(source)


vusa::clear_script_objects()
