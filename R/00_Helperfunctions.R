## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Helperfunctions
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##' *INFO*
##' LET OP endpoints unl_[naam]+s
get_kek_data <- function(endpoint, token = Sys.getenv("KEK_ACCESS_TOKEN")) {
  # List of valid endpoints
  valid_endpoints <- c(
    "collegejaars", "faculteitpersoneels", "faculteitfinanciens", "faculteitstudentens",
    "normenpertoetsingvorms", "normenperwerkvorms", "faculteits", "opleidings",
    "opleidingpersoneels", "opleidingstudentens", "vaks", "vakrangenpertoetsingvorms",
    "vakrangenperwerkvorms", "vakwerkvorms", "universiteits", "kalenderjaars", "rangenlijsts"
  )

  # Check if the endpoint is valid
  if (!endpoint %in% valid_endpoints) {
    stop(paste(
      "Invalid endpoint:", endpoint,
      "\nValid endpoints are:", paste(valid_endpoints, collapse = ", ")
    ))
  }

  # Construct the URL
  # Remove test in URL for production
  base_url <- "https://kekunltest.api.crm4.dynamics.com/api/data/v9.2/"
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
    as.data.frame() %>%
    select(starts_with("value.unl_")) %>%
    rename_with(~ str_remove(., "value."))

  return(df)
}


# Function to replace NA with NULL
replace_na_with_null <- function(x) {
  if (is.list(x)) {
    return(lapply(x, replace_na_with_null))
  } else if (is.na(x)) {
    return(NULL)
  } else {
    return(x)
  }
}

send_data_to_kek <- function(data, endpoint, access_token = Sys.getenv("KEK_ACCESS_TOKEN")) {
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

  # Construct the URL
  # Remove test in URL for production
  base_url <- "https://kekunltest.api.crm4.dynamics.com/api/data/v9.2/"
  url <- paste0(base_url, "unl_", endpoint)

  # Set up headers
  headers <- add_headers(
    "Content-Type" = "application/json",
    "OData-MaxVersion" = "4.0",
    "OData-Version" = "4.0",
    "Authorization" = paste("Bearer", access_token),
    "Prefer" = "return=representation",
    "Accept" = "IEEE754Compatible=true"
  )



  # Sample the data and send to API
  results <- data %>%
    rowwise() %>%
    mutate(api_result = list({
      row_data <- as.list(cur_data()) %>%
        replace_na_with_null()

      tryCatch(
        {
          response <- POST(
            url,
            headers,
            body = row_data,
            encode = "json"
          )

          list(
            status = status_code(response),
            content = content(response, "text")
          )
        },
        error = function(e) {
          list(
            status = "Error",
            content = as.character(e)
          )
        }
      )
    })) %>%
    ungroup() %>%
    pull(api_result)

  # Optional: Add a small delay between requests
  Sys.sleep(0.5)

  return(results)
}



# Function to clean faculty names
clean_faculty_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_remove("^vu\\s*-\\s*") %>%
    str_trim() %>%
    str_replace("^acta$", "thk")
}


convert_to_academic_year <- function(year) {
  paste0(year, "-", year + 1)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Function to extract weeks
unpack_weeks <- function(data) {
  column_name <- "week"
  # Check if the column exists in the data frame
  if (!(column_name %in% colnames(data))) {
    paste("Column", column_name, "does not exist in the data frame.")
  }
  # Extract the column values
  column_values <- data[[column_name]]
  # Return the vector of column values
  return(column_values)
}

## Function to extract duration
unpack_duration <- function(data) {
  column_name <- "activityDurationMins"
  # Check if the column exists in the data frame
  if (!(column_name %in% colnames(data))) {
    paste("Column", column_name, "does not exist in the data frame.")
  }
  # Extract the column values
  column_values <- data[[column_name]]
  # Return the vector of column values
  return(column_values)
}

