## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2025 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Authenticate KeK API
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##' *NOTE*
##' Be careful with changing this variable
## Set environment to test or production
if (!exists("sKEK_environment")) {
  sKEK_environment <- "test"
  # sKEK_environment <- "prod"
}

## Set resource based on environment (test/production)
resource <- case_when(sKEK_environment == "test" ~ "https://kekunltest.api.crm4.dynamics.com",
                      sKEK_environment == "prod" ~ "https://kekunl.api.crm4.dynamics.com",
                      TRUE ~ NA)

authority <- paste0("https://login.microsoftonline.com/", Sys.getenv("KEK_TENANT_ID"), "/oauth2/token")

# Prepare headers and body for the POST request
headers <- add_headers("Content-Type" = "application/x-www-form-urlencoded")
body <- list(
  grant_type = "client_credentials",
  client_id = Sys.getenv("KEK_CLIENT_ID"),
  client_secret = Sys.getenv("KEK_CLIENT_SECRET"),
  resource = resource
)

# Send the POST request
response <- POST(authority, body = body, headers, encode = "form")

# Check if the request was successful
if (status_code(response) == 200) {
  # Extract the access token from the response and set it as a system environment variable
  Sys.setenv(KEK_ACCESS_TOKEN = content(response)$access_token)
  message("Authentication successful. Access token set as environment variable 'KEK_ACCESS_TOKEN'.")
} else {
  message(paste(
    "Authentication failed. Status code:", status_code(response),
    "Error response:", content(response, "text")
  ))
}


clear_script_objects()
