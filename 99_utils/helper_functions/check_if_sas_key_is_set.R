#' Check if SAS Token is Set and Prompt if Missing
#'
#' This function verifies that the SAS_TOKEN environment variable is set.
#' If running interactively and the token is missing, it prompts the user
#' to enter a valid SAS token. If not interactive, it stops with an error.
#'
#' @return Invisible NULL if SAS token is valid, otherwise stops with error
#'
#' @details
#' This function is used throughout the ETL pipeline to ensure Azure Storage
#' authentication is available before attempting blob operations.
#'
#' @examples
#' \dontrun{
#' # Check if SAS token is set before proceeding
#' check_if_sas_key_is_set()
#' }
#'
#' @export
.check_if_sas_key_is_set <- function() {
  # First check if R studio has a key
  if (Sys.getenv("username") == .get_azure_keyvault_secret(.config$secrets$azure$server_name)) {
    Sys.setenv("SAS_TOKEN" =  .get_azure_keyvault_secret(.config$secrets$azure$sas_token))
  }

  # Secondly, check if user needs to input a token or if token is expired
  if (Sys.getenv("SAS_TOKEN") == "") {
    .ask_user_to_provide_sas_token()
  } else if (.check_if_token_is_expired(Sys.getenv("SAS_TOKEN"))) {
    .ask_user_to_provide_sas_token()
  }
}

#' Check if a SAS token is expired
#'
#' Extracts the expiration timestamp from a Shared Access Signature (SAS) token and compares it
#' to the current time to determine if the token has expired. If the token is invalid or an error
#' occurs during processing, it is treated as expired.
#'
#' @param SAS_TOKEN A character string containing the SAS token, including the `se` (expiry) parameter.
#'
#' @return Logical `TRUE` if the token is expired or invalid, `FALSE` if it is still valid.
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd_hms now
.check_if_token_is_expired <- function(SAS_TOKEN) {
  tryCatch(
    {
     # Fetch the SE parameter from the SAS_TOKEN, containing the expiration date and time
     se_parameter <- stringr::str_extract(SAS_TOKEN, "(?<=se=)[^&]+")
     if (se_parameter == "") {
       message("The SAS_Token does not contain an enddate, provide a working SAS_Token Please do so by creating a new one in
             the Azure storage environment and adding it as a system variable using Sys.setenv(SAS_TOKEN = '')")
       return(TRUE)
     }

     # Extract the time to a lubridate ready format
     exp_time <- lubridate::ymd_hms(se_parameter, tz = "UTC")
     # Check if the SAS_Token is expired
     is_expired <- exp_time < lubridate::now(tzone = "UTC")
     if (is_expired) {
       message("The SAS_Token is expired or not working, generate a new one. Please do so by creating a new one in
             the Azure storage environment and adding it as a system variable using Sys.setenv(SAS_TOKEN = '')")
       return(TRUE)
     } else {
       return(FALSE)
     }
    },
    error = function(e) {
      print(e)
      message("The SAS_Token is expired or not working, generate a new one. Please do so by creating a new one in
             the Azure storage environment and adding it as a system variable using Sys.setenv(SAS_TOKEN = '')")
      return(TRUE)
    }
  )
}

#' Prompt User to Provide a SAS Token
#'
#' Interactively prompts the user to enter a SAS token for Azure Blob Storage and sets it as a system environment variable.
#' Validates that the token is not empty and stops execution if it is invalid or missing.
#'
#' @return No return value. Sets the `SAS_TOKEN` environment variable if successful; otherwise, stops with an error.
#'
#' @importFrom rstudioapi showPrompt
#' @export
.ask_user_to_provide_sas_token <- function() {
  if (interactive()) {
    # Prompt the user to add a valid SAS_TOKEN to env
    SAS_TOKEN <- rstudioapi::showPrompt(
      title = "Please generate a valid SAS Token in Azure",
      message = "SAS Token:",
      default = ""
    )
    tryCatch(
      {
        Sys.setenv("SAS_TOKEN" = SAS_TOKEN)
      },
      error = function(e) {
        stop("The SAS_Token is empty or needs to be set. Please do so by creating a new one in
             the Azure storage environment and adding it as a system variable using Sys.setenv(SAS_TOKEN = '')")
      }
    )

    # Check if user input is valid
    if (Sys.getenv("SAS_TOKEN") == "") {
      stop("The SAS_Token is empty or needs to be set. Please do so by creating a new one in
             the Azure storage environment and adding it as a system variable using Sys.setenv(SAS_TOKEN = '')")
    }
  }
}
