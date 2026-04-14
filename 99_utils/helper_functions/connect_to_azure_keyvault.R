#' Connect to Azure Key Vault
#'
#' Establishes a connection to the Azure Key Vault at the specified URL.
#'
#' @param vault_url A character string specifying the URL of the Azure Key Vault.
#' @return An object representing the Azure Key Vault connection.
#' @importFrom AzureKeyVault key_vault
#' @examples
#' vault <- connect_to_azure_keyvault(vault_url)
.connect_to_azure_keyvault <- function(vault_url) {
  vault <- AzureKeyVault::key_vault(vault_url)
  tryCatch({
    vault <- AzureKeyVault::key_vault(
      vault_url,
      auth_type = "device_code"
    )
    ## Check if the test key can be accessed
    vault$secrets$get("test")
  }, error = function(e) {
    ## stop the pipeline to prevent dataloss, signal the admin to check authorization
    .handle_connection_error(e)
    stop(e)
  })
  return(vault)
}

#' Handle Key Vault connection errors
#'
#' Sends a Key Vault connection error message to Slack in production
#' or prints it to the console in other environments.
#'
#' @details
#' Expects an error object \code{error} with a \code{message} field in scope.
#'
#' @return
#' No return value; called for side effects.
#'
.handle_connection_error <- function(error){
  if (tolower(Sys.getenv("ENVIRONMENT", "dev") == "prod")) {
    slack_vm(paste0("education-analytics Key Vault niet bereikbaar. Zorg ervoor dat je op de VM authenticeert. Dit doe je door op de VM in te loggen, Voorbereidingen te runnen en in het openspringende scherm eventuele stappen te volgen om te authenticeren: ", error$message))
  } else{
    message(paste0("education-analytics Key Vault niet bereikbaar: ", error$message))
  }
}
