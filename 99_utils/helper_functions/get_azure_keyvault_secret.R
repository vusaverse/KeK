#' Retrieve a Secret from Azure Key Vault
#'
#' Retrieves the value of a secret stored in the connected Azure Key Vault.
#'
#' @param key A character string specifying the name of the secret to retrieve.
#'
#' @return A character string containing the value of the retrieved secret.
#' @details Assumes that a connection to the Azure Key Vault is available via the `.vault` object.
#' @examples
#' secret_value <- .get_azure_keyvault_secret("my-secret")
.get_azure_keyvault_secret <- function(key) {
  return(.vault$secrets$get(key)$value)
}
