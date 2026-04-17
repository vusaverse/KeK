#' Set a Secret in Azure Key Vault
#'
#' Stores a secret in the connected Azure Key Vault.
#'
#' @param key A character string specifying the name of the secret.
#' @param value A character string specifying the value to store.
#'
#' @return Invisibly returns the result of the secret creation operation.
#' @details Assumes that a connection to the Azure Key Vault is available via the `.vault` object.
#' @examples
#' set_azure_keyvault_secret("my-secret", "supersecretvalue")
.set_azure_keyvault_secret <- function(key, value) {
  .vault$secrets$create(key, value)
}
