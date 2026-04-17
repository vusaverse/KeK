#' Connect to Azure Storage Container
#'
#' Creates a connection to an Azure Storage container using SAS token authentication.
#'
#' @param endpoint_url Character string. The Azure Storage endpoint URL.
#' @param container_name Character string. Name of the container to connect to.
#'
#' @return An AzureStor container object for blob operations.
#'
#' @details
#' This function uses the SAS_TOKEN environment variable for authentication.
#' Ensure the token is set before calling this function.
#'
#' @examples
#' \dontrun{
#' endpoint <- "https://mystorageaccount.blob.core.windows.net"
#' container <- connect_to_azure_container(endpoint, "mycontainer")
#' }
#'
#' @export
.connect_to_azure_container <- function(endpoint_url,
                                       container_name) {
  # Validate inputs
  if (is.null(endpoint_url) || endpoint_url == "") {
    stop("endpoint_url cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(container_name) || container_name == "") {
    stop("container_name cannot be NULL or empty.", call. = FALSE)
  }

  sas_token <- Sys.getenv("SAS_TOKEN")
  if (sas_token == "") {
    stop("SAS_TOKEN environment variable is not set. Please set it using Sys.setenv(SAS_TOKEN = 'your_token').", call. = FALSE)
  }

  tryCatch(
    {
      bl_endp_sas <- AzureStor::storage_endpoint(endpoint_url, sas = sas_token)
      cont <- AzureStor::storage_container(bl_endp_sas, container_name)
      return(cont)
    },
    error = function(e) {
      stop("Failed to connect to Azure container '", container_name, "': ", e$message, call. = FALSE)
    }
  )
}

#' Create Azure Storage Endpoint URL
#'
#' Constructs a properly formatted Azure Storage endpoint URL from a storage account name.
#'
#' @param storage_account Character string. The name of the Azure Storage account.
#'
#' @return Character string. The complete Azure Storage endpoint URL.
#'
#' @examples
#' create_endpoint_url("mystorageaccount")
#' # Returns: "https://mystorageaccount.blob.core.windows.net"
#'
.create_endpoint_url <- function(storage_account) {
  if (is.null(storage_account) || storage_account == "") {
    stop("storage_account cannot be NULL or empty.", call. = FALSE)
  }
  endpoint_url <- sprintf("https://%s.blob.core.windows.net", storage_account)
  return(endpoint_url)
}
