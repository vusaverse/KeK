#' Get Azure Container Connection and List Blobs
#'
#' Establishes a connection to an Azure Storage container and returns a list of all blobs
#' within that container. This is a utility function commonly used across ETL pipelines.
#'
#' @param config List. Configuration object containing Azure storage settings,
#'   including storage account name and container names.
#' @param container_type Character string. Type of container to connect to
#'   (default: "raw"). Must correspond to a key in config$azure_storage.
#' @param env Character string. Environment name (default: "dev").
#'   Currently not used but maintained for consistency.
#' @param sas_token Character string. SAS token for authentication.
#'   Defaults to the SAS_TOKEN environment variable.
#'
#' @return A data.frame/tibble containing blob information from AzureStor::list_blobs().
#'
#' @details
#' The function dynamically constructs the container key by appending "_container"
#' to the container_type parameter. For example, container_type = "raw" looks for
#' config$azure_storage$raw_container.
#'
#' @examples
#' \dontrun{
#' config <- .load_config()
#' blobs <- get_azure_container_and_blobs(config, container_type = "bronze")
#' }
#'
#' @export
get_azure_container_and_blobs <- function(config,
                                          container_type = "raw",
                                          env = "dev",
                                          sas_token = Sys.getenv("SAS_TOKEN")) {
  # Validate inputs
  if (is.null(config) || !is.list(config)) {
    stop("config must be a non-NULL list object.", call. = FALSE)
  }
  if (is.null(container_type) || container_type == "") {
    stop("container_type cannot be NULL or empty.", call. = FALSE)
  }

  # Give a graceful error message when the sas_token is empty
  .check_if_sas_key_is_set()

  # Map container_type to the correct config key
  container_key <- paste0(container_type, "_container")

  # Extract storage account and container name dynamically
  if (is.null(config$azure_storage)) {
    stop("config$azure_storage is missing from configuration.", call. = FALSE)
  }
  if (is.null(config$azure_storage$account) || config$azure_storage$account == "") {
    stop("config$azure_storage$account is missing or empty.", call. = FALSE)
  }

  storage_account <- config$azure_storage$account
  if (!container_key %in% names(config$azure_storage)) {
    stop("Container type '", container_type, "' not found in config. Available types: ",
      paste(grep("_container$", names(config$azure_storage), value = TRUE), collapse = ", "),
      call. = FALSE
    )
  }
  container_name <- config$azure_storage[[container_key]]

  # Build endpoint URL
  endpoint_url <- sprintf("https://%s.blob.core.windows.net", storage_account)

  tryCatch(
    {
      # Create endpoint and container objects
      bl_endp_sas <- AzureStor::storage_endpoint(endpoint_url, sas = sas_token)
      cont <- AzureStor::blob_container(bl_endp_sas, container_name)

      # List blobs in the container
      files_s <- AzureStor::list_blobs(cont)
      if (is.null(files_s)) {
        stop("Failed to list blobs in container '", container_name, "'.", call. = FALSE)
      }
      return(files_s)
    },
    error = function(e) {
      stop("Failed to connect to Azure container '", container_name, "': ", e$message, call. = FALSE)
    }
  )
}
