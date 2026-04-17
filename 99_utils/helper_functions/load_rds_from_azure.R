#' Load an RDS Object from Azure Storage
#'
#' Loads an RDS object from a specified Azure Storage container using configuration settings.
#' This is a core utility function for retrieving processed data from different storage layers.
#'
#' @param config List. Configuration object containing Azure storage account and container information.
#' @param container_type Character string. Container type identifier (e.g., "silver", "gold", "bronze").
#'   Must correspond to a key in config$azure_storage.
#' @param file_name Character string. Name or path of the RDS file in the container.
#' @param sas_token Character string. SAS token for authentication. Defaults to SAS_TOKEN environment variable.
#'
#' @return The loaded R object from the RDS file.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Constructs the container key by appending "_container" to container_type
#'   \item Validates that the container type exists in the configuration
#'   \item Creates an authenticated connection to Azure Storage
#'   \item Loads and returns the RDS object
#' }
#'
#' @examples
#' \dontrun{
#' config <- .load_config()
#' data <- load_rds_from_azure(config, "bronze", "mtcars_enriched.rds")
#' }
#'
#' @export
load_rds_from_azure <- function(config, container_type, file_name, sas_token = Sys.getenv("SAS_TOKEN")) {
  # Validate inputs
  if (is.null(config) || !is.list(config)) {
    stop("config must be a non-NULL list object.", call. = FALSE)
  }
  if (is.null(container_type) || container_type == "") {
    stop("container_type cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(file_name) || file_name == "") {
    stop("file_name cannot be NULL or empty.", call. = FALSE)
  }
  if (sas_token == "") {
    stop("SAS_TOKEN environment variable is not set.", call. = FALSE)
  }

  # Build container key and extract info
  container_key <- paste0(container_type, "_container")

  if (is.null(config$azure_storage)) {
    stop("config$azure_storage is missing from configuration.", call. = FALSE)
  }
  if (is.null(config$azure_storage$account) || config$azure_storage$account == "") {
    stop("config$azure_storage$account is missing or empty.", call. = FALSE)
  }
  if (!container_key %in% names(config$azure_storage)) {
    stop("Container type '", container_type, "' not found in config.azure_storage. Available types: ",
      paste(grep("_container$", names(config$azure_storage), value = TRUE), collapse = ", "),
      call. = FALSE
    )
  }

  storage_account <- config$azure_storage$account
  container_name <- config$azure_storage[[container_key]]
  endpoint_url <- sprintf("https://%s.blob.core.windows.net", storage_account)

  tryCatch(
    {
      bl_endp_sas <- AzureStor::storage_endpoint(endpoint_url, sas = sas_token)
      cont <- AzureStor::blob_container(bl_endp_sas, container_name)

      # Load the RDS object
      AzureStor::storage_load_rds(cont, file_name)
    },
    error = function(e) {
      stop("Failed to load RDS file '", file_name, "' from Azure container '",
        container_name, "': ", e$message,
        call. = FALSE
      )
    }
  )
}
