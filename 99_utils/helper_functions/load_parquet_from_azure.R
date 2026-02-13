#' Download and Read a Parquet File from Azure Blob Storage
#'
#' Downloads a Parquet file from an Azure Blob Storage container to a temporary file,
#' reads it into R as a data frame, and then deletes the temporary file.
#'
#' @param file Character string. The path (within the container) to the Parquet file to download.
#' @param config List. Configuration object containing Azure storage account and container information.
#' @param container_type Character. The type of container to use (e.g., "bronze", "silver", "gold"). Default is "bronze".
#' @param col_select Optional. Columns to select when reading the Parquet file (passed to arrow::read_parquet).
#' @param ... Additional arguments passed to arrow::read_parquet.
#'
#' @return A data.frame containing the contents of the Parquet file.
#'
#' @details
#' This function handles the complete process of downloading and reading a Parquet file
#' from Azure Storage, including proper cleanup of temporary files.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   azure_storage = list(
#'     account = "your_storage_account",
#'     bronze_container = "your_container_name"
#'   )
#' )
#' df <- load_parquet_from_azure("path/to/file.parquet", config, col_select = c("col1", "col2"))
# Helper: Download file from Azure Blob to temp, then read as Parquet
#' }
load_parquet_from_azure <- function(file, config, container_type = "bronze", col_select = NULL, ...) {
  # Validate inputs
  if (is.null(file) || file == "") {
    stop("file cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(config) || !is.list(config)) {
    stop("config must be a non-NULL list object.", call. = FALSE)
  }
  if (is.null(container_type) || container_type == "") {
    stop("container_type cannot be NULL or empty.", call. = FALSE)
  }

  container_key <- paste0(container_type, "_container")
  if (is.null(config$azure_storage) || !container_key %in% names(config$azure_storage)) {
    stop("Container type '", container_type, "' not found in config.azure_storage.", call. = FALSE)
  }

  tryCatch(
    {
      # Step 1: Connect to container
      container_info <- get_azure_container_and_blobs(config, container_type = container_type)
      # This often gives you a storage container object and the listing.
      # If not, you might need to explicitly get the container:
      container_name <- config$azure_storage[[container_key]]
      container <- .connect_to_azure_container(
        .create_endpoint_url(config$azure_storage$account),
        container_name
      )

      # Step 2: Download file to temp
      temp_file <- tempfile(fileext = ".parquet")
      AzureStor::storage_download(container, src = file, dest = temp_file, overwrite = TRUE)

      # Verify file was downloaded
      if (!file.exists(temp_file) || file.size(temp_file) == 0) {
        stop("Failed to download or file is empty: ", file, call. = FALSE)
      }

      # Step 3: Read the Parquet file with optional column selection
      df <- arrow::read_parquet(
        file = temp_file,
        col_select = {{ col_select }},
        as_data_frame = TRUE,
        ...
      )

      # Clean up the temporary file
      unlink(temp_file)

      if (is.null(df) || nrow(df) == 0) {
        stop("Parquet file '", file, "' is empty or could not be read.", call. = FALSE)
      }

      return(df)
    },
    error = function(e) {
      stop("Failed to load Parquet file '", file, "' from Azure: ", e$message, call. = FALSE)
    }
  )
}
