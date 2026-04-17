#' Read Documentation from Azure Blob Storage
#'
#' Downloads a CSV file from the 'documentation' container in Azure Blob Storage
#' and reads it into R as a data.frame.
#'
#' @param file_name Character string. Name of the documentation file in the Azure container.
#'
#' @return A data.frame containing the contents of the documentation file.
#'
#' @details
#' This function creates a temporary connection to the 'documentation' container,
#' downloads the specified file to a temporary location, reads it as a CSV with
#' semicolon separator, and returns the resulting data.frame.
#'
#' @examples
#' \dontrun{
#' # Read dataset documentation
#' doc <- read_documentation_from_azure_blob_storage("mtcars_enriched.csv")
#'
#' # Read ETL export log
#' log <- read_documentation_from_azure_blob_storage("etl_export_log.csv")
#' }
#'
#' @export
.read_documentation_from_azure_blob_storage <- function(file_name) {
  # Validate inputs
  if (is.null(file_name) || file_name == "") {
    stop("file_name cannot be NULL or empty.", call. = FALSE)
  }

  # Check if storage_account is available
  if (!exists("storage_account") || is.null(storage_account) || storage_account == "") {
    stop("storage_account must be defined and non-empty.", call. = FALSE)
  }

  tryCatch(
    {
      ## Create a temporary container, to access the documentation
      cont_documentation <- .connect_to_azure_container(
        .create_endpoint_url(storage_account),
        "documentation"
      )
      ## Download the documentation file to temp
      temp_file_path <- download_to_temp(cont_documentation, file_name)
      ## Read the documentation
      dfDocumentation <- read.csv2(temp_file_path, stringsAsFactors = FALSE)

      if (is.null(dfDocumentation) || nrow(dfDocumentation) == 0) {
        stop("Documentation file '", file_name, "' is empty or could not be read.", call. = FALSE)
      }

      return(dfDocumentation)
    },
    error = function(e) {
      stop("Failed to read documentation file '", file_name, "' from Azure: ", e$message, call. = FALSE)
    }
  )
}
