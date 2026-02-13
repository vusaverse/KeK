#' Log Dataset Execution Time to Azure
#'
#' Updates the ETL export log with the current dataset name and timestamp,
#' and uploads the updated log to Azure Blob Storage.
#'
#' @param dataset_name Character. Name of the dataset being logged.
#'
#' @details Reads the existing export log, removes previous entries for the current dataset,
#'   appends a new row with the current time, and writes the updated log back to Azure.
#'
#' @return None. This function is called for its side effect of updating the log in Azure.
#'
#' @note Requires global variable \code{storage_account} to be set.
#'   Also expects a valid SAS token to be available via the \code{SAS_TOKEN} environment variable.
#'
#' @examples
#' \dontrun{
#' # Log completion of a dataset processing
#' log_run_time_to_azure("mtcars_enriched")
#' }
#'
#' @importFrom dplyr mutate filter add_row
#' @export
.log_run_time_to_azure <- function(dataset_name) {
  # Validate inputs
  if (is.null(dataset_name) || dataset_name == "") {
    stop("dataset_name cannot be NULL or empty.", call. = FALSE)
  }

  # Check if storage_account is available
  if (!exists("storage_account") || is.null(storage_account) || storage_account == "") {
    stop("storage_account must be defined and non-empty.", call. = FALSE)
  }

  tryCatch(
    {
      etl_export_log <- .read_documentation_from_azure_blob_storage("etl_export_log.csv") %>%
        dplyr::mutate(
          dataset = as.character(dataset),
          timestamp = as.character(timestamp) # or as.POSIXct() if needed
        ) %>%
        dplyr::filter(dataset != dataset_name) %>%
        dplyr::add_row(
          dataset = dataset_name,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          script_name = .process_script_name()
        )

      upload_file_to_azure_blob_storage(
        storage_account,
        Sys.getenv("SAS_TOKEN"),
        "documentation",
        etl_export_log,
        "etl_export_log",
        "csv"
      )
    },
    error = function(e) {
      stop("Failed to log run time for dataset '", dataset_name, "': ", e$message, call. = FALSE)
    }
  )
}

# Helper function to determine the script name
.process_script_name <- function() {
  # Check call stack (headless / sourced)
  for (i in sys.nframe():1) {
    if (!is.null(sys.frame(i)$ofile)) {
      return(basename(sys.frame(i)$ofile))
    }
  }

  # Fallback to RStudio interactive
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(basename(rstudioapi::getActiveDocumentContext()$path))
  }

  return("")
}

