#' Upload File to Azure Blob Storage
#'
#' Uploads data to Azure Blob Storage in the specified format (RDS, CSV, or Parquet).
#' This is a generic upload function that delegates to specific format handlers.
#'
#' @param storage_account Character string. Name of the Azure Storage account.
#' @param sas_token Character string. SAS token for authentication.
#' @param container_name Character string. Name of the container to upload to.
#' @param data Data.frame or tibble. The data to upload.
#' @param dataset_name Character string. Base name for the file (extension added automatically).
#' @param file_type Character string. File format: "rds", "csv", or "parquet".
#'
#' @return Invisible NULL. Function is called for side effects.
#'
#' @details
#' This function checks for a valid SAS token before proceeding and then
#' delegates to the appropriate format-specific upload function.
#'
#' @examples
#' \dontrun{
#' upload_file_to_azure_blob_storage(
#'   storage_account = "mystorageaccount",
#'   sas_token = Sys.getenv("SAS_TOKEN"),
#'   container_name = "bronze",
#'   data = mtcars,
#'   dataset_name = "mtcars_enriched",
#'   file_type = "parquet"
#' )
#' }
#'
#' @export
upload_file_to_azure_blob_storage <- function(
    storage_account,
    sas_token,
    container_name,
    data,
    dataset_name,
    file_type) {
  # Validate inputs
  if (is.null(storage_account) || storage_account == "") {
    stop("storage_account cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(sas_token) || sas_token == "") {
    stop("sas_token cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(container_name) || container_name == "") {
    stop("container_name cannot be NULL or empty.", call. = FALSE)
  }
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a non-NULL data frame.", call. = FALSE)
  }
  if (is.null(dataset_name) || dataset_name == "") {
    stop("dataset_name cannot be NULL or empty.", call. = FALSE)
  }
  if (!file_type %in% c("rds", "csv", "parquet")) {
    stop("file_type must be one of: 'rds', 'csv', 'parquet'.", call. = FALSE)
  }

  # First check if the SAS_KEY is not empty
  .check_if_sas_key_is_set()

  tryCatch(
    {
      endpoint_url <- sprintf("https://%s.blob.core.windows.net", storage_account)
      bl_endp_sas <- AzureStor::storage_endpoint(endpoint_url, sas = sas_token)
      cont <- AzureStor::storage_container(bl_endp_sas, container_name)

      if (file_type == "rds") {
        upload_rds_to_azure_blob_storage(data, cont, dataset_name)
      } else if (file_type == "csv") {
        upload_csv_to_azure_blob_storage(data, cont, dataset_name)
      } else if (file_type == "parquet") {
        upload_parquet_to_azure_blob_storage(data, cont, dataset_name)
      }
    },
    error = function(e) {
      stop("Failed to upload file to Azure: ", e$message, call. = FALSE)
    }
  )
}

#' Upload RDS File to Azure Blob Storage
#'
#' Uploads a data object as an RDS file to Azure Blob Storage.
#'
#' @param data Data.frame or any R object. The data to save as RDS.
#' @param cont AzureStor container object. The Azure Storage container.
#' @param dataset_name Character string. Base name for the file (will get .rds extension).
#'
#' @return Invisible NULL. Function is called for side effects.
#'
#' @examples
#' \dontrun{
#' container <- connect_to_azure_container(endpoint_url, "mycontainer")
#' upload_rds_to_azure_blob_storage(mtcars, container, "mtcars_data")
#' }
#'
upload_rds_to_azure_blob_storage <- function(
    data,
    cont,
    dataset_name) {
  # Validate inputs
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a non-NULL data frame.", call. = FALSE)
  }
  if (is.null(cont)) {
    stop("cont (Azure container) cannot be NULL.", call. = FALSE)
  }
  if (is.null(dataset_name) || dataset_name == "") {
    stop("dataset_name cannot be NULL or empty.", call. = FALSE)
  }

  tryCatch(
    {
      file_path <- file.path(paste0(dataset_name, ".rds"))
      AzureStor::storage_save_rds(data, cont, file_path)
    },
    error = function(e) {
      stop("Failed to upload RDS file '", dataset_name, ".rds': ", e$message, call. = FALSE)
    }
  )
}

#' Upload CSV File to Azure Blob Storage
#'
#' Uploads a data frame as a CSV file to Azure Blob Storage using semicolon separator.
#'
#' @param data Data.frame or tibble. The data to save as CSV.
#' @param cont AzureStor container object. The Azure Storage container.
#' @param dataset_name Character string. Base name for the file (will get .csv extension).
#'
#' @return Invisible NULL. Function is called for side effects.
#'
#' @details
#' Uses AzureStor::storage_write_csv2() which writes CSV files with semicolon
#' separators, consistent with European locale conventions.
#'
#' @examples
#' \dontrun{
#' container <- connect_to_azure_container(endpoint_url, "mycontainer")
#' upload_csv_to_azure_blob_storage(mtcars, container, "mtcars_data")
#' }
#'
upload_csv_to_azure_blob_storage <- function(
    data,
    cont,
    dataset_name) {
  # Validate inputs
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a non-NULL data frame.", call. = FALSE)
  }
  if (is.null(cont)) {
    stop("cont (Azure container) cannot be NULL.", call. = FALSE)
  }
  if (is.null(dataset_name) || dataset_name == "") {
    stop("dataset_name cannot be NULL or empty.", call. = FALSE)
  }

  tryCatch(
    {
      file_path <- file.path(paste0(dataset_name, ".csv"))
      AzureStor::storage_write_csv2(data, cont, file_path)
    },
    error = function(e) {
      stop("Failed to upload CSV file '", dataset_name, ".csv': ", e$message, call. = FALSE)
    }
  )
}

upload_parquet_to_azure_blob_storage <- function(
    data,
    cont,
    dataset_name) {
  # Validate inputs
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a non-NULL data frame.", call. = FALSE)
  }
  if (is.null(cont)) {
    stop("cont (Azure container) cannot be NULL.", call. = FALSE)
  }
  if (is.null(dataset_name) || dataset_name == "") {
    stop("dataset_name cannot be NULL or empty.", call. = FALSE)
  }

  tryCatch(
    {
      # Create temporary parquet file
      local_parquet_file <- tempfile(fileext = ".parquet")
      arrow::write_parquet(data, local_parquet_file)

      # Verify the file was created
      if (!file.exists(local_parquet_file) || file.size(local_parquet_file) == 0) {
        stop("Failed to create temporary parquet file.", call. = FALSE)
      }

      # Upload to Azure Blob Storage
      file_path <- file.path(paste0(dataset_name, ".parquet"))
      AzureStor::upload_blob(
        cont,
        src = local_parquet_file,
        dest = file_path
      )

      # Clean up temporary file
      unlink(local_parquet_file)
      invisible(NULL)
    },
    error = function(e) {
      stop("Failed to upload Parquet file '", dataset_name, ".parquet': ", e$message, call. = FALSE)
    }
  )
}

#' Upload an XLSX Workbook to Azure Blob Storage
#'
#' Saves a given `openxlsx` workbook to a temporary `.xlsx` file and uploads it
#' to the specified Azure Blob Storage container using the `AzureStor` package.
#'
#' @param wb A `Workbook` object from the `openxlsx` package to be saved and uploaded.
#' @param cont An Azure Blob Storage container object created by `AzureStor::blob_container()`.
#' @param dataset_name A character string specifying the base name for the uploaded `.xlsx` file (without extension).
#'
#' @return Invisibly returns `NULL`. Used for its side effect of uploading a file to Azure.
#'
#' @importFrom openxlsx saveWorkbook
#' @importFrom AzureStor upload_blob
#'
#' @examples
#' \dontrun{
#' wb <- openxlsx::createWorkbook()
#' cont <- AzureStor::blob_container("https://<account>.blob.core.windows.net/<container>", key = "<key>")
#' upload_xlsx_workbook_to_azure_blob_storage(wb, cont, "my_dataset")
#' }
#'
upload_xlsx_workbook_to_azure_blob_storage <- function(
    wb,
    cont,
    dataset_name) {
  tryCatch(
    {
      local_xlsx_file <- tempfile(fileext = ".xlsx")

      openxlsx::saveWorkbook(
        wb,
        local_xlsx_file,
        overwrite = TRUE
      )


      # Verify the file was created
      if (!file.exists(local_xlsx_file) || file.size(local_xlsx_file) == 0) {
        stop("Failed to create temporary xlsx file.", call. = FALSE)
      }

      # Upload to Azure Blob Storage
      file_path <- file.path(paste0(dataset_name, ".xlsx"))

      AzureStor::upload_blob(
        cont,
        src = local_xlsx_file,
        dest = file_path
      )

      # Clean up temporary file
      unlink(local_xlsx_file)
      invisible(NULL)
    },
    error = function(e) {
      stop("Failed to upload XLSX file '", dataset_name, ".xlsx': ", e$message, call. = FALSE)
    }
  )
}
