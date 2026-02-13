#' Download and read an Excel file from an Azure Blob Storage container
#'
#' @param container AzureStor container object (e.g., 'cont')
#' @param file_path Path to the file within the container (e.g., "NSE/2026/.../file.xlsx")
#' @param sheet Sheet name or index to read (default = 1)
#' @param ... Additional arguments passed to readxl::read_excel()
#'
#' @return A data frame containing the sheet contents
#' @examples
#' df <- read_excel_from_blob(cont, "NSE/2026/.../file.xlsx", sheet = 1)
read_excel_from_blob <- function(container, file_path, sheet = 1, ...) {

  # Create temporary file path
  tf <- base::tempfile(fileext = ".xlsx")

  # Ensure temporary file is deleted even if an error occurs
  base::on.exit(base::try(base::unlink(tf), silent = TRUE), add = TRUE)

  # Download the Excel file from Azure Blob Storage
  AzureStor::storage_download(container, src = file_path, dest = tf, overwrite = TRUE)

  # Optional: validate that the sheet exists
  available_sheets <- readxl::excel_sheets(tf)
  if (is.numeric(sheet)) {
    if (sheet < 1 || sheet > base::length(available_sheets)) {
      base::stop(base::sprintf(
        "Sheet index %s does not exist. Available sheets: %s",
        sheet, base::paste(available_sheets, collapse = ", ")
      ))
    }
  } else if (is.character(sheet) && !sheet %in% available_sheets) {
    base::stop(base::sprintf(
      "Sheet '%s' not found. Available sheets: %s",
      sheet, base::paste(available_sheets, collapse = ", ")
    ))
  }

  # Read the Excel sheet
  df <- readxl::read_excel(tf, sheet = sheet, ...)

  # Return data frame
  return(df)
}
