#' Upload a file to the Azure raw container
#' @param cont Destination container in the Blob storage
#' @param local_path Path to the local file to upload
#' @param directory Name of the destination directory in the Blob storage
#' @param file_name Filename for the file in the Blob storage
#' @return TRUE if upload succeeded, error otherwise
upload_file_to_raw_sa <- function(cont,
                                  local_path,
                                  directory = "",
                                  file_name) {
  # This function connects to the 'raw' container using the standard project setup
  # and uploads the file as-is. Assumes authentication and endpoint are handled in setup.

  AzureStor::upload_blob(cont, src = local_path, dest = paste0(directory, "/", file_name))
  invisible(TRUE)
}
