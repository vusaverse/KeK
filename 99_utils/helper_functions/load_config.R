#' Load Configuration from YAML Files
#'
#' Loads and merges configuration settings from multiple YAML files to create
#' a comprehensive configuration object for the education analytics pipeline.
#'
#' @param env Character string. The environment to load the configuration for (default: "dev").
#'   Must match an environment defined in environments.yaml.
#' @param path Character string. Path to the main YAML configuration file (default: "99_utils/config.yaml").
#'
#' @return A list containing the complete configuration settings including:
#'   \itemize{
#'     \item project: Project metadata
#'     \item paths: File system paths for different data stages
#'     \item blob_storage: Azure Storage configuration
#'     \item azure_storage: Container-specific Azure Storage settings
#'     \item common_paths: Shared path configurations
#'     \item logging_level: Environment-specific logging level
#'     \item datasets: Dataset-specific configurations
#'     \item olap: OLAP cube configuration settings
#'   }
#'
#' @details
#' This function reads and merges multiple YAML configuration files:
#' \itemize{
#'   \item config.yaml: Main project configuration
#'   \item datasets.yaml: Dataset-specific settings
#'   \item olap.yaml: OLAP configuration
#'   \item azure_storage.yaml: Azure Storage container configuration
#'   \item environments.yaml: Environment-specific settings
#' }
#'
#' @examples
#' \dontrun{
#' # Load default development configuration
#' config <- .load_config()
#'
#' # Load production configuration
#' config_prod <- .load_config(env = "prod")
#' }
#'
#' @export
.load_config <- function(env = "dev", path = "99_utils/config_files/config.yaml") {
  # Use yaml::read_yaml to read the main config
  config <- yaml::read_yaml(path)

  # Read split config files and join them
  datasets <- yaml::read_yaml("99_utils/config_files/datasets.yaml")$datasets
  olap <- yaml::read_yaml("99_utils/config_files/olap.yaml")$olap
  azure_storage <- yaml::read_yaml("99_utils/config_files/azure_storage.yaml")$azure_storage
  environments <- yaml::read_yaml("99_utils/config_files/environments.yaml")$environments
  sftp <- yaml::read_yaml("99_utils/config_files/sftp.yaml")
  download <- yaml::read_yaml("99_utils/config_files/download.yaml")
  secrets <- yaml::read_yaml("99_utils/config_files/secrets.yaml")

  if (!env %in% names(environments)) {
    stop(paste("Environment", env, "not found in environments.yaml"))
  }

  # Build the final config object
  list(
    project = config$project,
    paths = list(
      reports = config$common_paths$reports,
      logs = config$common_paths$logs,
      raw_data = environments[[env]]$raw_data,
      clean_data = environments[[env]]$clean_data
    ),
    blob_storage = list(
      account = environments[[env]]$blob_storage_account,
      container = environments[[env]]$blob_container
    ),
    azure_storage = list(
      account = azure_storage[[env]]$account,
      aanmeldingen_container = azure_storage[[env]]$containers$aanmeldingen,
      raw_container = azure_storage[[env]]$containers$raw,
      bronze_container = azure_storage[[env]]$containers$bronze,
      silver_container = azure_storage[[env]]$containers$silver,
      gold_container = azure_storage[[env]]$containers$gold,
      export_container = azure_storage[[env]]$containers$export,
      finance_container = azure_storage[[env]]$containers$finance
    ),
    common_paths = list(
      documentation_local = config$common_paths$documentation$local,
      github = config$common_paths$snippets$github,
      mapping_tables = config$common_paths$mapping_tables
    ),
    logging_level = environments[[env]]$logging_level,
    datasets = datasets,
    olap = olap,
    sftp = sftp$sftp,
    download = download$downloads,
    secrets = secrets$secrets
  )
}
