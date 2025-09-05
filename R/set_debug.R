#' Enable or Disable Debug Output
#'
#' Controls the verbosity of debug messages throughout the msaidPlatformR package.
#' When debug mode is enabled, functions will print additional information about
#' their operations, including API endpoints, parameters, and internal processing steps.
#' This is useful for troubleshooting authentication issues, API connectivity problems,
#' or understanding the data flow during experiment retrieval and caching operations.
#'
#' @param enabled Logical. TRUE to enable debug output, FALSE to disable it.
#'   Default is FALSE (debug disabled).
#'
#' @return NULL (invisible). The function does not return a value but sets the internal debug state.
#'
#' @details
#' When debug mode is enabled, the following functions will produce additional output:
#' \itemize{
#'   \item \code{\link{platform_login}} - Shows region, stage, endpoint, and authentication flow details
#'   \item \code{\link{platform_list_experiments}} - Shows API endpoint and request parameters  
#'   \item \code{\link{platform_read_experiment_results}} - Shows region, stage, endpoint, and data processing steps
#'   \item \code{\link{platform_clear_experiment_cache}} - Shows cache clearing operations and target directories
#' }
#'
#' Debug mode persists for the duration of the R session or until explicitly disabled.
#' The debug state is stored in an internal environment and does not affect other R packages.
#'
#' @section Troubleshooting Use Cases:
#' Enable debug mode when experiencing:
#' \itemize{
#'   \item Authentication failures - to see which endpoints are being contacted
#'   \item Network connectivity issues - to verify API endpoint construction
#'   \item Data retrieval problems - to trace the experiment resolution and caching process
#'   \item Cache management issues - to see which files and directories are being affected
#' }
#'
#' @examples
#' \dontrun{
#' # Enable debug output
#' platform_set_debug(TRUE)
#'
#' # Now all function calls will show debug information
#' platform_login()  # Will show detailed authentication flow
#' 
#' # List experiments with debug info
#' experiments <- platform_list_experiments(name_includes = "test")
#'
#' # Disable debug output
#' platform_set_debug(FALSE)
#'
#' # Functions will now operate silently (except for normal output)
#' more_experiments <- platform_list_experiments()
#' }
#'
#' @seealso All platform functions support debug output when enabled.
#'   Use \code{\link{platform_login}}, \code{\link{platform_list_experiments}},
#'   \code{\link{platform_read_experiment_results}}, \code{\link{platform_clear_experiment_cache}}
#'
#' @export
platform_set_debug <- function(enabled = FALSE){
  .internal_env$debug <- enabled
}
