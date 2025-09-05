#' shorten_cache_path
#' shortens cache paths by removing organization_uuid and account_uuid portions to address Windows path length limitations
#' @param path original cache path that may contain organization_uuid= and account_uuid= portions
#'
#' @return shortened path starting from experiment_uuid=
#'
#' @examples \dontrun{
#'   # Shorten a cache path by removing organization and account UUID portions
#'   shorten_cache_path("result-db/v1/psms.parquet/organization_uuid=abc/experiment_uuid=ghi")
#' }
shorten_cache_path <- function(path) {
  # Validate that path starts with 'result-db/'
  if (!startsWith(path, "result-db/")) {
    stop(sprintf("Invalid cache path: path must start with 'result-db/', got: %s", path))
  }
  
  # Replace organization_uuid=.*experiment_uuid= with experiment_uuid=
  shortened_path <- sub("/organization_uuid=.*experiment_uuid=", "/experiment_uuid=", path)
  return(shortened_path)
}
