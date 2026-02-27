#' init_internal_env
#'
#' @return nothing
#'
#' @examples \dontrun{
#'   init_internal_env()
#' }
init_internal_env <- function(){
  .internal_env$sessions <- list()
  .internal_env$active_stage <- NULL
  .internal_env$cache_root <- file.path(Sys.getenv("HOME"), ".msaid", "platform", "cache","experiments")
  .internal_env$debug <- FALSE
}
# Child R environment for msaidPlatformR package
.internal_env <- new.env(parent=asNamespace(packageName()))
init_internal_env()
