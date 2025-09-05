#' init_internal_env
#'
#' @return nothing
#'
#' @examples \dontrun{
#'   init_internal_env()
#' }
init_internal_env <- function(){
  region <- "eu-central-1"
  stage <- "prod"
  refresh_token <- NULL
  id_token <- NULL
  endpoint <- NULL
  cache_root <- file.path(Sys.getenv("HOME"), ".msaid", "platform", "cache","experiments")
  debug <- FALSE
  # this is invoked at the initial set-up
  if (!exists("refresh_token",envir = .internal_env)){
    .internal_env$region <- region
    .internal_env$stage <- stage
    .internal_env$refresh_token <- refresh_token
    .internal_env$id_token <- id_token
    .internal_env$endpoint <- endpoint
    .internal_env$cache_root <- cache_root
    .internal_env$debug <- debug
  } else { # this is invoked when user logs out
    set_env_var("region",region)
    set_env_var("stage",stage)
    set_env_var("refresh_token",refresh_token)
    set_env_var("id_token",id_token)
    set_env_var("endpoint",endpoint)
    set_env_var("cache_root",cache_root)
    set_env_var("debug",debug)
  }
}
# Child R environment for msaidPlatformR package
.internal_env <- new.env(parent=asNamespace(packageName()))
init_internal_env()
