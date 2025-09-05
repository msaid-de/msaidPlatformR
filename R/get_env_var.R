#' get_env_var
#' gets environment variable
#' @param env_name environment variable name
#'
#' @return env variable value
#' @examples
#'  \dontrun{
#'  get_env_var("")
#'  }
get_env_var <- function(env_name){
  env_value <- get0(x=env_name,envir=.internal_env)
  return (env_value)
}
