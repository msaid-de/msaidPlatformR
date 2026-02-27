#' Session variable names stored per-stage
#' @keywords internal
SESSION_VARS <- c("region", "stage", "id_token", "refresh_token", "endpoint")

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
  if (env_name %in% SESSION_VARS) {
    active <- .internal_env$active_stage
    if (is.null(active)) return(NULL)
    sessions <- .internal_env$sessions
    if (is.null(sessions[[active]])) return(NULL)
    return(sessions[[active]][[env_name]])
  }
  env_value <- get0(x=env_name,envir=.internal_env)
  return (env_value)
}
