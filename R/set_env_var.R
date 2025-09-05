#' set_env_var
#' sets environment variable
#' @param env_name environment variable name
#' @param env_value environment variable's value
#'
#'
#' @examples
#'  \dontrun{
#'  set_env_var("","","")
#'  }
set_env_var <- function(env_name,env_value){
  unlockBinding(env_name,.internal_env)
  assign(env_name, env_value, envir = .internal_env)
  lockBinding(env_name,.internal_env)
}
