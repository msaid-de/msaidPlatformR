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
  if (env_name %in% SESSION_VARS) {
    active <- .internal_env$active_stage
    if (is.null(active)) {
      stop("Cannot set session variable '", env_name, "': no active stage. Call platform_login() first.")
    }
    .internal_env$sessions[[active]][[env_name]] <- env_value
    return(invisible())
  }
  .internal_env[[env_name]] <- env_value
}
