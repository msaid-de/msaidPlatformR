#' Switch Active Stage
#'
#' Sets the active stage to a previously logged-in stage session.
#' All subsequent API calls will use the credentials from this stage.
#'
#' @param stage Character string. The stage to switch to (e.g., "prod", "dev", "testing").
#'   Must be a stage that was previously logged into with \code{\link{platform_login}}.
#'
#' @return NULL (invisible).
#'
#' @examples
#' \dontrun{
#' platform_login(stage = "prod")
#' platform_login(stage = "dev")
#'
#' # Switch back to prod without re-authenticating
#' platform_use(stage = "prod")
#' }
#'
#' @seealso \code{\link{platform_login}} for logging in,
#'   \code{\link{platform_logout}} for logging out
#'
#' @export
platform_use <- function(stage) {
  if (is.null(.internal_env$sessions[[stage]])) {
    stop("No session found for stage '", stage, "'. Call platform_login(stage = '", stage, "') first.")
  }
  .internal_env$active_stage <- stage
  message("Now using stage '", stage, "'")
  return(invisible())
}
