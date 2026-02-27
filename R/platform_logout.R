#' Logout from MSAID Platform
#'
#' Clears stored authentication tokens and resets the internal environment state,
#' effectively logging the user out from the MSAID Platform. After calling this function,
#' you will need to call \code{\link{platform_login}} again to authenticate before
#' accessing any platform resources.
#'
#' @param stage Character string or NULL. If provided, only clears the session for the
#'   specified stage (e.g., "dev", "prod"). If the cleared stage was the active stage,
#'   the active stage is set to NULL.
#'   If NULL (default), clears all sessions and resets to initial state.
#'
#' @return NULL (invisible). The function does not return a value but prints
#'   "Logout was successful" to confirm the logout operation.
#'
#' @details
#' This function performs the following operations:
#' \itemize{
#'   \item Clears stored ID tokens used for API authentication
#'   \item Clears stored refresh tokens used for automatic token renewal
#'   \item Resets all internal environment variables to their default state
#'   \item Does not clear the local experiment data cache (use \code{\link{platform_clear_experiment_cache}} for that)
#' }
#'
#' @note This function only clears local tokens and does not invalidate tokens on the server side.
#'   For complete security, especially on shared machines, consider also clearing your browser's
#'   authentication cookies for the MSAID Platform.
#'
#' @examples
#' \dontrun{
#' # Login to platform
#' platform_login()
#'
#' # Do some work...
#' experiments <- platform_list_experiments()
#'
#' # Logout when finished
#' platform_logout()
#'
#' # Logout from a specific stage only
#' platform_logout(stage = "dev")
#' }
#'
#' @seealso \code{\link{platform_login}} for logging in,
#'   \code{\link{platform_clear_experiment_cache}} for clearing cached data
#'
#' @export
platform_logout <- function(stage = NULL) {
  if (is.null(stage)) {
    # Full reset: clear all sessions
    init_internal_env()
  } else {
    # Remove only the specified stage session
    .internal_env$sessions[[stage]] <- NULL

    # If the removed stage was the active stage, clear it
    if (identical(.internal_env$active_stage, stage)) {
      .internal_env$active_stage <- NULL
    }
  }
  message("Logout was successful")
}
