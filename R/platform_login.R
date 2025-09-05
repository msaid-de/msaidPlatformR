#' Authenticate with MSAID Platform
#'
#' Authenticates the user with the MSAID Platform mass spectrometry data analysis platform using OAuth/OIDC flow.
#' Opens a browser window for login, starts a local web server to receive the authentication
#' callback, and stores the access tokens for subsequent API calls. If a valid refresh token is available,
#' it will be used automatically to obtain new access tokens without requiring browser authentication.
#'
#' @param region Character string. Region where the platform is deployed.
#'   Default is 'eu-central-1'.
#' @param stage Character string. Environment stage to connect to. Must be one of:
#'   \itemize{
#'     \item "prod" - Production environment (default)
#'     \item "dev" - Development environment
#'     \item "testing" - Testing environment
#'   }
#' @param endpoint Character string or NULL. Custom API endpoint URL. If NULL (default),
#'   the endpoint is automatically constructed based on region and stage parameters.
#'   Format: "https://api.\{prefix\}.\{suffix\}.msaid.io"
#' @param open_browser Logical. Whether to automatically open the browser for authentication.
#'   Default is TRUE. When FALSE, only prints the authentication URL.
#'
#' @return Invisible NULL. The function stores authentication tokens internally and does not return them directly.
#'   Success is indicated by the message "Login was successful".
#'
#' @details
#' The authentication process involves:
#' \enumerate{
#'   \item Check for existing valid refresh token and attempt automatic token refresh
#'   \item If no valid token exists, start a local web server on port 8000
#'   \item Open browser to login page with authorization request
#'   \item Wait for user to complete login and receive authorization code via redirect
#'   \item Exchange authorization code for access and refresh tokens
#'   \item Store tokens securely for use in subsequent API calls
#' }
#'
#' For remote server usage (e.g., RStudio Server), ensure port 8000 is forwarded:
#' \code{ssh -L 8000:localhost:8000 <remote-server>}
#'
#' @note Port 8000 must be available on localhost for the OAuth redirect to work properly.
#'   The function will fail if this port is already in use.
#'
#' @examples
#' \dontrun{
#' # Login to production environment (default)
#' platform_login()
#'
#' # Login to production in a specific region
#' platform_login(region = "eu-central-1", stage = "prod")
#'
#' # Login to development environment
#' platform_login(region = "eu-west-1", stage = "dev")
#'
#' # Login with custom endpoint
#' platform_login(endpoint = "https://api.custom.msaid.io")
#'
#' # Login without automatically opening browser
#' platform_login(open_browser = FALSE)
#' }
#'
#' @seealso \code{\link{platform_logout}} for logging out,
#'   \code{\link{platform_set_debug}} for enabling debug output during authentication
#'
#' @importFrom httpuv startServer service stopServer
#' @importFrom httr GET POST content http_error
#' @export
platform_login <- function(region='eu-central-1', stage='prod', endpoint=NULL, open_browser=TRUE) {

  remote_server_note <- "Note: If you are connecting to a RStudio session on a remote server, use open_browser = FALSE and ssh forwarding (see ?platform_login)."

  auth_code_holder <- new.env()
  auth_code_holder$code <- NULL
  auth_code_holder$refresh_token <- NULL

  auth_code_holder$refresh_token <- get_env_var("refresh_token")

  cognito_token_url <- ""
  endpoint_suffix <- ""
  endpoint_prefix <- ""

  if (stage == "prod"){
    cognito_token_url <- sprintf("https://platform-prod-idp.auth.%s.amazoncognito.com",region)
    endpoint_suffix <- "platform"
    endpoint_prefix <- region
  } else if ((stage == "dev") || (stage == "testing")) {
    cognito_token_url <- sprintf("https://alexandria-%s-idp.auth.%s.amazoncognito.com",stage,region)
    endpoint_suffix <- "alexandria.cluster.internal"
    endpoint_prefix <- stage
  } else {
    stop("Platform login failed: Stage parameter is incorrect")
  }

  if (is.null(endpoint)){
    endpoint <- sprintf("https://api.%s.%s.msaid.io",endpoint_prefix,endpoint_suffix)
  }

  set_env_var("region",region)
  set_env_var("stage",stage)
  set_env_var("endpoint",endpoint)
  debug <- get_env_var("debug")
  if (debug){
    message("region: ", region)
    message("stage: ", stage)
    message("endpoint: ", endpoint)
  }
  # Fetch Cognito User Pool information
  user_pool_info_url <- sprintf("%s/v1/regions", endpoint)
  response <- httr::GET(user_pool_info_url)

  if (httr::http_error(response)) {
    stop("Platform login failed: Failed to fetch user pool information.")
  }
  user_pool_info <- httr::content(response, as = "parsed", type = "application/json")

  cognito_client_id <- user_pool_info[[1]]$clientId
  user_pool_id <- user_pool_info[[1]]$userPoolId

  # Try using the refresh token
  if (!is.null(auth_code_holder$refresh_token)) {
    tokens <- create_id_token(auth_code_holder$refresh_token,cognito_token_url,cognito_client_id)
    if (!is.null(tokens$id_token)) {
      set_env_var("id_token",tokens$id_token)
      return(invisible())
    }
  }

  # Start a simple web server to receive the code
  server <- httpuv::startServer("127.0.0.1", 8000, list(
    call = function(req) {
      auth_code_holder$code <- strsplit(req$QUERY_STRING, "?code=")[[1]][2]
      return(list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        body = "<html><body><h2>Login successful. You may close this window.</h2></body></html>"
      ))
    }
  ))
  on.exit(httpuv::stopServer(server), add = TRUE)

  # Perform browser-based login
  auth_url <- sprintf("%s/login?client_id=%s&redirect_uri=http://localhost:8000/login.html&response_type=code",cognito_token_url, cognito_client_id)

  # Handle browser opening based on open_browser parameter
  if (open_browser) {
    # Try to open browser automatically
    tryCatch({
      utils::browseURL(auth_url)
      cat("\nOpening browser automatically. If it doesn't open, please visit this URL:\n\n", auth_url, "\n\n", sep = "")
    }, error = function(e) {
      cat("\nFailed to open browser automatically. Please visit this URL:\n\n", auth_url, "\n\n", sep = "")
    })
    cat(remote_server_note, "\n\n")
  } else {
    cat("\nPlease open this URL in your browser:\n\n", auth_url, "\n\n", sep = "")
    cat("For remote server usage (e.g., RStudio Server), ensure port 8000 is forwarded:\n")
    cat("ssh -L 8000:localhost:8000 <remote-server>\n\n")
  }

  while (is.null(auth_code_holder$code)) {
    httpuv::service()       # this processes incoming HTTP requests
    Sys.sleep(0.1)  # sleep to reduce CPU usage
  }

  body <- list(
    grant_type = "authorization_code",
    client_id = cognito_client_id,
    code = auth_code_holder$code,
    redirect_uri = "http://localhost:8000/login.html"
  )

  exchange_code_token_url <- sprintf("%s/oauth2/token",cognito_token_url)

  tryCatch({
    res <- httr::POST(exchange_code_token_url, body = body, encode = "form")

    if (httr::http_error(res)) {
      stop(httr::content(res, as = "text"))
    }

    tokens <- httr::content(res, as = "parsed", type = "application/json")
    set_env_var("refresh_token",tokens$refresh_token)
    set_env_var("id_token",tokens$id_token)
  }, error = function(e) {
    error_msg <- as.character(e$message)
    cat("Platform login failed: ", error_msg)
    cat(remote_server_note)
    stop(error_msg)
  })
  cat("Login was successful")
  return(invisible())
}
