#' create_id_token
#' creates id token by providing refresh token to the coginito
#' @param refresh_token string
#' @param cognito_token_url string
#' @param cognito_client_id string
#'
#' @return tokens object
#' @examples
#' \dontrun{
#'   create_id_token("","","")
#' }
#' @importFrom httr POST http_error content
create_id_token <- function(refresh_token,cognito_token_url,cognito_client_id) {
  body <- list(
    grant_type = "refresh_token",
    client_id = cognito_client_id,
    refresh_token = refresh_token
  )
  refresh_token_url <- sprintf("%s/oauth2/token",cognito_token_url)
  res <- httr::POST(refresh_token_url, body = body, encode="form")
  if (httr::http_error(res)) {
    return(NULL)
  }
  tokens <- httr::content(res, as = "parsed", type = "application/json")
  return(tokens)
}
