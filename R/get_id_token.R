#' get_id_token
#' returns the id token to the msaid platform
#' @return id_token
#'
#' @examples \dontrun{
#' get_id_token()
#' }
#' @importFrom httr GET status_code add_headers http_error
get_id_token<- function(){
  id_token <- get_env_var("id_token")
  endpoint <- get_env_var("endpoint")
  region <- get_env_var("region")
  stage <- get_env_var("stage")
  experiment_endpoint <- sprintf("%s/v1/experiments",endpoint)
  response <- httr::GET(experiment_endpoint, httr::add_headers(Authorization = paste("Bearer", id_token)))
  if (httr::http_error(response)){
    print("Refreshing ID token")
    platform_login(region=region,stage=stage)
  }
  id_token <- get_env_var("id_token")
  return(id_token)
}
