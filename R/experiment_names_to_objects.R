#' experiment_names_to_objects
#' converts the experiment names to experiment objects
#' @param experiment_names names of the experiments
#'
#' @return list of experiment objects (pageItems from API response)
#'
#' @examples \dontrun{
#' platform_read_parquets("")
#' }
#' @importFrom httr GET content add_headers http_error

experiment_names_to_objects <- function(experiment_names){
  endpoint <- get_env_var("endpoint")
  id_token <- get_id_token()

  experiment_endpoint <- sprintf("%s/v1/experiments",endpoint)
  query_parts <- paste0("name,==,", experiment_names)
  experiments_query <- paste(query_parts, collapse = ";")
  page_size <- 1000

  if (length(experiment_names) > page_size) {
    stop("Too many experiments for request")
  }

  experiment_params <- list(
    page=0,
    pageSize=page_size,
    filter=experiments_query
  )
  response <- httr::GET(experiment_endpoint, httr::add_headers(Authorization = paste("Bearer", id_token)),query=experiment_params)
  if (httr::http_error(response)){
    print("Error occurred fetching of experiments")
    stop(response)
  }
  experiments <- httr::content(response, as = "parsed", type = "application/json")

  if (length(experiment_names) != length(experiments$pageItems)) {
    found_experiment_names <- sapply(experiments$pageItems, function(exp) exp$name)
    missing_experiments <- setdiff(experiment_names, found_experiment_names)
    if (length(missing_experiments) > 0) {
      stop("Could not find the following experiments: ", paste(missing_experiments, collapse = ", "))
    }
  }

  return(experiments$pageItems)
}
