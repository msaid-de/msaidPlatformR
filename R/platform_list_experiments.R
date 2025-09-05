#' List Available Experiments
#'
#' Retrieves a list of experiments available on the MSAID Platform that the authenticated user has access to.
#' Results can be filtered by experiment name, username, tags, and creation date range. The function returns
#' a data frame containing experiment metadata including names, UUIDs, creation dates, and other properties.
#'
#' @param name_includes Character string or NULL. Filter experiments whose names contain this substring.
#'   Case-sensitive partial matching. If NULL (default), no name filtering is applied.
#' @param name Character string or NULL. Filter experiments with this exact name.
#'   If NULL (default), no exact name filtering is applied.
#' @param username Character string or NULL. Filter experiments created by this username.
#'   If NULL (default), shows experiments from all users the current user has access to.
#' @param tags Character vector. Filter experiments that have all of these tags.
#'   Default is empty vector c() (no tag filtering).
#' @param from Character string, POSIXct object, or NULL. Filter experiments created after this timestamp.
#'   Can be provided as ISO 8601 string (e.g., "2024-01-01T00:00:00Z") or POSIXct object.
#'   If NULL (default), no start date filtering is applied.
#' @param to Character string, POSIXct object, or NULL. Filter experiments created before this timestamp.
#'   Can be provided as ISO 8601 string (e.g., "2024-12-31T23:59:59Z") or POSIXct object.
#'   If NULL (default), no end date filtering is applied.
#'
#' @return A data frame containing experiment information with columns such as:
#'   \itemize{
#'     \item \code{name} - Experiment name
#'     \item \code{uuid} - Unique identifier for the experiment
#'     \item \code{description} - Experiment description
#'     \item \code{username} - Username of the experiment creator
#'     \item \code{createdAt} - Creation timestamp
#'     \item \code{tags} - Associated tags
#'     \item \code{status} - Current experiment status
#'   }
#'   Returns an empty data frame if no experiments match the filtering criteria.
#'
#' @details
#' The function uses pagination internally with a page size of 100 experiments. If more than 100 experiments
#' match your filters, the function will throw an error asking you to narrow your selection. This prevents
#' accidentally retrieving very large result sets.
#'
#' Authentication is required before calling this function. Use \code{\link{platform_login}} to authenticate.
#'
#' @section Filtering Tips:
#' \itemize{
#'   \item Use \code{name_includes} for flexible name searching (e.g., "proteomics" to find all proteomics experiments)
#'   \item Use \code{name} for exact matches when you know the complete experiment name
#'   \item Combine multiple filters to narrow down results (e.g., username + date range)
#'   \item Use date filtering to find recent experiments or experiments from specific time periods
#' }
#'
#' @examples
#' \dontrun{
#' # List all available experiments
#' all_experiments <- platform_list_experiments()
#'
#' # Find experiments containing "proteomics" in the name
#' proteomics_experiments <- platform_list_experiments(name_includes = "proteomics")
#'
#' # Find experiments by a specific user
#' user_experiments <- platform_list_experiments(username = "john.doe")
#'
#' # Find experiments from the last month
#' recent_experiments <- platform_list_experiments(
#'   from = as.POSIXct(Sys.Date() - 30)
#' )
#'
#' # Find experiments from a specific date range with tags
#' filtered_experiments <- platform_list_experiments(
#'   from = "2024-01-01T00:00:00Z", 
#'   to = "2024-12-31T23:59:59Z",
#'   tags = c("human", "plasma")
#' )
#'
#' # Find a specific experiment by exact name
#' specific_experiment <- platform_list_experiments(name = "my_proteomics_study_v2")
#' }
#'
#' @seealso \code{\link{platform_login}} for authentication,
#'   \code{\link{platform_read_experiment_results}} for reading experiment data
#'
#' @export
#' @importFrom httr GET content add_headers http_error
platform_list_experiments <- function(name_includes=NULL,name=NULL,username=NULL,tags=c(),from=NULL,to=NULL){
  id_token <- get_id_token()
  endpoint <- get_env_var("endpoint")
  debug <- get_env_var("debug")
  pageSize <- 100
  if (debug){
    region <- get_env_var("region")
    stage <- get_env_var("stage")
    message("region: ", region)
    message("stage: ", stage)
    message("endpoint: ", endpoint)
  }

  filter_str <- create_filter_experiment_list(name_includes,name,username,tags,from,to)
  experiment_endpoint <- sprintf("%s/v1/experiments",endpoint)

  experiment_params <- list(
    page=0,
    pageSize=pageSize,
    filter=filter_str
  )
  response <- httr::GET(experiment_endpoint, httr::add_headers(Authorization = paste("Bearer", id_token)),query=experiment_params)
  if (httr::http_error(response)){
    stop(response)
  }

  content <- httr::content(response, as = "text")

  experiments <- jsonlite::fromJSON(content)

  if (experiments$totalItemCount > pageSize){
    stop("Too many experiments. Please narrow your selection using filters.")
  }

  if (length(experiments$pageItems) == 0) {
    return(data.frame())
  }

  experiments_df <- experiments$pageItems

  return(experiments_df)
}
