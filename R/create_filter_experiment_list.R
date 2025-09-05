#' create_filter_experiment_list
#'
#' @param name_includes if name includes certain sub-string
#' @param name  exact experiment name
#' @param username  the username filter
#' @param tags  the tag of the experiments
#' @param from filter experiments created after this timestamp (ISO 8601 format or POSIXct object)
#' @param to filter experiments created before this timestamp (ISO 8601 format or POSIXct object)
#'
#' @return filter string
#'
#' @examples  \dontrun{
#' create_filter_experiment_list(name="hello")
#'}
create_filter_experiment_list <- function(name_includes=NULL,name=NULL,username=NULL,tags=c(),from=NULL,to=NULL){
  filter_str <- ""
  if(!is.null(name_includes)){
    filter_str <- sprintf("name,includes,%s",name_includes)
  }
  if(!is.null(name)){
    name_filter <- sprintf("name,==,%s",name)
    filter_str <- concat_filter(filter_str,name_filter)
  }
  if(!is.null(username)){
    username_filter <- sprintf("username,==,%s",username)
    filter_str <- concat_filter(filter_str,username_filter)
  }
  if(length(tags) > 0){
    tags_filter <- paste0(sprintf("tags,includesAll,%s", tags), collapse = ";")
    filter_str <- concat_filter(filter_str,tags_filter)
  }
  if(!is.null(from)){
    tryCatch({
      from <- validate_and_normalize_timestamp(from)
    }, error = function(e) {
      stop(sprintf("Parameter 'from': %s", e$message))
    })
    from_filter <- sprintf("createdAt,>=,%s",from)
    filter_str <- concat_filter(filter_str,from_filter)
  }
  if(!is.null(to)){
    tryCatch({
      to <- validate_and_normalize_timestamp(to)
    }, error = function(e) {
      stop(sprintf("Parameter 'to': %s", e$message))
    })
    to_filter <- sprintf("createdAt,<=,%s",to)
    filter_str <- concat_filter(filter_str,to_filter)
  }
  return (filter_str)
}
