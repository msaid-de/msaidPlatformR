#' concat_filter
#'
#' @param total_filter_str total filter string
#' @param partial_filter_str partially added filter string
#'
#' @return result filter
#'
#' @examples \dontrun{
#' concat_filter("","")
#' }
concat_filter <- function(total_filter_str,partial_filter_str){
  result_filter <- ""
  if (total_filter_str != ""){
    result_filter <- paste0(total_filter_str,";")
    result_filter <- paste0(result_filter,partial_filter_str)
  } else {
    result_filter <- partial_filter_str
  }
  return (result_filter)
}
